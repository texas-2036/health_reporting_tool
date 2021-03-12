
# Load Data -----------------------------------------------------------------------------------
us_map <- "https://code.highcharts.com/mapdata/countries/us/us-all.js"
#tx_map <- "https://code.highcharts.com/mapdata/countries/us/us-tx-all.js"
childhood_obesity <- readr::read_rds('clean_data/Children/childhood_obesity_data.rds')

wic_obesity <- read_rds('clean_data/Children/wic_obesity_data.rds')

phys_inactivity_hs_ts <- tibble(
  year = c("2007","2009","2011", "2013", "2015", "2017", "2019"),
  value = c(25.7, 27.2, 27.1, 30.0, NA, 25.2, 22.9)
)

phys_inactivity_dem_table <- read_rds("clean_data/Children/phys_inactivity_dem_table.rds")

vaping_tx_hs_dem_table <- read_rds("clean_data/Children/vaping_tx_hs_students_chart.rds")


# text module ----
childhood_rf_ui <- function(id) {
  
  fluidRow(
           tabBox(
             # title = "Risk Factors",
             id = "tabset1", 
             side="left",
             width = 12,
             # height = 600,
             tabPanel(title="Obesity", 
                      fluidRow(
                      column(width = 5,
                             h2("Obesity"),
                             includeMarkdown("markdown/childhood/risk_factors/obesity.md")),
                      column(width = 7,
                             highcharter::highchartOutput(NS(id, "obesity_map")))),
                      hr(),
                      fluidRow(
                        column(width = 5,
                               h2("The Cost of Obesity"),
                               includeMarkdown("markdown/childhood/risk_factors/obesity_cost.md")),
                        column(width = 7,
                               h2("Policy Spotlight"),
                               includeMarkdown("markdown/childhood/risk_factors/obesity_wic.md"),
                               highcharter::highchartOutput(NS(id, "obesity_wic_chart"))))),
             tabPanel(title="Physical Inactivity", 
                      fluidRow(
                        column(width = 6,
                               h2("Physical Inactivity"),
                               includeMarkdown("markdown/childhood/risk_factors/physical_inactivity.md"),
                               highcharter::highchartOutput(NS(id, "phys_inactivity_hs_chart"))),
                        column(width = 6,
                               gt::gt_output(NS(id, "phys_inactivity_hs_demographics"))))),
             tabPanel(title="Adolescence Smoking", 
                      fluidRow(
                        column(width = 5,
                               h2("Adolescence Smoking"),
                               includeMarkdown("markdown/childhood/risk_factors/smoking.md")),
                        column(width = 7,
                               highcharter::highchartOutput(NS(id, "tobacco_hs_use_ts"), height="325px"),
                               hr(),
                               highcharter::highchartOutput(NS(id, "vaping_youth_comparison"), height="175px"))),
                      hr(),
                      fluidRow(
                        column(width = 6,
                               h2("A Closer Look at Vaping"),
                               includeMarkdown("markdown/childhood/risk_factors/vaping_spotlight.md")),
                        column(width = 6,
                               gt::gt_output(NS(id, "vaping_demographics")))
                      ))
             )
           )
  
}

childhood_rf_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- ","
    options(highcharter.lang = hcoptslang)
    
    myMenuItems <- c("downloadCSV", "downloadXLS","separator", "downloadPNG", "downloadJPEG", "downloadPDF", "downloadSVG")
    
  output$obesity_map <- highcharter::renderHighchart({
      
    col_pal <- RColorBrewer::brewer.pal(9,"Blues")
    
    hcmap(map = us_map,
          data = childhood_obesity,
          value = "Value",
          joinBy = c("name","State Name"),
          name = "% of overweight or obese children",
          borderColor = "#FAFAFA",
          borderWidth = 0.1,
          tooltip = list(valueSuffix = '%'))  %>% 
      hc_legend(layout='vertical',
                align='left',
                verticalAlign='bottom',
                itemMarginTop=10,
                itemMarginBottom=10) %>% 
      hc_colorAxis(stops = color_stops(n=8, colors=col_pal),
                   reversed=FALSE) %>%
      hc_credits(
        enabled = TRUE,
        useHTML = TRUE,
        text = "SOURCE: America's Health Rankings Analysis of U.S. Department of Health and Human Services, Maternal and Child Health Bureau National Survey of Children's Health, 2018-2019", 
        #based on the Child and Adolescent Health Measurement Initiative (CAHMI) component of the National Survey of Children’s Health",
        href = "https://www.americashealthrankings.org/explore/health-of-women-and-children/measure/youth_overweight/state/ALL?edition-year=2020") %>%
      hc_title(text="Percentage of Overweight or Obese Youth, 2018-2019") %>% 
      hc_subtitle(text="The percentage of children ages 10-17 who are overweight or obese") %>% 
      hc_add_theme(tx2036_hc_light())
      
    })
  
  output$obesity_wic_chart <- highcharter::renderHighchart({
    
    highchart() %>% 
      hc_add_series(wic_obesity %>% filter(state_name!="Texas"), 
                    type="line", 
                    hcaes(x=year, y=data_value, group=state_name), 
                    color="#DBDCDD") %>% 
      hc_add_series(wic_obesity %>% filter(state_name=="Texas"),
                    type="line", 
                    hcaes(x=year, y=data_value),
                    lineWidth=5,
                    name="Texas") %>% 
      hc_title(text="Percent of WIC participants ages 2-4 with obesity") %>%
      hc_subtitle(text="Obesity is defined as body mass index (BMI)-for-age and sex ≥95th percentile based on the 2000 CDC growth chart; BMI was calculated from measured weight and height (weight [kg]/ height [m²]).Children with missing values of height, weight, and BMI were excluded.") %>%
      hc_yAxis(title=list(text="% of Adults Reporting Frequent Mental Distress"),
               labels = list(enabled=TRUE,
                             format = "{value}%")) %>% 
      hc_xAxis(tickColor = "#ffffff", 
               min = 0.5,
               max = 3.5,
               tickInterval = 1,
               maxPadding = 0,
               endOnTick = FALSE,
               startOnTick = FALSE,
               useHTML = TRUE,
               alternateGridColor = "#f3f3f3",
               categories = c("2008","2010","2012","2014","2016"),
               title = list(text = "Year")) %>%
      hc_legend(layout = "proximate", align = "right") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Women, Infants, and Children Participant and Program Characteristics (WIC) via the Center for Disease Control.",
        href = "https://nccd.cdc.gov/dnpao_dtm/rdPage.aspx?rdReport=DNPAO_DTM.ExploreByTopic&islClass=OWS&islTopic=OWS1&go=GO") %>%
      hc_add_theme(tx2036_hc_light())
    
  })
  
  output$phys_inactivity_hs_chart <- highcharter::renderHighchart({
    
    phys_inactivity_hs_ts %>% 
      hchart("column", hcaes(x=year, y=value)) %>% 
      hc_title(text="Physical Activity Trends Among Texas High Schoolers") %>% 
      hc_subtitle(text="Shown: High School Students Who Were Physically Active At Least 60 Minutes Per Day On All 7 Days") %>% 
      hc_yAxis(title=list(text="% of HS Students Reporting Physical Activity"),
               min = 0,
               max = 100,
               labels = list(enabled=TRUE,
                             format = "{value}%")) %>%
      hc_xAxis(tickColor = "#ffffff",
               useHTML = TRUE,
               # alternateGridColor = "#f3f3f3",
               title = list(text = "Year"),
               plotBands = list(label = list(text = "No Survey Data", rotation = 90, y = 100, x=-5,
                                             style = list(color = "rgba(255,255,255, 1)", fontWeight = '500',
                                                          fontSize='16px',textTransform='uppercase')),
                                color = "rgba(0, 45, 116, 0.2)",
                                width = 2,
                                from = 3.5,
                                to = 4.5)) %>%
      hc_legend(enabled = FALSE) %>%
      hc_colorAxis(
        stops = color_stops(10, viridisLite::inferno(8, direction = -1))) %>% 
      hc_credits(
        enabled = TRUE,
        text = "High School Youth Risk Behavior Survey Data | SOURCE: Centers for Disease Control and Prevention",
        href = "https://yrbs-explorer.services.cdc.gov/#/graphs?questionCode=QNPA7DAY&topicCode=C06&location=TX&year=2019") %>%
      highcharter::hc_add_theme(texas2036::tx2036_hc_light())
    
  })
  
  output$phys_inactivity_hs_demographics <- gt::render_gt({
    
    bar_chart <- function(value, color = "red", display_value = NULL){
      
      # Choose to display percent of total
      if (is.null(display_value)) {
        display_value <- "&nbsp;"
      } else {
        display_value <- display_value
      }
      
      # paste color and value into the html string
      glue::glue("<span style=\"display: inline-block; direction: ltr; border-radius: 4px; padding-right: 2px; background-color: {color}; color: {color}; width: {value}%\"> {display_value} </span>")
    }
    
    col_pal <- function(value){
      
      # set high and low
      domain_range <- range(c(min(phys_inactivity_dem_table$data_value),min(phys_inactivity_dem_table$data_value)))
      
      # create the color based of domain
      scales::col_numeric(
        paletteer::paletteer_d("ggsci::blue_material") %>% as.character(), 
        domain = c(min(value), max(value))
      )(value)
    }
    
    phys_inactivity_dem_gt <- phys_inactivity_dem_table %>%
      mutate(
        bar = round(data_value*2.1, digits = 1),
        color = col_pal(bar),
        bar_chart = bar_chart(bar, color = color),
        bar_chart = map(bar_chart, ~gt::html(as.character(.x)))) %>% 
      select(-bar, -color)
    
    phys_inactivity_dem_gt %>% 
      gt(rowname_col = "stratification", groupname_col = "stratification_category") %>% 
      tab_header(
        title = md("**Key Demographics for Students Meeting CDC Activity Recommendation**"),
        subtitle = "Shown is the 2019 prevalence of daily physical activity by student demographic, in Texas."
      ) %>% 
      tab_source_note(
        source_note = md("[Centers for Disease Control and Prevention (CDC). High School Youth Risk Behavior Survey Data.](https://yrbs-explorer.services.cdc.gov/#/graphs?questionCode=QNPA7DAY&topicCode=C06&location=TX&year=2019)")
      ) %>% 
      fmt_percent(columns = vars(data_value),
                  decimals = 1,
                  scale_values = FALSE) %>% 
      cols_label(
        data_value = "%",
        bar_chart = " ") %>%
      cols_width(vars(bar_chart) ~ px(200),
                 vars(data_value) ~ px(75),
                 vars(stratification) ~ px(250),
      ) %>% 
      cols_align(
        align = "center",
        columns = vars(data_value)
      ) %>% 
      cols_align(
        align = "left",
        columns = vars(bar_chart)
      ) %>% 
      tab_options(
        table.width = "95%",
        column_labels.hidden = TRUE,
        row_group.border.top.width = px(3),
        row_group.border.top.color = "#3A4A9F",
        row_group.border.bottom.color = "#DBDCDD",
        row_group.background.color = "#f4f4f4",
        row_group.font.size = "16px !important",
        row_group.font.weight = "700",
        row_group.padding = "4px",
        row_group.text_transform = "initial",
        table_body.hlines.color = "white",
        table.border.top.color = "white",
        table.border.top.width = px(1),
        table.border.bottom.color = "white",
        table.border.bottom.width = px(1),
        table_body.border.bottom.width = px(.5),
        table_body.border.bottom.color = "black",
        column_labels.border.bottom.color = "black",
        column_labels.border.bottom.width = px(1)
      ) 
    
  })
  
  output$tobacco_hs_use_ts <- highcharter::renderHighchart({
    
    tobacco_youth_compare <- tibble(year=c("2001","2003","2005","2007","2009",
                                          "2011","2013","2015","2017","2019"),
                                   pct=c(28.4,NA,24.2,21.1,21.2,
                                         17.4,14.1,NA,7.4,4.9))
    tobacco_youth_compare %>% 
      hchart("column", hcaes(x=year, y=pct)) %>% 
      hc_title(text="Prevalence of current cigarette use over time among Texas High School Students") %>% 
      hc_colorAxis(
        stops = color_stops(10, viridisLite::inferno(8, direction = 1))) %>%
      hc_legend(enabled=FALSE) %>% 
      hc_yAxis(labels=list(format="{value}%")) %>% 
      hc_credits(
        enabled = TRUE,
        text = "High School Youth Risk Behavior Survey Data | SOURCE: Centers for Disease Control and Prevention",
        href = "https://yrbs-explorer.services.cdc.gov/#/graphs?questionCode=H32&topicCode=C02&location=TX&year=2019") %>%
      highcharter::hc_add_theme(texas2036::tx2036_hc_light())
    
  })
  
  output$vaping_youth_comparison <- highcharter::renderHighchart({
    
    vaping_df_compare <- tibble(geo=c("TX","US"),
                                pct=c(.187,.327)
    )
    highchart() %>% 
      hc_add_series(vaping_df_compare%>%filter(geo =='TX') , type = "bar", hcaes(x=geo, y=pct*100), name ='TX') %>% 
      hc_add_series(vaping_df_compare%>%filter(geo =='US'), type = "bar", hcaes(x=geo, y=pct*100), name ='US') %>% 
      hc_xAxis(tickColor = "#ffffff", 
               opposite = FALSE,
               useHTML = TRUE,
               title = list(text = 'State'),
               categories = c(" ")) %>% 
      hc_yAxis(labels = list(format = "{value}%")) %>% 
      
      hc_title(text="Electronic Vaping Rates Among High School Students, 2019") %>%
      hc_credits(
        enabled = TRUE,
        text = "High School Youth Risk Behavior Survey Data | SOURCE: Centers for Disease Control and Prevention",
        href = "https://yrbs-explorer.services.cdc.gov/#/tables?questionCode=H35&topicCode=C02&location=TX&year=2019") %>%
      highcharter::hc_add_theme(texas2036::tx2036_hc_light())
    
    
  })
  
  output$vaping_demographics <- render_gt({
    
    vaping_tx_hs_dem_table
    
    bar_chart <- function(value, color = "red", display_value = NULL){
      
      # Choose to display percent of total
      if (is.null(display_value)) {
        display_value <- "&nbsp;"
      } else {
        display_value <- display_value
      }
      
      # paste color and value into the html string
      glue::glue("<span style=\"display: inline-block; direction: ltr; border-radius: 4px; padding-right: 2px; background-color: {color}; color: {color}; width: {value}%\"> {display_value} </span>")
    }
    
    col_pal <- function(value){
      
      # set high and low
      domain_range <- range(c(min(vaping_tx_hs_dem_table$data_value),min(vaping_tx_hs_dem_table$data_value)))
      
      # create the color based of domain
      scales::col_numeric(
        paletteer::paletteer_d("ggsci::blue_material") %>% as.character(), 
        domain = c(min(value), max(value))
      )(value)
    }
    
    vaping_dem_gt <- vaping_tx_hs_dem_table %>%
      mutate(
        bar = round(data_value*2.1, digits = 1),
        color = col_pal(bar),
        bar_chart = bar_chart(bar, color = color),
        bar_chart = map(bar_chart, ~gt::html(as.character(.x)))) %>% 
      select(-bar, -color)
    
    vaping_dem_gt %>% 
      gt(rowname_col = "stratification", groupname_col = "stratification_category") %>% 
      tab_header(
        title = md("**Key Demographics For High School Students Who Currently Use Electronic Vapor Products**"),
        subtitle = "Shown is the 2019 current electronic vapor product use by high school student demographic, in Texas."
      ) %>% 
      tab_source_note(
        source_note = md("[Centers for Disease Control and Prevention (CDC). 1991-2019 High School Youth Risk Behavior Survey Data.](https://yrbs-explorer.services.cdc.gov/#/graphs?questionCode=H35&topicCode=C02&location=TX&year=2019)")
      ) %>% 
      fmt_percent(columns = vars(data_value),
                  decimals = 1,
                  scale_values = FALSE) %>% 
      cols_label(
        data_value = "%",
        bar_chart = " ") %>%
      cols_width(vars(bar_chart) ~ px(200),
                 vars(data_value) ~ px(75),
                 vars(stratification) ~ px(250),
      ) %>% 
      cols_align(
        align = "center",
        columns = vars(data_value)
      ) %>% 
      cols_align(
        align = "left",
        columns = vars(bar_chart)
      ) %>% 
      tab_options(
        table.width = "95%",
        column_labels.hidden = TRUE,
        row_group.border.top.width = px(3),
        row_group.border.top.color = "#3A4A9F",
        row_group.border.bottom.color = "#DBDCDD",
        row_group.background.color = "#f4f4f4",
        row_group.font.size = "16px !important",
        row_group.font.weight = "700",
        row_group.padding = "4px",
        row_group.text_transform = "initial",
        table_body.hlines.color = "white",
        table.border.top.color = "white",
        table.border.top.width = px(1),
        table.border.bottom.color = "white",
        table.border.bottom.width = px(1),
        table_body.border.bottom.width = px(.5),
        table_body.border.bottom.color = "black",
        column_labels.border.bottom.color = "black",
        column_labels.border.bottom.width = px(1)
      ) 
    
  })
  
  
  
    
  })
  
}