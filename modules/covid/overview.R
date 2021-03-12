
# Load Data -----------------------------------------------------------------------------------
#us_map <- "https://code.highcharts.com/mapdata/countries/us/us-all.js"
tx_map <- "https://code.highcharts.com/mapdata/countries/us/us-tx-all.js"
case_rate_map <- read_rds("clean_data/covid/case_rate_map.rds") %>% 
  mutate(case_rate_per_1k=round(case_rate_per_1k, digits=1))

hosp_rate_by_age_us <- read_rds("clean_data/covid/hosp_rate_by_age_us.rds")

covid_rf_medical_conditions_tbl <- read_rds("clean_data/covid/covid_rf_medical_conditions_tbl.rds")

case_fatality_demographics <- read_rds("clean_data/covid/case_fatality_demographics.rds") %>% 
  mutate(group=str_to_title(group)) %>% 
  filter(group!='Population')

age_adjusted_deaths_by_race <- read_rds("clean_data/covid/age_adjusted_deaths_by_race.rds")

svi_death_rate_map <- read_rds("clean_data/covid/svi_death_rate_map.rds")

# text module ----
covid_overview_ui <- function(id) {
  
  fluidRow(
           tabBox(
             id = "tabset1", 
             side="left",
             width = 12,
             tabPanel(title="Overview", 
                      fluidRow(
                      column(width = 6,
                             h2("Overview"),
                             includeMarkdown("markdown/covid/covid.md")),
                      column(width = 6,
                             highcharter::highchartOutput(NS(id, "covid_case_rate_map"), height="650px")))),
             tabPanel("Prevention", 
                      fluidRow(
                        column(width = 12,
                               h2("Prevention"),
                               includeMarkdown("markdown/covid/covid_prevention.md")))),
             tabPanel("Risk Factors",  
                      fluidRow(
                        column(width = 6,
                               h2("COVID-19 & Age"),
                               includeMarkdown("markdown/covid/covid_rf_age.md")),
                        column(width = 6,
                               highcharter::highchartOutput(NS(id, "covid_hosp_rate_by_age_us")))),
                      hr(),
                      fluidRow(
                        column(width = 6,
                               h2("COVID-19 & Medical Conditions"),
                               includeMarkdown("markdown/covid/covid_rf_medical.md")),
                        column(width = 6,
                               includeMarkdown("markdown/covid/covid_rf_medical_top_right.md"),
                               gt_output(NS(id, "covid_covid_rf_medical_conditions_tbl")))),
                      hr(),
                      fluidRow(
                        column(width = 6,
                               h2("COVID-19 & Race/Ethnicity"),
                               includeMarkdown("markdown/covid/covid_rf_race.md")),
                        column(width = 6,
                               htmlOutput(NS(id, "covid_case_fatality_demographics")),
                               highcharter::highchartOutput(NS(id, "covid_age_adjusted_deaths_by_race")))),
                      hr(),
                      fluidRow(
                        column(width = 6,
                               h2("COVID-19 & Socioeconomic Factors"),
                               includeMarkdown("markdown/covid/covid_rf_social_economic.md")),
                        column(width = 6,
                               highcharter::highchartOutput(NS(id, "covid_svi_death_rate_map"), height="650px"))))
             )
           )
  
}

covid_overview_server <- function(id, df) {
  
  moduleServer(id, function(input, output, session) {
    
  output$covid_case_rate_map <- highcharter::renderHighchart({
      
    col_pal <- RColorBrewer::brewer.pal(9,"YlOrBr")
    
    hcmap(map = tx_map,
          data = case_rate_map,
          value = "case_rate_per_1k",
          joinBy = c("name","county"),
          name = "COVID-19 Case Rate",
          borderColor = "#FAFAFA",
          borderWidth = 0.1,
          tooltip = list(
            valueSuffix = " Per 1,000 Residents")) %>%
      hc_legend(layout='vertical',
                align='left',
                verticalAlign='bottom',
                itemMarginTop=10,
                itemMarginBottom=10) %>% 
      hc_colorAxis(stops = color_stops(n=8, colors=col_pal),
                   reversed=FALSE) %>%
      hc_title(text="COVID-19 Case Rate, Per 1,000 People") %>%
      hc_subtitle(text="Rate of The Population (Per 1,000 people) who have become infected with COVID-19 since March 16, 2020.") %>%
      hc_credits(
        enabled = TRUE,
        text = "SOURCE: Texas Department of Health Services",
        href = "https://www.dshs.state.tx.us/coronavirus/") %>%
      hc_add_theme(tx2036_hc_light())
      
    })
  
  output$covid_hosp_rate_by_age_us <- highcharter::renderHighchart({
    
    hosp_rate_by_age_us %>% 
      arrange(order) %>% 
      hchart("column", hcaes(x=age_category, y=cumulative_rate)) %>% 
      hc_title(text="COVID-19 Associated Hospitalizations, by Age Group") %>%
      hc_subtitle(text="Data shown reflect through the end of the week ending on October 10, 2020.") %>% 
      hc_xAxis(title=list(text="Age Group")) %>% 
      hc_yAxis(title=list(text="Hospitalizations per 100,000 population")) %>% 
      hc_credits(
        enabled = TRUE,
        text = "SOURCE: National Center for Health Statistics (NCHS) Mortality Reporting System",
        href = "https://gis.cdc.gov/grasp/COVIDNet/COVID19_3.html
        ") %>% 
      highcharter::hc_add_theme(texas2036::tx2036_hc_light())
    
  })
  
  output$covid_covid_rf_medical_conditions_tbl <- render_gt({
    
    covid_rf_medical_conditions_tbl %>% 
      mutate_at(vars(tx_rank), scales::label_ordinal()) %>% 
      gt() %>% 
      tab_source_note(
        source_note = md("[America's Health Rankings Analysis of CDC Behavioral Risk Factor Surveillance System](https://www.americashealthrankings.org/health-topics/subtag-92?topics=category-15,tag-121)")
      ) %>% 
      fmt_percent(columns = vars(tx_rate,us_rate),
                  decimals = 1,
                  scale_values = FALSE) %>% 
      cols_label(
        condition = " ",
        tx_rate = "TX (%)",
        us_rate = "US (%)",
        tx_rank = "Texas Rank") %>%
      tab_options(
        table.width = "95%",
        table.font.size = "18px",
        table_body.hlines.color = "white",
        table.border.top.color = "white",
        table.border.top.width = px(1),
        table.border.bottom.color = "white",
        table.border.bottom.width = px(1),
        table_body.border.top.width = px(3),
        table_body.border.top.color = "#3A4A9F",
        table_body.border.bottom.width = px(3),
        table_body.border.bottom.color = "#3A4A9F",
        column_labels.border.bottom.color = "black",
        column_labels.border.bottom.width = px(1)
      ) 
    
  })
  
  output$covid_case_fatality_demographics <- renderUI({
      
    covid_demographics_charts <- unique(case_fatality_demographics$group) 
      
      map(covid_demographics_charts,
          ~highchart() %>% 
            hc_add_series(data = case_fatality_demographics %>% filter(group==.x),
                          "pie", 
                          hcaes(name=race_ethnicity, y=percent, color = race_ethnicity)) %>% 
            hc_add_theme(texas2036::tx2036_hc_light()) %>% 
            hc_title(text = .x,
                     style = list(fontSize = "16px", useHTML = TRUE)) %>% 
            hc_yAxis(labels = list(format = '{value}%'),
                     title = list(text = "% of Current Adult Smokers")) %>% 
            hc_plotOptions(series=list(
              groupPadding = 0
            )) %>% 
            hc_xAxis(categories = c(" "),
                     tickColor = "#ffffff", 
                     min = 0,
                     max = 0,
                     maxPadding = 0,
                     endOnTick = TRUE,
                     startOnTick = TRUE,
                     useHTML = TRUE) %>% 
            hc_legend(align = "right",
                      reversed = FALSE,
                      verticalAlign = "top",
                      layout = "vertical",
                      x = 0,
                      y = 35,
                      itemStyle = list(fontSize="14px", textTransform="uppercase"),
                      floating = TRUE)) %>% 
        hw_grid(rowheight = 300, ncol = 2) 
      
    })
  
  output$covid_age_adjusted_deaths_by_race <- highcharter::renderHighchart({
    
    highchart() %>% 
      hc_add_series(data = age_adjusted_deaths_by_race %>% arrange(rate_per_100k),
                    "bar", 
                    hcaes(x=subgroup,y=rate_per_100k,
                          color = subgroup,
                          labels= subgroup),
                    name = "Age Adjusted Death Rate (Per 100k)") %>% 
      hc_plotOptions(series=list(
        groupPadding = .15
      )) %>% 
      hc_xAxis(reversedStacks = TRUE,
               labels = list(style = list(fontSize="16px")),
               categories = c("Asian",
                              "White",
                              "All Deaths",
                              "Indigenous",
                              "Black",
                              "Latino")) %>%
      hc_yAxis(labels = list(format = '{value}'),
               title = list(text= "Rate Per 100,000 Persons")) %>% 
      hc_title(text="Age Adjusted COVID-19 Death Rate (Per 100k Persons) in Texas, by Race & Ethnicity") %>% 
      hc_subtitle(text="Source: APM Research Lab. Data shown reflects deaths through Oct. 13, 2020.") %>% 
      hc_legend(enabled = FALSE) %>%
      highcharter::hc_add_theme(texas2036::tx2036_hc_light())
    
  })
  
  output$covid_svi_death_rate_map <- highcharter::renderHighchart({
    
    col_pal <- RColorBrewer::brewer.pal(9,"Blues")
    
    covid_death_rate <- svi_death_rate_map %>% 
      select(name=county,lat,lon,z=death_rate_per_1k) %>% 
      drop_na(lat)
    
    hcmap(map = tx_map,
          data = svi_death_rate_map,
          value = "svi_score",
          joinBy = c("name","county"),
          name = "Social Vulnerability Index Score",
          borderColor = "#fff",
          borderWidth = 0.5,
          tooltip = list(
            valueSuffix = "")) %>%
      hc_legend(layout='vertical',
                align='left',
                verticalAlign='bottom',
                itemMarginTop=10,
                itemMarginBottom=10) %>% 
      hc_colorAxis(stops = color_stops(n=8, colors=col_pal),
                   reversed=FALSE) %>%
      hc_title(text="CDC Social Vulnerability Index (SVI) Scores, by County") %>%
      hc_subtitle(text="The SVI uses 15 U.S. census variables to help local officials identify communities that may need support before, during, or after disasters.") %>%
      hc_add_theme(tx2036_hc_light())
    
  })
  
  output$covid_death_rate_map <- highcharter::renderHighchart({
    
    col_pal <- RColorBrewer::brewer.pal(9,"Purples")
    
    covid_death_rate <- svi_death_rate_map %>% 
      select(name=county,lat,lon,z=death_rate_per_1k) %>% 
      mutate(z=round(z,digits=1)) %>% 
      drop_na(lat)
    
    hcmap(map = tx_map,
          showInLegend = FALSE,
          nullColor="#fff") %>%
      hc_add_series(
        data = covid_death_rate, 
        showInLegend = TRUE,
        type = "mapbubble",
        borderColor = "#2d2d2d",
        color = "#3A4A9F",
        colorOpacity = 1,
        borderWidth = 0.75,
        name = "County Death Rate", 
        minSize = "0.1%",
        maxSize = "4%"
      ) %>% 
      hc_title(text="COVID-19 Death Rate, Per 1,000 Cases") %>%
      hc_subtitle(text="Rate of the infected population (Per 1,000 Infected Persons) who have died from COVID-19. A larger bubble size reflects a larger death rate.") %>%
      hc_add_theme(tx2036_hc_light())
    
  })
  
  outputOptions(output, "covid_case_rate_map", suspendWhenHidden = FALSE)
  
  outputOptions(output, "covid_hosp_rate_by_age_us", suspendWhenHidden = FALSE)
  
  outputOptions(output, "covid_covid_rf_medical_conditions_tbl", suspendWhenHidden = FALSE)
  
  outputOptions(output, "covid_case_fatality_demographics", suspendWhenHidden = FALSE)
  
  outputOptions(output, "covid_age_adjusted_deaths_by_race", suspendWhenHidden = FALSE)
  
  outputOptions(output, "covid_svi_death_rate_map", suspendWhenHidden = FALSE)
  
  outputOptions(output, "covid_death_rate_map", suspendWhenHidden = FALSE)
    
  })
  
}