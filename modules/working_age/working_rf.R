# Load Data ---------------------------------------------------------------------------------
#us_map <- "https://code.highcharts.com/mapdata/countries/us/us-all.js"
tx_map <- "https://code.highcharts.com/mapdata/countries/us/us-tx-all.js"
obesity_map_wk <- readr::read_rds("clean_data/Working Age/risk_factors/obesity_county_map.rds")

obesity_chart_wk <- readr::read_rds("clean_data/Working Age/risk_factors/obesity_chart.rds")
 
phys_inactivity_chart_wk <- readr::read_rds("clean_data/Working Age/risk_factors/phys_inactivity_chart_wk.rds")

smoking_map_wk <- readr::read_rds("clean_data/Working Age/risk_factors/smoking_map_wk.rds")

smoking_chart_wk <- readr::read_rds("clean_data/Working Age/risk_factors/smoking_chart_wk.rds") %>% 
  mutate(measure_name=str_remove(measure_name, "Smoking - "))


# text module ----
working_rf_ui <- function(id) {
  
  fluidRow(
           tabBox(
             title = NULL,
             id = "tabset1", 
             side="left",
             width = 12,
             # height = 600,
             tabPanel(title="Obesity", 
                      fluidRow(
                      column(width = 5,
                             h2("Obesity"),
                             includeMarkdown("markdown/working_age/risk_factors/obesity_top.md")),
                      column(width = 7,
                             highcharter::highchartOutput(NS(id, "obesity_map"), height="650px"))),
                      hr(),
                      fluidRow(
                        column(width = 7,
                               highcharter::highchartOutput(NS(id, "obesity_chart")),
                               includeMarkdown("markdown/working_age/risk_factors/obesity_bottom_left.md")),
                        column(width = 5,
                               h2("Obesity Costs"),
                               includeMarkdown("markdown/working_age/risk_factors/obesity_bottom_right.md")
                               ))),
             tabPanel(title="Physical Inactivity", 
                      fluidRow(
                        column(width = 5,
                               h2("Physical Inactivity"),
                               includeMarkdown("markdown/working_age/risk_factors/physical_inactivity.md"),
                               highcharter::highchartOutput(NS(id, "phys_inactivity_chart"))),
                        column(width = 7,
                               highcharter::highchartOutput(NS(id, "phys_inactivity_map"), height="650px")))),
             tabPanel(title="Smoking", 
                      fluidRow(
                        column(width = 5,
                               h2("Smoking"),
                               includeMarkdown("markdown/working_age/risk_factors/smoking_left_top.md"),
                               htmlOutput(NS(id, "smoking_chart")),
                               includeMarkdown("markdown/working_age/risk_factors/smoking_left_bottom.md")),
                        column(width = 7,
                               highcharter::highchartOutput(NS(id, "smoking_map"), height = "700px"))),
                      hr(),
                      fluidRow(
                        column(width = 12,
                               h2("Cost of Smoking-Related Illnesses"),
                               includeMarkdown("markdown/working_age/risk_factors/smoking_bottom_headers.md"))
                      ),
                      fluidRow(
                        column(width = 6,
                               h3("Evidence Based Interventions^[3]"),
                               includeMarkdown("markdown/working_age/risk_factors/smoking_bottom_evidence.md")),
                        column(width = 6,
                               h3("Associated Cost Savings^[3]"),
                               includeMarkdown("markdown/working_age/risk_factors/smoking_bottom_cost_savings.md"))))
             )
           )
  
}

working_rf_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- ","
    options(highcharter.lang = hcoptslang)
    
    myMenuItems <- c("downloadCSV", "downloadXLS","separator", "downloadPNG", "downloadJPEG", "downloadPDF", "downloadSVG")
    
  output$obesity_map <- highcharter::renderHighchart({
      
      col_pal <- RColorBrewer::brewer.pal(9,"Blues")
      
      hcmap(map = tx_map,
            data = obesity_map_wk,
            value = "percentage",
            joinBy = c("name","county"),
            name = "%",
            borderColor = "#FAFAFA",
            borderWidth = 0.1,
            tooltip = list(
              valueDecimals = 2,
              valueSuffix = "%")) %>% 
        hc_legend(layout='vertical',
                  align='left',
                  verticalAlign='bottom',
                  itemMarginTop=10,
                  itemMarginBottom=10) %>% 
        hc_colorAxis(stops = color_stops(n=8, colors=col_pal),
                     reversed=FALSE) %>%
        hc_credits(
          enabled = TRUE,
          text = "SOURCE: CDC Interactive Diabetes Atlas | DATA: CDC Interactive Diabetes Atlas",
          href = "https://gis.cdc.gov/grasp/diabetes/DiabetesAtlas.html#") %>%
        hc_title(text="Obesity Prevalence Among Texas Adults, 2017") %>% 
        hc_subtitle(text="Age-adjusted percentage of adults age 20 and older who are obese.") %>% 
        highcharter::hc_add_theme(texas2036::tx2036_hc_light())
      
    })
  
  output$obesity_chart <- highcharter::renderHighchart({
    
    highchart() %>% 
      hc_add_series(obesity_chart_wk %>% filter(state_name!="Texas"), 
                    type="line", 
                    hcaes(x=edition, y=value, group=state_name), 
                    color="#DBDCDD") %>% 
      hc_add_series(obesity_chart_wk %>% filter(state_name=="Texas"),
                    type="line", 
                    hcaes(x=edition, y=value),
                    lineWidth=5,
                    name="Texas") %>% 
      hc_title(text="Obesity Trends Among Adults") %>%
      hc_subtitle(text="% of adults with a body mass index (BMI) of 30.0 or higher based on reported height and weight in Texas and Peer States identified by Texas 2036.") %>%
      hc_yAxis(title=list(text="% of Adults with BMI of 30.0 or Higher"),
               labels = list(enabled=TRUE,
                             format = "{value}%")) %>% 
      hc_xAxis(tickColor = "#ffffff", 
               min = 0.5,
               max = 6.5,
               tickInterval = 1,
               maxPadding = 0,
               endOnTick = FALSE,
               startOnTick = FALSE,
               useHTML = TRUE,
               alternateGridColor = "#f3f3f3",
               categories = c("2012","2013","2014","2015","2016","2017","2018","2019"),
               title = list(text = "Year of America's Health Ranking Report")) %>%
      hc_legend(layout = "proximate", align = "right") %>% 
      hc_credits(
        enabled = TRUE,
        text = "America's Health Rankings analysis of CDC, Behavioral Risk Factor Surveillance System, United Health Foundation.",
        href = "https://datalab.texas2036.org/mskvxdg/america-s-health-rankings-annual-report?accesskey=jjobgc") %>%
      hc_add_theme(tx2036_hc_light())
    
  })
  
  output$phys_inactivity_map <- highcharter::renderHighchart({
    
    col_pal <- RColorBrewer::brewer.pal(9,"Greens")
    
    hcmap(map = tx_map,
          data = phys_inactivity_map_df,
          value = "value",
          joinBy = c("name","county"),
          name = "Percent Inactive",
          borderColor = "#FAFAFA",
          borderWidth = 0.1,
          tooltip = list(
            valueDecimals = 2,
            valueSuffix = "%")) %>% 
      hc_legend(layout='vertical',
                align='left',
                verticalAlign='bottom',
                itemMarginTop=10,
                itemMarginBottom=10) %>% 
      hc_colorAxis(stops = color_stops(n=8, colors=col_pal),
                   reversed=FALSE) %>%
      hc_credits(
        enabled = TRUE,
        text = "SOURCE: County Health Rankings Analysis | DATA: CDC Interactive Diabetes Atlas",
        href = "https://www.countyhealthrankings.org/app/texas/2020/measure/factors/70/map") %>%
      hc_title(text="Adult Texans Reporting No Leisure-time Physical Activity, 2016") %>% 
      hc_subtitle(text="Shown: Percentage of adults age 20 and over.") %>% 
      highcharter::hc_add_theme(texas2036::tx2036_hc_light())
    
  })
  
  output$phys_inactivity_chart <- highcharter::renderHighchart({
    
    highchart() %>% 
      hc_add_series(phys_inactivity_chart_wk %>% filter(state_name!="Texas"), 
                    type="line", 
                    hcaes(x=edition, y=value, group=state_name), 
                    color="#DBDCDD") %>% 
      hc_add_series(phys_inactivity_chart_wk %>% filter(state_name=="Texas"),
                    type="line", 
                    hcaes(x=edition, y=value),
                    lineWidth=5,
                    name="Texas") %>% 
      hc_title(text="Physical Inactivity Trends Among Adults") %>%
      hc_subtitle(text="Physical Inactivity Trends Among Adults in Texas and Peer States identified by Texas 2036.") %>%
      hc_yAxis(title=list(text="% of Adults Reporting Physical Inactivity"),
               labels = list(enabled=TRUE,
                             format = "{value}%")) %>% 
      hc_xAxis(tickColor = "#ffffff", 
               min = 0.5,
               max = 6.5,
               tickInterval = 1,
               maxPadding = 0,
               endOnTick = FALSE,
               startOnTick = FALSE,
               useHTML = TRUE,
               alternateGridColor = "#f3f3f3",
               categories = c("2012","2013","2014","2015","2016","2017","2018","2019"),
               title = list(text = "Year of America's Health Ranking Report")) %>%
      hc_legend(layout = "proximate", align = "right") %>% 
      hc_credits(
        enabled = TRUE,
        text = "America's Health Rankings analysis of CDC, Behavioral Risk Factor Surveillance System, United Health Foundation, AmericasHealthRankings.org, Accessed 2020.",
        href = "https://datalab.texas2036.org/mskvxdg/america-s-health-rankings-annual-report?accesskey=lgiytld") %>%
      hc_add_theme(tx2036_hc_light())
    
  })
  
  output$smoking_chart <- renderUI({
    
    smoking_charts <- unique(smoking_chart_wk$measure_name) 
    
    map(smoking_charts,
        ~highchart() %>% 
          hc_add_series(data = smoking_chart_wk %>% filter(measure_name==.x),
                        "bar", 
                        hcaes(x=measure_name ,y=value, 
                              group = state_abbr,
                              labels=state_abbr)) %>% 
          hc_add_theme(texas2036::tx2036_hc_light()) %>% 
          hc_title(text = .x,
                   style = list(fontSize = "16px", useHTML = TRUE)) %>% 
          hc_yAxis(labels = list(format = '{value}%'),
                   title = list(text = "% of demographic who are Current Adult Smokers")) %>% 
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
      hw_grid(rowheight = 140, ncol = 1) 
    
  })
  
  output$smoking_map <- highcharter::renderHighchart({
    
    col_pal <- RColorBrewer::brewer.pal(9,"Reds")
    
    hcmap(map = tx_map,
          data = smoking_map_wk,
          value = "value",
          joinBy = c("name","county"),
          name = "% of Adults Who Are Current Smokers",
          borderColor = "#FAFAFA",
          borderWidth = 0.1,
          tooltip = list(
            valueDecimals = 2,
            valueSuffix = "%")) %>% 
      hc_legend(layout='vertical',
                align='left',
                verticalAlign='bottom',
                itemMarginTop=10,
                itemMarginBottom=10) %>% 
      hc_colorAxis(stops = color_stops(n=8, colors=col_pal),
                   reversed=FALSE) %>%
      hc_credits(
        enabled = TRUE,
        text = "SOURCE: County Health Rankings Analysis of CDC Behavioral Risk Factor Surveillance System",
        href = "https://www.countyhealthrankings.org/app/texas/2020/measure/factors/9/map") %>%
      hc_title(text="Smoking Prevalence Among Adult Texans, 2017") %>% 
      hc_subtitle(text="According to County Health Ranking, Adult Smokers is defined as the percentage of the adult population in a county who both report that they currently smoke every day or most days and have smoked at least 100 cigarettes in their lifetime.") %>% 
      highcharter::hc_add_theme(texas2036::tx2036_hc_light())
    
  })
  
  outputOptions(output, "obesity_map", suspendWhenHidden = FALSE)
  
  outputOptions(output, "obesity_chart", suspendWhenHidden = FALSE)
  
  outputOptions(output, "phys_inactivity_map", suspendWhenHidden = FALSE)
  
  outputOptions(output, "phys_inactivity_chart", suspendWhenHidden = FALSE)
  
  outputOptions(output, "smoking_chart", suspendWhenHidden = FALSE)
  
  outputOptions(output, "smoking_map", suspendWhenHidden = FALSE)
    
  })
  
}