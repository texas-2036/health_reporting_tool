# Load Data -----------------------------------------------------------------------------------

diabetes_chart_wk <- readr::read_rds("clean_data/Working Age/conditions/diabetes_chart_wk.rds")

mh_chart_ts_wk <- readr::read_rds("clean_data/Working Age/conditions/mh_chart_ts_wk.rds")

mh_chart_bar_wk <- readr::read_rds("clean_data/Working Age/conditions/mh_chart_bar_wk.rds")

hh_map_wk <- readr::read_rds("clean_data/Working Age/conditions/heart_health.rds")  

# text module ----
working_cnd_ui <- function(id) {

  fluidRow(
           tabBox(
             # title = "Conditions",
             id = "tabset1", 
             side="left",
             width = 12,
             tabPanel(title="Diabetes", 
                      fluidRow(
                        column(width = 5,
                               h2("Diabetes"),
                               includeMarkdown("markdown/working_age/conditions/diabetes.md")),
                        column(width = 7,
                               highcharter::highchartOutput(NS(id, "diabetes_chart"))))),
             tabPanel(title="Mental Health", 
                      fluidRow(
                        column(width = 6,
                               h2("Mental Health"),
                               includeMarkdown("markdown/working_age/conditions/mental_health.md")),
                        column(width = 6,
                               highcharter::highchartOutput(NS(id, "mh_chart_ts")),
                               htmlOutput(NS(id, "mh_chart_bar"))))),
             tabPanel(title="Heart Health",  
                      fluidRow(
                        column(width = 6,
                               h2("Heart Health"),
                               includeMarkdown("markdown/working_age/conditions/heart_health.md")),
                        column(width = 6,
                               highcharter::highchartOutput(NS(id, "hh_map"), height="650px"))))
             )
           )
  
}

working_cnd_server <- function(id, df) {
  
  moduleServer(id, function(input, output, session) {
    
  output$diabetes_chart <- highcharter::renderHighchart({
      
    highchart() %>% 
      hc_add_series(diabetes_chart_wk %>% filter(state_name!="Texas"), 
                    type="line", 
                    hcaes(x=edition, y=value, group=state_name), 
                    color="#DBDCDD") %>% 
      hc_add_series(diabetes_chart_wk %>% filter(state_name=="Texas"),
                    type="line", 
                    hcaes(x=edition, y=value),
                    lineWidth=5,
                    name="Texas") %>% 
      hc_title(text="Diagnosed Diabetes Among Adults") %>%
      hc_subtitle(text="Diabetes Trends Among Adults Aged 18+ in Texas and Peer States identified by Texas 2036.") %>%
      hc_yAxis(title=list(text="% of Adults Reporting Diabetes Diagnosis"),
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
        text = "America's Health Rankings analysis of CDC, Behavioral Risk Factor Surveillance System.",
        href = "https://datalab.texas2036.org/mskvxdg/america-s-health-rankings-annual-report?accesskey=hujmlqb") %>%
      hc_add_theme(tx2036_hc_light())
      
    })
  
  output$mh_chart_ts <- highcharter::renderHighchart({
    
    highchart() %>% 
      hc_add_series(mh_chart_ts_wk %>% filter(state_name!="Texas"), 
                    type="line", 
                    hcaes(x=edition, y=value, group=state_name), 
                    color="#DBDCDD") %>% 
      hc_add_series(mh_chart_ts_wk %>% filter(state_name=="Texas"),
                    type="line", 
                    hcaes(x=edition, y=value),
                    lineWidth=5,
                    name="Texas") %>% 
      hc_title(text="Frequent Mental Distress Among Adults") %>%
      hc_subtitle(text="Frequent Mental Distress Trends (14 or more poor mental health days in the past 30 days) Among Adults in Texas and Peer States identified by Texas 2036.") %>%
      hc_yAxis(title=list(text="% of Adults Reporting Frequent Mental Distress"),
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
        text = "America's Health Rankings analysis of CDC, Behavioral Risk Factor Surveillance System.",
        href = "https://datalab.texas2036.org/mskvxdg/america-s-health-rankings-annual-report?accesskey=ghvskpe") %>%
      hc_add_theme(tx2036_hc_light())
    
  })
  
  output$mh_chart_bar <- renderUI({
    
    mh_wk_charts <- unique(mh_chart_bar_wk$measure_name) 
    
    map(mh_wk_charts,
        ~highchart() %>% 
          hc_add_series(data = mh_chart_bar_wk %>% filter(measure_name==.x),
                        "bar", 
                        hcaes(x=measure_name ,y=value, 
                              group = state_abbr,
                              labels=state_abbr)) %>% 
          hc_add_theme(texas2036::tx2036_hc_light()) %>% 
          hc_title(text = .x,
                   style = list(fontSize = "16px", useHTML = TRUE)) %>% 
          hc_yAxis(labels = list(format = '{value}%'),
                   title = list(text = "% of Demographic Reporting Frequent Mental Distress")) %>% 
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
      hw_grid(rowheight = 175, ncol = 1) 
    
  })
  
  output$hh_map <- highcharter::renderHighchart({
    
    hh_map_wk$display_name <- gsub(' County', '', hh_map_wk$display_name)
    
    
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- ","
    options(highcharter.lang = hcoptslang)
    
    col_pal <- RColorBrewer::brewer.pal(9,"RdPu")
    
    hcmap(map = "countries/us/us-tx-all",
          data = hh_map_wk,
          value = "Value",
          joinBy = c("name","display_name"),
          name = "Death Rate Per 100,000",
          borderColor = "#FAFAFA",
          borderWidth = 0.1,
          tooltip = list(
            valueDecimals = 2,
            valueSuffix = " Deaths Per 100,000 People")) %>% 
      hc_legend(layout='vertical',
                align='left',
                verticalAlign='bottom',
                itemMarginTop=10,
                itemMarginBottom=10) %>% 
      hc_colorAxis(stops = color_stops(n=8, colors=col_pal),
                   min = 80,
                   max = 315,
                   reversed=FALSE) %>%
      hc_credits(
        enabled = TRUE,
        useHTML = TRUE,
        text = "SOURCE: Interactive Atlas of Heart Disease and Stroke, by Center for Disease Control and Prevention.",
        href = "https://www.cdc.gov/dhdsp/maps/atlas/index.html") %>%
      hc_title(text="Cardiovascular Disease Deaths, Rate Per 100,000, 2018-2018") %>% 
      hc_subtitle(text="The chart below shows the combined rate of deaths from all cardiovascular related diseases in Texas. Represented in the data are adults aged 45-64 of all races and genders.") %>% 
      highcharter::hc_add_theme(texas2036::tx2036_hc_light())
    
  })
    
  })
  
}