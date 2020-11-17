
# Load Data -----------------------------------------------------------------------------------

diabetes <- read_rds("clean_data/Aging/conditions/diabetes_data.rds") %>% 
  filter(peer_states=="Peer State")

mental_health <- read_rds("clean_data/Aging/conditions/mental_distress.rds") %>% 
  filter(peer_states=="Peer State")

heart_health <- read_rds("clean_data/Aging/conditions/heart_health.rds")

alzheimers <- read_rds("clean_data/Aging/conditions/alzheimers_data.rds") %>% 
  filter(year=="2018")

# text module ----
aging_cnd_ui <- function(id) {
  
  fluidRow(
           tabBox(
             title = NULL,
             id = "tabset1", 
             side="left",
             width = 12,
             tabPanel(title="Diabetes", 
                      fluidRow(
                      column(width = 5,
                             h2("Diabetes"),
                             includeMarkdown("markdown/aging/conditions/diabetes.md")),
                      column(width = 7,
                             highcharter::highchartOutput(NS(id, "diabetes_chart"), height="550px")))),
             tabPanel("Mental Health", 
                      fluidRow(
                        column(width = 5,
                               h2("Mental Health"),
                               includeMarkdown("markdown/aging/conditions/mental_health.md")),
                        column(width = 7,
                               highcharter::highchartOutput(NS(id, "mental_health_chart"), height="550px")))),
             tabPanel("Heart Health", 
                      fluidRow(
                        column(width = 6,
                               h2("Heart Health"),
                               includeMarkdown("markdown/aging/conditions/heart_health.md")),
                        column(width = 6,
                               highcharter::highchartOutput(NS(id, "heart_health_chart"), height="550px")))),
             tabPanel("Alzheimer's",  
                      fluidRow(
                        column(width = 12,
                               h2("Alzheimer's"),
                               includeMarkdown("markdown/aging/conditions/alzheimers_top.md"))),
                      fluidRow(
                        column(width = 6,
                               highcharter::highchartOutput(NS(id, "alzheimers_chart"), height="550px")),
                        column(width = 6,
                               includeMarkdown("markdown/aging/conditions/alzheimers_right.md"))))
             )
           )
  
}

aging_cnd_server <- function(id, df) {
  
  moduleServer(id, function(input, output, session) {
    
  output$diabetes_chart <- highcharter::renderHighchart({
      
    highchart() %>% 
      hc_add_series(diabetes %>% filter(states!="Texas"), 
                    type="line", 
                    hcaes(x=edition, y=value, group=states), 
                    color="#DBDCDD") %>% 
      hc_add_series(diabetes %>% filter(states=="Texas"),
                    type="line", 
                    hcaes(x=edition, y=value),
                    lineWidth=5,
                    name="Texas") %>% 
      hc_title(text="Diabetes Prevalence Among Seniors") %>%
      hc_subtitle(text="Diabetes Trends Among Seniors in Texas and Peer States identified by Texas 2036. Lines in Grey are Peer States") %>%
      hc_yAxis(title=list(text="% of Seniors with Diabetes"),
               labels = list(enabled=TRUE,
                             format = "{value}%")) %>% 
      hc_xAxis(tickColor = "#ffffff", 
               # opposite = TRUE,
               useHTML = TRUE,
               alternateGridColor = "#f3f3f3",
               categories = c("2013","2014","2015","2016","2017","2018","2019","2020"),
               title = list(text = "Year of America's Health Ranking Report")) %>%
      hc_credits(
        enabled = TRUE,
        text = "America's Health Rankings analysis of CDC, Behavioral Risk Factor Surveillance System.",
        href = "https://datalab.texas2036.org/mskvxdg/america-s-health-rankings-annual-report?accesskey=lauxjy") %>%
      hc_add_theme(tx2036_hc_light())
      
    })
  
  output$mental_health_chart <- highcharter::renderHighchart({
    
    highchart() %>% 
      hc_add_series(mental_health %>% filter(states!="Texas"), 
                    type="line", 
                    hcaes(x=edition, y=value, group=states), 
                    color="#DBDCDD") %>% 
      hc_add_series(mental_health %>% filter(states=="Texas"),
                    type="line", 
                    hcaes(x=edition, y=value),
                    lineWidth=5,
                    name="Texas") %>% 
      hc_title(text="Frequent Mental Distress Among Seniors") %>%
      hc_subtitle(text="Frequent Mental Distress Trends Among Seniors in Texas and Peer States identified by Texas 2036.") %>%
      hc_yAxis(title=list(text="% of Seniors Reporting Frequent Mental Distress"),
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
               categories = c("2013","2014","2015","2016","2017","2018","2019", "2020"),
               title = list(text = "Year of America's Health Ranking Report")) %>%
      hc_legend(layout = "proximate", align = "right") %>% 
      hc_credits(
        enabled = TRUE,
        text = "America's Health Rankings analysis of CDC, Behavioral Risk Factor Surveillance System.",
        href = "https://datalab.texas2036.org/mskvxdg/america-s-health-rankings-annual-report?accesskey=ccfaajf") %>%
      hc_add_theme(tx2036_hc_light())
    
  })
  
  output$heart_health_chart <- highcharter::renderHighchart({
    
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- ","
    options(highcharter.lang = hcoptslang)
    
    col_pal <- RColorBrewer::brewer.pal(9,"Reds")
    
    hcmap(map = "countries/us/us-all",
          data = heart_health,
          value = "value",
          joinBy = c("name","state_name"),
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
                   reversed=FALSE) %>%
      hc_credits(
        enabled = TRUE,
        useHTML = TRUE,
        text = "SOURCE: Interactive Atlas of Heart Disease and Stroke, by Centers for Disease Control and Prevention.",
        href = "https://nccd.cdc.gov/DHDSPAtlas/?state=State") %>%
      hc_title(text="Cardiovascular Disease Deaths, Rate Per 100,000") %>% 
      hc_subtitle(text="The chart below shows the combined rate of deaths from all cardiovascular related diseases in the United States. Represented in the data are adults aged 65+ of all races and genders.") %>% 
      highcharter::hc_add_theme(texas2036::tx2036_hc_light())
    
  })
  
  output$alzheimers_chart <- highcharter::renderHighchart({
    
    col_pal <- RColorBrewer::brewer.pal(9,"Purples")
    
    hcmap(map = "countries/us/us-all",
          data = alzheimers,
          value = "rate",
          joinBy = c("hc-a2","state"),
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
                   reversed=FALSE) %>%
      hc_credits(
        enabled = TRUE,
        useHTML = TRUE,
        text = "SOURCE: Alzheimer's Disease Mortality by State, by Center for Disease Control and Prevention.",
        href = "https://www.cdc.gov/nchs/pressroom/sosmap/alzheimers_mortality/alzheimers_disease.htm") %>%
      hc_title(text="Alzheimer's Disease Mortality, Rate Per 100,000") %>% 
      hc_subtitle(text="The chart below shows the age-adjusted mortality rate of deaths from Alzheimer's diseases in the United States.") %>% 
      highcharter::hc_add_theme(texas2036::tx2036_hc_light())
    
  })
  
  outputOptions(output, "mental_health_chart", suspendWhenHidden = FALSE)
  
  outputOptions(output, "heart_health_chart", suspendWhenHidden = FALSE)
  
  outputOptions(output, "alzheimers_chart", suspendWhenHidden = FALSE)
  
  })
  
}