diabetes_65_plus <- readr::read_rds("clean_data/Aging/risk_factors/aging_rf_obesity.rds")

# text module ----
aging_rf_ui <- function(id) {
  
  fluidRow(
           tabBox(
             title = "Risk Factors",
             id = "tabset1", 
             side="left",
             width = 12,
             # height = 600,
             tabPanel(title="Obesity", 
                      fluidRow(
                      column(width = 5,
                             h2("Obesity"),
                             includeMarkdown("markdown/aging/risk_factors/obesity.md")),
                      column(width = 7,
                             highcharter::highchartOutput(NS(id, "obesity_chart")))),
                      hr(),
                      fluidRow(
                        column(width = 12,
                               h2("Key Takeaways"),
                               includeMarkdown("markdown/aging/risk_factors/obesity.md"))
                      )),
             tabPanel(title="Physical Inactivity", 
                      fluidRow(
                        column(width = 5,
                               h2("Physical Inactivity"),
                               includeMarkdown("markdown/aging/risk_factors/physical_inactivity.md")),
                        column(width = 7,
                               highcharter::highchartOutput(NS(id, "phys_inactivity_chart")))),
                      hr(),
                      fluidRow(
                        column(width = 12,
                               h2("Key Takeaways"),
                               includeMarkdown("markdown/aging/risk_factors/physical_inactivity.md"))
                      )),
             tabPanel(title="Social Isolation", 
                      fluidRow(
                        column(width = 5,
                               h2("Social Isolation"),
                               includeMarkdown("markdown/aging/risk_factors/social_isolation.md")),
                        column(width = 7,
                               highcharter::highchartOutput(NS(id, "social_isolation_chart")))),
                      hr(),
                      fluidRow(
                        column(width = 12,
                               h2("Key Takeaways"),
                               includeMarkdown("markdown/aging/risk_factors/social_isolation.md"))
                      ))
             )
           )
  
}

aging_rf_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- ","
    options(highcharter.lang = hcoptslang)
    
    myMenuItems <- c("downloadCSV", "downloadXLS","separator", "downloadPNG", "downloadJPEG", "downloadPDF", "downloadSVG")
    
  output$obesity_chart <- highcharter::renderHighchart({
      
    diabetes_65_plus %>% 
      hchart("line", 
             hcaes(x=date, y=data_value),
             tooltip = list(pointFormat="<hr style='color:#3A4A9F;margin:2px 2px !important'><span style='font-weight:800;font-size:1.25em'>{point.y:.0f}%</span><span style='font-weight:400;font-size:1.25em'> of Texans Aged 65 years<br>& older reported being obese.</span>")) %>%
      hc_title(text="Obesity rates are climbing among older Texans",
               useHTML = TRUE) %>% 
      hc_subtitle(text="In the past six years, senior obesity in Texas increased 16% from 26.3% to 30.6% of adults ages 65+.",
                  useHTML = TRUE) %>%
      hc_credits(
        enabled = TRUE,
        text = "SOURCE: U.S. Centers for Disease Control and Prevention | DATA: Behavioral Risk Factor Surveillance System",
        href = "https://datalab.texas2036.org/bwhqgjc/behavioral-risk-factor-surveillance-system-brfss-prevalence-data?accesskey=xclhoac") %>%
      hc_yAxis(title = list(text = "Growth Rate From Previous Day"),
               format = "{value}%") %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(headerFormat = "<span style='font-size:1.25em;font-weight:800;text-transform:uppercase'>DURING {point.key:%Y}...</span><br>",
                 useHTML = TRUE) %>%
      highcharter::hc_add_theme(texas2036::tx2036_hc_light())
      
    })
  
  output$phys_inactivity_chart <- highcharter::renderHighchart({
    
    diabetes_65_plus %>% 
      hchart("line", hcaes(x=date, y=data_value)) %>% 
      hc_title(text="This is the title") %>% 
      highcharter::hc_add_theme(texas2036::tx2036_hc_light())
    
  })
  
  output$social_isolation_chart <- highcharter::renderHighchart({
    
    diabetes_65_plus %>% 
      hchart("line", hcaes(x=date, y=data_value)) %>% 
      hc_title(text="This is the title") %>% 
      highcharter::hc_add_theme(texas2036::tx2036_hc_light())
    
  })
    
  })
  
}