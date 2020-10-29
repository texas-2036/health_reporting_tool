# text module ----
aging_pol_ui <- function(id) {
  
  fluidRow(
           tabBox(
             title = "Risk Factors",
             id = "tabset1", 
             side="left",
             width = 12,
             tabPanel(title="Obesity", 
                      fluidRow(
                      column(width = 6,
                             h2("Obesity"),
                             includeMarkdown("markdown/aging/risk_factors/obesity.md")),
                      column(width = 6,
                             highcharter::highchartOutput(NS(id, "diabetes_chart"))))),
             tabPanel("Physical Inactivity", 
                      h2("Obesity")),
             tabPanel("Smoking",  
                      h2("obesity"))
             )
           )
  
}

aging_pol_server <- function(id, df) {
  
  moduleServer(id, function(input, output, session) {
    
  output$diabetes_chart <- highcharter::renderHighchart({
      
      df %>% 
      hchart("line", hcaes(x=year, y=x75_percentage)) %>% 
      hc_title(text="This is the title") %>% 
      highcharter::hc_add_theme(texas2036::tx2036_hc_light())
      
    })
    
  })
  
}