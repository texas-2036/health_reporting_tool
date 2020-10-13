# text module ----
childhood_overview_ui <- function(id) {
  
  fluidRow(
           tabBox(
             title = "Overview",
             id = "tabset1", 
             side="left",
             width = 12,
             tabPanel(title="Obesity", 
                      fluidRow(
                      column(width = 6,
                             h2("Obesity"),
                             includeMarkdown("markdown/aging/risk_factors/obesity.md")),
                      column(width = 6,
                             highcharter::highchartOutput(NS(id, "obesity_chart"))))),
             tabPanel("Physical Inactivity", 
                      fluidRow(
                        column(width = 6,
                               h2("Physical Inactivity"),
                               includeMarkdown("markdown/aging/risk_factors/physical_inactivity.md")),
                        column(width = 6,
                               highcharter::highchartOutput(NS(id, "phys_inactivity_chart"))))),
             tabPanel("Social Isolation",  
                      fluidRow(
                        column(width = 6,
                               h2("Social Isolation"),
                               includeMarkdown("markdown/aging/risk_factors/social_isolation.md")),
                        column(width = 6,
                               highcharter::highchartOutput(NS(id, "social_isolation_chart")))))
             )
           )
  
}

childhood_overview_server <- function(id, df) {
  
  moduleServer(id, function(input, output, session) {
    
  output$obesity_chart <- highcharter::renderHighchart({
      
      df %>% 
      hchart("line", hcaes(x=year, y=x75_percentage)) %>% 
      hc_title(text="This is the title") %>% 
      highcharter::hc_add_theme(texas2036::tx2036_hc_light())
      
    })
  
  output$phys_inactivity_chart <- highcharter::renderHighchart({
    
    df %>% 
      hchart("line", hcaes(x=year, y=x75_percentage)) %>% 
      hc_title(text="This is the title") %>% 
      highcharter::hc_add_theme(texas2036::tx2036_hc_light())
    
  })
  
  output$social_isolation_chart <- highcharter::renderHighchart({
    
    df %>% 
      hchart("line", hcaes(x=year, y=x75_percentage)) %>% 
      hc_title(text="This is the title") %>% 
      highcharter::hc_add_theme(texas2036::tx2036_hc_light())
    
  })
    
  })
  
}