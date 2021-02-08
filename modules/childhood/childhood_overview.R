
# Load Data -----------------------------------------------------------------------------------

change_in_childhood_pop <- read_rds("clean_data/Children/change_in_childhood_pop.rds")

# text module ----
childhood_overview_ui <- function(id) {
  
         box(side="left",
             width = 12,
             fluidRow(
               column(width = 12,
                      column(width = 6,
                             h2("Childhood/Adolescence in Texas"),
                             includeMarkdown("markdown/childhood/overview/overview.md")),
                      column(width = 6,
                             highcharter::highchartOutput(NS(id, "childhood_overview_chart"), height="500px")))),
             hr(),
             fluidRow(
               column(width = 12,
                      h2("Key Takeaways"),
                      includeMarkdown("markdown/childhood/overview/key_takeaways.md"))
             )
             )
  
}

childhood_overview_server <- function(id, df) {
  
  moduleServer(id, function(input, output, session) {
    
  output$childhood_overview_chart <- highcharter::renderHighchart({
      
    change_in_childhood_pop %>% 
      hchart("bar", hcaes(x=state_name, y=value),
             negativeColor = "#F26852") %>% 
      hc_title(text="Change in the Under-18 Population in The Six Largest States") %>% 
      hc_subtitle(text="Between 2010-2018") %>% 
      highcharter::hc_add_theme(texas2036::tx2036_hc_light()) %>% 
      hc_yAxis(title=list(text="% of Change in Population Between 2010 & 2018"),
               labels = list(enabled=TRUE,
                             format = "{value}%")) %>% 
      hc_xAxis(title = list(enabled = FALSE)) %>% 
      hc_credits(
        enabled = TRUE,
        text = "Young Texans: Demographic Overview | Fiscal Notes, Texas Comptroller of Public Accounts",
        href = "https://comptroller.texas.gov/economy/fiscal-notes/2020/feb/texans.php")
      
    })
  
  outputOptions(output, "childhood_overview_chart", suspendWhenHidden = FALSE)
  
  })
  
}