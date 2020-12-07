# Load Data -----------------------------------------------------------------------------------

diabetes_trends_women_18_44 <- read_rds("clean_data/maternity/diabetes_trends_women_18_44.rds")


# text module ----
maternity_cnd_ui <- function(id) {
  
  fluidRow(
           tabBox(
             ##title = "Risk Factors",
             id = "tabset1", 
             side="left",
             width = 12,
             tabPanel(title="Diabetes", 
                      fluidRow(
                      column(width = 6,
                             h2("Obesity"),
                             includeMarkdown("markdown/aging/risk_factors/obesity.md")),
                      column(width = 6))),
             tabPanel("Heart Health", 
                      h2("Obesity")),
             tabPanel("Mental Health",  
                      h2("obesity"))
             )
           )
  
}

maternity_cnd_server <- function(id, df) {
  
  moduleServer(id, function(input, output, session) {
    
    
  })
  
}