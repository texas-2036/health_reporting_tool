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
                             h2("Diabetes and Gestational Diabetes"),
                             includeMarkdown("markdown/maternity/conditions/diabetes.md")),
                      column(width = 6,
                             h2("Diabetes Rates Among Women Ages 18-44"),
                             )),
                      fluidRow(
                        column(width = 6,
                               includeMarkdown("markdown/maternity/conditions/diabetes_graphic_callout.md")),
                        column(width = 6,
                               h2("Diabetes Rates Among Women Ages 18-44"),
                               includeMarkdown("markdown/maternity/conditions/diabetes_top_right.md")
                              )),
                      
                      ),
             tabPanel(title = "Heart Health", 
                      fluidRow(
               column(width = 6,
                      h2("High Blood Pressure"),
                      includeMarkdown("markdown/maternity/conditions/high_blood_pressure.md")),
               column(width = 6,
                      includeMarkdown("markdown/maternity/conditions/high_blood_pressure_top_right.md")
               )),
               fluidRow(
                 column(width = 6,
                        h2("Preeclampsia"),
                        includeMarkdown("markdown/maternity/conditions/preeclampsia.md")),
                 column(width = 6
                 ))
               ), 
             tabPanel(title = "Mental Health",  
                      fluidRow(
                        column(width = 6,
                               h2("Depression During Pregnancy"),
                               includeMarkdown("markdown/maternity/conditions/depression.md")),
                        column(width = 6,
                               h2("Treating Depression During Pregnancy"),
                               includeMarkdown("markdown/maternity/conditions/treating_depression.md")
                        )),
                      fluidRow(
                        column(width = 6,
                               h2("Postpartum Depression"),
                               includeMarkdown("markdown/maternity/conditions/postpartum_depression.md")),
                        column(width = 6,
                               includeMarkdown("markdown/maternity/conditions/postpartum_depression_bottom_right.md")
                        ))
                      
                      )
             )
           )
  
}

maternity_cnd_server <- function(id, df) {
  
  moduleServer(id, function(input, output, session) {
    
    
  })
  
}