# load data ----------

##this section will be where all the data for this page will be loaded once that data is collected

# text module ----
maternity_pol_ui <- function(id) {
  
  fluidRow(
           tabBox(
             id = "tabset1", 
             side="left",
             width = 12,
             tabPanel(title="Uninsurance", 
                      fluidRow(
                      column(width = 6,
                             h2("Uninsurance Among Women of Child-bearing age"),
                             includeMarkdown("markdown/maternity/policy/uninsurance.md"))
                      )),
             tabPanel(title = "Provider Access",
                      fluidRow(
                      column(width = 6,
                      h2("Access to Obstetrics and Gynecology"),
                      includeMarkdown("markdown/maternity/policy/access.md"))
                      )),
             tabPanel(title = "Prevention",
                      fluidRow(
                      column(width = 6,
                             h2("Prenatal Care"),
                             includeMarkdown("markdown/maternity/policy/prenatal_care.md")
                             )  
                      ),
                      fluidRow(
                        column(width = 6,
                               h2("Prenatal Care by Race/Ethnicity"),
                               includeMarkdown("markdown/maternity/policy/prenatal_care_race.md")
                        )  
                      ),
                      fluidRow(
                        column(width = 6,
                               h2("Preterm Birth"),
                               includeMarkdown("markdown/maternity/policy/preterm_birth.md")
                        )  
                      ),
                      fluidRow(
                        column(width = 6,
                               h2("Preterm Birth Outcomes and Risk Factors"),
                               includeMarkdown("markdown/maternity/policy/preterm_birth_outcomes_rf.md")
                        ),
                        column(width = 6,
                               includeMarkdown("markdown/maternity/policy/preterm_birth_outcomes_rf_bottom_right.md")
                        ) 
                      ),
                      fluidRow(
                        column(width = 6,
                               h2("Infant Mortality"),
                               includeMarkdown("markdown/maternity/policy/infant_mortality.md")
                        )  
                      ),
                      fluidRow(
                        column(width = 6,
                               h2("Infant Mortality Causes"),
                               includeMarkdown("markdown/maternity/policy/infant_mortality_causes.md")
                        ),
                        column(width = 6,
                               includeMarkdown("markdown/maternity/policy/infant_mortality_causes_bottom_right.md")
                        ), 
                      ),
             ))
  )
  
}

maternity_pol_server <- function(id, df) {
  
  moduleServer(id, function(input, output, session) {
  
    
  })
  
}