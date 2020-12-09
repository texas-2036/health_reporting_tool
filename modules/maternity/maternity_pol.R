# load data ----------

##this section will be where all the data for this page will be loaded once that data is collected
prenatal_care <- readr::read_rds("clean_data/maternity/prenatal_deficiency_rate_map.rds")

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
                             ),
                      column(width = 6,
                             highcharter::highchartOutput(NS(id, "prenatal_birth_map"))
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
    
    
    # Prenatal Care - Map --------------------------------------------------------
    
    output$prenatal_birth_map <- highcharter::renderHighchart({
      
      col_pal <- RColorBrewer::brewer.pal(9,"Reds")
      
      hcmap(map = "countries/us/us-tx-all",
            data = prenatal_care,
            value = "percent",
            joinBy = c("name","county_of_residence"),
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
          text = "SOURCE: CDC Birth Files",
          href = "https://www.cdc.gov/nchs/data/nvsr/nvsr68/nvsr68_13-508.pdf") %>%
        hc_title(text="Percent of Live Births not Receiving Prenatal Care in the First Trimester") %>% 
        # hc_size(height=550) %>% 
        highcharter::hc_add_theme(texas2036::tx2036_hc_light())
    })
    
    
  })
  
}