# Load Data -----------------------------------------------------------------------------------

#diabetes data
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
                             highcharter::highchartOutput(NS(id, "diabetes_chart")),
                             includeMarkdown("markdown/maternity/conditions/diabetes_top_right.md"),
                             )),
                      fluidRow(
                        column(width = 6,
                               includeMarkdown("markdown/maternity/conditions/diabetes_graphic_callout.md")),
                        column(width = 6,
                               h2("Diabetes Rates Among Women Ages 18-44"),
                               
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
    output$diabetes_chart <- highcharter::renderHighchart(
      
      highchart() %>%
        hc_add_series(diabetes_trends_women_18_44 %>% filter(state_name!="Texas"), 
                      type="line", 
                      hcaes(x=edition, y=value, group=state_name), 
                      color="#DBDCDD") %>%
        hc_add_series(access_pcp %>% filter(state_name=="Texas"),
                      type="line", 
                      hcaes(x=edition, y=value),
                      lineWidth=5,
                      name="Texas") %>% 
        hc_title(text="Diabetes Rates Among Women Ages 18-44") %>%
        hc_subtitle(text="Diabetes Rates Among Women in Texas and Peer States identified by Texas 2036.") %>%
        hc_yAxis(title=list(text="Percentage of Women Ages 18-44"),
                 labels = list(enabled=TRUE,
                               format = "{value}")) %>% 
        hc_xAxis(tickColor = "#ffffff", 
                 # opposite = TRUE,
                 min = 0.5,
                 max = 13.5,
                 tickInterval = 1,
                 maxPadding = 0,
                 endOnTick = FALSE,
                 startOnTick = FALSE,
                 useHTML = TRUE,
                 alternateGridColor = "#f3f3f3",
                 categories = c("2016","2017","2018","2019", "2020"),
                 title = list(text = "Year")) %>%
        hc_legend(layout = "proximate", align = "right") %>% 
        hc_credits(
          enabled = TRUE,
          text = "America's Health Rankings analysis of CDC BRFSS",
          href = "https://www.americashealthrankings.org") %>%
        hc_add_theme(tx2036_hc_light())
    )
    
    
  })
  
}