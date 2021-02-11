
# Load Data -----------------------------------------------------------------------------------
child_mental_health <- read_rds('clean_data/Children/child_mental_health_data.rds') %>%
  na.omit() %>% select(-13)

child_mental_health$age <- gsub("\\?6-11", "6-11", child_mental_health$age)
child_mental_health$age <- gsub("\\?3-5", "3-5", child_mental_health$age)
child_mental_health$age <- gsub("\\?12-17", "12-17", child_mental_health$age)

teen_suicide_rates <- read_rds("clean_data/Children/teen_suicide_rates.rds")

teen_suicide_rates <- teen_suicide_rates %>% filter(state_name!="West Virginia")

# text module ----
childhood_cnd_ui <- function(id) {
  
  fluidRow(
           tabBox(
             # title = "Conditions",
             id = "tabset1", 
             side="left",
             width = 12,
             tabPanel(title="Mental Health", 
                      fluidRow(
                        column(width = 6,
                               h2("Mental Health"),
                               includeMarkdown("markdown/childhood/conditions/mental_health.md")),
                        column(width = 6,
                               highcharter::highchartOutput(NS(id, "childhood_cnd_mh_chart"), height = "500px"))),
                      hr(),
                      fluidRow(
                        column(width = 6,
                               h2("Teen Suicide Rates"),
                               includeMarkdown("markdown/childhood/conditions/mh_teen_suicide.md")),
                        column(width = 6,
                               highcharter::highchartOutput(NS(id, "teen_suicide_mh_chart"), height = "500px")))),
             tabPanel(title="Diabetes", 
                      fluidRow(
                      column(width = 7,
                             h2("Diabetes in Childhood"),
                             includeMarkdown("markdown/childhood/conditions/diabetes.md")),
                      column(width = 5,
                             includeMarkdown("markdown/childhood/conditions/diabetes_right.md"))))
             )
           )
  
}

childhood_cnd_server <- function(id, df) {
  
  moduleServer(id, function(input, output, session) {
    
  output$childhood_cnd_mh_chart <- highcharter::renderHighchart({
      
    highchart() %>%
      hc_add_series(child_mental_health, type = 'bar', hcaes(x = age, y = depression_percent), name = 'Depression') %>%
      hc_add_series(child_mental_health, type = 'bar', hcaes(x = age, y = anxiety_percent), name = 'Anxiety') %>%
      hc_add_series(child_mental_health, type = 'bar', hcaes(x = age, y = behavorial_disorder_percent), name = 'Behavioral Disorders') %>% 
      hc_xAxis(tickColor = "#ffffff", 
               opposite = FALSE,
               useHTML = TRUE,
               categories = c("Ages 3-5","Ages 6-11","Ages 12-17")) %>%
      hc_add_theme(tx2036_hc_light()) %>%
      hc_title(text="Depression, Anxiety and Behavior Disorders by Age") %>%
      hc_subtitle(text = 'Percentage of children in age cohort') %>%
      hc_credits(
        enabled = TRUE,
        useHTML = TRUE,
        text = "Analysis of the 2016 National Survey of Children's Health",
        href = "https://doi.org/10.1016/j.jpeds.2018.09.021")
      
    })
  
  output$teen_suicide_mh_chart <- highcharter::renderHighchart({
    
    highchart() %>%
      hc_add_series(teen_suicide_rates %>% filter(state_name!="Texas"),
                    'line',
                    hcaes(x=edition, y=value, group=state_name),
                    color="#DBDCDD") %>%
      hc_add_series(teen_suicide_rates %>% filter(state_name=="Texas"),
                    'line',
                    hcaes(x=edition, y=value),
                    lineWidth=5,
                    name="Texas") %>%
      hc_title(text="Teen Suicide Rates") %>%
      hc_subtitle(text='Number of deaths by suicide per 100,000 adolescents ages 15-19 among Texas and peer states identified by Texas 2036') %>%
      hc_yAxis(title=list(text="Rates")) %>% 
      hc_xAxis(tickColor = "#ffffff", 
               min = 0.5,
               max = 2.5,
               tickInterval = 1,
               maxPadding = 0,
               endOnTick = FALSE,
               startOnTick = FALSE,
               useHTML = TRUE,
               alternateGridColor = "#f3f3f3",
               categories = c("2016","2018","2019","2020"),
               title = list(text = "Year of America's Health Ranking Report")) %>%
      hc_legend(layout = "proximate", align = "right") %>% 
      hc_credits(enabled = TRUE,
                 text = "America's Health Rankings analysis of CDC WONDER AmericasHealthRankings.org, Accessed 2020.",
                 href = "https://datalab.texas2036.org/uxoopxe/health-of-women-and-children-report-for-u-s?accesskey=potjbm") %>%
      hc_add_theme(tx2036_hc_light())
    
  })
  
    
  })
  
}