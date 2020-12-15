# Load Data -----------------------------------------------------------------------------------

#diabetes data
diabetes_trends_women_18_44 <- read_rds("clean_data/maternity/diabetes_trends_women_18_44.rds")

#postpartum depression data
post_part <- readr::read_rds("clean_data/maternity/postpartum_depression_map.rds")


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
                               h2('Rates of Maternal Diabetes by Race/Ethnicity'),
                               img(src = "figures/maternity/conditions/fig_35_rates_of_maternal_diabetes_by_race.jpg",
                                   width = "90%"),
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
                      h2("Rates of Maternal Hypertension by Race/Ethnicity"),
                      img(src = "figures/maternity/conditions/fig_34_rates_of_maternal_hypertension_by_race.jpg",
                          width = "90%"),
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
                               highcharter::highchartOutput(NS(id, "postpartum_map")),
                               includeMarkdown("markdown/maternity/conditions/postpartum_depression_bottom_right.md")
                        ))
                      
                      )
             )
           )
  
}

maternity_cnd_server <- function(id, df) {
  
  moduleServer(id, function(input, output, session) {
    
    # maternal diabetes chart module ----    
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
    
    # postpartum depression map module ----    
    output$postpartum_map <- highcharter::renderHighchart({
      
      hcoptslang <- getOption("highcharter.lang")
      hcoptslang$thousandsSep <- ","
      options(highcharter.lang = hcoptslang)
      
      col_pal <- RColorBrewer::brewer.pal(9,"Blues")
      
      hcmap(map = "countries/us/us-all",
            data = post_part,
            value = "value",
            joinBy = c("name","state_name"),
            name = "Percentage of Women with a Recent Live Birth",
            borderColor = "#FAFAFA",
            borderWidth = 0.1,
            tooltip = list(
              valueDecimals = 2,
              valueSuffix = " Percentage of Women with a Recent Live Birth")) %>% 
        hc_legend(layout='vertical',
                  align='left',
                  verticalAlign='bottom',
                  itemMarginTop=10,
                  itemMarginBottom=10) %>% 
        hc_colorAxis(stops = color_stops(n=8, colors=col_pal),
                     min = 9.7,
                     max = 23.5,
                     reversed=FALSE) %>%
        hc_credits(
          enabled = TRUE,
          useHTML = TRUE,
          text = "SOURCE: America's Health Rankings",
          href = "https://www.americashealthrankings.org/explore/health-of-women-and-children/measure/postpartum_depression/state/TX?edition-year=2018") %>%
        hc_title(text="Percentage of Women with a Recent Live Birth Who Reported Experiencing Postpartum Depressive Symptoms") %>% 
        highcharter::hc_add_theme(texas2036::tx2036_hc_light())
    })
    
    
  })
  
}