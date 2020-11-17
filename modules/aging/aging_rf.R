
# Load Data -----------------------------------------------------------------------------------

obesity_plus <- readr::read_rds("clean_data/Aging/risk_factors/aging_rf_obesity.rds")

phys_inactivity_chart_tx <- readr::read_rds("clean_data/Aging/risk_factors/aging_rf_phys_inactivity.rds") %>% 
  mutate(edition=as.character(edition)) %>% 
  arrange(edition, states) %>% 
  filter(states=="Texas")

phys_inactivity_chart_peers <- readr::read_rds("clean_data/Aging/risk_factors/aging_rf_phys_inactivity.rds") %>% 
  mutate(edition=as.character(edition)) %>% 
  arrange(edition, states) %>% 
  filter(states!="Texas")

social_iso_map_df <- read_rds("clean_data/Aging/social_iso_data.rds") %>% 
  clean_names() %>% 
  filter(state_name!="United States") %>% 
  select(edition, state_name, measure_name, value, peer, source)

phys_inactivity_map_df <- readr::read_rds("clean_data/Aging/risk_factors/phys_inactivity_map.rds")

# UI Module Code ----
aging_rf_ui <- function(id) {
  
  fluidRow(
           tabBox(
             title = NULL,
             id = "tabset1", 
             side="left",
             width = 12,
             # height = 600,
             tabPanel(title="Obesity", 
                      fluidRow(
                      column(width = 5,
                             h2("Obesity"),
                             includeMarkdown("markdown/aging/overview/overview.md")),
                      column(width = 7,
                             highcharter::highchartOutput(NS(id, "obesity_charts"))))
                      ),
             tabPanel(title="Physical Inactivity", 
                      fluidRow(
                        column(width = 6,
                               h2("Physical Inactivity"),
                               includeMarkdown("markdown/aging/risk_factors/physical_inactivity.md"),
                               highcharter::highchartOutput(NS(id, "phys_inactivity_chart"), height="500px"),
                               ),
                        column(width = 6,
                               highcharter::highchartOutput(NS(id, "phys_inactivity_map"), height="650px")))),
             tabPanel(title="Social Isolation", 
                      fluidRow(
                        column(width = 5,
                               h2("Social Isolation"),
                               includeMarkdown("markdown/aging/risk_factors/social_isolation.md")),
                        column(width = 7,
                               highcharter::highchartOutput(NS(id, "social_isolation_chart"), height="650px"))),
                      hr())
             )
           )
  
}


# Server Module Code --------------------------------------------------------------------------

aging_rf_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    # w <- Waiter$new(html = spin_facebook(), 
    #                 id = c(NS(id,"obesity_charts"),
    #                        NS(id,"phys_inactivity_chart"),
    #                        NS(id,"phys_inactivity_map"), 
    #                        NS(id,"social_isolation_chart")))
    # 
    # dataset <- reactive({
    #   
    #   input$draw
    #   
    #   w$show()
    #   
    #   Sys.sleep(3)
    #   
    # })
    
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- ","
    options(highcharter.lang = hcoptslang)
    
    myMenuItems <- c("downloadCSV", "downloadXLS","separator", "downloadPNG", "downloadJPEG", "downloadPDF", "downloadSVG")
 

# Obesity - Chart -----------------------------------------------------------------------------

     
  output$obesity_charts <- highcharter::renderHighchart({
    
    # dataset()
      
    obesity_plus %>% 
      hchart("line", 
             hcaes(x=date, y=data_value),
             tooltip = list(pointFormat="<hr style='color:#3A4A9F;margin:2px 2px !important'><span style='font-weight:800;font-size:1.25em'>{point.y:.0f}%</span><span style='font-weight:400;font-size:1.25em'> of Texans Aged 65 years<br>& older reported being obese.</span>")) %>%
      hc_title(text="Obesity Rates Among Texans Ages 65+",
               useHTML = TRUE) %>% 
      hc_subtitle(text="In the past six years, senior obesity in Texas increased 16% from 26.3% to 30.6% of adults ages 65+.",
                  useHTML = TRUE) %>%
      hc_credits(
        enabled = TRUE,
        text = "SOURCE: U.S. Centers for Disease Control and Prevention | DATA: Behavioral Risk Factor Surveillance System",
        href = "https://datalab.texas2036.org/bwhqgjc/behavioral-risk-factor-surveillance-system-brfss-prevalence-data?accesskey=xclhoac") %>%
      hc_yAxis(title = list(text = "% of 65+ population"),
               format = "{value}%") %>% 
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(headerFormat = "<span style='font-size:1.25em;font-weight:800;text-transform:uppercase'>DURING {point.key:%Y}...</span><br>",
                 useHTML = TRUE) %>%
      highcharter::hc_add_theme(texas2036::tx2036_hc_light())
      
    })
  

# Phys. Inactivity - Chart -----------------------------------------------------------------
  
  output$phys_inactivity_chart <- highcharter::renderHighchart({
    
    # dataset()
    
    highchart() %>% 
      hc_add_series(phys_inactivity_chart_peers, 
                    type="line", 
                    hcaes(x=edition, y=value, group=states), 
                    color="#DBDCDD") %>% 
      hc_add_series(phys_inactivity_chart_tx,
                    type="line", 
                    hcaes(x=edition, y=value),
                    lineWidth=5,
                    name="Texas") %>% 
      #hc_title(text="Physical Inactivity Trends Among Seniors") %>%
      #hc_subtitle(text="Physical Inactivity Trends Among Seniors in Texas and Peer States identified by Texas 2036") %>%
      hc_yAxis(title=list(text="% of Seniors Reporting Physical Inactivity"),
               labels = list(enabled=TRUE,
                             format = "{value}%")) %>% 
      hc_xAxis(tickColor = "#ffffff", 
               opposite = TRUE,
               useHTML = TRUE,
               alternateGridColor = "#f3f3f3",
               categories = c("2013","2014","2015","2016","2017","2018","2019"),
               title = list(text = "Year of America's Health Ranking Report")) %>%
      hc_credits(
        enabled = TRUE,
        text = "America's Health Rankings analysis of CDC, Behavioral Risk Factor Surveillance System, United Health Foundation, AmericasHealthRankings.org, Accessed 2020.",
        href = "https://datalab.texas2036.org/ljosakb/america-s-health-rankings-senior-report?accesskey=esasind") %>%
      hc_add_theme(tx2036_hc_light())
    
  })
  

# Phys. Inactivity - Map --------------------------------------------------------

  output$phys_inactivity_map <- highcharter::renderHighchart({
    
    # dataset()
    
    col_pal <- RColorBrewer::brewer.pal(9,"Greens")
    
    hcmap(map = "countries/us/us-tx-all",
          data = phys_inactivity_map_df,
          value = "value",
          joinBy = c("name","county"),
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
        text = "SOURCE: County Health Rankings Analysis | DATA: CDC Interactive Diabetes Atlas",
        href = "https://datalab.texas2036.org/fywtqfb/texas-county-health-ranking?accesskey=yygfyw") %>%
      hc_title(text="Texans ages 65+ reporting no leisure-time physical activity") %>% 
      hc_subtitle(text="Percentage of adults ages 65 and older in fair or better health who Reporting Doing No Leisure Time Physical Activity.") %>% 
      # hc_size(height=550) %>% 
    highcharter::hc_add_theme(texas2036::tx2036_hc_light())
    
  })
  

# Social Isolation - Map ----------------------------------------------------------------------

  output$social_isolation_chart <- highcharter::renderHighchart({
    
    # dataset()
    
    col_pal <- RColorBrewer::brewer.pal(9,"Blues")
    
    hcmap(map = "countries/us/us-all",
          data = social_iso_map_df,
          value = "value",
          joinBy = c("name","state_name"),
          name = "% At Risk of Social Isolation",
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
        useHTML = TRUE,
        text = "SOURCE: America's Health Rankings Analysis | DATA: U.S. Census Bureau, American Community Survey, 2014-2018",
        href = "https://datalab.texas2036.org/fywtqfb/texas-county-health-ranking?accesskey=yygfyw") %>%
      hc_title(text="Texans Aged 65+ At Risk of Social Isolation") %>% 
      hc_subtitle(text="Risk of social isolation is calculated by America's Health Rankings using a model that incorporates risk factors in adults ages 65 and older using the following indicators from publicly available census data: poverty; living alone; divorced, separated or widowed; never married; disability; and independent living difficulty") %>% 
      # hc_size(height=550) %>% 
      highcharter::hc_add_theme(texas2036::tx2036_hc_light())
    
  })
    
    outputOptions(output, "phys_inactivity_chart", suspendWhenHidden = FALSE)
    
    outputOptions(output, "phys_inactivity_map", suspendWhenHidden = FALSE)
    
    outputOptions(output, "social_isolation_chart", suspendWhenHidden = FALSE)
    
  })
  
}