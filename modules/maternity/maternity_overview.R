# Load  Data ----------------------------------------------------------------------------------
#us_map <- "https://code.highcharts.com/mapdata/countries/us/us-all.js"
tx_map <- "https://code.highcharts.com/mapdata/countries/us/us-tx-all.js"
birth_rate_per_1k <- read_rds("clean_data/maternity/birth_rates_per1k.rds")

distr_of_births_by_race <- read_rds("clean_data/maternity/distr_of_births_by_race.rds")

county_birth_rate_map <- read_rds("clean_data/maternity/county_birth_rate_map.rds")

county_teen_birth_rate_map <- read_rds("clean_data/maternity/county_teen_birth_rate_map.rds")

teen_birth_rates <- read_rds("clean_data/maternity/teen_birth_rate_map.rds")

# text module ----
maternity_overview_ui <- function(id) {
  
  fluidRow(
    box(
      side="left",
      width = 12,
      fluidRow(
        column(width = 6,
               h2("Maternity in Texas"),
               includeMarkdown("markdown/maternity/overview/maternity.md")),
        column(width = 6,
               highcharter::highchartOutput(NS(id, "m_birth_rate_per_1k"), height = "600px"))),
      hr(),
      fluidRow(
        column(width = 6,
               h2("Birth Rate"),
               includeMarkdown("markdown/maternity/overview/birth_rate.md"),
               highcharter::highchartOutput(NS(id, "m_distr_of_births_by_race"))),
        column(width = 6,
               highcharter::highchartOutput(NS(id, "m_county_birth_rate_map"), height = "600px"))),
      hr(),
      fluidRow(
        column(width = 6,
               h2("Teen Birth Rate"),
               includeMarkdown("markdown/maternity/overview/teen_birth_rate.md"),
               highcharter::highchartOutput(NS(id, "teen_birthrates"), height = "600px")),
        column(width = 6,
               highcharter::highchartOutput(NS(id, "m_county_teen_birth_rate_map"), height = "600px")))
    )
  )
  
}

maternity_overview_server <- function(id, df) {
  
  moduleServer(id, function(input, output, session) {
    
    output$m_birth_rate_per_1k <- highcharter::renderHighchart({
      
      highchart() %>% 
        hc_add_series(birth_rate_per_1k %>% filter(state_name!="Texas"), 
                      type="line", 
                      hcaes(x=edition, y=value, group=state_name, labels = edition), 
                      color="#DBDCDD") %>% 
        hc_add_series(birth_rate_per_1k %>% filter(state_name=="Texas"),
                      type="line", 
                      hcaes(x=edition, y=value, labels = edition),
                      lineWidth=5,
                      name="Texas") %>% 
        hc_title(text="Birth Rate in Texas & the United States") %>%
        hc_subtitle(text="Data for birth rates shown represents data between 2000-2018") %>%
        hc_yAxis(title=list(text="Rate Per 1,000 Population"),
                 labels = list(enabled=TRUE,
                               format = "{value}")) %>% 
        hc_xAxis(tickColor = "#ffffff", 
                 min = 0.5,
                 max = 16.5,
                 tickInterval = 1,
                 maxPadding = 0,
                 endOnTick = FALSE,
                 startOnTick = FALSE,
                 useHTML = TRUE,
                 alternateGridColor = "#f3f3f3",
                 categories = c("2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"),
                 title = list(text = "Year")) %>%
        hc_legend(layout = "proximate", align = "right") %>% 
        hc_credits(
          enabled = TRUE,
          text = "SOURCE: Texas Department of State Health Services, 2019 Healthy Texas Mothers & Babies Data Book.",
          href = "https://www.dshs.texas.gov/healthytexasbabies/data.aspx.") %>%
        hc_add_theme(tx2036_hc_light())
      
    })
    
    output$m_distr_of_births_by_race <- highcharter::renderHighchart({
      
      distr_of_births_by_race %>% 
        mutate(group=str_to_title(group)) %>% 
        hchart("area", hcaes(x=year, y=value, group=group, colors=group)) %>% 
        hc_title(
          text ="Distribution of Race/Ethnicity Groups Among All Live Births",
          useHTML = TRUE) %>% 
        hc_subtitle(
          text ="Shown: Data for Texas Between 2009-2018",
          useHTML = TRUE) %>% 
        hc_yAxis(title = list(text ="% of Total, By Race & Ethnicity")) %>%
        hc_xAxis(title=NULL) %>% 
        # hc_legend(enabled=FALSE) %>% 
        hc_tooltip(table = TRUE, sort = TRUE,
                   shared=TRUE) %>%
        hc_plotOptions(area = list(fillOpacity=.5),
                       series = list(stacking = "percent")) %>%
        hc_credits(
          enabled = TRUE,
          text = "Source: Texas Department of State Health Services | Data: 2019 Healthy Texas Mothers & Babies Data Book",
          href = "https://www.dshs.texas.gov/healthytexasbabies/data.aspx") %>%
        highcharter::hc_add_theme(texas2036::tx2036_hc_light())
      
    })
    
    output$m_county_birth_rate_map <- highcharter::renderHighchart({
      
      col_pal <- RColorBrewer::brewer.pal(9,"Blues")
      
      hcmap(map = tx_map,
            data = county_birth_rate_map,
            value = "rate",
            joinBy = c("name","county_of_residence"),
            name = "County Birth Rate (Per 1,000 Population)",
            borderColor = "#FAFAFA",
            borderWidth = 0.1,
            tooltip = list(
              valueSuffix = " Births Per 1,000 Population")) %>%
        hc_legend(layout='vertical',
                  align='left',
                  verticalAlign='bottom',
                  itemMarginTop=10,
                  itemMarginBottom=10) %>% 
        hc_colorAxis(stops = color_stops(n=8, colors=col_pal),
                     reversed=FALSE) %>%
        hc_title(text="Birth Rates Among Texas Counties, 2010-2014") %>%
        hc_subtitle(text="Births per 1,000 Population") %>%
        hc_credits(
          enabled = TRUE,
          text = "SOURCE: Texas Department of State Health Services",
          href = "https://www.dshs.texas.gov/chs/vstat/vs14/map2.aspx") %>%
        hc_add_theme(tx2036_hc_light())
      
    })
    
    output$teen_birthrates <- highcharter::renderHighchart({
      
      highchart() %>%
        hc_add_series(teen_birth_rates %>% filter(race != 'Texas'), 
                      type="line", 
                      hcaes(x=year, y=value, group=race)) %>% 
        hc_add_series(teen_birth_rates %>% filter(race == 'Texas'), 
                      type="line", 
                      hcaes(x=year, y=value, group=race), 
                      name="Texas",
                      lineWidth = 5) %>% 
        hc_title(text="Teen (15-19 years old) Birth Rate by Race/Ethnicity") %>%
        hc_yAxis(title=list(text="Rate per 1,000 Population"),
                 labels = list(enabled=TRUE,
                               format = "{value}")) %>%
        hc_xAxis(tickColor = "#ffffff", 
                 tickInterval = 1,
                 maxPadding = 0,
                 endOnTick = FALSE,
                 startOnTick = FALSE,
                 useHTML = TRUE,
                 alternateGridColor = "#f3f3f3",
                 title = list(text = "Year")) %>%
        hc_legend(layout = "proximate", align = "right") %>%
        hc_credits(
          enabled = TRUE,
          text = "Note: The 2017 and 2018 data are provisional. | SOURCE: Texas Department of State Health Services, 2019 Healthy Texas Mothers & Babies Data Book",
          href = "https://www.dshs.texas.gov/healthytexasbabies/data.aspx") %>%
        hc_add_theme(tx2036_hc_light())
      
    })
    
    
    output$m_county_teen_birth_rate_map <- highcharter::renderHighchart({
      
      col_pal <- RColorBrewer::brewer.pal(9,"Purples")
      
      hcmap(map = tx_map,
            data = county_teen_birth_rate_map,
            value = "birth_rate",
            joinBy = c("name","county"),
            name = "Teen Birth Rate Per 1,000 Population",
            borderColor = "#FAFAFA",
            borderWidth = 0.1,
            tooltip = list(
              valueSuffix = " Teen Births Per 1,000 Population")) %>%
        hc_legend(layout='vertical',
                  align='left',
                  verticalAlign='bottom',
                  itemMarginTop=10,
                  itemMarginBottom=10) %>% 
        hc_colorAxis(stops = color_stops(n=8, colors=col_pal),
                     reversed=FALSE) %>%
        hc_title(text="Estimated Teen Birth Rate For Females Aged 15-19 (Per 1,000 Population), by County, 2018") %>%
        hc_subtitle(text="Teen Births per 1,000 Population") %>%
        hc_credits(
          enabled = TRUE,
          text = "Khan D, Hamilton B, Rossen LM, He Y, Wei R, Dienes E. Teen birth rates for age group 15–19 in the United States by county, 2003–2018. National Center for Health Statistics. 2020.",
          href = "https://www.cdc.gov/nchs/data-visualization/county-teen-births/#data-tables") %>%
        hc_add_theme(tx2036_hc_light())
      
    })
    
    
  })
  
}