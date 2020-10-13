proj_all_pop <- readr::read_rds("clean_data/Aging/overview/pop_proj_all.rds")

proj_65_pop <- readr::read_rds("clean_data/Aging/overview/pop_proj_65.rds")



# text module ----
aging_overview_ui <- function(id) {
  
  fluidRow(
           box(
             # title = "Overview of Aging Texans",
             side="left",
             width = 12,
             fluidRow(
               column(width = 12,
                      h2("Aging in Texas"),
                      includeMarkdown("markdown/aging/overview/overview.md"))),
             hr(),
             fluidRow(
               column(width = 6,
                      highcharter::highchartOutput(NS(id, "pop_proj_all"))),
               column(width = 6,
                      highcharter::highchartOutput(NS(id, "pop_proj_65_by_race")))),
             hr(),
             fluidRow(
               column(width = 12,
                      h2("Key Takeaways"),
                      includeMarkdown("markdown/aging/risk_factors/social_isolation.md"))
             )
             )
  )
            
  
}

aging_overview_server <- function(id, df) {
  
  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$thousandsSep <- ","
  options(highcharter.lang = hcoptslang)
  
  myMenuItems <- c("downloadCSV", "downloadXLS","separator", "downloadPNG", "downloadJPEG", "downloadPDF", "downloadSVG")
  
  moduleServer(id, function(input, output, session) {
    
  output$pop_proj_all <- highcharter::renderHighchart({
      
    proj_all_pop %>% 
      hchart("area", hcaes(x=year, y=group_pop, group=age_groups, colors=age_groups)
             # tooltip = list(pointFormat = "<br><span style='color:{point.color}'>\u25CF</span> <b>{series.name}</b>: {point.pct:,.2f}%"),
             ) %>% 
      hc_title(
        text ="Texas Population Projections, by Age Group",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text ="Shown: Entire Population, Between 2010-2050",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="% of Total, By Age Group")) %>%
      hc_xAxis(title=NULL) %>% 
      # hc_legend(enabled=FALSE) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 shared=TRUE) %>%
      hc_plotOptions(area = list(fillOpacity=.5),
                     series = list(stacking = "percent")) %>%
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas Demographic Center | Data: Texas Population Projections",
        href = "https://demographics.texas.gov/Data/TPEPP/Projections/Index") %>%
      highcharter::hc_add_theme(texas2036::tx2036_hc_light())
      
    })
  
  output$pop_proj_65_by_race <- highcharter::renderHighchart({
    
    proj_65_pop %>% 
      mutate(group_pct=group_pct*100) %>% 
      hchart("line", hcaes(x=year, y=group_pct, group=dem_groups, colors=dem_groups),
             tooltip = list(pointFormat = "<br><span style='color:{point.color}'>\u25CF</span> <b>{series.name}</b>: {point.group_pct:,.2f}%"),
      ) %>% 
      hc_title(
        text ="Growth of Aging Population, by Race + Ethnicity",
        useHTML = TRUE) %>% 
      hc_subtitle(
        text ="Shown: Population Aged 65+ Between 2010-2050, by Demographic Group.",
        useHTML = TRUE) %>% 
      hc_yAxis(title = list(text ="% of All Population Aged 65+, By Race + Ethnicity")) %>%
      hc_xAxis(title=NULL) %>% 
      hc_tooltip(table = TRUE, sort = TRUE,
                 shared=TRUE) %>%
      hc_plotOptions(area = list(fillOpacity=.5)) %>%
      hc_credits(
        enabled = TRUE,
        text = "Source: Texas Demographic Center | Data: Texas Population Projections",
        href = "https://demographics.texas.gov/Data/TPEPP/Projections/Index") %>%
      highcharter::hc_add_theme(texas2036::tx2036_hc_light())
    
  })
    
  })
  
}