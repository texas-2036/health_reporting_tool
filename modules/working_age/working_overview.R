
# Load Data -----------------------------------------------------------------------------------

proj_all_pop <- readr::read_rds("clean_data/Aging/overview/pop_proj_all.rds")

# text module ----
working_overview_ui <- function(id) {
  
  fluidRow(
    box(
      # title = "Overview of Aging Texans",
      side="left",
      width = 12,
      fluidRow(
        column(width = 5,
               h2("Working Age Texans"),
               includeMarkdown("markdown/working_age/overview/overview.md")),
        column(width = 7,
               highcharter::highchartOutput(NS(id, "pop_proj_all")))),
      hr(),
      fluidRow(
        column(width = 12,
               h2("Key Takeaways"),
               includeMarkdown("markdown/working_age/overview/key_takeaways.md"))
      )
    )
  )
  
}

working_overview_server <- function(id, df) {
  
  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$thousandsSep <- ","
  options(highcharter.lang = hcoptslang)
  
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
  
  outputOptions(output, "pop_proj_all", suspendWhenHidden = FALSE)
    
  })
  
}
