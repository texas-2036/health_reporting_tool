# load data ----------

##this section will be where all the data for this page will be loaded once that data is collected
prenatal_care <- readr::read_rds("clean_data/maternity/prenatal_deficiency_rate_map.rds")
infant_mortality <- readr::read_rds("clean_data/maternity/infant_mortality_trends.rds")
im_map <-readr::read_rds("clean_data/maternity/infant_mortality_map.rds")
im_demo_trends <- readr::read_rds("clean_data/maternity/infant_mortality_demographic_trends.rds")

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
                      includeMarkdown("markdown/maternity/policy/access.md"),
                      img(src = "figures/maternity/policy/Percent of Primary Care Physician Demand Exceeding Supply and FTE shortage_surplus.png",
                          width = "90%")
                      )
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
                        ),
                        column(width = 6,
                               h2("Percent of Live Births Receiving Prenatal Care in the First Trimester by Race/Ethnicity"),
                               img(src = "figures/maternity/policy/fig_26_pct_of_live_births_with_prenatal_care_by_race.jpg",
                                   width = "90%")
                        )
                      ),
                      fluidRow(
                        column(width = 6,
                               h2("Preterm Birth"),
                               includeMarkdown("markdown/maternity/policy/preterm_birth.md"),
                               img(src = "figures/maternity/policy/fig_13_pct_born_preterm_tx_vs_us.jpg",
                                   width = "90%")
                        ),
                        column(width = 6,
                               h2("Percent of Live Births Born Preterm in Texas Counties"),
                               img(src = "figures/maternity/policy/fig_15_pct_born_preterm.jpg",
                                   width = "90%")
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
                               includeMarkdown("markdown/maternity/policy/infant_mortality.md"),
                               highcharter::highchartOutput(NS(id, "infant_mort"))
                        ),
                        column(width = 6,
                               highcharter::highchartOutput(NS(id, 'infant_mort_tx_map'))
                        )
                      ),
                      fluidRow(
                        column(width = 6,
                               h2("Infant Mortality Causes"),
                               includeMarkdown("markdown/maternity/policy/infant_mortality_causes.md")
                        ),
                        column(width = 6,
                               highcharter::highchartOutput(NS(id, 'im_demo_trends')),
                               includeMarkdown("markdown/maternity/policy/infant_mortality_causes_bottom_right.md")
                        ) 
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
    
    
    output$infant_mort <- highcharter::renderHighchart({
      
      highchart() %>% 
        hc_add_series(infant_mortality %>% filter(state_name!="Texas"), 
                      type="line", 
                      hcaes(x=edition, y=value, group=state_name), 
                      color="#DBDCDD") %>% 
        hc_add_series(infant_mortality %>% filter(state_name=="Texas"),
                      type="line", 
                      hcaes(x=edition, y=value),
                      lineWidth=5,
                      name="Texas") %>% 
        hc_title(text="Infant Mortality Rate in Texas, Peer States, and the United States") %>%
        #hc_subtitle(text="Diabetes Trends Among Adults Aged 18+ in Texas and Peer States identified by Texas 2036.") %>%
        hc_yAxis(title=list(text="Deaths per 100,000 Births"),
                 labels = list(enabled=TRUE,
                               format = "{value}")) %>% 
        hc_xAxis(tickColor = "#ffffff", 
                 min = 0.5,
                 max = 3,
                 tickInterval = 1,
                 maxPadding = 0,
                 endOnTick = FALSE,
                 startOnTick = FALSE,
                 useHTML = TRUE,
                 alternateGridColor = "#f3f3f3",
                 categories = c("2016","2018","2019", "2020"),
                 title = list(text = "Year")) %>%
        hc_legend(layout = "proximate", align = "right") %>% 
        hc_credits(
          enabled = TRUE,
          text = "SOURCE: CDC WONDER.",
          href = "https://wonder.cdc.gov/lbd-current.html") %>%
        hc_add_theme(tx2036_hc_light())
      
      
    })
    
    output$infant_mort_tx_map <- highcharter::renderHighchart({
      
      col_pal <- RColorBrewer::brewer.pal(9,"Blues")
      
      
      hcmap(map = "countries/us/us-tx-all",
            data = im_map,
            value = "rate",
            joinBy = c("name","county_of_residence"),
            name = "Rate",
            borderColor = "#FAFAFA",
            borderWidth = 0.1,
            tooltip = list(
              valueSuffix = "%")) %>%
        hc_legend(layout='vertical',
                  align='left',
                  verticalAlign='bottom',
                  itemMarginTop=10,
                  itemMarginBottom=10) %>% 
        hc_colorAxis(stops = color_stops(n=8, colors=col_pal),
                     reversed=FALSE) %>%
        hc_title(text="Infant Mortality Rate in Texas Counties") %>%
        hc_subtitle(text="Deaths per 1,000 live births") %>%
        hc_credits(
          enabled = TRUE,
          text = "SOURCE: 2019 Healthy Texas Mothers and Babies (HTMB) Report",
          href = "https://wonder.cdc.gov/natality.html") %>%
        hc_add_theme(tx2036_hc_light())
    })
    
    output$im_demo_trends <- highcharter::renderHighchart({
      
      highchart() %>%
        hc_add_series(im_demo_trends,
                      type="line", 
                      hcaes(x=edition, y=value, group=measure_name), 
                      color="#808080")%>% 
        hc_title(text="Infant Mortality Rate in Texas by Race/Ethnicity") %>%
        hc_yAxis(title=list(text="Deaths per 1,000 Live Births"),
                 labels = list(enabled=TRUE,
                               format = "{value} Deaths")) %>% 
        hc_xAxis(tickColor = "#ffffff", 
                 min = 0.5,
                 max = 3,
                 tickInterval = 1,
                 maxPadding = 0,
                 endOnTick = FALSE,
                 startOnTick = FALSE,
                 useHTML = TRUE,
                 alternateGridColor = "#f3f3f3",
                 categories = c("2016","2018","2019", "2020"),
                 title = list(text = "Year")) %>%
        hc_legend(layout = "proximate", align = "right") %>% 
        hc_credits(
          enabled = TRUE,
          text = "SOURCE: 2019 Healthy Texas Mothers and Babies (HTMB) Report.",
          href = "https://wonder.cdc.gov/natality.html") %>%
        hc_add_theme(tx2036_hc_light())
      
    })
    
  })
  
}