
# Load Data -----------------------------------------------------------------------------------
barriers <- read_rds("clean_data/Working Age/policy/barriers_to_care_us.rds")

prevention_wk <- read_rds("clean_data/Working Age/policy/flu_working_adults.rds")

access_pcp <- read_rds("clean_data/Working Age/policy/access_pcp.rds")

access_mh <- read_rds("clean_data/Working Age/policy/access_mh.rds")

uninsured_race <- read_rds("clean_data/Working Age/policy/uninsured_race.rds")

uninsured_income <- read_rds("clean_data/Working Age/policy/uninsured_income.rds")

uninsured_rates <- read_rds("clean_data/Working Age/policy/uninsured_rates.rds")

tx_uninsurance <- read_rds("clean_data/Working Age/policy/tx_uninsured.rds")

# text module ----
working_pol_ui <- function(id) {
  
  fluidRow(
           tabBox(
             # title = "Policies & Clinical Care",
             id = "tabset1", 
             side="left",
             width = 12,
             tabPanel(title="Uninsurance",
                      fluidRow(
                        column(width = 6,
                               h2("Uninsurance"),
                               includeMarkdown("markdown/working_age/policy/uninsurance_intro_top.md"),
                               highcharter::highchartOutput(NS(id, "uninsured_chart"))),
                        column(width = 6,
                               highcharter::highchartOutput(NS(id, "uninsured_map")))),
                      hr(),
                      fluidRow(
                      column(width = 6,
                             h2("Why does uninsurance matter?"),
                             includeMarkdown("markdown/working_age/policy/uninsurance_top.md")),
                      column(width = 6,
                             highcharter::highchartOutput(NS(id, "barriers_chart")))),
                      hr(),
                      h2("Characteristics of the Nonelderly Uninsured, 2018"),
                      fluidRow(
                        column(width = 6,
                               highcharter::highchartOutput(NS(id, "uninsurance_income"))),
                        column(width = 6,
                               highcharter::highchartOutput(NS(id, "uninsurance_race"))))),
             tabPanel("Provider Access", 
                      fluidRow(
                        column(width = 5,
                               h2("Primary Care Access"),
                               includeMarkdown("markdown/working_age/policy/provider_pcp_access.md")),
                        column(width = 7,
                               highcharter::highchartOutput(NS(id, "provider_pcp_chart")))),
                      hr(),
                      fluidRow(
                        column(width = 5,
                               h2("Mental Health Access"),
                               includeMarkdown("markdown/working_age/policy/provider_mh_access.md")),
                        column(width = 7,
                               highcharter::highchartOutput(NS(id, "provider_mh_chart"))))),
             tabPanel("Prevention",  
                      fluidRow(
                        column(width = 12,
                        h2("Flu Vaccination Among Working Age Texans"),
                        includeMarkdown("markdown/working_age/policy/prevention.md"),
                        highchartOutput(NS(id, "prevention_chart_wrk")))))
             )
           )
  
}

working_pol_server <- function(id, df) {
  
  moduleServer(id, function(input, output, session) {
    
    
  output$uninsured_chart <- highcharter::renderHighchart({
    
    highchart() %>%
      hc_add_series(uninsured_rates %>% filter(state!="Texas"), 
                    type="line", 
                    hcaes(x=year, y=value, group=state), 
                    color="#DBDCDD") %>% 
      hc_add_series(uninsured_rates %>% filter(state=="Texas"),
                    type="line", 
                    hcaes(x=year, y=value),
                    lineWidth=5,
                    name="Texas") %>% 
      hc_title(text="Texas Uninsured Rate Compared to Peer States, Ages 18-64, 2008-2018") %>%
      hc_yAxis(title=list(text="Percent"),
               labels = list(enabled=TRUE,
                             format = "{value}%")) %>% 
      hc_xAxis(tickColor = "#ffffff", 
               # opposite = TRUE,
               tickInterval = 1,
               maxPadding = 0,
               endOnTick = FALSE,
               startOnTick = FALSE,
               useHTML = TRUE,
               alternateGridColor = "#f3f3f3",
               categories = c("2008","2009","2010","2011",
                              "2012","2013","2014","2015","2016","2017","2018"),
               title = list(text = "Year")) %>%
      hc_tooltip(valueSuffix = "%") %>%
      hc_legend(layout = "proximate", align = "right") %>% 
      hc_credits(
        enabled = TRUE,
        text = "SOURCE: Small Area Health Insurance Estimates (SAHIE), U.S. Census Bureau",
        href = "https://www.census.gov/data-tools/demo/sahie/#/") %>%
      hc_add_theme(tx2036_hc_light())
    
  })
  
    
  output$uninsured_map <- highcharter::renderHighchart({
    
    col_pal <- RColorBrewer::brewer.pal(9,"RdPu")
    
    hcmap(map = "countries/us/us-tx-all",
          data = tx_uninsurance,
          value = "value",
          joinBy = c("name","county"),
          name = "Uninsurance Rate",
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
                   min = 10,
                   max = 35,
                   reversed=FALSE) %>%
      hc_credits(
        enabled = TRUE,
        useHTML = TRUE,
        text = "SOURCE: Small Area Health Insurance Estimates (SAHIE), U.S. Census Bureau",
        href = "https://www.census.gov/data-tools/demo/sahie/#/") %>%
      hc_title(text="Uninsurance Rate in Texas Counties, Ages 18-64, 2018") 
      highcharter::hc_add_theme(texas2036::tx2036_hc_light())
    
  })
  
  
    
  output$barriers_chart <- highcharter::renderHighchart({
      
      highchart() %>% 
      hc_add_series(data = barriers %>% mutate(pct=pct*100),
             "bar", 
             hcaes(x=care_status,y=pct, 
                   group = insurance_type, 
                   labels=insurance_type)) %>% 
      hc_plotOptions(series=list(
        groupPadding = .15
      )) %>% 
      hc_xAxis(reversedStacks = TRUE,
               labels = list(style = list(fontSize="16px")),
               categories = c("No usual source of care",
                              # "Postponed seeking care due to cost",
                              "Went without needed care due to cost",
                              "Postponed or did not get needed prescription drug due to cost")) %>% 
      hc_yAxis(labels = list(format = '{value}%')) %>% 
      hc_title(text="Barriers to Care Among Nonelderly Adults in the US by Insurance Status, 2018") %>% 
      hc_subtitle(text="Source: Kaiser Family Foundation Analysis of 2018 National Health Interview Survey") %>% 
      hc_legend(align = "right",
                reversed = TRUE,
                verticalAlign = "top",
                layout = "vertical",
                x = -20,
                y = 220,
                itemStyle = list(fontSize="14px", textTransform="uppercase"),
                floating = TRUE) %>%
      highcharter::hc_add_theme(texas2036::tx2036_hc_light())
      
    })
  
  
  
  
  output$uninsurance_race <- highcharter::renderHighchart({
    
    uninsured_race %>% 
      mutate(percent = round(percent*100, digits=0)) %>% 
      arrange(percent) %>% 
      hchart("pie", 
             hcaes(name=race, y=percent),
             tooltip = list(pointFormat = "% of All Uninsured: {point.y}%")) %>% 
      hc_legend(enabled=FALSE) %>% 
      hc_xAxis(title=list(enabled=FALSE)) %>% 
      hc_yAxis(title = list(text= "% of All Uninsured, Ages 19-64"),
               labels = list(format = '{value}%')) %>% 
      hc_title(text="Race/Ethnicity") %>% 
      hc_subtitle(text="People of color make up 61% of the nonelderly Texas population but account for about 76% of the total nonelderly uninsured population.") %>% 
      highcharter::hc_add_theme(texas2036::tx2036_hc_light())
    
  })
  
  output$uninsurance_income <- highcharter::renderHighchart({
    
    uninsured_income %>% 
      mutate(pct_group = round(pct_group*100, digits=0)) %>% 
      hchart("pie", 
             hcaes(name=group, y=pct_group, color = group),
             tooltip = list(pointFormat = "% of All Uninsured: {point.y}%")) %>% 
      hc_legend(enabled=FALSE) %>% 
      hc_xAxis(title=list(enabled=FALSE)) %>% 
      hc_yAxis(title = list(text= "% of All Uninsured, Ages 19-64"),
               labels = list(format = '{value}%')) %>% 
      hc_title(text="Family Incomes (% of FPL)") %>% 
      hc_subtitle(text="Individuals with income below 200% of the Federal Poverty Level (FPL) are at the highest risk of being uninsured. About 86% of the uninsured were in families with incomes below 400% of poverty.") %>% 
      highcharter::hc_add_theme(texas2036::tx2036_hc_light())
    
  })
  
  output$provider_pcp_chart <- highcharter::renderHighchart({
    
    highchart() %>% 
      hc_add_series(access_pcp %>% filter(state_name!="Texas"), 
                    type="line", 
                    hcaes(x=edition, y=value, group=state_name), 
                    color="#DBDCDD") %>% 
      hc_add_series(access_pcp %>% filter(state_name=="Texas"),
                    type="line", 
                    hcaes(x=edition, y=value),
                    lineWidth=5,
                    name="Texas") %>% 
      hc_title(text="Access To Primary Care Providers") %>%
      hc_subtitle(text="Access to Primary Care Providers Among Texas and Peer States identified by Texas 2036.") %>%
      hc_yAxis(title=list(text="Providers Per 100,000 Population"),
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
               categories = c("2005","2006","2007","2008","2009","2010","2011",
                              "2012","2013","2014","2015","2016","2017","2018","2019"),
               title = list(text = "Year of America's Health Ranking Report")) %>%
      hc_legend(layout = "proximate", align = "right") %>% 
      hc_credits(
        enabled = TRUE,
        text = "America's Health Rankings analysis of Special data request for information on active state licensed physicians provided by Redi-Data.",
        href = "https://datalab.texas2036.org/mskvxdg/america-s-health-rankings-annual-report?accesskey=qtdfqqb") %>%
      hc_add_theme(tx2036_hc_light())
    
  })
  
  output$provider_mh_chart <- highcharter::renderHighchart({
    
    highchart() %>% 
      hc_add_series(access_mh %>% filter(state_name!="Texas"), 
                    type="line", 
                    hcaes(x=edition, y=value, group=state_name), 
                    color="#DBDCDD") %>% 
      hc_add_series(access_mh %>% filter(state_name=="Texas"),
                    type="line", 
                    hcaes(x=edition, y=value),
                    lineWidth=5,
                    name="Texas") %>% 
      # hc_plotOptions(series=list(
      #   cursor = "pointer",
      #   events=list(
      #     mouseOver=JS('function() {
      #                  originalColor = this.color;
      #                  this.update({
      #                  color:"red"})}')
      #   ))) %>% 
      hc_title(text="Access To Mental Health Providers") %>%
      hc_subtitle(text="Access to Mental Health Providers Among Texas and Peer States identified by Texas 2036.") %>%
      hc_yAxis(title=list(text="Providers Per 100,000 Population"),
               labels = list(enabled=TRUE,
                             format = "{value}")) %>% 
      hc_xAxis(tickColor = "#ffffff", 
               min = 0.5,
               max = 1.5,
               tickInterval = 1,
               maxPadding = 0,
               endOnTick = FALSE,
               startOnTick = FALSE,
               useHTML = TRUE,
               alternateGridColor = "#f3f3f3",
               categories = c("2017","2018","2019"),
               title = list(text = "Year of America's Health Ranking Report")) %>%
      hc_legend(layout = "proximate", align = "right") %>% 
      hc_credits(
        enabled = TRUE,
        text = "America's Health Rankings analysis of U.S. HHS, Centers for Medicare & Medicaid Services, National Plan and Provider Enumeration System.",
        href = "https://datalab.texas2036.org/mskvxdg/america-s-health-rankings-annual-report?accesskey=mlrafhb") %>%
      hc_add_theme(tx2036_hc_light())
    
  })
  
  output$prevention_chart_wrk <- renderHighchart({

    prevention_wk %>% 
      hchart("column", hcaes(x=year, y=pct)) %>%
      hc_title(text="Flu Vaccination for Texans Aged 18-64, by Year") %>%
      hc_subtitle(text="Shown: Estimated % of Population Aged 18-64 Reporting 'Yes' to 'Flu Shot in the Past year?'") %>% 
      hc_yAxis(title=list(text="Est. % of Vaccinated Texans, Aged 18-64"),
               min = 0,
               max = 100,
               labels = list(enabled=TRUE,
                             format = "{value}%")) %>%
      hc_xAxis(tickColor = "#ffffff",
               useHTML = TRUE,
               alternateGridColor = "#f3f3f3",
               title = list(text = "Year")) %>%
      hc_legend(enabled = FALSE) %>%
      hc_colorAxis(
        stops = color_stops(10, viridisLite::inferno(8, direction = -1))) %>% 
      hc_credits(
        enabled = TRUE,
        text = "DATA: Texas Behavioral Risk Factor Surveillance System | SOURCE: Texas Department of State Health Services",
        href = "http://healthdata.dshs.texas.gov/dashboard/surveys-and-profiles/behavioral-risk-factor-surveillance-system") %>%
      highcharter::hc_add_theme(texas2036::tx2036_hc_light())
    
  })
  
  outputOptions(output, "barriers_chart", suspendWhenHidden = FALSE)
  
  outputOptions(output, "uninsurance_race", suspendWhenHidden = FALSE)
  
  outputOptions(output, "uninsurance_income", suspendWhenHidden = FALSE)
  
  outputOptions(output, "provider_pcp_chart", suspendWhenHidden = FALSE)
  
  outputOptions(output, "provider_mh_chart", suspendWhenHidden = FALSE)
  
  outputOptions(output, "prevention_chart_wrk", suspendWhenHidden = FALSE)
  
    
  })
  
}