# Load Data -----------------------------------------------------------------------------------

obesity_trends_women_18_44 <- read_rds("clean_data/maternity/obesity_trends_women_18_44.rds") %>%
  filter(state_name != 'West Virginia')

phys_inactivity_trends_women_18_44 <- read_rds("clean_data/maternity/phys_inactivity_trends_women_18_44.rds") %>%
  filter(state_name != 'West Virginia')

smoking_trends_women_18_44 <- read_rds("clean_data/maternity/smoking_trends_women_18_44.rds")

smoking_maternal <- read_rds("clean_data/maternity/maternal_smoking.rds")


# text module ----
maternity_rf_ui <- function(id) {
  
  fluidRow(
           tabBox(
             id = "tabset1", 
             side="left",
             width = 12,
             # height = 600,
             tabPanel(title="Obesity", 
                      fluidRow(
                      column(width = 5,
                             h2("Obesity"),
                             includeMarkdown("markdown/maternity/risk_factors/obesity.md")),
                      column(width = 7,
                             highcharter::highchartOutput(NS(id, "m_obesity_trends_women_18_44"), height = "600px"))),
                      hr(),
                      fluidRow(
                        column(width = 6,
                               h2("Pre-Pregnancy Obesity"),
                               includeMarkdown("markdown/maternity/risk_factors/pre_pregnancy_obesity.md"),
                               img(src = "figures/maternity/risk_factors/fig_31_maternal_prepregnancy_obesity_by_race.jpg",
                                   width = "90%"),
                               a("SOURCE:  Texas Department of State Health Services, 2019 Healthy Texas Mothers & Babies Data Book", href="https://www.dshs.texas.gov/healthytexasbabies/data.aspx"),
                               a("NOTE: The 2017 and 2018 data are provisional"),
                               includeMarkdown("markdown/maternity/risk_factors/pre_pregnancy_obesity_co.md"),
                        ),
                        column(width = 6,
                               h2("Pre-Pregnancy Obesity Rates"),
                               img(src = "figures/maternity/risk_factors/fig_33_pct_of_births_to_obese_mother.jpg",
                                   width = "90%"),
                               a("SOURCE:  Texas Department of State Health Services, 2019 Healthy Texas Mothers & Babies Data Book", href="https://www.dshs.texas.gov/healthytexasbabies/data.aspx"),
                               includeMarkdown("markdown/maternity/risk_factors/pre_pregnancy_obesity_co_right.md"))
                      )
                      ),
             tabPanel(title="Physical Inactivity",
                      fluidRow(
                        column(width = 5,
                               h2("Physical Inactivity"),
                               includeMarkdown("markdown/maternity/risk_factors/physical_inactivity.md"),
                               includeMarkdown("markdown/maternity/risk_factors/physical_inactivity_co.md")),
                        column(width = 7,
                               highcharter::highchartOutput(NS(id, "m_phys_inactivity_trends_women_18_44"), height = "600px")))),
             tabPanel(title="Smoking",
                      fluidRow(
                        column(width = 5,
                               h2("Smoking"),
                               includeMarkdown("markdown/maternity/risk_factors/smoking.md")),
                        column(width = 7,
                               highcharter::highchartOutput(NS(id, "m_smoking_trends_women_18_44"), height = "500px"),
                               includeMarkdown("markdown/maternity/risk_factors/smoking_co_right.md"))),
                      hr(),
                      fluidRow(
                        column(width = 6,
                               highcharter::highchartOutput(NS(id, "maternal_smoking_trends2"), height = "500px"),
                               includeMarkdown("markdown/maternity/risk_factors/smoking_bottom_co_left.md")),
                        column(width = 6,
                               h2('Percent of Live Births Where Mother Smoked During Pregnancy, 2017'),
                               img(src = "figures/maternity/risk_factors/fig_29_pct_of_births_where_mother_smoked.jpg",
                                   width = "90%"),
                               a("SOURCE:  Texas Department of State Health Services, 2019 Healthy Texas Mothers & Babies Data Book", href="https://www.dshs.texas.gov/healthytexasbabies/data.aspx"),
                               includeMarkdown("markdown/maternity/risk_factors/smoking_bottom_co_right.md"))
                      )
             )
             )
           )
  
}

maternity_rf_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- ","
    options(highcharter.lang = hcoptslang)
    
    myMenuItems <- c("downloadCSV", "downloadXLS","separator", "downloadPNG", "downloadJPEG", "downloadPDF", "downloadSVG")
    
  output$m_obesity_trends_women_18_44 <- highcharter::renderHighchart({
      
    highchart() %>% 
      hc_add_series(obesity_trends_women_18_44 %>% filter(state_name!="Texas"), 
                    type="line", 
                    hcaes(x=edition, y=value, group=state_name, labels = edition), 
                    color="#DBDCDD") %>% 
      hc_add_series(obesity_trends_women_18_44 %>% filter(state_name=="Texas"),
                    type="line", 
                    hcaes(x=edition, y=value, labels = edition),
                    lineWidth=5,
                    name="Texas") %>% 
      hc_title(text="Obesity Trends Among Women Aged 18-44") %>%
      hc_subtitle(text="Shown are trends in Texas and peer states identified by Texas 2036.") %>%
      hc_yAxis(title=list(text="% of Women Aged 18-44"),
               labels = list(enabled=TRUE,
                             format = "{value}%")) %>% 
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
      hc_credits(
        enabled = TRUE,
        text = "America's Health Rankings analysis of CDC, Behavioral Risk Factor Surveillance System.",
        href = "https://datalab.texas2036.org/uxoopxe/health-of-women-and-children-report-for-u-s?accesskey=ydwsfcd") %>%
      hc_add_theme(tx2036_hc_light())
      
    })
  
  output$m_phys_inactivity_trends_women_18_44 <- highcharter::renderHighchart({

    highchart() %>%
      hc_add_series(phys_inactivity_trends_women_18_44 %>% filter(state_name!="Texas"),
                    type="line",
                    hcaes(x=edition, y=value, group=state_name, labels = edition),
                    color="#DBDCDD") %>%
      hc_add_series(phys_inactivity_trends_women_18_44 %>% filter(state_name=="Texas"),
                    type="line",
                    hcaes(x=edition, y=value, labels = edition),
                    lineWidth=5,
                    name="Texas") %>%
      hc_title(text="Physical Inactivity Trends Among Women Aged 18-44") %>%
      hc_subtitle(text="Shown are trends in Texas and peer states identified by Texas 2036.") %>%
      hc_yAxis(title=list(text="% of Women Aged 18-44"),
               labels = list(enabled=TRUE,
                             format = "{value}%")) %>%
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
      hc_credits(
        enabled = TRUE,
        text = "America's Health Rankings analysis of CDC, Behavioral Risk Factor Surveillance System.",
        href = "https://datalab.texas2036.org/uxoopxe/health-of-women-and-children-report-for-u-s?accesskey=rrcnmzd") %>%
      hc_add_theme(tx2036_hc_light())

  })

  output$m_smoking_trends_women_18_44 <- highcharter::renderHighchart({

    highchart() %>%
      hc_add_series(smoking_trends_women_18_44 %>% filter(state_name!="Texas"),
                    type="line",
                    hcaes(x=edition, y=value, group=state_name, labels = edition),
                    color="#DBDCDD") %>%
      hc_add_series(smoking_trends_women_18_44 %>% filter(state_name=="Texas"),
                    type="line",
                    hcaes(x=edition, y=value, labels = edition),
                    lineWidth=5,
                    name="Texas") %>%
      hc_title(text="Smoking Trends Among Women Aged 18-44") %>%
      hc_subtitle(text="Shown are trends in Texas and peer states identified by Texas 2036.") %>%
      hc_yAxis(title=list(text="% of Women Aged 18-44"),
               labels = list(enabled=TRUE,
                             format = "{value}")) %>%
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
      hc_credits(
        enabled = TRUE,
        text = "SOURCE: America's Health Rankings analysis of CDC, Behavioral Risk Factor Surveillance System.",
        href = "https://datalab.texas2036.org/uxoopxe/health-of-women-and-children-report-for-u-s?accesskey=zngcflg") %>%
      hc_add_theme(tx2036_hc_light())

  })

  output$maternal_smoking_trends <- highcharter::renderHighchart({

    highchart() %>%
      hc_add_series(smoking_maternal %>% filter(race != "Texas"),
                    type = "line",
                    hcaes(x=year, y=value, group=race, labels = year)) %>%
      hc_add_series(smoking_maternal %>% filter(race == 'Texas'),
                    type = 'line',
                    hcaes(x=year, y=value, labels = year),
                    lineWidth=5,
                    name="Texas") %>%
      hc_title(text="Percent of Live Births in Texas Where Mother Smoked During Pregnancy") %>%
      hc_yAxis(title=list(text="Percentage of Live Births"),
               labels = list(enabled=TRUE,
                             format = "{value}%")) %>%
      hc_xAxis(tickColor = "#ffffff",
               tickInterval = 1,
               maxPadding = 0,
               endOnTick = FALSE,
               startOnTick = FALSE,
               useHTML = TRUE,
               alternateGridColor = "#f3f3f3",
               title = list(text = "Year")) %>%
      hc_tooltip(valueSuffix = "%") %>%
      hc_legend(layout = "proximate", align = "right") %>%
      hc_credits(
        enabled = TRUE,
        text = "NOTE: The 2017 and 2018 data are provisional. | SOURCE: Texas Department of State Health Services, 2019 Healthy Texas Mothers & Babies Data Book.",
        href = "https://www.dshs.texas.gov/healthytexasbabies/data.aspx") %>%
      hc_add_theme(tx2036_hc_light())
  })
  
  ## THIS PLOT IS CALLED TWICE (OBESITY AND SMOKING TAB) - NEED TO MAKE TWO DIFFERENT VERSIONS OR EVERYTHING BREAKS
  output$maternal_smoking_trends2 <- highcharter::renderHighchart({
    
    highchart() %>%
      hc_add_series(smoking_maternal %>% filter(race != "Texas"),
                    type = "line",
                    hcaes(x=year, y=value, group=race, labels = year)) %>%
      hc_add_series(smoking_maternal %>% filter(race == 'Texas'),
                    type = 'line',
                    hcaes(x=year, y=value, labels = year),
                    lineWidth=5,
                    name="Texas") %>%
      hc_title(text="Percent of Live Births in Texas Where Mother Smoked During Pregnancy") %>%
      hc_yAxis(title=list(text="Percentage of Live Births"),
               labels = list(enabled=TRUE,
                             format = "{value}%")) %>%
      hc_xAxis(tickColor = "#ffffff",
               tickInterval = 1,
               maxPadding = 0,
               endOnTick = FALSE,
               startOnTick = FALSE,
               useHTML = TRUE,
               alternateGridColor = "#f3f3f3",
               title = list(text = "Year")) %>%
      hc_tooltip(valueSuffix = "%") %>%
      hc_legend(layout = "proximate", align = "right") %>%
      hc_credits(
        enabled = TRUE,
        text = "NOTE: The 2017 and 2018 data are provisional. | SOURCE: Texas Department of State Health Services, 2019 Healthy Texas Mothers & Babies Data Book.",
        href = "https://www.dshs.texas.gov/healthytexasbabies/data.aspx") %>%
      hc_add_theme(tx2036_hc_light())
  })
    
  })
  
}