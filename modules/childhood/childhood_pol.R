
# Load Data -----------------------------------------------------------------------------------

child_uninsurance_rates <- read_rds("clean_data/Children/child_uninsurance_rates.rds") %>% 
  select(county, uninsured_percent)

child_uninsured_rates_peers <- read_rds("clean_data/Children/child_uninsured_rates.rds")

primary_care_children_access <- read_rds("clean_data/Children/child_access_to_care.rds") %>%
  select(-value)

childhood_immunization_rates <- read_rds("clean_data/Children/childhood_immunization_rates")

# text module ----
childhood_pol_ui <- function(id) {
  
  fluidRow(
           tabBox(
             id = "tabset1", 
             side="left",
             width = 12,
             tabPanel(title="Uninsurance", 
                      fluidRow(
                      column(width = 6,
                             h2("Uninsurance"),
                             includeMarkdown("markdown/childhood/policy/uninisured.md"),
                             highcharter::highchartOutput(NS(id, "uninsurance_rate_children_peers"), height = "300px")),
                      column(width = 6,
                             highcharter::highchartOutput(NS(id, "uninsurance_rate_children"), height="600px"))),
                      hr(),
                      fluidRow(
                        column(width = 6,
                               h2("Children Eligible for but not Enrolled in Medicaid/CHIP"),
                               includeMarkdown("markdown/childhood/policy/uninsured_strategies.md")),
                        column(width = 6,
                               highcharter::highchartOutput(NS(id, "uninsurance_rate_children_pie"))))),
             tabPanel(title="Provider Access", 
                      fluidRow(
                        column(width = 5,
                               h2("Provider Access"),
                               includeMarkdown("markdown/childhood/policy/access.md")),
                        column(width = 7,
                               highcharter::highchartOutput(NS(id, "access_to_care_children"), height="650px")))),
             tabPanel(title="Prevention", 
                      fluidRow(
                        column(width = 5,
                               h2("Prevention"),
                               includeMarkdown("markdown/childhood/policy/prevention.md")),
                        column(width = 7,
                               highcharter::highchartOutput(NS(id, "prevention_children")))))
             )
           )
  
}

childhood_pol_server <- function(id, df) {
  
  moduleServer(id, function(input, output, session) {
    
  output$uninsurance_rate_children_peers <- highcharter::renderHighchart({
      
    highchart() %>% 
      hc_add_series(child_uninsured_rates_peers %>% filter(state_name!="Texas"), 
                    type="line", 
                    hcaes(x=edition, y=value, group=state_name), 
                    color="#DBDCDD") %>% 
      hc_add_series(child_uninsured_rates_peers %>% filter(state_name=="Texas"),
                    type="line", 
                    hcaes(x=edition, y=value),
                    lineWidth=5,
                    name="Texas") %>% 
      # hc_title(text="Uninsured Rates Among Children") %>%
      hc_subtitle(text="Shown are the rates of uninsured children among Texas and peer states identified by Texas 2036.") %>%
      hc_yAxis(title=list(text="% of Children Who Are Uninsured"),
               labels = list(enabled=TRUE,
                             format = "{value}")) %>% 
      hc_xAxis(tickColor = "#ffffff", 
               # opposite = TRUE,
               # min = 0.5,
               # max = 4.5,
               tickInterval = 1,
               maxPadding = 0,
               endOnTick = FALSE,
               startOnTick = FALSE,
               useHTML = TRUE,
               alternateGridColor = "#f3f3f3",
               categories = c("2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"),
               title = list(text = "Year")) %>%
      hc_legend(layout = "proximate", align = "right") %>% 
      hc_credits(
        enabled = TRUE,
        text = "Health Insurance Coverage Status and Type of Coverage by State, U.S. Census Bureau, 2018",
        href = "https://datalab.texas2036.org/udfegke/health-insurance-coverage-status-and-type-of-coverage-by-state-united-states?accesskey=qfzcrmb") %>%
      hc_add_theme(tx2036_hc_light())
      
    })
    
  output$uninsurance_rate_children <- highcharter::renderHighchart({
      
    col_pal <- RColorBrewer::brewer.pal(9,"Greens")
    
    
    hcmap(map = "countries/us/us-tx-all",
          data = child_uninsurance_rates,
          value = "uninsured_percent",
          joinBy = c("name","county"),
          name = "Percent Uninsured",
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
      hc_title(text="Percentage of Uninsured Children") %>%
      hc_subtitle(text="Percentage of Uninsured Children, 2018") %>%
      hc_credits(
        enabled = TRUE,
        text = "SOURCE: Small Area Health Insurance Estimates (SAHIE) Program",
        href = "https://www.census.gov/data-tools/demo/sahie/#/?s_statefips=48") %>%
      hc_add_theme(tx2036_hc_light())
      
    })
  
  output$uninsurance_rate_children_pie <- highcharter::renderHighchart({
    
    child_uninsured_pie <- tibble(group = c("Medicaid/CHIP-Eligible", "Family Income Exceeds Eligibility Thresholds", "Meets Income Requirements But Ineligible Because of Immigration Status"),
                                  value = c(56.5, 32.8, 10.7)) 
    
    child_uninsured_pie %>% 
      hchart("pie", 
             hcaes(name=group, y=value, color = group),
             tooltip = list(pointFormat = "{point.y}%")) %>% 
      hc_legend(enabled=FALSE) %>% 
      hc_xAxis(title=list(enabled=FALSE)) %>% 
      hc_yAxis(title = list(text= "% of All Uninsured, Ages 19-64"),
               labels = list(format = '{value}%')) %>% 
      hc_title(text="Share of Uninsured Children Eligible for Medicaid/CHIP in 2017") %>% 
      hc_subtitle(text="Individuals with income below 200% of the Federal Poverty Level (FPL)5 are at the highest risk of being uninsured. About 86% of the uninsured were in families with incomes below 400% of poverty.") %>% 
      hc_credits(
        enabled = TRUE,
        text = "SOURCE: Urban Institue Analysis of 2017 American Community Survey data from the Integrated Public Use Microdata Series",
        href = "https://www.urban.org/research/publication/improvements-uninsurance-and-medicaidchip-participation-among-children-and-parents-stalled-2017/view/full_report") %>%
      highcharter::hc_add_theme(texas2036::tx2036_hc_light())
    
  })
  
  output$access_to_care_children <- highcharter::renderHighchart({
    
    col_pal <- RColorBrewer::brewer.pal(11,"RdBu")
    
    hcmap(map = "countries/us/us-all",
          data = primary_care_children_access,
          value = "rank",
          joinBy = c("name","state_name"),
          name = "Rank",
          borderColor = "#FAFAFA",
          borderWidth = 0.1)  %>% 
      hc_legend(layout='vertical',
                align='left',
                verticalAlign='bottom',
                itemMarginTop=10,
                itemMarginBottom=10) %>% 
      hc_colorAxis(stops = color_stops(n=9, colors=rev(col_pal))) %>%
      hc_credits(
        enabled = TRUE,
        text = "America's Health Rankings composite measure, United Health Foundation.",
        href = "https://www.americashealthrankings.org/explore/health-of-women-and-children/measure/access_children/state/ALL?edition-year=2020") %>%
      hc_title(text="National Rankings of Access to Care for Children") %>% 
      hc_subtitle(text="Shown are rankings based on America's Health Rankings sum of weighted 'z-scores' of all ranked access to care measures for children.") %>% 
      hc_add_theme(tx2036_hc_light())
    
  })
  
  output$prevention_children <- highcharter::renderHighchart({
    
    highchart() %>% 
      hc_add_series(childhood_immunization_rates %>% filter(state_name!="Texas"), 
                    type="line", 
                    hcaes(x=edition, y=value, group=state_name), 
                    color="#DBDCDD") %>% 
      hc_add_series(childhood_immunization_rates %>% filter(state_name=="Texas"),
                    type="line", 
                    hcaes(x=edition, y=value),
                    lineWidth=5,
                    name="Texas") %>% 
      hc_title(text="Immunization Rates Among Children") %>%
      hc_subtitle(text="Shown are percentage trends of children ages 19-35 months who received recommended doses of diphtheria, tetanus and acellular pertussis (DTaP); measles, mumps and rubella (MMR); polio; Haemophilus influenzae type b (Hib); hepatitis B; varicella; and pneumococcal conjugate vaccines for Texas and peer states identified by Texas 2036.") %>%
      hc_yAxis(title=list(text="% of children with Immunizations"),
               labels = list(enabled=TRUE,
                             format = "{value}")) %>% 
      hc_xAxis(tickColor = "#ffffff", 
               # opposite = TRUE,
               min = 0.5,
               max = 6.5,
               tickInterval = 1,
               maxPadding = 0,
               endOnTick = FALSE,
               startOnTick = FALSE,
               useHTML = TRUE,
               alternateGridColor = "#f3f3f3",
               categories = c("2012","2013","2014","2015","2016","2017","2018","2019"),
               title = list(text = "Year of America's Health Rankings Report")) %>%
      hc_legend(layout = "proximate", align = "right") %>% 
      hc_credits(
        enabled = TRUE,
        text = "America's Health Rankings analysis of CDC, National Immunization Survey-Child, United Health Foundation.",
        href = "https://www.americashealthrankings.org/explore/annual/measure/Immunize/state/ALL") %>%
      hc_add_theme(tx2036_hc_light())
  
  })
  
  outputOptions(output, "uninsurance_rate_children_peers", suspendWhenHidden = FALSE)
  
  outputOptions(output, "uninsurance_rate_children", suspendWhenHidden = FALSE)
  
  outputOptions(output, "uninsurance_rate_children_pie", suspendWhenHidden = FALSE)
  
  outputOptions(output, "access_to_care_children", suspendWhenHidden = FALSE)
  
  outputOptions(output, "prevention_children", suspendWhenHidden = FALSE)
    
  })
  
}