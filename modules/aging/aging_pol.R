# Load Data -----------------------------------------------------------------------------------

access_to_hc <- read_rds("clean_data/Aging/policy/access_pcp.rds")

prevention <- read_rds("clean_data/Aging/policy/immunizations.rds")

nh_quality_peers <- read_rds("clean_data/Aging/policy/nh_quality_peers.rds")

nh_quality <- read_rds("clean_data/Aging/policy/nh_quality.rds")

# text module ----
aging_pol_ui <- function(id) {
  
  fluidRow(
           tabBox(
             title = NULL,
             id = "tabset1", 
             side="left",
             width = 12,
             tabPanel(title="Access To Care", 
                      fluidRow(
                      column(width = 6,
                             h2("Access To Care"),
                             includeMarkdown("markdown/aging/policy/access.md")),
                      column(width = 6,
                             highcharter::highchartOutput(NS(id, "access_chart"), height="550px")))),
             tabPanel("Prevention", 
                      fluidRow(
                        column(width = 12,
                               h2("Prevention"),
                               includeMarkdown("markdown/aging/policy/prevention.md")),
                        ),
                      hr(),
                      fluidRow(
                        column(width = 12,
                               div(class = "reactable2036-theme",
                                   div(class = "reactable2036-header",
                                       div(class = "reactable2036-title", 
                                           "Vaccination Trends Among Seniors In The United States"),
                                       div(class = "reactable2036-subtitle", 
                                           "Shown are the percentages of those aged 65 years and older who received a given vaccine within each state. While the data was reported in 2020 by America's Health Rankings, the latest Flu + Pneumonia rates come from 2018 surveys by the CDC while the latest Shingles rates come from 2017 surveys by the CDC."),
                                   reactableOutput(NS(id, "prevention_chart")),
                                   div(class = "reactable2036-caption", 
                                       a("SOURCE: America's Health Rankings analysis of CDC, Behavioral Risk Factor Surveillance System.", href = "https://www.americashealthrankings.org/explore/senior/measure/immunizations_sr/state/ALL"))
                               ))
                      ))),
             tabPanel("Nursing Home Quality",  
                      fluidRow(
                        column(width = 12,
                               h2("Nursing Home Quality"),
                               includeMarkdown("markdown/aging/policy/nh_quality.md"))),
                      fluidRow(
                        column(width = 6,
                               highcharter::highchartOutput(NS(id, "nh_quality_chart"), height="550px")),
                        column(width = 6,
                               highcharter::highchartOutput(NS(id, "nh_quality_map"), height="550px"))))
             )
           )
  
}

aging_pol_server <- function(id, df) {
  
  moduleServer(id, function(input, output, session) {
    
  output$access_chart <- highcharter::renderHighchart({
      
    highchart() %>% 
      hc_add_series(access_to_hc %>% filter(states!="Texas"), 
                    type="line", 
                    hcaes(x=edition, y=value, group=states), 
                    lineWidth=1,
                    color="#8C8F93") %>% 
      hc_add_series(access_to_hc %>% filter(states=="Texas"),
                    type="line", 
                    hcaes(x=edition, y=value),
                    lineWidth=5,
                    name="Texas") %>% 
      hc_title(text="Dedicated Health Care Access Among Seniors") %>%
      hc_subtitle(text="The chart below shows trends in the percentage of adults ages 65 and older who reported having one or more people they think of as their personal doctor or health care provider. Shown are Texas and Peer States identified by Texas 2036.") %>%
      hc_yAxis(title=list(text="% of Seniors Reporting Frequent Mental Distress"),
               labels = list(enabled=TRUE,
                             format = "{value}%")) %>% 
      hc_legend(layout = "proximate", align = "right") %>%
      hc_xAxis(tickColor = "#ffffff", 
               min = 0.5,
               max = 6.5,
               tickInterval = 1,
               maxPadding = 0,
               endOnTick = FALSE,
               startOnTick = FALSE,
               useHTML = TRUE,
               categories = c("2012","2013","2014","2015","2016","2017","2018","2019"),
               title = list(text = "Year of America's Health Ranking Report")) %>%
      hc_credits(
        enabled = TRUE,
        text = "America's Health Rankings analysis of CDC, Behavioral Risk Factor Surveillance System.",
        href = "https://datalab.texas2036.org/mskvxdg/america-s-health-rankings-annual-report?accesskey=uamzsne") %>%
      hc_add_theme(tx2036_hc_light())
      
    })
  
  output$prevention_chart <- renderReactable({
    
    bar_chart <- function(label, width = "100%", height = "14px", fill = "#00bfc4", background = NULL) {
      bar <- div(style = list(background = fill, width = width, height = height))
      chart <- div(style = list(flexGrow = 1, marginLeft = "6px", background = background), bar)
      div(style = list(display = "flex", alignItems = "center"), label, chart)
    }
    
    prevention_tbl <- prevention %>% 
      select(state= state_name, measure_name, value) %>% 
      mutate(measure_name = str_remove(measure_name, " Vaccination - Ages 65\\+"),
             path = "figures/flags/",
             flag_file = paste0(tolower(state),".png"),
             flag_file = str_replace(flag_file, " ", "_")) %>% 
      pivot_wider(names_from = measure_name, values_from=value) %>% 
      arrange(desc(Flu)) 
    
    prevention_tbl %>% 
      reactable(
        searchable = FALSE,
        showSortIcon = TRUE,
        selection = "multiple",
        defaultSelected = c(10,13),
        striped = FALSE,
        highlight = TRUE,
        bordered = FALSE,
        pagination = FALSE,
        fullWidth = TRUE,
        compact = TRUE,
        width = "auto",
        height = "auto",
        theme = reactableTheme(
          rowSelectedStyle = list(fontWeight="800"),
          color = "#2d2d2d",
          backgroundColor = "#fff",
          borderColor = "#f4f4f4",
          headerStyle = list(background = "#EAEFF6", fontWeight="800", letterSpacing="1px",
                             color = "#002D74",
                             textTransform="Uppercase",
                             fontSize="14px"),
          stripedColor = "#fff",
          highlightColor = "#2A7DE1",
          cellPadding = "8px 12px",
          style = list(fontFamily = "'Montserrat', -apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
          searchInputStyle = list(width = "100%")),
        columns = list(
          state = colDef(name = "State",
                         class = "border-right",
                         cell = function(value, index) {
                           div(
                             class = "states",
                             # img(class = "flag", 
                             #     alt = paste(value, "flag"), 
                             #     src = sprintf("figures/flags/%s.png", value)),
                             div(class = "states-name", value),
                             )
                         },
                         width = 300),
          path = colDef(show = FALSE),
          flag_file = colDef(show = FALSE),
          Flu = colDef(name = "Flu",
                       defaultSortOrder = "desc",
                       cell = function(value) {
                         value <- paste0(format(value, nsmall = 1), "%")
                         # Fix width here to align single and double-digit percentages
                         value <- format(value, width = 5, justify = "right")
                         bar_chart(value, width = value, fill = "#3A4A9F", background = "#e1e1e1")
                         # bar_chart(value, width = value, fill = "#3A4A9F", background = "#e1e1e1")
                         # bar_chart(value, width = width, fill = "#3A4A9F", background = "#e1e1e1")
                       },
                       align = "left",
                       style = list(fontFamily = "SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono','Courier New', monospace", whiteSpace = "pre")),
          Pneumonia = colDef(name = "Pneumonia",
                             defaultSortOrder = "desc",
                             cell = function(value) {
                               value <- paste0(format(value, nsmall = 1), "%")
                               # Fix width here to align single and double-digit percentages
                               value <- format(value, width = 5, justify = "right")
                               bar_chart(value, width = value, fill = "#00A9C5", background = "#e1e1e1")
                               # bar_chart(value, width = width, fill = "#00A9C5",, background = "#e1e1e1")
                             },
                             align = "left",
                             style = list(fontFamily = "SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono','Courier New', monospace", whiteSpace = "pre")),
          Shingles = colDef(name = "Shingles",
                            defaultSortOrder = "desc",
                            cell = function(value) {
                              value <- paste0(format(value, nsmall = 1), "%")
                              # Fix width here to align single and double-digit percentages
                              value <- format(value, width = 5, justify = "right")
                              bar_chart(value, width = value, fill = "#F26852", background = "#e1e1e1")
                              # bar_chart(value, width = width, fill = "#F26852", background = "#e1e1e1")
                            },
                            align = "left",
                            style = list(fontFamily = "SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono','Courier New', monospace", whiteSpace = "pre"))))
    
  })
  
  output$nh_quality_chart <- highcharter::renderHighchart({
    
     highchart() %>% 
      hc_add_series(nh_quality_peers %>% filter(state_name!="Texas"), 
                    type="line", 
                    hcaes(x=edition, y=value, group=state_name), 
                    color="#DBDCDD") %>% 
      hc_add_series(nh_quality_peers %>% filter(state_name=="Texas"),
                    type="line", 
                    hcaes(x=edition, y=value),
                    lineWidth=5,
                    name="Texas") %>% 
      # hc_title(text="Nursing Home Quality Trends") %>%
      hc_subtitle(text="Nursing Home Quality Trends Among Texas and Peer States identified by Texas 2036.") %>%
      hc_yAxis(title=list(text="% of High Quality Nursing Home Beds"),
               labels = list(enabled=TRUE,
                             format = "{value}%")) %>% 
      hc_xAxis(tickColor = "#ffffff", 
               # opposite = TRUE,
               min = 0.5,
               max = 2.5,
               tickInterval = 1,
               maxPadding = 0,
               endOnTick = FALSE,
               startOnTick = FALSE,
               useHTML = TRUE,
               alternateGridColor = "#f3f3f3",
               categories = c("2017","2018","2019","2020"),
               title = list(text = "Year of America's Health Ranking Report")) %>%
      hc_legend(layout = "proximate", align = "right") %>% 
      hc_credits(
        enabled = TRUE,
        text = "America's Health Rankings analysis of U.S. HHS, Centers for Medicare & Medicaid Services, Nursing Home Compare.",
        href = "https://datalab.texas2036.org/ljosakb/america-s-health-rankings-senior-report?accesskey=qbwgcag") %>%
      hc_add_theme(tx2036_hc_light())
    
  })
  
  output$nh_quality_map <- highcharter::renderHighchart({
    
    col_pal <- RColorBrewer::brewer.pal(9,"PuBu")
    
    hcmap(map = "countries/us/us-all",
          data = nh_quality,
          value = "value",
          joinBy = c("name","state_name"),
          name = "% of High Quality Nursing Home Beds",
          borderColor = "#4d4d4d",
          borderWidth = 0.2,
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
        text = "SOURCE: Interactive Atlas of Heart Disease and Stroke, by Center for Disease Control and Prevention.",
        href = "https://nccd.cdc.gov/DHDSPAtlas/?state=State") %>%
      hc_title(text="Nursing Home Quality In The United States") %>% 
      hc_subtitle(text="Shown are the percentage of certified nursing home beds rated four- or five-stars over a three-month period in each state.") %>% 
      highcharter::hc_add_theme(texas2036::tx2036_hc_light())
    
  })
  
  outputOptions(output, "access_chart", suspendWhenHidden = FALSE)

  outputOptions(output, "prevention_chart", suspendWhenHidden = FALSE)

  outputOptions(output, "nh_quality_chart", suspendWhenHidden = FALSE)
  
  # outputOptions(output, c("access_chart", "prevention_chart","nh_quality_chart"), suspendWhenHidden = FALSE)
  
    
  })
  
}