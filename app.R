# LOAD R PACKAGES -----------------------------------------------------------------------------
 
library(highcharter)
library(gt)
library(reactable)
library(janitor)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(magrittr)
library(metathis)
library(reactable)
library(readxl)
library(scales)
library(sever)
library(shiny)
library(shinyalert)
library(shinydashboard)
library(shinyLP)
library(sf)
library(texas2036)
library(tidyverse)
library(vroom)
library(waiter)
library(zoo)
library(viridisLite)
library(shinyjs)
library(shinyBS)
library(tippy)
library(here)

# HELPER FUNCTIONS ----------------------------------------------------------
  
disconnected <- sever_default(
  title = "Howdy!", 
  subtitle = "There's a lot of data here, so this app has been resting while you were away.", 
  button = "Push to Wake", 
  button_class = "info"
)

thumbnail_label <- function (title, label, content, button_link, button_label) {
  div(class = "row", div(class = "col-sm-14 col-md-12",
                         div(class = "thumbnail", 
                             HTML(title),
                             h3(label), 
                             actionButton(inputId=button_link, 
                                          label=button_label),
                             p(content)
                         )))
}


# LOAD DATASETS -------------------------------------------------------------------------------

# source("load_data.R")
# us_map <- "https://code.highcharts.com/mapdata/countries/us/us-all.js"
# tx_map <- "https://code.highcharts.com/mapdata/countries/us/us-tx-all.js"
# LOAD MODULES --------------------------------------------------------------------------------

## MATERNITY MODULES
source("modules/maternity/maternity_overview.R")
source("modules/maternity/maternity_rf.R")
source("modules/maternity/maternity_cnd.R")
source("modules/maternity/maternity_pol.R")

## CHILDHOOD MODULES
source("modules/childhood/childhood_overview.R")
source("modules/childhood/childhood_rf.R")
source("modules/childhood/childhood_cnd.R")
source("modules/childhood/childhood_pol.R")

## WORKING AGE MODULES
source("modules/working_age/working_overview.R")
source("modules/working_age/working_rf.R")
source("modules/working_age/working_cnd.R")
source("modules/working_age/working_pol.R")

## AGING MODULES
source("modules/aging/aging_overview.R")
source("modules/aging/aging_rf.R")
source("modules/aging/aging_cnd.R")
source("modules/aging/aging_pol.R")

# ## CONDITION MODULES
# # source("modules/aging/diabetes.R")
# # source("modules/aging/mental_health.R")
# # source("modules/aging/heart_health.R")

## COVID MODULES
source("modules/covid/overview.R")

## full application

### app ui

# HEADER CODE  ------------------------------------------------------------

header <- dashboardHeader(disable = FALSE,
                          title = tags$a(href='http://www.texas2036.org',
                                         HTML('<svg viewBox="0 0 227.4 83.5" style="height:4.5vh;padding-bottom:1.1vh;margin-top:7px"><path fill="#fff" d="M192.5 66.2c2.2 0 3.9.6 3.9 2.6v4.1c0 2-1.7 3.6-3.9 3.6-2.1 0-3.8-1.6-3.8-3.6v-5.1h-7.8v5.1c0 5.9 5.2 10.6 11.6 10.6 6.4 0 11.5-4.6 11.7-10.4.6 5.4 5.6 10.4 11.6 10.4 6.4 0 11.6-4.8 11.6-10.6v-7.4c0-5.8-5.2-10.6-11.6-10.6-1.4 0-2.7.2-3.9.6v-4.1c0-1.9 1.8-3.5 3.9-3.5 2.1 0 3.8 1.6 3.8 3.5v2.2h7.8v-2.2c0-4-2.5-7.5-6.1-9.3 3.6-1.8 6.1-5.3 6.1-9.3V10.5c0-5.8-5.2-10.5-11.6-10.5-6.1 0-11.1 4.3-11.6 9.8-.4-5.5-5.5-9.8-11.7-9.8-6.4 0-11.6 4.7-11.6 10.6v2.6h7.8v-2.6c0-1.9 1.7-3.5 3.8-3.5 2.2 0 3.9 1.6 3.9 3.5v.8l-.1.1-13 15.6c-2.3 2.8-2.4 3-2.4 5.9v10.5h4.1c-2.5 1.9-4.1 4.8-4.1 8v2.2h7.8v-2.2c0-1.9 1.7-3.5 3.8-3.5 2.2 0 3.9 1.6 3.9 3.5v4.1c0 2-1.7 3.6-3.9 3.6h-2.4v7.1h2.4zm19.4-55.6c0-1.9 1.7-3.5 3.8-3.5 2.1 0 3.8 1.6 3.8 3.5v20.7c0 2-1.7 3.6-3.8 3.6-2.1 0-3.8-1.6-3.8-3.6V10.6zm-7.8 57c-.3-1.9-1.3-3.3-2.9-5 1.6-1.6 2.6-3.7 2.9-5.9v10.9zm-15.4-32.8v-2.6l13.1-15.8c1.6-1.9 2.2-2.6 2.3-3.8v20.3c0 .5 0 .9.1 1.3l2.1 6.4h6.8l-5.5 4 2.1 6.5-5.5-4-5.5 4 2.1-6.5-5.5-4h6.8l1.9-5.9h-15.3zm30.9 38.1c0 2-1.7 3.6-3.8 3.6-2.2 0-3.9-1.6-3.9-3.6v-7.4c0-1.9 1.8-3.5 3.9-3.5 2.1 0 3.8 1.6 3.8 3.5v7.4zM8.4 82.7V8H0V0h24.8v8h-8.4v74.8h-8zm45.4 0H33V0h20.8v8H41v29.5h12.8v8H41v29.4h12.8v7.8zm70.2 0V45.3h-12.8v37.4h-8V14.4c0-8 6.5-14.4 14.4-14.4 7.8 0 14.3 6.5 14.3 14.4v68.3H124zm0-68.3c0-3.6-2.9-6.5-6.3-6.5-3.6 0-6.5 2.9-6.5 6.5v22.9H124V14.4zm37.6 6.1v-6.2c0-3.5-2.9-6.3-6.3-6.3-3.5 0-6.3 2.9-6.3 6.3v6.3c0 1.5 0 1.5.4 2.1l17.9 31.6c2.4 4.2 2.4 4.2 2.4 7.8v6.2c0 8-6.5 14.3-14.3 14.3S141 76.4 141 68.4v-6.2h8v6.2c0 3.6 2.9 6.5 6.3 6.5 3.5 0 6.3-2.9 6.3-6.5v-6.2c0-1.4 0-1.4-.4-2l-17.9-31.6c-2.4-4.2-2.4-4.2-2.4-8v-6.3C141 6.5 147.5 0 155.3 0s14.3 6.5 14.3 14.3v6.2h-8zM95.9 0h-8.2l-9.2 28.7L69.3 0h-8.2l13.3 41.6L61.1 83h8.3l9.1-28.5L87.6 83h8.3L82.6 41.6z"></path><svg>'),
                                         tags$title('Health Evaluation of Life Phases (HELP)')))
# SIDEBAR CODE  ------------------------------------------------------------
sidebar <- dashboardSidebar(disable = FALSE,
                            # startExpanded = TRUE,
                            tags$head(
                              tags$style(HTML("
                                        .sidebar { height: 90vh; overflow-y: auto; } " )
                              )),
                            sidebarMenu(
                              id = "tabs",
                              menuItem("Introduction",
                                       tabName = "intro", 
                                       selected = TRUE,
                                       icon = icon("square", class="fad fa-square")
                              ),
                              actionLink("button", "LIFE PHASES", class = "btn-section"),
                              menuItem("Maternity", expandedName = "maternity_expand", tabName = "Maternity",
                                       icon = icon("door-open", class="fad fa-baby-carriage"),
                                       menuSubItem('Overview',
                                                   tabName = 'maternity_overview',
                                                   icon = NULL),
                                       menuSubItem('Risk Factors',
                                                   tabName = 'maternity_rf',
                                                   icon = NULL),
                                       menuSubItem('Conditions',
                                                    tabName = 'maternity_cnd',
                                                    icon = NULL),
                                       menuSubItem('Policy & Clinical Care',
                                                    tabName = 'maternity_pol',
                                                    icon = NULL)
                                       ),
                              menuItem("Childhood", expandedName = "childhood_expand", tabName = "Childhood/Adolescence",
                                       icon = icon("door-open", class="fad fa-child"),
                                       menuSubItem('Overview', 
                                                   tabName = 'childhood_overview',
                                                   icon = NULL),
                                       menuSubItem('Risk Factors', 
                                                   tabName = 'childhood_rf',
                                                   icon = NULL),
                                       menuSubItem('Conditions', 
                                                   tabName = 'childhood_cnd',
                                                   icon = NULL),
                                       menuSubItem('Policy & Clinical Care', 
                                                   tabName = 'childhood_pol',
                                                   icon = NULL)),
                              menuItem("Working Age", expandedName = "working_expand", tabName = "Working Age",
                                       icon = icon("door-open", class="fad fa-user-hard-hat"),
                                       menuSubItem('Overview', 
                                                   tabName = 'working_overview',
                                                   icon = NULL),
                                       menuSubItem('Risk Factors', 
                                                   tabName = 'working_rf',
                                                   icon = NULL),
                                       menuSubItem('Conditions', 
                                                   tabName = 'working_cnd',
                                                   icon = NULL),
                                       menuSubItem('Policy & Clinical Care', 
                                                   tabName = 'working_pol',
                                                   icon = NULL)),
                              menuItem("Aging", expandedName = "aging_expand", tabName = "Aging",
                                       icon = icon("door-open", class="fad fa-user-friends"),
                                       menuSubItem('Overview', 
                                                   tabName = 'aging_overview',
                                                   icon = NULL),
                                       menuSubItem('Risk Factors', 
                                                   tabName = 'aging_rf',
                                                   icon = NULL),
                                       menuSubItem('Conditions', 
                                                   tabName = 'aging_cnd',
                                                   icon = NULL),
                                       menuSubItem('Policy & Clinical Care', 
                                                   tabName = 'aging_pol',
                                                   icon = NULL)),
                              actionLink("button", "CONNECTIONS", class = "btn-section shiny-bound-input"),
                              # menuItem("Diabetes",
                              #          tabName = "diabetes", 
                              #          icon = icon("landmark", class="fad fa-tint")),
                              # menuItem("Mental Health",
                              #          tabName = "mental_health", 
                              #          icon = icon("landmark", class="fad fa-head-side-medical")),
                              # menuItem("Heart Health",
                              #          tabName = "heart_health", 
                              #          icon = icon("landmark", class="fad fa-heart")),
                              menuItem("COVID-19",
                                       tabName = "covid", 
                                       icon = icon("city", class="fad fa-virus"))
                              # menuItem("Credits",
                              #          tabName = "credits",
                              #          icon = icon("circle", class="fad fa-circle")),
                              # menuItem("Data",
                              #          tabName = "data",
                              #          icon = icon("circle", class="fad fa-circle")),
                              # actionButton("about", "About"),
                              # actionButton("learn", "Learn More")
                              )
                            )
# BODY CODE  ------------------------------------------------------------
# function to scroll to top of page
jsCode <- "shinyjs.scrolltop = function() {window.scrollTo(0, 0)};" 
body <- dashboardBody(
  tags$head(
    tags$script(src="https://kit.fontawesome.com/8abb217f2e.js", crossorigin="anonymous"),
    tags$link(rel="shortcut icon", href="favicon.png"),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    includeHTML(("www/google_analytics.html")),
    tags$script(HTML("$('body').addClass('fixed');")),
    tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Montserrat:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,400&display=swap"),
  tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Work+Sans:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,400&display=swap")),

  # **Waiter + Meta Page ----------------------------------------------------------
  use_sever(),
  use_waiter(),
  use_hostess(),
  useShinyjs(),
  extendShinyjs(text = jsCode, functions = c("scrolltop")),
  extendShinyjs(text = "shinyjs.activateTab = function(name){
                  setTimeout(function(){
                  $('a[href$=' + '\"#shiny-tab-' + name + '\"' + ']').closest('li').addClass('active')
                  }, 200);

                  }", functions = c("activateTab")),
  
  
  #HTML('<div data-iframe-height></div>'),
  ## Waiter doesn't leave anymore, so I turned it off for now
  # waiter_show_on_load(html = tagList(h4("Thanks for being patient while we get everything set up."),
  #                                    spin_cube_grid()),
  #                     color = "#EAEFF6",
  #                     logo = "logo.png"),
  useShinyalert(),
  meta() %>%
    meta_social(
      title = "Texas 2036 | Health Evaluation of Life Phases",
      description = "A Check-up on The Lone Star State",
      url = "http://healthy.texas2036.org",
      image = "https://texas-2036.github.io/covid-pages/images/trends_cover.png",
      image_alt = "Texas 2036 | Health Evaluation of Life Phases",
      twitter_creator = "@mrworthington",
      twitter_card_type = "summary",
      twitter_site = "@texas2036"
    ),
  # **Landing ----------------------------------------------------------
  tabItems(
    tabItem(tabName = "intro",
            jumbotron("Health Evaluation of Life Phases (HELP)", 
                      "A Check-up on The Lone Star State",
                      button = FALSE),
            hr(class="landing-hr"),
            # hr(style="border-top: 48px solid #fff;"),
            HTML("<i style='color:#F26852;display: block; margin-left: auto; margin-right: auto;margin-top:-64px;margin-bottom: -9px;font-size: 113px;}' class='fad fa-2x fa-heartbeat'></i>"),
            br(),
            fluidRow(
              box(width = 12, solidHeader = FALSE,
                  includeMarkdown("markdown/intro/overview.md"))
            ),
            fluidRow(
              column(6, 
                     
                     thumbnail_label(title="<i class='fad fa-baby-carriage fa-3x' style='color:#EAEFF6'></i>",
                                        label = 'Maternity',
                                        content = includeMarkdown("markdown/intro/maternity.md"),
                                        button_link ='explore_maternity', 
                                        button_label = 'Explore')),
              tippy_this("explore_maternity",
                        "The health of women before, during and after pregnancy is important for healthy
                        birth outcomes and to prevent future health problems for women and their children."),
              
              column(6, 
                     thumbnail_label(title="<i class='fad fa-child fa-3x' style='color:#EAEFF6'></i>", 
                                        label = 'Childhood',
                                        content = includeMarkdown("markdown/intro/childhood.md"),
                                        button_link ='explore_childhood', 
                                        button_label = 'Explore'))),
            tippy_this("explore_childhood",
            "Children’s well-being determines the health of the next generation - healthy children 
                      are more likely to become healthy adults. We want Texas children to be as healthy as possible, and 
                      their health can help predict future health challenges for families, communities, and the health care system."),
            fluidRow(
              column(6, 
                     thumbnail_label(title="<i class='fad fa-user-hard-hat fa-3x' style='color:#EAEFF6'></i>", 
                                        label = 'Working Age',
                                        content = includeMarkdown("markdown/intro/working_age.md"),
                                        button_link ='explore_working', 
                                        button_label = 'Explore')),
              tippy_this("explore_working", 
                        "Working age adults are the backbone of Texas’ economy. They need to be healthy to work and to care for children and older adults."),
              column(6,  
                     thumbnail_label(title="<i class='fad fa-user-friends fa-3x' style='color:#EAEFF6'></i>",
                                        label = 'Aging',
                                        content = includeMarkdown("markdown/intro/aging.md"),
                                        button_link ='explore_aging', 
                                        button_label = 'Explore')),
              tippy_this("explore_aging", 
              "As people age, health care needs increase due to chronic and acute conditions, 
                        but there are still many ways to improve quality of life and reduce health risks for Texas seniors."),
            fluidRow(
              box(width = 12, solidHeader = FALSE,
                      h2("Executive Summary"),
                      includeMarkdown("markdown/intro/executive_summary.md"))),
            )),
    # **Maternity Section ---------------------------------------------------------------------------
    
    tabItem(tabName = "maternity_overview",
            h2("Maternity | Overview", class="page-header1"),
            hr(class="page-header-hr"),
            maternity_overview_ui("maternity_overview")),
    tabItem(tabName = "maternity_rf",
            h2("Maternity | Risk Factors", class="page-header1"),
            hr(class="page-header-hr"),
            maternity_rf_ui("maternity_rf_charts")),
    tabItem(tabName = "maternity_cnd",
             h2("Maternity | Conditions", class="page-header1"),
             hr(class="page-header-hr"),
             maternity_cnd_ui("maternity_cnd_charts")),
     tabItem(tabName = "maternity_pol",
             h2("Maternity | Policy & Clinical Care", class="page-header1"),
             hr(class="page-header-hr"),
             h1("pol_charts"),
             maternity_pol_ui("maternity_pol_charts")),
    # # **Childhood Section ---------------------------------------------------------------------------
    tabItem(tabName = "childhood_overview",
            h2("Childhood/Adolescence | Overview", class="page-header1"),
            hr(class="page-header-hr"),
            childhood_overview_ui("childhood_overview")),
    tabItem(tabName = "childhood_rf",
            h2("Childhood/Adolescence | Risk Factors", class="page-header1"),
            hr(class="page-header-hr"),
            childhood_rf_ui("childhood_rf_charts")),
    tabItem(tabName = "childhood_cnd",
            h2("Childhood/Adolescence | Conditions", class="page-header1"),
            hr(class="page-header-hr"),
            childhood_cnd_ui("childhood_cnd_charts")),
    tabItem(tabName = "childhood_pol",
            h2("Childhood/Adolescence | Policy & Clinical Care", class="page-header1"),
            hr(class="page-header-hr"),
            childhood_pol_ui("childhood_pol_charts")),
    # # **Working Age Section ---------------------------------------------------------------------------
    tabItem(tabName = "working_overview",
            h2("Working Age | Overview", class="page-header1"),
            hr(class="page-header-hr"),
            working_overview_ui("working_overview")),
    tabItem(tabName = "working_rf",
            h2("Working Age | Risk Factors", class="page-header1"),
            hr(class="page-header-hr"),
            working_rf_ui("working_rf_charts")),
    tabItem(tabName = "working_cnd",
            h2("Working Age | Conditions", class="page-header1"),
            hr(class="page-header-hr"),
            working_cnd_ui("working_cnd_charts")),
    tabItem(tabName = "working_pol",
            h2("Working Age | Policy & Clinical Care", class="page-header1"),
            hr(class="page-header-hr"),
            working_pol_ui("working_pol_charts")),
    # # **Aging Section ---------------------------------------------------------------------------
    tabItem(tabName = "aging_overview",
            h2("Aging | Overview", class="page-header1"),
            hr(class="page-header-hr"),
            aging_overview_ui("aging_overview")),
    tabItem(tabName = "aging_rf",
            h2("Aging | Risk Factors", class="page-header1"),
            hr(class="page-header-hr"),
            aging_rf_ui("aging_rf_charts")),
    tabItem(tabName = "aging_cnd",
            h2("Aging | Conditions", class="page-header1"),
            hr(class="page-header-hr"),
            aging_cnd_ui("aging_cnd_charts")),
    tabItem(tabName = "aging_pol",
            h2("Aging | Policy & Clinical Care", class="page-header1"),
            hr(class="page-header-hr"),
            aging_pol_ui("aging_pol_charts")),
    # **Condition - Diabetes ---------------------------------------------------------------------------
    # tabItem(tabName = "diabetes",
    #         h1("diabetes_charts")),
    # # **Condition - Mental Health ---------------------------------------------------------------------------
    # tabItem(tabName = "mental_health",
    #         h1("mh_charts")),
    # # **Condition - Heart Health ---------------------------------------------------------------------------
    # tabItem(tabName = "heart_health",
    #         h1("hh_charts")),
    # **COVID-19 ---------------------------------------------------------------------------
    tabItem(tabName = "covid",
            h2("COVID-19 | Spotlight", class="page-header1"),
            hr(class="page-header-hr"),
            covid_overview_ui("covid_charts"))
    ),
    hr(),
    tags$footer(includeMarkdown("footer.md"), align = "center")
  )

# THE DASHBOARD -----------------------------------------------------

ui <- dashboardPage(title="Texas 2036 | Health of Texans",
                    header = header,
                    sidebar = sidebar,
                    body = body
)


# THE SERVER ------------------------------------------------

server <- function(input, output, session) { 
  
  # App Disconnect Dialogue ----------------------------------------------------------
  
  sever(html = disconnected, bg_color = "#3A4A9F", opacity = .92)
  
  # Tab Switching Functions ---------------------------------------------------------------------
  
  # Navigate from Explore buttons to that tab panel
  observeEvent(input$explore_maternity, {
    updateTabItems(session, "tabs", "maternity_overview")
    # scroll to top of page
    js$scrolltop()
    # open sidebar panel
    #js$activateTab("Maternity")
  })

  observeEvent(input$explore_childhood, {
    updateTabItems(session, "tabs", "childhood_overview")
    # scroll to top of page
    js$scrolltop()
    # open sidebar panel
    #js$activateTab("Childhood")
  })

  observeEvent(input$explore_working, {
    updateTabItems(session, "tabs", "working_overview")
    # scroll to top of page
    js$scrolltop()
    # open sidebar panel
    #js$activateTab("Working Age")
  })

  observeEvent(input$explore_aging, {
    updateTabItems(session, "tabs", "aging_overview")
    # scroll to top of page
    js$scrolltop()
    # open sidebar panel
    #js$activateTab("Aging")
  })

  # When a new section is opened, navigate directly to the overview subsection
  observeEvent(input$sidebarItemExpanded, {
    if(input$sidebarItemExpanded == "maternity_expand"){
      updateTabItems(session, "tabs", selected = "maternity_overview")
    }
  })
  observeEvent(input$sidebarItemExpanded, {
    if(input$sidebarItemExpanded == "childhood_expand"){
      updateTabItems(session, "tabs", selected = "childhood_overview")
    }
  })
  observeEvent(input$sidebarItemExpanded, {
    if(input$sidebarItemExpanded == "working_expand"){
      updateTabItems(session, "tabs", selected = "working_overview")
    }
  })
  observeEvent(input$sidebarItemExpanded, {
    if(input$sidebarItemExpanded == "aging_expand"){
      updateTabItems(session, "tabs", selected = "aging_overview")
    }
  })

  #Sys.sleep(1) # do something that takes time
  waiter_hide()
  
  ## MATERNITY SERVER MODULES
  maternity_overview_server("maternity_overview")
  maternity_rf_server("maternity_rf_charts")
  maternity_cnd_server("maternity_cnd_charts")
  maternity_pol_server("maternity_pol_charts")
  
  ## CHILDHOOD SERVER MODULES
  childhood_overview_server("childhood_overview")
  childhood_rf_server("childhood_rf_charts")
  childhood_cnd_server("childhood_cnd_charts")
  childhood_pol_server("childhood_pol_charts")
  
  ## WORKING SERVER MODULES
  working_overview_server("working_overview")
  working_rf_server("working_rf_charts")
  working_cnd_server("working_cnd_charts")
  working_pol_server("working_pol_charts")
  
  ## AGING SERVER MODULES
  aging_overview_server("aging_overview")
  aging_rf_server("aging_rf_charts")
  aging_cnd_server("aging_cnd_charts")
  aging_pol_server("aging_pol_charts")
  
  # # # ## CONDITIONS SERVER MODULES
  # # diabetes_server("diabetes_charts")
  # # heart_health_server("heart_health_charts")
  # # mental_health_server("heart_health_charts")
  
  ## COVID SERVER MODULE
  covid_overview_server("covid_charts")

}

shinyApp(ui, server)