#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(highcharter)
library(dplyr)
library(dbplyr)
library(xts)
library(tidyverse)
library(shinydashboard)
library(shinyjs)
library(shinycustomloader)
library(shinyBS)
library(config)
library(tidyquant)
library(shiny.i18n)
library(httr)
library(future)
library(promises)
library(readxl)
library(logger)
library(shinyWidgets)
library(DT)

plan(multisession)
log_threshold(TRACE)
log_layout(layout_glue_colors)
shinyOptions(cache = cachem::cache_disk("./app_cache/cache/"))

i18n <- Translator$new(translation_json_path = "www/translation_fr.json")
i18n$set_translation_language("en")


dashboard_ui_slider_date_end <- Sys.Date()
dashboard_ui_slider_date_start <- paste(Sys.Date()-7,"00:00:00",sep=" ")



ui <- dashboardPage(
  
  dashboardHeader(title = "CCEI-HFED DashBoard"),
  dashboardSidebar(
    usei18n(i18n),
    collapsed = TRUE,
    sidebarMenu( id = "rted_menu",
                 menuItem(i18n$t("Dashboard"), tabName = "dashboard", icon = icon("chart-line")),
                 menuItem(i18n$t("Newfoundland & Labrador"),tabName = "NFL", icon = icon("chart-bar")),
                 menuItem(i18n$t("Prince Edward Island"),tabName = "pei", icon = icon("chart-bar")),
                 menuItem(i18n$t("Nova Scotia"),tabName = "NS", icon = icon("chart-bar")),
                 menuItem(i18n$t("New Brunswick"),tabName = "NB", icon = icon("chart-bar")),
                 menuItem(i18n$t("Quebec"),tabName = "QB", icon = icon("chart-bar")),
                 menuItem(i18n$t("Ontario"),tabName = "ON", icon = icon("chart-bar")),
                 menuItem(i18n$t("Alberta"),tabName = "AB", icon = icon("chart-bar")),
                 menuItem(i18n$t("British Columbia"),tabName = "BC", icon = icon("chart-bar")),
                 menuItem(i18n$t("Downloads"),tabName = "Dwn", icon = icon("download")),
                 menuItem(i18n$t("Data Dictionary"),tabName = "DD", icon = icon("book")),
                 menuItem(i18n$t("Settings"),tabName = "ST", icon = icon("tools"))
    )
  ),
  dashboardBody(
    
    shinyjs::useShinyjs(),
    usei18n(i18n),
    tags$style(HTML("


.box.box-solid.box-success>.box-header {
  color:#fff;
  background:#26374A
                    }

.box.box-solid.box-success{
border-bottom-color:#26374A;
border-left-color:#26374A;
border-right-color:#26374A;
border-top-color:#26374A;
box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
transition: 0.3s;
}

.box.box-solid.box-success:hover{
box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2);
}

.box.box-solid.box-warning>.box-header {
  color:#fff;
  background:#26374A
                    }

.box.box-solid.box-warning{
border-bottom-color:#ecf0f5;
border-left-color:#ecf0f5;
border-right-color:#ecf0f5;
border-top-color:#26374A;
}

.skin-blue .main-header .navbar, .skin-blue .main-header .logo{
color:#fff;
background:#26374A
}

#live_clock{
text-align: center;
}

#nb_filter{
width: 100px;
text-align: center;
}

#content_para{
text-align: justify;
text-justify: inter-word;
}

#NB_1{
margin-top: 15px;
margin-bottom : -15px;
}

#dash_title{
margin-left: 10px;
}

#hlp_txt_dwn{
margin-left: 5px;
}

.red_output{
color: white;
font-family: none;
}

.green_output{
color: white;
font-family: none;
}

#dashboard_page{
margin-left: 25px;
margin-right: 25px;

}

#shiny-tab-dashboard{

}

#hl{
float:right;
}

#header_title{
float:left;
}


#lang_btn{
float:right;
}

#bd_twr{
float:right;
padding-left:10px;
animation: blink-animation 1s steps(5, start) infinite;
        -webkit-animation: blink-animation 1s steps(5, start) infinite;
        
}
@keyframes blink-animation {
  to {
    visibility: hidden;
  }
}
@-webkit-keyframes blink-animation {
  to {
    visibility: hidden;
  }
}

#server_stat {
  font-family: Arial, Helvetica, sans-serif;
  border-collapse: collapse;
  width: 100%;
}

#server_stat td, #server_stat th {
  border: 1px solid #ddd;
  padding: 8px;
}

#server_stat tr:nth-child(even){background-color: #f2f2f2;}

#server_stat tr:hover {background-color: #ddd;}

#server_stat th {
  padding-top: 12px;
  padding-bottom: 12px;
  text-align: left;
  background-color: #04AA6D;
  color: white;
}

#bc_er_txt{
color: red;
font-style: italic;
}






                                    ")),
    tabItems(
      tabItem(tabName = "dashboard",
              fluidPage( id = "dashboard_page",
                         fluidRow(div(h1(id = "header_title",i18n$t("High-frequency electricity data (HFED): Interactive dashboard")),
                                      div(id = "lang_btn",
                                          bsButton("Btn_EN","English",icon = icon("language"),style = "primary",size = "small",type = "action"),
                                          bsButton("Btn_FR","French",icon = icon("language"),style = "primary",size = "small",type = "action")))),
                         br(),
                         
                         fluidRow(h3(id = "content_para",p("The HFED dashboard makes it easy to access important Canadian electricity information and supports Canada's path to net-zero emissions by providing high-quality electricity data in near real-time."), 
                                     p("Developed by the Canadian Center for Energy Information (CCEI) in collaboration with Natural Resources Canada and the Canadian Energy Regulator, the HFED dashboard uses web scraping to gather publicly-available electricity data from provincial and territorial utilities across Canada. These data are then consolidated into a standardized central database for easy access and analysis, including historical data for select provinces."),
                                     p("On the dashboard landing page below, data are displayed for available provinces and territories at the hourly level, with new data points being added as they become available. To download data files and access more detailed information about a particular province or territory (e.g. fuel type, imports and exports, and wind percentage) simply select that region."),
                                     p("For more energy information, check out",a(href="https://energy-information.canada.ca/"," Canadian Center for Energy Information"), "- a convenient one-stop virtual shop for independent and trusted information on energy in Canada.")
                         )),
                         br(),
                         fluidRow(box(title = HTML(paste("  <i id='bd_twr' class='fas fa-broadcast-tower'></i>",i18n$t("Live Data"))),id="live_dashboard", status = "primary", width="100%" ,solidHeader = TRUE,
                                      fluidRow(
                                        box(
                                          title = fluidRow(id = "dash_title",
                                                           fluidRow(column(width = 8, h4(i18n$t("Newfoundland & Labrador"))),column(width = 12, id="nflmean",uiOutput("NFL_MEAN")))),
                                          width = 6, status = "success", solidHeader = TRUE,
                                          collapsible = TRUE,
                                          column(
                                            fluidRow(withLoader(uiOutput("NFL_load"), type = "html", loader = "loader3")),
                                            fluidRow(bsButton("button_NFL","For more detailed information, click here", icon = icon("chart-bar"), style = "primary", block = TRUE)),width = 12)
                                        ),
                                        box(
                                          title = fluidRow(id = "dash_title",
                                                           fluidRow(column(width = 8, h4(i18n$t("Prince Edward Island"))),column(width = 12, id="peimean",uiOutput("PEI_MEAN")))),
                                          width = 6, status = "success", solidHeader = TRUE,
                                          collapsible = TRUE,
                                          column(
                                            fluidRow(withLoader(uiOutput("PEI_load"), type = "html", loader = "loader3")),
                                            fluidRow(bsButton("button_pei","For more detailed information, click here", icon = icon("chart-bar"), style = "primary", block = TRUE)),width = 12)),
                                      ),
                                      fluidRow(
                                        box(
                                          title = fluidRow(id = "dash_title",
                                                           fluidRow(column(width = 8, h4(i18n$t("Nova Scotia"))),column(width = 12, id="nsmean",uiOutput("NS_MEAN")))),
                                          width = 6, status = "success", solidHeader = TRUE,
                                          collapsible = TRUE,
                                          column(
                                            fluidRow(withLoader(uiOutput("NS_load"), type = "html", loader = "loader3")),
                                            fluidRow(bsButton("button_NS","For more detailed information, click here", icon = icon("chart-bar"), style = "primary", block = TRUE)),width = 12)),
                                        box(
                                          title = fluidRow(id = "dash_title",
                                                           fluidRow(column(width = 8, h4(i18n$t("New Brunswick"))),column(width = 12, id="nbmean",uiOutput("NB_MEAN")))),
                                          width = 6, status = "success", solidHeader = TRUE,
                                          collapsible = TRUE,
                                          column(
                                            fluidRow(withLoader(uiOutput("NB_load"), type = "html", loader = "loader3")),
                                            fluidRow(bsButton("button_NB","For more detailed information, click here", icon = icon("chart-bar"), style = "primary", block = TRUE)),width = 12)),
                                      ),
                                      fluidRow(
                                        box(
                                          title = fluidRow(id = "dash_title",
                                                           fluidRow(column(width = 8, h4(i18n$t("Quebec"))),column(width = 12, id="qbmean",uiOutput("QB_MEAN")))),
                                          width = 6, status = "success", solidHeader = TRUE,
                                          collapsible = TRUE,
                                          column(
                                            fluidRow(withLoader(uiOutput("QB_load"), type = "html", loader = "loader3")),
                                            fluidRow(bsButton("button_QB","For more detailed information, click here", icon = icon("chart-bar"), style = "primary", block = TRUE)),width = 12)),
                                        box(
                                          title = fluidRow(id = "dash_title",
                                                           fluidRow(column(width = 8, h4(i18n$t("Ontario"))),column(width = 12, id="onmean",uiOutput("ON_MEAN")))),
                                          width = 6, status = "success", solidHeader = TRUE,
                                          collapsible = TRUE,
                                          column(
                                            fluidRow(withLoader(uiOutput("ON_load"), type = "html", loader = "loader3")),
                                            fluidRow(bsButton("button_ON","For more detailed information, click here", icon = icon("chart-bar"), style = "primary", block = TRUE)),width = 12)),
                                      ),
                                      fluidRow(
                                        box(
                                          title = fluidRow(id = "dash_title",
                                                           fluidRow(column(width = 8, h4(i18n$t("Alberta"))),column(width =12, id="abmean",uiOutput("AB_MEAN")))),
                                          width = 6, status = "success", solidHeader = TRUE,
                                          collapsible = TRUE,
                                          column(
                                            fluidRow(withLoader(uiOutput("AB_load"), type = "html", loader = "loader3")),
                                            fluidRow(bsButton("button_AB","For more detailed information, click here", icon = icon("chart-bar"), style = "primary", block = TRUE)),width = 12)),
                                        box(
                                          title = fluidRow(id = "dash_title",
                                                           fluidRow(column(width = 8, h4(i18n$t("British Columbia"))),column(width = 12, id="bcmean",uiOutput("BC_MEAN")))),
                                          width = 6, status = "success", solidHeader = TRUE,
                                          collapsible = TRUE,
                                          column(
                                            fluidRow(withLoader(uiOutput("BC_load"), type = "html", loader = "loader3")),
                                            fluidRow(bsButton("button_BC","For more detailed information, click here", icon = icon("chart-bar"), style = "primary", block = TRUE)),width = 12)),
                                      ))))),
      
      tabItem(tabName = "pei",
              fluidPage(id = "dashboard_page",
                        fluidRow(div(h2(id = "header_title",i18n$t("Prince Edward Island")),
                                     div(id = "lang_btn",
                                         bsButton("Btn_EN_pei","English",icon = icon("language"),style = "primary",size = "small",type = "action"),
                                         bsButton("Btn_FR_pei","French",icon = icon("language"),style = "primary",size = "small",type = "action")))),
                        
                        br(),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = i18n$t("Load"), width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_pei", h4(i18n$t("Frequency")), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3,conditionalPanel(condition = "input.select_fr_pei != 1", sliderInput("pei_dates_1",
                                                                                                                                                "Dates",
                                                                                                                                                min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                timeFormat="%Y-%m-%d")))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_pei == 1",
                                                     fluidRow(withLoader(uiOutput("PEI_ON_LOAD_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_pei == 6",
                                                     fluidRow(withLoader(uiOutput("PEI_ON_LOAD_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_pei == 3",
                                                     fluidRow(withLoader(uiOutput("PEI_ON_LOAD_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_pei == 4",
                                                     fluidRow(withLoader(uiOutput("PEI_ON_LOAD_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_pei == 5",
                                                     fluidRow(withLoader(uiOutput("PEI_ON_LOAD_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = i18n$t("Fuel Type"), width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_pei_2", h4("Frequency"),
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_pei_2 != 1",sliderInput("pei_dates_2",
                                                                                                                                                  "Dates",
                                                                                                                                                  min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                  max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                  value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                  timeFormat="%Y-%m-%d")))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_pei_2 == 1",
                                                     fluidRow(withLoader(uiOutput("PEI_ON_WIND_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_pei_2 == 6",
                                                     fluidRow(withLoader(uiOutput("PEI_ON_WIND_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_pei_2 == 3",
                                                     fluidRow(withLoader(uiOutput("PEI_ON_WIND_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_pei_2 == 4",
                                                     fluidRow(withLoader(uiOutput("PEI_ON_WIND_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_pei_2 == 5",
                                                     fluidRow(withLoader(uiOutput("PEI_ON_WIND_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                         fluidRow(
                           column(width =10, offset = 2,
                                  box(
                                    title = "Domestic & Export", width = 10, status = "warning", solidHeader = TRUE,
                                    collapsible = TRUE,
                                    fluidRow(column(width = 2, selectInput("select_fr_pei_3", h4("Frequency"), 
                                                                           choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                             column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_pei_3 != 1",sliderInput("pei_dates_3",
                                                                                                                                                   "Dates",
                                                                                                                                                   min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                   max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                   value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                   timeFormat="%Y-%m-%d")))
                                    ),
                                    conditionalPanel( condition = "input.select_fr_pei_3 == 1",
                                                      fluidRow(withLoader(uiOutput("PEI_EXPORT_YEARLY"),type = "html", loader = "loader3"))),
                                    conditionalPanel( condition = "input.select_fr_pei_3 == 6",
                                                      fluidRow(withLoader(uiOutput("PEI_EXPORT_ALL"),type = "html", loader = "loader3"))),
                                    conditionalPanel( condition = "input.select_fr_pei_3 == 3",
                                                      fluidRow(withLoader(uiOutput("PEI_EXPORT_WEEKLY"),type = "html", loader = "loader3"))),
                                    conditionalPanel( condition = "input.select_fr_pei_3 == 4",
                                                      fluidRow(withLoader(uiOutput("PEI_EXPORT_DAILY"),type = "html", loader = "loader3"))),
                                    conditionalPanel( condition = "input.select_fr_pei_3 == 5",
                                                      fluidRow(withLoader(uiOutput("PEI_EXPORT_HOURLY"),type = "html", loader = "loader3"))),
                                    fluidRow(
                                      column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                      column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                    )
                                  ))),
                         fluidRow(
                           column(width =10, offset = 2,
                                  box(
                                    title = "Wind Percentage", width = 10, status = "warning", solidHeader = TRUE,
                                    collapsible = TRUE,
                                    fluidRow(column(width = 2, selectInput("select_fr_pei_4", h4("Frequency"), 
                                                                           choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                             column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_pei_4 != 1",sliderInput("pei_dates_4",
                                                                                                                                                   "Dates",
                                                                                                                                                   min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                   max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                   value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                   timeFormat="%Y-%m-%d")))
                                    ),
                                    conditionalPanel( condition = "input.select_fr_pei_4 == 1",
                                                      fluidRow(withLoader(uiOutput("PEI_LOCAL_YEARLY"),type = "html", loader = "loader3"))),
                                    conditionalPanel( condition = "input.select_fr_pei_4 == 6",
                                                      fluidRow(withLoader(uiOutput("PEI_LOCAL_ALL"),type = "html", loader = "loader3"))),
                                    conditionalPanel( condition = "input.select_fr_pei_4 == 3",
                                                      fluidRow(withLoader(uiOutput("PEI_LOCAL_WEEKLY"),type = "html", loader = "loader3"))),
                                    conditionalPanel( condition = "input.select_fr_pei_4 == 4",
                                                      fluidRow(withLoader(uiOutput("PEI_LOCAL_DAILY"),type = "html", loader = "loader3"))),
                                    conditionalPanel( condition = "input.select_fr_pei_4 == 5",
                                                      fluidRow(withLoader(uiOutput("PEI_LOCAL_HOURLY"),type = "html", loader = "loader3"))),
                                    fluidRow(
                                      column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                      column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                    )
                                  )))
              )),
      tabItem(tabName = "NS",
              fluidPage(id = "dashboard_page",
                        fluidRow(div(h2(id = "header_title",i18n$t("Nova Scotia")),
                                     div(id = "lang_btn",
                                         bsButton("Btn_EN_ns","English",icon = icon("language"),style = "primary",size = "small",type = "action"),
                                         bsButton("Btn_FR_ns","French",icon = icon("language"),style = "primary",size = "small",type = "action")))),
                        
                        br(),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Load", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_ns", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "5 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_ns != 1",sliderInput("ns_dates_1",
                                                                                                                                               "Dates",
                                                                                                                                               min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                               max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                               value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                               timeFormat="%Y-%m-%d")))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_ns == 1",
                                                     fluidRow(withLoader(uiOutput("NS_LOAD_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns == 6",
                                                     fluidRow(withLoader(uiOutput("NS_LOAD_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns == 3",
                                                     fluidRow(withLoader(uiOutput("NS_LOAD_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns == 4",
                                                     fluidRow(withLoader(uiOutput("NS_LOAD_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns == 5",
                                                     fluidRow(withLoader(uiOutput("NS_LOAD_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Import", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_ns_1", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "5 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_ns_1 != 1",sliderInput("ns_dates_2",
                                                                                                                                                 "Dates",
                                                                                                                                                 min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                 max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                 value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                 timeFormat="%Y-%m-%d")))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_ns_1 == 1",
                                                     fluidRow(withLoader(uiOutput("NS_IMPORT_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns_1 == 6",
                                                     fluidRow(withLoader(uiOutput("NS_IMPORT_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns_1 == 3",
                                                     fluidRow(withLoader(uiOutput("NS_IMPORT_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns_1 == 4",
                                                     fluidRow(withLoader(uiOutput("NS_IMPORT_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns_1 == 5",
                                                     fluidRow(withLoader(uiOutput("NS_IMPORT_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Export", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_ns_2", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "5 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_ns_2 != 1",sliderInput("ns_dates_3",
                                                                                                                                                 "Dates",
                                                                                                                                                 min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                 max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                 value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                 timeFormat="%Y-%m-%d")))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_ns_2 == 1",
                                                     fluidRow(withLoader(uiOutput("NS_EXPORT_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns_2 == 6",
                                                     fluidRow(withLoader(uiOutput("NS_EXPORT_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns_2 == 3",
                                                     fluidRow(withLoader(uiOutput("NS_EXPORT_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns_2 == 4",
                                                     fluidRow(withLoader(uiOutput("NS_EXPORT_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns_2 == 5",
                                                     fluidRow(withLoader(uiOutput("NS_EXPORT_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Net Scheduled Interchange - NSI", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_ns_3", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "5 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_ns_3 != 1",sliderInput("ns_dates_4",
                                                                                                                                                 "Dates",
                                                                                                                                                 min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                 max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                 value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                 timeFormat="%Y-%m-%d")))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_ns_3 == 1",
                                                     fluidRow(withLoader(uiOutput("NS_NSI_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns_3 == 6",
                                                     fluidRow(withLoader(uiOutput("NS_NSI_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns_3 == 3",
                                                     fluidRow(withLoader(uiOutput("NS_NSI_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns_3 == 4",
                                                     fluidRow(withLoader(uiOutput("NS_NSI_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns_3 == 5",
                                                     fluidRow(withLoader(uiOutput("NS_NSI_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
              )),
      tabItem(tabName = "NB",
              fluidPage(id = "dashboard_page",
                        fluidRow(div(h2(id = "header_title",i18n$t("New Brunswick")),
                                     div(id = "lang_btn",
                                         bsButton("Btn_EN_nb","English",icon = icon("language"),style = "primary",size = "small",type = "action"),
                                         bsButton("Btn_FR_nb","French",icon = icon("language"),style = "primary",size = "small",type = "action")))),
                        
                        br(),
                        fluidRow(column(width =10, offset = 2,
                                                  box(
                                                    title = "Load", width = 10, status = "warning", solidHeader = TRUE,
                                                    collapsible = TRUE,
                                                    fluidRow(column(width = 2, selectInput("select_fr_nb", h4("Frequency"), 
                                                                                           choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "5 Min" = 6), selected = 6)),
                                                             column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_nb != 1",sliderInput("nb_dates_1",
                                                                                                                                                                "Dates",
                                                                                                                                                                min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                                max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                                value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                                timeFormat="%Y-%m-%d")))
                                                    ),
                                                    conditionalPanel( condition = "input.select_fr_nb == 1",
                                                                      fluidRow(withLoader(uiOutput("NB_LOAD_YEARLY"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb == 6",
                                                                      fluidRow(withLoader(uiOutput("NB_LOAD_ALL"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb == 3",
                                                                      fluidRow(withLoader(uiOutput("NB_LOAD_WEEKLY"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb == 4",
                                                                      fluidRow(withLoader(uiOutput("NB_LOAD_DAILY"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb == 5",
                                                                      fluidRow(withLoader(uiOutput("NB_LOAD_HOURLY"),type = "html", loader = "loader3"))),
                                                    fluidRow(
                                                      column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                                      column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                                    )
                                                  ))),
                                         fluidRow(
                                           column(width =10, offset = 2,
                                                  box(
                                                    title = "Demand", width = 10, status = "warning", solidHeader = TRUE,
                                                    collapsible = TRUE,
                                                    fluidRow(column(width = 2, selectInput("select_fr_nb_2", h4("Frequency"), 
                                                                                           choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "5 Min" = 6), selected = 6)),
                                                             column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_nb_2 != 1",sliderInput("nb_dates_2",
                                                                                                                                                                  "Dates",
                                                                                                                                                                  min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                                  max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                                  value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                                  timeFormat="%Y-%m-%d")))
                                                    ),
                                                    conditionalPanel( condition = "input.select_fr_nb_2 == 1",
                                                                      fluidRow(withLoader(uiOutput("NB_DEMAND_YEARLY"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb_2 == 6",
                                                                      fluidRow(withLoader(uiOutput("NB_DEMAND_ALL"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb_2 == 3",
                                                                      fluidRow(withLoader(uiOutput("NB_DEMAND_WEEKLY"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb_2 == 4",
                                                                      fluidRow(withLoader(uiOutput("NB_DEMAND_DAILY"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb_2 == 5",
                                                                      fluidRow(withLoader(uiOutput("NB_DEMAND_HOURLY"),type = "html", loader = "loader3"))),
                                                    fluidRow(
                                                      column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                                      column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                                    )
                                                  ))),
                                         fluidRow(
                                           column(width =10, offset = 2,
                                                  box(
                                                    title = "10 Min Reserve", width = 10, status = "warning", solidHeader = TRUE,
                                                    collapsible = TRUE,
                                                    fluidRow(column(width = 2, selectInput("select_fr_nb_3", h4("Frequency"), 
                                                                                           choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "5 Min" = 6), selected = 6)),
                                                             column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_nb_3 != 1",sliderInput("nb_dates_3",
                                                                                                                                                                  "Dates",
                                                                                                                                                                  min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                                  max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                                  value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                                  timeFormat="%Y-%m-%d")))
                                                    ),
                                                    conditionalPanel( condition = "input.select_fr_nb_3 == 1",
                                                                      fluidRow(withLoader(uiOutput("NB_RESERVE_YEARLY"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb_3 == 6",
                                                                      fluidRow(withLoader(uiOutput("NB_RESERVE_ALL"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb_3 == 3",
                                                                      fluidRow(withLoader(uiOutput("NB_RESERVE_WEEKLY"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb_3 == 4",
                                                                      fluidRow(withLoader(uiOutput("NB_RESERVE_DAILY"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb_3 == 5",
                                                                      fluidRow(withLoader(uiOutput("NB_RESERVE_HOURLY"),type = "html", loader = "loader3"))),
                                                    fluidRow(
                                                      column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                                      column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                                    )
                                                  ))),
                                         fluidRow(
                                           column(width =10, offset = 2,
                                                  box(
                                                    title = "30 Min Reserve", width = 10, status = "warning", solidHeader = TRUE,
                                                    collapsible = TRUE,
                                                    fluidRow(column(width = 2, selectInput("select_fr_nb_4", h4("Frequency"), 
                                                                                           choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "5 Min" = 6), selected = 6)),
                                                             column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_nb_4 != 1",sliderInput("nb_dates_4",
                                                                                                                                                                  "Dates",
                                                                                                                                                                  min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                                  max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                                  value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                                  timeFormat="%Y-%m-%d")))
                                                    ),
                                                    conditionalPanel( condition = "input.select_fr_nb_4 == 1",
                                                                      fluidRow(withLoader(uiOutput("NB_RESERVE2_YEARLY"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb_4 == 6",
                                                                      fluidRow(withLoader(uiOutput("NB_RESERVE2_ALL"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb_4 == 3",
                                                                      fluidRow(withLoader(uiOutput("NB_RESERVE2_WEEKLY"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb_4 == 4",
                                                                      fluidRow(withLoader(uiOutput("NB_RESERVE2_DAILY"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb_4 == 5",
                                                                      fluidRow(withLoader(uiOutput("NB_RESERVE2_HOURLY"),type = "html", loader = "loader3"))),
                                                    fluidRow(
                                                      column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                                      column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                                    )
                                                  )))
                        
                        
              )),
      tabItem(tabName = "ON",
              fluidPage(id = "dashboard_page",
                        fluidRow(div(h2(id = "header_title",i18n$t("Ontario")),
                                     div(id = "lang_btn",
                                         bsButton("Btn_EN_on","English",icon = icon("language"),style = "primary",size = "small",type = "action"),
                                         bsButton("Btn_FR_on","French",icon = icon("language"),style = "primary",size = "small",type = "action")))),
                        
                        br(),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Total Energy", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_on1", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_on1 != 1",sliderInput("on_dates_1",
                                                                                                                                                "Dates",
                                                                                                                                                min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                timeFormat="%Y-%m-%d")))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_on1 == 1",
                                                     fluidRow(withLoader(uiOutput("ON_LOAD_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_on1 == 6",
                                                     fluidRow(withLoader(uiOutput("ON_LOAD_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_on1 == 3",
                                                     fluidRow(withLoader(uiOutput("ON_LOAD_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_on1 == 4",
                                                     fluidRow(withLoader(uiOutput("ON_LOAD_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_on1 == 5",
                                                     fluidRow(withLoader(uiOutput("ON_LOAD_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Total Loss", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_on2", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_on2 != 1",sliderInput("on_dates_2",
                                                                                                                                                "Dates",
                                                                                                                                                min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                timeFormat="%Y-%m-%d")))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_on2 == 1",
                                                     fluidRow(withLoader(uiOutput("ON_MRKTDMND_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_on2 == 6",
                                                     fluidRow(withLoader(uiOutput("ON_MRKTDMND_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_on2 == 3",
                                                     fluidRow(withLoader(uiOutput("ON_MRKTDMND_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_on2 == 4",
                                                     fluidRow(withLoader(uiOutput("ON_MRKTDMND_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_on2 == 5",
                                                     fluidRow(withLoader(uiOutput("ON_MRKTDMND_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                       
              )),
      tabItem(tabName = "NFL",
              fluidPage(id = "dashboard_page",
                        fluidRow(div(h2(id = "header_title",i18n$t("Newfoundland & Labrador")),
                                     div(id = "lang_btn",
                                         bsButton("Btn_EN_nfl","English",icon = icon("language"),style = "primary",size = "small",type = "action"),
                                         bsButton("Btn_FR_nfl","French",icon = icon("language"),style = "primary",size = "small",type = "action")))),
                        
                        br(),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Load", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_nfl", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_nfl != 1",sliderInput("nfl_dates_1",
                                                                                                                                                "Dates",
                                                                                                                                                min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                timeFormat="%Y-%m-%d")))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_nfl == 1",
                                                     fluidRow(withLoader(uiOutput("NFL_LOAD_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_nfl == 6",
                                                     fluidRow(withLoader(uiOutput("NFL_LOAD_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_nfl == 3",
                                                     fluidRow(withLoader(uiOutput("NFL_LOAD_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_nfl == 4",
                                                     fluidRow(withLoader(uiOutput("NFL_LOAD_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_nfl == 5",
                                                     fluidRow(withLoader(uiOutput("NFL_LOAD_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 )))
              )),
      tabItem(tabName = "QB",
              fluidPage(id = "dashboard_page",
                        fluidRow(div(h2(id = "header_title",i18n$t("Quebec")),
                                     div(id = "lang_btn",
                                         bsButton("Btn_EN_qb","English",icon = icon("language"),style = "primary",size = "small",type = "action"),
                                         bsButton("Btn_FR_qb","French",icon = icon("language"),style = "primary",size = "small",type = "action")))),
                        
                        br(),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Total Production", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_qb_1", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_qb_1 != 1",sliderInput("qb_dates_1",
                                                                                                                                                 "Dates",
                                                                                                                                                 min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                 max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                 value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                 timeFormat="%Y-%m-%d")))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_qb_1 == 1",
                                                     fluidRow(withLoader(highchartOutput("QB_TOT_PRODUCTION_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_1 == 6",
                                                     fluidRow(withLoader(highchartOutput("QB_TOT_PRODUCTION_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_1 == 3",
                                                     fluidRow(withLoader(highchartOutput("QB_TOT_PRODUCTION_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_1 == 4",
                                                     fluidRow(withLoader(highchartOutput("QB_TOT_PRODUCTION_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_1 == 5",
                                                     fluidRow(withLoader(highchartOutput("QB_TOT_PRODUCTION_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Fuel Type - I", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_qb_2", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_qb_2 != 1",sliderInput("qb_dates_2",
                                                                                                                                                 "Dates",
                                                                                                                                                 min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                 max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                 value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                 timeFormat="%Y-%m-%d")))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_qb_2 == 1",
                                                     fluidRow(withLoader(highchartOutput("QB_FUEL_1_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_2 == 6",
                                                     fluidRow(withLoader(highchartOutput("QB_FUEL_1_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_2 == 3",
                                                     fluidRow(withLoader(highchartOutput("QB_FUEL_1_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_2 == 4",
                                                     fluidRow(withLoader(highchartOutput("QB_FUEL_1_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_2 == 5",
                                                     fluidRow(withLoader(highchartOutput("QB_FUEL_1_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Fuel Type - II", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_qb_3", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_qb_3 != 1",sliderInput("qb_dates_3",
                                                                                                                                                 "Dates",
                                                                                                                                                 min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                 max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                 value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                 timeFormat="%Y-%m-%d")))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_qb_3 == 1",
                                                     fluidRow(withLoader(highchartOutput("QB_FUEL_2_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_3 == 6",
                                                     fluidRow(withLoader(highchartOutput("QB_FUEL_2_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_3 == 3",
                                                     fluidRow(withLoader(highchartOutput("QB_FUEL_2_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_3 == 4",
                                                     fluidRow(withLoader(highchartOutput("QB_FUEL_2_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_3 == 5",
                                                     fluidRow(withLoader(highchartOutput("QB_FUEL_2_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Fuel Type - III", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_qb_4", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_qb_4 != 1",sliderInput("qb_dates_4",
                                                                                                                                                 "Dates",
                                                                                                                                                 min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                 max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                 value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                 timeFormat="%Y-%m-%d")))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_qb_4 == 1",
                                                     fluidRow(withLoader(highchartOutput("QB_FUEL_3_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_4 == 6",
                                                     fluidRow(withLoader(highchartOutput("QB_FUEL_3_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_4 == 3",
                                                     fluidRow(withLoader(highchartOutput("QB_FUEL_3_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_4 == 4",
                                                     fluidRow(withLoader(highchartOutput("QB_FUEL_3_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_4 == 5",
                                                     fluidRow(withLoader(highchartOutput("QB_FUEL_3_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Electricity Statistics - I", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_qb_5", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_qb_5 != 1",sliderInput("qb_dates_5",
                                                                                                                                                 "Dates",
                                                                                                                                                 min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                 max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                 value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                 timeFormat="%Y-%m-%d")))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_qb_5 == 1",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_1_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_5 == 6",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_1_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_5 == 3",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_1_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_5 == 4",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_1_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_5 == 5",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_1_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Electricity Statistics - II", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_qb_6", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_qb_6 != 1",sliderInput("qb_dates_6",
                                                                                                                                                 "Dates",
                                                                                                                                                 min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                 max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                 value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                 timeFormat="%Y-%m-%d")))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_qb_6 == 1",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_2_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_6 == 6",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_2_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_6 == 3",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_2_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_6 == 4",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_2_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_6 == 5",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_2_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Electricity Statistics - III", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_qb_7", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_qb_7 != 1",sliderInput("qb_dates_7",
                                                                                                                                                 "Dates",
                                                                                                                                                 min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                 max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                 value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                 timeFormat="%Y-%m-%d")))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_qb_7 == 1",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_3_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_7 == 6",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_3_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_7 == 3",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_3_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_7 == 4",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_3_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_7 == 5",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_3_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Electricity Statistics - IV", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_qb_8", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_qb_8 != 1",sliderInput("qb_dates_8",
                                                                                                                                                 "Dates",
                                                                                                                                                 min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                 max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                 value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                 timeFormat="%Y-%m-%d")))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_qb_8 == 1",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_4_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_8 == 6",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_4_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_8 == 3",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_4_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_8 == 4",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_4_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_8 == 5",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_4_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Electricity Statistics - V", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_qb_9", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_qb_9 != 1",sliderInput("qb_dates_9",
                                                                                                                                                 "Dates",
                                                                                                                                                 min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                 max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                 value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                 timeFormat="%Y-%m-%d")))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_qb_9 == 1",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_5_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_9 == 6",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_5_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_9 == 3",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_5_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_9 == 4",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_5_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_9 == 5",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_5_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Electricity Statistics - VI", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_qb_10", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_qb_10 != 1",sliderInput("qb_dates_10",
                                                                                                                                                  "Dates",
                                                                                                                                                  min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                  max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                  value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                  timeFormat="%Y-%m-%d")))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_qb_10 == 1",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_6_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_10 == 6",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_6_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_10 == 3",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_6_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_10 == 4",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_6_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_10 == 5",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_6_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Electricity Statistics - VII", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_qb_11", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_qb_11 != 1",sliderInput("qb_dates_11",
                                                                                                                                                  "Dates",
                                                                                                                                                  min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                  max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                  value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                  timeFormat="%Y-%m-%d")))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_qb_11 == 1",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_7_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_11 == 6",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_7_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_11 == 3",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_7_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_11 == 4",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_7_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_11 == 5",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_7_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Electricity Statistics - VIII", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_qb_12", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_qb_12 != 1",sliderInput("qb_dates_12",
                                                                                                                                                  "Dates",
                                                                                                                                                  min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                  max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                  value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                  timeFormat="%Y-%m-%d")))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_qb_12 == 1",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_8_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_12 == 6",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_8_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_12 == 3",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_8_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_12 == 4",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_8_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_qb_12 == 5",
                                                     fluidRow(withLoader(highchartOutput("QB_ENE_8_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        
              )),
      tabItem(tabName = "AB",
              fluidPage(id = "dashboard_page",
                        fluidRow(div(h2(id = "header_title",i18n$t("Alberta")),
                                     div(id = "lang_btn",
                                         bsButton("Btn_EN_ab","English",icon = icon("language"),style = "primary",size = "small",type = "action"),
                                         bsButton("Btn_FR_ab","French",icon = icon("language"),style = "primary",size = "small",type = "action")))),
                        
                        br(),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Hydro", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_ab_8", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_ab_8 != 1",sliderInput("ab_dates_8",
                                                                                                                                                 "Dates",
                                                                                                                                                 min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                 max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                 value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                 timeFormat="%Y-%m-%d"))),
                                            column(width = 2, selectInput("ab_ind_8_ff", h4("Asset"), 
                                                                          choices = list(
                                                                            "Taylor Hydro (TAY1)" = "Taylor Hydro (TAY1)",
                                                                            "Bow River Hydro (BOW1)" = "Bow River Hydro (BOW1)",
                                                                            "Bighorn Hydro (BIG)" = "Bighorn Hydro (BIG)",
                                                                            "Raymond Reservoir (RYMD)" = "Raymond Reservoir (RYMD)",
                                                                            "Oldman River (OMRH)" = "Oldman River (OMRH)",
                                                                            "Dickson Dam (DKSN)" = "Dickson Dam (DKSN)",
                                                                            "Irrican Hydro (ICP1)" = "Irrican Hydro (ICP1)",
                                                                            "Brazeau Hydro (BRA)" = "Brazeau Hydro (BRA)",
                                                                            "Chin Chute (CHIN)" = "Chin Chute (CHIN)"
                                                                          ), selected = "Chin Chute (CHIN)"))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_ab_8 == 1",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_8_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ab_8 == 6",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_8_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ab_8 == 3",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_8_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ab_8 == 4",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_8_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ab_8 == 5",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_8_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Storage", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_ab_9", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_ab_9 != 1",sliderInput("ab_dates_9",
                                                                                                                                                 "Dates",
                                                                                                                                                 min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                 max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                 value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                 timeFormat="%Y-%m-%d"))),
                                            column(width = 2, selectInput("ab_ind_9_ff", h4("Asset"), 
                                                                          choices = list(
                                                                            "eReserve1 Rycroft (ERV1)" = "eReserve1 Rycroft (ERV1)",
                                                                            "eReserve2 Buffalo Creek (ERV2)" = "eReserve2 Buffalo Creek (ERV2)",
                                                                            "Summerview (SUM1)" = "Summerview (SUM1)"
                                                                          ), selected = "Summerview (SUM1)"))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_ab_9 == 1",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_9_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ab_9 == 6",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_9_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ab_9 == 3",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_9_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ab_9 == 4",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_9_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ab_9 == 5",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_9_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Solar", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_ab_10", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_ab_10 != 1",sliderInput("ab_dates_10",
                                                                                                                                                  "Dates",
                                                                                                                                                  min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                  max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                  value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                  timeFormat="%Y-%m-%d"))),
                                            column(width = 2, selectInput("ab_ind_10_ff", h4("Asset"), 
                                                                          choices = list(
                                                                            "Brooks Solar (BSC1)" = "Brooks Solar (BSC1)",
                                                                            "Suffield (SUF1)" = "Suffield (SUF1)",
                                                                            "Travers (TVS1)" = "Travers (TVS1)",
                                                                            "Hull (HUL1)" = "Hull (HUL1)",
                                                                            "Westfield Yellow Lake (WEF1)" = "Westfield Yellow Lake (WEF1)",
                                                                            "Claresholm 1 (CLR1)" = "Claresholm 1 (CLR1)",
                                                                            "BRD1 Burdett (BRD1)" = "BRD1 Burdett (BRD1)",
                                                                            "Jenner (JER1)" = "Jenner (JER1)",
                                                                            "Vauxhall (VXH1)" = "Vauxhall (VXH1)",
                                                                            "BUR1 Burdett (BUR1)" ="BUR1 Burdett (BUR1)",
                                                                            "Hays (HYS1)" = "Hays (HYS1)",
                                                                            "Innisfail (INF1)" = "Innisfail (INF1)",
                                                                            "Claresholm 2 (CLR2)" = "Claresholm 2 (CLR2)"
                                                                          ), selected = "Claresholm 2 (CLR2)"))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_ab_10 == 1",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_10_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ab_10 == 6",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_10_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ab_10 == 3",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_10_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ab_10 == 4",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_10_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ab_10 == 5",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_10_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Wind", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_ab_11", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_ab_11 != 1",sliderInput("ab_dates_11",
                                                                                                                                                  "Dates",
                                                                                                                                                  min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                  max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                  value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                  timeFormat="%Y-%m-%d"))),
                                            column(width = 2, selectInput("ab_ind_11_ff", h4("Asset"), 
                                                                          choices = list(
                                                                            "Blue Trail Wind (BTR1)*"="Blue Trail Wind (BTR1)*",
                                                                            "Ghost Pine (NEP1)*"="Ghost Pine (NEP1)*",
                                                                            "BUL1 Bull Creek (BUL1)*"="BUL1 Bull Creek (BUL1)*",
                                                                            "Enmax Taber (TAB1)*"="Enmax Taber (TAB1)*",
                                                                            "Castle Rock Ridge 2 (CRR2)*"="Castle Rock Ridge 2 (CRR2)*",
                                                                            "Cowley Ridge (CRE3)*"="Cowley Ridge (CRE3)*",
                                                                            "Oldman 2 Wind Farm 1 (OWF1)*"="Oldman 2 Wind Farm 1 (OWF1)*",
                                                                            "Suncor Chin Chute (SCR3)*"="Suncor Chin Chute (SCR3)*",
                                                                            "Castle River #1 (CR1)*"="Castle River #1 (CR1)*",
                                                                            "Suncor Magrath (SCR2)*"="Suncor Magrath (SCR2)*",
                                                                            "Kettles Hill (KHW1)*"="Kettles Hill (KHW1)*",
                                                                            "McBride Lake Windfarm (AKE1)*"="McBride Lake Windfarm (AKE1)*",
                                                                            "Windrise (WRW1)*"="Windrise (WRW1)*",
                                                                            "BUL2 Bull Creek (BUL2)*"="BUL2 Bull Creek (BUL2)*",
                                                                            "Halkirk Wind Power Facility (HAL1)*"="Halkirk Wind Power Facility (HAL1)*",
                                                                            "Riverview (RIV1)*"="Riverview (RIV1)*",
                                                                            "Summerview 1 (IEW1)*"="Summerview 1 (IEW1)*",
                                                                            "Whitla 2 (WHT2)*"="Whitla 2 (WHT2)*",
                                                                            "Blackspring Ridge (BSR1)*"="Blackspring Ridge (BSR1)*",
                                                                            "Summerview 2 (IEW2)*"="Summerview 2 (IEW2)*",
                                                                            "Soderglen Wind  (GWW1)*"="Soderglen Wind  (GWW1)*",
                                                                            "Ardenville Wind (ARD1)*"="Ardenville Wind (ARD1)*",
                                                                            "Whitla 1 (WHT1)*"="Whitla 1 (WHT1)*",
                                                                            "Wintering Hills (SCR4)*"="Wintering Hills (SCR4)*",
                                                                            "Rattlesnake Ridge Wind (RTL1)*"="Rattlesnake Ridge Wind (RTL1)*",
                                                                            "Castle Rock Wind Farm (CRR1)*"="Castle Rock Wind Farm (CRR1)*"
                                                                          ), selected = "Castle Rock Wind Farm (CRR1)*"))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_ab_11 == 1",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_11_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ab_11 == 6",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_11_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ab_11 == 3",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_11_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ab_11 == 4",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_11_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ab_11 == 5",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_11_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Biomass", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_ab_12", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_ab_12 != 1",sliderInput("ab_dates_12",
                                                                                                                                                  "Dates",
                                                                                                                                                  min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                  max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                  value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                  timeFormat="%Y-%m-%d"))),
                                            column(width = 2, selectInput("ab_ind_12_ff", h4("Asset"), 
                                                                          choices = list(
                                                                            "Taylor Hydro (TAY1)" = "Taylor Hydro (TAY1)",
                                                                            "Bow River Hydro (BOW1)" = "Bow River Hydro (BOW1)",
                                                                            "Bighorn Hydro (BIG)" = "Bighorn Hydro (BIG)",
                                                                            "Raymond Reservoir (RYMD)" = "Raymond Reservoir (RYMD)",
                                                                            "Oldman River (OMRH)" = "Oldman River (OMRH)",
                                                                            "Dickson Dam (DKSN)" = "Dickson Dam (DKSN)",
                                                                            "Irrican Hydro (ICP1)" = "Irrican Hydro (ICP1)",
                                                                            "Brazeau Hydro (BRA)" = "Brazeau Hydro (BRA)",
                                                                            "Chin Chute (CHIN)" = "Chin Chute (CHIN)"
                                                                          ), selected = "Chin Chute (CHIN)"))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_ab_12 == 1",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_12_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ab_12 == 6",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_12_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ab_12 == 3",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_12_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ab_12 == 4",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_12_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ab_12 == 5",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_12_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Dual", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_ab_13", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_ab_13 != 1",sliderInput("ab_dates_13",
                                                                                                                                                  "Dates",
                                                                                                                                                  min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                  max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                  value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                  timeFormat="%Y-%m-%d"))),
                                            column(width = 2, selectInput("ab_ind_13_ff", h4("Asset"), 
                                                                          choices = list(
                                                                            "Taylor Hydro (TAY1)" = "Taylor Hydro (TAY1)",
                                                                            "Bow River Hydro (BOW1)" = "Bow River Hydro (BOW1)",
                                                                            "Bighorn Hydro (BIG)" = "Bighorn Hydro (BIG)",
                                                                            "Raymond Reservoir (RYMD)" = "Raymond Reservoir (RYMD)",
                                                                            "Oldman River (OMRH)" = "Oldman River (OMRH)",
                                                                            "Dickson Dam (DKSN)" = "Dickson Dam (DKSN)",
                                                                            "Irrican Hydro (ICP1)" = "Irrican Hydro (ICP1)",
                                                                            "Brazeau Hydro (BRA)" = "Brazeau Hydro (BRA)",
                                                                            "Chin Chute (CHIN)" = "Chin Chute (CHIN)"
                                                                          ), selected = "Chin Chute (CHIN)"))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_ab_13 == 1",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_13_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ab_13 == 6",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_13_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ab_13 == 3",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_13_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ab_13 == 4",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_13_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ab_13 == 5",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_13_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Coal", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_ab_14", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_ab_14 != 1",sliderInput("ab_dates_14",
                                                                                                                                                  "Dates",
                                                                                                                                                  min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                  max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                  value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                  timeFormat="%Y-%m-%d"))),
                                            column(width = 2, selectInput("ab_ind_14_ff", h4("Asset"), 
                                                                          choices = list(
                                                                            "Taylor Hydro (TAY1)" = "Taylor Hydro (TAY1)",
                                                                            "Bow River Hydro (BOW1)" = "Bow River Hydro (BOW1)",
                                                                            "Bighorn Hydro (BIG)" = "Bighorn Hydro (BIG)",
                                                                            "Raymond Reservoir (RYMD)" = "Raymond Reservoir (RYMD)",
                                                                            "Oldman River (OMRH)" = "Oldman River (OMRH)",
                                                                            "Dickson Dam (DKSN)" = "Dickson Dam (DKSN)",
                                                                            "Irrican Hydro (ICP1)" = "Irrican Hydro (ICP1)",
                                                                            "Brazeau Hydro (BRA)" = "Brazeau Hydro (BRA)",
                                                                            "Chin Chute (CHIN)" = "Chin Chute (CHIN)"
                                                                          ), selected = "Chin Chute (CHIN)"))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_ab_14 == 1",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_14_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ab_14 == 6",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_14_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ab_14 == 3",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_14_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ab_14 == 4",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_14_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ab_14 == 5",
                                                     fluidRow(withLoader(highchartOutput("AB_ENE_14_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        
                        
                        
                        
                        
              )),
      tabItem(tabName = "BC",
              fluidPage(id = "dashboard_page",
                        fluidRow(div(h2(id = "header_title",i18n$t("British Columbia")),
                                     div(id = "lang_btn",
                                         bsButton("Btn_EN_bc","English",icon = icon("language"),style = "primary",size = "small",type = "action"),
                                         bsButton("Btn_FR_bc","French",icon = icon("language"),style = "primary",size = "small",type = "action")))),
                        fluidRow(h4(textOutput("timer_bc"))),
                        br(),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Balancing Authority Load - BAL", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_bc_1", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_bc_1 != 1",sliderInput("bc_dates_1",
                                                                                                                                                 "Dates",
                                                                                                                                                 min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                 max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                 value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                 timeFormat="%Y-%m-%d")))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_bc_1 == 1",
                                                     fluidRow(withLoader(uiOutput("BC_LOAD_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_bc_1 == 6",
                                                     fluidRow(withLoader(uiOutput("BC_LOAD_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_bc_1 == 3",
                                                     fluidRow(withLoader(uiOutput("BC_LOAD_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_bc_1 == 4",
                                                     fluidRow(withLoader(uiOutput("BC_LOAD_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_bc_1 == 5",
                                                     fluidRow(withLoader(uiOutput("BC_LOAD_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 )))
                        
                        
              )),
      tabItem(tabName = "DD",
              fluidRow(
                column(width =10, offset = 2,
                       box(
                         title = "Data Dictionary", width = 10, status = "warning", solidHeader = TRUE,
                         collapsible = TRUE, dataTableOutput("DATA_DICTIONARY_TABLE"))))
      ),
      tabItem(tabName = "ST",
              fluidPage(
                box(title = "Server Status", status = "primary", solidHeader = TRUE, width = "100%", collapsible = FALSE,uiOutput("SERVER_STATUS"))
              )
      ),
      tabItem(tabName = "Dwn",
              fluidPage(
                box(title = "Downloads", status = "success", solidHeader = TRUE, width = "100%", collapsible = FALSE,
                    fluidRow(
                      column(width = 5,
                      selectInput("prvnc_list",h3("Province"),choices = list(
                        "Newfoundland & Labrador",
                        "Prince Edward Island",
                        "Nova Scotia",
                        "New Brunswick",
                        "Quebec",
                        "Ontario",
                        "Alberta",
                        "British Columbia"
                        ))),
                      column(width = 2, selectInput("enf_flow",h3("Energy Flow"),choices = list("DEMAND"))),
                      column(width = 5, dateRangeInput("download_dates",h3("Date range")))),
                      hr(),
                      fluidRow(
                      column(width = 12,withLoader(DT::dataTableOutput("DOWNLOAD_TABLE"),type = "html", loader = "loader3"))),
                      hr(),
                      fluidRow(
                      column(width = 12,
                      downloadBttn("button_dwnld","Download data","unite","primary","md",TRUE,TRUE,icon("download"))
                      ))
                    ) 
                    
                ))
      )
      
    ))  



#bussines logic

base_url_prefix <- "https://fdi-design-sdmx.aaw-dev.cloud.statcan.ca/rest/data/CCEI,"
base_url_suffix <- "&dimensionAtObservation=AllDimensions"
base_url_suffix_lastnobs <- "&lastNObservations="
dataset <- function(dataflow,ref_area,freq,energy_flow,startdate,enddate,nlastobs){
  if(is.null(startdate)&&is.null(enddate)&&!is.null(nlastobs)){
    dataset_url <- paste(base_url_prefix,dataflow,",1.0/",freq,".","..",energy_flow,"?",base_url_suffix,base_url_suffix_lastnobs,nlastobs,sep = "")
  }
  else if(is.null(nlastobs)&&is.null(startdate)&&is.null(enddate)){
    dataset_url <- paste(base_url_prefix,dataflow,",1.0/",freq,".","..",energy_flow,"?",base_url_suffix,sep = "")
  }
  else{
    dataset_url <- paste(base_url_prefix,dataflow,",1.0/",freq,".","..",energy_flow,"?","startPeriod=",startdate,"&endPeriod=",enddate,"&",base_url_suffix,sep = "")
  }
  api_cl_st_t <- Sys.time()
  raw_data <- future({GET(dataset_url, accept("application/vnd.sdmx.data+csv; charset=utf-8"))})
  bin <- content(value(raw_data), "text", encoding = "ISO-8859-1")
  clean_data <- read.table(text = bin, sep =",", header = TRUE, stringsAsFactors = FALSE)
  api_cl_en_t <- Sys.time()
  sortdata <- clean_data
  api_pr_st_t <- Sys.time()
  sortdata$DATETIME_LOCAL <- str_replace(sortdata$DATETIME_LOCAL,"T"," ")
  sortdata$DATETIME_LOCAL <- as.POSIXlt(sortdata$DATETIME_LOCAL,format="%Y-%m-%d %H:%M:%S")
  api_pr_en_t <- Sys.time()
  log_info('DataFlow:{dataflow} -- Variable:{energy_flow} -- {difftime(api_cl_en_t,api_cl_st_t)} sec for response time -- {difftime(api_pr_en_t,api_pr_st_t)} sec for process time -- rows: {nrow(sortdata)}')
  if(status_code(value(raw_data)) == 200){
    log_success('Status:{status_code(value(raw_data))}')
  }
  else{
    log_error('Status:{status_code(value(raw_data))}')
  }
  
  if(!is.null(energy_flow))
  {
    dataset_energyflow <- subset(sortdata,subset = (sortdata$ENERGY_FLOWS == energy_flow))
  }
  else
  {
    dataset_energyflow <- sortdata
  }
  return(dataset_energyflow)
}

status_api <- function(dataflow,ref_area,freq,energy_flow,startdate,enddate,nlastobs){
  if(is.null(startdate)&&is.null(enddate)&&!is.null(nlastobs)){
    dataset_url <- paste(base_url_prefix,dataflow,",1.0/",freq,".","..",energy_flow,"?",base_url_suffix,base_url_suffix_lastnobs,nlastobs,sep = "")
  }
  else if(is.null(nlastobs)&&is.null(startdate)&&is.null(enddate)){
    dataset_url <- paste(base_url_prefix,dataflow,",1.0/",freq,".","..",energy_flow,"?",base_url_suffix,sep = "")
  }
  else{
    dataset_url <- paste(base_url_prefix,dataflow,",1.0/",freq,".","..",energy_flow,"?","startPeriod=",startdate,"&endPeriod=",enddate,"&",base_url_suffix,sep = "")
  }
  api_cl_st_t <- Sys.time()
  raw_data <- future({GET(dataset_url, accept("application/vnd.sdmx.data+csv; charset=utf-8"))})
  bin <- content(value(raw_data), "text", encoding = "ISO-8859-1")
  return(status_code(value(raw_data)))
}

download_api <- function(dataflow,ref_area,freq,energy_flow,startdate,enddate,nlastobs,firstnobs)
  {
  if(is.null(startdate)&&is.null(enddate)&&!is.null(nlastobs))
    {
   dataset_url <- paste(base_url_prefix,dataflow,",1.0/",freq,".","..",energy_flow,"?",base_url_suffix,base_url_suffix_lastnobs,nlastobs,"&","firstNObservations=",firstnobs,sep = "")
   raw_data <- future({GET(dataset_url, accept("application/vnd.sdmx.data+csv; charset=utf-8"))})
   bin <- content(value(raw_data), "text", encoding = "ISO-8859-1")
   clean_data <- read.table(text = bin, sep =",", header = TRUE, stringsAsFactors = FALSE)
   api_cl_en_t <- Sys.time()
   sortdata <- clean_data
   api_pr_st_t <- Sys.time()
   sortdata$DATETIME_LOCAL <- str_replace(sortdata$DATETIME_LOCAL,"T"," ")
   sortdata$DATETIME_LOCAL <- as.POSIXlt(sortdata$DATETIME_LOCAL,format="%Y-%m-%d %H:%M:%S")
  }
  log_info("download_api->{dataset_url} ->{nrow(sortdata)}")
  return(sortdata)
  }





# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  if_local <- Sys.getenv('SHINY_PORT')==""
  
  if(if_local){
    Sys.setenv(R_CONFIG_ACTIVE = "local")
  }
  if(!if_local){
    Sys.setenv(R_CONFIG_ACTIVE = "shinyapps")
  }
  
  config <- config::get()
  
  # Dates for visualizations 
  Previous_date <- as.Date(Sys.Date())-(5*365)
  previous_time <- paste(Previous_date,"00:00:00",sep=" ")
  
  Previous_date_1 <- as.Date(Sys.Date())-(1)
  previous_time_1 <- paste(Previous_date_1,"00:00:00",sep=" ")
  
  sdmx_date_start <- as.character(as.Date(Sys.Date())-(1))
  sdmx_date_end <- as.character(as.Date(Sys.Date()))
  
  sdmx_date_start_ind_p <<- as.character(as.Date(Sys.Date())-(90))
  sdmx_date_end_ind_p <<- as.character(as.Date(Sys.Date()))
  
  sdmx_date_start_ind_nb <<- as.character(as.Date(Sys.Date())-(90))
  sdmx_date_end_ind_nb <<- as.character(as.Date(Sys.Date()))
  
  sdmx_date_start_ind_nfl <<- as.character(as.Date(Sys.Date())-(90))
  sdmx_date_end_ind_nfl <<- as.character(as.Date(Sys.Date()))
  
  sdmx_date_start_ind_ns <<- as.character(as.Date(Sys.Date())-(90))
  sdmx_date_end_ind_ns <<- as.character(as.Date(Sys.Date()))
  
  
  
  
  
  #abload_data <- tbl(con, config$provinces$AB$table15) %>% arrange(Date_time_local) %>% collect()
  #abload_date <- as.Date(tail(abload_data$Date_time_local,1))
  #abload_subset <- subset(abload_data,subset = Date_time_local >= (abload_date - 1) & Date_time_local <= abload_date)
  #ab_load_ts_1 <-  xts(abload_subset$Calgary,abload_subset$Date_time_local)
  #ab_load_ts_2 <-  xts(abload_subset$Central,abload_subset$Date_time_local)
  #ab_load_ts_3 <-  xts(abload_subset$Edmonton,abload_subset$Date_time_local)
  #ab_load_ts_4 <-  xts(abload_subset$Northeast,abload_subset$Date_time_local)
  #ab_load_ts_5 <-  xts(abload_subset$Northwest,abload_subset$Date_time_local)
  #ab_load_ts_6 <-  xts(abload_subset$South,abload_subset$Date_time_local)
  
  #ab_load_chart <- highchart() %>% 
    #hc_xAxis(type = "datetime") %>%
    #hc_add_series(ab_load_ts_1, name="Calgary Load", type = "line")%>%
    #hc_add_series(ab_load_ts_2, name="Central Load", type = "line",color = "red")%>%
    #hc_add_series(ab_load_ts_3, name="Edmonton Load", type = "line",color = "lightgreen")%>%
    #hc_add_series(ab_load_ts_4, name="Northeast Load", type = "line",color = "purple")%>%
    #hc_add_series(ab_load_ts_5, name="Northwest Load", type = "line",color = "orange")%>%
    #hc_add_series(ab_load_ts_6, name="South Load", type = "line",color = "brown")%>%
    #hc_navigator(enabled = TRUE)
  
  #pei_ind_dat <- tbl(con, config$provinces$PEI$table1) %>% arrange(Date_time_local) %>% collect()
  #nb_ind_dat <- tbl(con, config$provinces$NB$table1) %>% arrange(Date_time_local) %>% collect()
  #ns_ind_dat <- tbl(con, config$provinces$NS$table1) %>% arrange(Date_time_local) %>% collect()
  #bc_ind_dat <- tbl(con, config$provinces$BC$table1) %>% arrange(Date_time_local) %>% collect()
  #ab_ind_dat <- tbl(con, config$provinces$AB$table15) %>% arrange(Date_time_local) %>% collect()
  #on_ind_dat <- tbl(con, config$provinces$ONT$table1) %>% arrange(date_time_local) %>% collect()
  #bc_ind_dat_1 <- tbl(con, config$provinces$BC$table3) %>% arrange(date_time_local) %>% collect()
  #ns_ind_dat_1 <- tbl(con, config$provinces$NS$table2) %>% arrange(Date_time_local) %>% collect()
  #nfl_ind_dat <- tbl(con, config$provinces$NFL$table1) %>% arrange(Date_time_local) %>% collect()
  #qb_ind_dat_1 <- tbl(con, config$provinces$QUEBEC$table2) %>% arrange(Date_time_local) %>% collect()
  #qb_ind_dat_2 <- tbl(con, config$provinces$QUEBEC$table1) %>% arrange(Date_time_local) %>% collect()
  #qb_ind_dat_3 <- tbl(con, config$provinces$QUEBEC$table3) %>% arrange(Date_time_local) %>% collect()
  #ab_ind_dat_8 <- tbl(con, config$provinces$AB$table8) %>% arrange(Update_Time)  %>% collect()
  #ab_ind_dat_9 <- tbl(con, config$provinces$AB$table9) %>% arrange(Update_Time) %>% collect()
  #ab_ind_dat_10 <- tbl(con, config$provinces$AB$table10) %>% arrange(Update_Time) %>% collect()
  #ab_ind_dat_11 <- tbl(con, config$provinces$AB$table11) %>% arrange(Update_Time) %>% collect()
  #ab_ind_dat_12 <- tbl(con, config$provinces$AB$table12) %>% arrange(Update_Time) %>% collect()
  #ab_ind_dat_13 <- tbl(con, config$provinces$AB$table13) %>% arrange(Update_Time) %>% collect()
  #ab_ind_dat_14 <- tbl(con, config$provinces$AB$table14) %>% arrange(Update_Time) %>% collect()
  
  #ab_ind_dat_1 <- tbl(con, config$provinces$AB$table1) %>% arrange(Date_time_local) %>% collect()
  #ab_ind_dat_2 <- tbl(con, config$provinces$AB$table2) %>% arrange(Date) %>% collect()
  #ab_ind_dat_3 <- tbl(con, config$provinces$AB$table3) %>% arrange(Date_time_local) %>% collect()
  #ab_ind_dat_4 <- tbl(con, config$provinces$AB$table4) %>% arrange(Date_time_local) %>% collect()
  #ab_ind_dat_5 <- tbl(con, config$provinces$AB$table5) %>% arrange(Date_time_local) %>% collect()
  #ab_ind_dat_6 <- tbl(con, config$provinces$AB$table6) %>% arrange(Date_time_local) %>% collect()
  #ab_ind_dat_7 <- tbl(con, config$provinces$AB$table7) %>% arrange(Date_time_local) %>% collect()
  
  
  output$timer <- renderText({invalidateLater(1000, session)
    paste("",as.POSIXlt(Sys.time(), tz = "UTC"),"(UTC)")})
  
  output$timer_pei <- renderText({invalidateLater(1000, session)
    paste("",as.POSIXlt(Sys.time(), tz = "UTC"),"(UTC)")})
  
  output$timer_bc <- renderText({invalidateLater(1000, session)
    paste("",as.POSIXlt(Sys.time(), tz = "UTC"),"(UTC)")})
  
  output$timer_ab <- renderText({invalidateLater(1000, session)
    paste("",as.POSIXlt(Sys.time(), tz = "UTC"),"(UTC)")})
  
  output$timer_on <- renderText({invalidateLater(1000, session)
    paste("",as.POSIXlt(Sys.time(), tz = "UTC"),"(UTC)")})
  
  output$timer_nb <- renderText({invalidateLater(1000, session)
    paste("",as.POSIXlt(Sys.time(), tz = "UTC"),"(UTC)")})
  
  output$timer_ns <- renderText({invalidateLater(1000, session)
    paste("",as.POSIXlt(Sys.time(), tz = "UTC"),"(UTC)")})
  
  output$timer_nfl <- renderText({invalidateLater(1000, session)
    paste("",as.POSIXlt(Sys.time(), tz = "UTC"),"(UTC)")})
  
  output$timer_qb <- renderText({invalidateLater(1000, session)
    paste("",as.POSIXlt(Sys.time(), tz = "UTC"),"(UTC)")})
  
  observeEvent(input$rted_menu, 
  {
   if (input$rted_menu == "dashboard")
   {
  #new brunswick front dashboard
  st_tm_dsh_nb <- Sys.time()
  check_nb_stat_api <- function(){status_api("DF_HFED_NB","CA_NB","H","LOAD",NULL,NULL,1)}
  get_nb_stat_api <- function(){status_api("DF_HFED_NB","CA_NB","H","LOAD",NULL,NULL,1)}
  nb_stat_api <- reactivePoll(intervalMillis = 1800000, session = session,
                              checkFunc = check_nb_stat_api, valueFunc = get_nb_stat_api)
  nb_status <- reactive({nb_stat_api()})
  observeEvent(nb_stat_api(),{
      if(setequal(nb_status(),200)){
      nb_api_stat <<- reactive({HTML(paste("<i style='color:green; font-style:italic;'> Okay:",nb_status(),"</i>"))})
      test_nb_dat <<- dataset("DF_HFED_NB","CA_NB","H","LOAD",NULL,NULL,1) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
      log_info("Current NB Date: {test_nb_dat$DATETIME_LOCAL}")
      if((as.Date(test_nb_dat$DATETIME_LOCAL) == as.Date(Sys.time()))||(as.Date(test_nb_dat$DATETIME_LOCAL) == as.Date(Sys.time())-1))
      {
        check_db_nb <- function(){dataset("DF_HFED_NB","CA_NB","H","LOAD",sdmx_date_start,sdmx_date_end,NULL) %>% count(DATETIME_LOCAL)}
        get_db_nb <- function(){dataset("DF_HFED_NB","CA_NB","H","LOAD",sdmx_date_start,sdmx_date_end,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()}
        log_success("Using Latest Data for NB")
        nb_src_stat <<- reactive({HTML("<i style='color:green; font-style:italic;'> Okay</i>")})
      }
      else
      {
        check_db_nb <- function(){dataset("DF_HFED_NB","CA_NB","H","LOAD",NULL,NULL,10) %>% count(DATETIME_LOCAL)}
        get_db_nb <- function(){dataset("DF_HFED_NB","CA_NB","H","LOAD",NULL,NULL,10) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()}
        log_error("Using Nrow Data for NB")
        nb_src_stat <<- reactive({HTML("<i style='color:red; font-style:italic;'> Error</i>")})
      }
      nbload_data_pre <- reactivePoll(intervalMillis = 1800000, session = session,
                                      checkFunc = check_db_nb, valueFunc = get_db_nb)
      nbload_data <- reactive({nbload_data_pre()})
      nbload_data_mean_cr <- reactive({head(nbload_data()$OBS_VALUE,1)})
      nbload_data_mean_pst <- reactive({nbload_data()$OBS_VALUE[2]})
      nbload_data_mean_diff <- reactive({(nbload_data_mean_cr()-nbload_data_mean_pst())/nbload_data_mean_pst()})
      nbload_data_mean_prcnt <- reactive({nbload_data_mean_diff()*100})
      observeEvent(nbload_data_pre(),{
        
        if(nbload_data_mean_prcnt() < 0){
          removeClass("nbmean","green_output")
          addClass("nbmean","red_output")
          output$NB_MEAN <- renderUI({HTML(paste("<h4><i class='fa fa-arrow-down'>",abs(round(nbload_data_mean_prcnt(),digits = 2)),"% in last hour</i></h4>"))})
        }
        if(nbload_data_mean_prcnt() > 0){
          removeClass("nbmean","red_output")
          addClass("nbmean","green_output")
          output$NB_MEAN <- renderUI({HTML(paste("<h4><i class='fa fa-arrow-up'>",abs(round(nbload_data_mean_prcnt(),digits = 2)),"% in last hour</i></h4>"))})
        }})
      nbload_subset <- reactive({nbload_data()})
      nb_load_ts <-  reactive({xts(nbload_subset()$OBS_VALUE,as.POSIXlt(nbload_subset()$DATETIME_LOCAL,format = "%Y-%m-%d %H:%M:%S"))})
      
      
      output$NB_load <- renderUI({
        highchart(height = 400) %>% 
          hc_xAxis(type = "datetime",labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(nb_load_ts(), type = "line", name = "Load: ", color = "maroon") %>%
          hc_plotOptions(series = list(turboThreshold = 1)) %>% 
          hc_navigator(enabled = TRUE)})
      
      
      log_success("Dashboard started, Status:{nb_status()}")
      
    }
    else if(nb_status() != 200)
    {
      output$NB_load <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='nb_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",nb_status(),"Error</i>
                                     </div>
                                           "))})
      nb_src_stat <<- reactive({HTML("<i style='color:red; font-style:italic;'> Error</i>")})
      nb_api_stat <<- reactive({HTML(paste("<i style='color:red; font-style:italic;'> Error:",nb_status(),"</i>"))})
      log_error("Error Dashboard started, Status:{nb_status()}")
    }
    
    en_tm_dsh_nb <- Sys.time()
    log_info('{difftime(en_tm_dsh_nb,st_tm_dsh_nb)} sec for NB dashboard')
  })
  #end new brunswick
  
  #start nova scotia front
  st_tm_dsh_ns <- Sys.time()
  check_ns_stat_api <- function(){status_api("DF_HFED_NS","CA_NS","H","LOAD",NULL,NULL,1)}
  get_ns_stat_api <- function(){status_api("DF_HFED_NS","CA_NS","H","LOAD",NULL,NULL,1)}
  ns_stat_api <- reactivePoll(intervalMillis = 1800000, session = session,
                              checkFunc = check_ns_stat_api, valueFunc = get_ns_stat_api)
  ns_status <- reactive({ns_stat_api()})
  observeEvent(ns_stat_api(),{
    if(setequal(ns_status(),200)){
      ns_api_stat <<- reactive({HTML(paste("<i style='color:green; font-style:italic;'> Okay:",ns_status(),"</i>"))})
      test_ns_dat <<- dataset("DF_HFED_NS","CA_NS","N","LOAD",NULL,NULL,1) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
      log_info("Current NS Date: {test_ns_dat$DATETIME_LOCAL}")
      if((as.Date(test_ns_dat$DATETIME_LOCAL) == as.Date(Sys.time()))||(as.Date(test_ns_dat$DATETIME_LOCAL) == as.Date(Sys.time())-1))
      {
        check_db_ns <- function(){dataset("DF_HFED_NS","CA_NS","H","LOAD",sdmx_date_start,sdmx_date_end,NULL) %>% count(DATETIME_LOCAL)}
        get_db_ns <- function(){dataset("DF_HFED_NS","CA_NS","H","LOAD",sdmx_date_start,sdmx_date_end,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()}
        log_success("Using Latest Data for NS")
        ns_src_stat <<- reactive({HTML("<i style='color:green; font-style:italic;'> Okay</i>")})
      }
      else
      {
        check_db_ns <- function(){dataset("DF_HFED_NS","CA_NS","H","LOAD",NULL,NULL,10) %>% count(DATETIME_LOCAL)}
        get_db_ns <- function(){dataset("DF_HFED_NS","CA_NS","H","LOAD",NULL,NULL,10) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()}
        log_error("Using Nrow Data for NS")
        ns_src_stat <<- reactive({HTML("<i style='color:red; font-style:italic;'> Error</i>")})
      }
      nsload_data_pre <- reactivePoll(intervalMillis = 1800000, session = session,
                                      checkFunc = check_db_ns, valueFunc = get_db_ns)
      nsload_data <- reactive({nsload_data_pre()})
      nsload_data_mean_cr <- reactive({head(nsload_data()$OBS_VALUE,1)})
      nsload_data_mean_pst <- reactive({nsload_data()$OBS_VALUE[2]})
      nsload_data_mean_diff <- reactive({(nsload_data_mean_cr()-nsload_data_mean_pst())/nsload_data_mean_pst()})
      nsload_data_mean_prcnt <- reactive({nsload_data_mean_diff()*100})
      observeEvent(nsload_data_pre(),{
        
        if(nsload_data_mean_prcnt() < 0){
          removeClass("nsmean","green_output")
          addClass("nsmean","red_output")
          output$NS_MEAN <- renderUI({HTML(paste("<h4><i class='fa fa-arrow-down'>",abs(round(nsload_data_mean_prcnt(),digits = 2)),"% in last hour</i></h4>"))})
        }
        if(nsload_data_mean_prcnt() > 0){
          removeClass("nsmean","red_output")
          addClass("nsmean","green_output")
          output$NS_MEAN <- renderUI({HTML(paste("<h4><i class='fa fa-arrow-up'>",abs(round(nsload_data_mean_prcnt(),digits = 2)),"% in last hour</i></h4>"))})
        }})
      nsload_subset <- reactive({nsload_data()})
      ns_load_ts <-  reactive({xts(nsload_subset()$OBS_VALUE,as.POSIXlt(nsload_subset()$DATETIME_LOCAL,format = "%Y-%m-%d %H:%M:%S"))})
      
      
      output$NS_load <- renderUI({
        highchart(height = 400) %>% 
          hc_xAxis(type = "datetime") %>%
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>% 
          hc_add_series(ns_load_ts(), type = "line", name = "Load: ", color = "maroon") %>% 
          hc_plotOptions(series = list(turboThreshold = 1)) %>%
          hc_navigator(enabled = TRUE)})
      
      
      log_success("Dashboard started, Status:{ns_status()}")
      
    }
    else if(ns_status() != 200)
    {
      output$NS_load <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='ns_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",ns_status(),"Error</i>
                                     </div>
                                           "))})
      ns_src_stat <<- reactive({HTML("<i style='color:red; font-style:italic;'> Error</i>")})
      ns_api_stat <<- reactive({HTML(paste("<i style='color:red; font-style:italic;'> Error:",ns_status(),"</i>"))})
      log_error("Error Dashboard started, Status:{ns_status()}")
    }
    
    en_tm_dsh_ns <- Sys.time()
    log_info('{difftime(en_tm_dsh_ns,st_tm_dsh_ns)} sec for NS dashboard')
  })
  #end nova scotia
  
  #BC Front Dashboard
  st_tm_dsh_bc <- Sys.time()
  check_bc_stat_api <- function(){status_api("DF_HFED_BC","CA_BC","H","LOAD",NULL,NULL,1)}
  get_bc_stat_api <- function(){status_api("DF_HFED_BC","CA_BC","H","LOAD",NULL,NULL,1)}
  bc_stat_api <- reactivePoll(intervalMillis = 1800000, session = session,
               checkFunc = check_bc_stat_api, valueFunc = get_bc_stat_api)
  bc_status <- reactive({bc_stat_api()})
  observeEvent(bc_stat_api(),{
  if(bc_status() == 200){
    bc_api_stat <<- reactive({HTML(paste("<i style='color:green; font-style:italic;'> Okay:",bc_status(),"</i>"))})
    test_bc_dat <<- dataset("DF_HFED_BC","CA_BC","H","LOAD",NULL,NULL,1) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
    log_info("Current BC Date: {test_bc_dat$DATETIME_LOCAL}")
    if((as.Date(test_bc_dat$DATETIME_LOCAL) == as.Date(Sys.time()))||(as.Date(test_bc_dat$DATETIME_LOCAL) == as.Date(Sys.time())-1))
    {
      check_db_bc <- function(){dataset("DF_HFED_BC","CA_BC","H","LOAD",sdmx_date_start,sdmx_date_end,NULL) %>% count(DATETIME_LOCAL)}
      get_db_bc <- function(){dataset("DF_HFED_BC","CA_BC","H","LOAD",sdmx_date_start,sdmx_date_end,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()}
      log_success("Using Latest Data for BC")
      bc_src_stat <<- reactive({HTML("<i style='color:green; font-style:italic;'> Okay</i>")})
    }
    else
    {
      check_db_bc <- function(){dataset("DF_HFED_BC","CA_BC","H","LOAD",NULL,NULL,10) %>% count(DATETIME_LOCAL)}
      get_db_bc <- function(){dataset("DF_HFED_BC","CA_BC","H","LOAD",NULL,NULL,10) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()}
      log_error("Using Nrow Data for BC")
      bc_src_stat <<- reactive({HTML("<i style='color:red; font-style:italic;'> Error</i>")})
    }
  bcload_data_pre <- reactivePoll(intervalMillis = 1800000, session = session,
                                  checkFunc = check_db_bc, valueFunc = get_db_bc)
  bcload_data <- reactive({bcload_data_pre()})
  bcload_data_mean_cr <- reactive({head(bcload_data()$OBS_VALUE,1)})
  bcload_data_mean_pst <- reactive({bcload_data()$OBS_VALUE[2]})
  bcload_data_mean_diff <- reactive({(bcload_data_mean_cr()-bcload_data_mean_pst())/bcload_data_mean_pst()})
  bcload_data_mean_prcnt <- reactive({bcload_data_mean_diff()*100})
  observeEvent(bcload_data_pre(),{
    
    if(bcload_data_mean_prcnt() < 0){
      removeClass("bcmean","green_output")
      addClass("bcmean","red_output")
      output$BC_MEAN <- renderUI({HTML(paste("<h4><i class='fa fa-arrow-down'>",abs(round(bcload_data_mean_prcnt(),digits = 2)),"% in last hour</i></h4>"))})
    }
    if(bcload_data_mean_prcnt() > 0){
      removeClass("bcmean","red_output")
      addClass("bcmean","green_output")
      output$BC_MEAN <- renderUI({HTML(paste("<h4><i class='fa fa-arrow-up'>",abs(round(bcload_data_mean_prcnt(),digits = 2)),"% in last hour</i></h4>"))})
    }})
  bcload_subset <- reactive({bcload_data()})
  bc_load_ts <-  reactive({xts(bcload_subset()$OBS_VALUE,as.POSIXlt(bcload_subset()$DATETIME_LOCAL,format = "%Y-%m-%d %H:%M:%S"))})
  
  
  output$BC_load <- renderUI({
    highchart(height = 400) %>% 
      hc_xAxis(type = "datetime") %>%
      hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
      hc_add_series(bc_load_ts(), type = "line", name = "Load: ", color = "maroon") %>% 
      hc_plotOptions(series = list(turboThreshold = 1)) %>%
      hc_navigator(enabled = TRUE)})
  
  
  log_success("Dashboard started, Status:{bc_status()}")
  
  }
  else if(bc_status() != 200)
    {
    output$BC_load <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='bc_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",bc_status(),"Error</i>
                                     </div>
                                           "))})
    bc_src_stat <<- reactive({HTML("<i style='color:red; font-style:italic;'> Error</i>")})
    bc_api_stat <<- reactive({HTML(paste("<i style='color:red; font-style:italic;'> Error:",bc_status(),"</i>"))})
    log_error("Error Dashboard started, Status:{bc_status()}")
    }
  
  en_tm_dsh_bc <- Sys.time()
  log_info('{difftime(en_tm_dsh_bc,st_tm_dsh_bc)} sec for BC dashboard')
  })
  #BC-front-end
  
  #output$AB_load <- renderHighchart({ab_load_chart})
  
  # start front Ontario 
  st_tm_dsh_on <- Sys.time()
  check_on_stat_api <- function(){status_api("DF_HFED_ON","CA_ON","H","ONTARIO_DEMAND",NULL,NULL,1)}
  get_on_stat_api <- function(){status_api("DF_HFED_ON","CA_ON","H","ONTARIO_DEMAND",NULL,NULL,1)}
  on_stat_api <- reactivePoll(intervalMillis = 1800000, session = session,
                              checkFunc = check_on_stat_api, valueFunc = get_on_stat_api)
  on_status <- reactive({on_stat_api()})
  observeEvent(on_stat_api(),{
    if(on_status() == 200){
      on_api_stat <<- reactive({HTML(paste("<i style='color:green; font-style:italic;'> Okay:",on_status(),"</i>"))})
      test_on_dat <<- dataset("DF_HFED_ON","CA_ON","H","ONTARIO_DEMAND",NULL,NULL,1) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
      log_info("Current ON Date: {test_on_dat$DATETIME_LOCAL}")
      if((as.Date(test_on_dat$DATETIME_LOCAL) == as.Date(Sys.time()))||(as.Date(test_on_dat$DATETIME_LOCAL) == as.Date(Sys.time())-1))
      {
        check_db_on <- function(){dataset("DF_HFED_ON","CA_ON","H","ONTARIO_DEMAND",sdmx_date_start,sdmx_date_end,NULL) %>% count(DATETIME_LOCAL)}
        get_db_on <- function(){dataset("DF_HFED_ON","CA_ON","H","ONTARIO_DEMAND",sdmx_date_start,sdmx_date_end,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()}
        log_success("Using Latest Data for ON")
        on_src_stat <<- reactive({HTML("<i style='color:green; font-style:italic;'> Okay</i>")})
      }
      else
      {
        check_db_on <- function(){dataset("DF_HFED_ON","CA_ON","H","ONTARIO_DEMAND",NULL,NULL,10) %>% count(DATETIME_LOCAL)}
        get_db_on <- function(){dataset("DF_HFED_ON","CA_ON","H","ONTARIO_DEMAND",NULL,NULL,10) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()}
        log_error("Using Nrow Data for ON")
        on_src_stat <<- reactive({HTML("<i style='color:red; font-style:italic;'> Error</i>")})
      }
      onload_data_pre <- reactivePoll(intervalMillis = 1800000, session = session,
                                      checkFunc = check_db_on, valueFunc = get_db_on)
      onload_data <- reactive({onload_data_pre()})
      onload_data_mean_cr <- reactive({head(onload_data()$OBS_VALUE,1)})
      onload_data_mean_pst <- reactive({onload_data()$OBS_VALUE[2]})
      onload_data_mean_diff <- reactive({(onload_data_mean_cr()-onload_data_mean_pst())/onload_data_mean_pst()})
      onload_data_mean_prcnt <- reactive({onload_data_mean_diff()*100})
      observeEvent(onload_data_pre(),{
        
        if(onload_data_mean_prcnt() < 0){
          removeClass("onmean","green_output")
          addClass("onmean","red_output")
          output$ON_MEAN <- renderUI({HTML(paste("<h4><i class='fa fa-arrow-down'>",abs(round(onload_data_mean_prcnt(),digits = 2)),"% in last hour</i></h4>"))})
        }
        if(onload_data_mean_prcnt() > 0){
          removeClass("onmean","red_output")
          addClass("onmean","green_output")
          output$ON_MEAN <- renderUI({HTML(paste("<h4><i class='fa fa-arrow-up'>",abs(round(onload_data_mean_prcnt(),digits = 2)),"% in last hour</i></h4>"))})
        }})
      onload_subset <- reactive({onload_data()})
      on_load_ts <-  reactive({xts(onload_subset()$OBS_VALUE,as.POSIXlt(onload_subset()$DATETIME_LOCAL,format = "%Y-%m-%d %H:%M:%S"))})
      
      
      output$ON_load <- renderUI({
        highchart(height = 400) %>% 
          hc_xAxis(type = "datetime") %>%
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(on_load_ts(), type = "line", name = "Load: ", color = "maroon") %>% 
          hc_plotOptions(series = list(turboThreshold = 1)) %>%
          hc_navigator(enabled = TRUE)})
      
      
      log_success("Dashboard started, Status:{on_status()}")
      
    }
    else if(on_status() != 200)
    {
      output$ON_load <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='on_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",on_status(),"Error</i>
                                     </div>
                                           "))})
      on_src_stat <<- reactive({HTML("<i style='color:red; font-style:italic;'> Error</i>")})
      on_api_stat <<- reactive({HTML(paste("<i style='color:red; font-style:italic;'> Error:",on_status(),"</i>"))})
      log_error("Error Dashboard started, Status:{on_status()}")
    }
    
    en_tm_dsh_on <- Sys.time()
    log_info('{difftime(en_tm_dsh_on,st_tm_dsh_on)} sec for ON dashboard')
  })
#end Ontario front

#start Prince edward island front
  st_tm_dsh_pei <- Sys.time()
  test_pei_dat <<- dataset("DF_HFED_PE","CA_PE","H","ON_ISL_LOAD",NULL,NULL,1) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
  check_pei_stat_api <- function(){status_api("DF_HFED_PE","CA_PE","H","ON_ISL_LOAD",NULL,NULL,1)}
  get_pei_stat_api <- function(){status_api("DF_HFED_PE","CA_PE","H","ON_ISL_LOAD",NULL,NULL,1)}
  pei_stat_api <- reactivePoll(intervalMillis = 1800000, session = session,
                               checkFunc = check_pei_stat_api, valueFunc = get_pei_stat_api)
  pei_status <- reactive({pei_stat_api()})
  observeEvent(pei_stat_api(),{
    if(pei_status() == 200){
      pei_api_stat <<- reactive({HTML(paste("<i style='color:green; font-style:italic;'> Okay:",pei_status(),"</i>"))})
      log_info("Current PEI Date: {test_pei_dat$DATETIME_LOCAL}")
      if((as.Date(test_pei_dat$DATETIME_LOCAL) == as.Date(Sys.time()))||(as.Date(test_pei_dat$DATETIME_LOCAL) == as.Date(Sys.time())-1))
      {
        check_db_pei <- function(){dataset("DF_HFED_PE","CA_PE","H","ON_ISL_LOAD",sdmx_date_start,sdmx_date_end,NULL) %>% count(DATETIME_LOCAL)}
        get_db_pei <- function(){dataset("DF_HFED_PE","CA_PE","H","ON_ISL_LOAD",sdmx_date_start,sdmx_date_end,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()}
        log_success("Using Latest Data for PEI")
        pei_src_stat <<- reactive({HTML("<i style='color:green; font-style:italic;'> Okay</i>")})
      }
      else
      {
        check_db_pei <- function(){dataset("DF_HFED_PE","CA_PE","H","ON_ISL_LOAD",NULL,NULL,10) %>% count(DATETIME_LOCAL)}
        get_db_pei <- function(){dataset("DF_HFED_PE","CA_PE","H","ON_ISL_LOAD",NULL,NULL,10) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()}
        log_error("Using Nrow Data for PEI")
        pei_src_stat <<- reactive({HTML("<i style='color:red; font-style:italic;'> Error</i>")})
      }
      peiload_data_pre <- reactivePoll(intervalMillis = 1800000, session = session,
                                       checkFunc = check_db_pei, valueFunc = get_db_pei)
      peiload_data <- reactive({peiload_data_pre()})
      peiload_data_mean_cr <- reactive({head(peiload_data()$OBS_VALUE,1)})
      peiload_data_mean_pst <- reactive({peiload_data()$OBS_VALUE[2]})
      peiload_data_mean_diff <- reactive({(peiload_data_mean_cr()-peiload_data_mean_pst())/peiload_data_mean_pst()})
      peiload_data_mean_prcnt <- reactive({peiload_data_mean_diff()*100})
      observeEvent(peiload_data_pre(),{
        
        if(peiload_data_mean_prcnt() < 0){
          removeClass("peimean","green_output")
          addClass("peimean","red_output")
          output$PEI_MEAN <- renderUI({HTML(paste("<h4><i class='fa fa-arrow-down'>",abs(round(peiload_data_mean_prcnt(),digits = 2)),"% in last hour</i></h4>"))})
        }
        if(peiload_data_mean_prcnt() > 0){
          removeClass("peimean","red_output")
          addClass("peimean","green_output")
          output$PEI_MEAN <- renderUI({HTML(paste("<h4><i class='fa fa-arrow-up'>",abs(round(peiload_data_mean_prcnt(),digits = 2)),"% in last hour</i></h4>"))})
        }})
      peiload_subset <- reactive({peiload_data()})
      pei_load_ts <-  reactive({xts(peiload_subset()$OBS_VALUE,as.POSIXlt(peiload_subset()$DATETIME_LOCAL,format = "%Y-%m-%d %H:%M:%S"))})
      
      
      output$PEI_load <- renderUI({
        highchart(height = 400) %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(pei_load_ts(), type = "line", name = "Load: ", color = "maroon") %>% 
          hc_plotOptions(series = list(turboThreshold = 1)) %>%
          hc_navigator(enabled = TRUE)})
      
      
      log_success("Dashboard started, Status:{pei_status()}")
      
    }
    else if(pei_status() != 200)
    {
      output$PEI_load <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='pei_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",pei_status(),"Error</i>
                                     </div>
                                           "))})
      pei_src_stat <<- reactive({HTML("<i style='color:red; font-style:italic;'> Error</i>")})
      pei_api_stat <<- reactive({HTML(paste("<i style='color:red; font-style:italic;'> Error:",pei_status(),"</i>"))})
      log_error("Error Dashboard started, Status:{pei_status()}")
    }
    
    en_tm_dsh_pei <- Sys.time()
    log_info('{difftime(en_tm_dsh_pei,st_tm_dsh_pei)} sec for PEI dashboard')
  })
#end PEI front

#start front Newfound & labradour
  st_tm_dsh_nfl <- Sys.time()
  check_nfl_stat_api <- function(){status_api("DF_HFED_NL","CA_NL","H","DEMAND",NULL,NULL,1)}
  get_nfl_stat_api <- function(){status_api("DF_HFED_NL","CA_NL","H","DEMAND",NULL,NULL,1)}
  nfl_stat_api <- reactivePoll(intervalMillis = 1800000, session = session,
                               checkFunc = check_nfl_stat_api, valueFunc = get_nfl_stat_api)
  nfl_status <- reactive({nfl_stat_api()})
  observeEvent(nfl_stat_api(),{
    if(nfl_status() == 200)
      {
      nfl_api_stat <<- reactive({HTML(paste("<i style='color:green; font-style:italic;'> Okay:",nfl_status(),"</i>"))})
      test_nfl_dat <<- dataset("DF_HFED_NL","CA_NL","H","DEMAND",NULL,NULL,1) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
      log_info("Current NFL Date: {test_nfl_dat$DATETIME_LOCAL}")
      if((as.Date(test_nfl_dat$DATETIME_LOCAL) == as.Date(Sys.time()))||(as.Date(test_nfl_dat$DATETIME_LOCAL) == as.Date(Sys.time())-1))
      {
        check_db_nfl <- function(){dataset("DF_HFED_NL","CA_NL","H","DEMAND",sdmx_date_start,sdmx_date_end,NULL) %>% count(DATETIME_LOCAL)}
        get_db_nfl <- function(){dataset("DF_HFED_NL","CA_NL","H","DEMAND",sdmx_date_start,sdmx_date_end,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()}
        log_success("Using Latest Data for NFL")
        nfl_src_stat <<- reactive({HTML("<i style='color:green; font-style:italic;'> Okay</i>")})
      }
      else
      {
        check_db_nfl <- function(){dataset("DF_HFED_NL","CA_NL","H","DEMAND",NULL,NULL,10) %>% count(DATETIME_LOCAL)}
        get_db_nfl <- function(){dataset("DF_HFED_NL","CA_NL","H","DEMAND",NULL,NULL,10) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()}
        log_error("Using Nrow Data for NFL")
        nfl_src_stat <<- reactive({HTML("<i style='color:red; font-style:italic;'> Error</i>")})
      }
      nflload_data_pre <- reactivePoll(intervalMillis = 1800000, session = session,
                                       checkFunc = check_db_nfl, valueFunc = get_db_nfl)
      nflload_data <- reactive({nflload_data_pre()})
      nflload_data_mean_cr <- reactive({head(nflload_data()$OBS_VALUE,1)})
      nflload_data_mean_pst <- reactive({nflload_data()$OBS_VALUE[2]})
      nflload_data_mean_diff <- reactive({(nflload_data_mean_cr()-nflload_data_mean_pst())/nflload_data_mean_pst()})
      nflload_data_mean_prcnt <- reactive({nflload_data_mean_diff()*100})
      observeEvent(nflload_data_pre(),{
        
        if(nflload_data_mean_prcnt() < 0){
          removeClass("nflmean","green_output")
          addClass("nflmean","red_output")
          output$NFL_MEAN <- renderUI({HTML(paste("<h4><i class='fa fa-arrow-down'>",abs(round(nflload_data_mean_prcnt(),digits = 2)),"% in last hour</i></h4>"))})
        }
        if(nflload_data_mean_prcnt() > 0){
          removeClass("nflmean","red_output")
          addClass("nflmean","green_output")
          output$NFL_MEAN <- renderUI({HTML(paste("<h4><i class='fa fa-arrow-up'>",abs(round(nflload_data_mean_prcnt(),digits = 2)),"% in last hour</i></h4>"))})
        }})
      nflload_subset <- reactive({nflload_data()})
      nfl_load_ts <-  reactive({xts(nflload_subset()$OBS_VALUE,as.POSIXlt(nflload_subset()$DATETIME_LOCAL,format = "%Y-%m-%d %H:%M:%S"))})
      
      
      output$NFL_load <- renderUI({
        highchart(height = 400) %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(nfl_load_ts(), type = "line", name = "Load: ", color = "maroon") %>% 
          hc_plotOptions(series = list(turboThreshold = 1)) %>%
          hc_navigator(enabled = TRUE)})
      
      
      log_success("Dashboard started, Status:{nfl_status()}")
      
    }
    else if((nfl_status() != 200) || (is.na(nfl_status())))
    {
      output$NFL_load <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='nfl_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",nfl_status(),"Error</i>
                                     </div>
                                           "))})
      nfl_src_stat <<- reactive({HTML("<i style='color:red; font-style:italic;'> Error</i>")})
      nfl_api_stat <<- reactive({HTML(paste("<i style='color:red; font-style:italic;'> Error:",nfl_status(),"</i>"))})
      log_error("Error Dashboard started, Status:{nfl_status()}")
    }
    
    en_tm_dsh_nfl <- Sys.time()
    log_info('{difftime(en_tm_dsh_nfl,st_tm_dsh_nfl)} sec for NFL dashboard')
  })
#end front NFL
  
#start front Quebec  
  st_tm_dsh_qb <- Sys.time()
  check_qb_stat_api <- function(){status_api("DF_HFED_QC","CA_QC","H","DEMAND",NULL,NULL,1)}
  get_qb_stat_api <- function(){status_api("DF_HFED_QC","CA_QC","H","DEMAND",NULL,NULL,1)}
  qb_stat_api <- reactivePoll(intervalMillis = 1800000, session = session,
                              checkFunc = check_qb_stat_api, valueFunc = get_qb_stat_api)
  qb_status <- reactive({qb_stat_api()})
  observeEvent(qb_stat_api(),{
    if(qb_status() == 200)
      {
      qb_api_stat <<- reactive({HTML(paste("<i style='color:green; font-style:italic;'> Okay:",qb_status(),"</i>"))})
      test_qb_dat <<- dataset("DF_HFED_QC","CA_QC","H","DEMAND",NULL,NULL,1) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
      log_info("Current QB Date: {test_qb_dat$DATETIME_LOCAL}")
      if((as.Date(test_qb_dat$DATETIME_LOCAL) == as.Date(Sys.time()))||(as.Date(test_qb_dat$DATETIME_LOCAL) == as.Date(Sys.time())-1))
      {
        check_db_qb <- function(){dataset("DF_HFED_QC","CA_QC","H","DEMAND",sdmx_date_start,sdmx_date_end,NULL) %>% count(DATETIME_LOCAL)}
        get_db_qb <- function(){dataset("DF_HFED_QC","CA_QC","H","DEMAND",sdmx_date_start,sdmx_date_end,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()}
        log_success("Using Latest Data for QB")
        qb_src_stat <<- reactive({HTML("<i style='color:green; font-style:italic;'> Okay</i>")})
      }
      else
      {
        check_db_qb <- function(){dataset("DF_HFED_QC","CA_QC","H","DEMAND",NULL,NULL,10) %>% count(DATETIME_LOCAL)}
        get_db_qb <- function(){dataset("DF_HFED_QC","CA_QC","H","DEMAND",NULL,NULL,10) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()}
        log_error("Using Nrow Data for QB")
        qb_src_stat <<- reactive({HTML("<i style='color:red; font-style:italic;'> Error</i>")})
      }
      qbload_data_pre <- reactivePoll(intervalMillis = 1800000, session = session,
                                      checkFunc = check_db_qb, valueFunc = get_db_qb)
      qbload_data <- reactive({qbload_data_pre()})
      qbload_data_mean_cr <- reactive({head(qbload_data()$OBS_VALUE,1)})
      qbload_data_mean_pst <- reactive({qbload_data()$OBS_VALUE[2]})
      qbload_data_mean_diff <- reactive({(qbload_data_mean_cr()-qbload_data_mean_pst())/qbload_data_mean_pst()})
      qbload_data_mean_prcnt <- reactive({qbload_data_mean_diff()*100})
      observeEvent(qbload_data_pre(),{
        
        if(qbload_data_mean_prcnt() < 0){
          removeClass("qbmean","green_output")
          addClass("qbmean","red_output")
          output$QB_MEAN <- renderUI({HTML(paste("<h4><i class='fa fa-arrow-down'>",abs(round(qbload_data_mean_prcnt(),digits = 2)),"% in last hour</i></h4>"))})
        }
        if(qbload_data_mean_prcnt() > 0){
          removeClass("qbmean","red_output")
          addClass("qbmean","green_output")
          output$QB_MEAN <- renderUI({HTML(paste("<h4><i class='fa fa-arrow-up'>",abs(round(qbload_data_mean_prcnt(),digits = 2)),"% in last hour</i></h4>"))})
        }})
      qbload_subset <- reactive({qbload_data()})
      qb_load_ts <-  reactive({xts(qbload_subset()$OBS_VALUE,as.POSIXlt(qbload_subset()$DATETIME_LOCAL,format = "%Y-%m-%d %H:%M:%S"))})
      
      
      output$QB_load <- renderUI({
        highchart(height = 400) %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(qb_load_ts(), type = "line", name = "Load: ", color = "maroon") %>% 
          hc_plotOptions(series = list(turboThreshold = 1)) %>%
          hc_navigator(enabled = TRUE)})
      
      
      log_success("Dashboard started, Status:{qb_status()}")
      
    }
    else if((qb_status() != 200) || (is.na(qb_status())))
    {
      output$QB_load <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='qb_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",qb_status(),"Error</i>
                                     </div>
                                           "))})
      qb_src_stat <<- reactive({HTML("<i style='color:red; font-style:italic;'> Error</i>")})
      qb_api_stat <<- reactive({HTML(paste("<i style='color:red; font-style:italic;'> Error:",qb_status(),"</i>"))})
      log_error("Error Dashboard started, Status:{qb_status()}")
    }
    
    en_tm_dsh_qb <- Sys.time()
    log_info('{difftime(en_tm_dsh_qb,st_tm_dsh_qb)} sec for QB dashboard')
  })
#end front QB
   }
  
else if(input$rted_menu == "pei")
    {
  #PRINCE EDWARD ISLAND START
  st_tm_dsh_pei <- Sys.time()
  test_pei_dat_ind <<- dataset("DF_HFED_PE","CA_PE","N","ON_ISL_LOAD",NULL,NULL,1) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
  check_pei_stat_api_ind <- function(){status_api("DF_HFED_PE","CA_PE","N","ON_ISL_LOAD",NULL,NULL,1)}
  get_pei_stat_api_ind <- function(){status_api("DF_HFED_PE","CA_PE","N","ON_ISL_LOAD",NULL,NULL,1)}
  pei_stat_api_ind <- reactivePoll(intervalMillis = 1800000, session = session,
                                   checkFunc = check_pei_stat_api_ind, valueFunc = get_pei_stat_api_ind)
  pei_status_ind <- reactive({pei_stat_api_ind()})
  observeEvent(pei_stat_api_ind(),{
    if(pei_status_ind() == 200){
      if((as.Date(test_pei_dat_ind$DATETIME_LOCAL) == as.Date(Sys.time()))||(as.Date(test_pei_dat_ind$DATETIME_LOCAL) == as.Date(Sys.time())-1))
      {
        pei_ind_dat_load <<- dataset("DF_HFED_PE","CA_PE","N","ON_ISL_LOAD",sdmx_date_start_ind_p,sdmx_date_end_ind_p,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
        pei_ind_dat_fossil <<- dataset("DF_HFED_PE","CA_PE","N","ON_ISL_FOSSIL",sdmx_date_start_ind_p,sdmx_date_end_ind_p,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
        pei_ind_dat_wind_export <<- dataset("DF_HFED_PE","CA_PE","N","WIND_EXPORT",sdmx_date_start_ind_p,sdmx_date_end_ind_p,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
        pei_ind_dat_wind_percent <<- dataset("DF_HFED_PE","CA_PE","N","WIND_PERCENT",sdmx_date_start_ind_p,sdmx_date_end_ind_p,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
        pei_ind_dat_wind <<- dataset("DF_HFED_PE","CA_PE","N","ON_ISL_WIND",sdmx_date_start_ind_p,sdmx_date_end_ind_p,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
        pei_ind_dat_wind_local <<- dataset("DF_HFED_PE","CA_PE","N","WIND_LOCAL",sdmx_date_start_ind_p,sdmx_date_end_ind_p,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
        pei_ind_dat_cables <<- dataset("DF_HFED_PE","CA_PE","N","IMPORT_CABLES",sdmx_date_start_ind_p,sdmx_date_end_ind_p,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
        #updating the dates after the data loads (PEI)
        pei_dd_1 <- as.Date(head(pei_ind_dat_load$DATETIME_LOCAL,1))-(7)
        pei_dd_1_1 <- paste(pei_dd_1,"00:00:00",sep=" ")
        pei_dd_2_2 <- as.Date(head(pei_ind_dat_load$DATETIME_LOCAL,1))
        updateSliderInput(session, "pei_dates_1",
                          min = as.Date(tail(pei_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          max = as.Date(head(pei_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          value=c(as.Date(pei_dd_1_1),pei_dd_2_2),
                          timeFormat="%Y-%m-%d")
        updateSliderInput(session, "pei_dates_2",
                          min = as.Date(tail(pei_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          max = as.Date(head(pei_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          value=c(as.Date(pei_dd_1_1),pei_dd_2_2),
                          timeFormat="%Y-%m-%d")
        updateSliderInput(session, "pei_dates_3",
                          min = as.Date(tail(pei_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          max = as.Date(head(pei_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          value=c(as.Date(pei_dd_1_1),pei_dd_2_2),
                          timeFormat="%Y-%m-%d")
        updateSliderInput(session, "pei_dates_4",
                          min = as.Date(tail(pei_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          max = as.Date(head(pei_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          value=c(as.Date(pei_dd_1_1),pei_dd_2_2),
                          timeFormat="%Y-%m-%d")
        log_success("Using latest data for PEI")
      }
      else{
        pei_ind_dat_load <<- dataset("DF_HFED_PE","CA_PE","N","ON_ISL_LOAD",NULL,NULL,500) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
        pei_ind_dat_fossil <<- dataset("DF_HFED_PE","CA_PE","N","ON_ISL_FOSSIL",NULL,NULL,500) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
        pei_ind_dat_wind_export <<- dataset("DF_HFED_PE","CA_PE","N","WIND_EXPORT",NULL,NULL,500) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
        pei_ind_dat_wind_percent <<- dataset("DF_HFED_PE","CA_PE","N","WIND_PERCENT",NULL,NULL,500) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
        pei_ind_dat_wind <<- dataset("DF_HFED_PE","CA_PE","N","ON_ISL_WIND",NULL,NULL,500) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
        pei_ind_dat_wind_local <<- dataset("DF_HFED_PE","CA_PE","N","WIND_LOCAL",NULL,NULL,500) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
        pei_ind_dat_cables <<- dataset("DF_HFED_PE","CA_PE","N","IMPORT_CABLES",NULL,NULL,500) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
        #updating the dates after the data loads (PEI)
        pei_dd_1 <- as.Date(head(pei_ind_dat_load$DATETIME_LOCAL,1))-(7)
        pei_dd_1_1 <- paste(pei_dd_1,"00:00:00",sep=" ")
        pei_dd_2_2 <- as.Date(head(pei_ind_dat_load$DATETIME_LOCAL,1))
        updateSliderInput(session, "pei_dates_1",
                          min = as.Date(tail(pei_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          max = as.Date(head(pei_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          value=c(as.Date(pei_dd_1_1),pei_dd_2_2),
                          timeFormat="%Y-%m-%d")
        updateSliderInput(session, "pei_dates_2",
                          min = as.Date(tail(pei_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          max = as.Date(head(pei_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          value=c(as.Date(pei_dd_1_1),pei_dd_2_2),
                          timeFormat="%Y-%m-%d")
        updateSliderInput(session, "pei_dates_3",
                          min = as.Date(tail(pei_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          max = as.Date(head(pei_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          value=c(as.Date(pei_dd_1_1),pei_dd_2_2),
                          timeFormat="%Y-%m-%d")
        updateSliderInput(session, "pei_dates_4",
                          min = as.Date(tail(pei_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          max = as.Date(head(pei_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          value=c(as.Date(pei_dd_1_1),pei_dd_2_2),
                          timeFormat="%Y-%m-%d")
        log_error("Using NROWS data for PEI")
      }
      
      st_tm_dsh_pei_1 <- Sys.time()
      pei_date_ind_1_1 <- reactive({paste(input$pei_dates_1[1],"00:00:00",sep = " ")})
      pei_date_ind_1_2 <- reactive({paste(input$pei_dates_1[2],"00:00:00",sep = " ")})
      pei_ind_subset_dat <- reactive({subset(pei_ind_dat_load,subset = (pei_ind_dat_load$DATETIME_LOCAL >= pei_date_ind_1_1() & pei_ind_dat_load$DATETIME_LOCAL <= pei_date_ind_1_2()))})
      pei_ind_dat_ts <- reactive({xts(pei_ind_dat_load$OBS_VALUE,pei_ind_dat_load$DATETIME_LOCAL)})
      pei_ind_subset_ts <- reactive({xts(pei_ind_subset_dat()$OBS_VALUE,pei_ind_subset_dat()$DATETIME_LOCAL)})
      pei_ind_dat_ts_yearly <- reactive({to.yearly(pei_ind_dat_ts())})
      pei_ind_dat_ts_monthly <- reactive({to.monthly(pei_ind_subset_ts())})
      pei_ind_dat_ts_weekly <- reactive({to.weekly(pei_ind_subset_ts())})
      pei_ind_dat_ts_daily <- reactive({to.daily(pei_ind_subset_ts())})
      pei_ind_dat_ts_hourly <- reactive({
        pei_hr <- to.hourly(pei_ind_subset_ts())
        return(pei_hr)
      })
      
      output$PEI_ON_LOAD_YEARLY <- renderUI({
        highchart() %>%
          hc_xAxis(type = "datetime") %>%
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(pei_ind_dat_ts_yearly()[,(colnames(pei_ind_dat_ts_yearly()) %in% c('pei_ind_dat_ts().High'))], type = "line", name = "High Load: ", color = "lightgreen") %>%
          hc_add_series(pei_ind_dat_ts_yearly()[,(colnames(pei_ind_dat_ts_yearly()) %in% c('pei_ind_dat_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
          hc_navigator(enabled = TRUE) %>%
          hc_exporting(enabled = TRUE, filename = "PEI_load_yearly",buttons = list(contextButton = list(menuItems = 
                                                                                                         c("viewFullscreen", "printChart", "separator", "downloadPNG", "downloadJPEG", "downloadPDF", "downloadSVG"))))
        })
      output$PEI_ON_LOAD_WEEKLY <- renderUI({
        highchart() %>%
          hc_xAxis(type = "datetime") %>%
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(pei_ind_dat_ts_weekly()[,(colnames(pei_ind_dat_ts_weekly()) %in% c('pei_ind_subset_ts().High'))], type = "line", name = "High Load: ", color = "lightgreen") %>%
          hc_add_series(pei_ind_dat_ts_weekly()[,(colnames(pei_ind_dat_ts_weekly()) %in% c('pei_ind_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
          hc_navigator(enabled = TRUE) %>%
          hc_exporting(enabled = TRUE, filename = "PEI_load_yearly",buttons = list(contextButton = list(menuItems = 
                                                                                                          c("viewFullscreen", "printChart", "separator", "downloadPNG", "downloadJPEG", "downloadPDF", "downloadSVG"))))
      })
      output$PEI_ON_LOAD_DAILY <- renderUI({
        highchart() %>%
          hc_xAxis(type = "datetime") %>%
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(pei_ind_dat_ts_daily()[,(colnames(pei_ind_dat_ts_daily()) %in% c('pei_ind_subset_ts().High'))], type = "line", name = "High Load: ", color = "lightgreen") %>%
          hc_add_series(pei_ind_dat_ts_daily()[,(colnames(pei_ind_dat_ts_daily()) %in% c('pei_ind_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
          hc_navigator(enabled = TRUE) %>%
          hc_exporting(enabled = TRUE, filename = "PEI_load_yearly",buttons = list(contextButton = list(menuItems = 
                                                                                                          c("viewFullscreen", "printChart", "separator", "downloadPNG", "downloadJPEG", "downloadPDF", "downloadSVG"))))
      })
      output$PEI_ON_LOAD_HOURLY <- renderUI({
        highchart() %>%
          hc_xAxis(type = "datetime") %>%
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(pei_ind_dat_ts_hourly()[,(colnames(pei_ind_dat_ts_hourly()) %in% c('pei_ind_subset_ts().High'))], type = "line", name = "High Load: ", color = "lightgreen") %>%
          hc_add_series(pei_ind_dat_ts_hourly()[,(colnames(pei_ind_dat_ts_hourly()) %in% c('pei_ind_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
          hc_navigator(enabled = TRUE) %>%
          hc_exporting(enabled = TRUE, filename = "PEI_load_yearly",buttons = list(contextButton = list(menuItems = 
                                                                                                          c("viewFullscreen", "printChart", "separator", "downloadPNG", "downloadJPEG", "downloadPDF", "downloadSVG"))))
      })
      output$PEI_ON_LOAD_ALL <- renderUI({
        highchart() %>%
          hc_xAxis(type = "datetime") %>%
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(pei_ind_subset_ts(), type = "line", name = "Load: ") %>%
          hc_navigator(enabled = TRUE) %>%
          hc_exporting(enabled = TRUE, filename = "PEI_load_yearly",buttons = list(contextButton = list(menuItems = 
                                                                                                          c("viewFullscreen", "printChart", "separator", "downloadPNG", "downloadJPEG", "downloadPDF", "downloadSVG"))))
        
      })
      en_tm_dsh_pei_1 <- Sys.time()
      log_info('{difftime(en_tm_dsh_pei_1,st_tm_dsh_pei_1)} sec for PEI Main 1 dashboard')
      
      st_tm_dsh_pei_2 <- Sys.time()
      pei_date_ind_2_1 <- reactive({paste(input$pei_dates_2[1],"00:00:00",sep = " ")})
      pei_date_ind_2_2 <- reactive({paste(input$pei_dates_2[2],"00:00:00",sep = " ")})
      pei_ind_subset_dat_2_wind <- reactive({subset(pei_ind_dat_wind,subset = (pei_ind_dat_wind$DATETIME_LOCAL >= pei_date_ind_2_1() & pei_ind_dat_wind$DATETIME_LOCAL <= pei_date_ind_2_2()))})
      pei_ind_subset_dat_2_fossil <- reactive({subset(pei_ind_dat_fossil,subset = (pei_ind_dat_fossil$DATETIME_LOCAL >= pei_date_ind_2_1() & pei_ind_dat_fossil$DATETIME_LOCAL <= pei_date_ind_2_2()))})
      pei_ind_dat_ts_2 <- reactive({xts(pei_ind_dat_wind$OBS_VALUE,pei_ind_dat_wind$DATETIME_LOCAL)})
      pei_ind_dat_ts_2_fossil <- reactive({xts(pei_ind_dat_fossil$OBS_VALUE,pei_ind_dat_fossil$DATETIME_LOCAL)})
      pei_ind_subset_ts_2 <- reactive({xts(pei_ind_subset_dat_2_wind()$OBS_VALUE,pei_ind_subset_dat_2_wind()$DATETIME_LOCAL)})
      pei_ind_subset_ts_2_fossil <- reactive({xts(pei_ind_subset_dat_2_fossil()$OBS_VALUE,pei_ind_subset_dat_2_fossil()$DATETIME_LOCAL)})
      pei_ind_dat_ts_yearly_2 <- reactive({to.yearly(pei_ind_dat_ts_2())})
      pei_ind_dat_ts_monthly_2 <- reactive({to.monthly(pei_ind_subset_ts_2())})
      pei_ind_dat_ts_weekly_2 <- reactive({to.weekly(pei_ind_subset_ts_2())})
      pei_ind_dat_ts_daily_2 <- reactive({to.daily(pei_ind_subset_ts_2())})
      pei_ind_dat_ts_hourly_2 <- reactive({to.hourly(pei_ind_subset_ts_2())})
      pei_ind_dat_ts_yearly_2_fossil <- reactive({to.yearly(pei_ind_dat_ts_2_fossil())})
      pei_ind_dat_ts_monthly_2_fossil<- reactive({to.monthly(pei_ind_subset_ts_2_fossil())})
      pei_ind_dat_ts_weekly_2_fossil <- reactive({to.weekly(pei_ind_subset_ts_2_fossil())})
      pei_ind_dat_ts_daily_2_fossil <- reactive({to.daily(pei_ind_subset_ts_2_fossil())})
      pei_ind_dat_ts_hourly_2_fossil <- reactive({to.hourly(pei_ind_subset_ts_2_fossil())})
      
      output$PEI_ON_WIND_YEARLY <- renderUI({
        highchart() %>%
          hc_xAxis(type = "datetime") %>%
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(pei_ind_dat_ts_yearly_2()[,(colnames(pei_ind_dat_ts_yearly_2()) %in% c('pei_ind_dat_ts_2().High'))], type = "line", name = "High Wind Load: ", color = "lightgreen") %>%
          hc_add_series(pei_ind_dat_ts_yearly_2()[,(colnames(pei_ind_dat_ts_yearly_2()) %in% c('pei_ind_dat_ts_2().Low'))], type = "line", name = "Low Wind Load: ", color = "red") %>%
          hc_add_series(pei_ind_dat_ts_yearly_2_fossil()[,(colnames(pei_ind_dat_ts_yearly_2_fossil()) %in% c('pei_ind_dat_ts_2_fossil().High'))], type = "line", name = "High Fossil Load: ", color = "maroon") %>%
          hc_add_series(pei_ind_dat_ts_yearly_2_fossil()[,(colnames(pei_ind_dat_ts_yearly_2_fossil()) %in% c('pei_ind_dat_ts_2_fossil().Low'))], type = "line", name = "Low Fossil Load: ", color = "blue") %>%
          hc_navigator(enabled = TRUE)})
      output$PEI_ON_WIND_WEEKLY <- renderUI({
        highchart() %>%
          hc_xAxis(type = "datetime") %>%
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(pei_ind_dat_ts_weekly_2()[,(colnames(pei_ind_dat_ts_weekly_2()) %in% c('pei_ind_subset_ts_2().High'))], type = "line", name = "High Wind Load: ", color = "lightgreen") %>%
          hc_add_series(pei_ind_dat_ts_weekly_2()[,(colnames(pei_ind_dat_ts_weekly_2()) %in% c('pei_ind_subset_ts_2().Low'))], type = "line", name = "Low Wind Load: ", color = "red") %>%
          hc_add_series(pei_ind_dat_ts_weekly_2_fossil()[,(colnames(pei_ind_dat_ts_weekly_2_fossil()) %in% c('pei_ind_subset_ts_2_fossil().High'))], type = "line", name = "High Fossil Load: ", color = "maroon") %>%
          hc_add_series(pei_ind_dat_ts_weekly_2_fossil()[,(colnames(pei_ind_dat_ts_weekly_2_fossil()) %in% c('pei_ind_subset_ts_2_fossil().Low'))], type = "line", name = "Low Fossil Load: ", color = "blue") %>%
          hc_navigator(enabled = TRUE)})
      output$PEI_ON_WIND_DAILY <- renderUI({
        highchart() %>%
          hc_xAxis(type = "datetime") %>%
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(pei_ind_dat_ts_daily_2()[,(colnames(pei_ind_dat_ts_daily_2()) %in% c('pei_ind_subset_ts_2().High'))], type = "line", name = "High Wind Load: ", color = "lightgreen") %>%
          hc_add_series(pei_ind_dat_ts_daily_2()[,(colnames(pei_ind_dat_ts_daily_2()) %in% c('pei_ind_subset_ts_2().Low'))], type = "line", name = "Low Wind Load: ", color = "red") %>%
          hc_add_series(pei_ind_dat_ts_daily_2_fossil()[,(colnames(pei_ind_dat_ts_daily_2_fossil()) %in% c('pei_ind_subset_ts_2_fossil().High'))], type = "line", name = "High Fossil Load: ", color = "maroon") %>%
          hc_add_series(pei_ind_dat_ts_daily_2_fossil()[,(colnames(pei_ind_dat_ts_daily_2_fossil()) %in% c('pei_ind_subset_ts_2_fossil().Low'))], type = "line", name = "Low Fossil Load: ", color = "blue") %>%
          hc_navigator(enabled = TRUE)})
      output$PEI_ON_WIND_HOURLY <- renderUI({
        highchart() %>%
          hc_xAxis(type = "datetime") %>%
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(pei_ind_dat_ts_hourly_2()[,(colnames(pei_ind_dat_ts_hourly_2()) %in% c('pei_ind_subset_ts_2().High'))], type = "line", name = "High Wind Load: ", color = "lightgreen") %>%
          hc_add_series(pei_ind_dat_ts_hourly_2()[,(colnames(pei_ind_dat_ts_hourly_2()) %in% c('pei_ind_subset_ts_2().Low'))], type = "line", name = "Low Wind Load: ", color = "red") %>%
          hc_add_series(pei_ind_dat_ts_hourly_2_fossil()[,(colnames(pei_ind_dat_ts_hourly_2_fossil()) %in% c('pei_ind_subset_ts_2_fossil().High'))], type = "line", name = "High Fossil Load: ", color = "maroon") %>%
          hc_add_series(pei_ind_dat_ts_hourly_2_fossil()[,(colnames(pei_ind_dat_ts_hourly_2_fossil()) %in% c('pei_ind_subset_ts_2_fossil().Low'))], type = "line", name = "Low Fossil Load: ", color = "blue") %>%
          hc_navigator(enabled = TRUE)})
      output$PEI_ON_WIND_ALL <- renderUI({
        highchart() %>%
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_navigator(enabled = TRUE) %>%
          hc_add_series(pei_ind_subset_ts_2(), type = "line", name = "Wind Load: ", color = "lightgreen")%>%
          hc_add_series(pei_ind_subset_ts_2_fossil(), type = "line", name = "Fossil Load: ", color = "maroon")
      })
      en_tm_dsh_pei_2 <- Sys.time()
      log_info('{difftime(en_tm_dsh_pei_2,st_tm_dsh_pei_2)} sec for PEI Main 2 dashboard')
      
      pei_date_ind_3_1 <- reactive({paste(input$pei_dates_3[1],"00:00:00",sep = " ")})
      pei_date_ind_3_2 <- reactive({paste(input$pei_dates_3[2],"00:00:00",sep = " ")})
      pei_exp_subset_dat_wind_export <- reactive({subset(pei_ind_dat_wind_export,subset = (pei_ind_dat_wind_export$DATETIME_LOCAL >= pei_date_ind_3_1() & pei_ind_dat_wind_export$DATETIME_LOCAL <= pei_date_ind_3_2()))})
      pei_exp_subset_dat_wind_local <- reactive({subset(pei_ind_dat_wind_local,subset = (pei_ind_dat_wind_local$DATETIME_LOCAL >= pei_date_ind_3_1() & pei_ind_dat_wind_local$DATETIME_LOCAL <= pei_date_ind_3_2()))})
      pei_exp_dat_ts <- reactive({xts(pei_ind_dat_wind_export$OBS_VALUE,pei_ind_dat_wind_export$DATETIME_LOCAL)})
      pei_exp_local_dat_ts <- reactive({xts(pei_ind_dat_wind_local$OBS_VALUE,pei_ind_dat_wind_local$DATETIME_LOCAL)})
      pei_exp_subset_ts <- reactive({xts(pei_exp_subset_dat_wind_export()$OBS_VALUE,pei_exp_subset_dat_wind_export()$DATETIME_LOCAL)})
      pei_exp_local_subset_ts <- reactive({xts(pei_exp_subset_dat_wind_local()$OBS_VALUE,pei_exp_subset_dat_wind_local()$DATETIME_LOCAL)})
      pei_exp_dat_ts_yearly <- reactive({to.yearly(pei_exp_dat_ts())})
      pei_exp_dat_ts_monthly <- reactive({to.monthly(pei_exp_subset_ts())})
      pei_exp_dat_ts_weekly <- reactive({to.weekly(pei_exp_subset_ts())})
      pei_exp_dat_ts_daily <- reactive({to.daily(pei_exp_subset_ts())})
      pei_exp_dat_ts_hourly <- reactive({to.hourly(pei_exp_subset_ts())})
      pei_exp_local_dat_ts_yearly <- reactive({to.yearly(pei_exp_local_dat_ts())})
      pei_exp_local_dat_ts_monthly <- reactive({to.monthly(pei_exp_local_subset_ts())})
      pei_exp_local_dat_ts_weekly <- reactive({to.weekly(pei_exp_local_subset_ts())})
      pei_exp_local_dat_ts_daily <- reactive({to.daily(pei_exp_local_subset_ts())})
      pei_exp_local_dat_ts_hourly <- reactive({to.hourly(pei_exp_local_subset_ts())})
      
      output$PEI_EXPORT_YEARLY <- renderUI({
        highchart() %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(pei_exp_dat_ts_yearly()[,(colnames(pei_exp_dat_ts_yearly()) %in% c('pei_exp_dat_ts().High'))], type = "line", name = "High Export Load: ", color = "lightgreen") %>%
          hc_add_series(pei_exp_dat_ts_yearly()[,(colnames(pei_exp_dat_ts_yearly()) %in% c('pei_exp_dat_ts().Low'))], type = "line", name = "Low Export Load: ", color = "red") %>%
          hc_add_series(pei_exp_local_dat_ts_yearly()[,(colnames(pei_exp_local_dat_ts_yearly()) %in% c('pei_exp_local_dat_ts().High'))], type = "line", name = "High Local Load: ", color = "maroon") %>%
          hc_add_series(pei_exp_local_dat_ts_yearly()[,(colnames(pei_exp_local_dat_ts_yearly()) %in% c('pei_exp_local_dat_ts().Low'))], type = "line", name = "Low Local Load: ", color = "blue") %>%
          hc_navigator(enabled = TRUE)})
      output$PEI_EXPORT_WEEKLY <- renderUI({
        highchart() %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(pei_exp_dat_ts_weekly()[,(colnames(pei_exp_dat_ts_weekly()) %in% c('pei_exp_subset_ts().High'))], type = "line", name = "High Export Load: ", color = "lightgreen") %>%
          hc_add_series(pei_exp_dat_ts_weekly()[,(colnames(pei_exp_dat_ts_weekly()) %in% c('pei_exp_subset_ts().Low'))], type = "line", name = "Low Export Load: ", color = "red") %>%
          hc_add_series(pei_exp_local_dat_ts_weekly()[,(colnames(pei_exp_local_dat_ts_weekly()) %in% c('pei_exp_local_subset_ts().High'))], type = "line", name = "High Local Load: ", color = "maroon") %>%
          hc_add_series(pei_exp_local_dat_ts_weekly()[,(colnames(pei_exp_local_dat_ts_weekly()) %in% c('pei_exp_local_subset_ts().Low'))], type = "line", name = "Low Local Load: ", color = "blue") %>%
          hc_navigator(enabled = TRUE)})
      output$PEI_EXPORT_DAILY <- renderUI({
        highchart() %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(pei_exp_dat_ts_daily()[,(colnames(pei_exp_dat_ts_daily()) %in% c('pei_exp_subset_ts().High'))], type = "line", name = "High Export Load: ", color = "lightgreen") %>%
          hc_add_series(pei_exp_dat_ts_daily()[,(colnames(pei_exp_dat_ts_daily()) %in% c('pei_exp_subset_ts().Low'))], type = "line", name = "Low Export Load: ", color = "red") %>%
          hc_add_series(pei_exp_local_dat_ts_daily()[,(colnames(pei_exp_local_dat_ts_daily()) %in% c('pei_exp_local_subset_ts().High'))], type = "line", name = "High Local Load: ", color = "maroon") %>%
          hc_add_series(pei_exp_local_dat_ts_daily()[,(colnames(pei_exp_local_dat_ts_daily()) %in% c('pei_exp_local_subset_ts().Low'))], type = "line", name = "Low Local Load: ", color = "blue") %>%
          hc_navigator(enabled = TRUE)})
      output$PEI_EXPORT_HOURLY <- renderUI({
        highchart() %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(pei_exp_dat_ts_hourly()[,(colnames(pei_exp_dat_ts_hourly()) %in% c('pei_exp_subset_ts().High'))], type = "line", name = "High Export Load: ", color = "lightgreen") %>%
          hc_add_series(pei_exp_dat_ts_hourly()[,(colnames(pei_exp_dat_ts_hourly()) %in% c('pei_exp_subset_ts().Low'))], type = "line", name = "Low Export Load: ", color = "red") %>%
          hc_add_series(pei_exp_local_dat_ts_hourly()[,(colnames(pei_exp_local_dat_ts_hourly()) %in% c('pei_exp_local_subset_ts().High'))], type = "line", name = "High Local Load: ", color = "maroon") %>%
          hc_add_series(pei_exp_local_dat_ts_hourly()[,(colnames(pei_exp_local_dat_ts_hourly()) %in% c('pei_exp_local_subset_ts().Low'))], type = "line", name = "Low Local Load: ", color = "blue") %>%
          hc_navigator(enabled = TRUE)})
      output$PEI_EXPORT_ALL <- renderUI({
        highchart() %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_navigator(enabled = TRUE) %>% 
          hc_add_series(pei_exp_subset_ts(), type = "line", name = "Export Load: ", color = "maroon") %>%
          hc_add_series(pei_exp_local_subset_ts(), type = "line", name = "Local Load: ", color = "lightgreen")
      })
      
      pei_date_ind_4_1 <- reactive({paste(input$pei_dates_4[1],"00:00:00",sep = " ")})
      pei_date_ind_4_2 <- reactive({paste(input$pei_dates_4[2],"00:00:00",sep = " ")})
      pei_loc_subset_dat <- reactive({subset(pei_ind_dat_wind_percent,subset = (pei_ind_dat_wind_percent$DATETIME_LOCAL >= pei_date_ind_4_1() & pei_ind_dat_wind_percent$DATETIME_LOCAL <= pei_date_ind_4_2()))})
      pei_loc_dat_ts <- reactive({xts(pei_ind_dat_wind_percent$OBS_VALUE,pei_ind_dat_wind_percent$DATETIME_LOCAL)})
      pei_loc_subset_ts <- reactive({xts(pei_loc_subset_dat()$OBS_VALUE,pei_loc_subset_dat()$DATETIME_LOCAL)})
      pei_loc_dat_ts_yearly <- reactive({to.yearly(pei_loc_dat_ts())})
      pei_loc_dat_ts_monthly <- reactive({to.monthly(pei_loc_subset_ts())})
      pei_loc_dat_ts_weekly <- reactive({to.weekly(pei_loc_subset_ts())})
      pei_loc_dat_ts_daily <- reactive({to.daily(pei_loc_subset_ts())})
      pei_loc_dat_ts_hourly <- reactive({to.hourly(pei_loc_subset_ts())})
      
      
      output$PEI_LOCAL_YEARLY <- renderUI({
        highchart() %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(pei_loc_dat_ts_yearly()[,(colnames(pei_loc_dat_ts_yearly()) %in% c('pei_loc_dat_ts().High'))], type = "line", name = "High Local Load: ", color = "lightgreen") %>%
          hc_add_series(pei_loc_dat_ts_yearly()[,(colnames(pei_loc_dat_ts_yearly()) %in% c('pei_loc_dat_ts().Low'))], type = "line", name = "Low Local Load: ", color = "red") %>%
          hc_navigator(enabled = TRUE)})
      output$PEI_LOCAL_WEEKLY <- renderUI({
        highchart() %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(pei_loc_dat_ts_weekly()[,(colnames(pei_loc_dat_ts_weekly()) %in% c('pei_loc_subset_ts().High'))], type = "line", name = "High Local Load: ", color = "lightgreen") %>%
          hc_add_series(pei_loc_dat_ts_weekly()[,(colnames(pei_loc_dat_ts_weekly()) %in% c('pei_loc_subset_ts().Low'))], type = "line", name = "Low Local Load: ", color = "red") %>%
          hc_navigator(enabled = TRUE)})
      output$PEI_LOCAL_DAILY <- renderUI({
        highchart() %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(pei_loc_dat_ts_daily()[,(colnames(pei_loc_dat_ts_daily()) %in% c('pei_loc_subset_ts().High'))], type = "line", name = "High Local Load: ", color = "lightgreen") %>%
          hc_add_series(pei_loc_dat_ts_daily()[,(colnames(pei_loc_dat_ts_daily()) %in% c('pei_loc_subset_ts().Low'))], type = "line", name = "Low Local Load: ", color = "red") %>%
          hc_navigator(enabled = TRUE)})
      output$PEI_LOCAL_HOURLY <- renderUI({
        highchart() %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(pei_loc_dat_ts_hourly()[,(colnames(pei_loc_dat_ts_hourly()) %in% c('pei_loc_subset_ts().High'))], type = "line", name = "High Local Load: ", color = "lightgreen") %>%
          hc_add_series(pei_loc_dat_ts_hourly()[,(colnames(pei_loc_dat_ts_hourly()) %in% c('pei_loc_subset_ts().Low'))], type = "line", name = "Low Local Load: ", color = "red") %>%
          hc_navigator(enabled = TRUE)})
      output$PEI_LOCAL_ALL <- renderUI({
        highchart() %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_navigator(enabled = TRUE) %>% 
          hc_add_series(pei_loc_subset_ts(), type = "line", name = "Local Load: ", color = "brown")
      })
    }  
    else if(pei_status() != 200)
    {
      #GPH 1
      output$PEI_LOCAL_YEARLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='pei_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",pei_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$PEI_LOCAL_WEEKLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='pei_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",pei_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$PEI_LOCAL_DAILY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='pei_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",pei_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$PEI_LOCAL_HOURLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='pei_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",pei_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$PEI_LOCAL_ALL <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='pei_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",pei_status_ind(),"Error</i>
                                     </div>
                                          "))})
      #GPH 1 END
      #GPH 2
      output$PEI_ON_LOAD_YEARLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='pei_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",pei_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$PEI_ON_LOAD_WEEKLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='pei_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",pei_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$PEI_ON_LOAD_DAILY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='pei_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",pei_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$PEI_ON_LOAD_HOURLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='pei_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",pei_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$PEI_ON_LOAD_ALL <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='pei_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",pei_status_ind(),"Error</i>
                                     </div>
                                          "))})
      #GPH 2 END
      #GPH 3
      output$PEI_ON_WIND_YEARLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='pei_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",pei_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$PEI_ON_WIND_WEEKLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='pei_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",pei_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$PEI_ON_WIND_DAILY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='pei_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",pei_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$PEI_ON_WIND_HOURLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='pei_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",pei_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$PEI_ON_WIND_ALL <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='pei_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",pei_status_ind(),"Error</i>
                                     </div>
                                          "))})
      #GPH 3 END
      #GPH 4
      output$PEI_EXPORT_YEARLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='pei_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",pei_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$PEI_EXPORT_WEEKLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='pei_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",pei_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$PEI_EXPORT_DAILY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='pei_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",pei_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$PEI_EXPORT_HOURLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='pei_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",pei_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$PEI_EXPORT_ALL <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='pei_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",pei_status_ind(),"Error</i>
                                     </div>
                                          "))})
      #GPH 4 END
      
      log_error("Error Dashboard started - provincial page, Status:{pei_status_ind()}")
    }
  })
    }
  
  #PRINCE EDWARD ISLAND END
  
  else if(input$rted_menu == "NB")
  {
  #NEW BRUNSWICK START
  st_tm_dsh_nb <- Sys.time()
  test_nb_dat_ind <<- dataset("DF_HFED_NB","CA_NB","H","LOAD",NULL,NULL,1) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
  check_nb_stat_api_ind <- function(){status_api("DF_HFED_NB","CA_NB","H","LOAD",NULL,NULL,1)}
  get_nb_stat_api_ind <- function(){status_api("DF_HFED_NB","CA_NB","H","LOAD",NULL,NULL,1)}
  nb_stat_api_ind <- reactivePoll(intervalMillis = 1800000, session = session,
                                  checkFunc = check_nb_stat_api_ind, valueFunc = get_nb_stat_api_ind)
  nb_status_ind <- reactive({nb_stat_api_ind()})
  observeEvent(nb_stat_api_ind(),{
    if(nb_status_ind() == 200){
      if((as.Date(test_nb_dat_ind$DATETIME_LOCAL) == as.Date(Sys.time()))||(as.Date(test_nb_dat_ind$DATETIME_LOCAL) == as.Date(Sys.time())-1))
      {
        nb_ind_dat_load <<- dataset("DF_HFED_NB","CA_NB","H","LOAD",sdmx_date_start_ind_nb,sdmx_date_end_ind_nb,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
        nb_ind_dat_rm_30 <<- dataset("DF_HFED_NB","CA_NB","H","RM_30",sdmx_date_start_ind_nb,sdmx_date_end_ind_nb,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
        nb_ind_dat_rm_10 <<- dataset("DF_HFED_NB","CA_NB","H","RM_10",sdmx_date_start_ind_nb,sdmx_date_end_ind_nb,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
        nb_ind_dat_srm_10 <<- dataset("DF_HFED_NB","CA_NB","H","SRM_10",sdmx_date_start_ind_nb,sdmx_date_end_ind_nb,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
        nb_ind_dat_nsi <<- dataset("DF_HFED_NB","CA_NB","H","NSI",sdmx_date_start_ind_nb,sdmx_date_end_ind_nb,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
        nb_ind_dat_demand <<- dataset("DF_HFED_NB","CA_NB","H","DEMAND",sdmx_date_start_ind_nb,sdmx_date_end_ind_nb,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
        #updating the dates after the data loads (NB)
        nb_dd_1 <- as.Date(head(nb_ind_dat_load$DATETIME_LOCAL,1))-(7)
        nb_dd_1_1 <- paste(nb_dd_1,"00:00:00",sep=" ")
        nb_dd_2_2 <- as.Date(head(nb_ind_dat_load$DATETIME_LOCAL,1))
        updateSliderInput(session, "nb_dates_1",
                          min = as.Date(tail(nb_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          max = as.Date(head(nb_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          value=c(as.Date(nb_dd_1_1),nb_dd_2_2),
                          timeFormat="%Y-%m-%d")
        updateSliderInput(session, "nb_dates_2",
                          min = as.Date(tail(nb_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          max = as.Date(head(nb_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          value=c(as.Date(nb_dd_1_1),nb_dd_2_2),
                          timeFormat="%Y-%m-%d")
        updateSliderInput(session, "nb_dates_3",
                          min = as.Date(tail(nb_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          max = as.Date(head(nb_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          value=c(as.Date(nb_dd_1_1),nb_dd_2_2),
                          timeFormat="%Y-%m-%d")
        updateSliderInput(session, "nb_dates_4",
                          min = as.Date(tail(nb_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          max = as.Date(head(nb_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          value=c(as.Date(nb_dd_1_1),nb_dd_2_2),
                          timeFormat="%Y-%m-%d")
        log_success("Using latest data for NB")
      }
      else{
        nb_ind_dat_load <<- dataset("DF_HFED_NB","CA_NB","H","LOAD",NULL,NULL,250) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
        nb_ind_dat_rm_30 <<- dataset("DF_HFED_NB","CA_NB","H","RM_30",NULL,NULL,250) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
        nb_ind_dat_rm_10 <<- dataset("DF_HFED_NB","CA_NB","H","RM_10",NULL,NULL,250) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
        nb_ind_dat_srm_10 <<- dataset("DF_HFED_NB","CA_NB","H","SRM_10",NULL,NULL,250) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
        nb_ind_dat_nsi <<- dataset("DF_HFED_NB","CA_NB","H","NSI",NULL,NULL,250) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
        nb_ind_dat_demand <<- dataset("DF_HFED_NB","CA_NB","H","DEMAND",NULL,NULL,250) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
        #updating the dates after the data loads (NB)
        nb_dd_1 <- as.Date(head(nb_ind_dat_load$DATETIME_LOCAL,1))-(7)
        nb_dd_1_1 <- paste(nb_dd_1,"00:00:00",sep=" ")
        nb_dd_2_2 <- as.Date(head(nb_ind_dat_load$DATETIME_LOCAL,1))
        updateSliderInput(session, "nb_dates_1",
                          min = as.Date(tail(nb_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          max = as.Date(head(nb_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          value=c(as.Date(nb_dd_1_1),nb_dd_2_2),
                          timeFormat="%Y-%m-%d")
        updateSliderInput(session, "nb_dates_2",
                          min = as.Date(tail(nb_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          max = as.Date(head(nb_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          value=c(as.Date(nb_dd_1_1),nb_dd_2_2),
                          timeFormat="%Y-%m-%d")
        updateSliderInput(session, "nb_dates_3",
                          min = as.Date(tail(nb_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          max = as.Date(head(nb_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          value=c(as.Date(nb_dd_1_1),nb_dd_2_2),
                          timeFormat="%Y-%m-%d")
        updateSliderInput(session, "nb_dates_4",
                          min = as.Date(tail(nb_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          max = as.Date(head(nb_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                          value=c(as.Date(nb_dd_1_1),nb_dd_2_2),
                          timeFormat="%Y-%m-%d")
        log_error("Using NROWS data for NB")
      }
      nb_date_ind_1_1 <- reactive({paste(input$nb_dates_1[1],"00:00:00",sep = " ")})
      nb_date_ind_1_2 <- reactive({paste(input$nb_dates_1[2],"00:00:00",sep = " ")})
      nb_ind_subset_dat <- reactive({subset(nb_ind_dat_load,subset = (nb_ind_dat_load$DATETIME_LOCAL >= nb_date_ind_1_1() & nb_ind_dat_load$DATETIME_LOCAL <= nb_date_ind_1_2()))})
      nb_ind_dat_ts <- reactive({xts(nb_ind_dat_load$OBS_VALUE,nb_ind_dat_load$DATETIME_LOCAL)})
      nb_ind_subset_ts <- reactive({xts(nb_ind_subset_dat()$OBS_VALUE,nb_ind_subset_dat()$DATETIME_LOCAL)})
      nb_ind_dat_ts_yearly <- reactive({to.yearly(nb_ind_dat_ts())})
      nb_ind_dat_ts_monthly <- reactive({to.monthly(nb_ind_subset_ts())})
      nb_ind_dat_ts_weekly <- reactive({to.weekly(nb_ind_subset_ts())})
      nb_ind_dat_ts_daily <- reactive({to.daily(nb_ind_subset_ts())})
      nb_ind_dat_ts_hourly <- reactive({to.hourly(nb_ind_subset_ts())})
      output$NB_LOAD_YEARLY <- renderUI({
        highchart() %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(nb_ind_dat_ts_yearly()[,(colnames(nb_ind_dat_ts_yearly()) %in% c('nb_ind_dat_ts().High'))], type = "line", name = "High Load: ", color = "lightgreen") %>%
          hc_add_series(nb_ind_dat_ts_yearly()[,(colnames(nb_ind_dat_ts_yearly()) %in% c('nb_ind_dat_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
          hc_navigator(enabled = TRUE)})
      output$NB_LOAD_WEEKLY <- renderUI({
        highchart() %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(nb_ind_dat_ts_weekly()[,(colnames(nb_ind_dat_ts_weekly()) %in% c('nb_ind_subset_ts().High'))], type = "line", name = "High Load: ", color = "lightgreen") %>%
          hc_add_series(nb_ind_dat_ts_weekly()[,(colnames(nb_ind_dat_ts_weekly()) %in% c('nb_ind_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
          hc_navigator(enabled = TRUE)})
      output$NB_LOAD_DAILY <- renderUI({
        highchart() %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(nb_ind_dat_ts_daily()[,(colnames(nb_ind_dat_ts_daily()) %in% c('nb_ind_subset_ts().High'))], type = "line", name = "High Load: ", color = "lightgreen") %>%
          hc_add_series(nb_ind_dat_ts_daily()[,(colnames(nb_ind_dat_ts_daily()) %in% c('nb_ind_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
          hc_navigator(enabled = TRUE)})
      output$NB_LOAD_HOURLY <- renderUI({
        highchart() %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(nb_ind_dat_ts_hourly()[,(colnames(nb_ind_dat_ts_hourly()) %in% c('nb_ind_subset_ts().High'))], type = "line", name = "High Load: ", color = "lightgreen") %>%
          hc_add_series(nb_ind_dat_ts_hourly()[,(colnames(nb_ind_dat_ts_hourly()) %in% c('nb_ind_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
          hc_navigator(enabled = TRUE)})
      output$NB_LOAD_ALL <- renderUI({
        highchart() %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
          hc_add_series(nb_ind_subset_ts(), type = "line", name = "High Load: ", color = "lightgreen")
      })
      
      nb_date_ind_2_1 <- reactive({paste(input$nb_dates_2[1],"00:00:00",sep = " ")})
      nb_date_ind_2_2 <- reactive({paste(input$nb_dates_2[2],"00:00:00",sep = " ")})
      nb_dem_subset_dat <- reactive({subset(nb_ind_dat_demand,subset = (nb_ind_dat_demand$DATETIME_LOCAL >= nb_date_ind_2_1() & nb_ind_dat_demand$DATETIME_LOCAL <= nb_date_ind_2_2()))})
      nb_dem_dat_ts <- reactive({xts(nb_ind_dat_demand$OBS_VALUE,nb_ind_dat_demand$DATETIME_LOCAL)})
      nb_dem_subset_ts <- reactive({xts(nb_dem_subset_dat()$OBS_VALUE,nb_dem_subset_dat()$DATETIME_LOCAL)})
      nb_dem_dat_ts_yearly <- reactive({to.yearly(nb_dem_dat_ts())})
      nb_dem_dat_ts_monthly <- reactive({to.monthly(nb_dem_subset_ts())})
      nb_dem_dat_ts_weekly <- reactive({to.weekly(nb_dem_subset_ts())})
      nb_dem_dat_ts_daily <- reactive({to.daily(nb_dem_subset_ts())})
      nb_dem_dat_ts_hourly <- reactive({to.hourly(nb_dem_subset_ts())})
      
      output$NB_DEMAND_YEARLY <- renderUI({
        highchart() %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(nb_dem_dat_ts_yearly()[,(colnames(nb_dem_dat_ts_yearly()) %in% c('nb_dem_dat_ts().High'))], type = "line", name = "High Demand: ", color = "lightgreen") %>%
          hc_add_series(nb_dem_dat_ts_yearly()[,(colnames(nb_dem_dat_ts_yearly()) %in% c('nb_dem_dat_ts().Low'))], type = "line", name = "Low Demand: ", color = "red") %>%
          hc_navigator(enabled = TRUE)})
      output$NB_DEMAND_WEEKLY <- renderUI({
        highchart() %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(nb_dem_dat_ts_weekly()[,(colnames(nb_dem_dat_ts_weekly()) %in% c('nb_dem_subset_ts().High'))], type = "line", name = "High Demand: ", color = "lightgreen") %>%
          hc_add_series(nb_dem_dat_ts_weekly()[,(colnames(nb_dem_dat_ts_weekly()) %in% c('nb_dem_subset_ts().Low'))], type = "line", name = "Low Demand: ", color = "red") %>%
          hc_navigator(enabled = TRUE)})
      output$NB_DEMAND_DAILY <- renderUI({
        highchart() %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(nb_dem_dat_ts_daily()[,(colnames(nb_dem_dat_ts_daily()) %in% c('nb_dem_subset_ts().High'))], type = "line", name = "High Demand: ", color = "lightgreen") %>%
          hc_add_series(nb_dem_dat_ts_daily()[,(colnames(nb_dem_dat_ts_daily()) %in% c('nb_dem_subset_ts().Low'))], type = "line", name = "Low Demand: ", color = "red") %>%
          hc_navigator(enabled = TRUE)})
      output$NB_DEMAND_HOURLY <- renderUI({
        highchart() %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(nb_dem_dat_ts_hourly()[,(colnames(nb_dem_dat_ts_hourly()) %in% c('nb_dem_subset_ts().High'))], type = "line", name = "High Demand: ", color = "lightgreen") %>%
          hc_add_series(nb_dem_dat_ts_hourly()[,(colnames(nb_dem_dat_ts_hourly()) %in% c('nb_dem_subset_ts().Low'))], type = "line", name = "Low Demand: ", color = "red") %>%
          hc_navigator(enabled = TRUE)})
      output$NB_DEMAND_ALL <- renderUI({
        highchart() %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_navigator(enabled = TRUE) %>% 
          hc_add_series(nb_dem_subset_ts(), type = "line", name = "Demand: ", color = "purple")
      })
      
      nb_date_ind_3_1 <- reactive({paste(input$nb_dates_3[1],"00:00:00",sep = " ")})
      nb_date_ind_3_2 <- reactive({paste(input$nb_dates_3[2],"00:00:00",sep = " ")})
      nb_10_subset_dat <- reactive({subset(nb_ind_dat_rm_10,subset = (nb_ind_dat_rm_10$DATETIME_LOCAL >= nb_date_ind_3_1() & nb_ind_dat_rm_10$DATETIME_LOCAL <= nb_date_ind_3_2()))})
      nb_10_dat_ts <- reactive({xts(nb_ind_dat_rm_10$OBS_VALUE,nb_ind_dat_rm_10$DATETIME_LOCAL)})
      nb_10_subset_ts <- reactive({xts(nb_10_subset_dat()$OBS_VALUE,nb_10_subset_dat()$DATETIME_LOCAL)})
      nb_10_dat_ts_yearly <- reactive({to.yearly(nb_10_dat_ts())})
      nb_10_dat_ts_monthly <- reactive({to.monthly(nb_10_subset_ts())})
      nb_10_dat_ts_weekly <- reactive({to.weekly(nb_10_subset_ts())})
      nb_10_dat_ts_daily <- reactive({to.daily(nb_10_subset_ts())})
      nb_10_dat_ts_hourly <- reactive({to.hourly(nb_10_subset_ts())})
      
      output$NB_RESERVE_YEARLY <- renderUI({
        highchart() %>% 
          hc_xAxis(type = "datetime") %>%
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(nb_10_dat_ts_yearly()[,(colnames(nb_10_dat_ts_yearly()) %in% c('nb_10_dat_ts().High'))], type = "line", name = "High Reserve: ", color = "lightgreen") %>%
          hc_add_series(nb_10_dat_ts_yearly()[,(colnames(nb_10_dat_ts_yearly()) %in% c('nb_10_dat_ts().Low'))], type = "line", name = "Low Reserve: ", color = "red") %>%
          hc_navigator(enabled = TRUE)})
      output$NB_RESERVE_WEEKLY <- renderUI({
        highchart() %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(nb_10_dat_ts_weekly()[,(colnames(nb_10_dat_ts_weekly()) %in% c('nb_10_subset_ts().High'))], type = "line", name = "High Reserve: ", color = "lightgreen") %>%
          hc_add_series(nb_10_dat_ts_weekly()[,(colnames(nb_10_dat_ts_weekly()) %in% c('nb_10_subset_ts().Low'))], type = "line", name = "Low Reserve: ", color = "red") %>%
          hc_navigator(enabled = TRUE)})
      output$NB_RESERVE_DAILY <- renderUI({
        highchart() %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(nb_10_dat_ts_daily()[,(colnames(nb_10_dat_ts_daily()) %in% c('nb_10_subset_ts.High'))], type = "line", name = "High Reserve: ", color = "lightgreen") %>%
          hc_add_series(nb_10_dat_ts_daily()[,(colnames(nb_10_dat_ts_daily()) %in% c('nb_10_subset_ts.Low'))], type = "line", name = "Low Reserve: ", color = "red") %>%
          hc_navigator(enabled = TRUE)})
      output$NB_RESERVE_HOURLY <- renderUI({
        highchart() %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(nb_10_dat_ts_hourly()[,(colnames(nb_10_dat_ts_hourly()) %in% c('nb_10_subset_ts.High'))], type = "line", name = "High Reserve: ", color = "lightgreen") %>%
          hc_add_series(nb_10_dat_ts_hourly()[,(colnames(nb_10_dat_ts_hourly()) %in% c('nb_10_subset_ts.Low'))], type = "line", name = "Low Reserve: ", color = "red") %>%
          hc_navigator(enabled = TRUE)})
      output$NB_RESERVE_ALL <- renderUI({
        highchart() %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
          #hc_loading() %>%
          hc_add_series(nb_10_subset_ts(), type = "line", name = "Reserve Load: ", color = "orange")
      })
      
      nb_date_ind_4_1 <- reactive({paste(input$nb_dates_4[1],"00:00:00",sep = " ")})
      nb_date_ind_4_2 <- reactive({paste(input$nb_dates_4[2],"00:00:00",sep = " ")})
      nb_30_subset_dat <- reactive({subset(nb_ind_dat_rm_30,subset = (nb_ind_dat_rm_30$DATETIME_LOCAL >= nb_date_ind_4_1() & nb_ind_dat_rm_30$DATETIME_LOCAL <= nb_date_ind_4_2()))})
      nb_30_dat_ts <- reactive({xts(nb_ind_dat_rm_30$OBS_VALUE,nb_ind_dat_rm_30$DATETIME_LOCAL)})
      nb_30_subset_ts <- reactive({xts(nb_30_subset_dat()$OBS_VALUE,nb_30_subset_dat()$DATETIME_LOCAL)})
      nb_30_dat_ts_yearly <- reactive({to.yearly(nb_30_dat_ts())})
      nb_30_dat_ts_monthly <- reactive({to.monthly(nb_30_subset_ts())})
      nb_30_dat_ts_weekly <- reactive({to.weekly(nb_30_subset_ts())})
      nb_30_dat_ts_daily <- reactive({to.daily(nb_30_subset_ts())})
      nb_30_dat_ts_hourly <- reactive({to.hourly(nb_30_subset_ts())})
      
      output$NB_RESERVE2_YEARLY <- renderUI({
        highchart() %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(nb_30_dat_ts_yearly()[,(colnames(nb_30_dat_ts_yearly()) %in% c('nb_30_dat_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
          hc_add_series(nb_30_dat_ts_yearly()[,(colnames(nb_30_dat_ts_yearly()) %in% c('nb_30_dat_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
          hc_navigator(enabled = TRUE)})
      output$NB_RESERVE2_WEEKLY <- renderUI({
        highchart() %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(nb_30_dat_ts_weekly()[,(colnames(nb_30_dat_ts_weekly()) %in% c('nb_30_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
          hc_add_series(nb_30_dat_ts_weekly()[,(colnames(nb_30_dat_ts_weekly()) %in% c('nb_30_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
          hc_navigator(enabled = TRUE)})
      output$NB_RESERVE2_DAILY <- renderUI({
        highchart() %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(nb_30_dat_ts_daily()[,(colnames(nb_30_dat_ts_daily()) %in% c('nb_30_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
          hc_add_series(nb_30_dat_ts_daily()[,(colnames(nb_30_dat_ts_daily()) %in% c('nb_30_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
          hc_navigator(enabled = TRUE)})
      output$NB_RESERVE2_HOURLY <- renderUI({
        highchart() %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_add_series(nb_30_dat_ts_hourly()[,(colnames(nb_30_dat_ts_hourly()) %in% c('nb_30_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
          hc_add_series(nb_30_dat_ts_hourly()[,(colnames(nb_30_dat_ts_hourly()) %in% c('nb_30_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
          hc_navigator(enabled = TRUE)})
      output$NB_RESERVE2_ALL <- renderUI({
        highchart() %>% 
          hc_xAxis(type = "datetime") %>% 
          hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
          hc_navigator(enabled = TRUE) %>% 
          hc_add_series(nb_30_subset_ts(), type = "line", name = "High Load: ", color = "lightgreen")
      })
    }  
    else if(nb_status() != 200)
    {
      #GPH 1
      output$NB_LOAD_YEARLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='nb_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",nb_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$NB_LOAD_WEEKLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='nb_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",nb_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$NB_LOAD_DAILY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='nb_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",nb_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$NB_LOAD_HOURLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='nb_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",nb_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$NB_LOAD_ALL <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='nb_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",nb_status_ind(),"Error</i>
                                     </div>
                                          "))})
      #GPH 1 END
      #GPH 2
      output$NB_DEMAND_YEARLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='nb_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",nb_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$NB_DEMAND_WEEKLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='nb_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",nb_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$NB_DEMAND_DAILY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='nb_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",nb_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$NB_DEMAND_HOURLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='nb_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",nb_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$NB_DEMAND_ALL <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='nb_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",nb_status_ind(),"Error</i>
                                     </div>
                                          "))})
      #GPH 2 END
      #GPH 3
      output$NB_RESERVE_YEARLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='nb_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",nb_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$NB_RESERVE_WEEKLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='nb_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",nb_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$NB_RESERVE_DAILY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='nb_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",nb_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$NB_RESERVE_HOURLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='nb_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",nb_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$NB_RESERVE_ALL <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='nb_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",nb_status_ind(),"Error</i>
                                     </div>
                                          "))})
      #GPH 3 END
      #GPH 4
      output$NB_RESERVE2_YEARLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='nb_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",nb_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$NB_RESERVE2_WEEKLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='nb_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",nb_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$NB_RESERVE2_DAILY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='nb_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",nb_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$NB_RESERVE2_HOURLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='nb_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",nb_status_ind(),"Error</i>
                                     </div>
                                          "))})
      output$NB_RESERVE2_ALL <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='nb_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",nb_status_ind(),"Error</i>
                                     </div>
                                          "))})
      #GPH 4 END
      
      log_error("Error Dashboard started - provincial page, Status:{nb_status_ind()}")
    }
  })
  }
  #NEW BRUNSWICK END
  
  else if(input$rted_menu == "NFL")
  {
    #NEWFOUNDLAND & LABRADOR START
    st_tm_dsh_nfl <- Sys.time()
    test_nfl_dat_ind <<- dataset("DF_HFED_NL","CA_NL","H","DEMAND",NULL,NULL,1) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
    check_nfl_stat_api_ind <- function(){status_api("DF_HFED_NL","CA_NL","H","DEMAND",NULL,NULL,1)}
    get_nfl_stat_api_ind <- function(){status_api("DF_HFED_NL","CA_NL","H","DEMAND",NULL,NULL,1)}
    nfl_stat_api_ind <- reactivePoll(intervalMillis = 1800000, session = session,
                                     checkFunc = check_nfl_stat_api_ind, valueFunc = get_nfl_stat_api_ind)
    nfl_status_ind <- reactive({nfl_stat_api_ind()})
    observeEvent(nfl_stat_api_ind(),{
      if(nfl_status_ind() == 200){
        if((as.Date(test_nfl_dat_ind$DATETIME_LOCAL) == as.Date(Sys.time()))||(as.Date(test_nfl_dat_ind$DATETIME_LOCAL) == as.Date(Sys.time())-1))
        {
          nfl_ind_dat_load <<- dataset("DF_HFED_NL","CA_NL","H","DEMAND",sdmx_date_start_ind_nfl,sdmx_date_end_ind_nfl,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
          #updating the dates after the data loads (NB)
          nfl_dd_1 <- as.Date(head(nfl_ind_dat_load$DATETIME_LOCAL,1))-(7)
          nfl_dd_1_1 <- paste(nfl_dd_1,"00:00:00",sep=" ")
          nfl_dd_2_2 <- as.Date(head(nfl_ind_dat_load$DATETIME_LOCAL,1))
          updateSliderInput(session, "nfl_dates_1",
                            min = as.Date(tail(nfl_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                            max = as.Date(head(nfl_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                            value=c(as.Date(nfl_dd_1_1),nfl_dd_2_2),
                            timeFormat="%Y-%m-%d")
          log_success("Using latest data for NFL")
        }
        else{
          nfl_ind_dat_load <<- dataset("DF_HFED_NL","CA_NL","H","DEMAND",NULL,NULL,250) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
          #updating the dates after the data loads (NB)
          nfl_dd_1 <- as.Date(head(nfl_ind_dat_load$DATETIME_LOCAL,1))-(7)
          nfl_dd_1_1 <- paste(nfl_dd_1,"00:00:00",sep=" ")
          nfl_dd_2_2 <- as.Date(head(nfl_ind_dat_load$DATETIME_LOCAL,1))
          updateSliderInput(session, "nfl_dates_1",
                            min = as.Date(tail(nfl_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                            max = as.Date(head(nfl_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                            value=c(as.Date(nfl_dd_1_1),nfl_dd_2_2),
                            timeFormat="%Y-%m-%d")
          log_error("Using NROWS data for NFL")
        }
        nfl_date_ind_1_1 <- reactive({paste(input$nfl_dates_1[1],"00:00:00",sep = " ")})
        nfl_date_ind_1_2 <- reactive({paste(input$nfl_dates_1[2],"00:00:00",sep = " ")})
        nfl_load_subset_dat <- reactive({subset(nfl_ind_dat_load,subset = (nfl_ind_dat_load$DATETIME_LOCAL >= nfl_date_ind_1_1() & nfl_ind_dat_load$DATETIME_LOCAL <= nfl_date_ind_1_2()))})
        nfl_load_dat_ts <- reactive({xts(nfl_ind_dat_load$OBS_VALUE,nfl_ind_dat_load$DATETIME_LOCAL)})
        nfl_load_subset_ts <- reactive({xts(nfl_load_subset_dat()$OBS_VALUE,nfl_load_subset_dat()$DATETIME_LOCAL)})
        nfl_load_dat_ts_yearly <- reactive({to.yearly(nfl_load_dat_ts())})
        nfl_load_dat_ts_monthly <- reactive({to.monthly(nfl_load_subset_ts())})
        nfl_load_dat_ts_weekly <- reactive({to.weekly(nfl_load_subset_ts())})
        nfl_load_dat_ts_daily <- reactive({to.daily(nfl_load_subset_ts())})
        nfl_load_dat_ts_hourly <- reactive({to.hourly(nfl_load_subset_ts())})
        
        output$NFL_LOAD_YEARLY <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>% 
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_add_series(nfl_load_dat_ts_yearly()[,(colnames(nfl_load_dat_ts_yearly()) %in% c('nfl_load_dat_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
            hc_add_series(nfl_load_dat_ts_yearly()[,(colnames(nfl_load_dat_ts_yearly()) %in% c('nfl_load_dat_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
            hc_navigator(enabled = TRUE)})
        output$NFL_LOAD_WEEKLY <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>% 
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_add_series(nfl_load_dat_ts_weekly()[,(colnames(nfl_load_dat_ts_weekly()) %in% c('nfl_load_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
            hc_add_series(nfl_load_dat_ts_weekly()[,(colnames(nfl_load_dat_ts_weekly()) %in% c('nfl_load_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
            hc_navigator(enabled = TRUE)})
        output$NFL_LOAD_DAILY <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>% 
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_add_series(nfl_load_dat_ts_daily()[,(colnames(nfl_load_dat_ts_daily()) %in% c('nfl_load_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
            hc_add_series(nfl_load_dat_ts_daily()[,(colnames(nfl_load_dat_ts_daily()) %in% c('nfl_load_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
            hc_navigator(enabled = TRUE)})
        output$NFL_LOAD_HOURLY <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>% 
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_add_series(nfl_load_dat_ts_hourly()[,(colnames(nfl_load_dat_ts_hourly()) %in% c('nfl_load_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
            hc_add_series(nfl_load_dat_ts_hourly()[,(colnames(nfl_load_dat_ts_hourly()) %in% c('nfl_load_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
            hc_navigator(enabled = TRUE)})
        output$NFL_LOAD_ALL <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>% 
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_navigator(enabled = TRUE) %>% 
            hc_add_series(nfl_load_subset_ts(), type = "line", name = "High Load: ", color = "green")
        })
      }
        else if(nfl_status() != 200)
        {
          #GPH 1
          output$NFL_LOAD_YEARLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='nb_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",nb_status_ind(),"Error</i>
                                     </div>
                                          "))})
        }
      #NEWFOUNDLAND & LABRADOR END
    })}
    
  else if(input$rted_menu == "BC")
  {
    #BC Start
    st_tm_dsh_bc <- Sys.time()
    test_bc_dat_ind <<- dataset("DF_HFED_BC","CA_BC","H","LOAD",NULL,NULL,1) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
    check_bc_stat_api_ind <- function(){status_api("DF_HFED_BC","CA_BC","H","LOAD",NULL,NULL,1)}
    get_bc_stat_api_ind <- function(){status_api("DF_HFED_BC","CA_BC","H","LOAD",NULL,NULL,1)}
    bc_stat_api_ind <- reactivePoll(intervalMillis = 1800000, session = session,
                                    checkFunc = check_bc_stat_api_ind, valueFunc = get_bc_stat_api_ind)
    bc_status_ind <- reactive({bc_stat_api_ind()})
    observeEvent(bc_stat_api_ind(),{
      if(bc_status_ind() == 200){
        if((as.Date(test_bc_dat_ind$DATETIME_LOCAL) == as.Date(Sys.time()))||(as.Date(test_bc_dat_ind$DATETIME_LOCAL) == as.Date(Sys.time())-1))
        {
          bc_ind_dat_load <<- dataset("DF_HFED_BC","CA_BC","H","LOAD",sdmx_date_start_ind_bc,sdmx_date_end_ind_bc,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
          #updating the dates after the data loads (NB)
          bc_dd_1 <- as.Date(head(bc_ind_dat_load$DATETIME_LOCAL,1))-(7)
          bc_dd_1_1 <- paste(bc_dd_1,"00:00:00",sep=" ")
          bc_dd_2_2 <- as.Date(head(bc_ind_dat_load$DATETIME_LOCAL,1))
          updateSliderInput(session, "bc_dates_1",
                            min = as.Date(tail(bc_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                            max = as.Date(head(bc_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                            value=c(as.Date(bc_dd_1_1),bc_dd_2_2),
                            timeFormat="%Y-%m-%d")
          log_success("Using latest data for BC")
        }
        else{
          bc_ind_dat_load <<- dataset("DF_HFED_BC","CA_BC","H","LOAD",NULL,NULL,500) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
          #updating the dates after the data loads (NB)
          bc_dd_1 <- as.Date(head(bc_ind_dat_load$DATETIME_LOCAL,1))-(7)
          bc_dd_1_1 <- paste(bc_dd_1,"00:00:00",sep=" ")
          bc_dd_2_2 <- as.Date(head(bc_ind_dat_load$DATETIME_LOCAL,1))
          updateSliderInput(session, "bc_dates_1",
                            min = as.Date(tail(bc_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                            max = as.Date(head(bc_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                            value=c(as.Date(bc_dd_1_1),bc_dd_2_2),
                            timeFormat="%Y-%m-%d")
          log_error("Using NROWS data for BC")
        }
        bc_date_ind_1_1 <- reactive({paste(input$bc_dates_1[1],"00:00:00",sep = " ")})
        bc_date_ind_1_2 <- reactive({paste(input$bc_dates_1[2],"00:00:00",sep = " ")})
        bc_load_subset_dat <- reactive({subset(bc_ind_dat_load,subset = (bc_ind_dat_load$DATETIME_LOCAL >= bc_date_ind_1_1() & bc_ind_dat_load$DATETIME_LOCAL <= bc_date_ind_1_2()))})
        bc_load_dat_ts <- reactive({xts(bc_ind_dat_load$OBS_VALUE,bc_ind_dat_load$DATETIME_LOCAL)})
        bc_load_subset_ts <- reactive({xts(bc_load_subset_dat()$OBS_VALUE,bc_load_subset_dat()$DATETIME_LOCAL)})
        bc_load_dat_ts_yearly <- reactive({to.yearly(bc_load_dat_ts())})
        bc_load_dat_ts_monthly <- reactive({to.monthly(bc_load_subset_ts())})
        bc_load_dat_ts_weekly <- reactive({to.weekly(bc_load_subset_ts())})
        bc_load_dat_ts_daily <- reactive({to.daily(bc_load_subset_ts())})
        bc_load_dat_ts_hourly <- reactive({to.hourly(bc_load_subset_ts())})
        
        output$BC_LOAD_YEARLY <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>% 
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_add_series(bc_load_dat_ts_yearly()[,(colnames(bc_load_dat_ts_yearly()) %in% c('bc_load_dat_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
            hc_add_series(bc_load_dat_ts_yearly()[,(colnames(bc_load_dat_ts_yearly()) %in% c('bc_load_dat_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
            hc_navigator(enabled = TRUE)})
        output$BC_LOAD_WEEKLY <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>% 
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_add_series(bc_load_dat_ts_weekly()[,(colnames(bc_load_dat_ts_weekly()) %in% c('bc_load_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
            hc_add_series(bc_load_dat_ts_weekly()[,(colnames(bc_load_dat_ts_weekly()) %in% c('bc_load_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
            hc_navigator(enabled = TRUE)})
        output$BC_LOAD_DAILY <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>% 
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_add_series(bc_load_dat_ts_daily()[,(colnames(bc_load_dat_ts_daily()) %in% c('bc_load_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
            hc_add_series(bc_load_dat_ts_daily()[,(colnames(bc_load_dat_ts_daily()) %in% c('bc_load_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
            hc_navigator(enabled = TRUE)})
        output$BC_LOAD_HOURLY <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>% 
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_add_series(bc_load_dat_ts_hourly()[,(colnames(bc_load_dat_ts_hourly()) %in% c('bc_load_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
            hc_add_series(bc_load_dat_ts_hourly()[,(colnames(bc_load_dat_ts_hourly()) %in% c('bc_load_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
            hc_navigator(enabled = TRUE)})
        output$BC_LOAD_ALL <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>% 
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_navigator(enabled = TRUE) %>% 
            hc_add_series(bc_load_subset_ts(), type = "line", name = "High Load: ", color = "green")
        })
      }
      else if(bc_status() != 200)
      {
        #GPH 1
        output$BC_LOAD_YEARLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='nb_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",bc_status_ind(),"Error</i>
                                     </div>
                                          "))})
      }
      #BC END
    })}

  else if(input$rted_menu == "NS")
   {
  # Nova Scotia START
    st_tm_dsh_ns <- Sys.time()
    test_ns_dat_ind <<- dataset("DF_HFED_NS","CA_NS","H","LOAD",NULL,NULL,1) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
    check_ns_stat_api_ind <- function(){status_api("DF_HFED_NS","CA_NS","H","LOAD",NULL,NULL,1)}
    get_ns_stat_api_ind <- function(){status_api("DF_HFED_NS","CA_NS","H","LOAD",NULL,NULL,1)}
    ns_stat_api_ind <- reactivePoll(intervalMillis = 1800000, session = session,
                                    checkFunc = check_ns_stat_api_ind, valueFunc = get_ns_stat_api_ind)
    ns_status_ind <- reactive({ns_stat_api_ind()})
    observeEvent(ns_stat_api_ind(),{
      if(ns_status_ind() == 200){
        if((as.Date(test_ns_dat_ind$DATETIME_LOCAL) == as.Date(Sys.time()))||(as.Date(test_ns_dat_ind$DATETIME_LOCAL) == as.Date(Sys.time())-1))
        {
          ns_ind_dat_load <<- dataset("DF_HFED_NS","CA_NS","H","LOAD",sdmx_date_start_ind_ns,sdmx_date_end_ind_ns,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
          ns_ind_dat_export <<- dataset("DF_HFED_NS","CA_NS","H","EXPORT",sdmx_date_start_ind_ns,sdmx_date_end_ind_ns,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
          ns_ind_dat_import <<- dataset("DF_HFED_NS","CA_NS","H","IMPORT",sdmx_date_start_ind_ns,sdmx_date_end_ind_ns,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
          ns_ind_dat_nsi <<- dataset("DF_HFED_NS","CA_NS","H","NSI",sdmx_date_start_ind_ns,sdmx_date_end_ind_ns,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
          #updating the dates after the data loads (NS)
          ns_dd_1 <- as.Date(head(ns_ind_dat_load$DATETIME_LOCAL,1))-(7)
          ns_dd_1_1 <- paste(ns_dd_1,"00:00:00",sep=" ")
          ns_dd_2_2 <- as.Date(head(ns_ind_dat_load$DATETIME_LOCAL,1))
          updateSliderInput(session, "ns_dates_1",
                            min = as.Date(tail(ns_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                            max = as.Date(head(ns_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                            value=c(as.Date(ns_dd_1_1),ns_dd_2_2),
                            timeFormat="%Y-%m-%d")
          updateSliderInput(session, "ns_dates_2",
                            min = as.Date(tail(ns_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                            max = as.Date(head(ns_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                            value=c(as.Date(ns_dd_1_1),ns_dd_2_2),
                            timeFormat="%Y-%m-%d")
          updateSliderInput(session, "ns_dates_3",
                            min = as.Date(tail(ns_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                            max = as.Date(head(ns_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                            value=c(as.Date(ns_dd_1_1),ns_dd_2_2),
                            timeFormat="%Y-%m-%d")
          updateSliderInput(session, "ns_dates_4",
                            min = as.Date(tail(ns_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                            max = as.Date(head(ns_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                            value=c(as.Date(ns_dd_1_1),ns_dd_2_2),
                            timeFormat="%Y-%m-%d")
          log_success("Using latest data for NS")
        }
        else{
          ns_ind_dat_load <<- dataset("DF_HFED_NS","CA_NS","H","LOAD",NULL,NULL,250) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
          ns_ind_dat_export <<- dataset("DF_HFED_NS","CA_NS","H","EXPORT",NULL,NULL,250) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
          ns_ind_dat_import <<- dataset("DF_HFED_NS","CA_NS","H","IMPORT",NULL,NULL,250) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
          ns_ind_dat_nsi <<- dataset("DF_HFED_NS","CA_NS","H","NSI",NULL,NULL,250) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
          #updating the dates after the data loads (NS)
          ns_dd_1 <- as.Date(head(ns_ind_dat_load$DATETIME_LOCAL,1))-(7)
          ns_dd_1_1 <- paste(ns_dd_1,"00:00:00",sep=" ")
          ns_dd_2_2 <- as.Date(head(ns_ind_dat_load$DATETIME_LOCAL,1))
          updateSliderInput(session, "ns_dates_1",
                            min = as.Date(tail(ns_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                            max = as.Date(head(ns_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                            value=c(as.Date(ns_dd_1_1),ns_dd_2_2),
                            timeFormat="%Y-%m-%d")
          updateSliderInput(session, "ns_dates_2",
                            min = as.Date(tail(ns_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                            max = as.Date(head(ns_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                            value=c(as.Date(ns_dd_1_1),ns_dd_2_2),
                            timeFormat="%Y-%m-%d")
          updateSliderInput(session, "ns_dates_3",
                            min = as.Date(tail(ns_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                            max = as.Date(head(ns_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                            value=c(as.Date(ns_dd_1_1),ns_dd_2_2),
                            timeFormat="%Y-%m-%d")
          updateSliderInput(session, "ns_dates_4",
                            min = as.Date(tail(ns_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                            max = as.Date(head(ns_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                            value=c(as.Date(ns_dd_1_1),ns_dd_2_2),
                            timeFormat="%Y-%m-%d")
          log_error("Using NROWS data for NS")
        }
        ns_date_ind_1_1 <- reactive({paste(input$ns_dates_1[1],"00:00:00",sep = " ")})
        ns_date_ind_1_2 <- reactive({paste(input$ns_dates_1[2],"00:00:00",sep = " ")})
        ns_ind_subset_dat <- reactive({subset(ns_ind_dat_load,subset = (ns_ind_dat_load$DATETIME_LOCAL >= ns_date_ind_1_1() & ns_ind_dat_load$DATETIME_LOCAL <= ns_date_ind_1_2()))})
        ns_ind_dat_ts <- reactive({xts(ns_ind_dat_load$OBS_VALUE,ns_ind_dat_load$DATETIME_LOCAL)})
        ns_ind_subset_ts <- reactive({xts(ns_ind_subset_dat()$OBS_VALUE,ns_ind_subset_dat()$DATETIME_LOCAL)})
        ns_ind_dat_ts_yearly <- reactive({to.yearly(ns_ind_dat_ts())})
        ns_ind_dat_ts_monthly <- reactive({to.monthly(ns_ind_subset_ts())})
        ns_ind_dat_ts_weekly <- reactive({to.weekly(ns_ind_subset_ts())})
        ns_ind_dat_ts_daily <- reactive({to.daily(ns_ind_subset_ts())})
        ns_ind_dat_ts_hourly <- reactive({to.hourly(ns_ind_subset_ts())})
        output$NS_LOAD_YEARLY <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>% 
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_add_series(ns_ind_dat_ts_yearly()[,(colnames(ns_ind_dat_ts_yearly()) %in% c('ns_ind_dat_ts().High'))], type = "line", name = "High Load: ", color = "lightgreen") %>%
            hc_add_series(ns_ind_dat_ts_yearly()[,(colnames(ns_ind_dat_ts_yearly()) %in% c('ns_ind_dat_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
            hc_navigator(enabled = TRUE)})
        output$NS_LOAD_WEEKLY <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>% 
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_add_series(ns_ind_dat_ts_weekly()[,(colnames(ns_ind_dat_ts_weekly()) %in% c('ns_ind_subset_ts().High'))], type = "line", name = "High Load: ", color = "lightgreen") %>%
            hc_add_series(ns_ind_dat_ts_weekly()[,(colnames(ns_ind_dat_ts_weekly()) %in% c('ns_ind_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
            hc_navigator(enabled = TRUE)})
        output$NS_LOAD_DAILY <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>% 
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_add_series(ns_ind_dat_ts_daily()[,(colnames(ns_ind_dat_ts_daily()) %in% c('ns_ind_subset_ts().High'))], type = "line", name = "High Load: ", color = "lightgreen") %>%
            hc_add_series(ns_ind_dat_ts_daily()[,(colnames(ns_ind_dat_ts_daily()) %in% c('ns_ind_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
            hc_navigator(enabled = TRUE)})
        output$NS_LOAD_HOURLY <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>% 
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_add_series(ns_ind_dat_ts_hourly()[,(colnames(ns_ind_dat_ts_hourly()) %in% c('ns_ind_subset_ts().High'))], type = "line", name = "High Load: ", color = "lightgreen") %>%
            hc_add_series(ns_ind_dat_ts_hourly()[,(colnames(ns_ind_dat_ts_hourly()) %in% c('ns_ind_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
            hc_navigator(enabled = TRUE)})
        output$NS_LOAD_ALL <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>% 
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_navigator(enabled = TRUE) %>% 
            hc_add_series(ns_ind_subset_ts(), type = "line", name = "High Load: ", color = "lightgreen")
        })
        
        ns_date_ind_2_1 <- reactive({paste(input$ns_dates_2[1],"00:00:00",sep = " ")})
        ns_date_ind_2_2 <- reactive({paste(input$ns_dates_2[2],"00:00:00",sep = " ")})
        ns_dem_subset_dat <- reactive({subset(ns_ind_dat_import,subset = (ns_ind_dat_import$DATETIME_LOCAL >= ns_date_ind_2_1() & ns_ind_dat_import$DATETIME_LOCAL <= ns_date_ind_2_2()))})
        ns_dem_dat_ts <- reactive({xts(ns_ind_dat_import$OBS_VALUE,ns_ind_dat_import$DATETIME_LOCAL)})
        ns_dem_subset_ts <- reactive({xts(ns_dem_subset_dat()$OBS_VALUE,ns_dem_subset_dat()$DATETIME_LOCAL)})
        ns_dem_dat_ts_yearly <- reactive({to.yearly(ns_dem_dat_ts())})
        ns_dem_dat_ts_monthly <- reactive({to.monthly(ns_dem_subset_ts())})
        ns_dem_dat_ts_weekly <- reactive({to.weekly(ns_dem_subset_ts())})
        ns_dem_dat_ts_daily <- reactive({to.daily(ns_dem_subset_ts())})
        ns_dem_dat_ts_hourly <- reactive({to.hourly(ns_dem_subset_ts())})
        
        output$NS_IMPORT_YEARLY <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>%
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_add_series(ns_dem_dat_ts_yearly()[,(colnames(ns_dem_dat_ts_yearly()) %in% c('ns_dem_dat_ts().High'))], type = "line", name = "High Demand: ", color = "lightgreen") %>%
            hc_add_series(ns_dem_dat_ts_yearly()[,(colnames(ns_dem_dat_ts_yearly()) %in% c('ns_dem_dat_ts().Low'))], type = "line", name = "Low Demand: ", color = "red") %>%
            hc_navigator(enabled = TRUE)})
        output$NS_IMPORT_WEEKLY <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>% 
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_add_series(ns_dem_dat_ts_weekly()[,(colnames(ns_dem_dat_ts_weekly()) %in% c('ns_dem_subset_ts().High'))], type = "line", name = "High Demand: ", color = "lightgreen") %>%
            hc_add_series(ns_dem_dat_ts_weekly()[,(colnames(ns_dem_dat_ts_weekly()) %in% c('ns_dem_subset_ts().Low'))], type = "line", name = "Low Demand: ", color = "red") %>%
            hc_navigator(enabled = TRUE)})
        output$NS_IMPORT_DAILY <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>% 
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_add_series(ns_dem_dat_ts_daily()[,(colnames(ns_dem_dat_ts_daily()) %in% c('ns_dem_subset_ts().High'))], type = "line", name = "High Demand: ", color = "lightgreen") %>%
            hc_add_series(ns_dem_dat_ts_daily()[,(colnames(ns_dem_dat_ts_daily()) %in% c('ns_dem_subset_ts().Low'))], type = "line", name = "Low Demand: ", color = "red") %>%
            hc_navigator(enabled = TRUE)})
        output$NS_IMPORT_HOURLY <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>% 
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_add_series(ns_dem_dat_ts_hourly()[,(colnames(ns_dem_dat_ts_hourly()) %in% c('ns_dem_subset_ts().High'))], type = "line", name = "High Demand: ", color = "lightgreen") %>%
            hc_add_series(ns_dem_dat_ts_hourly()[,(colnames(ns_dem_dat_ts_hourly()) %in% c('ns_dem_subset_ts().Low'))], type = "line", name = "Low Demand: ", color = "red") %>%
            hc_navigator(enabled = TRUE)})
        output$NS_IMPORT_ALL <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>% 
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_navigator(enabled = TRUE) %>% 
            hc_add_series(ns_dem_subset_ts(), type = "line", name = "Demand: ", color = "purple")
        })
        
        ns_date_ind_3_1 <- reactive({paste(input$ns_dates_3[1],"00:00:00",sep = " ")})
        ns_date_ind_3_2 <- reactive({paste(input$ns_dates_3[2],"00:00:00",sep = " ")})
        ns_10_subset_dat <- reactive({subset(ns_ind_dat_export,subset = (ns_ind_dat_export$DATETIME_LOCAL >= ns_date_ind_3_1() & ns_ind_dat_export$DATETIME_LOCAL <= ns_date_ind_3_2()))})
        ns_10_dat_ts <- reactive({xts(ns_ind_dat_export$OBS_VALUE,ns_ind_dat_export$DATETIME_LOCAL)})
        ns_10_subset_ts <- reactive({xts(ns_10_subset_dat()$OBS_VALUE,ns_10_subset_dat()$DATETIME_LOCAL)})
        ns_10_dat_ts_yearly <- reactive({to.yearly(ns_10_dat_ts())})
        ns_10_dat_ts_monthly <- reactive({to.monthly(ns_10_subset_ts())})
        ns_10_dat_ts_weekly <- reactive({to.weekly(ns_10_subset_ts())})
        ns_10_dat_ts_daily <- reactive({to.daily(ns_10_subset_ts())})
        ns_10_dat_ts_hourly <- reactive({to.hourly(ns_10_subset_ts())})
        
        output$NS_EXPORT_YEARLY <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>% 
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_add_series(ns_10_dat_ts_yearly()[,(colnames(ns_10_dat_ts_yearly()) %in% c('ns_10_dat_ts().High'))], type = "line", name = "High Reserve: ", color = "lightgreen") %>%
            hc_add_series(ns_10_dat_ts_yearly()[,(colnames(ns_10_dat_ts_yearly()) %in% c('ns_10_dat_ts().Low'))], type = "line", name = "Low Reserve: ", color = "red") %>%
            hc_navigator(enabled = TRUE)})
        output$NS_EXPORT_WEEKLY <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>% 
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_add_series(ns_10_dat_ts_weekly()[,(colnames(ns_10_dat_ts_weekly()) %in% c('ns_10_subset_ts().High'))], type = "line", name = "High Reserve: ", color = "lightgreen") %>%
            hc_add_series(ns_10_dat_ts_weekly()[,(colnames(ns_10_dat_ts_weekly()) %in% c('ns_10_subset_ts().Low'))], type = "line", name = "Low Reserve: ", color = "red") %>%
            hc_navigator(enabled = TRUE)})
        output$NS_EXPORT_DAILY <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>% 
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_add_series(ns_10_dat_ts_daily()[,(colnames(ns_10_dat_ts_daily()) %in% c('ns_10_subset_ts.High'))], type = "line", name = "High Reserve: ", color = "lightgreen") %>%
            hc_add_series(ns_10_dat_ts_daily()[,(colnames(ns_10_dat_ts_daily()) %in% c('ns_10_subset_ts.Low'))], type = "line", name = "Low Reserve: ", color = "red") %>%
            hc_navigator(enabled = TRUE)})
        output$NS_EXPORT_HOURLY <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>% 
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_add_series(ns_10_dat_ts_hourly()[,(colnames(ns_10_dat_ts_hourly()) %in% c('ns_10_subset_ts.High'))], type = "line", name = "High Reserve: ", color = "lightgreen") %>%
            hc_add_series(ns_10_dat_ts_hourly()[,(colnames(ns_10_dat_ts_hourly()) %in% c('ns_10_subset_ts.Low'))], type = "line", name = "Low Reserve: ", color = "red") %>%
            hc_navigator(enabled = TRUE)})
        output$NS_EXPORT_ALL <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>% 
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_navigator(enabled = TRUE) %>% 
            #hc_loading() %>%
            hc_add_series(ns_10_subset_ts(), type = "line", name = "Reserve Load: ", color = "orange")
        })
        
        ns_date_ind_4_1 <- reactive({paste(input$ns_dates_4[1],"00:00:00",sep = " ")})
        ns_date_ind_4_2 <- reactive({paste(input$ns_dates_4[2],"00:00:00",sep = " ")})
        ns_30_subset_dat <- reactive({subset(ns_ind_dat_nsi,subset = (ns_ind_dat_nsi$DATETIME_LOCAL >= ns_date_ind_4_1() & ns_ind_dat_nsi$DATETIME_LOCAL <= ns_date_ind_4_2()))})
        ns_30_dat_ts <- reactive({xts(ns_ind_dat_nsi$OBS_VALUE,ns_ind_dat_nsi$DATETIME_LOCAL)})
        ns_30_subset_ts <- reactive({xts(ns_30_subset_dat()$OBS_VALUE,ns_30_subset_dat()$DATETIME_LOCAL)})
        ns_30_dat_ts_yearly <- reactive({to.yearly(ns_30_dat_ts())})
        ns_30_dat_ts_monthly <- reactive({to.monthly(ns_30_subset_ts())})
        ns_30_dat_ts_weekly <- reactive({to.weekly(ns_30_subset_ts())})
        ns_30_dat_ts_daily <- reactive({to.daily(ns_30_subset_ts())})
        ns_30_dat_ts_hourly <- reactive({to.hourly(ns_30_subset_ts())})
        
        output$NS_NSI_YEARLY <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>% 
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_add_series(ns_30_dat_ts_yearly()[,(colnames(ns_30_dat_ts_yearly()) %in% c('ns_30_dat_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
            hc_add_series(ns_30_dat_ts_yearly()[,(colnames(ns_30_dat_ts_yearly()) %in% c('ns_30_dat_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
            hc_navigator(enabled = TRUE)})
        output$NS_NSI_WEEKLY <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>% 
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_add_series(ns_30_dat_ts_weekly()[,(colnames(ns_30_dat_ts_weekly()) %in% c('ns_30_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
            hc_add_series(ns_30_dat_ts_weekly()[,(colnames(ns_30_dat_ts_weekly()) %in% c('ns_30_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
            hc_navigator(enabled = TRUE)})
        output$NS_NSI_DAILY <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>% 
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_add_series(ns_30_dat_ts_daily()[,(colnames(ns_30_dat_ts_daily()) %in% c('ns_30_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
            hc_add_series(ns_30_dat_ts_daily()[,(colnames(ns_30_dat_ts_daily()) %in% c('ns_30_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
            hc_navigator(enabled = TRUE)})
        output$NS_NSI_HOURLY <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>% 
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_add_series(ns_30_dat_ts_hourly()[,(colnames(ns_30_dat_ts_hourly()) %in% c('ns_30_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
            hc_add_series(ns_30_dat_ts_hourly()[,(colnames(ns_30_dat_ts_hourly()) %in% c('ns_30_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
            hc_navigator(enabled = TRUE)})
        output$NS_NSI_ALL <- renderUI({
          highchart() %>% 
            hc_xAxis(type = "datetime") %>% 
            hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
            hc_navigator(enabled = TRUE) %>% 
            hc_add_series(ns_30_subset_ts(), type = "line", name = "High Load: ", color = "lightgreen")
        })
      }  
      else if(ns_status_ind() != 200)
      {
        #GPH 1
        output$NS_LOAD_YEARLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='ns_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",ns_status_ind(),"Error</i>
                                     </div>
                                          "))})
        output$NS_LOAD_WEEKLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='ns_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",ns_status_ind(),"Error</i>
                                     </div>
                                          "))})
        output$NS_LOAD_DAILY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='ns_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",ns_status_ind(),"Error</i>
                                     </div>
                                          "))})
        output$NS_LOAD_HOURLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='ns_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",ns_status_ind(),"Error</i>
                                     </div>
                                          "))})
        output$NS_LOAD_ALL <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='ns_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",ns_status_ind(),"Error</i>
                                     </div>
                                          "))})
        #GPH 1 END
        #GPH 2
        output$NS_IMPORT_YEARLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='ns_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",ns_status_ind(),"Error</i>
                                     </div>
                                          "))})
        output$NS_IMPORT_WEEKLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='ns_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",ns_status_ind(),"Error</i>
                                     </div>
                                          "))})
        output$NS_IMPORT_DAILY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='ns_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",ns_status_ind(),"Error</i>
                                     </div>
                                          "))})
        output$NS_IMPORT_HOURLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='ns_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",ns_status_ind(),"Error</i>
                                     </div>
                                          "))})
        output$NS_IMPORT_ALL <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='ns_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",ns_status_ind(),"Error</i>
                                     </div>
                                          "))})
        #GPH 2 END
        #GPH 3
        output$NS_EXPORT_YEARLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='ns_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",ns_status_ind(),"Error</i>
                                     </div>
                                          "))})
        output$NS_EXPORT_WEEKLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='ns_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",ns_status_ind(),"Error</i>
                                     </div>
                                          "))})
        output$NS_EXPORT_DAILY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='ns_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",ns_status_ind(),"Error</i>
                                     </div>
                                          "))})
        output$NS_EXPORT_HOURLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='ns_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",ns_status_ind(),"Error</i>
                                     </div>
                                          "))})
        output$NS_EXPORT_ALL <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='ns_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",ns_status_ind(),"Error</i>
                                     </div>
                                          "))})
        #GPH 3 END
        #GPH 4
        output$NS_NSI_YEARLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='ns_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",ns_status_ind(),"Error</i>
                                     </div>
                                          "))})
        output$NS_NSI_WEEKLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='ns_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",ns_status_ind(),"Error</i>
                                     </div>
                                          "))})
        output$NS_NSI_DAILY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='ns_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",ns_status_ind(),"Error</i>
                                     </div>
                                          "))})
        output$NS_NSI_HOURLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='ns_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",ns_status_ind(),"Error</i>
                                     </div>
                                          "))})
        output$NS_NSI_ALL <- renderUI({HTML(paste("<div style='height:400px;'>
                                     <br>
                                     <i id='ns_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",ns_status_ind(),"Error</i>
                                     </div>
                                          "))})
        #GPH 4 END
        
        log_error("Error Dashboard started - provincial page, Status:{ns_status_ind()}")
      }
    })}
  
     else if(input$rted_menu == "ON")
     {
       #Ontario START
       st_tm_dsh_on <- Sys.time()
       test_on_dat_ind <<- dataset("DF_HFED_ON","CA_ON","H","ONTARIO_DEMAND",NULL,NULL,1) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
       check_on_stat_api_ind <- function(){status_api("DF_HFED_ON","CA_ON","H","ONTARIO_DEMAND",NULL,NULL,1)}
       get_on_stat_api_ind <- function(){status_api("DF_HFED_ON","CA_ON","H","ONTARIO_DEMAND",NULL,NULL,1)}
       on_stat_api_ind <- reactivePoll(intervalMillis = 1800000, session = session,
                                       checkFunc = check_on_stat_api_ind, valueFunc = get_on_stat_api_ind)
       on_status_ind <- reactive({on_stat_api_ind()})
       observeEvent(on_stat_api_ind(),{
         if(on_status_ind() == 200){
           if((as.Date(test_on_dat_ind$DATETIME_LOCAL) == as.Date(Sys.time()))||(as.Date(test_on_dat_ind$DATETIME_LOCAL) == as.Date(Sys.time())-1))
           {
             on_ind_dat_demand <<- dataset("DF_HFED_ON","CA_ON","H","ONTARIO_DEMAND",sdmx_date_start_ind_on,sdmx_date_end_ind_on,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
             on_ind_dat_market <<- dataset("DF_HFED_ON","CA_ON","H","MARKET_ONTARIO",sdmx_date_start_ind_on,sdmx_date_end_ind_on,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
             #updating the dates after the data loads (NB)
             on_dd_1 <- as.Date(head(on_ind_dat_load$DATETIME_LOCAL,1))-(7)
             on_dd_1_1 <- paste(on_dd_1,"00:00:00",sep=" ")
             on_dd_2_2 <- as.Date(head(on_ind_dat_load$DATETIME_LOCAL,1))
             updateSliderInput(session, "on_dates_1",
                               min = as.Date(tail(on_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                               max = as.Date(head(on_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                               value=c(as.Date(on_dd_1_1),on_dd_2_2),
                               timeFormat="%Y-%m-%d")
             updateSliderInput(session, "on_dates_2",
                               min = as.Date(tail(on_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                               max = as.Date(head(on_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                               value=c(as.Date(on_dd_1_1),on_dd_2_2),
                               timeFormat="%Y-%m-%d")
             log_success("Using latest data for ON")
           }
           else{
             on_ind_dat_load <<- dataset("DF_HFED_ON","CA_ON","H","ONTARIO_DEMAND",NULL,NULL,250) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
             on_ind_dat_market <<- dataset("DF_HFED_ON","CA_ON","H","MARKET_ONTARIO",NULL,NULL,250) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()
             #updating the dates after the data loads (NB)
             on_dd_1 <- as.Date(head(on_ind_dat_load$DATETIME_LOCAL,1))-(7)
             on_dd_1_1 <- paste(on_dd_1,"00:00:00",sep=" ")
             on_dd_2_2 <- as.Date(head(on_ind_dat_load$DATETIME_LOCAL,1))
             updateSliderInput(session, "on_dates_1",
                               min = as.Date(tail(on_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                               max = as.Date(head(on_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                               value=c(as.Date(on_dd_1_1),on_dd_2_2),
                               timeFormat="%Y-%m-%d")
             updateSliderInput(session, "on_dates_2",
                               min = as.Date(tail(on_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                               max = as.Date(head(on_ind_dat_load$DATETIME_LOCAL,1),"%Y-%m-%d"),
                               value=c(as.Date(on_dd_1_1),on_dd_2_2),
                               timeFormat="%Y-%m-%d")
             log_error("Using NROWS data for ON")
           }
  
           on_date_ind_1_1 <- reactive({paste(input$on_dates_1[1],"00:00:00",sep = " ")})
           on_date_ind_1_2 <- reactive({paste(input$on_dates_1[2],"00:00:00",sep = " ")})
           on_load_subset_dat <- reactive({subset(on_ind_dat_load,subset = (on_ind_dat_load$DATETIME_LOCAL >= on_date_ind_1_1() & on_ind_dat_load$DATETIME_LOCAL <= on_date_ind_1_2()))})
           on_load_dat_ts <- reactive({xts(on_ind_dat_load$OBS_VALUE,on_ind_dat_load$DATETIME_LOCAL)})
           on_load_subset_ts <- reactive({xts(on_load_subset_dat()$OBS_VALUE,on_load_subset_dat()$DATETIME_LOCAL)})
           on_load_dat_ts_yearly <- reactive({to.yearly(on_load_dat_ts())})
           on_load_dat_ts_monthly <- reactive({to.monthly(on_load_subset_ts())})
           on_load_dat_ts_weekly <- reactive({to.weekly(on_load_subset_ts())})
           on_load_dat_ts_daily <- reactive({to.daily(on_load_subset_ts())})
           on_load_dat_ts_hourly <- reactive({to.hourly(on_load_subset_ts())})
  
           output$ON_LOAD_YEARLY <- renderUI({
             highchart() %>%
               hc_xAxis(type = "datetime") %>% 
               hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
               hc_add_series(on_load_dat_ts_yearly()[,(colnames(on_load_dat_ts_yearly()) %in% c('on_load_dat_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
               hc_add_series(on_load_dat_ts_yearly()[,(colnames(on_load_dat_ts_yearly()) %in% c('on_load_dat_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
               hc_navigator(enabled = TRUE)})
           output$ON_LOAD_WEEKLY <- renderUI({
             highchart() %>%
               hc_xAxis(type = "datetime") %>% 
               hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
               hc_add_series(on_load_dat_ts_weekly()[,(colnames(on_load_dat_ts_weekly()) %in% c('on_load_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
               hc_add_series(on_load_dat_ts_weekly()[,(colnames(on_load_dat_ts_weekly()) %in% c('on_load_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
               hc_navigator(enabled = TRUE)})
           output$ON_LOAD_DAILY <- renderUI({
             highchart() %>%
               hc_xAxis(type = "datetime") %>% 
               hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
               hc_add_series(on_load_dat_ts_daily()[,(colnames(on_load_dat_ts_daily()) %in% c('on_load_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
               hc_add_series(on_load_dat_ts_daily()[,(colnames(on_load_dat_ts_daily()) %in% c('on_load_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
               hc_navigator(enabled = TRUE)})
           output$ON_LOAD_HOURLY <- renderUI({
             highchart() %>%
               hc_xAxis(type = "datetime") %>% 
               hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
               hc_add_series(on_load_dat_ts_hourly()[,(colnames(on_load_dat_ts_hourly()) %in% c('on_load_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
               hc_add_series(on_load_dat_ts_hourly()[,(colnames(on_load_dat_ts_hourly()) %in% c('on_load_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
               hc_navigator(enabled = TRUE)})
           output$ON_LOAD_ALL <- renderUI({
             highchart() %>%
               hc_xAxis(type = "datetime") %>% 
               hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
               hc_navigator(enabled = TRUE) %>%
               hc_add_series(on_load_subset_ts(), type = "line", name = "High Load: ", color = "green")
           })

           on_date_ind_2_1 <- reactive({paste(input$on_dates_1[1],"00:00:00",sep = " ")})
           on_date_ind_2_2 <- reactive({paste(input$on_dates_1[2],"00:00:00",sep = " ")})
           on_mrktdmd_subset_dat <- reactive({subset(on_ind_dat_market,subset = (on_ind_dat_market$DATETIME_LOCAL >= on_date_ind_1_1() & on_ind_dat_market$DATETIME_LOCAL <= on_date_ind_1_2()))})
           on_mrktdmd_dat_ts <- reactive({xts(on_ind_dat_market$OBS_VALUE,on_ind_dat_market$DATETIME_LOCAL)})
           on_mrktdmd_subset_ts <- reactive({xts(on_mrktdmd_subset_dat()$OBS_VALUE,on_mrktdmd_subset_dat()$DATETIME_LOCAL)})
           on_mrktdmd_dat_ts_yearly <- reactive({to.yearly(on_mrktdmd_dat_ts())})
           on_mrktdmd_dat_ts_monthly <- reactive({to.monthly(on_mrktdmd_subset_ts())})
           on_mrktdmd_dat_ts_weekly <- reactive({to.weekly(on_mrktdmd_subset_ts())})
           on_mrktdmd_dat_ts_daily <- reactive({to.daily(on_mrktdmd_subset_ts())})
           on_mrktdmd_dat_ts_hourly <- reactive({to.hourly(on_mrktdmd_subset_ts())})
  
           output$ON_MRKTDMND_YEARLY <- renderUI({
             highchart() %>%
               hc_xAxis(type = "datetime") %>%
               hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
               hc_add_series(on_mrktdmd_dat_ts_yearly()[,(colnames(on_mrktdmd_dat_ts_yearly()) %in% c('on_mrktdmd_dat_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
               hc_add_series(on_mrktdmd_dat_ts_yearly()[,(colnames(on_mrktdmd_dat_ts_yearly()) %in% c('on_mrktdmd_dat_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
               hc_navigator(enabled = TRUE)})
           output$ON_MRKTDMND_WEEKLY <- renderUI({
             highchart() %>%
               hc_xAxis(type = "datetime") %>%
               hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
               hc_add_series(on_mrktdmd_dat_ts_weekly()[,(colnames(on_mrktdmd_dat_ts_weekly()) %in% c('on_mrktdmd_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
               hc_add_series(on_mrktdmd_dat_ts_weekly()[,(colnames(on_mrktdmd_dat_ts_weekly()) %in% c('on_mrktdmd_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
               hc_navigator(enabled = TRUE)})
           output$ON_MRKTDMND_DAILY <- renderUI({
             highchart() %>%
               hc_xAxis(type = "datetime") %>%
               hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
               hc_add_series(on_mrktdmd_dat_ts_daily()[,(colnames(on_mrktdmd_dat_ts_daily()) %in% c('on_mrktdmd_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
               hc_add_series(on_mrktdmd_dat_ts_daily()[,(colnames(on_mrktdmd_dat_ts_daily()) %in% c('on_mrktdmd_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
               hc_navigator(enabled = TRUE)})
           output$ON_MRKTDMND_HOURLY <- renderUI({
             highchart() %>%
               hc_xAxis(type = "datetime") %>%
               hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
               hc_add_series(on_mrktdmd_dat_ts_hourly()[,(colnames(on_mrktdmd_dat_ts_hourly()) %in% c('on_mrktdmd_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
               hc_add_series(on_mrktdmd_dat_ts_hourly()[,(colnames(on_mrktdmd_dat_ts_hourly()) %in% c('on_mrktdmd_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
               hc_navigator(enabled = TRUE)})
           output$ON_MRKTDMND_ALL <- renderUI({
             highchart() %>%
               hc_xAxis(type = "datetime") %>% 
               hc_yAxis(title = list(text = "MW"),labels = list(style = list(color = "black", 'font-style' = "bold", 'font-size' = "small"))) %>%
               hc_navigator(enabled = TRUE) %>%
               hc_add_series(on_mrktdmd_subset_ts(), type = "line", name = "High Load: ", color = "green")
           })
         }
         else if(on_status() != 200)
         {
           #GPH 1
           output$ON_LOAD_YEARLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                      <br>
                                      <i id='nb_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",nb_status_ind(),"Error</i>
                                      </div>
                                           "))})
           output$ON_MRKTDMND_YEARLY <- renderUI({HTML(paste("<div style='height:400px;'>
                                      <br>
                                      <i id='nb_er_txt' class='fa fa-exclamation'> Error, Dashboard Not Available. Reason:",nb_status_ind(),"Error</i>
                                      </div>
                                           "))})
         }
       })}
    
    if(input$rted_menu == "Dwn"){
      log_info("------------------------------------------DOWNLOAD SECTION------------------------------------------------------")
      
      observeEvent(input$prvnc_list,{
      if(input$prvnc_list == "Newfoundland & Labrador")
       {
      updateSelectInput(session,"enf_flow", choices = list("DEMAND"),selected = "DEMAND")
        dataflow <- reactive({"DF_HFED_NL"})
        prvnc_code <- reactive({"CA_NL"})
        var_choice <- reactive({input$enf_flow})
      NFL_dwnld_data <- download_api(dataflow(),prvnc_code(),"H",var_choice(),NULL,NULL,1,1)
      updateDateRangeInput(session, "download_dates",
                                    start = NFL_dwnld_data[2,'DATETIME_LOCAL'],
                                    min = NFL_dwnld_data[2,'DATETIME_LOCAL'],
                                    end = NFL_dwnld_data[1,'DATETIME_LOCAL'],
                                    max = NFL_dwnld_data[1,'DATETIME_LOCAL'])
      nfl_dwnld_date_1 <- reactive({as.character(input$download_dates[1])})
      nfl_dwnld_date_2 <- reactive({as.character(input$download_dates[2])})
      nfl_user_data <- reactive({dataset("DF_HFED_NL","CA_NL","H",input$enf_flow,nfl_dwnld_date_1(),nfl_dwnld_date_2(),NULL)})
      output$DOWNLOAD_TABLE <- DT::renderDataTable({(nfl_user_data()[c((1:3),(5:8),11,13)])})
      output$button_dwnld <- downloadHandler(
                filename = function() {
                  flname <- paste("HFED_",input$prvnc_list,input$enf_flow,"data")
                  paste(flname,"csv",sep = ".")
                },
                content = function(file)
                {
                  showNotification("Your File is Downloading, Please wait for some time.", type="message")
                 write.csv(nfl_user_data(), file, row.names = FALSE)
                }
               )
      
      }
      else if(input$prvnc_list == "Prince Edward Island")
      {
        
        observe({
        updateSelectInput(session,"enf_flow", choices = list("ON_ISL_LOAD","ON_ISL_FOSSIL","WIND_EXPORT","WIND_PERCENT","ON_ISL_WIND","WIND_LOCAL","IMPORT_CABLES"),selected = "ON_ISL_LOAD")
          dataflow <- reactive({"DF_HFED_PE"})
          prvnc_code <- reactive({"CA_PE"})
          var_choice <- reactive({input$enf_flow})
          pei_dwnld_data <- download_api(dataflow(),prvnc_code(),"H",var_choice(),NULL,NULL,1,1)
        updateDateRangeInput(session, "download_dates",
                             start = pei_dwnld_data[2,'DATETIME_LOCAL'],
                             min = pei_dwnld_data[2,'DATETIME_LOCAL'],
                             end = pei_dwnld_data[1,'DATETIME_LOCAL'],
                             max = pei_dwnld_data[1,'DATETIME_LOCAL'])
        pei_dwnld_date_1 <- reactive({as.character(input$download_dates[1])})
        pei_dwnld_date_2 <- reactive({as.character(input$download_dates[2])})
        pei_user_data <- reactive({dataset("DF_HFED_PE","CA_PE","H",input$enf_flow,pei_dwnld_date_1(),pei_dwnld_date_2(),NULL)})
        output$DOWNLOAD_TABLE <- DT::renderDataTable({(pei_user_data()[c((1:3),(5:8),11,13)])})
        output$button_dwnld <- downloadHandler(
          filename = function() {
            flname <- paste("HFED_",input$prvnc_list,input$enf_flow,"data")
            paste(flname,"csv",sep = ".")
          },
          content = function(file)
          {
            showNotification("Your File is Downloading, Please wait for some time.", type="message")
            write.csv(pei_user_data(), file, row.names = FALSE)
          }
        )
        })
      }
       else if(input$prvnc_list == "Nova Scotia")
        {
          
          observe({
          updateSelectInput(session,"enf_flow", choices = list("LOAD","EXPORT","IMPORT","NSI"),selected = "LOAD")
            dataflow <- reactive({"DF_HFED_NS"})
            prvnc_code <- reactive({"CA_NS"})
            var_choice <- reactive({input$enf_flow})
            ns_dwnld_data <- download_api(dataflow(),prvnc_code(),"H",var_choice(),NULL,NULL,1,1)
          updateDateRangeInput(session, "download_dates",
                               start = ns_dwnld_data[1,'DATETIME_LOCAL'],
                               min = ns_dwnld_data[1,'DATETIME_LOCAL'],
                               end = ns_dwnld_data[2,'DATETIME_LOCAL'],
                               max = ns_dwnld_data[2,'DATETIME_LOCAL'])
          ns_dwnld_date_1 <- reactive({as.character(input$download_dates[1])})
          ns_dwnld_date_2 <- reactive({as.character(input$download_dates[2])})
          ns_user_data <- reactive({dataset("DF_HFED_NS","CA_NS","H",input$enf_flow,ns_dwnld_date_1(),ns_dwnld_date_2(),NULL)})
          output$DOWNLOAD_TABLE <- DT::renderDataTable({(ns_user_data()[c((1:3),(5:8),11,13)])})
          output$button_dwnld <- downloadHandler(
            filename = function() {
              flname <- paste("HFED_",input$prvnc_list,input$enf_flow,"data")
              paste(flname,"csv",sep = ".")
            },
            content = function(file)
            {
              showNotification("Your File is Downloading, Please wait for some time.", type="message")
              write.csv(ns_user_data(), file, row.names = FALSE)
            }
          )
          })
        }
      })
      
      #download section ends
    }
  })
  
#   qb_date_ind_1_1 <- reactive({paste(input$qb_dates_1[1],"00:00:00",sep = " ")})
#   qb_date_ind_1_2 <- reactive({paste(input$qb_dates_1[2],"00:00:00",sep = " ")})
#   
#   qb_data_energy_subset_dat <- reactive({subset(qb_ind_dat_1,subset = (qb_ind_dat_1$Date_time_local >= qb_date_ind_1_1() & qb_ind_dat_1$Date_time_local <= qb_date_ind_1_2()))})
#   
#   qb_data_energy_dat_ts <- reactive({xts(qb_ind_dat_1$total_production,qb_ind_dat_1$Date_time_local)})
#   qb_data_energy_subset_ts <- reactive({xts(qb_data_energy_subset_dat()$total_production,qb_data_energy_subset_dat()$Date_time_local)})
#   
#   qb_data_energy_dat_ts_yearly <- reactive({to.yearly(qb_data_energy_dat_ts())})
#   qb_data_energy_dat_ts_monthly <- reactive({to.monthly(qb_data_energy_subset_ts())})
#   qb_data_energy_dat_ts_weekly <- reactive({to.weekly(qb_data_energy_subset_ts())})
#   qb_data_energy_dat_ts_daily <- reactive({to.daily(qb_data_energy_subset_ts())})
#   qb_data_energy_dat_ts_hourly <- reactive({to.hourly(qb_data_energy_subset_ts())})
#   
#   output$QB_TOT_PRODUCTION_YEARLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_yearly()[,(colnames(qb_data_energy_dat_ts_yearly()) %in% c('qb_data_energy_dat_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly()[,(colnames(qb_data_energy_dat_ts_yearly()) %in% c('qb_data_energy_dat_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_TOT_PRODUCTION_WEEKLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_weekly()[,(colnames(qb_data_energy_dat_ts_weekly()) %in% c('qb_data_energy_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly()[,(colnames(qb_data_energy_dat_ts_weekly()) %in% c('qb_data_energy_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_TOT_PRODUCTION_DAILY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_daily()[,(colnames(qb_data_energy_dat_ts_daily()) %in% c('qb_data_energy_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily()[,(colnames(qb_data_energy_dat_ts_daily()) %in% c('qb_data_energy_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_TOT_PRODUCTION_HOURLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_hourly()[,(colnames(qb_data_energy_dat_ts_hourly()) %in% c('qb_data_energy_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly()[,(colnames(qb_data_energy_dat_ts_hourly()) %in% c('qb_data_energy_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_TOT_PRODUCTION_ALL <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
#       hc_add_series(qb_data_energy_subset_ts(), type = "line", name = "High Load: ", color = "green")
#   })
#   
#   
#   qb_date_ind_2_1 <- reactive({paste(input$qb_dates_2[1],"00:00:00",sep = " ")})
#   qb_date_ind_2_2 <- reactive({paste(input$qb_dates_2[2],"00:00:00",sep = " ")})
#   
#   qb_data_energy_subset_dat_1 <- reactive({subset(qb_ind_dat_1,subset = (qb_ind_dat_1$Date_time_local >= qb_date_ind_2_1() & qb_ind_dat_1$Date_time_local <= qb_date_ind_2_2()))})
#   qb_data_energy_subset_dat_2 <- reactive({subset(qb_ind_dat_1,subset = (qb_ind_dat_1$Date_time_local >= qb_date_ind_2_1() & qb_ind_dat_1$Date_time_local <= qb_date_ind_2_2()))})
#   
#   qb_data_energy_dat_ts_1 <- reactive({xts(qb_ind_dat_1$hydraulic,qb_ind_dat_1$Date_time_local)})
#   qb_data_energy_subset_ts_1 <- reactive({xts(qb_data_energy_subset_dat_1()$hydraulic,qb_data_energy_subset_dat_1()$Date_time_local)})
#   
#   qb_data_energy_dat_ts_2 <- reactive({xts(qb_ind_dat_1$other,qb_ind_dat_1$Date_time_local)})
#   qb_data_energy_subset_ts_2 <- reactive({xts(qb_data_energy_subset_dat_2()$other,qb_data_energy_subset_dat_2()$Date_time_local)})
#   
#   qb_data_energy_dat_ts_yearly_1 <- reactive({to.yearly(qb_data_energy_dat_ts_1())})
#   qb_data_energy_dat_ts_monthly_1 <- reactive({to.monthly(qb_data_energy_subset_ts_1())})
#   qb_data_energy_dat_ts_weekly_1 <- reactive({to.weekly(qb_data_energy_subset_ts_1())})
#   qb_data_energy_dat_ts_daily_1 <- reactive({to.daily(qb_data_energy_subset_ts_1())})
#   qb_data_energy_dat_ts_hourly_1 <- reactive({to.hourly(qb_data_energy_subset_ts_1())})
#   
#   qb_data_energy_dat_ts_yearly_2 <- reactive({to.yearly(qb_data_energy_dat_ts_2())})
#   qb_data_energy_dat_ts_monthly_2 <- reactive({to.monthly(qb_data_energy_subset_ts_2())})
#   qb_data_energy_dat_ts_weekly_2 <- reactive({to.weekly(qb_data_energy_subset_ts_2())})
#   qb_data_energy_dat_ts_daily_2 <- reactive({to.daily(qb_data_energy_subset_ts_2())})
#   qb_data_energy_dat_ts_hourly_2 <- reactive({to.hourly(qb_data_energy_subset_ts_2())})
#   
#   output$QB_FUEL_1_YEARLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_yearly_1()[,(colnames(qb_data_energy_dat_ts_yearly_1()) %in% c('qb_data_energy_dat_ts_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly_1()[,(colnames(qb_data_energy_dat_ts_yearly_1()) %in% c('qb_data_energy_dat_ts_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly_2()[,(colnames(qb_data_energy_dat_ts_yearly_2()) %in% c('qb_data_energy_dat_ts_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly_2()[,(colnames(qb_data_energy_dat_ts_yearly_2()) %in% c('qb_data_energy_dat_ts_2().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_FUEL_1_WEEKLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_weekly_1()[,(colnames(qb_data_energy_dat_ts_weekly_1()) %in% c('qb_data_energy_subset_ts_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly_1()[,(colnames(qb_data_energy_dat_ts_weekly_1()) %in% c('qb_data_energy_subset_ts_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly_2()[,(colnames(qb_data_energy_dat_ts_weekly_2()) %in% c('qb_data_energy_subset_ts_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly_2()[,(colnames(qb_data_energy_dat_ts_weekly_2()) %in% c('qb_data_energy_subset_ts_2().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_FUEL_1_DAILY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_daily_1()[,(colnames(qb_data_energy_dat_ts_daily_1()) %in% c('qb_data_energy_subset_ts_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily_1()[,(colnames(qb_data_energy_dat_ts_daily_1()) %in% c('qb_data_energy_subset_ts_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily_2()[,(colnames(qb_data_energy_dat_ts_daily_2()) %in% c('qb_data_energy_subset_ts_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily_2()[,(colnames(qb_data_energy_dat_ts_daily_2()) %in% c('qb_data_energy_subset_ts_2().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_FUEL_1_HOURLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_hourly_1()[,(colnames(qb_data_energy_dat_ts_hourly_1()) %in% c('qb_data_energy_subset_ts_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly_1()[,(colnames(qb_data_energy_dat_ts_hourly_1()) %in% c('qb_data_energy_subset_ts_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly_2()[,(colnames(qb_data_energy_dat_ts_hourly_2()) %in% c('qb_data_energy_subset_ts_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly_2()[,(colnames(qb_data_energy_dat_ts_hourly_2()) %in% c('qb_data_energy_subset_ts_2().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_FUEL_1_ALL <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
#       hc_add_series(qb_data_energy_subset_ts_1(), type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_subset_ts_2(), type = "line", name = "High Load: ", color = "green")
#   })
#   
#   qb_date_ind_3_1 <- reactive({paste(input$qb_dates_3[1],"00:00:00",sep = " ")})
#   qb_date_ind_3_2 <- reactive({paste(input$qb_dates_3[2],"00:00:00",sep = " ")})
#   
#   qb_data_energy_subset_dat_3 <- reactive({subset(qb_ind_dat_1,subset = (qb_ind_dat_1$Date_time_local >= qb_date_ind_3_1() & qb_ind_dat_1$Date_time_local <= qb_date_ind_3_2()))})
#   qb_data_energy_subset_dat_4 <- reactive({subset(qb_ind_dat_1,subset = (qb_ind_dat_1$Date_time_local >= qb_date_ind_3_1() & qb_ind_dat_1$Date_time_local <= qb_date_ind_3_2()))})
#   
#   qb_data_energy_dat_ts_3 <- reactive({xts(qb_ind_dat_1$wind,qb_ind_dat_1$Date_time_local)})
#   qb_data_energy_subset_ts_3 <- reactive({xts(qb_data_energy_subset_dat_3()$wind,qb_data_energy_subset_dat_3()$Date_time_local)})
#   
#   qb_data_energy_dat_ts_4 <- reactive({xts(qb_ind_dat_1$solar,qb_ind_dat_1$Date_time_local)})
#   qb_data_energy_subset_ts_4 <- reactive({xts(qb_data_energy_subset_dat_4()$solar,qb_data_energy_subset_dat_4()$Date_time_local)})
#   
#   qb_data_energy_dat_ts_yearly_3 <- reactive({to.yearly(qb_data_energy_dat_ts_3())})
#   qb_data_energy_dat_ts_monthly_3 <- reactive({to.monthly(qb_data_energy_subset_ts_3())})
#   qb_data_energy_dat_ts_weekly_3 <- reactive({to.weekly(qb_data_energy_subset_ts_3())})
#   qb_data_energy_dat_ts_daily_3 <- reactive({to.daily(qb_data_energy_subset_ts_3())})
#   qb_data_energy_dat_ts_hourly_3 <- reactive({to.hourly(qb_data_energy_subset_ts_3())})
#   
#   qb_data_energy_dat_ts_yearly_4 <- reactive({to.yearly(qb_data_energy_dat_ts_4())})
#   qb_data_energy_dat_ts_monthly_4 <- reactive({to.monthly(qb_data_energy_subset_ts_4())})
#   qb_data_energy_dat_ts_weekly_4 <- reactive({to.weekly(qb_data_energy_subset_ts_4())})
#   qb_data_energy_dat_ts_daily_4 <- reactive({to.daily(qb_data_energy_subset_ts_4())})
#   qb_data_energy_dat_ts_hourly_4 <- reactive({to.hourly(qb_data_energy_subset_ts_4())})
#   
#   output$QB_FUEL_2_YEARLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_yearly_3()[,(colnames(qb_data_energy_dat_ts_yearly_3()) %in% c('qb_data_energy_dat_ts_3().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly_3()[,(colnames(qb_data_energy_dat_ts_yearly_3()) %in% c('qb_data_energy_dat_ts_3().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly_4()[,(colnames(qb_data_energy_dat_ts_yearly_4()) %in% c('qb_data_energy_dat_ts_4().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly_4()[,(colnames(qb_data_energy_dat_ts_yearly_4()) %in% c('qb_data_energy_dat_ts_4().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_FUEL_2_WEEKLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_weekly_3()[,(colnames(qb_data_energy_dat_ts_weekly_3()) %in% c('qb_data_energy_subset_ts_3().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly_3()[,(colnames(qb_data_energy_dat_ts_weekly_3()) %in% c('qb_data_energy_subset_ts_3().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly_4()[,(colnames(qb_data_energy_dat_ts_weekly_4()) %in% c('qb_data_energy_subset_ts_4().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly_4()[,(colnames(qb_data_energy_dat_ts_weekly_4()) %in% c('qb_data_energy_subset_ts_4().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_FUEL_2_DAILY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_daily_3()[,(colnames(qb_data_energy_dat_ts_daily_3()) %in% c('qb_data_energy_subset_ts_3().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily_3()[,(colnames(qb_data_energy_dat_ts_daily_3()) %in% c('qb_data_energy_subset_ts_3().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily_4()[,(colnames(qb_data_energy_dat_ts_daily_4()) %in% c('qb_data_energy_subset_ts_4().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily_4()[,(colnames(qb_data_energy_dat_ts_daily_4()) %in% c('qb_data_energy_subset_ts_4().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_FUEL_2_HOURLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_hourly_3()[,(colnames(qb_data_energy_dat_ts_hourly_3()) %in% c('qb_data_energy_subset_ts_3().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly_3()[,(colnames(qb_data_energy_dat_ts_hourly_3()) %in% c('qb_data_energy_subset_ts_3().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly_4()[,(colnames(qb_data_energy_dat_ts_hourly_4()) %in% c('qb_data_energy_subset_ts_4().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly_4()[,(colnames(qb_data_energy_dat_ts_hourly_4()) %in% c('qb_data_energy_subset_ts_4().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_FUEL_2_ALL <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
#       hc_add_series(qb_data_energy_subset_ts_3(), type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_subset_ts_4(), type = "line", name = "High Load: ", color = "green")
#   })
#   
#   qb_date_ind_4_1 <- reactive({paste(input$qb_dates_4[1],"00:00:00",sep = " ")})
#   qb_date_ind_4_2 <- reactive({paste(input$qb_dates_4[2],"00:00:00",sep = " ")})
#   
#   qb_data_energy_subset_dat_5 <- reactive({subset(qb_ind_dat_1,subset = (qb_ind_dat_1$Date_time_local >= qb_date_ind_4_1() & qb_ind_dat_1$Date_time_local <= qb_date_ind_4_2()))})
#   
#   qb_data_energy_dat_ts_5 <- reactive({xts(qb_ind_dat_1$thermal,qb_ind_dat_1$Date_time_local)})
#   qb_data_energy_subset_ts_5 <- reactive({xts(qb_data_energy_subset_dat_5()$thermal,qb_data_energy_subset_dat_5()$Date_time_local)})
#   
#   qb_data_energy_dat_ts_yearly_5 <- reactive({to.yearly(qb_data_energy_dat_ts_5())})
#   qb_data_energy_dat_ts_monthly_5 <- reactive({to.monthly(qb_data_energy_subset_ts_5())})
#   qb_data_energy_dat_ts_weekly_5 <- reactive({to.weekly(qb_data_energy_subset_ts_5())})
#   qb_data_energy_dat_ts_daily_5 <- reactive({to.daily(qb_data_energy_subset_ts_5())})
#   qb_data_energy_dat_ts_hourly_5 <- reactive({to.hourly(qb_data_energy_subset_ts_5())})
#   
#   output$QB_FUEL_3_YEARLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_yearly_5()[,(colnames(qb_data_energy_dat_ts_yearly_5()) %in% c('qb_data_energy_dat_ts_5().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly_5()[,(colnames(qb_data_energy_dat_ts_yearly_5()) %in% c('qb_data_energy_dat_ts_5().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_FUEL_3_WEEKLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_weekly_5()[,(colnames(qb_data_energy_dat_ts_weekly_5()) %in% c('qb_data_energy_subset_ts_5().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly_5()[,(colnames(qb_data_energy_dat_ts_weekly_5()) %in% c('qb_data_energy_subset_ts_5().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_FUEL_3_DAILY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_daily_5()[,(colnames(qb_data_energy_dat_ts_daily_5()) %in% c('qb_data_energy_subset_ts_5().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily_5()[,(colnames(qb_data_energy_dat_ts_daily_5()) %in% c('qb_data_energy_subset_ts_5().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_FUEL_3_HOURLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_hourly_5()[,(colnames(qb_data_energy_dat_ts_hourly_5()) %in% c('qb_data_energy_subset_ts_5().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly_5()[,(colnames(qb_data_energy_dat_ts_hourly_5()) %in% c('qb_data_energy_subset_ts_5().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_FUEL_3_ALL <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
#       hc_add_series(qb_data_energy_subset_ts_5(), type = "line", name = "High Load: ", color = "green")
#   })
#   
#   qb_date_ind_5_1 <- reactive({paste(input$qb_dates_5[1],"00:00:00",sep = " ")})
#   qb_date_ind_5_2 <- reactive({paste(input$qb_dates_5[2],"00:00:00",sep = " ")})
#   
#   qb_date_ind_6_1 <- reactive({paste(input$qb_dates_6[1],"00:00:00",sep = " ")})
#   qb_date_ind_6_2 <- reactive({paste(input$qb_dates_6[2],"00:00:00",sep = " ")})
#   
#   qb_date_ind_7_1 <- reactive({paste(input$qb_dates_7[1],"00:00:00",sep = " ")})
#   qb_date_ind_7_2 <- reactive({paste(input$qb_dates_7[2],"00:00:00",sep = " ")})
#   
#   qb_date_ind_8_1 <- reactive({paste(input$qb_dates_8[1],"00:00:00",sep = " ")})
#   qb_date_ind_8_2 <- reactive({paste(input$qb_dates_8[2],"00:00:00",sep = " ")})
#   
#   qb_date_ind_9_1 <- reactive({paste(input$qb_dates_9[1],"00:00:00",sep = " ")})
#   qb_date_ind_9_2 <- reactive({paste(input$qb_dates_9[2],"00:00:00",sep = " ")})
#   
#   qb_date_ind_10_1 <- reactive({paste(input$qb_dates_10[1],"00:00:00",sep = " ")})
#   qb_date_ind_10_2 <- reactive({paste(input$qb_dates_10[2],"00:00:00",sep = " ")})
#   
#   qb_date_ind_11_1 <- reactive({paste(input$qb_dates_11[1],"00:00:00",sep = " ")})
#   qb_date_ind_11_2 <- reactive({paste(input$qb_dates_11[2],"00:00:00",sep = " ")})
#   
#   qb_date_ind_12_1 <- reactive({paste(input$qb_dates_12[1],"00:00:00",sep = " ")})
#   qb_date_ind_12_2 <- reactive({paste(input$qb_dates_12[2],"00:00:00",sep = " ")})
#   
#   qb_data_energy_subset_dat_6 <- reactive({subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= qb_date_ind_5_1() & qb_ind_dat_3$Date_time_local <= qb_date_ind_5_2()))})
#   qb_data_energy_subset_dat_6_1 <- reactive({subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= qb_date_ind_5_1() & qb_ind_dat_3$Date_time_local <= qb_date_ind_5_2()))})
#   
#   qb_data_energy_dat_ts_6 <- reactive({xts(qb_ind_dat_3$gross_generation_HQP,qb_ind_dat_3$Date_time_local)})
#   qb_data_energy_subset_ts_6 <- reactive({xts(qb_data_energy_subset_dat_6()$gross_generation_HQP,qb_data_energy_subset_dat_6()$Date_time_local)})
#   
#   qb_data_energy_dat_ts_6_1 <- reactive({xts(qb_ind_dat_3$electricity_received_HQP,qb_ind_dat_3$Date_time_local)})
#   qb_data_energy_subset_ts_6_1 <- reactive({xts(qb_data_energy_subset_dat_6_1()$electricity_received_HQP,qb_data_energy_subset_dat_6_1()$Date_time_local)})
#   
#   qb_data_energy_dat_ts_yearly_6 <- reactive({to.yearly(qb_data_energy_dat_ts_6())})
#   qb_data_energy_dat_ts_monthly_6 <- reactive({to.monthly(qb_data_energy_subset_ts_6())})
#   qb_data_energy_dat_ts_weekly_6 <- reactive({to.weekly(qb_data_energy_subset_ts_6())})
#   qb_data_energy_dat_ts_daily_6 <- reactive({to.daily(qb_data_energy_subset_ts_6())})
#   qb_data_energy_dat_ts_hourly_6 <- reactive({to.hourly(qb_data_energy_subset_ts_6())})
#   
#   qb_data_energy_dat_ts_yearly_6_1 <- reactive({to.yearly(qb_data_energy_dat_ts_6_1())})
#   qb_data_energy_dat_ts_monthly_6_1 <- reactive({to.monthly(qb_data_energy_subset_ts_6_1())})
#   qb_data_energy_dat_ts_weekly_6_1 <- reactive({to.weekly(qb_data_energy_subset_ts_6_1())})
#   qb_data_energy_dat_ts_daily_6_1 <- reactive({to.daily(qb_data_energy_subset_ts_6_1())})
#   qb_data_energy_dat_ts_hourly_6_1 <- reactive({to.hourly(qb_data_energy_subset_ts_6_1())})
#   
#   output$QB_ENE_1_YEARLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_yearly_6()[,(colnames(qb_data_energy_dat_ts_yearly_6()) %in% c('qb_data_energy_dat_ts_6().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly_6()[,(colnames(qb_data_energy_dat_ts_yearly_6()) %in% c('qb_data_energy_dat_ts_6().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly_6_1()[,(colnames(qb_data_energy_dat_ts_yearly_6_1()) %in% c('qb_data_energy_dat_ts_6_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly_6_1()[,(colnames(qb_data_energy_dat_ts_yearly_6_1()) %in% c('qb_data_energy_dat_ts_6_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_1_WEEKLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_weekly_6()[,(colnames(qb_data_energy_dat_ts_weekly_6()) %in% c('qb_data_energy_subset_ts_6().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly_6()[,(colnames(qb_data_energy_dat_ts_weekly_6()) %in% c('qb_data_energy_subset_ts_6().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly_6_1()[,(colnames(qb_data_energy_dat_ts_weekly_6_1()) %in% c('qb_data_energy_subset_ts_6_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly_6_1()[,(colnames(qb_data_energy_dat_ts_weekly_6_1()) %in% c('qb_data_energy_subset_ts_6_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_1_DAILY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_daily_6()[,(colnames(qb_data_energy_dat_ts_daily_6()) %in% c('qb_data_energy_subset_ts_6().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily_6()[,(colnames(qb_data_energy_dat_ts_daily_6()) %in% c('qb_data_energy_subset_ts_6().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily_6_1()[,(colnames(qb_data_energy_dat_ts_daily_6_1()) %in% c('qb_data_energy_subset_ts_6_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily_6_1()[,(colnames(qb_data_energy_dat_ts_daily_6_1()) %in% c('qb_data_energy_subset_ts_6_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_1_HOURLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_hourly_6()[,(colnames(qb_data_energy_dat_ts_hourly_6()) %in% c('qb_data_energy_subset_ts_6().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly_6()[,(colnames(qb_data_energy_dat_ts_hourly_6()) %in% c('qb_data_energy_subset_ts_6().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly_6_1()[,(colnames(qb_data_energy_dat_ts_hourly_6_1()) %in% c('qb_data_energy_subset_ts_6_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly_6_1()[,(colnames(qb_data_energy_dat_ts_hourly_6_1()) %in% c('qb_data_energy_subset_ts_6_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_1_ALL <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
#       hc_add_series(qb_data_energy_subset_ts_6(), type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_subset_ts_6_1(), type = "line", name = "High Load: ", color = "green")
#   })
#   
#   
#   qb_data_energy_subset_dat_7 <- reactive({subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= qb_date_ind_6_1() & qb_ind_dat_3$Date_time_local <= qb_date_ind_6_2()))})
#   qb_data_energy_subset_dat_7_1 <- reactive({subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= qb_date_ind_6_1() & qb_ind_dat_3$Date_time_local <= qb_date_ind_6_2()))})
#   
#   qb_data_energy_dat_ts_7 <- reactive({xts(qb_ind_dat_3$producer_consumption_HQP,qb_ind_dat_3$Date_time_local)})
#   qb_data_energy_subset_ts_7 <- reactive({xts(qb_data_energy_subset_dat_7()$producer_consumption_HQP,qb_data_energy_subset_dat_6()$Date_time_local)})
#   
#   qb_data_energy_dat_ts_7_1 <- reactive({xts(qb_ind_dat_3$consumption_loss_interruption,qb_ind_dat_3$Date_time_local)})
#   qb_data_energy_subset_ts_7_1 <- reactive({xts(qb_data_energy_subset_dat_7_1()$consumption_loss_interruption,qb_data_energy_subset_dat_6_1()$Date_time_local)})
#   
#   qb_data_energy_dat_ts_yearly_7 <- reactive({to.yearly(qb_data_energy_dat_ts_7())})
#   qb_data_energy_dat_ts_monthly_7 <- reactive({to.monthly(qb_data_energy_subset_ts_7())})
#   qb_data_energy_dat_ts_weekly_7 <- reactive({to.weekly(qb_data_energy_subset_ts_7())})
#   qb_data_energy_dat_ts_daily_7 <- reactive({to.daily(qb_data_energy_subset_ts_7())})
#   qb_data_energy_dat_ts_hourly_7 <- reactive({to.hourly(qb_data_energy_subset_ts_7())})
#   
#   qb_data_energy_dat_ts_yearly_7_1 <- reactive({to.yearly(qb_data_energy_dat_ts_7_1())})
#   qb_data_energy_dat_ts_monthly_7_1 <- reactive({to.monthly(qb_data_energy_subset_ts_7_1())})
#   qb_data_energy_dat_ts_weekly_7_1 <- reactive({to.weekly(qb_data_energy_subset_ts_7_1())})
#   qb_data_energy_dat_ts_daily_7_1 <- reactive({to.daily(qb_data_energy_subset_ts_7_1())})
#   qb_data_energy_dat_ts_hourly_7_1 <- reactive({to.hourly(qb_data_energy_subset_ts_7_1())})
#   
#   output$QB_ENE_2_YEARLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_yearly_7()[,(colnames(qb_data_energy_dat_ts_yearly_7()) %in% c('qb_data_energy_dat_ts_7().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly_7()[,(colnames(qb_data_energy_dat_ts_yearly_7()) %in% c('qb_data_energy_dat_ts_7().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly_7_1()[,(colnames(qb_data_energy_dat_ts_yearly_7_1()) %in% c('qb_data_energy_dat_ts_7_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly_7_1()[,(colnames(qb_data_energy_dat_ts_yearly_7_1()) %in% c('qb_data_energy_dat_ts_7_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_2_WEEKLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_weekly_7()[,(colnames(qb_data_energy_dat_ts_weekly_7()) %in% c('qb_data_energy_subset_ts_7().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly_7()[,(colnames(qb_data_energy_dat_ts_weekly_7()) %in% c('qb_data_energy_subset_ts_7().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly_7_1()[,(colnames(qb_data_energy_dat_ts_weekly_7_1()) %in% c('qb_data_energy_subset_ts_7_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly_7_1()[,(colnames(qb_data_energy_dat_ts_weekly_7_1()) %in% c('qb_data_energy_subset_ts_7_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_2_DAILY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_daily_7()[,(colnames(qb_data_energy_dat_ts_daily_7()) %in% c('qb_data_energy_subset_ts_7().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily_7()[,(colnames(qb_data_energy_dat_ts_daily_7()) %in% c('qb_data_energy_subset_ts_7().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily_7_1()[,(colnames(qb_data_energy_dat_ts_daily_7_1()) %in% c('qb_data_energy_subset_ts_7_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily_7_1()[,(colnames(qb_data_energy_dat_ts_daily_7_1()) %in% c('qb_data_energy_subset_ts_7_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_2_HOURLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_hourly_7()[,(colnames(qb_data_energy_dat_ts_hourly_7()) %in% c('qb_data_energy_subset_ts_7().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly_7()[,(colnames(qb_data_energy_dat_ts_hourly_7()) %in% c('qb_data_energy_subset_ts_7().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly_7_1()[,(colnames(qb_data_energy_dat_ts_hourly_7_1()) %in% c('qb_data_energy_subset_ts_7_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly_7_1()[,(colnames(qb_data_energy_dat_ts_hourly_7_1()) %in% c('qb_data_energy_subset_ts_7_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_2_ALL <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
#       hc_add_series(qb_data_energy_subset_ts_7(), type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_subset_ts_7_1(), type = "line", name = "High Load: ", color = "green")
#   })
#   
#   
#   
#   qb_data_energy_subset_dat_8 <- reactive({subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= qb_date_ind_7_1() & qb_ind_dat_3$Date_time_local <= qb_date_ind_7_2()))})
#   qb_data_energy_subset_dat_8_1 <- reactive({subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= qb_date_ind_7_1() & qb_ind_dat_3$Date_time_local <= qb_date_ind_7_2()))})
#   
#   qb_data_energy_dat_ts_8 <- reactive({xts(qb_ind_dat_3$load,qb_ind_dat_3$Date_time_local)})
#   qb_data_energy_subset_ts_8 <- reactive({xts(qb_data_energy_subset_dat_8()$load,qb_data_energy_subset_dat_8()$Date_time_local)})
#   
#   qb_data_energy_dat_ts_8_1 <- reactive({xts(qb_ind_dat_3$consumption,qb_ind_dat_3$Date_time_local)})
#   qb_data_energy_subset_ts_8_1 <- reactive({xts(qb_data_energy_subset_dat_8_1()$consumption,qb_data_energy_subset_dat_8_1()$Date_time_local)})
#   
#   qb_data_energy_dat_ts_yearly_8 <- reactive({to.yearly(qb_data_energy_dat_ts_8())})
#   qb_data_energy_dat_ts_monthly_8 <- reactive({to.monthly(qb_data_energy_subset_ts_8())})
#   qb_data_energy_dat_ts_weekly_8 <- reactive({to.weekly(qb_data_energy_subset_ts_8())})
#   qb_data_energy_dat_ts_daily_8 <- reactive({to.daily(qb_data_energy_subset_ts_8())})
#   qb_data_energy_dat_ts_hourly_8 <- reactive({to.hourly(qb_data_energy_subset_ts_8())})
#   
#   qb_data_energy_dat_ts_yearly_8_1 <- reactive({to.yearly(qb_data_energy_dat_ts_8_1())})
#   qb_data_energy_dat_ts_monthly_8_1 <- reactive({to.monthly(qb_data_energy_subset_ts_8_1())})
#   qb_data_energy_dat_ts_weekly_8_1 <- reactive({to.weekly(qb_data_energy_subset_ts_8_1())})
#   qb_data_energy_dat_ts_daily_8_1 <- reactive({to.daily(qb_data_energy_subset_ts_8_1())})
#   qb_data_energy_dat_ts_hourly_8_1 <- reactive({to.hourly(qb_data_energy_subset_ts_8_1())})
#   
#   output$QB_ENE_3_YEARLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_yearly_8()[,(colnames(qb_data_energy_dat_ts_yearly_8()) %in% c('qb_data_energy_dat_ts_8().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly_8()[,(colnames(qb_data_energy_dat_ts_yearly_8()) %in% c('qb_data_energy_dat_ts_8().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly_8_1()[,(colnames(qb_data_energy_dat_ts_yearly_8_1()) %in% c('qb_data_energy_dat_ts_8_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly_8_1()[,(colnames(qb_data_energy_dat_ts_yearly_8_1()) %in% c('qb_data_energy_dat_ts_8_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_3_WEEKLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_weekly_8()[,(colnames(qb_data_energy_dat_ts_weekly_8()) %in% c('qb_data_energy_subset_ts_8().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly_8()[,(colnames(qb_data_energy_dat_ts_weekly_8()) %in% c('qb_data_energy_subset_ts_8().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly_8_1()[,(colnames(qb_data_energy_dat_ts_weekly_8_1()) %in% c('qb_data_energy_subset_ts_8_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly_8_1()[,(colnames(qb_data_energy_dat_ts_weekly_8_1()) %in% c('qb_data_energy_subset_ts_8_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_3_DAILY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_daily_8()[,(colnames(qb_data_energy_dat_ts_daily_8()) %in% c('qb_data_energy_subset_ts_8().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily_8()[,(colnames(qb_data_energy_dat_ts_daily_8()) %in% c('qb_data_energy_subset_ts_8().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily_8_1()[,(colnames(qb_data_energy_dat_ts_daily_8_1()) %in% c('qb_data_energy_subset_ts_8_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily_8_1()[,(colnames(qb_data_energy_dat_ts_daily_8_1()) %in% c('qb_data_energy_subset_ts_8_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_3_HOURLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_hourly_8()[,(colnames(qb_data_energy_dat_ts_hourly_8()) %in% c('qb_data_energy_subset_ts_8().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly_8()[,(colnames(qb_data_energy_dat_ts_hourly_8()) %in% c('qb_data_energy_subset_ts_8().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly_8_1()[,(colnames(qb_data_energy_dat_ts_hourly_8_1()) %in% c('qb_data_energy_subset_ts_8_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly_8_1()[,(colnames(qb_data_energy_dat_ts_hourly_8_1()) %in% c('qb_data_energy_subset_ts_8_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_3_ALL <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
#       hc_add_series(qb_data_energy_subset_ts_8(), type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_subset_ts_8_1(), type = "line", name = "High Load: ", color = "green")
#   })
#   
#   
#   qb_data_energy_subset_dat_9 <- reactive({subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= qb_date_ind_8_1() & qb_ind_dat_3$Date_time_local <= qb_date_ind_8_2()))})
#   qb_data_energy_subset_dat_9_1 <- reactive({subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= qb_date_ind_8_1() & qb_ind_dat_3$Date_time_local <= qb_date_ind_8_2()))})
#   
#   qb_data_energy_dat_ts_9 <- reactive({xts(qb_ind_dat_3$export,qb_ind_dat_3$Date_time_local)})
#   qb_data_energy_subset_ts_9 <- reactive({xts(qb_data_energy_subset_dat_9()$export,qb_data_energy_subset_dat_9()$Date_time_local)})
#   
#   qb_data_energy_dat_ts_9_1 <- reactive({xts(qb_ind_dat_3$electrivity_delivered_HQD,qb_ind_dat_3$Date_time_local)})
#   qb_data_energy_subset_ts_9_1 <- reactive({xts(qb_data_energy_subset_dat_9_1()$electrivity_delivered_HQD,qb_data_energy_subset_dat_9_1()$Date_time_local)})
#   
#   qb_data_energy_dat_ts_yearly_9 <- reactive({to.yearly(qb_data_energy_dat_ts_9())})
#   qb_data_energy_dat_ts_monthly_9 <- reactive({to.monthly(qb_data_energy_subset_ts_9())})
#   qb_data_energy_dat_ts_weekly_9 <- reactive({to.weekly(qb_data_energy_subset_ts_9())})
#   qb_data_energy_dat_ts_daily_9 <- reactive({to.daily(qb_data_energy_subset_ts_9())})
#   qb_data_energy_dat_ts_hourly_9 <- reactive({to.hourly(qb_data_energy_subset_ts_9())})
#   
#   qb_data_energy_dat_ts_yearly_9_1 <- reactive({to.yearly(qb_data_energy_dat_ts_9_1())})
#   qb_data_energy_dat_ts_monthly_9_1 <- reactive({to.monthly(qb_data_energy_subset_ts_9_1())})
#   qb_data_energy_dat_ts_weekly_9_1 <- reactive({to.weekly(qb_data_energy_subset_ts_9_1())})
#   qb_data_energy_dat_ts_daily_9_1 <- reactive({to.daily(qb_data_energy_subset_ts_9_1())})
#   qb_data_energy_dat_ts_hourly_9_1 <- reactive({to.hourly(qb_data_energy_subset_ts_9_1())})
#   
#   output$QB_ENE_4_YEARLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_yearly_9()[,(colnames(qb_data_energy_dat_ts_yearly_9()) %in% c('qb_data_energy_dat_ts_9().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly_9()[,(colnames(qb_data_energy_dat_ts_yearly_9()) %in% c('qb_data_energy_dat_ts_9().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly_9_1()[,(colnames(qb_data_energy_dat_ts_yearly_9_1()) %in% c('qb_data_energy_dat_ts_9_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly_9_1()[,(colnames(qb_data_energy_dat_ts_yearly_9_1()) %in% c('qb_data_energy_dat_ts_9_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_4_WEEKLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_weekly_9()[,(colnames(qb_data_energy_dat_ts_weekly_9()) %in% c('qb_data_energy_subset_ts_9().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly_9()[,(colnames(qb_data_energy_dat_ts_weekly_9()) %in% c('qb_data_energy_subset_ts_9().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly_9_1()[,(colnames(qb_data_energy_dat_ts_weekly_9_1()) %in% c('qb_data_energy_subset_ts_9_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly_9_1()[,(colnames(qb_data_energy_dat_ts_weekly_9_1()) %in% c('qb_data_energy_subset_ts_9_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_4_DAILY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_daily_9()[,(colnames(qb_data_energy_dat_ts_daily_9()) %in% c('qb_data_energy_subset_ts_9().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily_9()[,(colnames(qb_data_energy_dat_ts_daily_9()) %in% c('qb_data_energy_subset_ts_9().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily_9_1()[,(colnames(qb_data_energy_dat_ts_daily_9_1()) %in% c('qb_data_energy_subset_ts_9_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily_9_1()[,(colnames(qb_data_energy_dat_ts_daily_9_1()) %in% c('qb_data_energy_subset_ts_9_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_4_HOURLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_hourly_9()[,(colnames(qb_data_energy_dat_ts_hourly_9()) %in% c('qb_data_energy_subset_ts_9().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly_9()[,(colnames(qb_data_energy_dat_ts_hourly_9()) %in% c('qb_data_energy_subset_ts_9().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly_9_1()[,(colnames(qb_data_energy_dat_ts_hourly_9_1()) %in% c('qb_data_energy_subset_ts_9_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly_9_1()[,(colnames(qb_data_energy_dat_ts_hourly_9_1()) %in% c('qb_data_energy_subset_ts_9_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_4_ALL <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
#       hc_add_series(qb_data_energy_subset_ts_9(), type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_subset_ts_9_1(), type = "line", name = "High Load: ", color = "green")
#   })
#   
#   
#   qb_data_energy_subset_dat_10 <- reactive({subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= qb_date_ind_9_1() & qb_ind_dat_3$Date_time_local <= qb_date_ind_9_2()))})
#   qb_data_energy_subset_dat_10_1 <- reactive({subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= qb_date_ind_9_1() & qb_ind_dat_3$Date_time_local <= qb_date_ind_9_2()))})
#   
#   qb_data_energy_dat_ts_10 <- reactive({xts(qb_ind_dat_3$vol_nonheritage_supply,qb_ind_dat_3$Date_time_local)})
#   qb_data_energy_subset_ts_10 <- reactive({xts(qb_data_energy_subset_dat_10()$vol_nonheritage_supply,qb_data_energy_subset_dat_10()$Date_time_local)})
#   
#   qb_data_energy_dat_ts_10_1 <- reactive({xts(qb_ind_dat_3$vol_excessofheritage_mobilized,qb_ind_dat_3$Date_time_local)})
#   qb_data_energy_subset_ts_10_1 <- reactive({xts(qb_data_energy_subset_dat_10_1()$vol_excessofheritage_mobilized,qb_data_energy_subset_dat_10_1()$Date_time_local)})
#   
#   qb_data_energy_dat_ts_yearly_10 <- reactive({to.yearly(qb_data_energy_dat_ts_10())})
#   qb_data_energy_dat_ts_monthly_10 <- reactive({to.monthly(qb_data_energy_subset_ts_10())})
#   qb_data_energy_dat_ts_weekly_10 <- reactive({to.weekly(qb_data_energy_subset_ts_10())})
#   qb_data_energy_dat_ts_daily_10 <- reactive({to.daily(qb_data_energy_subset_ts_10())})
#   qb_data_energy_dat_ts_hourly_10 <- reactive({to.hourly(qb_data_energy_subset_ts_10())})
#   
#   qb_data_energy_dat_ts_yearly_10_1 <- reactive({to.yearly(qb_data_energy_dat_ts_10_1())})
#   qb_data_energy_dat_ts_monthly_10_1 <- reactive({to.monthly(qb_data_energy_subset_ts_10_1())})
#   qb_data_energy_dat_ts_weekly_10_1 <- reactive({to.weekly(qb_data_energy_subset_ts_10_1())})
#   qb_data_energy_dat_ts_daily_10_1 <- reactive({to.daily(qb_data_energy_subset_ts_10_1())})
#   qb_data_energy_dat_ts_hourly_10_1 <- reactive({to.hourly(qb_data_energy_subset_ts_10_1())})
#   
#   output$QB_ENE_5_YEARLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_yearly_10()[,(colnames(qb_data_energy_dat_ts_yearly_10()) %in% c('qb_data_energy_dat_ts_10().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly_10()[,(colnames(qb_data_energy_dat_ts_yearly_10()) %in% c('qb_data_energy_dat_ts_10().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly_10_1()[,(colnames(qb_data_energy_dat_ts_yearly_10_1()) %in% c('qb_data_energy_dat_ts_10_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly_10_1()[,(colnames(qb_data_energy_dat_ts_yearly_10_1()) %in% c('qb_data_energy_dat_ts_10_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_5_WEEKLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_weekly_10()[,(colnames(qb_data_energy_dat_ts_weekly_10()) %in% c('qb_data_energy_subset_ts_10().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly_10()[,(colnames(qb_data_energy_dat_ts_weekly_10()) %in% c('qb_data_energy_subset_ts_10().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly_10_1()[,(colnames(qb_data_energy_dat_ts_weekly_10_1()) %in% c('qb_data_energy_subset_ts_10_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly_10_1()[,(colnames(qb_data_energy_dat_ts_weekly_10_1()) %in% c('qb_data_energy_subset_ts_10_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_5_DAILY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_daily_10()[,(colnames(qb_data_energy_dat_ts_daily_10()) %in% c('qb_data_energy_subset_ts_10().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily_10()[,(colnames(qb_data_energy_dat_ts_daily_10()) %in% c('qb_data_energy_subset_ts_10().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily_10_1()[,(colnames(qb_data_energy_dat_ts_daily_10_1()) %in% c('qb_data_energy_subset_ts_10_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily_10_1()[,(colnames(qb_data_energy_dat_ts_daily_10_1()) %in% c('qb_data_energy_subset_ts_10_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_5_HOURLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_hourly_10()[,(colnames(qb_data_energy_dat_ts_hourly_10()) %in% c('qb_data_energy_subset_ts_10().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly_10()[,(colnames(qb_data_energy_dat_ts_hourly_10()) %in% c('qb_data_energy_subset_ts_10().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly_10_1()[,(colnames(qb_data_energy_dat_ts_hourly_10_1()) %in% c('qb_data_energy_subset_ts_10_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly_10_1()[,(colnames(qb_data_energy_dat_ts_hourly_10_1()) %in% c('qb_data_energy_subset_ts_10_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_5_ALL <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
#       hc_add_series(qb_data_energy_subset_ts_10(), type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_subset_ts_10_1(), type = "line", name = "High Load: ", color = "green")
#   })
#   
#   
#   qb_data_energy_subset_dat_11 <- reactive({subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= qb_date_ind_10_1() & qb_ind_dat_3$Date_time_local <= qb_date_ind_10_2()))})
#   qb_data_energy_subset_dat_11_1 <- reactive({subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= qb_date_ind_10_1() & qb_ind_dat_3$Date_time_local <= qb_date_ind_10_2()))})
#   qb_data_energy_subset_dat_11_2 <- reactive({subset(qb_ind_dat_2,subset = (qb_ind_dat_2$Date_Time_UTC >= qb_date_ind_12_1() & qb_ind_dat_2$Date_Time_UTC <= qb_date_ind_12_2()))})
#   
#   qb_data_energy_dat_ts_11 <- reactive({xts(qb_ind_dat_3$total_consumption,qb_ind_dat_3$Date_time_local)})
#   qb_data_energy_subset_ts_11 <- reactive({xts(qb_data_energy_subset_dat_11()$total_consumption,qb_data_energy_subset_dat_11()$Date_time_local)})
#   
#   qb_data_energy_dat_ts_11_1 <- reactive({xts(qb_ind_dat_3$unitcost_excess,qb_ind_dat_3$Date_time_local)})
#   qb_data_energy_subset_ts_11_1 <- reactive({xts(qb_data_energy_subset_dat_11_1()$unitcost_excess,qb_data_energy_subset_dat_11_1()$Date_time_local)})
#   
#   qb_data_energy_dat_ts_11_2 <- reactive({xts(qb_ind_dat_2$total_demand,qb_ind_dat_2$Date_Time_UTC)})
#   qb_data_energy_subset_ts_11_2 <- reactive({xts(qb_data_energy_subset_dat_11_2()$total_demand,qb_data_energy_subset_dat_11_2()$Date_Time_UTC)})
#   
#   qb_data_energy_dat_ts_yearly_11 <- reactive({to.yearly(qb_data_energy_dat_ts_11())})
#   qb_data_energy_dat_ts_monthly_11 <- reactive({to.monthly(qb_data_energy_subset_ts_11())})
#   qb_data_energy_dat_ts_weekly_11 <- reactive({to.weekly(qb_data_energy_subset_ts_11())})
#   qb_data_energy_dat_ts_daily_11 <- reactive({to.daily(qb_data_energy_subset_ts_11())})
#   qb_data_energy_dat_ts_hourly_11 <- reactive({to.hourly(qb_data_energy_subset_ts_11())})
#   
#   qb_data_energy_dat_ts_yearly_11_1 <- reactive({to.yearly(qb_data_energy_dat_ts_11_1())})
#   qb_data_energy_dat_ts_monthly_11_1 <- reactive({to.monthly(qb_data_energy_subset_ts_11_1())})
#   qb_data_energy_dat_ts_weekly_11_1 <- reactive({to.weekly(qb_data_energy_subset_ts_11_1())})
#   qb_data_energy_dat_ts_daily_11_1 <- reactive({to.daily(qb_data_energy_subset_ts_11_1())})
#   qb_data_energy_dat_ts_hourly_11_1 <- reactive({to.hourly(qb_data_energy_subset_ts_11_1())})
#   
#   qb_data_energy_dat_ts_yearly_11_2 <- reactive({to.yearly(qb_data_energy_dat_ts_11_2())})
#   qb_data_energy_dat_ts_monthly_11_2 <- reactive({to.monthly(qb_data_energy_subset_ts_11_2())})
#   qb_data_energy_dat_ts_weekly_11_2 <- reactive({to.weekly(qb_data_energy_subset_ts_11_2())})
#   qb_data_energy_dat_ts_daily_11_2 <- reactive({to.daily(qb_data_energy_subset_ts_11_2())})
#   qb_data_energy_dat_ts_hourly_11_2 <- reactive({to.hourly(qb_data_energy_subset_ts_11_2())})
#   
#   output$QB_ENE_6_YEARLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_yearly_11()[,(colnames(qb_data_energy_dat_ts_yearly_11()) %in% c('qb_data_energy_dat_ts_11().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly_11()[,(colnames(qb_data_energy_dat_ts_yearly_11()) %in% c('qb_data_energy_dat_ts_11().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly_11_1()[,(colnames(qb_data_energy_dat_ts_yearly_11_1()) %in% c('qb_data_energy_dat_ts_11_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly_11_1()[,(colnames(qb_data_energy_dat_ts_yearly_11_1()) %in% c('qb_data_energy_dat_ts_11_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_6_WEEKLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_weekly_11()[,(colnames(qb_data_energy_dat_ts_weekly_11()) %in% c('qb_data_energy_subset_ts_11().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly_11()[,(colnames(qb_data_energy_dat_ts_weekly_11()) %in% c('qb_data_energy_subset_ts_11().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly_11_1()[,(colnames(qb_data_energy_dat_ts_weekly_11_1()) %in% c('qb_data_energy_subset_ts_11_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly_11_1()[,(colnames(qb_data_energy_dat_ts_weekly_11_1()) %in% c('qb_data_energy_subset_ts_11_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_6_DAILY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_daily_11()[,(colnames(qb_data_energy_dat_ts_daily_11()) %in% c('qb_data_energy_subset_ts_11().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily_11()[,(colnames(qb_data_energy_dat_ts_daily_11()) %in% c('qb_data_energy_subset_ts_11().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily_11_1()[,(colnames(qb_data_energy_dat_ts_daily_11_1()) %in% c('qb_data_energy_subset_ts_11_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily_11_1()[,(colnames(qb_data_energy_dat_ts_daily_11_1()) %in% c('qb_data_energy_subset_ts_11_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_6_HOURLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_hourly_11()[,(colnames(qb_data_energy_dat_ts_hourly_11()) %in% c('qb_data_energy_subset_ts_11().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly_11()[,(colnames(qb_data_energy_dat_ts_hourly_11()) %in% c('qb_data_energy_subset_ts_11().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly_11_1()[,(colnames(qb_data_energy_dat_ts_hourly_11_1()) %in% c('qb_data_energy_subset_ts_11_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly_11_1()[,(colnames(qb_data_energy_dat_ts_hourly_11_1()) %in% c('qb_data_energy_subset_ts_11_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_6_ALL <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
#       hc_add_series(qb_data_energy_subset_ts_11(), type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_subset_ts_11_1(), type = "line", name = "High Load: ", color = "green")
#   })
#   
#   output$QB_ENE_8_YEARLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_yearly_11_2()[,(colnames(qb_data_energy_dat_ts_yearly_11_2()) %in% c('qb_data_energy_dat_ts_11_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly_11_2()[,(colnames(qb_data_energy_dat_ts_yearly_11_2()) %in% c('qb_data_energy_dat_ts_11_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_8_WEEKLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_weekly_11_2()[,(colnames(qb_data_energy_dat_ts_weekly_11_2()) %in% c('qb_data_energy_subset_ts_11_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly_11_2()[,(colnames(qb_data_energy_dat_ts_weekly_11_2()) %in% c('qb_data_energy_subset_ts_11_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_8_DAILY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_daily_11_2()[,(colnames(qb_data_energy_dat_ts_daily_11_2()) %in% c('qb_data_energy_subset_ts_11_2().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily_11_2()[,(colnames(qb_data_energy_dat_ts_daily_11_2()) %in% c('qb_data_energy_subset_ts_11_2().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_8_HOURLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_hourly_11_2()[,(colnames(qb_data_energy_dat_ts_hourly_11_2()) %in% c('qb_data_energy_subset_ts_11_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly_11_2()[,(colnames(qb_data_energy_dat_ts_hourly_11_2()) %in% c('qb_data_energy_subset_ts_11_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_8_ALL <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
#       hc_add_series(qb_data_energy_subset_ts_11_2(), type = "line", name = "High Load: ", color = "green")
#   })
#   
#   
#   qb_data_energy_subset_dat_12 <- reactive({subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= qb_date_ind_11_1() & qb_ind_dat_3$Date_time_local <= qb_date_ind_11_2()))})
#   qb_data_energy_subset_dat_12_1 <- reactive({subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= qb_date_ind_11_1() & qb_ind_dat_3$Date_time_local <= qb_date_ind_11_2()))})
#   
#   
#   qb_data_energy_dat_ts_12 <- reactive({xts(qb_ind_dat_3$cost,qb_ind_dat_3$Date_time_local)})
#   qb_data_energy_subset_ts_12 <- reactive({xts(qb_data_energy_subset_dat_12()$cost,qb_data_energy_subset_dat_12()$Date_time_local)})
#   
#   qb_data_energy_dat_ts_12_1 <- reactive({xts(qb_ind_dat_3$cost_hourly,qb_ind_dat_3$Date_time_local)})
#   qb_data_energy_subset_ts_12_1 <- reactive({xts(qb_data_energy_subset_dat_12_1()$cost_hourly,qb_data_energy_subset_dat_12_1()$Date_time_local)})
#   
#   qb_data_energy_dat_ts_yearly_12 <- reactive({to.yearly(qb_data_energy_dat_ts_12())})
#   qb_data_energy_dat_ts_monthly_12 <- reactive({to.monthly(qb_data_energy_subset_ts_12())})
#   qb_data_energy_dat_ts_weekly_12 <- reactive({to.weekly(qb_data_energy_subset_ts_12())})
#   qb_data_energy_dat_ts_daily_12 <- reactive({to.daily(qb_data_energy_subset_ts_12())})
#   qb_data_energy_dat_ts_hourly_12 <- reactive({to.hourly(qb_data_energy_subset_ts_12())})
#   
#   qb_data_energy_dat_ts_yearly_12_1 <- reactive({to.yearly(qb_data_energy_dat_ts_12_1())})
#   qb_data_energy_dat_ts_monthly_12_1 <- reactive({to.monthly(qb_data_energy_subset_ts_12_1())})
#   qb_data_energy_dat_ts_weekly_12_1 <- reactive({to.weekly(qb_data_energy_subset_ts_12_1())})
#   qb_data_energy_dat_ts_daily_12_1 <- reactive({to.daily(qb_data_energy_subset_ts_12_1())})
#   qb_data_energy_dat_ts_hourly_12_1 <- reactive({to.hourly(qb_data_energy_subset_ts_12_1())})
#   
#   output$QB_ENE_7_YEARLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_yearly_12()[,(colnames(qb_data_energy_dat_ts_yearly_12()) %in% c('qb_data_energy_dat_ts_12().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly_12()[,(colnames(qb_data_energy_dat_ts_yearly_12()) %in% c('qb_data_energy_dat_ts_12().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly_12_1()[,(colnames(qb_data_energy_dat_ts_yearly_12_1()) %in% c('qb_data_energy_dat_ts_12_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_yearly_12_1()[,(colnames(qb_data_energy_dat_ts_yearly_12_1()) %in% c('qb_data_energy_dat_ts_12_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_7_WEEKLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_weekly_12()[,(colnames(qb_data_energy_dat_ts_weekly_12()) %in% c('qb_data_energy_subset_ts_12().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly_12()[,(colnames(qb_data_energy_dat_ts_weekly_12()) %in% c('qb_data_energy_subset_ts_12().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly_12_1()[,(colnames(qb_data_energy_dat_ts_weekly_12_1()) %in% c('qb_data_energy_subset_ts_12_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_weekly_12_1()[,(colnames(qb_data_energy_dat_ts_weekly_12_1()) %in% c('qb_data_energy_subset_ts_12_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_7_DAILY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_daily_12()[,(colnames(qb_data_energy_dat_ts_daily_12()) %in% c('qb_data_energy_subset_ts_12().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily_12()[,(colnames(qb_data_energy_dat_ts_daily_12()) %in% c('qb_data_energy_subset_ts_12().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily_12_1()[,(colnames(qb_data_energy_dat_ts_daily_12_1()) %in% c('qb_data_energy_subset_ts_12_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_daily_12_1()[,(colnames(qb_data_energy_dat_ts_daily_12_1()) %in% c('qb_data_energy_subset_ts_12_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_7_HOURLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(qb_data_energy_dat_ts_hourly_12()[,(colnames(qb_data_energy_dat_ts_hourly_12()) %in% c('qb_data_energy_subset_ts_12().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly_12()[,(colnames(qb_data_energy_dat_ts_hourly_12()) %in% c('qb_data_energy_subset_ts_12().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly_12_1()[,(colnames(qb_data_energy_dat_ts_hourly_12_1()) %in% c('qb_data_energy_subset_ts_12_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_dat_ts_hourly_12_1()[,(colnames(qb_data_energy_dat_ts_hourly_12_1()) %in% c('qb_data_energy_subset_ts_12_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$QB_ENE_7_ALL <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
#       hc_add_series(qb_data_energy_subset_ts_12(), type = "line", name = "High Load: ", color = "green") %>%
#       hc_add_series(qb_data_energy_subset_ts_12_1(), type = "line", name = "High Load: ", color = "green")
#   })
#   
#   
#   
#   
#   
#   
#   ab_date_ind_8_1 <- reactive({paste(input$ab_dates_8[1],"00:00:00",sep = " ")})
#   ab_date_ind_8_2 <- reactive({paste(input$ab_dates_8[2],"00:00:00",sep = " ")})
#   ab_ind_dat_8_asset_filter <- reactive({input$ab_ind_8_ff})
#   
#   ab_ind_dat_8_sub_1 <- reactive({subset(ab_ind_dat_8,subset = (ab_ind_dat_8$Update_Time >= ab_date_ind_8_1() & ab_ind_dat_8$Update_Time <= ab_date_ind_8_2()))})
#   ab_ind_dat_8_sub_2 <- reactive({subset(ab_ind_dat_8_sub_1(),subset = (ab_ind_dat_8_sub_1()$ASSET == ab_ind_dat_8_asset_filter()))})
#   
#   ab_ind_dat_8_sub_1_xts_1_1 <- reactive({xts(ab_ind_dat_8_sub_2()$MC,ab_ind_dat_8_sub_2()$Update_Time)})
#   ab_ind_dat_8_sub_1_xts_1_2 <- reactive({xts(ab_ind_dat_8_sub_2()$TNG,ab_ind_dat_8_sub_2()$Update_Time)})
#   ab_ind_dat_8_sub_1_xts_1_3 <- reactive({xts(ab_ind_dat_8_sub_2()$DCR,ab_ind_dat_8_sub_2()$Update_Time)})
#   
#   ab_ind_dat_8_sub_1_xts_2_1 <- reactive({xts(ab_ind_dat_8_sub_1()$MC,ab_ind_dat_8_sub_1()$Update_Time)})
#   ab_ind_dat_8_sub_1_xts_2_2 <- reactive({xts(ab_ind_dat_8_sub_1()$TNG,ab_ind_dat_8_sub_1()$Update_Time)})
#   ab_ind_dat_8_sub_1_xts_2_3 <- reactive({xts(ab_ind_dat_8_sub_1()$DCR,ab_ind_dat_8_sub_1()$Update_Time)})
#   
#   ab_ind_dat_8_sub_1_xts_1_1_yearly_1 <- reactive({to.yearly(ab_ind_dat_8_sub_1_xts_2_1())})
#   ab_ind_dat_8_sub_1_xts_1_1_yearly_2 <- reactive({to.yearly(ab_ind_dat_8_sub_1_xts_2_2())})
#   ab_ind_dat_8_sub_1_xts_1_1_yearly_3 <- reactive({to.yearly(ab_ind_dat_8_sub_1_xts_2_3())})
#   ab_ind_dat_8_sub_1_xts_1_1_monthly_1 <- reactive({to.monthly(ab_ind_dat_8_sub_1_xts_1_1())})
#   ab_ind_dat_8_sub_1_xts_1_1_monthly_2 <- reactive({to.monthly(ab_ind_dat_8_sub_1_xts_1_2())})
#   ab_ind_dat_8_sub_1_xts_1_1_monthly_3 <- reactive({to.monthly(ab_ind_dat_8_sub_1_xts_1_3())})
#   ab_ind_dat_8_sub_1_xts_1_1_weekly_1 <- reactive({to.weekly(ab_ind_dat_8_sub_1_xts_1_1())})
#   ab_ind_dat_8_sub_1_xts_1_1_weekly_2 <- reactive({to.weekly(ab_ind_dat_8_sub_1_xts_1_2())})
#   ab_ind_dat_8_sub_1_xts_1_1_weekly_3 <- reactive({to.weekly(ab_ind_dat_8_sub_1_xts_1_3())})
#   ab_ind_dat_8_sub_1_xts_1_1_daily_1 <- reactive({to.daily(ab_ind_dat_8_sub_1_xts_1_1())})
#   ab_ind_dat_8_sub_1_xts_1_1_daily_2 <- reactive({to.daily(ab_ind_dat_8_sub_1_xts_1_2())})
#   ab_ind_dat_8_sub_1_xts_1_1_daily_3 <- reactive({to.daily(ab_ind_dat_8_sub_1_xts_1_3())})
#   ab_ind_dat_8_sub_1_xts_1_1_hourly_1 <- reactive({to.hourly(ab_ind_dat_8_sub_1_xts_1_1())})
#   ab_ind_dat_8_sub_1_xts_1_1_hourly_2 <- reactive({to.hourly(ab_ind_dat_8_sub_1_xts_1_2())})
#   ab_ind_dat_8_sub_1_xts_1_1_hourly_3 <- reactive({to.hourly(ab_ind_dat_8_sub_1_xts_1_3())})
#   
#   output$AB_ENE_8_YEARLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_yearly_1()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_yearly_1()) %in% c('ab_ind_dat_8_sub_1_xts_2_1().High'))], type = "line", name = "High MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_yearly_1()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_yearly_1()) %in% c('ab_ind_dat_8_sub_1_xts_2_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
#       hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_yearly_2()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_yearly_2()) %in% c('ab_ind_dat_8_sub_1_xts_2_2().High'))], type = "line", name = "High TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_yearly_2()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_yearly_2()) %in% c('ab_ind_dat_8_sub_1_xts_2_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
#       hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_yearly_3()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_yearly_3()) %in% c('ab_ind_dat_8_sub_1_xts_2_3().High'))], type = "line", name = "High DCR", color = "green") %>%
#       hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_yearly_3()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_yearly_3()) %in% c('ab_ind_dat_8_sub_1_xts_2_3().Low'))], type = "line", name = "Low DCR ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$AB_ENE_8_WEEKLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_weekly_1()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_weekly_1()) %in% c('ab_ind_dat_8_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_weekly_1()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_weekly_1()) %in% c('ab_ind_dat_8_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
#       hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_weekly_2()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_weekly_2()) %in% c('ab_ind_dat_8_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_weekly_2()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_weekly_2()) %in% c('ab_ind_dat_8_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
#       hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_weekly_3()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_weekly_3()) %in% c('ab_ind_dat_8_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
#       hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_weekly_3()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_weekly_3()) %in% c('ab_ind_dat_8_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$AB_ENE_8_DAILY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_daily_1()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_daily_1()) %in% c('ab_ind_dat_8_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_daily_1()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_daily_1()) %in% c('ab_ind_dat_8_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
#       hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_daily_2()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_daily_2()) %in% c('ab_ind_dat_8_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_daily_2()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_daily_2()) %in% c('ab_ind_dat_8_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
#       hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_daily_3()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_daily_3()) %in% c('ab_ind_dat_8_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
#       hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_daily_3()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_daily_3()) %in% c('ab_ind_dat_8_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$AB_ENE_8_HOURLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_hourly_1()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_hourly_1()) %in% c('ab_ind_dat_8_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_hourly_1()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_hourly_1()) %in% c('ab_ind_dat_8_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
#       hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_hourly_2()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_hourly_2()) %in% c('ab_ind_dat_8_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_hourly_2()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_hourly_2()) %in% c('ab_ind_dat_8_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
#       hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_hourly_3()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_hourly_3()) %in% c('ab_ind_dat_8_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
#       hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_hourly_3()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_hourly_3()) %in% c('ab_ind_dat_8_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$AB_ENE_8_ALL <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
#       hc_add_series(ab_ind_dat_8_sub_1_xts_1_1(), type = "line", name = "MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_8_sub_1_xts_1_2(), type = "line", name = "TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_8_sub_1_xts_1_3(), type = "line", name = "DCR", color = "green")
#   })
#   
#   ab_date_ind_9_1 <- reactive({paste(input$ab_dates_9[1],"00:00:00",sep = " ")})
#   ab_date_ind_9_2 <- reactive({paste(input$ab_dates_9[2],"00:00:00",sep = " ")})
#   ab_ind_dat_9_asset_filter <- reactive({input$ab_ind_9_ff})
#   
#   ab_ind_dat_9_sub_1 <- reactive({subset(ab_ind_dat_9,subset = (ab_ind_dat_9$Update_Time >= ab_date_ind_9_1() & ab_ind_dat_9$Update_Time <= ab_date_ind_9_2()))})
#   ab_ind_dat_9_sub_2 <- reactive({subset(ab_ind_dat_9_sub_1(),subset = (ab_ind_dat_9_sub_1()$ASSET == ab_ind_dat_9_asset_filter()))})
#   
#   ab_ind_dat_9_sub_1_xts_1_1 <- reactive({xts(ab_ind_dat_9_sub_2()$MC,ab_ind_dat_9_sub_2()$Update_Time)})
#   ab_ind_dat_9_sub_1_xts_1_2 <- reactive({xts(ab_ind_dat_9_sub_2()$TNG,ab_ind_dat_9_sub_2()$Update_Time)})
#   ab_ind_dat_9_sub_1_xts_1_3 <- reactive({xts(ab_ind_dat_9_sub_2()$DCR,ab_ind_dat_9_sub_2()$Update_Time)})
#   
#   ab_ind_dat_9_sub_1_xts_2_1 <- reactive({xts(ab_ind_dat_9_sub_1()$MC,ab_ind_dat_9_sub_1()$Update_Time)})
#   ab_ind_dat_9_sub_1_xts_2_2 <- reactive({xts(ab_ind_dat_9_sub_1()$TNG,ab_ind_dat_9_sub_1()$Update_Time)})
#   ab_ind_dat_9_sub_1_xts_2_3 <- reactive({xts(ab_ind_dat_9_sub_1()$DCR,ab_ind_dat_9_sub_1()$Update_Time)})
#   
#   ab_ind_dat_9_sub_1_xts_1_1_yearly_1 <- reactive({to.yearly(ab_ind_dat_9_sub_1_xts_2_1())})
#   ab_ind_dat_9_sub_1_xts_1_1_yearly_2 <- reactive({to.yearly(ab_ind_dat_9_sub_1_xts_2_2())})
#   ab_ind_dat_9_sub_1_xts_1_1_yearly_3 <- reactive({to.yearly(ab_ind_dat_9_sub_1_xts_2_3())})
#   ab_ind_dat_9_sub_1_xts_1_1_monthly_1 <- reactive({to.monthly(ab_ind_dat_9_sub_1_xts_1_1())})
#   ab_ind_dat_9_sub_1_xts_1_1_monthly_2 <- reactive({to.monthly(ab_ind_dat_9_sub_1_xts_1_2())})
#   ab_ind_dat_9_sub_1_xts_1_1_monthly_3 <- reactive({to.monthly(ab_ind_dat_9_sub_1_xts_1_3())})
#   ab_ind_dat_9_sub_1_xts_1_1_weekly_1 <- reactive({to.weekly(ab_ind_dat_9_sub_1_xts_1_1())})
#   ab_ind_dat_9_sub_1_xts_1_1_weekly_2 <- reactive({to.weekly(ab_ind_dat_9_sub_1_xts_1_2())})
#   ab_ind_dat_9_sub_1_xts_1_1_weekly_3 <- reactive({to.weekly(ab_ind_dat_9_sub_1_xts_1_3())})
#   ab_ind_dat_9_sub_1_xts_1_1_daily_1 <- reactive({to.daily(ab_ind_dat_9_sub_1_xts_1_1())})
#   ab_ind_dat_9_sub_1_xts_1_1_daily_2 <- reactive({to.daily(ab_ind_dat_9_sub_1_xts_1_2())})
#   ab_ind_dat_9_sub_1_xts_1_1_daily_3 <- reactive({to.daily(ab_ind_dat_9_sub_1_xts_1_3())})
#   ab_ind_dat_9_sub_1_xts_1_1_hourly_1 <- reactive({to.hourly(ab_ind_dat_9_sub_1_xts_1_1())})
#   ab_ind_dat_9_sub_1_xts_1_1_hourly_2 <- reactive({to.hourly(ab_ind_dat_9_sub_1_xts_1_2())})
#   ab_ind_dat_9_sub_1_xts_1_1_hourly_3 <- reactive({to.hourly(ab_ind_dat_9_sub_1_xts_1_3())})
#   
#   output$AB_ENE_9_YEARLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_yearly_1()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_yearly_1()) %in% c('ab_ind_dat_9_sub_1_xts_2_1().High'))], type = "line", name = "High MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_yearly_1()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_yearly_1()) %in% c('ab_ind_dat_9_sub_1_xts_2_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
#       hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_yearly_2()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_yearly_2()) %in% c('ab_ind_dat_9_sub_1_xts_2_2().High'))], type = "line", name = "High TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_yearly_2()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_yearly_2()) %in% c('ab_ind_dat_9_sub_1_xts_2_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
#       hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_yearly_3()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_yearly_3()) %in% c('ab_ind_dat_9_sub_1_xts_2_3().High'))], type = "line", name = "High DCR", color = "green") %>%
#       hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_yearly_3()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_yearly_3()) %in% c('ab_ind_dat_9_sub_1_xts_2_3().Low'))], type = "line", name = "Low DCR ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$AB_ENE_9_WEEKLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_weekly_1()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_weekly_1()) %in% c('ab_ind_dat_9_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_weekly_1()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_weekly_1()) %in% c('ab_ind_dat_9_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
#       hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_weekly_2()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_weekly_2()) %in% c('ab_ind_dat_9_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_weekly_2()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_weekly_2()) %in% c('ab_ind_dat_9_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
#       hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_weekly_3()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_weekly_3()) %in% c('ab_ind_dat_9_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
#       hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_weekly_3()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_weekly_3()) %in% c('ab_ind_dat_9_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$AB_ENE_9_DAILY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_daily_1()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_daily_1()) %in% c('ab_ind_dat_9_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_daily_1()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_daily_1()) %in% c('ab_ind_dat_9_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
#       hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_daily_2()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_daily_2()) %in% c('ab_ind_dat_9_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_daily_2()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_daily_2()) %in% c('ab_ind_dat_9_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
#       hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_daily_3()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_daily_3()) %in% c('ab_ind_dat_9_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
#       hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_daily_3()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_daily_3()) %in% c('ab_ind_dat_9_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$AB_ENE_9_HOURLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_hourly_1()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_hourly_1()) %in% c('ab_ind_dat_9_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_hourly_1()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_hourly_1()) %in% c('ab_ind_dat_9_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
#       hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_hourly_2()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_hourly_2()) %in% c('ab_ind_dat_9_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_hourly_2()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_hourly_2()) %in% c('ab_ind_dat_9_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
#       hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_hourly_3()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_hourly_3()) %in% c('ab_ind_dat_9_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
#       hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_hourly_3()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_hourly_3()) %in% c('ab_ind_dat_9_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$AB_ENE_9_ALL <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
#       hc_add_series(ab_ind_dat_9_sub_1_xts_1_1(), type = "line", name = "MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_9_sub_1_xts_1_2(), type = "line", name = "TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_9_sub_1_xts_1_3(), type = "line", name = "DCR", color = "green")
#   })
#   
#   ab_date_ind_10_1 <- reactive({paste(input$ab_dates_10[1],"00:00:00",sep = " ")})
#   ab_date_ind_10_2 <- reactive({paste(input$ab_dates_10[2],"00:00:00",sep = " ")})
#   ab_ind_dat_10_asset_filter <- reactive({input$ab_ind_10_ff})
#   
#   ab_ind_dat_10_sub_1 <- reactive({subset(ab_ind_dat_10,subset = (ab_ind_dat_10$Update_Time >= ab_date_ind_10_1() & ab_ind_dat_10$Update_Time <= ab_date_ind_10_2()))})
#   ab_ind_dat_10_sub_2 <- reactive({subset(ab_ind_dat_10_sub_1(),subset = (ab_ind_dat_10_sub_1()$ASSET == ab_ind_dat_10_asset_filter()))})
#   
#   ab_ind_dat_10_sub_1_xts_1_1 <- reactive({xts(ab_ind_dat_10_sub_2()$MC,ab_ind_dat_10_sub_2()$Update_Time)})
#   ab_ind_dat_10_sub_1_xts_1_2 <- reactive({xts(ab_ind_dat_10_sub_2()$TNG,ab_ind_dat_10_sub_2()$Update_Time)})
#   ab_ind_dat_10_sub_1_xts_1_3 <- reactive({xts(ab_ind_dat_10_sub_2()$DCR,ab_ind_dat_10_sub_2()$Update_Time)})
#   
#   ab_ind_dat_10_sub_1_xts_2_1 <- reactive({xts(ab_ind_dat_10_sub_1()$MC,ab_ind_dat_10_sub_1()$Update_Time)})
#   ab_ind_dat_10_sub_1_xts_2_2 <- reactive({xts(ab_ind_dat_10_sub_1()$TNG,ab_ind_dat_10_sub_1()$Update_Time)})
#   ab_ind_dat_10_sub_1_xts_2_3 <- reactive({xts(ab_ind_dat_10_sub_1()$DCR,ab_ind_dat_10_sub_1()$Update_Time)})
#   
#   ab_ind_dat_10_sub_1_xts_1_1_yearly_1 <- reactive({to.yearly(ab_ind_dat_10_sub_1_xts_2_1())})
#   ab_ind_dat_10_sub_1_xts_1_1_yearly_2 <- reactive({to.yearly(ab_ind_dat_10_sub_1_xts_2_2())})
#   ab_ind_dat_10_sub_1_xts_1_1_yearly_3 <- reactive({to.yearly(ab_ind_dat_10_sub_1_xts_2_3())})
#   ab_ind_dat_10_sub_1_xts_1_1_monthly_1 <- reactive({to.monthly(ab_ind_dat_10_sub_1_xts_1_1())})
#   ab_ind_dat_10_sub_1_xts_1_1_monthly_2 <- reactive({to.monthly(ab_ind_dat_10_sub_1_xts_1_2())})
#   ab_ind_dat_10_sub_1_xts_1_1_monthly_3 <- reactive({to.monthly(ab_ind_dat_10_sub_1_xts_1_3())})
#   ab_ind_dat_10_sub_1_xts_1_1_weekly_1 <- reactive({to.weekly(ab_ind_dat_10_sub_1_xts_1_1())})
#   ab_ind_dat_10_sub_1_xts_1_1_weekly_2 <- reactive({to.weekly(ab_ind_dat_10_sub_1_xts_1_2())})
#   ab_ind_dat_10_sub_1_xts_1_1_weekly_3 <- reactive({to.weekly(ab_ind_dat_10_sub_1_xts_1_3())})
#   ab_ind_dat_10_sub_1_xts_1_1_daily_1 <- reactive({to.daily(ab_ind_dat_10_sub_1_xts_1_1())})
#   ab_ind_dat_10_sub_1_xts_1_1_daily_2 <- reactive({to.daily(ab_ind_dat_10_sub_1_xts_1_2())})
#   ab_ind_dat_10_sub_1_xts_1_1_daily_3 <- reactive({to.daily(ab_ind_dat_10_sub_1_xts_1_3())})
#   ab_ind_dat_10_sub_1_xts_1_1_hourly_1 <- reactive({to.hourly(ab_ind_dat_10_sub_1_xts_1_1())})
#   ab_ind_dat_10_sub_1_xts_1_1_hourly_2 <- reactive({to.hourly(ab_ind_dat_10_sub_1_xts_1_2())})
#   ab_ind_dat_10_sub_1_xts_1_1_hourly_3 <- reactive({to.hourly(ab_ind_dat_10_sub_1_xts_1_3())})
#   
#   output$AB_ENE_10_YEARLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_yearly_1()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_yearly_1()) %in% c('ab_ind_dat_10_sub_1_xts_2_1().High'))], type = "line", name = "High MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_yearly_1()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_yearly_1()) %in% c('ab_ind_dat_10_sub_1_xts_2_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
#       hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_yearly_2()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_yearly_2()) %in% c('ab_ind_dat_10_sub_1_xts_2_2().High'))], type = "line", name = "High TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_yearly_2()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_yearly_2()) %in% c('ab_ind_dat_10_sub_1_xts_2_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
#       hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_yearly_3()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_yearly_3()) %in% c('ab_ind_dat_10_sub_1_xts_2_3().High'))], type = "line", name = "High DCR", color = "green") %>%
#       hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_yearly_3()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_yearly_3()) %in% c('ab_ind_dat_10_sub_1_xts_2_3().Low'))], type = "line", name = "Low DCR ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$AB_ENE_10_WEEKLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_weekly_1()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_weekly_1()) %in% c('ab_ind_dat_10_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_weekly_1()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_weekly_1()) %in% c('ab_ind_dat_10_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
#       hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_weekly_2()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_weekly_2()) %in% c('ab_ind_dat_10_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_weekly_2()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_weekly_2()) %in% c('ab_ind_dat_10_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
#       hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_weekly_3()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_weekly_3()) %in% c('ab_ind_dat_10_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
#       hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_weekly_3()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_weekly_3()) %in% c('ab_ind_dat_10_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$AB_ENE_10_DAILY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_daily_1()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_daily_1()) %in% c('ab_ind_dat_10_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_daily_1()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_daily_1()) %in% c('ab_ind_dat_10_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
#       hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_daily_2()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_daily_2()) %in% c('ab_ind_dat_10_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_daily_2()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_daily_2()) %in% c('ab_ind_dat_10_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
#       hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_daily_3()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_daily_3()) %in% c('ab_ind_dat_10_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
#       hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_daily_3()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_daily_3()) %in% c('ab_ind_dat_10_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$AB_ENE_10_HOURLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_hourly_1()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_hourly_1()) %in% c('ab_ind_dat_10_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_hourly_1()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_hourly_1()) %in% c('ab_ind_dat_10_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
#       hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_hourly_2()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_hourly_2()) %in% c('ab_ind_dat_10_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_hourly_2()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_hourly_2()) %in% c('ab_ind_dat_10_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
#       hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_hourly_3()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_hourly_3()) %in% c('ab_ind_dat_10_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
#       hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_hourly_3()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_hourly_3()) %in% c('ab_ind_dat_10_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$AB_ENE_10_ALL <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
#       hc_add_series(ab_ind_dat_10_sub_1_xts_1_1(), type = "line", name = "MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_10_sub_1_xts_1_2(), type = "line", name = "TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_10_sub_1_xts_1_3(), type = "line", name = "DCR", color = "green")
#   })
#   
#   ab_date_ind_11_1 <- reactive({paste(input$ab_dates_11[1],"00:00:00",sep = " ")})
#   ab_date_ind_11_2 <- reactive({paste(input$ab_dates_11[2],"00:00:00",sep = " ")})
#   ab_ind_dat_11_asset_filter <- reactive({input$ab_ind_11_ff})
#   
#   ab_ind_dat_11_sub_1 <- reactive({subset(ab_ind_dat_11,subset = (ab_ind_dat_11$Update_Time >= ab_date_ind_11_1() & ab_ind_dat_11$Update_Time <= ab_date_ind_11_2()))})
#   ab_ind_dat_11_sub_2 <- reactive({subset(ab_ind_dat_11_sub_1(),subset = (ab_ind_dat_11_sub_1()$ASSET == ab_ind_dat_11_asset_filter()))})
#   
#   ab_ind_dat_11_sub_1_xts_1_1 <- reactive({xts(ab_ind_dat_11_sub_2()$MC,ab_ind_dat_11_sub_2()$Update_Time)})
#   ab_ind_dat_11_sub_1_xts_1_2 <- reactive({xts(ab_ind_dat_11_sub_2()$TNG,ab_ind_dat_11_sub_2()$Update_Time)})
#   ab_ind_dat_11_sub_1_xts_1_3 <- reactive({xts(ab_ind_dat_11_sub_2()$DCR,ab_ind_dat_11_sub_2()$Update_Time)})
#   
#   ab_ind_dat_11_sub_1_xts_2_1 <- reactive({xts(ab_ind_dat_11_sub_1()$MC,ab_ind_dat_11_sub_1()$Update_Time)})
#   ab_ind_dat_11_sub_1_xts_2_2 <- reactive({xts(ab_ind_dat_11_sub_1()$TNG,ab_ind_dat_11_sub_1()$Update_Time)})
#   ab_ind_dat_11_sub_1_xts_2_3 <- reactive({xts(ab_ind_dat_11_sub_1()$DCR,ab_ind_dat_11_sub_1()$Update_Time)})
#   
#   ab_ind_dat_11_sub_1_xts_1_1_yearly_1 <- reactive({to.yearly(ab_ind_dat_11_sub_1_xts_2_1())})
#   ab_ind_dat_11_sub_1_xts_1_1_yearly_2 <- reactive({to.yearly(ab_ind_dat_11_sub_1_xts_2_2())})
#   ab_ind_dat_11_sub_1_xts_1_1_yearly_3 <- reactive({to.yearly(ab_ind_dat_11_sub_1_xts_2_3())})
#   ab_ind_dat_11_sub_1_xts_1_1_monthly_1 <- reactive({to.monthly(ab_ind_dat_11_sub_1_xts_1_1())})
#   ab_ind_dat_11_sub_1_xts_1_1_monthly_2 <- reactive({to.monthly(ab_ind_dat_11_sub_1_xts_1_2())})
#   ab_ind_dat_11_sub_1_xts_1_1_monthly_3 <- reactive({to.monthly(ab_ind_dat_11_sub_1_xts_1_3())})
#   ab_ind_dat_11_sub_1_xts_1_1_weekly_1 <- reactive({to.weekly(ab_ind_dat_11_sub_1_xts_1_1())})
#   ab_ind_dat_11_sub_1_xts_1_1_weekly_2 <- reactive({to.weekly(ab_ind_dat_11_sub_1_xts_1_2())})
#   ab_ind_dat_11_sub_1_xts_1_1_weekly_3 <- reactive({to.weekly(ab_ind_dat_11_sub_1_xts_1_3())})
#   ab_ind_dat_11_sub_1_xts_1_1_daily_1 <- reactive({to.daily(ab_ind_dat_11_sub_1_xts_1_1())})
#   ab_ind_dat_11_sub_1_xts_1_1_daily_2 <- reactive({to.daily(ab_ind_dat_11_sub_1_xts_1_2())})
#   ab_ind_dat_11_sub_1_xts_1_1_daily_3 <- reactive({to.daily(ab_ind_dat_11_sub_1_xts_1_3())})
#   ab_ind_dat_11_sub_1_xts_1_1_hourly_1 <- reactive({to.hourly(ab_ind_dat_11_sub_1_xts_1_1())})
#   ab_ind_dat_11_sub_1_xts_1_1_hourly_2 <- reactive({to.hourly(ab_ind_dat_11_sub_1_xts_1_2())})
#   ab_ind_dat_11_sub_1_xts_1_1_hourly_3 <- reactive({to.hourly(ab_ind_dat_11_sub_1_xts_1_3())})
#   
#   output$AB_ENE_11_YEARLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_yearly_1()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_yearly_1()) %in% c('ab_ind_dat_11_sub_1_xts_2_1().High'))], type = "line", name = "High MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_yearly_1()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_yearly_1()) %in% c('ab_ind_dat_11_sub_1_xts_2_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
#       hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_yearly_2()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_yearly_2()) %in% c('ab_ind_dat_11_sub_1_xts_2_2().High'))], type = "line", name = "High TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_yearly_2()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_yearly_2()) %in% c('ab_ind_dat_11_sub_1_xts_2_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
#       hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_yearly_3()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_yearly_3()) %in% c('ab_ind_dat_11_sub_1_xts_2_3().High'))], type = "line", name = "High DCR", color = "green") %>%
#       hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_yearly_3()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_yearly_3()) %in% c('ab_ind_dat_11_sub_1_xts_2_3().Low'))], type = "line", name = "Low DCR ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$AB_ENE_11_WEEKLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_weekly_1()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_weekly_1()) %in% c('ab_ind_dat_11_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_weekly_1()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_weekly_1()) %in% c('ab_ind_dat_11_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
#       hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_weekly_2()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_weekly_2()) %in% c('ab_ind_dat_11_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_weekly_2()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_weekly_2()) %in% c('ab_ind_dat_11_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
#       hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_weekly_3()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_weekly_3()) %in% c('ab_ind_dat_11_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
#       hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_weekly_3()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_weekly_3()) %in% c('ab_ind_dat_11_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$AB_ENE_11_DAILY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_daily_1()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_daily_1()) %in% c('ab_ind_dat_11_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_daily_1()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_daily_1()) %in% c('ab_ind_dat_11_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
#       hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_daily_2()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_daily_2()) %in% c('ab_ind_dat_11_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_daily_2()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_daily_2()) %in% c('ab_ind_dat_11_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
#       hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_daily_3()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_daily_3()) %in% c('ab_ind_dat_11_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
#       hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_daily_3()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_daily_3()) %in% c('ab_ind_dat_11_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$AB_ENE_11_HOURLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_hourly_1()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_hourly_1()) %in% c('ab_ind_dat_11_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_hourly_1()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_hourly_1()) %in% c('ab_ind_dat_11_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
#       hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_hourly_2()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_hourly_2()) %in% c('ab_ind_dat_11_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_hourly_2()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_hourly_2()) %in% c('ab_ind_dat_11_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
#       hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_hourly_3()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_hourly_3()) %in% c('ab_ind_dat_11_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
#       hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_hourly_3()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_hourly_3()) %in% c('ab_ind_dat_11_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$AB_ENE_11_ALL <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
#       hc_add_series(ab_ind_dat_11_sub_1_xts_1_1(), type = "line", name = "MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_11_sub_1_xts_1_2(), type = "line", name = "TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_11_sub_1_xts_1_3(), type = "line", name = "DCR", color = "green")
#   })
#   
#   ab_date_ind_12_1 <- reactive({paste(input$ab_dates_12[1],"00:00:00",sep = " ")})
#   ab_date_ind_12_2 <- reactive({paste(input$ab_dates_12[2],"00:00:00",sep = " ")})
#   ab_ind_dat_12_asset_filter <- reactive({input$ab_ind_12_ff})
#   
#   ab_ind_dat_12_sub_1 <- reactive({subset(ab_ind_dat_12,subset = (ab_ind_dat_12$Update_Time >= ab_date_ind_12_1() & ab_ind_dat_12$Update_Time <= ab_date_ind_12_2()))})
#   ab_ind_dat_12_sub_2 <- reactive({subset(ab_ind_dat_12_sub_1(),subset = (ab_ind_dat_12_sub_1()$ASSET == ab_ind_dat_12_asset_filter()))})
#   
#   ab_ind_dat_12_sub_1_xts_1_1 <- reactive({xts(ab_ind_dat_12_sub_2()$MC,ab_ind_dat_12_sub_2()$Update_Time)})
#   ab_ind_dat_12_sub_1_xts_1_2 <- reactive({xts(ab_ind_dat_12_sub_2()$TNG,ab_ind_dat_12_sub_2()$Update_Time)})
#   ab_ind_dat_12_sub_1_xts_1_3 <- reactive({xts(ab_ind_dat_12_sub_2()$DCR,ab_ind_dat_12_sub_2()$Update_Time)})
#   
#   ab_ind_dat_12_sub_1_xts_2_1 <- reactive({xts(ab_ind_dat_12_sub_1()$MC,ab_ind_dat_12_sub_1()$Update_Time)})
#   ab_ind_dat_12_sub_1_xts_2_2 <- reactive({xts(ab_ind_dat_12_sub_1()$TNG,ab_ind_dat_12_sub_1()$Update_Time)})
#   ab_ind_dat_12_sub_1_xts_2_3 <- reactive({xts(ab_ind_dat_12_sub_1()$DCR,ab_ind_dat_12_sub_1()$Update_Time)})
#   
#   ab_ind_dat_12_sub_1_xts_1_1_yearly_1 <- reactive({to.yearly(ab_ind_dat_12_sub_1_xts_2_1())})
#   ab_ind_dat_12_sub_1_xts_1_1_yearly_2 <- reactive({to.yearly(ab_ind_dat_12_sub_1_xts_2_2())})
#   ab_ind_dat_12_sub_1_xts_1_1_yearly_3 <- reactive({to.yearly(ab_ind_dat_12_sub_1_xts_2_3())})
#   ab_ind_dat_12_sub_1_xts_1_1_monthly_1 <- reactive({to.monthly(ab_ind_dat_12_sub_1_xts_1_1())})
#   ab_ind_dat_12_sub_1_xts_1_1_monthly_2 <- reactive({to.monthly(ab_ind_dat_12_sub_1_xts_1_2())})
#   ab_ind_dat_12_sub_1_xts_1_1_monthly_3 <- reactive({to.monthly(ab_ind_dat_12_sub_1_xts_1_3())})
#   ab_ind_dat_12_sub_1_xts_1_1_weekly_1 <- reactive({to.weekly(ab_ind_dat_12_sub_1_xts_1_1())})
#   ab_ind_dat_12_sub_1_xts_1_1_weekly_2 <- reactive({to.weekly(ab_ind_dat_12_sub_1_xts_1_2())})
#   ab_ind_dat_12_sub_1_xts_1_1_weekly_3 <- reactive({to.weekly(ab_ind_dat_12_sub_1_xts_1_3())})
#   ab_ind_dat_12_sub_1_xts_1_1_daily_1 <- reactive({to.daily(ab_ind_dat_12_sub_1_xts_1_1())})
#   ab_ind_dat_12_sub_1_xts_1_1_daily_2 <- reactive({to.daily(ab_ind_dat_12_sub_1_xts_1_2())})
#   ab_ind_dat_12_sub_1_xts_1_1_daily_3 <- reactive({to.daily(ab_ind_dat_12_sub_1_xts_1_3())})
#   ab_ind_dat_12_sub_1_xts_1_1_hourly_1 <- reactive({to.hourly(ab_ind_dat_12_sub_1_xts_1_1())})
#   ab_ind_dat_12_sub_1_xts_1_1_hourly_2 <- reactive({to.hourly(ab_ind_dat_12_sub_1_xts_1_2())})
#   ab_ind_dat_12_sub_1_xts_1_1_hourly_3 <- reactive({to.hourly(ab_ind_dat_12_sub_1_xts_1_3())})
#   
#   output$AB_ENE_12_YEARLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_yearly_1()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_yearly_1()) %in% c('ab_ind_dat_12_sub_1_xts_2_1().High'))], type = "line", name = "High MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_yearly_1()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_yearly_1()) %in% c('ab_ind_dat_12_sub_1_xts_2_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
#       hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_yearly_2()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_yearly_2()) %in% c('ab_ind_dat_12_sub_1_xts_2_2().High'))], type = "line", name = "High TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_yearly_2()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_yearly_2()) %in% c('ab_ind_dat_12_sub_1_xts_2_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
#       hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_yearly_3()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_yearly_3()) %in% c('ab_ind_dat_12_sub_1_xts_2_3().High'))], type = "line", name = "High DCR", color = "green") %>%
#       hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_yearly_3()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_yearly_3()) %in% c('ab_ind_dat_12_sub_1_xts_2_3().Low'))], type = "line", name = "Low DCR ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$AB_ENE_12_WEEKLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_weekly_1()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_weekly_1()) %in% c('ab_ind_dat_12_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_weekly_1()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_weekly_1()) %in% c('ab_ind_dat_12_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
#       hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_weekly_2()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_weekly_2()) %in% c('ab_ind_dat_12_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_weekly_2()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_weekly_2()) %in% c('ab_ind_dat_12_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
#       hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_weekly_3()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_weekly_3()) %in% c('ab_ind_dat_12_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
#       hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_weekly_3()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_weekly_3()) %in% c('ab_ind_dat_12_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$AB_ENE_12_DAILY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_daily_1()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_daily_1()) %in% c('ab_ind_dat_12_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_daily_1()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_daily_1()) %in% c('ab_ind_dat_12_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
#       hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_daily_2()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_daily_2()) %in% c('ab_ind_dat_12_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_daily_2()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_daily_2()) %in% c('ab_ind_dat_12_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
#       hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_daily_3()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_daily_3()) %in% c('ab_ind_dat_12_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
#       hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_daily_3()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_daily_3()) %in% c('ab_ind_dat_12_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$AB_ENE_12_HOURLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_hourly_1()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_hourly_1()) %in% c('ab_ind_dat_12_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_hourly_1()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_hourly_1()) %in% c('ab_ind_dat_12_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
#       hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_hourly_2()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_hourly_2()) %in% c('ab_ind_dat_12_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_hourly_2()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_hourly_2()) %in% c('ab_ind_dat_12_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
#       hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_hourly_3()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_hourly_3()) %in% c('ab_ind_dat_12_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
#       hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_hourly_3()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_hourly_3()) %in% c('ab_ind_dat_12_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$AB_ENE_12_ALL <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
#       hc_add_series(ab_ind_dat_12_sub_1_xts_1_1(), type = "line", name = "MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_12_sub_1_xts_1_2(), type = "line", name = "TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_12_sub_1_xts_1_3(), type = "line", name = "DCR", color = "green")
#   })
#   
#   ab_date_ind_13_1 <- reactive({paste(input$ab_dates_13[1],"00:00:00",sep = " ")})
#   ab_date_ind_13_2 <- reactive({paste(input$ab_dates_13[2],"00:00:00",sep = " ")})
#   ab_ind_dat_13_asset_filter <- reactive({input$ab_ind_13_ff})
#   
#   ab_ind_dat_13_sub_1 <- reactive({subset(ab_ind_dat_13,subset = (ab_ind_dat_13$Update_Time >= ab_date_ind_13_1() & ab_ind_dat_13$Update_Time <= ab_date_ind_13_2()))})
#   ab_ind_dat_13_sub_2 <- reactive({subset(ab_ind_dat_13_sub_1(),subset = (ab_ind_dat_13_sub_1()$ASSET == ab_ind_dat_13_asset_filter()))})
#   
#   ab_ind_dat_13_sub_1_xts_1_1 <- reactive({xts(ab_ind_dat_13_sub_2()$MC,ab_ind_dat_13_sub_2()$Update_Time)})
#   ab_ind_dat_13_sub_1_xts_1_2 <- reactive({xts(ab_ind_dat_13_sub_2()$TNG,ab_ind_dat_13_sub_2()$Update_Time)})
#   ab_ind_dat_13_sub_1_xts_1_3 <- reactive({xts(ab_ind_dat_13_sub_2()$DCR,ab_ind_dat_13_sub_2()$Update_Time)})
#   
#   ab_ind_dat_13_sub_1_xts_2_1 <- reactive({xts(ab_ind_dat_13_sub_1()$MC,ab_ind_dat_13_sub_1()$Update_Time)})
#   ab_ind_dat_13_sub_1_xts_2_2 <- reactive({xts(ab_ind_dat_13_sub_1()$TNG,ab_ind_dat_13_sub_1()$Update_Time)})
#   ab_ind_dat_13_sub_1_xts_2_3 <- reactive({xts(ab_ind_dat_13_sub_1()$DCR,ab_ind_dat_13_sub_1()$Update_Time)})
#   
#   ab_ind_dat_13_sub_1_xts_1_1_yearly_1 <- reactive({to.yearly(ab_ind_dat_13_sub_1_xts_2_1())})
#   ab_ind_dat_13_sub_1_xts_1_1_yearly_2 <- reactive({to.yearly(ab_ind_dat_13_sub_1_xts_2_2())})
#   ab_ind_dat_13_sub_1_xts_1_1_yearly_3 <- reactive({to.yearly(ab_ind_dat_13_sub_1_xts_2_3())})
#   ab_ind_dat_13_sub_1_xts_1_1_monthly_1 <- reactive({to.monthly(ab_ind_dat_13_sub_1_xts_1_1())})
#   ab_ind_dat_13_sub_1_xts_1_1_monthly_2 <- reactive({to.monthly(ab_ind_dat_13_sub_1_xts_1_2())})
#   ab_ind_dat_13_sub_1_xts_1_1_monthly_3 <- reactive({to.monthly(ab_ind_dat_13_sub_1_xts_1_3())})
#   ab_ind_dat_13_sub_1_xts_1_1_weekly_1 <- reactive({to.weekly(ab_ind_dat_13_sub_1_xts_1_1())})
#   ab_ind_dat_13_sub_1_xts_1_1_weekly_2 <- reactive({to.weekly(ab_ind_dat_13_sub_1_xts_1_2())})
#   ab_ind_dat_13_sub_1_xts_1_1_weekly_3 <- reactive({to.weekly(ab_ind_dat_13_sub_1_xts_1_3())})
#   ab_ind_dat_13_sub_1_xts_1_1_daily_1 <- reactive({to.daily(ab_ind_dat_13_sub_1_xts_1_1())})
#   ab_ind_dat_13_sub_1_xts_1_1_daily_2 <- reactive({to.daily(ab_ind_dat_13_sub_1_xts_1_2())})
#   ab_ind_dat_13_sub_1_xts_1_1_daily_3 <- reactive({to.daily(ab_ind_dat_13_sub_1_xts_1_3())})
#   ab_ind_dat_13_sub_1_xts_1_1_hourly_1 <- reactive({to.hourly(ab_ind_dat_13_sub_1_xts_1_1())})
#   ab_ind_dat_13_sub_1_xts_1_1_hourly_2 <- reactive({to.hourly(ab_ind_dat_13_sub_1_xts_1_2())})
#   ab_ind_dat_13_sub_1_xts_1_1_hourly_3 <- reactive({to.hourly(ab_ind_dat_13_sub_1_xts_1_3())})
#   
#   output$AB_ENE_13_YEARLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_yearly_1()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_yearly_1()) %in% c('ab_ind_dat_13_sub_1_xts_2_1().High'))], type = "line", name = "High MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_yearly_1()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_yearly_1()) %in% c('ab_ind_dat_13_sub_1_xts_2_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
#       hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_yearly_2()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_yearly_2()) %in% c('ab_ind_dat_13_sub_1_xts_2_2().High'))], type = "line", name = "High TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_yearly_2()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_yearly_2()) %in% c('ab_ind_dat_13_sub_1_xts_2_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
#       hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_yearly_3()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_yearly_3()) %in% c('ab_ind_dat_13_sub_1_xts_2_3().High'))], type = "line", name = "High DCR", color = "green") %>%
#       hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_yearly_3()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_yearly_3()) %in% c('ab_ind_dat_13_sub_1_xts_2_3().Low'))], type = "line", name = "Low DCR ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$AB_ENE_13_WEEKLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_weekly_1()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_weekly_1()) %in% c('ab_ind_dat_13_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_weekly_1()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_weekly_1()) %in% c('ab_ind_dat_13_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
#       hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_weekly_2()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_weekly_2()) %in% c('ab_ind_dat_13_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_weekly_2()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_weekly_2()) %in% c('ab_ind_dat_13_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
#       hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_weekly_3()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_weekly_3()) %in% c('ab_ind_dat_13_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
#       hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_weekly_3()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_weekly_3()) %in% c('ab_ind_dat_13_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$AB_ENE_13_DAILY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_daily_1()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_daily_1()) %in% c('ab_ind_dat_13_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_daily_1()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_daily_1()) %in% c('ab_ind_dat_13_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
#       hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_daily_2()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_daily_2()) %in% c('ab_ind_dat_13_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_daily_2()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_daily_2()) %in% c('ab_ind_dat_13_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
#       hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_daily_3()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_daily_3()) %in% c('ab_ind_dat_13_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
#       hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_daily_3()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_daily_3()) %in% c('ab_ind_dat_13_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$AB_ENE_13_HOURLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_hourly_1()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_hourly_1()) %in% c('ab_ind_dat_13_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_hourly_1()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_hourly_1()) %in% c('ab_ind_dat_13_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
#       hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_hourly_2()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_hourly_2()) %in% c('ab_ind_dat_13_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_hourly_2()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_hourly_2()) %in% c('ab_ind_dat_13_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
#       hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_hourly_3()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_hourly_3()) %in% c('ab_ind_dat_13_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
#       hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_hourly_3()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_hourly_3()) %in% c('ab_ind_dat_13_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$AB_ENE_13_ALL <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
#       hc_add_series(ab_ind_dat_13_sub_1_xts_1_1(), type = "line", name = "MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_13_sub_1_xts_1_2(), type = "line", name = "TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_13_sub_1_xts_1_3(), type = "line", name = "DCR", color = "green")
#   })
#   
#   ab_date_ind_14_1 <- reactive({paste(input$ab_dates_14[1],"00:00:00",sep = " ")})
#   ab_date_ind_14_2 <- reactive({paste(input$ab_dates_14[2],"00:00:00",sep = " ")})
#   ab_ind_dat_14_asset_filter <- reactive({input$ab_ind_14_ff})
#   
#   ab_ind_dat_14_sub_1 <- reactive({subset(ab_ind_dat_14,subset = (ab_ind_dat_14$Update_Time >= ab_date_ind_14_1() & ab_ind_dat_14$Update_Time <= ab_date_ind_14_2()))})
#   ab_ind_dat_14_sub_2 <- reactive({subset(ab_ind_dat_14_sub_1(),subset = (ab_ind_dat_14_sub_1()$ASSET == ab_ind_dat_14_asset_filter()))})
#   
#   ab_ind_dat_14_sub_1_xts_1_1 <- reactive({xts(ab_ind_dat_14_sub_2()$MC,ab_ind_dat_14_sub_2()$Update_Time)})
#   ab_ind_dat_14_sub_1_xts_1_2 <- reactive({xts(ab_ind_dat_14_sub_2()$TNG,ab_ind_dat_14_sub_2()$Update_Time)})
#   ab_ind_dat_14_sub_1_xts_1_3 <- reactive({xts(ab_ind_dat_14_sub_2()$DCR,ab_ind_dat_14_sub_2()$Update_Time)})
#   
#   ab_ind_dat_14_sub_1_xts_2_1 <- reactive({xts(ab_ind_dat_14_sub_1()$MC,ab_ind_dat_14_sub_1()$Update_Time)})
#   ab_ind_dat_14_sub_1_xts_2_2 <- reactive({xts(ab_ind_dat_14_sub_1()$TNG,ab_ind_dat_14_sub_1()$Update_Time)})
#   ab_ind_dat_14_sub_1_xts_2_3 <- reactive({xts(ab_ind_dat_14_sub_1()$DCR,ab_ind_dat_14_sub_1()$Update_Time)})
#   
#   ab_ind_dat_14_sub_1_xts_1_1_yearly_1 <- reactive({to.yearly(ab_ind_dat_14_sub_1_xts_2_1())})
#   ab_ind_dat_14_sub_1_xts_1_1_yearly_2 <- reactive({to.yearly(ab_ind_dat_14_sub_1_xts_2_2())})
#   ab_ind_dat_14_sub_1_xts_1_1_yearly_3 <- reactive({to.yearly(ab_ind_dat_14_sub_1_xts_2_3())})
#   ab_ind_dat_14_sub_1_xts_1_1_monthly_1 <- reactive({to.monthly(ab_ind_dat_14_sub_1_xts_1_1())})
#   ab_ind_dat_14_sub_1_xts_1_1_monthly_2 <- reactive({to.monthly(ab_ind_dat_14_sub_1_xts_1_2())})
#   ab_ind_dat_14_sub_1_xts_1_1_monthly_3 <- reactive({to.monthly(ab_ind_dat_14_sub_1_xts_1_3())})
#   ab_ind_dat_14_sub_1_xts_1_1_weekly_1 <- reactive({to.weekly(ab_ind_dat_14_sub_1_xts_1_1())})
#   ab_ind_dat_14_sub_1_xts_1_1_weekly_2 <- reactive({to.weekly(ab_ind_dat_14_sub_1_xts_1_2())})
#   ab_ind_dat_14_sub_1_xts_1_1_weekly_3 <- reactive({to.weekly(ab_ind_dat_14_sub_1_xts_1_3())})
#   ab_ind_dat_14_sub_1_xts_1_1_daily_1 <- reactive({to.daily(ab_ind_dat_14_sub_1_xts_1_1())})
#   ab_ind_dat_14_sub_1_xts_1_1_daily_2 <- reactive({to.daily(ab_ind_dat_14_sub_1_xts_1_2())})
#   ab_ind_dat_14_sub_1_xts_1_1_daily_3 <- reactive({to.daily(ab_ind_dat_14_sub_1_xts_1_3())})
#   ab_ind_dat_14_sub_1_xts_1_1_hourly_1 <- reactive({to.hourly(ab_ind_dat_14_sub_1_xts_1_1())})
#   ab_ind_dat_14_sub_1_xts_1_1_hourly_2 <- reactive({to.hourly(ab_ind_dat_14_sub_1_xts_1_2())})
#   ab_ind_dat_14_sub_1_xts_1_1_hourly_3 <- reactive({to.hourly(ab_ind_dat_14_sub_1_xts_1_3())})
#   
#   output$AB_ENE_14_YEARLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_yearly_1()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_yearly_1()) %in% c('ab_ind_dat_14_sub_1_xts_2_1().High'))], type = "line", name = "High MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_yearly_1()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_yearly_1()) %in% c('ab_ind_dat_14_sub_1_xts_2_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
#       hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_yearly_2()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_yearly_2()) %in% c('ab_ind_dat_14_sub_1_xts_2_2().High'))], type = "line", name = "High TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_yearly_2()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_yearly_2()) %in% c('ab_ind_dat_14_sub_1_xts_2_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
#       hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_yearly_3()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_yearly_3()) %in% c('ab_ind_dat_14_sub_1_xts_2_3().High'))], type = "line", name = "High DCR", color = "green") %>%
#       hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_yearly_3()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_yearly_3()) %in% c('ab_ind_dat_14_sub_1_xts_2_3().Low'))], type = "line", name = "Low DCR ", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$AB_ENE_14_WEEKLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_weekly_1()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_weekly_1()) %in% c('ab_ind_dat_14_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_weekly_1()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_weekly_1()) %in% c('ab_ind_dat_14_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
#       hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_weekly_2()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_weekly_2()) %in% c('ab_ind_dat_14_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_weekly_2()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_weekly_2()) %in% c('ab_ind_dat_14_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
#       hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_weekly_3()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_weekly_3()) %in% c('ab_ind_dat_14_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
#       hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_weekly_3()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_weekly_3()) %in% c('ab_ind_dat_14_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$AB_ENE_14_DAILY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_daily_1()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_daily_1()) %in% c('ab_ind_dat_14_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_daily_1()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_daily_1()) %in% c('ab_ind_dat_14_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
#       hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_daily_2()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_daily_2()) %in% c('ab_ind_dat_14_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_daily_2()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_daily_2()) %in% c('ab_ind_dat_14_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
#       hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_daily_3()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_daily_3()) %in% c('ab_ind_dat_14_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
#       hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_daily_3()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_daily_3()) %in% c('ab_ind_dat_14_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$AB_ENE_14_HOURLY <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% 
#       hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_hourly_1()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_hourly_1()) %in% c('ab_ind_dat_14_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_hourly_1()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_hourly_1()) %in% c('ab_ind_dat_14_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
#       hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_hourly_2()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_hourly_2()) %in% c('ab_ind_dat_14_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_hourly_2()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_hourly_2()) %in% c('ab_ind_dat_14_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
#       hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_hourly_3()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_hourly_3()) %in% c('ab_ind_dat_14_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
#       hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_hourly_3()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_hourly_3()) %in% c('ab_ind_dat_14_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
#       hc_navigator(enabled = TRUE)})
#   output$AB_ENE_14_ALL <- renderHighchart({
#     highchart() %>% 
#       hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
#       hc_add_series(ab_ind_dat_14_sub_1_xts_1_1(), type = "line", name = "MC", color = "green") %>%
#       hc_add_series(ab_ind_dat_14_sub_1_xts_1_2(), type = "line", name = "TNG", color = "green") %>%
#       hc_add_series(ab_ind_dat_14_sub_1_xts_1_3(), type = "line", name = "DCR", color = "green")
#   })
#   
  
  data_dict <- read_xlsx("www/data_dictionay.xlsx", sheet = config$dd_sheet)
  data_dict_df <- as.data.frame(data_dict)
  output$DATA_DICTIONARY_TABLE <- renderDataTable({data_dict_df})
  
  data_dict_nb <- read_xlsx("www/data_dictionary.xlsx",sheet = "New_Brunswick")
  data_dict_df_nb <- as.data.frame(data_dict_nb)
  output$DATA_DICTIONARY_NB <- renderDataTable({data_dict_df_nb})
  
  
    
    output$SERVER_STATUS <- renderUI({
      tags$table(
        id="server_stat",
        style = "width:100%",
        tags$tr(
          tags$th("Province"),
          tags$th("Status-API"),
          tags$th("Status-Source")
        ),
        tags$tr(
          tags$td("Ontario"),
          tags$td(on_api_stat()),
          tags$td(on_src_stat())
        ),
        tags$tr(
          tags$td("Prince Edward Island"),
          tags$td(pei_api_stat()),
          tags$td(pei_src_stat())
        ),
        tags$tr(
          tags$td("Nova Scotia"),
          tags$td(ns_api_stat()),
          tags$td(ns_src_stat())
        ),
        tags$tr(
          tags$td("New Brunswick"),
          tags$td(nb_api_stat()),
          tags$td(nb_src_stat())
        ),
        tags$tr(
          tags$td("British Colombia"),
          tags$td(bc_api_stat()),
          tags$td(bc_src_stat())
        ),
        tags$tr(
          tags$td("Newfoundland & Labrador"),
          tags$td(bc_api_stat()),
          tags$td(bc_src_stat())
        ),
        tags$tr(
          tags$td("Alberta"),
          tags$td("ok")
        ),
        tags$tr(
          tags$td("Quebec"),
          tags$td(qb_api_stat()),
          tags$td(qb_src_stat())
        )
      )
  })
  
  
  
  #Individual Province Visualization
  
  #Button Function
  
  observeEvent(input$button_pei,{
    updateTabsetPanel(session = session, inputId = "rted_menu", selected = "pei")
  })
  
  observeEvent(input$button_NS,{
    updateTabsetPanel(session = session, inputId = "rted_menu", selected = "NS")
  })
  
  observeEvent(input$button_NB,{
    updateTabsetPanel(session = session, inputId = "rted_menu", selected = "NB")
  })
  
  observeEvent(input$button_ON,{
    updateTabsetPanel(session = session, inputId = "rted_menu", selected = "ON")
  })
  
  observeEvent(input$button_AB,{
    updateTabsetPanel(session = session, inputId = "rted_menu", selected = "AB")
  })
  
  observeEvent(input$button_BC,{
    updateTabsetPanel(session = session, inputId = "rted_menu", selected = "BC")
  })
  
  observeEvent(input$button_NFL,{
    updateTabsetPanel(session = session, inputId = "rted_menu", selected = "NFL")
  })
  
  observeEvent(input$button_QB,{
    updateTabsetPanel(session = session, inputId = "rted_menu", selected = "QB")
  })
  
  # observeEvent(input$button_pei_ind_1,{
  #   createAlert(session, "NB_1", content = paste("please select this table:",config$provinces$NB$table1, sep = " "), style = "info", dismiss = TRUE)
  #   Sys.sleep(0.5)
  #   updateTabsetPanel(session = session, inputId = "rted_menu", selected = "Dwn") 
  # })
  
  
  #Language button Control Start
  observeEvent(input$Btn_EN,{
    update_lang(session, "en")
  })
  observeEvent(input$Btn_EN_pei,{
    update_lang(session, "en")
  })
  observeEvent(input$Btn_EN_ns,{
    update_lang(session, "en")
  })
  observeEvent(input$Btn_EN_nb,{
    update_lang(session, "en")
  })
  observeEvent(input$Btn_EN_nfl,{
    update_lang(session, "en")
  })
  observeEvent(input$Btn_EN_on,{
    update_lang(session, "en")
  })
  observeEvent(input$Btn_EN_ab,{
    update_lang(session, "en")
  })
  observeEvent(input$Btn_EN_bc,{
    update_lang(session, "en")
  })
  observeEvent(input$Btn_EN_qb,{
    update_lang(session, "en")
  })
  
  observeEvent(input$Btn_FR,{
    update_lang(session, "fr")
  })
  observeEvent(input$Btn_FR_pei,{
    update_lang(session, "fr")
  })
  observeEvent(input$Btn_FR_ns,{
    update_lang(session, "fr")
  })
  observeEvent(input$Btn_FR_nb,{
    update_lang(session, "fr")
  })
  observeEvent(input$Btn_FR_nfl,{
    update_lang(session, "fr")
  })
  observeEvent(input$Btn_FR_on,{
    update_lang(session, "fr")
  })
  observeEvent(input$Btn_FR_ab,{
    update_lang(session, "fr")
  })
  observeEvent(input$Btn_FR_bc,{
    update_lang(session, "fr")
  })
  observeEvent(input$Btn_FR_qb,{
    update_lang(session, "fr")
  })
  #language control button end
  
  
  # File Download System
  #observeEvent(input$rted_menu, {
    #if(input$rted_menu == "Dwn")
     #{
    #   updateDateRangeInput(session, "nb_date_range",
    #                        start = tail(nb_ind_dat_load$DATETIME_LOCAL,1),
    #                        min = tail(nb_ind_dat_load$DATETIME_LOCAL,1),
    #                        end = head(nb_ind_dat_load$DATETIME_LOCAL,1),
    #                        max = head(nb_ind_dat_load$DATETIME_LOCAL,1))
    #   updateDateRangeInput(session, "ns_date_range",
    #                        start = head(ns_ind_dat$Date_time_local,1),
    #                        min = head(ns_ind_dat$Date_time_local,1),
    #                        end = tail(ns_ind_dat$Date_time_local,1),
    #                        max = tail(ns_ind_dat$Date_time_local,1))
    #   updateDateRangeInput(session, "ns_date_range_1",
    #                        start = head(ns_ind_dat_1$Date_time_local,1),
    #                        min = head(ns_ind_dat_1$Date_time_local,1),
    #                        end = tail(ns_ind_dat_1$Date_time_local,1),
    #                        max = tail(ns_ind_dat_1$Date_time_local,1))
    #   updateDateRangeInput(session, "ab_date_range",
    #                        start = head(nbload_data()$Date_time_local,1),
    #                        min = head(nbload_data()$Date_time_local,1),
    #                        end = tail(nbload_data()$Date_time_local,1),
    #                        max = tail(nbload_data()$Date_time_local,1))
    #   updateDateRangeInput(session, "bc_date_range",
    #                        start = head(bc_ind_dat$Date_time_local,1),
    #                        min = head(bc_ind_dat$Date_time_local,1),
    #                        end = tail(bc_ind_dat$Date_time_local,1),
    #                        max = tail(bc_ind_dat$Date_time_local,1))
    #   updateDateRangeInput(session, "bc_date_range_1",
    #                        start = head(bc_ind_dat_1$date_time_local,1),
    #                        min = head(bc_ind_dat_1$date_time_local,1),
    #                        end = tail(bc_ind_dat_1$date_time_local,1),
    #                        max = tail(bc_ind_dat_1$date_time_local,1))
    #   
    #   updateDateRangeInput(session, "on_date_range",
    #                        start = head(on_ind_dat$date_time_local,1),
    #                        min = head(on_ind_dat$date_time_local,1),
    #                        end = tail(on_ind_dat$date_time_local,1),
    #                        max = tail(on_ind_dat$date_time_local,1))
    #   updateDateRangeInput(session, "pei_date_range",
    #                        start = tail(pei_ind_dat_load$DATETIME_LOCAL,1),
    #                        min = tail(pei_ind_dat_load$DATETIME_LOCAL,1),
    #                        end = head(pei_ind_dat_load$DATETIME_LOCAL,1),
    #                        max = head(pei_ind_dat_load$DATETIME_LOCAL,1))
    #   updateDateRangeInput(session, "nfl_date_range",
    #                        start = head(nfl_ind_dat$Date_time_local,1),
    #                        min = head(nfl_ind_dat$Date_time_local,1),
    #                        end = tail(nfl_ind_dat$Date_time_local,1),
    #                        max = tail(nfl_ind_dat$Date_time_local,1))
    #   updateDateRangeInput(session, "qb_date_range",
    #                        start = head(qb_ind_dat_1$Date_time_local,1),
    #                        min = head(qb_ind_dat_1$Date_time_local,1),
    #                        end = tail(qb_ind_dat_1$Date_time_local,1),
    #                        max = tail(qb_ind_dat_1$Date_time_local,1))
    #   updateDateRangeInput(session, "qb_date_range_1",
    #                        start = head(qb_ind_dat_2$Date_time_local,1),
    #                        min = head(qb_ind_dat_2$Date_time_local,1),
    #                        end = tail(qb_ind_dat_2$Date_time_local,1),
    #                        max = tail(qb_ind_dat_2$Date_time_local,1))
    #   updateDateRangeInput(session, "qb_date_range_2",
    #                        start = head(qb_ind_dat_3$Date_time_local,1),
    #                        min = head(qb_ind_dat_3$Date_time_local,1),
    #                        end = tail(qb_ind_dat_3$Date_time_local,1),
    #                        max = tail(qb_ind_dat_3$Date_time_local,1))
    #   
    #   updateSelectInput(session,"ab_ind_9_ff", choices = unique(ab_ind_dat_9$ASSET))
    #   
    #   
    
    #}
    #   if(!is.null(input$ns_filter))
    #   {
    #     shinyjs::show("ns_dwn_1")
    #     observeEvent(input$ns_filter,{
    #       output$data_ns_1 <- downloadHandler(
    #         
    #         filename = function() {
    #           paste(config$provinces$NS$table1,"csv",sep = ".")
    #         },
    #         content = function(file)
    #         {
    #           showNotification("Your File is Downloading, Please wait for some time.", type="message")
    #           date_1 <- paste(input$ns_date_range[1],"00:00:00",sep = " ")
    #           date_2 <- paste(input$ns_date_range[2],"00:00:00",sep = " ")
    #           nsload_subset_download <- subset(ns_ind_dat,subset = (ns_ind_dat$Date_time_local >= date_1 & ns_ind_dat$Date_time_local <= date_2))
    #           write.csv(nsload_subset_download, file, row.names = FALSE)
    #         }
    #       )
    #     })
    #     
    #   }
    #   if(!is.null(input$ns_filter_1))
    #   {
    #     shinyjs::show("ns_dwn_1")
    #     observeEvent(input$ns_filter_1,{
    #       output$data_ns_2 <- downloadHandler(
    #         
    #         filename = function() {
    #           paste(config$provinces$NS$table2,"csv",sep = ".")
    #         },
    #         content = function(file)
    #         {
    #           showNotification("Your File is Downloading, Please wait for some time.", type="message")
    #           date_1 <- paste(input$ns_date_range_1[1],"00:00:00",sep = " ")
    #           date_2 <- paste(input$ns_date_range_1[2],"00:00:00",sep = " ")
    #           nsload_subset_download_1 <- subset(ns_ind_dat_1,subset = (ns_ind_dat_1$Date_time_local >= date_1 & ns_ind_dat_1$Date_time_local <= date_2))
    #           write.csv(nsload_subset_download_1, file, row.names = FALSE)
    #         }
    #       )
    #     })
    #     
    #   }
    #   if(!is.null(input$bc_filter))
    #   {
    #     shinyjs::show("ns_dwn_1")
    #     observeEvent(input$bc_filter,{
    #       output$data_bc_1 <- downloadHandler(
    #         
    #         filename = function() {
    #           paste(config$provinces$BC$table1,"csv",sep = ".")
    #         },
    #         content = function(file)
    #         {
    #           showNotification("Your File is Downloading, Please wait for some time.", type="message")
    #           date_1 <- paste(input$bc_date_range[1],"00:00:00",sep = " ")
    #           date_2 <- paste(input$bc_date_range[2],"00:00:00",sep = " ")
    #           bcload_subset_download <- subset(bc_ind_dat,subset = (bc_ind_dat$Date_time_local >= date_1 & bc_ind_dat$Date_time_local <= date_2))
    #           write.csv(bcload_subset_download, file, row.names = FALSE)
    #         }
    #       )
    #     })
    #     
    #   }
    #   if(!is.null(input$bc_filter_1))
    #   {
    #     shinyjs::show("ns_dwn_1")
    #     observeEvent(input$bc_filter_1,{
    #       output$data_bc_2 <- downloadHandler(
    #         
    #         filename = function() {
    #           paste(config$provinces$BC$table3,"csv",sep = ".")
    #         },
    #         content = function(file)
    #         {
    #           showNotification("Your File is Downloading, Please wait for some time.", type="message")
    #           date_1 <- paste(input$bc_date_range_1[1],"00:00:00",sep = " ")
    #           date_2 <- paste(input$bc_date_range_1[2],"00:00:00",sep = " ")
    #           bcload_subset_download_1 <- subset(bc_ind_dat_1,subset = (bc_ind_dat_1$date_time_local >= date_1 & bc_ind_dat_1$date_time_local <= date_2))
    #           write.csv(bcload_subset_download_1, file, row.names = FALSE)
    #         }
    #       )
    #     })
    #     
    #   }
    #   
    #   if(!is.null(input$pei_filter))
    #   {
    #     shinyjs::show("ns_dwn_1")
    #     observeEvent(input$pei_filter,{
    #       output$data_pei_1 <- downloadHandler(
    #         
    #         filename = function() {
    #           paste("PEI-TABLE","csv",sep = ".")
    #         },
    #         content = function(file)
    #         {
    #           showNotification("Your File is Downloading, Please wait for some time.", type="message")
    #           date_1 <- paste(input$pei_date_range[1],"00:00:00",sep = " ")
    #           date_2 <- paste(input$pei_date_range[2],"00:00:00",sep = " ")
    #           peiload_subset_download <- subset(pei_ind_dat_load,subset = (pei_ind_dat_load$DATETIME_LOCAL >= date_1 & pei_ind_dat_load$DATETIME_LOCAL <= date_2))
    #           write.csv(peiload_subset_download, file, row.names = FALSE)
    #         }
    #       )
    #     })
    #     
    #   }
    #   if(!is.null(input$nfl_filter))
    #   {
    #     shinyjs::show("ns_dwn_1")
    #     observeEvent(input$nfl_filter,{
    #       output$data_nfl <- downloadHandler(
    #         
    #         filename = function() {
    #           paste(config$provinces$NFL$table1,"csv",sep = ".")
    #         },
    #         content = function(file)
    #         {
    #           showNotification("Your File is Downloading, Please wait for some time.", type="message")
    #           date_1 <- paste(input$nfl_date_range[1],"00:00:00",sep = " ")
    #           date_2 <- paste(input$nfl_date_range[2],"00:00:00",sep = " ")
    #           nflload_subset_download <- subset(nfl_ind_dat,subset = (nfl_ind_dat$Date_time_local >= date_1 & nfl_ind_dat$Date_time_local <= date_2))
    #           write.csv(nflload_subset_download, file, row.names = FALSE)
    #         }
    #       )
    #     })
    #     
    #   }
    #   if(!is.null(input$qb_filter))
    #   {
    #     shinyjs::show("ns_dwn_1")
    #     observeEvent(input$qb_filter,{
    #       output$data_qb_1 <- downloadHandler(
    #         
    #         filename = function() {
    #           paste(config$provinces$QUEBEC$table1,"csv",sep = ".")
    #         },
    #         content = function(file)
    #         {
    #           showNotification("Your File is Downloading, Please wait for some time.", type="message")
    #           date_1 <- paste(input$qb_date_range[1],"00:00:00",sep = " ")
    #           date_2 <- paste(input$qb_date_range[2],"00:00:00",sep = " ")
    #           qbload_subset_download <- subset(qb_ind_dat_1,subset = (qb_ind_dat_1$Date_time_local >= date_1 & qb_ind_dat_1$Date_time_local <= date_2))
    #           write.csv(qbload_subset_download, file, row.names = FALSE)
    #         }
    #       )
    #     })
    #     
    #   }
    #   if(!is.null(input$qb_filter_1))
    #   {
    #     shinyjs::show("ns_dwn_1")
    #     observeEvent(input$qb_filter_1,{
    #       output$data_qb_2 <- downloadHandler(
    #         
    #         filename = function() {
    #           paste(config$provinces$QUEBEC$table2,"csv",sep = ".")
    #         },
    #         content = function(file)
    #         {
    #           showNotification("Your File is Downloading, Please wait for some time.", type="message")
    #           date_1 <- paste(input$qb_date_range_1[1],"00:00:00",sep = " ")
    #           date_2 <- paste(input$qb_date_range_1[2],"00:00:00",sep = " ")
    #           qbload_subset_download_1 <- subset(qb_ind_dat_2,subset = (qb_ind_dat_2$Date_time_local >= date_1 & qb_ind_dat_2$Date_time_local <= date_2))
    #           write.csv(qbload_subset_download_1, file, row.names = FALSE)
    #         }
    #       )
    #     })
    #     
    #   }
    #   if(!is.null(input$qb_filter_2))
    #   {
    #     shinyjs::show("ns_dwn_1")
    #     observeEvent(input$qb_filter_2,{
    #       output$data_qb_3 <- downloadHandler(
    #         
    #         filename = function() {
    #           paste(config$provinces$QUEBEC$table3,"csv",sep = ".")
    #         },
    #         content = function(file)
    #         {
    #           showNotification("Your File is Downloading, Please wait for some time.", type="message")
    #           date_1 <- paste(input$qb_date_range_2[1],"00:00:00",sep = " ")
    #           date_2 <- paste(input$qb_date_range_2[2],"00:00:00",sep = " ")
    #           qbload_subset_download_2 <- subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= date_1 & qb_ind_dat_3$Date_time_local <= date_2))
    #           write.csv(qbload_subset_download_2, file, row.names = FALSE)
    #         }
    #       )
    #     })
    #     
    #   }
    # }
    #file download system end
    
    
    
    # if(input$rted_menu == "AB"){
    #   ab_dd_8_1 <- as.Date(tail(ab_ind_dat_8$Update_Time,1))-(7)
    #   ab_dd_8_1_1 <- paste(ab_dd_8_1,"00:00:00",sep=" ")
    #   ab_dd_8_2_2 <- as.Date(tail(ab_ind_dat_8$Update_Time,1))
    #   updateSliderInput(session, "ab_dates_8",
    #                     min = as.Date(head(ab_ind_dat_8$Update_Time,1),"%Y-%m-%d"),
    #                     max = as.Date(tail(ab_ind_dat_8$Update_Time,1),"%Y-%m-%d"),
    #                     value=c(as.Date(ab_dd_8_1_1),ab_dd_8_2_2),
    #                     timeFormat="%Y-%m-%d")
    #   updateSelectInput(session,"ab_ind_8_ff", choices = unique(ab_ind_dat_8$ASSET))
    #   ab_dd_9_1 <- as.Date(tail(ab_ind_dat_9$Update_Time,1))-(7)
    #   ab_dd_9_1_1 <- paste(ab_dd_9_1,"00:00:00",sep=" ")
    #   ab_dd_9_2_2 <- as.Date(tail(ab_ind_dat_9$Update_Time,1))
    #   updateSliderInput(session, "ab_dates_9",
    #                     min = as.Date(head(ab_ind_dat_9$Update_Time,1),"%Y-%m-%d"),
    #                     max = as.Date(tail(ab_ind_dat_9$Update_Time,1),"%Y-%m-%d"),
    #                     value=c(as.Date(ab_dd_9_1_1),ab_dd_9_2_2),
    #                     timeFormat="%Y-%m-%d")
    #   updateSelectInput(session,"ab_ind_9_ff", choices = unique(ab_ind_dat_9$ASSET))
    #   ab_dd_10_1 <- as.Date(tail(ab_ind_dat_10$Update_Time,1))-(7)
    #   ab_dd_10_1_1 <- paste(ab_dd_10_1,"00:00:00",sep=" ")
    #   ab_dd_10_2_2 <- as.Date(tail(ab_ind_dat_10$Update_Time,1))
    #   updateSliderInput(session, "ab_dates_10",
    #                     min = as.Date(head(ab_ind_dat_10$Update_Time,1),"%Y-%m-%d"),
    #                     max = as.Date(tail(ab_ind_dat_10$Update_Time,1),"%Y-%m-%d"),
    #                     value=c(as.Date(ab_dd_10_1_1),ab_dd_10_2_2),
    #                     timeFormat="%Y-%m-%d")
    #   updateSelectInput(session,"ab_ind_10_ff", choices = unique(ab_ind_dat_10$ASSET))
    #   ab_dd_11_1 <- as.Date(tail(ab_ind_dat_11$Update_Time,1))-(7)
    #   ab_dd_11_1_1 <- paste(ab_dd_11_1,"00:00:00",sep=" ")
    #   ab_dd_11_2_2 <- as.Date(tail(ab_ind_dat_11$Update_Time,1))
    #   updateSliderInput(session, "ab_dates_11",
    #                     min = as.Date(head(ab_ind_dat_11$Update_Time,1),"%Y-%m-%d"),
    #                     max = as.Date(tail(ab_ind_dat_11$Update_Time,1),"%Y-%m-%d"),
    #                     value=c(as.Date(ab_dd_11_1_1),ab_dd_11_2_2),
    #                     timeFormat="%Y-%m-%d")
    #   updateSelectInput(session,"ab_ind_11_ff", choices = unique(ab_ind_dat_11$ASSET))
    #   ab_dd_12_1 <- as.Date(tail(ab_ind_dat_12$Update_Time,1))-(7)
    #   ab_dd_12_1_1 <- paste(ab_dd_12_1,"00:00:00",sep=" ")
    #   ab_dd_12_2_2 <- as.Date(tail(ab_ind_dat_12$Update_Time,1))
    #   updateSliderInput(session, "ab_dates_12",
    #                     min = as.Date(head(ab_ind_dat_12$Update_Time,1),"%Y-%m-%d"),
    #                     max = as.Date(tail(ab_ind_dat_12$Update_Time,1),"%Y-%m-%d"),
    #                     value=c(as.Date(ab_dd_12_1_1),ab_dd_12_2_2),
    #                     timeFormat="%Y-%m-%d")
    #   updateSelectInput(session,"ab_ind_12_ff", choices = unique(ab_ind_dat_12$ASSET))
    #   ab_dd_13_1 <- as.Date(tail(ab_ind_dat_13$Update_Time,1))-(7)
    #   ab_dd_13_1_1 <- paste(ab_dd_13_1,"00:00:00",sep=" ")
    #   ab_dd_13_2_2 <- as.Date(tail(ab_ind_dat_13$Update_Time,1))
    #   updateSliderInput(session, "ab_dates_13",
    #                     min = as.Date(head(ab_ind_dat_13$Update_Time,1),"%Y-%m-%d"),
    #                     max = as.Date(tail(ab_ind_dat_13$Update_Time,1),"%Y-%m-%d"),
    #                     value=c(as.Date(ab_dd_13_1_1),ab_dd_13_2_2),
    #                     timeFormat="%Y-%m-%d")
    #   updateSelectInput(session,"ab_ind_13_ff", choices = unique(ab_ind_dat_13$ASSET))
    #   ab_dd_14_1 <- as.Date(tail(ab_ind_dat_14$Update_Time,1))-(7)
    #   ab_dd_14_1_1 <- paste(ab_dd_14_1,"00:00:00",sep=" ")
    #   ab_dd_14_2_2 <- as.Date(tail(ab_ind_dat_14$Update_Time,1))
    #   updateSliderInput(session, "ab_dates_14",
    #                     min = as.Date(head(ab_ind_dat_14$Update_Time,1),"%Y-%m-%d"),
    #                     max = as.Date(tail(ab_ind_dat_14$Update_Time,1),"%Y-%m-%d"),
    #                     value=c(as.Date(ab_dd_14_1_1),ab_dd_14_2_2),
    #                     timeFormat="%Y-%m-%d")
    #   updateSelectInput(session,"ab_ind_14_ff", choices = unique(ab_ind_dat_14$ASSET))
    # }
    # if(input$rted_menu == "QB"){
    #   qb_dd_1 <- as.Date(Sys.Date())-(7)
    #   qb_dd_1_1 <- paste(qb_dd_1,"00:00:00",sep=" ")
    #   qb_dd_2_2 <- as.Date(tail(qb_ind_dat_1$Date_time_local,1))
    #   qb_dd_3_3 <- as.Date(tail(qb_ind_dat_3$Date_time_local,1))
    #   qb_dd_4_4 <- as.Date(tail(qb_ind_dat_3$Date_time_local,1))-(7)
    #   qb_dd_5_5 <- as.Date(tail(qb_ind_dat_2$Date_Time_UTC,1))
    #   updateSliderInput(session, "qb_dates_1",
    #                     min = as.Date(head(qb_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
    #                     max = as.Date(tail(qb_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
    #                     value=c(as.Date(qb_dd_1_1),qb_dd_2_2),
    #                     timeFormat="%Y-%m-%d")
    #   updateSliderInput(session, "qb_dates_2",
    #                     min = as.Date(head(qb_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
    #                     max = as.Date(tail(qb_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
    #                     value=c(as.Date(qb_dd_1_1),qb_dd_2_2),
    #                     timeFormat="%Y-%m-%d")
    #   updateSliderInput(session, "qb_dates_3",
    #                     min = as.Date(head(qb_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
    #                     max = as.Date(tail(qb_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
    #                     value=c(as.Date(qb_dd_1_1),qb_dd_2_2),
    #                     timeFormat="%Y-%m-%d")
    #   updateSliderInput(session, "qb_dates_4",
    #                     min = as.Date(head(qb_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
    #                     max = as.Date(tail(qb_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
    #                     value=c(as.Date(qb_dd_1_1),qb_dd_2_2),
    #                     timeFormat="%Y-%m-%d")
    #   updateSliderInput(session, "qb_dates_5",
    #                     min = as.Date(head(qb_ind_dat_3$Date_time_local,1),"%Y-%m-%d"),
    #                     max = as.Date(tail(qb_ind_dat_3$Date_time_local,1),"%Y-%m-%d"),
    #                     value=c(as.Date(qb_dd_4_4),qb_dd_3_3),
    #                     timeFormat="%Y-%m-%d")
    #   updateSliderInput(session, "qb_dates_6",
    #                     min = as.Date(head(qb_ind_dat_3$Date_time_local,1),"%Y-%m-%d"),
    #                     max = as.Date(tail(qb_ind_dat_3$Date_time_local,1),"%Y-%m-%d"),
    #                     value=c(as.Date(qb_dd_4_4),qb_dd_3_3),
    #                     timeFormat="%Y-%m-%d")
    #   updateSliderInput(session, "qb_dates_7",
    #                     min = as.Date(head(qb_ind_dat_3$Date_time_local,1),"%Y-%m-%d"),
    #                     max = as.Date(tail(qb_ind_dat_3$Date_time_local,1),"%Y-%m-%d"),
    #                     value=c(as.Date(qb_dd_4_4),qb_dd_3_3),
    #                     timeFormat="%Y-%m-%d")
    #   updateSliderInput(session, "qb_dates_8",
    #                     min = as.Date(head(qb_ind_dat_3$Date_time_local,1),"%Y-%m-%d"),
    #                     max = as.Date(tail(qb_ind_dat_3$Date_time_local,1),"%Y-%m-%d"),
    #                     value=c(as.Date(qb_dd_4_4),qb_dd_3_3),
    #                     timeFormat="%Y-%m-%d")
    #   updateSliderInput(session, "qb_dates_9",
    #                     min = as.Date(head(qb_ind_dat_3$Date_time_local,1),"%Y-%m-%d"),
    #                     max = as.Date(tail(qb_ind_dat_3$Date_time_local,1),"%Y-%m-%d"),
    #                     value=c(as.Date(qb_dd_4_4),qb_dd_3_3),
    #                     timeFormat="%Y-%m-%d")
    #   updateSliderInput(session, "qb_dates_10",
    #                     min = as.Date(head(qb_ind_dat_3$Date_time_local,1),"%Y-%m-%d"),
    #                     max = as.Date(tail(qb_ind_dat_3$Date_time_local,1),"%Y-%m-%d"),
    #                     value=c(as.Date(qb_dd_4_4),qb_dd_3_3),
    #                     timeFormat="%Y-%m-%d")
    #   updateSliderInput(session, "qb_dates_11",
    #                     min = as.Date(head(qb_ind_dat_3$Date_time_local,1),"%Y-%m-%d"),
    #                     max = as.Date(tail(qb_ind_dat_3$Date_time_local,1),"%Y-%m-%d"),
    #                     value=c(as.Date(qb_dd_4_4),qb_dd_3_3),
    #                     timeFormat="%Y-%m-%d")
    #   updateSliderInput(session, "qb_dates_12",
    #                     min = as.Date(head(qb_ind_dat_2$Date_time_local,1),"%Y-%m-%d"),
    #                     max = as.Date(tail(qb_ind_dat_2$Date_time_local,1),"%Y-%m-%d"),
    #                     value=c(as.Date(qb_dd_1_1),qb_dd_5_5),
    #                     timeFormat="%Y-%m-%d")
    #   
    # }
    
  }




# Run the application 
shinyApp(ui = ui, server = server)
