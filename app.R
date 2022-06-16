#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(DBI)
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
library(readxl)
library(RODBC)
library(tidyquant)
library(googlesheets4)
library(googledrive)
library(shiny.i18n)
library(reactlog)
library(rsdmx)
library(httr)
library(RCurl)
library(curl)
library(future)
library(furrr)
library(XML)


n_cores <- availableCores()-1
plan(multicore, workers = n_cores)

i18n <- Translator$new(translation_json_path = "www/translation_fr.json")
i18n$set_translation_language("en")


dashboard_ui_slider_date_end <- Sys.Date()
dashboard_ui_slider_date_start <- paste(Sys.Date()-7,"00:00:00",sep=" ")


# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "CCEI-RTED DashBoard",dropdownMenu(type = "messages", icon = icon("cog"), headerText = NULL, badgeStatus = NULL)),
  dashboardSidebar(
    usei18n(i18n),
    sidebarMenu( id = "rted_menu",
                 menuItem(i18n$t("Dashboard"), tabName = "dashboard", icon = icon("chart-line")),
                 menuItem(i18n$t("Prince Edward Island"),tabName = "pei", icon = icon("chart-bar")),
                 menuItem(i18n$t("Nova Scotia"),tabName = "NS", icon = icon("chart-bar")),
                 menuItem(i18n$t("New Brunswick"),tabName = "NB", icon = icon("chart-bar")),
                 menuItem(i18n$t("Newfoundland & Labrador"),tabName = "NFL", icon = icon("chart-bar")),
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
    tags$head(HTML("<link rel='icon' type='image/x-icon' href='www/statslogo.png'>")),
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
color: red;
}

.green_output{
color: green;
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





                                    ")),
    tabItems(
      tabItem(tabName = "dashboard",
              fluidPage( id = "dashboard_page",
                         fluidRow(div(h2(id = "header_title",i18n$t("High Frequency Electricity Data Dashboard HFED - Pilot Project")),
                                      div(id = "lang_btn",
                                          bsButton("Btn_EN","English",icon = icon("language"),style = "primary",size = "small",type = "action"),
                                          bsButton("Btn_FR","French",icon = icon("language"),style = "primary",size = "small",type = "action")))),
                         fluidRow(h4(textOutput("timer"))),
                         br(),
                         fluidRow(h4(i18n$t("The Canadian Center for Energy Information (CCEI) presents a dashboard gathering publicly-available high-frequency electricity data in Canada."),
                                     i18n$t("Information is collected by web scraping. Data visualizations and datasets are updated as soon as new data points become available on provincial utility websites."), 
                                     i18n$t("This new source of information was developed by the CCEI with the support of many partners namely National Resource Canada (NRCan), the Canada Energy Regulator (CER) and Statistics Canada."),
                                     i18n$t("It aims at providing simplified access to timely (almost real-time) and detailed information (hourly granularity) on electricity and supporting key analysis on energy."))),
                         br(),
                         fluidRow(box(title = HTML(paste("  <i id='bd_twr' class='fas fa-broadcast-tower'></i>",i18n$t("Live Data"))),id="live_dashboard", status = "primary", width="100%" ,solidHeader = TRUE,
                                      fluidRow(
                                        box(
                                          title = fluidRow(id = "dash_title",
                                                           fluidRow(column(width = 8, h4(i18n$t("Prince Edward Island"))),column(width = 12, id="peimean",uiOutput("PEI_MEAN")))),
                                          width = 4, status = "success", solidHeader = TRUE,
                                          collapsible = TRUE,
                                          column(
                                            fluidRow(withLoader(highchartOutput("PEI_load"), type = "html", loader = "loader3")),
                                            fluidRow(bsButton("button_pei","For More Detailed Information, Click Here", icon = icon("chart-bar"), style = "primary", block = TRUE)),width = 12)
                                        ),
                                        box(
                                          title = fluidRow(id = "dash_title",
                                                           fluidRow(column(width = 8, h4(i18n$t("Nova Scotia"))),column(width = 12, id="nsmean",uiOutput("NS_MEAN")))),width = 4, status = "success", solidHeader = TRUE,
                                          collapsible = TRUE,
                                          column(
                                            fluidRow(withLoader(highchartOutput("NS_load"), type = "html", loader = "loader3")),
                                            fluidRow(bsButton("button_NS","For More Detailed Information, Click Here", icon = icon("chart-bar"), style = "primary", block = TRUE)),width = 12)),
                                        box(
                                          title = fluidRow(id = "dash_title",
                                                           fluidRow(column(width = 8, h4(i18n$t("New Brunswick"))),column(width = 12, id="nbmean",uiOutput("NB_MEAN")))),width = 4, status = "success", solidHeader = TRUE,
                                          collapsible = TRUE,
                                          column(
                                            fluidRow(withLoader(highchartOutput("NB_load"), type = "html", loader = "loader3")),
                                            fluidRow(bsButton("button_NB","For More Detailed Information, Click Here", icon = icon("chart-bar"), style = "primary", block = TRUE)),width = 12)),
                                      ),
                                      fluidRow(
                                        box(
                                          title = fluidRow(id = "dash_title",
                                                           fluidRow(column(width = 8, h4(i18n$t("Ontario"))),column(width = 12, id="onmean",uiOutput("ON_MEAN")))),width = 4, status = "success", solidHeader = TRUE,
                                          collapsible = TRUE,
                                          column(
                                            fluidRow(withLoader(highchartOutput("ON_load"), type = "html", loader = "loader3")),
                                            fluidRow(bsButton("button_ON","For More Detailed Information, Click Here", icon = icon("chart-bar"), style = "primary", block = TRUE)),width = 12)),
                                        box(
                                          title = fluidRow(id = "dash_title",
                                                           fluidRow(column(width = 8, h4(i18n$t("Alberta"))),column(width = 12, id="abmean",uiOutput("AB_MEAN")))),width = 4, status = "success", solidHeader = TRUE,
                                          collapsible = TRUE,
                                          column(
                                            fluidRow(withLoader(highchartOutput("AB_load"), type = "html", loader = "loader3")),
                                            fluidRow(bsButton("button_AB","For More Detailed Information, Click Here", icon = icon("chart-bar"), style = "primary", block = TRUE)),width = 12)),
                                        box(
                                          title = fluidRow(id = "dash_title",
                                                           fluidRow(column(width = 8, h4(i18n$t("British Columbia"))),column(width = 12, id="bcmean",uiOutput("BC_MEAN")))),width = 4, status = "success", solidHeader = TRUE,
                                          collapsible = TRUE,
                                          column(
                                            fluidRow(withLoader(highchartOutput("BC_load"), type = "html", loader = "loader3")),
                                            fluidRow(bsButton("button_BC","For More Detailed Information, Click Here", icon = icon("chart-bar"), style = "primary", block = TRUE)),width = 12)),
                                      ),
                                      fluidRow(
                                        box(
                                          title = fluidRow(id = "dash_title",
                                                           fluidRow(column(width = 8, h4(i18n$t("Newfoundland & Labrador"))),column(width =12, id="nflmean",uiOutput("NFL_MEAN")))),width = 6, status = "success", solidHeader = TRUE,
                                          collapsible = TRUE,
                                          column(
                                            fluidRow(withLoader(highchartOutput("NFL_load"), type = "html", loader = "loader3")),
                                            fluidRow(bsButton("button_NFL","For More Detailed Information, Click Here", icon = icon("chart-bar"), style = "primary", block = TRUE)),width = 12)),
                                        box(
                                          title = fluidRow(id = "dash_title",
                                                           fluidRow(column(width = 8, h4(i18n$t("Quebec"))),column(width = 12, id="qbmean",uiOutput("QB_MEAN")))),width = 6, status = "success", solidHeader = TRUE,
                                          collapsible = TRUE,
                                          column(
                                            fluidRow(withLoader(highchartOutput("QB_load"), type = "html", loader = "loader3")),
                                            fluidRow(bsButton("button_QB","For More Detailed Information, Click Here", icon = icon("chart-bar"), style = "primary", block = TRUE)),width = 12)),
                                      ))))),
      
      tabItem(tabName = "pei",
              fluidPage(id = "dashboard_page",
                        fluidRow(div(h2(id = "header_title",i18n$t("Prince Edward Island")),
                                     div(id = "lang_btn",
                                         bsButton("Btn_EN_pei","English",icon = icon("language"),style = "primary",size = "small",type = "action"),
                                         bsButton("Btn_FR_pei","French",icon = icon("language"),style = "primary",size = "small",type = "action")))),
                        fluidRow(h4(textOutput("timer_pei"))),
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
                                                     fluidRow(withLoader(highchartOutput("PEI_ON_LOAD_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_pei == 6",
                                                     fluidRow(withLoader(highchartOutput("PEI_ON_LOAD_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_pei == 3",
                                                     fluidRow(withLoader(highchartOutput("PEI_ON_LOAD_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_pei == 4",
                                                     fluidRow(withLoader(highchartOutput("PEI_ON_LOAD_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_pei == 5",
                                                     fluidRow(withLoader(highchartOutput("PEI_ON_LOAD_HOURLY"),type = "html", loader = "loader3"))),
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
                                                     fluidRow(withLoader(highchartOutput("PEI_ON_WIND_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_pei_2 == 6",
                                                     fluidRow(withLoader(highchartOutput("PEI_ON_WIND_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_pei_2 == 3",
                                                     fluidRow(withLoader(highchartOutput("PEI_ON_WIND_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_pei_2 == 4",
                                                     fluidRow(withLoader(highchartOutput("PEI_ON_WIND_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_pei_2 == 5",
                                                     fluidRow(withLoader(highchartOutput("PEI_ON_WIND_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Import & Export", width = 10, status = "warning", solidHeader = TRUE,
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
                                                     fluidRow(withLoader(highchartOutput("PEI_EXPORT_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_pei_3 == 6",
                                                     fluidRow(withLoader(highchartOutput("PEI_EXPORT_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_pei_3 == 3",
                                                     fluidRow(withLoader(highchartOutput("PEI_EXPORT_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_pei_3 == 4",
                                                     fluidRow(withLoader(highchartOutput("PEI_EXPORT_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_pei_3 == 5",
                                                     fluidRow(withLoader(highchartOutput("PEI_EXPORT_HOURLY"),type = "html", loader = "loader3"))),
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
                                                     fluidRow(withLoader(highchartOutput("PEI_LOCAL_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_pei_4 == 6",
                                                     fluidRow(withLoader(highchartOutput("PEI_LOCAL_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_pei_4 == 3",
                                                     fluidRow(withLoader(highchartOutput("PEI_LOCAL_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_pei_4 == 4",
                                                     fluidRow(withLoader(highchartOutput("PEI_LOCAL_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_pei_4 == 5",
                                                     fluidRow(withLoader(highchartOutput("PEI_LOCAL_HOURLY"),type = "html", loader = "loader3"))),
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
                        fluidRow(h4(textOutput("timer_ns"))),
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
                                                     fluidRow(withLoader(highchartOutput("NS_LOAD_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns == 6",
                                                     fluidRow(withLoader(highchartOutput("NS_LOAD_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns == 3",
                                                     fluidRow(withLoader(highchartOutput("NS_LOAD_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns == 4",
                                                     fluidRow(withLoader(highchartOutput("NS_LOAD_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns == 5",
                                                     fluidRow(withLoader(highchartOutput("NS_LOAD_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Export & Import - I", width = 10, status = "warning", solidHeader = TRUE,
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
                                                     fluidRow(withLoader(highchartOutput("NS_EXP_1_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns_1 == 6",
                                                     fluidRow(withLoader(highchartOutput("NS_EXP_1_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns_1 == 3",
                                                     fluidRow(withLoader(highchartOutput("NS_EXP_1_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns_1 == 4",
                                                     fluidRow(withLoader(highchartOutput("NS_EXP_1_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns_1 == 5",
                                                     fluidRow(withLoader(highchartOutput("NS_EXP_1_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Export & Import - II", width = 10, status = "warning", solidHeader = TRUE,
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
                                                     fluidRow(withLoader(highchartOutput("NS_EXP_2_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns_2 == 6",
                                                     fluidRow(withLoader(highchartOutput("NS_EXP_2_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns_2 == 3",
                                                     fluidRow(withLoader(highchartOutput("NS_EXP_2_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns_2 == 4",
                                                     fluidRow(withLoader(highchartOutput("NS_EXP_2_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns_2 == 5",
                                                     fluidRow(withLoader(highchartOutput("NS_EXP_2_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Export & Import - III", width = 10, status = "warning", solidHeader = TRUE,
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
                                                     fluidRow(withLoader(highchartOutput("NS_EXP_3_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns_3 == 6",
                                                     fluidRow(withLoader(highchartOutput("NS_EXP_3_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns_3 == 3",
                                                     fluidRow(withLoader(highchartOutput("NS_EXP_3_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns_3 == 4",
                                                     fluidRow(withLoader(highchartOutput("NS_EXP_3_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns_3 == 5",
                                                     fluidRow(withLoader(highchartOutput("NS_EXP_3_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Export & Import - IV", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_ns_4", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "5 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_ns_4 != 1",sliderInput("ns_dates_5",
                                                                                                                                                 "Dates",
                                                                                                                                                 min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                 max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                 value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                 timeFormat="%Y-%m-%d")))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_ns_4 == 1",
                                                     fluidRow(withLoader(highchartOutput("NS_EXP_4_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns_4 == 6",
                                                     fluidRow(withLoader(highchartOutput("NS_EXP_4_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns_4 == 3",
                                                     fluidRow(withLoader(highchartOutput("NS_EXP_4_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns_4 == 4",
                                                     fluidRow(withLoader(highchartOutput("NS_EXP_4_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_ns_4 == 5",
                                                     fluidRow(withLoader(highchartOutput("NS_EXP_4_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 )))
              )),
      tabItem(tabName = "NB",
              fluidPage(id = "dashboard_page",
                        fluidRow(div(h2(id = "header_title",i18n$t("New Brunswick")),
                                     div(id = "lang_btn",
                                         bsButton("Btn_EN_nb","English",icon = icon("language"),style = "primary",size = "small",type = "action"),
                                         bsButton("Btn_FR_nb","French",icon = icon("language"),style = "primary",size = "small",type = "action")))),
                        fluidRow(h4(textOutput("timer_nb"))),
                        br(),
                        tabBox( id = "tabnb", height = "100%", width = "100%",side = "right", 
                                tabPanel(h4("Visualization"),
                                         fluidRow(
                                           column(width =10, offset = 2,
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
                                                                      fluidRow(withLoader(highchartOutput("NB_LOAD_YEARLY"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb == 6",
                                                                      fluidRow(withLoader(highchartOutput("NB_LOAD_ALL"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb == 3",
                                                                      fluidRow(withLoader(highchartOutput("NB_LOAD_WEEKLY"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb == 4",
                                                                      fluidRow(withLoader(highchartOutput("NB_LOAD_DAILY"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb == 5",
                                                                      fluidRow(withLoader(highchartOutput("NB_LOAD_HOURLY"),type = "html", loader = "loader3"))),
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
                                                                      fluidRow(withLoader(highchartOutput("NB_DEMAND_YEARLY"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb_2 == 6",
                                                                      fluidRow(withLoader(highchartOutput("NB_DEMAND_ALL"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb_2 == 3",
                                                                      fluidRow(withLoader(highchartOutput("NB_DEMAND_WEEKLY"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb_2 == 4",
                                                                      fluidRow(withLoader(highchartOutput("NB_DEMAND_DAILY"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb_2 == 5",
                                                                      fluidRow(withLoader(highchartOutput("NB_DEMAND_HOURLY"),type = "html", loader = "loader3"))),
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
                                                                      fluidRow(withLoader(highchartOutput("NB_RESERVE_YEARLY"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb_3 == 6",
                                                                      fluidRow(withLoader(highchartOutput("NB_RESERVE_ALL"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb_3 == 3",
                                                                      fluidRow(withLoader(highchartOutput("NB_RESERVE_WEEKLY"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb_3 == 4",
                                                                      fluidRow(withLoader(highchartOutput("NB_RESERVE_DAILY"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb_3 == 5",
                                                                      fluidRow(withLoader(highchartOutput("NB_RESERVE_HOURLY"),type = "html", loader = "loader3"))),
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
                                                                      fluidRow(withLoader(highchartOutput("NB_RESERVE2_YEARLY"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb_4 == 6",
                                                                      fluidRow(withLoader(highchartOutput("NB_RESERVE2_ALL"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb_4 == 3",
                                                                      fluidRow(withLoader(highchartOutput("NB_RESERVE2_WEEKLY"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb_4 == 4",
                                                                      fluidRow(withLoader(highchartOutput("NB_RESERVE2_DAILY"),type = "html", loader = "loader3"))),
                                                    conditionalPanel( condition = "input.select_fr_nb_4 == 5",
                                                                      fluidRow(withLoader(highchartOutput("NB_RESERVE2_HOURLY"),type = "html", loader = "loader3"))),
                                                    fluidRow(
                                                      column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                                      column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                                    )
                                                  )))),
                                tabPanel(h4("Legends"),dataTableOutput("DATA_DICTIONARY_NB"))
                        )
                        
              )),
      tabItem(tabName = "ON",
              fluidPage(id = "dashboard_page",
                        fluidRow(div(h2(id = "header_title",i18n$t("Ontario")),
                                     div(id = "lang_btn",
                                         bsButton("Btn_EN_on","English",icon = icon("language"),style = "primary",size = "small",type = "action"),
                                         bsButton("Btn_FR_on","French",icon = icon("language"),style = "primary",size = "small",type = "action")))),
                        fluidRow(h4(textOutput("timer_on"))),
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
                                                     fluidRow(withLoader(highchartOutput("ON_TOT_ENERGY_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_on1 == 6",
                                                     fluidRow(withLoader(highchartOutput("ON_TOT_ENERGY_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_on1 == 3",
                                                     fluidRow(withLoader(highchartOutput("ON_TOT_ENERGY_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_on1 == 4",
                                                     fluidRow(withLoader(highchartOutput("ON_TOT_ENERGY_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_on1 == 5",
                                                     fluidRow(withLoader(highchartOutput("ON_TOT_ENERGY_HOURLY"),type = "html", loader = "loader3"))),
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
                                                     fluidRow(withLoader(highchartOutput("ON_TOT_LOSS_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_on2 == 6",
                                                     fluidRow(withLoader(highchartOutput("ON_TOT_LOSS_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_on2 == 3",
                                                     fluidRow(withLoader(highchartOutput("ON_TOT_LOSS_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_on2 == 4",
                                                     fluidRow(withLoader(highchartOutput("ON_TOT_LOSS_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_on2 == 5",
                                                     fluidRow(withLoader(highchartOutput("ON_TOT_LOSS_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "Total Load", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_on3", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_on3 != 1",sliderInput("on_dates_3",
                                                                                                                                                "Dates",
                                                                                                                                                min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                timeFormat="%Y-%m-%d")))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_on3 == 1",
                                                     fluidRow(withLoader(highchartOutput("ON_TOT_LOAD_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_on3 == 6",
                                                     fluidRow(withLoader(highchartOutput("ON_TOT_LOAD_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_on3 == 3",
                                                     fluidRow(withLoader(highchartOutput("ON_TOT_LOAD_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_on3 == 4",
                                                     fluidRow(withLoader(highchartOutput("ON_TOT_LOAD_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_on3 == 5",
                                                     fluidRow(withLoader(highchartOutput("ON_TOT_LOAD_HOURLY"),type = "html", loader = "loader3"))),
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
                                   fluidRow(column(width = 2, selectInput("select_fr_on4", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_on4 != 1",sliderInput("on_dates_4",
                                                                                                                                                "Dates",
                                                                                                                                                min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                timeFormat="%Y-%m-%d")))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_on4 == 1",
                                                     fluidRow(withLoader(highchartOutput("ON_TOT_DEMAND_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_on4 == 6",
                                                     fluidRow(withLoader(highchartOutput("ON_TOT_DEMAND_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_on4 == 3",
                                                     fluidRow(withLoader(highchartOutput("ON_TOT_DEMAND_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_on4 == 4",
                                                     fluidRow(withLoader(highchartOutput("ON_TOT_DEMAND_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_on4 == 5",
                                                     fluidRow(withLoader(highchartOutput("ON_TOT_DEMAND_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 )))
              )),
      tabItem(tabName = "NFL",
              fluidPage(id = "dashboard_page",
                        fluidRow(div(h2(id = "header_title",i18n$t("Newfoundland & Labrador")),
                                     div(id = "lang_btn",
                                         bsButton("Btn_EN_nfl","English",icon = icon("language"),style = "primary",size = "small",type = "action"),
                                         bsButton("Btn_FR_nfl","French",icon = icon("language"),style = "primary",size = "small",type = "action")))),
                        fluidRow(h4(textOutput("timer_nfl"))),
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
                                                     fluidRow(withLoader(highchartOutput("NFL_LOAD_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_nfl == 6",
                                                     fluidRow(withLoader(highchartOutput("NFL_LOAD_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_nfl == 3",
                                                     fluidRow(withLoader(highchartOutput("NFL_LOAD_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_nfl == 4",
                                                     fluidRow(withLoader(highchartOutput("NFL_LOAD_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_nfl == 5",
                                                     fluidRow(withLoader(highchartOutput("NFL_LOAD_HOURLY"),type = "html", loader = "loader3"))),
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
                        fluidRow(h4(textOutput("timer_qb"))),
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
                        fluidRow(h4(textOutput("timer_ab"))),
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
                                   title = "Balancing_Authority_Load (BAL)", width = 10, status = "warning", solidHeader = TRUE,
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
                                                     fluidRow(withLoader(highchartOutput("BC_LOAD_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_bc_1 == 6",
                                                     fluidRow(withLoader(highchartOutput("BC_LOAD_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_bc_1 == 3",
                                                     fluidRow(withLoader(highchartOutput("BC_LOAD_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_bc_1 == 4",
                                                     fluidRow(withLoader(highchartOutput("BC_LOAD_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_bc_1 == 5",
                                                     fluidRow(withLoader(highchartOutput("BC_LOAD_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "BC - US Exchange", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_bc_2", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_bc_2 != 1",sliderInput("bc_dates_2",
                                                                                                                                                 "Dates",
                                                                                                                                                 min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                 max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                 value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                 timeFormat="%Y-%m-%d")))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_bc_2 == 1",
                                                     fluidRow(withLoader(highchartOutput("BC_US_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_bc_2 == 6",
                                                     fluidRow(withLoader(highchartOutput("BC_US_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_bc_2 == 3",
                                                     fluidRow(withLoader(highchartOutput("BC_US_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_bc_2 == 4",
                                                     fluidRow(withLoader(highchartOutput("BC_US_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_bc_2 == 5",
                                                     fluidRow(withLoader(highchartOutput("BC_US_HOURLY"),type = "html", loader = "loader3"))),
                                   fluidRow(
                                     column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                                     column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                                   )
                                 ))),
                        fluidRow(
                          column(width =10, offset = 2,
                                 box(
                                   title = "BC - AB Exchange", width = 10, status = "warning", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   fluidRow(column(width = 2, selectInput("select_fr_bc_3", h4("Frequency"), 
                                                                          choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                            column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_bc_3 != 1",sliderInput("bc_dates_3",
                                                                                                                                                 "Dates",
                                                                                                                                                 min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                                 max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                                 value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                                 timeFormat="%Y-%m-%d")))
                                   ),
                                   conditionalPanel( condition = "input.select_fr_bc_3 == 1",
                                                     fluidRow(withLoader(highchartOutput("BC_AB_YEARLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_bc_3 == 6",
                                                     fluidRow(withLoader(highchartOutput("BC_AB_ALL"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_bc_3 == 3",
                                                     fluidRow(withLoader(highchartOutput("BC_AB_WEEKLY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_bc_3 == 4",
                                                     fluidRow(withLoader(highchartOutput("BC_AB_DAILY"),type = "html", loader = "loader3"))),
                                   conditionalPanel( condition = "input.select_fr_bc_3 == 5",
                                                     fluidRow(withLoader(highchartOutput("BC_AB_HOURLY"),type = "html", loader = "loader3"))),
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
              fluidRow(
                box(
                  title = "New Brunswick",status = "success", solidHeader = TRUE,collapsible = TRUE,
                  fluidRow(
                    column(width = 6,
                           dateRangeInput("nb_date_range", h5("Date range")),
                           actionButton("nb_filter", "Apply")
                    ),
                    column(width = 4,
                           radioButtons("radio_nb", h3("Tables"),
                                        choices = list("None" = 0, "nb_realtime_alldata_new_brunswick_utc" = 1),selected = 0),
                           conditionalPanel( condition = "input.radio_nb == 1",
                                             fluidRow(id="nb_dwn_1",downloadButton("data_nb", "Download Data"))),
                           conditionalPanel( condition = "input.radio_nb == 0",
                                             helpText("Note: Please Select a File to Download"))
                    )),
                  fluidRow(id = "dwn1", column(bsAlert("NB_1"), width = 12)),
                  fluidRow(id = "hlp_txt_dwn",helpText("Note: Please apply filter before downloading data (Eg. for default dates press apply, for selected dates, select dates and press apply."))
                ),
                box(
                  title = "NovaScotia",status = "success", solidHeader = TRUE,collapsible = TRUE,
                  column(width = 6,
                         conditionalPanel( condition = "input.radio_ns == 1 || input.radio_ns == 0",dateRangeInput("ns_date_range", h5("Date range_1"))),
                         conditionalPanel( condition = "input.radio_ns == 2",dateRangeInput("ns_date_range_1", h5("Date range_2"))),
                         conditionalPanel( condition = "input.radio_ns == 1 || input.radio_ns == 0",actionButton("ns_filter", "Apply")),
                         conditionalPanel( condition = "input.radio_ns == 2", actionButton("ns_filter_1","Apply"))
                  ),
                  column(offset = 0.7,width = 6,
                         fluidRow(radioButtons("radio_ns", h3("Tables"),
                                               choices = list("None" = 0, "ns_realtime_load_novascotia_utc" = 1,"ns_realtime_trade_novascotia_utc" = 2),selected = 0)),
                         conditionalPanel( condition = "input.radio_ns == 1",
                                           fluidRow(downloadButton("data_ns_1", "Download Data"))),
                         conditionalPanel( condition = "input.radio_ns == 0",
                                           fluidRow(helpText("Note: Please Select a File to Download"))),
                         conditionalPanel( condition = "input.radio_ns == 2",
                                           fluidRow(downloadButton("data_ns_2", "Download Data")))
                  )
                )),
              fluidRow(
                box(
                  title = "Alberta",status = "success", solidHeader = TRUE,collapsible = TRUE,
                  column(width = 6,
                         conditionalPanel( condition = "input.radio_ab == 1 || input.radio_ab == 0",dateRangeInput("ab_date_range", h5("Date range_1"))),
                         conditionalPanel( condition = "input.radio_ab == 1 || input.radio_ab == 0",actionButton("ab_filter", "Apply")),
                         
                         conditionalPanel( condition = "input.radio_ab == 2",dateRangeInput("ab_date_range_1", h5("Date range_2"))),
                         conditionalPanel( condition = "input.radio_ab == 2", actionButton("ab_filter_1","Apply")),
                         
                         conditionalPanel( condition = "input.radio_ab == 3",dateRangeInput("ab_date_range_2", h5("Date range_3"))),
                         conditionalPanel( condition = "input.radio_ab == 3",actionButton("ab_filter_2", "Apply")),
                         
                         conditionalPanel( condition = "input.radio_ab == 4",dateRangeInput("ab_date_range_3", h5("Date range_4"))),
                         conditionalPanel( condition = "input.radio_ab == 4", actionButton("ab_filter_3","Apply")),
                         
                         conditionalPanel( condition = "input.radio_ab == 5",dateRangeInput("ab_date_range_4", h5("Date range_5"))),
                         conditionalPanel( condition = "input.radio_ab == 5",actionButton("ab_filter_4", "Apply")),
                         
                         conditionalPanel( condition = "input.radio_ab == 6",dateRangeInput("ab_date_range_5", h5("Date range_6"))),
                         conditionalPanel( condition = "input.radio_ab == 6", actionButton("ab_filter_5","Apply")),
                         
                         conditionalPanel( condition = "input.radio_ab == 7",dateRangeInput("ab_date_range_6", h5("Date range_7"))),
                         conditionalPanel( condition = "input.radio_ab == 7",actionButton("ab_filter_6", "Apply")),
                         
                         conditionalPanel( condition = "input.radio_ab == 8",dateRangeInput("ab_date_range_7", h5("Date range_8"))),
                         conditionalPanel( condition = "input.radio_ab == 8", actionButton("ab_filter_7","Apply")),
                         
                         conditionalPanel( condition = "input.radio_ab == 9",dateRangeInput("ab_date_range_8", h5("Date range_9"))),
                         conditionalPanel( condition = "input.radio_ab == 9",actionButton("ab_filter_8", "Apply")),
                         
                         conditionalPanel( condition = "input.radio_ab == 10",dateRangeInput("ab_date_range_9", h5("Date range_10"))),
                         conditionalPanel( condition = "input.radio_ab == 10", actionButton("ab_filter_9","Apply")),
                         
                         conditionalPanel( condition = "input.radio_ab == 11",dateRangeInput("ab_date_range_10", h5("Date range_11"))),
                         conditionalPanel( condition = "input.radio_ab == 11",actionButton("ab_filter_10", "Apply")),
                         
                         conditionalPanel( condition = "input.radio_ab == 12",dateRangeInput("ab_date_range_11", h5("Date range_12"))),
                         conditionalPanel( condition = "input.radio_ab == 12", actionButton("ab_filter_11","Apply")),
                         
                         conditionalPanel( condition = "input.radio_ab == 13",dateRangeInput("ab_date_range_12", h5("Date range_13"))),
                         conditionalPanel( condition = "input.radio_ab == 13",actionButton("ab_filter_12", "Apply")),
                         
                         conditionalPanel( condition = "input.radio_ab == 14",dateRangeInput("ab_date_range_13", h5("Date range_14"))),
                         conditionalPanel( condition = "input.radio_ab == 14", actionButton("ab_filter_13","Apply")),
                         
                         conditionalPanel( condition = "input.radio_ab == 15",dateRangeInput("ab_date_range_14", h5("Date range_15"))),
                         conditionalPanel( condition = "input.radio_ab == 15", actionButton("ab_filter_14","Apply"))
                  ),
                  column(offset = 0.7,width = 6,
                         radioButtons("radio_ab", h3("Tables"),
                                      choices = list("None" = 0,
                                                     "ab_hourly_demand_utc" = 1,
                                                     "ab_test_3" = 2,
                                                     "ab_test_4" = 3,
                                                     "ab_test_5_summary" = 4,
                                                     "ab_test_5_generation" = 5,
                                                     "ab_test_5_interchange" = 6,
                                                     "ab_test_5_gas" = 7,
                                                     "ab_test_5_hydro" = 8,
                                                     "ab_test_5_storage" = 9,
                                                     "ab_test_5_solar" = 10,
                                                     "ab_test_5_wind" = 11,
                                                     "ab_test_5_biomass" = 12,
                                                     "ab_test_5_dual" = 13,
                                                     "ab_test_5_coal" = 14,
                                                     "ab_hourly_zonal_demand_hist_utc" = 15
                                      ),selected = 0),
                         conditionalPanel( condition = "input.radio_ab == 1",
                                           downloadButton("data_ab", "Download Data")),
                         conditionalPanel( condition = "input.radio_ab == 2",
                                           downloadButton("data_ab_1", "Download Data")),
                         conditionalPanel( condition = "input.radio_ab == 3",
                                           downloadButton("data_ab_2", "Download Data")),
                         conditionalPanel( condition = "input.radio_ab == 4",
                                           downloadButton("data_ab_3", "Download Data")),
                         conditionalPanel( condition = "input.radio_ab == 5",
                                           downloadButton("data_ab_4", "Download Data")),
                         conditionalPanel( condition = "input.radio_ab == 6",
                                           downloadButton("data_ab_5", "Download Data")),
                         conditionalPanel( condition = "input.radio_ab == 7",
                                           downloadButton("data_ab_6", "Download Data")),
                         conditionalPanel( condition = "input.radio_ab == 8",
                                           downloadButton("data_ab_7", "Download Data")),
                         conditionalPanel( condition = "input.radio_ab == 9",
                                           downloadButton("data_ab_8", "Download Data")),
                         conditionalPanel( condition = "input.radio_ab == 10",
                                           downloadButton("data_ab_9", "Download Data")),
                         conditionalPanel( condition = "input.radio_ab == 11",
                                           downloadButton("data_ab_10", "Download Data")),
                         conditionalPanel( condition = "input.radio_ab == 12",
                                           downloadButton("data_ab_11", "Download Data")),
                         conditionalPanel( condition = "input.radio_ab == 13",
                                           downloadButton("data_ab_12", "Download Data")),
                         conditionalPanel( condition = "input.radio_ab == 14",
                                           downloadButton("data_ab_13", "Download Data")),
                         conditionalPanel( condition = "input.radio_ab == 15",
                                           downloadButton("data_ab_14", "Download Data")),
                         conditionalPanel( condition = "input.radio_ab == 0",
                                           helpText("Note: Please Select a File to Download"))
                  )
                  
                ),
                box(
                  title = "British Columbia",status = "success", solidHeader = TRUE,collapsible = TRUE,
                  column(width = 6,
                         conditionalPanel( condition = "input.radio_bc == 1 || input.radio_bc == 0",dateRangeInput("bc_date_range", h5("Date range_1"))),
                         conditionalPanel( condition = "input.radio_bc == 1 || input.radio_bc == 0",actionButton("bc_filter", "Apply")),
                         
                         conditionalPanel( condition = "input.radio_bc == 2",dateRangeInput("bc_date_range_1", h5("Date range_2"))),
                         conditionalPanel( condition = "input.radio_bc == 2", actionButton("bc_filter_1","Apply")),
                         
                  ),
                  column(offset = 0.7,width = 6,
                         fluidRow(radioButtons("radio_bc", h3("Tables"),
                                               choices = list("None" = 0,
                                                              "bc_hourly_table_1_rted_utc" = 1,
                                                              "bc_hourly_table_3_rted_utc" = 2
                                               ),selected = 0)),
                         conditionalPanel( condition = "input.radio_bc == 1",
                                           fluidRow(downloadButton("data_bc_1", "Download Data"))),
                         conditionalPanel( condition = "input.radio_bc == 0",
                                           fluidRow(helpText("Note: Please Select a File to Download"))),
                         conditionalPanel( condition = "input.radio_bc == 2",
                                           fluidRow(downloadButton("data_bc_2", "Download Data")))
                  )
                )),
              fluidRow(
                box(
                  title = "Ontario",status = "success", solidHeader = TRUE,collapsible = TRUE,
                  column(width = 6,
                         dateRangeInput("on_date_range", h5("Date range")),
                         actionButton("on_filter", "Apply")
                  ),
                  column(offset = 0.7,width = 6,
                         radioButtons("radio_on", h3("Tables"),
                                      choices = list("None" = 0,"Ontario_Load_RTED" = 1),selected = 0),
                         conditionalPanel( condition = "input.radio_on == 1",
                                           downloadButton("data_on", "Download Data")),
                         conditionalPanel( condition = "input.radio_on == 0",
                                           helpText("Note: Please Select a File to Download"))
                  )
                  
                ),
                box(
                  title = "Prince Edward Island",status = "success", solidHeader = TRUE,collapsible = TRUE,
                  column(width = 6,
                         dateRangeInput("pei_date_range", h5("Date range")),
                         actionButton("pei_filter", "Apply")
                  ),
                  column(offset = 0.7,width = 6,
                         fluidRow(radioButtons("radio_pei", h3("Tables"),
                                               choices = list("None" = 0,"pei_rted" = 1),selected = 0)),
                         conditionalPanel( condition = "input.radio_pei == 1",
                                           fluidRow(downloadButton("data_pei_1", "Download Data"))),
                         conditionalPanel( condition = "input.radio_pei == 0",
                                           fluidRow(helpText("Note: Please Select a File to Download"))),
                  )
                )),
              fluidRow(
                box(
                  title = "NewFoundland & Labrador",status = "success", solidHeader = TRUE,collapsible = TRUE,
                  column(width = 6,
                         dateRangeInput("nfl_date_range", h5("Date range")),
                         actionButton("nfl_filter", "Apply")
                  ),
                  column(offset = 0.7,width = 6,
                         radioButtons("radio_nfl", h3("Tables"),
                                      choices = list("None" = 0,"NFL_rt_load" = 1),selected = 0),
                         conditionalPanel( condition = "input.radio_nfl == 1",
                                           downloadButton("data_nfl", "Download Data")),
                         conditionalPanel( condition = "input.radio_nfl == 0",
                                           helpText("Note: Please Select a File to Download"))
                  )
                  
                ),
                box(
                  title = "Quebec",status = "success", solidHeader = TRUE,collapsible = TRUE,
                  column(width = 6,
                         conditionalPanel( condition = "input.radio_qb == 1 || input.radio_qb == 0",dateRangeInput("qb_date_range", h5("Date range_1"))),
                         conditionalPanel( condition = "input.radio_qb == 1 || input.radio_qb == 0",actionButton("qb_filter", "Apply")),
                         
                         conditionalPanel( condition = "input.radio_qb == 2",dateRangeInput("qb_date_range_1", h5("Date range_2"))),
                         conditionalPanel( condition = "input.radio_qb == 2", actionButton("qb_filter_1","Apply")),
                         
                         conditionalPanel( condition = "input.radio_qb == 3",dateRangeInput("qb_date_range_2", h5("Date range_3"))),
                         conditionalPanel( condition = "input.radio_qb == 3",actionButton("qb_filter_2", "Apply"))
                         
                  ),
                  column(offset = 0.7,width = 6,
                         fluidRow(radioButtons("radio_qb", h3("Tables"),
                                               choices = list("None" = 0,"hfed_qb_quarterhour_demand" = 1,"hfed_qb_hourly_production" = 2,"hfed_qb_hourly_allvar_hist" = 3),selected = 0)),
                         conditionalPanel( condition = "input.radio_qb == 1",
                                           fluidRow(downloadButton("data_qb_1", "Download Data"))),
                         conditionalPanel( condition = "input.radio_qb == 2",
                                           fluidRow(downloadButton("data_qb_2", "Download Data"))),
                         conditionalPanel( condition = "input.radio_qb == 3",
                                           fluidRow(downloadButton("data_qb_3", "Download Data"))),
                         conditionalPanel( condition = "input.radio_qb == 0",
                                           fluidRow(helpText("Note: Please Select a File to Download")))
                  )
                ))
      )
      
    ))  
)


#bussines logic

base_url_prefix <- "https://fdi-design-sdmx.aaw-dev.cloud.statcan.ca/rest/data/CCEI,"
base_url_suffix <- "&dimensionAtObservation=AllDimensions"
base_url_suffix_lastnobs <- "&lastNObservations="
dataset <- function(dataflow,ref_area,energy_flow,startdate,enddate,nlastobs){
  if(is.null(startdate)&&is.null(enddate)){
    dataset_url <- paste(base_url_prefix,dataflow,",1.0/N.","..",energy_flow,"?",base_url_suffix,base_url_suffix_lastnobs,nlastobs,sep = "")
  }
  if(is.null(nlastobs)&&is.null(startdate)&&is.null(enddate)){
    dataset_url <- paste(base_url_prefix,dataflow,",1.0/N.","..",energy_flow,"?",base_url_suffix,sep = "")
  }
  else{
    dataset_url <- paste(base_url_prefix,dataflow,",1.0/N.","..",energy_flow,"?","startPeriod=",startdate,"&endPeriod=",enddate,"&",base_url_suffix,sep = "")
  }
  raw_data <- GET(dataset_url, accept("application/vnd.sdmx.data+csv; charset=utf-8"))
  bin <- content(raw_data, "text", encoding = "ISO-8859-1")
  clean_data <- read.table(text = bin, sep =",", header = TRUE, stringsAsFactors = FALSE)
  sortdata <- clean_data
  sortdata$DATETIME_LOCAL <- str_replace(sortdata$DATETIME_LOCAL,"T"," ")
  sortdata$DATETIME_LOCAL <- as.POSIXlt(sortdata$DATETIME_LOCAL,format="%Y-%m-%d %H:%M:%S")
  print(paste("DataFlow",dataflow,"Status",status_code(raw_data),sep = ":"))
  
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
  #con <- DBI::dbConnect(odbc::odbc(), 
                        #UID = config$username,
                        #TDS_Version = config$TDS_version,
                        #Port = config$Port,
                        #Encrypt = config$encrypt,
                        #TrustServerCertificate = config$TSC,
                        #Driver=config$driver,
                        #Server = config$server, Database = config$database,
                        #Authentication = config$auth,
                        #timeoutSecs = 30)
  
  
  
  Previous_date <- as.Date(Sys.Date())-(5*365)
  previous_time <- paste(Previous_date,"00:00:00",sep=" ")
  
  Previous_date_1 <- as.Date(Sys.Date())-(1)
  previous_time_1 <- paste(Previous_date_1,"00:00:00",sep=" ")
  
  
  
  
  
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
  
  
  
  check_db_nb <- function(){dataset("DF_HFED_NB","CA_NB","LOAD",NULL,NULL,NULL) %>% count(DATETIME_LOCAL)}
  get_data_nb <- function(){dataset("DF_HFED_NB","CA_NB","LOAD",NULL,NULL,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()}
  nbload_data_pre <- reactivePoll(intervalMillis = 300000, session = session,
                                  checkFunc = check_db_nb, valueFunc = get_data_nb)
  nbload_data <- reactive({nbload_data_pre()})
  nbload_data_mean_cr <- reactive({tail(nbload_data()$OBS_VALUE,1)})
  nbload_data_mean_pst <- reactive({nbload_data()$OBS_VALUE[nrow(nbload_data())-1]})
  nbload_data_mean_diff <- reactive({nbload_data_mean_cr()-nbload_data_mean_pst()})
  nbload_data_mean_prcnt <- reactive({nbload_data_mean_diff()/100})
  observeEvent(nbload_data_pre(),{
    
    if(nbload_data_mean_prcnt() < 0){
      removeClass("nbmean","green_output")
      addClass("nbmean","red_output")
      output$NB_MEAN <- renderUI({HTML(paste("<h4><i class='fa fa-arrow-down'>",round(nbload_data_mean_prcnt(),digits = 2),"% in last Hour</i></h4>"))})
    }
    if(nbload_data_mean_prcnt() > 0){
      removeClass("nbmean","red_output")
      addClass("nbmean","green_output")
      output$NB_MEAN <- renderUI({HTML(paste("<h4><i class='fa fa-arrow-up'>",round(nbload_data_mean_prcnt(),digits = 2),"% in last Hour</i></h4>"))})
    }})
  nbload_subset <- reactive({subset(nbload_data(),subset = nbload_data()$DATETIME_LOCAL >= previous_time_1)})
  nb_load_ts <-  reactive({xts(nbload_subset()$OBS_VALUE,nbload_subset()$DATETIME_LOCAL)})
  
  output$NB_load <- renderHighchart({ 
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_load_ts(), type = "line", id = "ts", name = "Load: ") %>% 
      hc_navigator(enabled = TRUE)
  })
  
  check_db_ns <- function(){dataset("DF_HFED_NS","CA_NS","LOAD",NULL,NULL,NULL) %>% count(DATETIME_LOCAL)}
  get_data_ns <- function(){dataset("DF_HFED_NS","CA_NS","LOAD",NULL,NULL,NULL) %>% arrange(DATETIME_LOCAL) %>% collect()}
  nsload_data_pre <- reactivePoll(intervalMillis = 300000, session = session,
                                  checkFunc = check_db_ns, valueFunc = get_data_ns)
  nsload_data <- reactive({nsload_data_pre()})
  nsload_data_mean_cr <- reactive({tail(nsload_data()$OBS_VALUE,1)})
  nsload_data_mean_pst <- reactive({nsload_data()$OBS_VALUE[nrow(nsload_data())-1]})
  nsload_data_mean_diff <- reactive({nsload_data_mean_cr()-nsload_data_mean_pst()})
  nsload_data_mean_prcnt <- reactive({nsload_data_mean_diff()/100})
  observeEvent(nsload_data_pre(),{
    
    if(nsload_data_mean_prcnt() < 0){
      removeClass("nsmean","green_output")
      addClass("nsmean","red_output")
      output$NS_MEAN <- renderUI({HTML(paste("<h4><i class='fa fa-arrow-down'>",round(nsload_data_mean_prcnt(),digits = 2),"% in last Hour</i></h4>"))})
    }
    if(nsload_data_mean_prcnt() > 0){
      removeClass("nsmean","red_output")
      addClass("nsmean","green_output")
      output$NS_MEAN <- renderUI({HTML(paste("<h4><i class='fa fa-arrow-up'>",round(nsload_data_mean_prcnt(),digits = 2),"% in last Hour</i></h4>"))})
    }})
  nsload_subset <- reactive({subset(nsload_data(),subset = nsload_data()$DATETIME_LOCAL >= previous_time_1)})
  ns_load_ts <-  reactive({xts(nsload_subset()$OBS_VALUE,nsload_subset()$DATETIME_LOCAL)})
  
  output$NS_load <- renderHighchart({highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_load_ts(), type = "line", name = "Load: ", color = "green") %>% 
      hc_navigator(enabled = TRUE)}) 
  
  check_db_bc <- function(){dataset("DF_HFED_BC","CA_BC","LOAD",NULL,NULL,NULL) %>% count(DATETIME_LOCAL)}
  get_db_bc <- function(){dataset("DF_HFED_BC","CA_BC","LOAD",NULL,NULL,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()}
  bcload_data_pre <- reactivePoll(intervalMillis = 300000, session = session,
                                  checkFunc = check_db_bc, valueFunc = get_db_bc)
  bcload_data <- reactive({bcload_data_pre()})
  bcload_data_mean_cr <- reactive({tail(bcload_data()$OBS_VALUE,1)})
  bcload_data_mean_pst <- reactive({bcload_data()$OBS_VALUE[nrow(bcload_data())-1]})
  bcload_data_mean_diff <- reactive({bcload_data_mean_cr()-bcload_data_mean_pst()})
  bcload_data_mean_prcnt <- reactive({bcload_data_mean_diff()/100})
  observeEvent(bcload_data_pre(),{
    
    if(bcload_data_mean_prcnt() < 0){
      removeClass("bcmean","green_output")
      addClass("bcmean","red_output")
      output$BC_MEAN <- renderUI({HTML(paste("<h4><i class='fa fa-arrow-down'>",round(bcload_data_mean_prcnt(),digits = 2),"% in last Hour</i></h4>"))})
    }
    if(bcload_data_mean_prcnt() > 0){
      removeClass("bcmean","red_output")
      addClass("bcmean","green_output")
      output$BC_MEAN <- renderUI({HTML(paste("<h4><i class='fa fa-arrow-up'>",round(bcload_data_mean_prcnt(),digits = 2),"% in last Hour</i></h4>"))})
    }})
  bcload_subset <- reactive({bcload_data()[(1:10),]})
  bc_load_ts <-  reactive({xts(bcload_subset()$OBS_VALUE,as.POSIXlt(bcload_subset()$DATETIME_LOCAL,format = "%Y-%m-%d %H:%M:%S"))})
  
  output$BC_load <- renderHighchart({highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(bc_load_ts(), type = "line", name = "Load: ", color = "pink") %>% 
      hc_navigator(enabled = TRUE)})
  
  #output$AB_load <- renderHighchart({ab_load_chart})
  
  check_db_on <- function(){dataset("DF_HFED_ON","CA_ON","ONTARIO_DEMAND",NULL,NULL,NULL) %>% count(DATETIME_LOCAL)}
  get_data_on <- function(){dataset("DF_HFED_ON","CA_ON","ONTARIO_DEMAND",NULL,NULL,NULL) %>% arrange(DATETIME_LOCAL) %>% collect()}
  onload_data_pre <- reactivePoll(intervalMillis = 300000, session = session,
                                  checkFunc = check_db_on, valueFunc = get_data_on)
  onload_data <- reactive({onload_data_pre()})
  onload_data_mean_cr <- reactive({tail(onload_data()$OBS_VALUE,1)})
  onload_data_mean_pst <- reactive({onload_data()$OBS_VALUE[nrow(onload_data())-1]})
  onload_data_mean_diff <- reactive({onload_data_mean_cr()-onload_data_mean_pst()})
  onload_data_mean_prcnt <- reactive({onload_data_mean_diff()/100})
  observeEvent(onload_data_pre(),{
    
    if(onload_data_mean_prcnt() < 0){
      removeClass("onmean","green_output")
      addClass("onmean","red_output")
      output$ON_MEAN <- renderUI({HTML(paste("<h4><i class='fa fa-arrow-down'>",round(onload_data_mean_prcnt(),digits = 2),"% in last Hour</i></h4>"))})
    }
    if(onload_data_mean_prcnt() > 0){
      removeClass("onmean","red_output")
      addClass("onmean","green_output")
      output$ON_MEAN <- renderUI({HTML(paste("<h4><i class='fa fa-arrow-up'>",round(onload_data_mean_prcnt(),digits = 2),"% in last Hour</i></h4>"))})
    }})
  
  onload_subset <- reactive({subset(onload_data(),subset = onload_data()$DATETIME_LOCAL >= previous_time_1)})
  on_load_ts <-  reactive({xts(onload_subset()$OBS_VALUE,onload_subset()$DATETIME_LOCAL)})
  
  output$ON_load <- renderHighchart({highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_load_ts(), type = "line", name = "Load: ", color = "orange") %>% 
      hc_navigator(enabled = TRUE)})
  
  check_db_pei <- function(){dataset("DF_HFED_PE","CA_PE","ON_ISL_LOAD",NULL,NULL,NULL) %>% count(DATETIME_LOCAL)}
  get_data_pei <- function(){dataset("DF_HFED_PE","CA_PE","ON_ISL_LOAD",NULL,NULL,NULL) %>% arrange(DATETIME_LOCAL) %>% collect()}
  peiload_data_pre <- reactivePoll(intervalMillis = 300000, session = session,
                                   checkFunc = check_db_pei, valueFunc = get_data_pei)
  peiload_data <- reactive({peiload_data_pre()})
  peiload_data_mean_cr <- reactive({tail(peiload_data()$OBS_VALUE,1)})
  peiload_data_mean_pst <- reactive({peiload_data()$OBS_VALUE[nrow(peiload_data())-1]})
  peiload_data_mean_diff <- reactive({peiload_data_mean_cr()-peiload_data_mean_pst()})
  peiload_data_mean_prcnt <- reactive({peiload_data_mean_diff()/100})
  observeEvent(peiload_data_pre(),{
    
    if(peiload_data_mean_prcnt() < 0){
      removeClass("peimean","green_output")
      addClass("peimean","red_output")
      output$PEI_MEAN <- renderUI({HTML(paste("<h4><i class='fa fa-arrow-down'>",round(peiload_data_mean_prcnt(),digits = 2),"% in last Hour</i></h4>"))})
    }
    if(peiload_data_mean_prcnt() > 0){
      removeClass("peimean","red_output")
      addClass("peimean","green_output")
      output$PEI_MEAN <- renderUI({HTML(paste("<h4><i class='fa fa-arrow-up'>",round(peiload_data_mean_prcnt(),digits = 2),"% in last Hour</i></h4>"))})
    }})
  
  
  peiload_subset <- reactive({subset(peiload_data(),subset = peiload_data()$DATETIME_LOCAL >= previous_time_1)})
  pei_load_ts <-  reactive({xts(peiload_subset()$OBS_VALUE,peiload_subset()$DATETIME_LOCAL)})
  
  output$PEI_load <- renderHighchart({highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_load_ts(), type = "line", name = "Load: ", color = "red") %>% 
      hc_navigator(enabled = TRUE)}) 
  
  check_db_nfl <- function(){dataset("DF_HFED_NL","CA_NL","DEMAND",NULL,NULL,NULL) %>% count(DATETIME_LOCAL)}
  get_data_nfl <- function(){dataset("DF_HFED_NL","CA_NL","DEMAND",NULL,NULL,NULL) %>% arrange(desc(DATETIME_LOCAL)) %>% collect()}
  nflload_data_pre <- reactivePoll(intervalMillis = 300000, session = session,
                                   checkFunc = check_db_nfl, valueFunc = get_data_nfl)
  nflload_data <- reactive({nflload_data_pre()})
  nflload_data_mean_cr <- reactive({tail(nflload_data()$OBS_VALUE,1)})
  nflload_data_mean_pst <- reactive({nflload_data()$OBS_VALUE[nrow(nflload_data())-1]})
  nflload_data_mean_diff <- reactive({nflload_data_mean_cr()-nflload_data_mean_pst()})
  nflload_data_mean_prcnt <- reactive({nflload_data_mean_diff()/100})
  observeEvent(nflload_data_pre(),{
    
    if(nflload_data_mean_prcnt() < 0){
      removeClass("nflmean","green_output")
      addClass("nflmean","red_output")
      output$NFL_MEAN <- renderUI({HTML(paste("<h4><i class='fa fa-arrow-down'>",round(nflload_data_mean_prcnt(),digits = 2),"% in last Hour</i></h4>"))})
    }
    if(nflload_data_mean_prcnt() > 0){
      removeClass("nflmean","red_output")
      addClass("nflmean","green_output")
      output$NFL_MEAN <- renderUI({HTML(paste("<h4><i class='fa fa-arrow-up'>",round(nflload_data_mean_prcnt(),digits = 2),"% in last Hour</i></h4>"))})
    }})
  
  nflload_subset <- reactive({nflload_data()[(1:10),]})
  nfl_load_ts <-  reactive({xts(nflload_subset()$OBS_VALUE,nflload_subset()$DATETIME_LOCAL)})
  
  output$NFL_load <- renderHighchart({highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nfl_load_ts(), type = "line", name = "Load: ", color = "red") %>% 
      hc_navigator(enabled = TRUE)})
  
  check_db_qb <- function(){dataset("DF_HFED_QC","CA_QC","DEMAND",NULL,NULL,NULL) %>% count(DATETIME_LOCAL)}
  get_data_qb <- function(){dataset("DF_HFED_QC","CA_QC","DEMAND",NULL,NULL,NULL) %>% arrange(DATETIME_LOCAL) %>% collect()}
  qbload_data_pre <- reactivePoll(intervalMillis = 300000, session = session,
                                  checkFunc = check_db_qb, valueFunc = get_data_qb)
  qbload_data <- reactive({qbload_data_pre()})
  qbload_data_mean_cr <- reactive({tail(qbload_data()$OBS_VALUE,1)})
  qbload_data_mean_pst <- reactive({qbload_data()$OBS_VALUE[nrow(qbload_data())-1]})
  qbload_data_mean_diff <- reactive({qbload_data_mean_cr()-qbload_data_mean_pst()})
  qbload_data_mean_prcnt <- reactive({qbload_data_mean_diff()/100})
  observeEvent(qbload_data_pre,{
    
    if(qbload_data_mean_prcnt() < 0){
      removeClass("qbmean","green_output")
      addClass("qbmean","red_output")
      output$QB_MEAN <- renderUI({HTML(paste("<h4><i class='fa fa-arrow-down'>",round(qbload_data_mean_prcnt(),digits = 2),"% in last Hour</i></h4>"))})
    }
    if(qbload_data_mean_prcnt() > 0){
      removeClass("qbmean","red_output")
      addClass("qbmean","green_output")
      output$QB_MEAN <- renderUI({HTML(paste("<h4><i class='fa fa-arrow-up'>",round(qbload_data_mean_prcnt(),digits = 2),"% in last Hour</i></h4>"))})
    }})
  
  qbload_subset <- reactive({subset(qbload_data(),subset = qbload_data()$DATETIME_LOCAL >= previous_time_1)})
  qb_load_ts <-  reactive({xts(qbload_subset()$OBS_VALUE,qbload_subset()$DATETIME_LOCAL)})
  
  output$QB_load <- renderHighchart({highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_load_ts(), type = "line", name = "Load: ", color = "red") %>% 
      hc_navigator(enabled = TRUE)})
  
  
  pei_date_ind_1_1 <- reactive({paste(input$pei_dates_1[1],"00:00:00",sep = " ")})
  pei_date_ind_1_2 <- reactive({paste(input$pei_dates_1[2],"00:00:00",sep = " ")})
  pei_ind_subset_dat <- reactive({subset(pei_ind_dat,subset = (pei_ind_dat$Date_time_local >= pei_date_ind_1_1() & pei_ind_dat$Date_time_local <= pei_date_ind_1_2()))})
  pei_ind_dat_ts <- reactive({xts(pei_ind_dat$on_island_load,pei_ind_dat$Date_time_local)})
  pei_ind_subset_ts <- reactive({xts(pei_ind_subset_dat()$on_island_load,pei_ind_subset_dat()$Date_time_local)})
  pei_ind_dat_ts_yearly <- reactive({to.yearly(pei_ind_dat_ts())})
  pei_ind_dat_ts_monthly <- reactive({to.monthly(pei_ind_subset_ts())})
  pei_ind_dat_ts_weekly <- reactive({to.weekly(pei_ind_subset_ts())})
  pei_ind_dat_ts_daily <- reactive({to.daily(pei_ind_subset_ts())})
  pei_ind_dat_ts_hourly <- reactive({
    pei_hr <- to.hourly(pei_ind_subset_ts()) 
    return(pei_hr)
  })
  
  output$PEI_ON_LOAD_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_ind_dat_ts_yearly()[,(colnames(pei_ind_dat_ts_yearly()) %in% c('pei_ind_dat_ts().High'))], type = "line", name = "High Load: ", color = "lightgreen") %>%
      hc_add_series(pei_ind_dat_ts_yearly()[,(colnames(pei_ind_dat_ts_yearly()) %in% c('pei_ind_dat_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_ON_LOAD_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_ind_dat_ts_weekly()[,(colnames(pei_ind_dat_ts_weekly()) %in% c('pei_ind_subset_ts().High'))], type = "line", name = "High Load: ", color = "lightgreen") %>%
      hc_add_series(pei_ind_dat_ts_weekly()[,(colnames(pei_ind_dat_ts_weekly()) %in% c('pei_ind_subset_ts().Low'))], type = "area", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_ON_LOAD_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_ind_dat_ts_daily()[,(colnames(pei_ind_dat_ts_daily()) %in% c('pei_ind_subset_ts().High'))], type = "line", name = "High Load: ", color = "lightgreen") %>%
      hc_add_series(pei_ind_dat_ts_daily()[,(colnames(pei_ind_dat_ts_daily()) %in% c('pei_ind_subset_ts().Low'))], type = "area", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_ON_LOAD_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_ind_dat_ts_hourly()[,(colnames(pei_ind_dat_ts_hourly()) %in% c('pei_ind_subset_ts().High'))], type = "line", name = "High Load: ", color = "lightgreen") %>%
      hc_add_series(pei_ind_dat_ts_hourly()[,(colnames(pei_ind_dat_ts_hourly()) %in% c('pei_ind_subset_ts().Low'))], type = "spline", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_ON_LOAD_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(pei_ind_subset_ts(), type = "line", name = "Load: ")
  })
  
  pei_date_ind_2_1 <- reactive({paste(input$pei_dates_2[1],"00:00:00",sep = " ")})
  pei_date_ind_2_2 <- reactive({paste(input$pei_dates_2[2],"00:00:00",sep = " ")})
  pei_ind_subset_dat_2 <- reactive({subset(pei_ind_dat,subset = (pei_ind_dat$Date_time_local >= pei_date_ind_2_1() & pei_ind_dat$Date_time_local <= pei_date_ind_2_2()))})
  pei_ind_dat_ts_2 <- reactive({xts(pei_ind_dat$on_island_wind,pei_ind_dat$Date_time_local)})
  pei_ind_dat_ts_2_fossil <- reactive({xts(pei_ind_dat$on_island_fossil,pei_ind_dat$Date_time_local)})
  pei_ind_subset_ts_2 <- reactive({xts(pei_ind_subset_dat_2()$on_island_wind,pei_ind_subset_dat_2()$Date_time_local)})
  pei_ind_subset_ts_2_fossil <- reactive({xts(pei_ind_subset_dat_2()$on_island_fossil,pei_ind_subset_dat_2()$Date_time_local)})
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
  
  output$PEI_ON_WIND_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_ind_dat_ts_yearly_2()[,(colnames(pei_ind_dat_ts_yearly_2()) %in% c('pei_ind_dat_ts_2().High'))], type = "line", name = "High Wind Load: ", color = "lightgreen") %>%
      hc_add_series(pei_ind_dat_ts_yearly_2()[,(colnames(pei_ind_dat_ts_yearly_2()) %in% c('pei_ind_dat_ts_2().Low'))], type = "line", name = "Low Wind Load: ", color = "red") %>%
      hc_add_series(pei_ind_dat_ts_yearly_2_fossil()[,(colnames(pei_ind_dat_ts_yearly_2_fossil()) %in% c('pei_ind_dat_ts_2_fossil().High'))], type = "line", name = "High Fossil Load: ", color = "pink") %>%
      hc_add_series(pei_ind_dat_ts_yearly_2_fossil()[,(colnames(pei_ind_dat_ts_yearly_2_fossil()) %in% c('pei_ind_dat_ts_2_fossil().Low'))], type = "line", name = "Low Fossil Load: ", color = "blue") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_ON_WIND_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_ind_dat_ts_weekly_2()[,(colnames(pei_ind_dat_ts_weekly_2()) %in% c('pei_ind_subset_ts_2().High'))], type = "line", name = "High Wind Load: ", color = "lightgreen") %>%
      hc_add_series(pei_ind_dat_ts_weekly_2()[,(colnames(pei_ind_dat_ts_weekly_2()) %in% c('pei_ind_subset_ts_2().Low'))], type = "area", name = "Low Wind Load: ", color = "red") %>%
      hc_add_series(pei_ind_dat_ts_weekly_2_fossil()[,(colnames(pei_ind_dat_ts_weekly_2_fossil()) %in% c('pei_ind_subset_ts_2_fossil().High'))], type = "line", name = "High Fossil Load: ", color = "pink") %>%
      hc_add_series(pei_ind_dat_ts_weekly_2_fossil()[,(colnames(pei_ind_dat_ts_weekly_2_fossil()) %in% c('pei_ind_subset_ts_2_fossil().Low'))], type = "area", name = "Low Fossil Load: ", color = "blue") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_ON_WIND_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_ind_dat_ts_daily_2()[,(colnames(pei_ind_dat_ts_daily_2()) %in% c('pei_ind_subset_ts_2().High'))], type = "line", name = "High Wind Load: ", color = "lightgreen") %>%
      hc_add_series(pei_ind_dat_ts_daily_2()[,(colnames(pei_ind_dat_ts_daily_2()) %in% c('pei_ind_subset_ts_2().Low'))], type = "area", name = "Low Wind Load: ", color = "red") %>%
      hc_add_series(pei_ind_dat_ts_daily_2_fossil()[,(colnames(pei_ind_dat_ts_daily_2_fossil()) %in% c('pei_ind_subset_ts_2_fossil().High'))], type = "line", name = "High Fossil Load: ", color = "pink") %>%
      hc_add_series(pei_ind_dat_ts_daily_2_fossil()[,(colnames(pei_ind_dat_ts_daily_2_fossil()) %in% c('pei_ind_subset_ts_2_fossil().Low'))], type = "area", name = "Low Fossil Load: ", color = "blue") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_ON_WIND_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_ind_dat_ts_hourly_2()[,(colnames(pei_ind_dat_ts_hourly_2()) %in% c('pei_ind_subset_ts_2().High'))], type = "line", name = "High Wind Load: ", color = "lightgreen") %>%
      hc_add_series(pei_ind_dat_ts_hourly_2()[,(colnames(pei_ind_dat_ts_hourly_2()) %in% c('pei_ind_subset_ts_2().Low'))], type = "spline", name = "Low Wind Load: ", color = "red") %>%
      hc_add_series(pei_ind_dat_ts_hourly_2_fossil()[,(colnames(pei_ind_dat_ts_hourly_2_fossil()) %in% c('pei_ind_subset_ts_2_fossil().High'))], type = "line", name = "High Fossil Load: ", color = "pink") %>%
      hc_add_series(pei_ind_dat_ts_hourly_2_fossil()[,(colnames(pei_ind_dat_ts_hourly_2_fossil()) %in% c('pei_ind_subset_ts_2_fossil().Low'))], type = "spline", name = "Low Fossil Load: ", color = "blue") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_ON_WIND_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(pei_ind_subset_ts_2(), type = "line", name = "Wind Load: ", color = "lightgreen")%>%
      hc_add_series(pei_ind_subset_ts_2_fossil(), type = "line", name = "Fossil Load: ", color = "pink")
  })
  
  pei_date_ind_3_1 <- reactive({paste(input$pei_dates_3[1],"00:00:00",sep = " ")})
  pei_date_ind_3_2 <- reactive({paste(input$pei_dates_3[2],"00:00:00",sep = " ")})
  pei_exp_subset_dat <- reactive({subset(pei_ind_dat,subset = (pei_ind_dat$Date_time_local >= pei_date_ind_3_1() & pei_ind_dat$Date_time_local <= pei_date_ind_3_2()))})
  pei_exp_dat_ts <- reactive({xts(pei_ind_dat$wind_export,pei_ind_dat$Date_time_local)})
  pei_exp_local_dat_ts <- reactive({xts(pei_ind_dat$wind_local,pei_ind_dat$Date_time_local)})
  pei_exp_subset_ts <- reactive({xts(pei_exp_subset_dat()$wind_export,pei_exp_subset_dat()$Date_time_local)})
  pei_exp_local_subset_ts <- reactive({xts(pei_exp_subset_dat()$wind_local,pei_exp_subset_dat()$Date_time_local)})
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
  
  output$PEI_EXPORT_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_exp_dat_ts_yearly()[,(colnames(pei_exp_dat_ts_yearly()) %in% c('pei_exp_dat_ts().High'))], type = "line", name = "High Export Load: ", color = "lightgreen") %>%
      hc_add_series(pei_exp_dat_ts_yearly()[,(colnames(pei_exp_dat_ts_yearly()) %in% c('pei_exp_dat_ts().Low'))], type = "line", name = "Low Export Load: ", color = "red") %>%
      hc_add_series(pei_exp_local_dat_ts_yearly()[,(colnames(pei_exp_local_dat_ts_yearly()) %in% c('pei_exp_local_dat_ts().High'))], type = "line", name = "High Local Load: ", color = "pink") %>%
      hc_add_series(pei_exp_local_dat_ts_yearly()[,(colnames(pei_exp_local_dat_ts_yearly()) %in% c('pei_exp_local_dat_ts().Low'))], type = "line", name = "Low Local Load: ", color = "blue") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_EXPORT_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_exp_dat_ts_weekly()[,(colnames(pei_exp_dat_ts_weekly()) %in% c('pei_exp_subset_ts().High'))], type = "line", name = "High Export Load: ", color = "lightgreen") %>%
      hc_add_series(pei_exp_dat_ts_weekly()[,(colnames(pei_exp_dat_ts_weekly()) %in% c('pei_exp_subset_ts().Low'))], type = "area", name = "Low Export Load: ", color = "red") %>%
      hc_add_series(pei_exp_local_dat_ts_weekly()[,(colnames(pei_exp_local_dat_ts_weekly()) %in% c('pei_exp_local_subset_ts().High'))], type = "line", name = "High Local Load: ", color = "pink") %>%
      hc_add_series(pei_exp_local_dat_ts_weekly()[,(colnames(pei_exp_local_dat_ts_weekly()) %in% c('pei_exp_local_subset_ts().Low'))], type = "area", name = "Low Local Load: ", color = "blue") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_EXPORT_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_exp_dat_ts_daily()[,(colnames(pei_exp_dat_ts_daily()) %in% c('pei_exp_subset_ts().High'))], type = "line", name = "High Export Load: ", color = "lightgreen") %>%
      hc_add_series(pei_exp_dat_ts_daily()[,(colnames(pei_exp_dat_ts_daily()) %in% c('pei_exp_subset_ts().Low'))], type = "area", name = "Low Export Load: ", color = "red") %>%
      hc_add_series(pei_exp_local_dat_ts_daily()[,(colnames(pei_exp_local_dat_ts_daily()) %in% c('pei_exp_local_subset_ts().High'))], type = "line", name = "High Local Load: ", color = "pink") %>%
      hc_add_series(pei_exp_local_dat_ts_daily()[,(colnames(pei_exp_local_dat_ts_daily()) %in% c('pei_exp_local_subset_ts().Low'))], type = "area", name = "Low Local Load: ", color = "blue") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_EXPORT_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_exp_dat_ts_hourly()[,(colnames(pei_exp_dat_ts_hourly()) %in% c('pei_exp_subset_ts().High'))], type = "line", name = "High Export Load: ", color = "lightgreen") %>%
      hc_add_series(pei_exp_dat_ts_hourly()[,(colnames(pei_exp_dat_ts_hourly()) %in% c('pei_exp_subset_ts().Low'))], type = "spline", name = "Low Export Load: ", color = "red") %>%
      hc_add_series(pei_exp_local_dat_ts_hourly()[,(colnames(pei_exp_local_dat_ts_hourly()) %in% c('pei_exp_local_subset_ts().High'))], type = "line", name = "High Local Load: ", color = "pink") %>%
      hc_add_series(pei_exp_local_dat_ts_hourly()[,(colnames(pei_exp_local_dat_ts_hourly()) %in% c('pei_exp_local_subset_ts().Low'))], type = "area", name = "Low Local Load: ", color = "blue") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_EXPORT_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(pei_exp_subset_ts(), type = "line", name = "Export Load: ", color = "pink") %>%
      hc_add_series(pei_exp_local_subset_ts(), type = "line", name = "Local Load: ", color = "lightgreen")
  })
  
  pei_date_ind_4_1 <- reactive({paste(input$pei_dates_4[1],"00:00:00",sep = " ")})
  pei_date_ind_4_2 <- reactive({paste(input$pei_dates_4[2],"00:00:00",sep = " ")})
  pei_loc_subset_dat <- reactive({subset(pei_ind_dat,subset = (pei_ind_dat$Date_time_local >= pei_date_ind_4_1() & pei_ind_dat$Date_time_local <= pei_date_ind_4_2()))})
  pei_loc_dat_ts <- reactive({xts(pei_ind_dat$percentage_wind,pei_ind_dat$Date_time_local)})
  pei_loc_subset_ts <- reactive({xts(pei_loc_subset_dat()$percentage_wind,pei_loc_subset_dat()$Date_time_local)})
  pei_loc_dat_ts_yearly <- reactive({to.yearly(pei_loc_dat_ts())})
  pei_loc_dat_ts_monthly <- reactive({to.monthly(pei_loc_subset_ts())})
  pei_loc_dat_ts_weekly <- reactive({to.weekly(pei_loc_subset_ts())})
  pei_loc_dat_ts_daily <- reactive({to.daily(pei_loc_subset_ts())})
  pei_loc_dat_ts_hourly <- reactive({to.hourly(pei_loc_subset_ts())})
  
  
  output$PEI_LOCAL_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_loc_dat_ts_yearly()[,(colnames(pei_loc_dat_ts_yearly()) %in% c('pei_loc_dat_ts().High'))], type = "line", name = "High Local Load: ", color = "lightgreen") %>%
      hc_add_series(pei_loc_dat_ts_yearly()[,(colnames(pei_loc_dat_ts_yearly()) %in% c('pei_loc_dat_ts().Low'))], type = "line", name = "Low Local Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_LOCAL_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_loc_dat_ts_weekly()[,(colnames(pei_loc_dat_ts_weekly()) %in% c('pei_loc_subset_ts().High'))], type = "line", name = "High Local Load: ", color = "lightgreen") %>%
      hc_add_series(pei_loc_dat_ts_weekly()[,(colnames(pei_loc_dat_ts_weekly()) %in% c('pei_loc_subset_ts().Low'))], type = "area", name = "Low Local Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_LOCAL_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_loc_dat_ts_daily()[,(colnames(pei_loc_dat_ts_daily()) %in% c('pei_loc_subset_ts().High'))], type = "line", name = "High Local Load: ", color = "lightgreen") %>%
      hc_add_series(pei_loc_dat_ts_daily()[,(colnames(pei_loc_dat_ts_daily()) %in% c('pei_loc_subset_ts().Low'))], type = "area", name = "Low Local Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_LOCAL_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_loc_dat_ts_hourly()[,(colnames(pei_loc_dat_ts_hourly()) %in% c('pei_loc_subset_ts().High'))], type = "line", name = "High Local Load: ", color = "lightgreen") %>%
      hc_add_series(pei_loc_dat_ts_hourly()[,(colnames(pei_loc_dat_ts_hourly()) %in% c('pei_loc_subset_ts().Low'))], type = "spline", name = "Low Local Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_LOCAL_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(pei_loc_subset_ts(), type = "spline", name = "Local Load: ", color = "brown")
  })
  
  nb_date_ind_1_1 <- reactive({paste(input$nb_dates_1[1],"00:00:00",sep = " ")})
  nb_date_ind_1_2 <- reactive({paste(input$nb_dates_1[2],"00:00:00",sep = " ")})
  nb_ind_subset_dat <- reactive({subset(nb_ind_dat,subset = (nb_ind_dat$Date_time_local >= nb_date_ind_1_1() & nb_ind_dat$Date_time_local <= nb_date_ind_1_2()))})
  nb_ind_dat_ts <- reactive({xts(nb_ind_dat$nb_load,nb_ind_dat$Date_time_local)})
  nb_ind_subset_ts <- reactive({xts(nb_ind_subset_dat()$nb_load,nb_ind_subset_dat()$Date_time_local)})
  nb_ind_dat_ts_yearly <- reactive({to.yearly(nb_ind_dat_ts())})
  nb_ind_dat_ts_monthly <- reactive({to.monthly(nb_ind_subset_ts())})
  nb_ind_dat_ts_weekly <- reactive({to.weekly(nb_ind_subset_ts())})
  nb_ind_dat_ts_daily <- reactive({to.daily(nb_ind_subset_ts())})
  nb_ind_dat_ts_hourly <- reactive({to.hourly(nb_ind_subset_ts())})
  output$NB_LOAD_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_ind_dat_ts_yearly()[,(colnames(nb_ind_dat_ts_yearly()) %in% c('nb_ind_dat_ts().High'))], type = "line", name = "High Load: ", color = "lightgreen") %>%
      hc_add_series(nb_ind_dat_ts_yearly()[,(colnames(nb_ind_dat_ts_yearly()) %in% c('nb_ind_dat_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_LOAD_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_ind_dat_ts_weekly()[,(colnames(nb_ind_dat_ts_weekly()) %in% c('nb_ind_subset_ts().High'))], type = "line", name = "High Load: ", color = "lightgreen") %>%
      hc_add_series(nb_ind_dat_ts_weekly()[,(colnames(nb_ind_dat_ts_weekly()) %in% c('nb_ind_subset_ts().Low'))], type = "area", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_LOAD_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_ind_dat_ts_daily()[,(colnames(nb_ind_dat_ts_daily()) %in% c('nb_ind_subset_ts().High'))], type = "line", name = "High Load: ", color = "lightgreen") %>%
      hc_add_series(nb_ind_dat_ts_daily()[,(colnames(nb_ind_dat_ts_daily()) %in% c('nb_ind_subset_ts().Low'))], type = "area", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_LOAD_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_ind_dat_ts_hourly()[,(colnames(nb_ind_dat_ts_hourly()) %in% c('nb_ind_subset_ts().High'))], type = "line", name = "High Load: ", color = "lightgreen") %>%
      hc_add_series(nb_ind_dat_ts_hourly()[,(colnames(nb_ind_dat_ts_hourly()) %in% c('nb_ind_subset_ts().Low'))], type = "spline", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_LOAD_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(nb_ind_subset_ts(), type = "line", name = "High Load: ", color = "green")
  })
  
  nb_date_ind_2_1 <- reactive({paste(input$nb_dates_2[1],"00:00:00",sep = " ")})
  nb_date_ind_2_2 <- reactive({paste(input$nb_dates_2[2],"00:00:00",sep = " ")})
  nb_dem_subset_dat <- reactive({subset(nb_ind_dat,subset = (nb_ind_dat$Date_time_local >= nb_date_ind_2_1() & nb_ind_dat$Date_time_local <= nb_date_ind_2_2()))})
  nb_dem_dat_ts <- reactive({xts(nb_ind_dat$nb_demand,nb_ind_dat$Date_time_local)})
  nb_dem_subset_ts <- reactive({xts(nb_dem_subset_dat()$nb_demand,nb_dem_subset_dat()$Date_time_local)})
  nb_dem_dat_ts_yearly <- reactive({to.yearly(nb_dem_dat_ts())})
  nb_dem_dat_ts_monthly <- reactive({to.monthly(nb_dem_subset_ts())})
  nb_dem_dat_ts_weekly <- reactive({to.weekly(nb_dem_subset_ts())})
  nb_dem_dat_ts_daily <- reactive({to.daily(nb_dem_subset_ts())})
  nb_dem_dat_ts_hourly <- reactive({to.hourly(nb_dem_subset_ts())})
  
  output$NB_DEMAND_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_dem_dat_ts_yearly()[,(colnames(nb_dem_dat_ts_yearly()) %in% c('nb_dem_dat_ts().High'))], type = "line", name = "High Demand: ", color = "lightgreen") %>%
      hc_add_series(nb_dem_dat_ts_yearly()[,(colnames(nb_dem_dat_ts_yearly()) %in% c('nb_dem_dat_ts().Low'))], type = "line", name = "Low Demand: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_DEMAND_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_dem_dat_ts_weekly()[,(colnames(nb_dem_dat_ts_weekly()) %in% c('nb_dem_subset_ts().High'))], type = "line", name = "High Demand: ", color = "lightgreen") %>%
      hc_add_series(nb_dem_dat_ts_weekly()[,(colnames(nb_dem_dat_ts_weekly()) %in% c('nb_dem_subset_ts().Low'))], type = "area", name = "Low Demand: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_DEMAND_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_dem_dat_ts_daily()[,(colnames(nb_dem_dat_ts_daily()) %in% c('nb_dem_subset_ts().High'))], type = "line", name = "High Demand: ", color = "lightgreen") %>%
      hc_add_series(nb_dem_dat_ts_daily()[,(colnames(nb_dem_dat_ts_daily()) %in% c('nb_dem_subset_ts().Low'))], type = "area", name = "Low Demand: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_DEMAND_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_dem_dat_ts_hourly()[,(colnames(nb_dem_dat_ts_hourly()) %in% c('nb_dem_subset_ts().High'))], type = "line", name = "High Demand: ", color = "lightgreen") %>%
      hc_add_series(nb_dem_dat_ts_hourly()[,(colnames(nb_dem_dat_ts_hourly()) %in% c('nb_dem_subset_ts().Low'))], type = "spline", name = "Low Demand: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_DEMAND_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(nb_dem_subset_ts(), type = "area", name = "Demand: ", color = "purple")
  })
  
  nb_date_ind_3_1 <- reactive({paste(input$nb_dates_3[1],"00:00:00",sep = " ")})
  nb_date_ind_3_2 <- reactive({paste(input$nb_dates_3[2],"00:00:00",sep = " ")})
  nb_10_subset_dat <- reactive({subset(nb_ind_dat,subset = (nb_ind_dat$Date_time_local >= nb_date_ind_3_1() & nb_ind_dat$Date_time_local <= nb_date_ind_3_2()))})
  nb_10_dat_ts <- reactive({xts(nb_ind_dat$min_10_reserve_margin,nb_ind_dat$Date_time_local)})
  nb_10_subset_ts <- reactive({xts(nb_10_subset_dat()$min_10_reserve_margin,nb_10_subset_dat()$Date_time_local)})
  nb_10_dat_ts_yearly <- reactive({to.yearly(nb_10_dat_ts())})
  nb_10_dat_ts_monthly <- reactive({to.monthly(nb_10_subset_ts())})
  nb_10_dat_ts_weekly <- reactive({to.weekly(nb_10_subset_ts())})
  nb_10_dat_ts_daily <- reactive({to.daily(nb_10_subset_ts())})
  nb_10_dat_ts_hourly <- reactive({to.hourly(nb_10_subset_ts())})
  
  output$NB_RESERVE_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_10_dat_ts_yearly()[,(colnames(nb_10_dat_ts_yearly()) %in% c('nb_10_dat_ts().High'))], type = "line", name = "High Reserve: ", color = "lightgreen") %>%
      hc_add_series(nb_10_dat_ts_yearly()[,(colnames(nb_10_dat_ts_yearly()) %in% c('nb_10_dat_ts().Low'))], type = "line", name = "Low Reserve: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_RESERVE_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_10_dat_ts_weekly()[,(colnames(nb_10_dat_ts_weekly()) %in% c('nb_10_subset_ts().High'))], type = "line", name = "High Reserve: ", color = "lightgreen") %>%
      hc_add_series(nb_10_dat_ts_weekly()[,(colnames(nb_10_dat_ts_weekly()) %in% c('nb_10_subset_ts().Low'))], type = "area", name = "Low Reserve: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_RESERVE_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_10_dat_ts_daily()[,(colnames(nb_10_dat_ts_daily()) %in% c('nb_10_subset_ts.High'))], type = "line", name = "High Reserve: ", color = "lightgreen") %>%
      hc_add_series(nb_10_dat_ts_daily()[,(colnames(nb_10_dat_ts_daily()) %in% c('nb_10_subset_ts.Low'))], type = "area", name = "Low Reserve: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_RESERVE_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_10_dat_ts_hourly()[,(colnames(nb_10_dat_ts_hourly()) %in% c('nb_10_subset_ts.High'))], type = "line", name = "High Reserve: ", color = "lightgreen") %>%
      hc_add_series(nb_10_dat_ts_hourly()[,(colnames(nb_10_dat_ts_hourly()) %in% c('nb_10_subset_ts.Low'))], type = "area", name = "Low Reserve: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_RESERVE_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      #hc_loading() %>%
      hc_add_series(nb_10_subset_ts(), type = "lollipop", name = "Reserve Load: ", color = "orange")
  })
  
  nb_date_ind_4_1 <- reactive({paste(input$nb_dates_4[1],"00:00:00",sep = " ")})
  nb_date_ind_4_2 <- reactive({paste(input$nb_dates_4[2],"00:00:00",sep = " ")})
  nb_30_subset_dat <- reactive({subset(nb_ind_dat,subset = (nb_ind_dat$Date_time_local >= nb_date_ind_4_1() & nb_ind_dat$Date_time_local <= nb_date_ind_4_2()))})
  nb_30_dat_ts <- reactive({xts(nb_ind_dat$min_30_reserve_margin,nb_ind_dat$Date_time_local)})
  nb_30_subset_ts <- reactive({xts(nb_30_subset_dat()$min_30_reserve_margin,nb_30_subset_dat()$Date_time_local)})
  nb_30_dat_ts_yearly <- reactive({to.yearly(nb_30_dat_ts())})
  nb_30_dat_ts_monthly <- reactive({to.monthly(nb_30_subset_ts())})
  nb_30_dat_ts_weekly <- reactive({to.weekly(nb_30_subset_ts())})
  nb_30_dat_ts_daily <- reactive({to.daily(nb_30_subset_ts())})
  nb_30_dat_ts_hourly <- reactive({to.hourly(nb_30_subset_ts())})
  
  output$NB_RESERVE2_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_30_dat_ts_yearly()[,(colnames(nb_30_dat_ts_yearly()) %in% c('nb_30_dat_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(nb_30_dat_ts_yearly()[,(colnames(nb_30_dat_ts_yearly()) %in% c('nb_30_dat_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_RESERVE2_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_30_dat_ts_weekly()[,(colnames(nb_30_dat_ts_weekly()) %in% c('nb_30_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(nb_30_dat_ts_weekly()[,(colnames(nb_30_dat_ts_weekly()) %in% c('nb_30_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_RESERVE2_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_30_dat_ts_daily()[,(colnames(nb_30_dat_ts_daily()) %in% c('nb_30_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(nb_30_dat_ts_daily()[,(colnames(nb_30_dat_ts_daily()) %in% c('nb_30_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_RESERVE2_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_30_dat_ts_hourly()[,(colnames(nb_30_dat_ts_hourly()) %in% c('nb_30_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(nb_30_dat_ts_hourly()[,(colnames(nb_30_dat_ts_hourly()) %in% c('nb_30_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_RESERVE2_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(nb_30_subset_ts(), type = "line", name = "High Load: ", color = "green")
  })
  
  nfl_date_ind_1_1 <- reactive({paste(input$nfl_dates_1[1],"00:00:00",sep = " ")})
  nfl_date_ind_1_2 <- reactive({paste(input$nfl_dates_1[2],"00:00:00",sep = " ")})
  nfl_load_subset_dat <- reactive({subset(nfl_ind_dat,subset = (nfl_ind_dat$Date_time_local >= nfl_date_ind_1_1() & nfl_ind_dat$Date_time_local <= nfl_date_ind_1_2()))})
  nfl_load_dat_ts <- reactive({xts(nfl_ind_dat$Net_Load_MW,nfl_ind_dat$Date_time_local)})
  nfl_load_subset_ts <- reactive({xts(nfl_load_subset_dat()$Net_Load_MW,nfl_load_subset_dat()$Date_time_local)})
  nfl_load_dat_ts_yearly <- reactive({to.yearly(nfl_load_dat_ts())})
  nfl_load_dat_ts_monthly <- reactive({to.monthly(nfl_load_subset_ts())})
  nfl_load_dat_ts_weekly <- reactive({to.weekly(nfl_load_subset_ts())})
  nfl_load_dat_ts_daily <- reactive({to.daily(nfl_load_subset_ts())})
  nfl_load_dat_ts_hourly <- reactive({to.hourly(nfl_load_subset_ts())})
  
  output$NFL_LOAD_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nfl_load_dat_ts_yearly()[,(colnames(nfl_load_dat_ts_yearly()) %in% c('nfl_load_dat_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(nfl_load_dat_ts_yearly()[,(colnames(nfl_load_dat_ts_yearly()) %in% c('nfl_load_dat_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NFL_LOAD_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nfl_load_dat_ts_weekly()[,(colnames(nfl_load_dat_ts_weekly()) %in% c('nfl_load_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(nfl_load_dat_ts_weekly()[,(colnames(nfl_load_dat_ts_weekly()) %in% c('nfl_load_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NFL_LOAD_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nfl_load_dat_ts_daily()[,(colnames(nfl_load_dat_ts_daily()) %in% c('nfl_load_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(nfl_load_dat_ts_daily()[,(colnames(nfl_load_dat_ts_daily()) %in% c('nfl_load_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NFL_LOAD_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nfl_load_dat_ts_hourly()[,(colnames(nfl_load_dat_ts_hourly()) %in% c('nfl_load_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(nfl_load_dat_ts_hourly()[,(colnames(nfl_load_dat_ts_hourly()) %in% c('nfl_load_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NFL_LOAD_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(nfl_load_subset_ts(), type = "line", name = "High Load: ", color = "green")
  })
  
  bc_date_ind_1_1 <- reactive({paste(input$bc_dates_1[1],"00:00:00",sep = " ")})
  bc_date_ind_1_2 <- reactive({paste(input$bc_dates_1[2],"00:00:00",sep = " ")})
  bc_date_ind_2_1 <- reactive({paste(input$bc_dates_2[1],"00:00:00",sep = " ")})
  bc_date_ind_2_2 <- reactive({paste(input$bc_dates_2[2],"00:00:00",sep = " ")})
  bc_date_ind_3_1 <- reactive({paste(input$bc_dates_3[1],"00:00:00",sep = " ")})
  bc_date_ind_3_2 <- reactive({paste(input$bc_dates_3[2],"00:00:00",sep = " ")})
  
  bc_load_subset_dat_1 <- reactive({subset(bc_ind_dat,subset = (bc_ind_dat$Date_time_local >= bc_date_ind_1_1() & bc_ind_dat$Date_time_local <= bc_date_ind_1_2()))})
  bc_load_subset_dat_2 <- reactive({subset(bc_ind_dat_1,subset = (bc_ind_dat_1$Date_time_local >= bc_date_ind_2_1() & bc_ind_dat_1$Date_time_local <= bc_date_ind_2_2()))})
  bc_load_subset_dat_3 <- reactive({subset(bc_ind_dat_1,subset = (bc_ind_dat_1$Date_time_local >= bc_date_ind_3_1() & bc_ind_dat_1$Date_time_local <= bc_date_ind_3_2()))})
  
  bc_load_dat_ts_1 <- reactive({xts(bc_ind_dat$Balancing_Authority_Load,bc_ind_dat$Date_time_local)})
  bc_load_dat_ts_2 <- reactive({xts(bc_ind_dat_1$BC_US_net_actual_flow,bc_ind_dat_1$Date_time_local)})
  bc_load_dat_ts_3 <- reactive({xts(bc_ind_dat_1$BC_AB_net_actual_flow,bc_ind_dat_1$Date_time_local)})
  
  bc_load_subset_ts_1 <- reactive({xts(bc_load_subset_dat_1()$Balancing_Authority_Load,bc_load_subset_dat_1()$Date_time_local)})
  bc_load_subset_ts_2 <- reactive({xts(bc_load_subset_dat_2()$BC_US_net_actual_flow,bc_load_subset_dat_2()$Date_time_local)})
  bc_load_subset_ts_3 <- reactive({xts(bc_load_subset_dat_3()$BC_AB_net_actual_flow,bc_load_subset_dat_3()$Date_time_local)})
  
  
  bc_load_dat_ts_1_yearly <- reactive({to.yearly(bc_load_dat_ts_1())})
  bc_load_dat_ts_1_monthly <- reactive({to.monthly(bc_load_subset_ts_1())})
  bc_load_dat_ts_1_weekly <- reactive({to.weekly(bc_load_subset_ts_1())})
  bc_load_dat_ts_1_daily <- reactive({to.daily(bc_load_subset_ts_1())})
  bc_load_dat_ts_1_hourly <- reactive({to.hourly(bc_load_subset_ts_1())})
  
  bc_load_dat_ts_2_yearly <- reactive({to.yearly(bc_load_dat_ts_2())})
  bc_load_dat_ts_2_monthly <- reactive({to.monthly(bc_load_subset_ts_2())})
  bc_load_dat_ts_2_weekly <- reactive({to.weekly(bc_load_subset_ts_2())})
  bc_load_dat_ts_2_daily <- reactive({to.daily(bc_load_subset_ts_2())})
  bc_load_dat_ts_2_hourly <- reactive({to.hourly(bc_load_subset_ts_2())})
  
  bc_load_dat_ts_3_yearly <- reactive({to.yearly(bc_load_dat_ts_3())})
  bc_load_dat_ts_3_monthly <- reactive({to.monthly(bc_load_subset_ts_3())})
  bc_load_dat_ts_3_weekly <- reactive({to.weekly(bc_load_subset_ts_3())})
  bc_load_dat_ts_3_daily <- reactive({to.daily(bc_load_subset_ts_3())})
  bc_load_dat_ts_3_hourly <- reactive({to.hourly(bc_load_subset_ts_3())})
  
  output$BC_LOAD_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(bc_load_dat_ts_1_yearly()[,(colnames(bc_load_dat_ts_1_yearly()) %in% c('bc_load_dat_ts_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(bc_load_dat_ts_1_yearly()[,(colnames(bc_load_dat_ts_1_yearly()) %in% c('bc_load_dat_ts_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$BC_LOAD_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(bc_load_dat_ts_1_weekly()[,(colnames(bc_load_dat_ts_1_weekly()) %in% c('bc_load_subset_ts_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(bc_load_dat_ts_1_weekly()[,(colnames(bc_load_dat_ts_1_weekly()) %in% c('bc_load_subset_ts_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$BC_LOAD_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(bc_load_dat_ts_1_daily()[,(colnames(bc_load_dat_ts_1_daily()) %in% c('bc_load_subset_ts_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(bc_load_dat_ts_1_daily()[,(colnames(bc_load_dat_ts_1_daily()) %in% c('bc_load_subset_ts_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$BC_LOAD_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(bc_load_dat_ts_1_hourly()[,(colnames(bc_load_dat_ts_1_hourly()) %in% c('bc_load_subset_ts_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(bc_load_dat_ts_1_hourly()[,(colnames(bc_load_dat_ts_1_hourly()) %in% c('bc_load_subset_ts_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$BC_LOAD_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(bc_load_subset_ts_1(), type = "line", name = "High Load: ", color = "green")
  })
  
  output$BC_US_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(bc_load_dat_ts_2_yearly()[,(colnames(bc_load_dat_ts_2_yearly()) %in% c('bc_load_dat_ts_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(bc_load_dat_ts_2_yearly()[,(colnames(bc_load_dat_ts_2_yearly()) %in% c('bc_load_dat_ts_2().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$BC_US_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(bc_load_dat_ts_2_weekly()[,(colnames(bc_load_dat_ts_2_weekly()) %in% c('bc_load_subset_ts_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(bc_load_dat_ts_2_weekly()[,(colnames(bc_load_dat_ts_2_weekly()) %in% c('bc_load_subset_ts_2().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$BC_US_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(bc_load_dat_ts_2_daily()[,(colnames(bc_load_dat_ts_2_daily()) %in% c('bc_load_subset_ts_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(bc_load_dat_ts_2_daily()[,(colnames(bc_load_dat_ts_2_daily()) %in% c('bc_load_subset_ts_2().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$BC_US_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(bc_load_dat_ts_2_hourly()[,(colnames(bc_load_dat_ts_2_hourly()) %in% c('bc_load_subset_ts_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(bc_load_dat_ts_2_hourly()[,(colnames(bc_load_dat_ts_2_hourly()) %in% c('bc_load_subset_ts_2().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$BC_US_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(bc_load_subset_ts_2(), type = "line", name = "High Load: ", color = "green")
  })
  
  output$BC_AB_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(bc_load_dat_ts_3_yearly()[,(colnames(bc_load_dat_ts_3_yearly()) %in% c('bc_load_dat_ts_3().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(bc_load_dat_ts_3_yearly()[,(colnames(bc_load_dat_ts_3_yearly()) %in% c('bc_load_dat_ts_3().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$BC_AB_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(bc_load_dat_ts_3_weekly()[,(colnames(bc_load_dat_ts_3_weekly()) %in% c('bc_load_subset_ts_3().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(bc_load_dat_ts_3_weekly()[,(colnames(bc_load_dat_ts_3_weekly()) %in% c('bc_load_subset_ts_3().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$BC_AB_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(bc_load_dat_ts_3_daily()[,(colnames(bc_load_dat_ts_3_daily()) %in% c('bc_load_subset_ts_3().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(bc_load_dat_ts_3_daily()[,(colnames(bc_load_dat_ts_3_daily()) %in% c('bc_load_subset_ts_3().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$BC_AB_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(bc_load_dat_ts_3_hourly()[,(colnames(bc_load_dat_ts_3_hourly()) %in% c('bc_load_subset_ts_3().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(bc_load_dat_ts_3_hourly()[,(colnames(bc_load_dat_ts_3_hourly()) %in% c('bc_load_subset_ts_3().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$BC_AB_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(bc_load_subset_ts_3(), type = "line", name = "High Load: ", color = "green")
  })
  
  ns_date_ind_1_1 <- reactive({paste(input$ns_dates_1[1],"00:00:00",sep = " ")})
  ns_date_ind_1_2 <- reactive({paste(input$ns_dates_1[2],"00:00:00",sep = " ")})
  ns_load_subset_dat <- reactive({subset(ns_ind_dat,subset = (ns_ind_dat$Date_time_local >= ns_date_ind_1_1() & ns_ind_dat$Date_time_local <= ns_date_ind_1_2()))})
  ns_load_dat_ts <- reactive({xts(ns_ind_dat$Net_Load,ns_ind_dat$Date_time_local)})
  ns_load_subset_ts <- reactive({xts(ns_load_subset_dat()$Net_Load,ns_load_subset_dat()$Date_time_local)})
  ns_load_dat_ts_yearly <- reactive({to.yearly(ns_load_dat_ts())})
  ns_load_dat_ts_monthly <- reactive({to.monthly(ns_load_subset_ts())})
  ns_load_dat_ts_weekly <- reactive({to.weekly(ns_load_subset_ts())})
  ns_load_dat_ts_daily <- reactive({to.daily(ns_load_subset_ts())})
  ns_load_dat_ts_hourly <- reactive({to.hourly(ns_load_subset_ts())})
  
  output$NS_LOAD_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_load_dat_ts_yearly()[,(colnames(ns_load_dat_ts_yearly()) %in% c('ns_load_dat_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_load_dat_ts_yearly()[,(colnames(ns_load_dat_ts_yearly()) %in% c('ns_load_dat_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_LOAD_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_load_dat_ts_weekly()[,(colnames(ns_load_dat_ts_weekly()) %in% c('ns_load_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_load_dat_ts_weekly()[,(colnames(ns_load_dat_ts_weekly()) %in% c('ns_load_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_LOAD_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_load_dat_ts_daily()[,(colnames(ns_load_dat_ts_daily()) %in% c('ns_load_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_load_dat_ts_daily()[,(colnames(ns_load_dat_ts_daily()) %in% c('ns_load_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_LOAD_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_load_dat_ts_hourly()[,(colnames(ns_load_dat_ts_hourly()) %in% c('ns_load_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_load_dat_ts_hourly()[,(colnames(ns_load_dat_ts_hourly()) %in% c('ns_load_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_LOAD_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(ns_load_subset_ts(), type = "line", name = "High Load: ", color = "green")
  })
  
  ns_date_ind_2_1 <- reactive({paste(input$ns_dates_2[1],"00:00:00",sep = " ")})
  ns_date_ind_2_2 <- reactive({paste(input$ns_dates_2[2],"00:00:00",sep = " ")})
  ns_date_ind_3_1 <- reactive({paste(input$ns_dates_3[1],"00:00:00",sep = " ")})
  ns_date_ind_3_2 <- reactive({paste(input$ns_dates_3[2],"00:00:00",sep = " ")})
  ns_date_ind_4_1 <- reactive({paste(input$ns_dates_4[1],"00:00:00",sep = " ")})
  ns_date_ind_4_2 <- reactive({paste(input$ns_dates_4[2],"00:00:00",sep = " ")})
  ns_date_ind_5_1 <- reactive({paste(input$ns_dates_5[1],"00:00:00",sep = " ")})
  ns_date_ind_5_2 <- reactive({paste(input$ns_dates_5[2],"00:00:00",sep = " ")})
  
  ns_exp_subset_dat_1 <- reactive({subset(ns_ind_dat_1,subset = (ns_ind_dat_1$Date_time_local >= ns_date_ind_2_1() & ns_ind_dat_1$Date_time_local <= ns_date_ind_2_2()))})
  ns_exp_subset_dat_2 <- reactive({subset(ns_ind_dat_1,subset = (ns_ind_dat_1$Date_time_local >= ns_date_ind_3_1() & ns_ind_dat_1$Date_time_local <= ns_date_ind_3_2()))})
  ns_exp_subset_dat_3 <- reactive({subset(ns_ind_dat_1,subset = (ns_ind_dat_1$Date_time_local >= ns_date_ind_4_1() & ns_ind_dat_1$Date_time_local <= ns_date_ind_4_2()))})
  ns_exp_subset_dat_4 <- reactive({subset(ns_ind_dat_1,subset = (ns_ind_dat_1$Date_time_local >= ns_date_ind_5_1() & ns_ind_dat_1$Date_time_local <= ns_date_ind_5_2()))})
  
  ns_exp_dat_ts_1 <- reactive({xts(ns_ind_dat_1$Highlands_Export,ns_ind_dat_1$Date_time_local)})
  ns_exp_dat_ts_2 <- reactive({xts(ns_ind_dat_1$East_End_Export_At_Sydney,ns_ind_dat_1$Date_time_local)})
  ns_exp_dat_ts_3 <- reactive({xts(ns_ind_dat_1$East_End_Export_At_East_Bay,ns_ind_dat_1$Date_time_local)})
  ns_exp_dat_ts_4 <- reactive({xts(ns_ind_dat_1$Cape_Breton_Export,ns_ind_dat_1$Date_time_local)})
  ns_exp_dat_ts_5 <- reactive({xts(ns_ind_dat_1$Onslow_Import,ns_ind_dat_1$Date_time_local)})
  ns_exp_dat_ts_6 <- reactive({xts(ns_ind_dat_1$NS_Export,ns_ind_dat_1$Date_time_local)})
  ns_exp_dat_ts_7 <- reactive({xts(ns_ind_dat_1$Onslow_South,ns_ind_dat_1$Date_time_local)})
  ns_exp_dat_ts_8 <- reactive({xts(ns_ind_dat_1$Flow_Into_Metro,ns_ind_dat_1$Date_time_local)})
  ns_exp_dat_ts_9 <- reactive({xts(ns_ind_dat_1$Western_Import,ns_ind_dat_1$Date_time_local)})
  ns_exp_dat_ts_10 <- reactive({xts(ns_ind_dat_1$Valley_Import,ns_ind_dat_1$Date_time_local)})
  ns_exp_dat_ts_11 <- reactive({xts(ns_ind_dat_1$Maritime_Link_Import,ns_ind_dat_1$Date_time_local)})
  
  ns_exp_subset_ts_1 <- reactive({xts(ns_exp_subset_dat_1()$Highlands_Export,ns_exp_subset_dat_1()$Date_time_local)})
  ns_exp_subset_ts_2 <- reactive({xts(ns_exp_subset_dat_1()$East_End_Export_At_Sydney,ns_exp_subset_dat_1()$Date_time_local)})
  ns_exp_subset_ts_3 <- reactive({xts(ns_exp_subset_dat_1()$East_End_Export_At_East_Bay,ns_exp_subset_dat_1()$Date_time_local)})
  
  ns_exp_subset_ts_4 <- reactive({xts(ns_exp_subset_dat_2()$Cape_Breton_Export,ns_exp_subset_dat_2()$Date_time_local)})
  ns_exp_subset_ts_5 <- reactive({xts(ns_exp_subset_dat_2()$NS_Export,ns_exp_subset_dat_2()$Date_time_local)})
  ns_exp_subset_ts_6 <- reactive({xts(ns_exp_subset_dat_2()$Flow_Into_Metro,ns_exp_subset_dat_2()$Date_time_local)})
  
  ns_exp_subset_ts_7 <- reactive({xts(ns_exp_subset_dat_3()$Western_Import,ns_exp_subset_dat_3()$Date_time_local)})
  ns_exp_subset_ts_8 <- reactive({xts(ns_exp_subset_dat_3()$Valley_Import,ns_exp_subset_dat_3()$Date_time_local)})
  ns_exp_subset_ts_9 <- reactive({xts(ns_exp_subset_dat_3()$Maritime_Link_Import,ns_exp_subset_dat_3()$Date_time_local)})
  
  ns_exp_subset_ts_10 <- reactive({xts(ns_exp_subset_dat_4()$Onslow_Import,ns_exp_subset_dat_4()$Date_time_local)})
  ns_exp_subset_ts_11 <- reactive({xts(ns_exp_subset_dat_4()$Onslow_South,ns_exp_subset_dat_4()$Date_time_local)})
  
  ns_exp_dat_ts_1_yearly <- reactive({to.yearly(ns_exp_dat_ts_1())})
  ns_exp_dat_ts_1_monthly <- reactive({to.monthly(ns_exp_subset_ts_1())})
  ns_exp_dat_ts_1_weekly <- reactive({to.weekly(ns_exp_subset_ts_1())})
  ns_exp_dat_ts_1_daily <- reactive({to.daily(ns_exp_subset_ts_1())})
  ns_exp_dat_ts_1_hourly <- reactive({to.hourly(ns_exp_subset_ts_1())})
  
  ns_exp_dat_ts_2_yearly <- reactive({to.yearly(ns_exp_dat_ts_2())})
  ns_exp_dat_ts_2_monthly <- reactive({to.monthly(ns_exp_subset_ts_2())})
  ns_exp_dat_ts_2_weekly <- reactive({to.weekly(ns_exp_subset_ts_2())})
  ns_exp_dat_ts_2_daily <- reactive({to.daily(ns_exp_subset_ts_2())})
  ns_exp_dat_ts_2_hourly <- reactive({to.hourly(ns_exp_subset_ts_2())})
  
  ns_exp_dat_ts_3_yearly <- reactive({to.yearly(ns_exp_dat_ts_3())})
  ns_exp_dat_ts_3_monthly <- reactive({to.monthly(ns_exp_subset_ts_3())})
  ns_exp_dat_ts_3_weekly <- reactive({to.weekly(ns_exp_subset_ts_3())})
  ns_exp_dat_ts_3_daily <- reactive({to.daily(ns_exp_subset_ts_3())})
  ns_exp_dat_ts_3_hourly <- reactive({to.hourly(ns_exp_subset_ts_3())})
  
  ns_exp_dat_ts_4_yearly <- reactive({to.yearly(ns_exp_dat_ts_4())})
  ns_exp_dat_ts_4_monthly <- reactive({to.monthly(ns_exp_subset_ts_4())})
  ns_exp_dat_ts_4_weekly <- reactive({to.weekly(ns_exp_subset_ts_4())})
  ns_exp_dat_ts_4_daily <- reactive({to.daily(ns_exp_subset_ts_4())})
  ns_exp_dat_ts_4_hourly <- reactive({to.hourly(ns_exp_subset_ts_4())})
  
  ns_exp_dat_ts_5_yearly <- reactive({to.yearly(ns_exp_dat_ts_5())})
  ns_exp_dat_ts_5_monthly <- reactive({to.monthly(ns_exp_subset_ts_5())})
  ns_exp_dat_ts_5_weekly <- reactive({to.weekly(ns_exp_subset_ts_5())})
  ns_exp_dat_ts_5_daily <- reactive({to.daily(ns_exp_subset_ts_5())})
  ns_exp_dat_ts_5_hourly <- reactive({to.hourly(ns_exp_subset_ts_5())})
  
  ns_exp_dat_ts_6_yearly <- reactive({to.yearly(ns_exp_dat_ts_6())})
  ns_exp_dat_ts_6_monthly <- reactive({to.monthly(ns_exp_subset_ts_6())})
  ns_exp_dat_ts_6_weekly <- reactive({to.weekly(ns_exp_subset_ts_6())})
  ns_exp_dat_ts_6_daily <- reactive({to.daily(ns_exp_subset_ts_6())})
  ns_exp_dat_ts_6_hourly <- reactive({to.hourly(ns_exp_subset_ts_6())})
  
  ns_exp_dat_ts_7_yearly <- reactive({to.yearly(ns_exp_dat_ts_7())})
  ns_exp_dat_ts_7_monthly <- reactive({to.monthly(ns_exp_subset_ts_7())})
  ns_exp_dat_ts_7_weekly <- reactive({to.weekly(ns_exp_subset_ts_7())})
  ns_exp_dat_ts_7_daily <- reactive({to.daily(ns_exp_subset_ts_7())})
  ns_exp_dat_ts_7_hourly <- reactive({to.hourly(ns_exp_subset_ts_7())})
  
  ns_exp_dat_ts_8_yearly <- reactive({to.yearly(ns_exp_dat_ts_8())})
  ns_exp_dat_ts_8_monthly <- reactive({to.monthly(ns_exp_subset_ts_8())})
  ns_exp_dat_ts_8_weekly <- reactive({to.weekly(ns_exp_subset_ts_8())})
  ns_exp_dat_ts_8_daily <- reactive({to.daily(ns_exp_subset_ts_8())})
  ns_exp_dat_ts_8_hourly <- reactive({to.hourly(ns_exp_subset_ts_8())})
  
  ns_exp_dat_ts_9_yearly <- reactive({to.yearly(ns_exp_dat_ts_9())})
  ns_exp_dat_ts_9_monthly <- reactive({to.monthly(ns_exp_subset_ts_9())})
  ns_exp_dat_ts_9_weekly <- reactive({to.weekly(ns_exp_subset_ts_9())})
  ns_exp_dat_ts_9_daily <- reactive({to.daily(ns_exp_subset_ts_9())})
  ns_exp_dat_ts_9_hourly <- reactive({to.hourly(ns_exp_subset_ts_9())})
  
  ns_exp_dat_ts_10_yearly <- reactive({to.yearly(ns_exp_dat_ts_10())})
  ns_exp_dat_ts_10_monthly <- reactive({to.monthly(ns_exp_subset_ts_10())})
  ns_exp_dat_ts_10_weekly <- reactive({to.weekly(ns_exp_subset_ts_10())})
  ns_exp_dat_ts_10_daily <- reactive({to.daily(ns_exp_subset_ts_10())})
  ns_exp_dat_ts_10_hourly <- reactive({to.hourly(ns_exp_subset_ts_10())})
  
  ns_exp_dat_ts_11_yearly <- reactive({to.yearly(ns_exp_dat_ts_11())})
  ns_exp_dat_ts_11_monthly <- reactive({to.monthly(ns_exp_subset_ts_11())})
  ns_exp_dat_ts_11_weekly <- reactive({to.weekly(ns_exp_subset_ts_11())})
  ns_exp_dat_ts_11_daily <- reactive({to.daily(ns_exp_subset_ts_11())})
  ns_exp_dat_ts_11_hourly <- reactive({to.hourly(ns_exp_subset_ts_11())})
  
  output$NS_EXP_1_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_1_yearly()[,(colnames(ns_exp_dat_ts_1_yearly()) %in% c('ns_exp_dat_ts_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_1_yearly()[,(colnames(ns_exp_dat_ts_1_yearly()) %in% c('ns_exp_dat_ts_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_2_yearly()[,(colnames(ns_exp_dat_ts_2_yearly()) %in% c('ns_exp_dat_ts_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_2_yearly()[,(colnames(ns_exp_dat_ts_2_yearly()) %in% c('ns_exp_dat_ts_2().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_3_yearly()[,(colnames(ns_exp_dat_ts_3_yearly()) %in% c('ns_exp_dat_ts_3().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_3_yearly()[,(colnames(ns_exp_dat_ts_3_yearly()) %in% c('ns_exp_dat_ts_3().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_1_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_1_weekly()[,(colnames(ns_exp_dat_ts_1_weekly()) %in% c('ns_exp_subset_ts_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_1_weekly()[,(colnames(ns_exp_dat_ts_1_weekly()) %in% c('ns_exp_subset_ts_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_2_weekly()[,(colnames(ns_exp_dat_ts_2_weekly()) %in% c('ns_exp_subset_ts_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_2_weekly()[,(colnames(ns_exp_dat_ts_2_weekly()) %in% c('ns_exp_subset_ts_2().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_3_weekly()[,(colnames(ns_exp_dat_ts_3_weekly()) %in% c('ns_exp_subset_ts_3().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_3_weekly()[,(colnames(ns_exp_dat_ts_3_weekly()) %in% c('ns_exp_subset_ts_3().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_1_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_1_daily()[,(colnames(ns_exp_dat_ts_1_daily()) %in% c('ns_exp_subset_ts_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_1_daily()[,(colnames(ns_exp_dat_ts_1_daily()) %in% c('ns_exp_subset_ts_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_2_daily()[,(colnames(ns_exp_dat_ts_2_daily()) %in% c('ns_exp_subset_ts_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_2_daily()[,(colnames(ns_exp_dat_ts_2_daily()) %in% c('ns_exp_subset_ts_2().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_3_daily()[,(colnames(ns_exp_dat_ts_3_daily()) %in% c('ns_exp_subset_ts_3().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_3_daily()[,(colnames(ns_exp_dat_ts_3_daily()) %in% c('ns_exp_subset_ts_3().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_1_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_1_hourly()[,(colnames(ns_exp_dat_ts_1_hourly()) %in% c('ns_exp_subset_ts_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_1_hourly()[,(colnames(ns_exp_dat_ts_1_hourly()) %in% c('ns_exp_subset_ts_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_2_hourly()[,(colnames(ns_exp_dat_ts_2_hourly()) %in% c('ns_exp_subset_ts_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_2_hourly()[,(colnames(ns_exp_dat_ts_2_hourly()) %in% c('ns_exp_subset_ts_2().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_3_hourly()[,(colnames(ns_exp_dat_ts_3_hourly()) %in% c('ns_exp_subset_ts_3().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_3_hourly()[,(colnames(ns_exp_dat_ts_3_hourly()) %in% c('ns_exp_subset_ts_3().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_1_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(ns_exp_subset_ts_1(), type = "line", name = "High Load: ", color = "green")%>%
      hc_add_series(ns_exp_subset_ts_2(), type = "line", name = "High Load: ", color = "green")%>%
      hc_add_series(ns_exp_subset_ts_3(), type = "line", name = "High Load: ", color = "green")
  })
  
  output$NS_EXP_2_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_4_yearly()[,(colnames(ns_exp_dat_ts_4_yearly()) %in% c('ns_exp_dat_ts_4().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_4_yearly()[,(colnames(ns_exp_dat_ts_4_yearly()) %in% c('ns_exp_dat_ts_4().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_5_yearly()[,(colnames(ns_exp_dat_ts_5_yearly()) %in% c('ns_exp_dat_ts_5().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_5_yearly()[,(colnames(ns_exp_dat_ts_5_yearly()) %in% c('ns_exp_dat_ts_5().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_6_yearly()[,(colnames(ns_exp_dat_ts_6_yearly()) %in% c('ns_exp_dat_ts_6().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_6_yearly()[,(colnames(ns_exp_dat_ts_6_yearly()) %in% c('ns_exp_dat_ts_6().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_2_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_4_weekly()[,(colnames(ns_exp_dat_ts_4_weekly()) %in% c('ns_exp_subset_ts_4().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_4_weekly()[,(colnames(ns_exp_dat_ts_4_weekly()) %in% c('ns_exp_subset_ts_4().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_5_weekly()[,(colnames(ns_exp_dat_ts_5_weekly()) %in% c('ns_exp_subset_ts_5().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_5_weekly()[,(colnames(ns_exp_dat_ts_5_weekly()) %in% c('ns_exp_subset_ts_5().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_6_weekly()[,(colnames(ns_exp_dat_ts_6_weekly()) %in% c('ns_exp_subset_ts_6().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_6_weekly()[,(colnames(ns_exp_dat_ts_6_weekly()) %in% c('ns_exp_subset_ts_6().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_2_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_4_daily()[,(colnames(ns_exp_dat_ts_4_daily()) %in% c('ns_exp_subset_ts_4().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_4_daily()[,(colnames(ns_exp_dat_ts_4_daily()) %in% c('ns_exp_subset_ts_4().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_5_daily()[,(colnames(ns_exp_dat_ts_5_daily()) %in% c('ns_exp_subset_ts_5().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_5_daily()[,(colnames(ns_exp_dat_ts_5_daily()) %in% c('ns_exp_subset_ts_5().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_6_daily()[,(colnames(ns_exp_dat_ts_6_daily()) %in% c('ns_exp_subset_ts_6().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_6_daily()[,(colnames(ns_exp_dat_ts_6_daily()) %in% c('ns_exp_subset_ts_6().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_2_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_4_hourly()[,(colnames(ns_exp_dat_ts_4_hourly()) %in% c('ns_exp_subset_ts_4().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_4_hourly()[,(colnames(ns_exp_dat_ts_4_hourly()) %in% c('ns_exp_subset_ts_4().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_5_hourly()[,(colnames(ns_exp_dat_ts_5_hourly()) %in% c('ns_exp_subset_ts_5().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_5_hourly()[,(colnames(ns_exp_dat_ts_5_hourly()) %in% c('ns_exp_subset_ts_5().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_6_hourly()[,(colnames(ns_exp_dat_ts_6_hourly()) %in% c('ns_exp_subset_ts_6().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_6_hourly()[,(colnames(ns_exp_dat_ts_6_hourly()) %in% c('ns_exp_subset_ts_6().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_2_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(ns_exp_subset_ts_4(), type = "line", name = "High Load: ", color = "green")%>%
      hc_add_series(ns_exp_subset_ts_5(), type = "line", name = "High Load: ", color = "green")%>%
      hc_add_series(ns_exp_subset_ts_6(), type = "line", name = "High Load: ", color = "green")
  })
  
  output$NS_EXP_3_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_7_yearly()[,(colnames(ns_exp_dat_ts_7_yearly()) %in% c('ns_exp_dat_ts_7().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_7_yearly()[,(colnames(ns_exp_dat_ts_7_yearly()) %in% c('ns_exp_dat_ts_7().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_8_yearly()[,(colnames(ns_exp_dat_ts_8_yearly()) %in% c('ns_exp_dat_ts_8().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_8_yearly()[,(colnames(ns_exp_dat_ts_8_yearly()) %in% c('ns_exp_dat_ts_8().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_9_yearly()[,(colnames(ns_exp_dat_ts_9_yearly()) %in% c('ns_exp_dat_ts_9().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_9_yearly()[,(colnames(ns_exp_dat_ts_9_yearly()) %in% c('ns_exp_dat_ts_9().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_3_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_7_weekly()[,(colnames(ns_exp_dat_ts_7_weekly()) %in% c('ns_exp_subset_ts_7().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_7_weekly()[,(colnames(ns_exp_dat_ts_7_weekly()) %in% c('ns_exp_subset_ts_7().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_8_weekly()[,(colnames(ns_exp_dat_ts_8_weekly()) %in% c('ns_exp_subset_ts_8().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_8_weekly()[,(colnames(ns_exp_dat_ts_8_weekly()) %in% c('ns_exp_subset_ts_8().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_9_weekly()[,(colnames(ns_exp_dat_ts_9_weekly()) %in% c('ns_exp_subset_ts_9().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_9_weekly()[,(colnames(ns_exp_dat_ts_9_weekly()) %in% c('ns_exp_subset_ts_9().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_3_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_7_daily()[,(colnames(ns_exp_dat_ts_7_daily()) %in% c('ns_exp_subset_ts_7().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_7_daily()[,(colnames(ns_exp_dat_ts_7_daily()) %in% c('ns_exp_subset_ts_7().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_8_daily()[,(colnames(ns_exp_dat_ts_8_daily()) %in% c('ns_exp_subset_ts_8().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_8_daily()[,(colnames(ns_exp_dat_ts_8_daily()) %in% c('ns_exp_subset_ts_8().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_9_daily()[,(colnames(ns_exp_dat_ts_9_daily()) %in% c('ns_exp_subset_ts_9().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_9_daily()[,(colnames(ns_exp_dat_ts_9_daily()) %in% c('ns_exp_subset_ts_9().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_3_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_7_hourly()[,(colnames(ns_exp_dat_ts_7_hourly()) %in% c('ns_exp_subset_ts_7().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_7_hourly()[,(colnames(ns_exp_dat_ts_7_hourly()) %in% c('ns_exp_subset_ts_7().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_8_hourly()[,(colnames(ns_exp_dat_ts_8_hourly()) %in% c('ns_exp_subset_ts_8().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_8_hourly()[,(colnames(ns_exp_dat_ts_8_hourly()) %in% c('ns_exp_subset_ts_8().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_9_hourly()[,(colnames(ns_exp_dat_ts_9_hourly()) %in% c('ns_exp_subset_ts_9().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_9_hourly()[,(colnames(ns_exp_dat_ts_9_hourly()) %in% c('ns_exp_subset_ts_9().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_3_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(ns_exp_subset_ts_7(), type = "line", name = "High Load: ", color = "green")%>%
      hc_add_series(ns_exp_subset_ts_8(), type = "line", name = "High Load: ", color = "green")%>%
      hc_add_series(ns_exp_subset_ts_9(), type = "line", name = "High Load: ", color = "green")
  })
  
  output$NS_EXP_4_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_10_yearly()[,(colnames(ns_exp_dat_ts_10_yearly()) %in% c('ns_exp_dat_ts_10().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_10_yearly()[,(colnames(ns_exp_dat_ts_10_yearly()) %in% c('ns_exp_dat_ts_10().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_11_yearly()[,(colnames(ns_exp_dat_ts_11_yearly()) %in% c('ns_exp_dat_ts_11().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_11_yearly()[,(colnames(ns_exp_dat_ts_11_yearly()) %in% c('ns_exp_dat_ts_11().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_4_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_10_weekly()[,(colnames(ns_exp_dat_ts_10_weekly()) %in% c('ns_exp_subset_ts_10().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_10_weekly()[,(colnames(ns_exp_dat_ts_10_weekly()) %in% c('ns_exp_subset_ts_10().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_11_weekly()[,(colnames(ns_exp_dat_ts_11_weekly()) %in% c('ns_exp_subset_ts_11().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_11_weekly()[,(colnames(ns_exp_dat_ts_11_weekly()) %in% c('ns_exp_subset_ts_11().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_4_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_10_daily()[,(colnames(ns_exp_dat_ts_10_daily()) %in% c('ns_exp_subset_ts_10().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_10_daily()[,(colnames(ns_exp_dat_ts_10_daily()) %in% c('ns_exp_subset_ts_10().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_11_daily()[,(colnames(ns_exp_dat_ts_11_daily()) %in% c('ns_exp_subset_ts_11().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_11_daily()[,(colnames(ns_exp_dat_ts_11_daily()) %in% c('ns_exp_subset_ts_11().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_4_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_10_hourly()[,(colnames(ns_exp_dat_ts_10_hourly()) %in% c('ns_exp_subset_ts_10().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_10_hourly()[,(colnames(ns_exp_dat_ts_10_hourly()) %in% c('ns_exp_subset_ts_10().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_11_hourly()[,(colnames(ns_exp_dat_ts_11_hourly()) %in% c('ns_exp_subset_ts_11().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_11_hourly()[,(colnames(ns_exp_dat_ts_11_hourly()) %in% c('ns_exp_subset_ts_11().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_4_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(ns_exp_subset_ts_10(), type = "line", name = "High Load: ", color = "green")%>%
      hc_add_series(ns_exp_subset_ts_11(), type = "line", name = "High Load: ", color = "green")
  })
  
  
  
  on_date_ind_1_1 <- reactive({paste(input$on_dates_1[1],"00:00:00",sep = " ")})
  on_date_ind_1_2 <- reactive({paste(input$on_dates_1[2],"00:00:00",sep = " ")})
  on_tot_energy_subset_dat <- reactive({subset(on_ind_dat,subset = (on_ind_dat$date_time_local >= on_date_ind_1_1() & on_ind_dat$date_time_local <= on_date_ind_1_2()))})
  on_tot_energy_dat_ts <- reactive({xts(on_ind_dat$total_energy,on_ind_dat$date_time_local)})
  on_tot_energy_subset_ts <- reactive({xts(on_tot_energy_subset_dat()$total_energy,on_tot_energy_subset_dat()$date_time_local)})
  on_tot_energy_dat_ts_yearly <- reactive({to.yearly(on_tot_energy_dat_ts())})
  on_tot_energy_dat_ts_monthly <- reactive({to.monthly(on_tot_energy_subset_ts())})
  on_tot_energy_dat_ts_weekly <- reactive({to.weekly(on_tot_energy_subset_ts())})
  on_tot_energy_dat_ts_daily <- reactive({to.daily(on_tot_energy_subset_ts())})
  on_tot_energy_dat_ts_hourly <- reactive({to.hourly(on_tot_energy_subset_ts())})
  
  output$ON_TOT_ENERGY_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_energy_dat_ts_yearly()[,(colnames(on_tot_energy_dat_ts_yearly()) %in% c('on_tot_energy_dat_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_energy_dat_ts_yearly()[,(colnames(on_tot_energy_dat_ts_yearly()) %in% c('on_tot_energy_dat_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_ENERGY_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_energy_dat_ts_weekly()[,(colnames(on_tot_energy_dat_ts_weekly()) %in% c('on_tot_energy_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_energy_dat_ts_weekly()[,(colnames(on_tot_energy_dat_ts_weekly()) %in% c('on_tot_energy_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_ENERGY_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_energy_dat_ts_daily()[,(colnames(on_tot_energy_dat_ts_daily()) %in% c('on_tot_energy_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_energy_dat_ts_daily()[,(colnames(on_tot_energy_dat_ts_daily()) %in% c('on_tot_energy_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_ENERGY_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_energy_dat_ts_hourly()[,(colnames(on_tot_energy_dat_ts_hourly()) %in% c('on_tot_energy_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_energy_dat_ts_hourly()[,(colnames(on_tot_energy_dat_ts_hourly()) %in% c('on_tot_energy_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_ENERGY_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(on_tot_energy_subset_ts(), type = "line", name = "High Load: ", color = "green")
  })
  
  on_date_ind_2_1 <- reactive({paste(input$on_dates_2[1],"00:00:00",sep = " ")})
  on_date_ind_2_2 <- reactive({paste(input$on_dates_2[2],"00:00:00",sep = " ")})
  on_tot_loss_subset_dat <- reactive({subset(on_ind_dat,subset = (on_ind_dat$date_time_local >= on_date_ind_2_1() & on_ind_dat$date_time_local <= on_date_ind_2_2()))})
  on_tot_loss_dat_ts <- reactive({xts(on_ind_dat$total_loss,on_ind_dat$date_time_local)})
  on_tot_loss_subset_ts <- reactive({xts(on_tot_loss_subset_dat()$total_loss,on_tot_loss_subset_dat()$date_time_local)})
  on_tot_loss_dat_ts_yearly <- reactive({to.yearly(on_tot_loss_dat_ts())})
  on_tot_loss_dat_ts_monthly <- reactive({to.monthly(on_tot_loss_subset_ts())})
  on_tot_loss_dat_ts_weekly <- reactive({to.weekly(on_tot_loss_subset_ts())})
  on_tot_loss_dat_ts_daily <- reactive({to.daily(on_tot_loss_subset_ts())})
  on_tot_loss_dat_ts_hourly <- reactive({to.hourly(on_tot_loss_subset_ts())})
  
  output$ON_TOT_LOSS_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_loss_dat_ts_yearly()[,(colnames(on_tot_loss_dat_ts_yearly()) %in% c('on_tot_loss_dat_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_loss_dat_ts_yearly()[,(colnames(on_tot_loss_dat_ts_yearly()) %in% c('on_tot_loss_dat_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_LOSS_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_loss_dat_ts_weekly()[,(colnames(on_tot_loss_dat_ts_weekly()) %in% c('on_tot_loss_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_loss_dat_ts_weekly()[,(colnames(on_tot_loss_dat_ts_weekly()) %in% c('on_tot_loss_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_LOSS_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_loss_dat_ts_daily()[,(colnames(on_tot_loss_dat_ts_daily()) %in% c('on_tot_loss_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_loss_dat_ts_daily()[,(colnames(on_tot_loss_dat_ts_daily()) %in% c('on_tot_loss_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_LOSS_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_loss_dat_ts_hourly()[,(colnames(on_tot_loss_dat_ts_hourly()) %in% c('on_tot_loss_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_loss_dat_ts_hourly()[,(colnames(on_tot_loss_dat_ts_hourly()) %in% c('on_tot_loss_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_LOSS_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(on_tot_loss_subset_ts(), type = "line", name = "High Load: ", color = "green")
  })
  
  on_date_ind_3_1 <- reactive({paste(input$on_dates_3[1],"00:00:00",sep = " ")})
  on_date_ind_3_2 <- reactive({paste(input$on_dates_3[2],"00:00:00",sep = " ")})
  on_tot_load_subset_dat <- reactive({subset(on_ind_dat,subset = (on_ind_dat$date_time_local >= on_date_ind_3_1() & on_ind_dat$date_time_local <= on_date_ind_3_2()))})
  on_tot_load_dat_ts <- reactive({xts(on_ind_dat$total_load,on_ind_dat$date_time_local)})
  on_tot_load_subset_ts <- reactive({xts(on_tot_load_subset_dat()$total_load,on_tot_load_subset_dat()$date_time_local)})
  on_tot_load_dat_ts_yearly <- reactive({to.yearly(on_tot_load_dat_ts())})
  on_tot_load_dat_ts_monthly <- reactive({to.monthly(on_tot_load_subset_ts())})
  on_tot_load_dat_ts_weekly <- reactive({to.weekly(on_tot_load_subset_ts())})
  on_tot_load_dat_ts_daily <- reactive({to.daily(on_tot_load_subset_ts())})
  on_tot_load_dat_ts_hourly <- reactive({to.hourly(on_tot_load_subset_ts())})
  
  output$ON_TOT_LOAD_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_load_dat_ts_yearly()[,(colnames(on_tot_load_dat_ts_yearly()) %in% c('on_tot_load_dat_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_load_dat_ts_yearly()[,(colnames(on_tot_load_dat_ts_yearly()) %in% c('on_tot_load_dat_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_LOAD_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_load_dat_ts_weekly()[,(colnames(on_tot_load_dat_ts_weekly()) %in% c('on_tot_load_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_load_dat_ts_weekly()[,(colnames(on_tot_load_dat_ts_weekly()) %in% c('on_tot_load_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_LOAD_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_load_dat_ts_daily()[,(colnames(on_tot_load_dat_ts_daily()) %in% c('on_tot_load_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_load_dat_ts_daily()[,(colnames(on_tot_load_dat_ts_daily()) %in% c('on_tot_load_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_LOAD_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_load_dat_ts_hourly()[,(colnames(on_tot_load_dat_ts_hourly()) %in% c('on_tot_load_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_load_dat_ts_hourly()[,(colnames(on_tot_load_dat_ts_hourly()) %in% c('on_tot_load_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_LOAD_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(on_tot_load_subset_ts(), type = "line", name = "High Load: ", color = "green")
  })
  
  on_date_ind_4_1 <- reactive({paste(input$on_dates_4[1],"00:00:00",sep = " ")})
  on_date_ind_4_2 <- reactive({paste(input$on_dates_4[2],"00:00:00",sep = " ")})
  on_tot_demand_subset_dat <- reactive({subset(on_ind_dat,subset = (on_ind_dat$date_time_local >= on_date_ind_4_1() & on_ind_dat$date_time_local <= on_date_ind_4_2()))})
  on_tot_demand_dat_ts <- reactive({xts(on_ind_dat$ontario_demand,on_ind_dat$date_time_local)})
  on_tot_demand_subset_ts <- reactive({xts(on_tot_demand_subset_dat()$ontario_demand,on_tot_demand_subset_dat()$date_time_local)})
  on_tot_demand_dat_ts_yearly <- reactive({to.yearly(on_tot_demand_dat_ts())})
  on_tot_demand_dat_ts_monthly <- reactive({to.monthly(on_tot_demand_subset_ts())})
  on_tot_demand_dat_ts_weekly <- reactive({to.weekly(on_tot_demand_subset_ts())})
  on_tot_demand_dat_ts_daily <- reactive({to.daily(on_tot_demand_subset_ts())})
  on_tot_demand_dat_ts_hourly <- reactive({to.hourly(on_tot_demand_subset_ts())})
  
  output$ON_TOT_DEMAND_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_demand_dat_ts_yearly()[,(colnames(on_tot_demand_dat_ts_yearly()) %in% c('on_tot_demand_dat_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_demand_dat_ts_yearly()[,(colnames(on_tot_demand_dat_ts_yearly()) %in% c('on_tot_demand_dat_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_DEMAND_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_demand_dat_ts_weekly()[,(colnames(on_tot_demand_dat_ts_weekly()) %in% c('on_tot_demand_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_demand_dat_ts_weekly()[,(colnames(on_tot_demand_dat_ts_weekly()) %in% c('on_tot_demand_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_DEMAND_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_demand_dat_ts_daily()[,(colnames(on_tot_demand_dat_ts_daily()) %in% c('on_tot_demand_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_demand_dat_ts_daily()[,(colnames(on_tot_demand_dat_ts_daily()) %in% c('on_tot_demand_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_DEMAND_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_demand_dat_ts_hourly()[,(colnames(on_tot_demand_dat_ts_hourly()) %in% c('on_tot_demand_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_demand_dat_ts_hourly()[,(colnames(on_tot_demand_dat_ts_hourly()) %in% c('on_tot_demand_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_DEMAND_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(on_tot_demand_subset_ts(), type = "line", name = "High Load: ", color = "green")
  })
  
  qb_date_ind_1_1 <- reactive({paste(input$qb_dates_1[1],"00:00:00",sep = " ")})
  qb_date_ind_1_2 <- reactive({paste(input$qb_dates_1[2],"00:00:00",sep = " ")})
  
  qb_data_energy_subset_dat <- reactive({subset(qb_ind_dat_1,subset = (qb_ind_dat_1$Date_time_local >= qb_date_ind_1_1() & qb_ind_dat_1$Date_time_local <= qb_date_ind_1_2()))})
  
  qb_data_energy_dat_ts <- reactive({xts(qb_ind_dat_1$total_production,qb_ind_dat_1$Date_time_local)})
  qb_data_energy_subset_ts <- reactive({xts(qb_data_energy_subset_dat()$total_production,qb_data_energy_subset_dat()$Date_time_local)})
  
  qb_data_energy_dat_ts_yearly <- reactive({to.yearly(qb_data_energy_dat_ts())})
  qb_data_energy_dat_ts_monthly <- reactive({to.monthly(qb_data_energy_subset_ts())})
  qb_data_energy_dat_ts_weekly <- reactive({to.weekly(qb_data_energy_subset_ts())})
  qb_data_energy_dat_ts_daily <- reactive({to.daily(qb_data_energy_subset_ts())})
  qb_data_energy_dat_ts_hourly <- reactive({to.hourly(qb_data_energy_subset_ts())})
  
  output$QB_TOT_PRODUCTION_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_yearly()[,(colnames(qb_data_energy_dat_ts_yearly()) %in% c('qb_data_energy_dat_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly()[,(colnames(qb_data_energy_dat_ts_yearly()) %in% c('qb_data_energy_dat_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_TOT_PRODUCTION_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_weekly()[,(colnames(qb_data_energy_dat_ts_weekly()) %in% c('qb_data_energy_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly()[,(colnames(qb_data_energy_dat_ts_weekly()) %in% c('qb_data_energy_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_TOT_PRODUCTION_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_daily()[,(colnames(qb_data_energy_dat_ts_daily()) %in% c('qb_data_energy_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_daily()[,(colnames(qb_data_energy_dat_ts_daily()) %in% c('qb_data_energy_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_TOT_PRODUCTION_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_hourly()[,(colnames(qb_data_energy_dat_ts_hourly()) %in% c('qb_data_energy_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly()[,(colnames(qb_data_energy_dat_ts_hourly()) %in% c('qb_data_energy_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_TOT_PRODUCTION_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(qb_data_energy_subset_ts(), type = "line", name = "High Load: ", color = "green")
  })
  
  
  qb_date_ind_2_1 <- reactive({paste(input$qb_dates_2[1],"00:00:00",sep = " ")})
  qb_date_ind_2_2 <- reactive({paste(input$qb_dates_2[2],"00:00:00",sep = " ")})
  
  qb_data_energy_subset_dat_1 <- reactive({subset(qb_ind_dat_1,subset = (qb_ind_dat_1$Date_time_local >= qb_date_ind_2_1() & qb_ind_dat_1$Date_time_local <= qb_date_ind_2_2()))})
  qb_data_energy_subset_dat_2 <- reactive({subset(qb_ind_dat_1,subset = (qb_ind_dat_1$Date_time_local >= qb_date_ind_2_1() & qb_ind_dat_1$Date_time_local <= qb_date_ind_2_2()))})
  
  qb_data_energy_dat_ts_1 <- reactive({xts(qb_ind_dat_1$hydraulic,qb_ind_dat_1$Date_time_local)})
  qb_data_energy_subset_ts_1 <- reactive({xts(qb_data_energy_subset_dat_1()$hydraulic,qb_data_energy_subset_dat_1()$Date_time_local)})
  
  qb_data_energy_dat_ts_2 <- reactive({xts(qb_ind_dat_1$other,qb_ind_dat_1$Date_time_local)})
  qb_data_energy_subset_ts_2 <- reactive({xts(qb_data_energy_subset_dat_2()$other,qb_data_energy_subset_dat_2()$Date_time_local)})
  
  qb_data_energy_dat_ts_yearly_1 <- reactive({to.yearly(qb_data_energy_dat_ts_1())})
  qb_data_energy_dat_ts_monthly_1 <- reactive({to.monthly(qb_data_energy_subset_ts_1())})
  qb_data_energy_dat_ts_weekly_1 <- reactive({to.weekly(qb_data_energy_subset_ts_1())})
  qb_data_energy_dat_ts_daily_1 <- reactive({to.daily(qb_data_energy_subset_ts_1())})
  qb_data_energy_dat_ts_hourly_1 <- reactive({to.hourly(qb_data_energy_subset_ts_1())})
  
  qb_data_energy_dat_ts_yearly_2 <- reactive({to.yearly(qb_data_energy_dat_ts_2())})
  qb_data_energy_dat_ts_monthly_2 <- reactive({to.monthly(qb_data_energy_subset_ts_2())})
  qb_data_energy_dat_ts_weekly_2 <- reactive({to.weekly(qb_data_energy_subset_ts_2())})
  qb_data_energy_dat_ts_daily_2 <- reactive({to.daily(qb_data_energy_subset_ts_2())})
  qb_data_energy_dat_ts_hourly_2 <- reactive({to.hourly(qb_data_energy_subset_ts_2())})
  
  output$QB_FUEL_1_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_yearly_1()[,(colnames(qb_data_energy_dat_ts_yearly_1()) %in% c('qb_data_energy_dat_ts_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly_1()[,(colnames(qb_data_energy_dat_ts_yearly_1()) %in% c('qb_data_energy_dat_ts_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly_2()[,(colnames(qb_data_energy_dat_ts_yearly_2()) %in% c('qb_data_energy_dat_ts_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly_2()[,(colnames(qb_data_energy_dat_ts_yearly_2()) %in% c('qb_data_energy_dat_ts_2().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_FUEL_1_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_weekly_1()[,(colnames(qb_data_energy_dat_ts_weekly_1()) %in% c('qb_data_energy_subset_ts_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly_1()[,(colnames(qb_data_energy_dat_ts_weekly_1()) %in% c('qb_data_energy_subset_ts_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly_2()[,(colnames(qb_data_energy_dat_ts_weekly_2()) %in% c('qb_data_energy_subset_ts_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly_2()[,(colnames(qb_data_energy_dat_ts_weekly_2()) %in% c('qb_data_energy_subset_ts_2().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_FUEL_1_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_daily_1()[,(colnames(qb_data_energy_dat_ts_daily_1()) %in% c('qb_data_energy_subset_ts_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_daily_1()[,(colnames(qb_data_energy_dat_ts_daily_1()) %in% c('qb_data_energy_subset_ts_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_daily_2()[,(colnames(qb_data_energy_dat_ts_daily_2()) %in% c('qb_data_energy_subset_ts_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_daily_2()[,(colnames(qb_data_energy_dat_ts_daily_2()) %in% c('qb_data_energy_subset_ts_2().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_FUEL_1_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_hourly_1()[,(colnames(qb_data_energy_dat_ts_hourly_1()) %in% c('qb_data_energy_subset_ts_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly_1()[,(colnames(qb_data_energy_dat_ts_hourly_1()) %in% c('qb_data_energy_subset_ts_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly_2()[,(colnames(qb_data_energy_dat_ts_hourly_2()) %in% c('qb_data_energy_subset_ts_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly_2()[,(colnames(qb_data_energy_dat_ts_hourly_2()) %in% c('qb_data_energy_subset_ts_2().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_FUEL_1_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(qb_data_energy_subset_ts_1(), type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_subset_ts_2(), type = "line", name = "High Load: ", color = "green")
  })
  
  qb_date_ind_3_1 <- reactive({paste(input$qb_dates_3[1],"00:00:00",sep = " ")})
  qb_date_ind_3_2 <- reactive({paste(input$qb_dates_3[2],"00:00:00",sep = " ")})
  
  qb_data_energy_subset_dat_3 <- reactive({subset(qb_ind_dat_1,subset = (qb_ind_dat_1$Date_time_local >= qb_date_ind_3_1() & qb_ind_dat_1$Date_time_local <= qb_date_ind_3_2()))})
  qb_data_energy_subset_dat_4 <- reactive({subset(qb_ind_dat_1,subset = (qb_ind_dat_1$Date_time_local >= qb_date_ind_3_1() & qb_ind_dat_1$Date_time_local <= qb_date_ind_3_2()))})
  
  qb_data_energy_dat_ts_3 <- reactive({xts(qb_ind_dat_1$wind,qb_ind_dat_1$Date_time_local)})
  qb_data_energy_subset_ts_3 <- reactive({xts(qb_data_energy_subset_dat_3()$wind,qb_data_energy_subset_dat_3()$Date_time_local)})
  
  qb_data_energy_dat_ts_4 <- reactive({xts(qb_ind_dat_1$solar,qb_ind_dat_1$Date_time_local)})
  qb_data_energy_subset_ts_4 <- reactive({xts(qb_data_energy_subset_dat_4()$solar,qb_data_energy_subset_dat_4()$Date_time_local)})
  
  qb_data_energy_dat_ts_yearly_3 <- reactive({to.yearly(qb_data_energy_dat_ts_3())})
  qb_data_energy_dat_ts_monthly_3 <- reactive({to.monthly(qb_data_energy_subset_ts_3())})
  qb_data_energy_dat_ts_weekly_3 <- reactive({to.weekly(qb_data_energy_subset_ts_3())})
  qb_data_energy_dat_ts_daily_3 <- reactive({to.daily(qb_data_energy_subset_ts_3())})
  qb_data_energy_dat_ts_hourly_3 <- reactive({to.hourly(qb_data_energy_subset_ts_3())})
  
  qb_data_energy_dat_ts_yearly_4 <- reactive({to.yearly(qb_data_energy_dat_ts_4())})
  qb_data_energy_dat_ts_monthly_4 <- reactive({to.monthly(qb_data_energy_subset_ts_4())})
  qb_data_energy_dat_ts_weekly_4 <- reactive({to.weekly(qb_data_energy_subset_ts_4())})
  qb_data_energy_dat_ts_daily_4 <- reactive({to.daily(qb_data_energy_subset_ts_4())})
  qb_data_energy_dat_ts_hourly_4 <- reactive({to.hourly(qb_data_energy_subset_ts_4())})
  
  output$QB_FUEL_2_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_yearly_3()[,(colnames(qb_data_energy_dat_ts_yearly_3()) %in% c('qb_data_energy_dat_ts_3().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly_3()[,(colnames(qb_data_energy_dat_ts_yearly_3()) %in% c('qb_data_energy_dat_ts_3().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly_4()[,(colnames(qb_data_energy_dat_ts_yearly_4()) %in% c('qb_data_energy_dat_ts_4().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly_4()[,(colnames(qb_data_energy_dat_ts_yearly_4()) %in% c('qb_data_energy_dat_ts_4().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_FUEL_2_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_weekly_3()[,(colnames(qb_data_energy_dat_ts_weekly_3()) %in% c('qb_data_energy_subset_ts_3().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly_3()[,(colnames(qb_data_energy_dat_ts_weekly_3()) %in% c('qb_data_energy_subset_ts_3().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly_4()[,(colnames(qb_data_energy_dat_ts_weekly_4()) %in% c('qb_data_energy_subset_ts_4().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly_4()[,(colnames(qb_data_energy_dat_ts_weekly_4()) %in% c('qb_data_energy_subset_ts_4().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_FUEL_2_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_daily_3()[,(colnames(qb_data_energy_dat_ts_daily_3()) %in% c('qb_data_energy_subset_ts_3().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_daily_3()[,(colnames(qb_data_energy_dat_ts_daily_3()) %in% c('qb_data_energy_subset_ts_3().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_daily_4()[,(colnames(qb_data_energy_dat_ts_daily_4()) %in% c('qb_data_energy_subset_ts_4().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_daily_4()[,(colnames(qb_data_energy_dat_ts_daily_4()) %in% c('qb_data_energy_subset_ts_4().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_FUEL_2_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_hourly_3()[,(colnames(qb_data_energy_dat_ts_hourly_3()) %in% c('qb_data_energy_subset_ts_3().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly_3()[,(colnames(qb_data_energy_dat_ts_hourly_3()) %in% c('qb_data_energy_subset_ts_3().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly_4()[,(colnames(qb_data_energy_dat_ts_hourly_4()) %in% c('qb_data_energy_subset_ts_4().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly_4()[,(colnames(qb_data_energy_dat_ts_hourly_4()) %in% c('qb_data_energy_subset_ts_4().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_FUEL_2_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(qb_data_energy_subset_ts_3(), type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_subset_ts_4(), type = "line", name = "High Load: ", color = "green")
  })
  
  qb_date_ind_4_1 <- reactive({paste(input$qb_dates_4[1],"00:00:00",sep = " ")})
  qb_date_ind_4_2 <- reactive({paste(input$qb_dates_4[2],"00:00:00",sep = " ")})
  
  qb_data_energy_subset_dat_5 <- reactive({subset(qb_ind_dat_1,subset = (qb_ind_dat_1$Date_time_local >= qb_date_ind_4_1() & qb_ind_dat_1$Date_time_local <= qb_date_ind_4_2()))})
  
  qb_data_energy_dat_ts_5 <- reactive({xts(qb_ind_dat_1$thermal,qb_ind_dat_1$Date_time_local)})
  qb_data_energy_subset_ts_5 <- reactive({xts(qb_data_energy_subset_dat_5()$thermal,qb_data_energy_subset_dat_5()$Date_time_local)})
  
  qb_data_energy_dat_ts_yearly_5 <- reactive({to.yearly(qb_data_energy_dat_ts_5())})
  qb_data_energy_dat_ts_monthly_5 <- reactive({to.monthly(qb_data_energy_subset_ts_5())})
  qb_data_energy_dat_ts_weekly_5 <- reactive({to.weekly(qb_data_energy_subset_ts_5())})
  qb_data_energy_dat_ts_daily_5 <- reactive({to.daily(qb_data_energy_subset_ts_5())})
  qb_data_energy_dat_ts_hourly_5 <- reactive({to.hourly(qb_data_energy_subset_ts_5())})
  
  output$QB_FUEL_3_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_yearly_5()[,(colnames(qb_data_energy_dat_ts_yearly_5()) %in% c('qb_data_energy_dat_ts_5().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly_5()[,(colnames(qb_data_energy_dat_ts_yearly_5()) %in% c('qb_data_energy_dat_ts_5().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_FUEL_3_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_weekly_5()[,(colnames(qb_data_energy_dat_ts_weekly_5()) %in% c('qb_data_energy_subset_ts_5().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly_5()[,(colnames(qb_data_energy_dat_ts_weekly_5()) %in% c('qb_data_energy_subset_ts_5().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_FUEL_3_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_daily_5()[,(colnames(qb_data_energy_dat_ts_daily_5()) %in% c('qb_data_energy_subset_ts_5().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_daily_5()[,(colnames(qb_data_energy_dat_ts_daily_5()) %in% c('qb_data_energy_subset_ts_5().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_FUEL_3_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_hourly_5()[,(colnames(qb_data_energy_dat_ts_hourly_5()) %in% c('qb_data_energy_subset_ts_5().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly_5()[,(colnames(qb_data_energy_dat_ts_hourly_5()) %in% c('qb_data_energy_subset_ts_5().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_FUEL_3_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(qb_data_energy_subset_ts_5(), type = "line", name = "High Load: ", color = "green")
  })
  
  qb_date_ind_5_1 <- reactive({paste(input$qb_dates_5[1],"00:00:00",sep = " ")})
  qb_date_ind_5_2 <- reactive({paste(input$qb_dates_5[2],"00:00:00",sep = " ")})
  
  qb_date_ind_6_1 <- reactive({paste(input$qb_dates_6[1],"00:00:00",sep = " ")})
  qb_date_ind_6_2 <- reactive({paste(input$qb_dates_6[2],"00:00:00",sep = " ")})
  
  qb_date_ind_7_1 <- reactive({paste(input$qb_dates_7[1],"00:00:00",sep = " ")})
  qb_date_ind_7_2 <- reactive({paste(input$qb_dates_7[2],"00:00:00",sep = " ")})
  
  qb_date_ind_8_1 <- reactive({paste(input$qb_dates_8[1],"00:00:00",sep = " ")})
  qb_date_ind_8_2 <- reactive({paste(input$qb_dates_8[2],"00:00:00",sep = " ")})
  
  qb_date_ind_9_1 <- reactive({paste(input$qb_dates_9[1],"00:00:00",sep = " ")})
  qb_date_ind_9_2 <- reactive({paste(input$qb_dates_9[2],"00:00:00",sep = " ")})
  
  qb_date_ind_10_1 <- reactive({paste(input$qb_dates_10[1],"00:00:00",sep = " ")})
  qb_date_ind_10_2 <- reactive({paste(input$qb_dates_10[2],"00:00:00",sep = " ")})
  
  qb_date_ind_11_1 <- reactive({paste(input$qb_dates_11[1],"00:00:00",sep = " ")})
  qb_date_ind_11_2 <- reactive({paste(input$qb_dates_11[2],"00:00:00",sep = " ")})
  
  qb_date_ind_12_1 <- reactive({paste(input$qb_dates_12[1],"00:00:00",sep = " ")})
  qb_date_ind_12_2 <- reactive({paste(input$qb_dates_12[2],"00:00:00",sep = " ")})
  
  qb_data_energy_subset_dat_6 <- reactive({subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= qb_date_ind_5_1() & qb_ind_dat_3$Date_time_local <= qb_date_ind_5_2()))})
  qb_data_energy_subset_dat_6_1 <- reactive({subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= qb_date_ind_5_1() & qb_ind_dat_3$Date_time_local <= qb_date_ind_5_2()))})
  
  qb_data_energy_dat_ts_6 <- reactive({xts(qb_ind_dat_3$gross_generation_HQP,qb_ind_dat_3$Date_time_local)})
  qb_data_energy_subset_ts_6 <- reactive({xts(qb_data_energy_subset_dat_6()$gross_generation_HQP,qb_data_energy_subset_dat_6()$Date_time_local)})
  
  qb_data_energy_dat_ts_6_1 <- reactive({xts(qb_ind_dat_3$electricity_received_HQP,qb_ind_dat_3$Date_time_local)})
  qb_data_energy_subset_ts_6_1 <- reactive({xts(qb_data_energy_subset_dat_6_1()$electricity_received_HQP,qb_data_energy_subset_dat_6_1()$Date_time_local)})
  
  qb_data_energy_dat_ts_yearly_6 <- reactive({to.yearly(qb_data_energy_dat_ts_6())})
  qb_data_energy_dat_ts_monthly_6 <- reactive({to.monthly(qb_data_energy_subset_ts_6())})
  qb_data_energy_dat_ts_weekly_6 <- reactive({to.weekly(qb_data_energy_subset_ts_6())})
  qb_data_energy_dat_ts_daily_6 <- reactive({to.daily(qb_data_energy_subset_ts_6())})
  qb_data_energy_dat_ts_hourly_6 <- reactive({to.hourly(qb_data_energy_subset_ts_6())})
  
  qb_data_energy_dat_ts_yearly_6_1 <- reactive({to.yearly(qb_data_energy_dat_ts_6_1())})
  qb_data_energy_dat_ts_monthly_6_1 <- reactive({to.monthly(qb_data_energy_subset_ts_6_1())})
  qb_data_energy_dat_ts_weekly_6_1 <- reactive({to.weekly(qb_data_energy_subset_ts_6_1())})
  qb_data_energy_dat_ts_daily_6_1 <- reactive({to.daily(qb_data_energy_subset_ts_6_1())})
  qb_data_energy_dat_ts_hourly_6_1 <- reactive({to.hourly(qb_data_energy_subset_ts_6_1())})
  
  output$QB_ENE_1_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_yearly_6()[,(colnames(qb_data_energy_dat_ts_yearly_6()) %in% c('qb_data_energy_dat_ts_6().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly_6()[,(colnames(qb_data_energy_dat_ts_yearly_6()) %in% c('qb_data_energy_dat_ts_6().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly_6_1()[,(colnames(qb_data_energy_dat_ts_yearly_6_1()) %in% c('qb_data_energy_dat_ts_6_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly_6_1()[,(colnames(qb_data_energy_dat_ts_yearly_6_1()) %in% c('qb_data_energy_dat_ts_6_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_1_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_weekly_6()[,(colnames(qb_data_energy_dat_ts_weekly_6()) %in% c('qb_data_energy_subset_ts_6().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly_6()[,(colnames(qb_data_energy_dat_ts_weekly_6()) %in% c('qb_data_energy_subset_ts_6().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly_6_1()[,(colnames(qb_data_energy_dat_ts_weekly_6_1()) %in% c('qb_data_energy_subset_ts_6_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly_6_1()[,(colnames(qb_data_energy_dat_ts_weekly_6_1()) %in% c('qb_data_energy_subset_ts_6_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_1_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_daily_6()[,(colnames(qb_data_energy_dat_ts_daily_6()) %in% c('qb_data_energy_subset_ts_6().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_daily_6()[,(colnames(qb_data_energy_dat_ts_daily_6()) %in% c('qb_data_energy_subset_ts_6().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_daily_6_1()[,(colnames(qb_data_energy_dat_ts_daily_6_1()) %in% c('qb_data_energy_subset_ts_6_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_daily_6_1()[,(colnames(qb_data_energy_dat_ts_daily_6_1()) %in% c('qb_data_energy_subset_ts_6_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_1_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_hourly_6()[,(colnames(qb_data_energy_dat_ts_hourly_6()) %in% c('qb_data_energy_subset_ts_6().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly_6()[,(colnames(qb_data_energy_dat_ts_hourly_6()) %in% c('qb_data_energy_subset_ts_6().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly_6_1()[,(colnames(qb_data_energy_dat_ts_hourly_6_1()) %in% c('qb_data_energy_subset_ts_6_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly_6_1()[,(colnames(qb_data_energy_dat_ts_hourly_6_1()) %in% c('qb_data_energy_subset_ts_6_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_1_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(qb_data_energy_subset_ts_6(), type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_subset_ts_6_1(), type = "line", name = "High Load: ", color = "green")
  })
  
  
  qb_data_energy_subset_dat_7 <- reactive({subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= qb_date_ind_6_1() & qb_ind_dat_3$Date_time_local <= qb_date_ind_6_2()))})
  qb_data_energy_subset_dat_7_1 <- reactive({subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= qb_date_ind_6_1() & qb_ind_dat_3$Date_time_local <= qb_date_ind_6_2()))})
  
  qb_data_energy_dat_ts_7 <- reactive({xts(qb_ind_dat_3$producer_consumption_HQP,qb_ind_dat_3$Date_time_local)})
  qb_data_energy_subset_ts_7 <- reactive({xts(qb_data_energy_subset_dat_7()$producer_consumption_HQP,qb_data_energy_subset_dat_6()$Date_time_local)})
  
  qb_data_energy_dat_ts_7_1 <- reactive({xts(qb_ind_dat_3$consumption_loss_interruption,qb_ind_dat_3$Date_time_local)})
  qb_data_energy_subset_ts_7_1 <- reactive({xts(qb_data_energy_subset_dat_7_1()$consumption_loss_interruption,qb_data_energy_subset_dat_6_1()$Date_time_local)})
  
  qb_data_energy_dat_ts_yearly_7 <- reactive({to.yearly(qb_data_energy_dat_ts_7())})
  qb_data_energy_dat_ts_monthly_7 <- reactive({to.monthly(qb_data_energy_subset_ts_7())})
  qb_data_energy_dat_ts_weekly_7 <- reactive({to.weekly(qb_data_energy_subset_ts_7())})
  qb_data_energy_dat_ts_daily_7 <- reactive({to.daily(qb_data_energy_subset_ts_7())})
  qb_data_energy_dat_ts_hourly_7 <- reactive({to.hourly(qb_data_energy_subset_ts_7())})
  
  qb_data_energy_dat_ts_yearly_7_1 <- reactive({to.yearly(qb_data_energy_dat_ts_7_1())})
  qb_data_energy_dat_ts_monthly_7_1 <- reactive({to.monthly(qb_data_energy_subset_ts_7_1())})
  qb_data_energy_dat_ts_weekly_7_1 <- reactive({to.weekly(qb_data_energy_subset_ts_7_1())})
  qb_data_energy_dat_ts_daily_7_1 <- reactive({to.daily(qb_data_energy_subset_ts_7_1())})
  qb_data_energy_dat_ts_hourly_7_1 <- reactive({to.hourly(qb_data_energy_subset_ts_7_1())})
  
  output$QB_ENE_2_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_yearly_7()[,(colnames(qb_data_energy_dat_ts_yearly_7()) %in% c('qb_data_energy_dat_ts_7().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly_7()[,(colnames(qb_data_energy_dat_ts_yearly_7()) %in% c('qb_data_energy_dat_ts_7().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly_7_1()[,(colnames(qb_data_energy_dat_ts_yearly_7_1()) %in% c('qb_data_energy_dat_ts_7_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly_7_1()[,(colnames(qb_data_energy_dat_ts_yearly_7_1()) %in% c('qb_data_energy_dat_ts_7_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_2_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_weekly_7()[,(colnames(qb_data_energy_dat_ts_weekly_7()) %in% c('qb_data_energy_subset_ts_7().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly_7()[,(colnames(qb_data_energy_dat_ts_weekly_7()) %in% c('qb_data_energy_subset_ts_7().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly_7_1()[,(colnames(qb_data_energy_dat_ts_weekly_7_1()) %in% c('qb_data_energy_subset_ts_7_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly_7_1()[,(colnames(qb_data_energy_dat_ts_weekly_7_1()) %in% c('qb_data_energy_subset_ts_7_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_2_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_daily_7()[,(colnames(qb_data_energy_dat_ts_daily_7()) %in% c('qb_data_energy_subset_ts_7().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_daily_7()[,(colnames(qb_data_energy_dat_ts_daily_7()) %in% c('qb_data_energy_subset_ts_7().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_daily_7_1()[,(colnames(qb_data_energy_dat_ts_daily_7_1()) %in% c('qb_data_energy_subset_ts_7_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_daily_7_1()[,(colnames(qb_data_energy_dat_ts_daily_7_1()) %in% c('qb_data_energy_subset_ts_7_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_2_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_hourly_7()[,(colnames(qb_data_energy_dat_ts_hourly_7()) %in% c('qb_data_energy_subset_ts_7().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly_7()[,(colnames(qb_data_energy_dat_ts_hourly_7()) %in% c('qb_data_energy_subset_ts_7().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly_7_1()[,(colnames(qb_data_energy_dat_ts_hourly_7_1()) %in% c('qb_data_energy_subset_ts_7_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly_7_1()[,(colnames(qb_data_energy_dat_ts_hourly_7_1()) %in% c('qb_data_energy_subset_ts_7_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_2_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(qb_data_energy_subset_ts_7(), type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_subset_ts_7_1(), type = "line", name = "High Load: ", color = "green")
  })
  
  
  
  qb_data_energy_subset_dat_8 <- reactive({subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= qb_date_ind_7_1() & qb_ind_dat_3$Date_time_local <= qb_date_ind_7_2()))})
  qb_data_energy_subset_dat_8_1 <- reactive({subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= qb_date_ind_7_1() & qb_ind_dat_3$Date_time_local <= qb_date_ind_7_2()))})
  
  qb_data_energy_dat_ts_8 <- reactive({xts(qb_ind_dat_3$load,qb_ind_dat_3$Date_time_local)})
  qb_data_energy_subset_ts_8 <- reactive({xts(qb_data_energy_subset_dat_8()$load,qb_data_energy_subset_dat_8()$Date_time_local)})
  
  qb_data_energy_dat_ts_8_1 <- reactive({xts(qb_ind_dat_3$consumption,qb_ind_dat_3$Date_time_local)})
  qb_data_energy_subset_ts_8_1 <- reactive({xts(qb_data_energy_subset_dat_8_1()$consumption,qb_data_energy_subset_dat_8_1()$Date_time_local)})
  
  qb_data_energy_dat_ts_yearly_8 <- reactive({to.yearly(qb_data_energy_dat_ts_8())})
  qb_data_energy_dat_ts_monthly_8 <- reactive({to.monthly(qb_data_energy_subset_ts_8())})
  qb_data_energy_dat_ts_weekly_8 <- reactive({to.weekly(qb_data_energy_subset_ts_8())})
  qb_data_energy_dat_ts_daily_8 <- reactive({to.daily(qb_data_energy_subset_ts_8())})
  qb_data_energy_dat_ts_hourly_8 <- reactive({to.hourly(qb_data_energy_subset_ts_8())})
  
  qb_data_energy_dat_ts_yearly_8_1 <- reactive({to.yearly(qb_data_energy_dat_ts_8_1())})
  qb_data_energy_dat_ts_monthly_8_1 <- reactive({to.monthly(qb_data_energy_subset_ts_8_1())})
  qb_data_energy_dat_ts_weekly_8_1 <- reactive({to.weekly(qb_data_energy_subset_ts_8_1())})
  qb_data_energy_dat_ts_daily_8_1 <- reactive({to.daily(qb_data_energy_subset_ts_8_1())})
  qb_data_energy_dat_ts_hourly_8_1 <- reactive({to.hourly(qb_data_energy_subset_ts_8_1())})
  
  output$QB_ENE_3_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_yearly_8()[,(colnames(qb_data_energy_dat_ts_yearly_8()) %in% c('qb_data_energy_dat_ts_8().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly_8()[,(colnames(qb_data_energy_dat_ts_yearly_8()) %in% c('qb_data_energy_dat_ts_8().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly_8_1()[,(colnames(qb_data_energy_dat_ts_yearly_8_1()) %in% c('qb_data_energy_dat_ts_8_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly_8_1()[,(colnames(qb_data_energy_dat_ts_yearly_8_1()) %in% c('qb_data_energy_dat_ts_8_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_3_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_weekly_8()[,(colnames(qb_data_energy_dat_ts_weekly_8()) %in% c('qb_data_energy_subset_ts_8().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly_8()[,(colnames(qb_data_energy_dat_ts_weekly_8()) %in% c('qb_data_energy_subset_ts_8().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly_8_1()[,(colnames(qb_data_energy_dat_ts_weekly_8_1()) %in% c('qb_data_energy_subset_ts_8_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly_8_1()[,(colnames(qb_data_energy_dat_ts_weekly_8_1()) %in% c('qb_data_energy_subset_ts_8_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_3_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_daily_8()[,(colnames(qb_data_energy_dat_ts_daily_8()) %in% c('qb_data_energy_subset_ts_8().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_daily_8()[,(colnames(qb_data_energy_dat_ts_daily_8()) %in% c('qb_data_energy_subset_ts_8().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_daily_8_1()[,(colnames(qb_data_energy_dat_ts_daily_8_1()) %in% c('qb_data_energy_subset_ts_8_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_daily_8_1()[,(colnames(qb_data_energy_dat_ts_daily_8_1()) %in% c('qb_data_energy_subset_ts_8_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_3_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_hourly_8()[,(colnames(qb_data_energy_dat_ts_hourly_8()) %in% c('qb_data_energy_subset_ts_8().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly_8()[,(colnames(qb_data_energy_dat_ts_hourly_8()) %in% c('qb_data_energy_subset_ts_8().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly_8_1()[,(colnames(qb_data_energy_dat_ts_hourly_8_1()) %in% c('qb_data_energy_subset_ts_8_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly_8_1()[,(colnames(qb_data_energy_dat_ts_hourly_8_1()) %in% c('qb_data_energy_subset_ts_8_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_3_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(qb_data_energy_subset_ts_8(), type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_subset_ts_8_1(), type = "line", name = "High Load: ", color = "green")
  })
  
  
  qb_data_energy_subset_dat_9 <- reactive({subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= qb_date_ind_8_1() & qb_ind_dat_3$Date_time_local <= qb_date_ind_8_2()))})
  qb_data_energy_subset_dat_9_1 <- reactive({subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= qb_date_ind_8_1() & qb_ind_dat_3$Date_time_local <= qb_date_ind_8_2()))})
  
  qb_data_energy_dat_ts_9 <- reactive({xts(qb_ind_dat_3$export,qb_ind_dat_3$Date_time_local)})
  qb_data_energy_subset_ts_9 <- reactive({xts(qb_data_energy_subset_dat_9()$export,qb_data_energy_subset_dat_9()$Date_time_local)})
  
  qb_data_energy_dat_ts_9_1 <- reactive({xts(qb_ind_dat_3$electrivity_delivered_HQD,qb_ind_dat_3$Date_time_local)})
  qb_data_energy_subset_ts_9_1 <- reactive({xts(qb_data_energy_subset_dat_9_1()$electrivity_delivered_HQD,qb_data_energy_subset_dat_9_1()$Date_time_local)})
  
  qb_data_energy_dat_ts_yearly_9 <- reactive({to.yearly(qb_data_energy_dat_ts_9())})
  qb_data_energy_dat_ts_monthly_9 <- reactive({to.monthly(qb_data_energy_subset_ts_9())})
  qb_data_energy_dat_ts_weekly_9 <- reactive({to.weekly(qb_data_energy_subset_ts_9())})
  qb_data_energy_dat_ts_daily_9 <- reactive({to.daily(qb_data_energy_subset_ts_9())})
  qb_data_energy_dat_ts_hourly_9 <- reactive({to.hourly(qb_data_energy_subset_ts_9())})
  
  qb_data_energy_dat_ts_yearly_9_1 <- reactive({to.yearly(qb_data_energy_dat_ts_9_1())})
  qb_data_energy_dat_ts_monthly_9_1 <- reactive({to.monthly(qb_data_energy_subset_ts_9_1())})
  qb_data_energy_dat_ts_weekly_9_1 <- reactive({to.weekly(qb_data_energy_subset_ts_9_1())})
  qb_data_energy_dat_ts_daily_9_1 <- reactive({to.daily(qb_data_energy_subset_ts_9_1())})
  qb_data_energy_dat_ts_hourly_9_1 <- reactive({to.hourly(qb_data_energy_subset_ts_9_1())})
  
  output$QB_ENE_4_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_yearly_9()[,(colnames(qb_data_energy_dat_ts_yearly_9()) %in% c('qb_data_energy_dat_ts_9().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly_9()[,(colnames(qb_data_energy_dat_ts_yearly_9()) %in% c('qb_data_energy_dat_ts_9().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly_9_1()[,(colnames(qb_data_energy_dat_ts_yearly_9_1()) %in% c('qb_data_energy_dat_ts_9_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly_9_1()[,(colnames(qb_data_energy_dat_ts_yearly_9_1()) %in% c('qb_data_energy_dat_ts_9_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_4_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_weekly_9()[,(colnames(qb_data_energy_dat_ts_weekly_9()) %in% c('qb_data_energy_subset_ts_9().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly_9()[,(colnames(qb_data_energy_dat_ts_weekly_9()) %in% c('qb_data_energy_subset_ts_9().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly_9_1()[,(colnames(qb_data_energy_dat_ts_weekly_9_1()) %in% c('qb_data_energy_subset_ts_9_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly_9_1()[,(colnames(qb_data_energy_dat_ts_weekly_9_1()) %in% c('qb_data_energy_subset_ts_9_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_4_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_daily_9()[,(colnames(qb_data_energy_dat_ts_daily_9()) %in% c('qb_data_energy_subset_ts_9().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_daily_9()[,(colnames(qb_data_energy_dat_ts_daily_9()) %in% c('qb_data_energy_subset_ts_9().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_daily_9_1()[,(colnames(qb_data_energy_dat_ts_daily_9_1()) %in% c('qb_data_energy_subset_ts_9_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_daily_9_1()[,(colnames(qb_data_energy_dat_ts_daily_9_1()) %in% c('qb_data_energy_subset_ts_9_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_4_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_hourly_9()[,(colnames(qb_data_energy_dat_ts_hourly_9()) %in% c('qb_data_energy_subset_ts_9().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly_9()[,(colnames(qb_data_energy_dat_ts_hourly_9()) %in% c('qb_data_energy_subset_ts_9().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly_9_1()[,(colnames(qb_data_energy_dat_ts_hourly_9_1()) %in% c('qb_data_energy_subset_ts_9_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly_9_1()[,(colnames(qb_data_energy_dat_ts_hourly_9_1()) %in% c('qb_data_energy_subset_ts_9_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_4_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(qb_data_energy_subset_ts_9(), type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_subset_ts_9_1(), type = "line", name = "High Load: ", color = "green")
  })
  
  
  qb_data_energy_subset_dat_10 <- reactive({subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= qb_date_ind_9_1() & qb_ind_dat_3$Date_time_local <= qb_date_ind_9_2()))})
  qb_data_energy_subset_dat_10_1 <- reactive({subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= qb_date_ind_9_1() & qb_ind_dat_3$Date_time_local <= qb_date_ind_9_2()))})
  
  qb_data_energy_dat_ts_10 <- reactive({xts(qb_ind_dat_3$vol_nonheritage_supply,qb_ind_dat_3$Date_time_local)})
  qb_data_energy_subset_ts_10 <- reactive({xts(qb_data_energy_subset_dat_10()$vol_nonheritage_supply,qb_data_energy_subset_dat_10()$Date_time_local)})
  
  qb_data_energy_dat_ts_10_1 <- reactive({xts(qb_ind_dat_3$vol_excessofheritage_mobilized,qb_ind_dat_3$Date_time_local)})
  qb_data_energy_subset_ts_10_1 <- reactive({xts(qb_data_energy_subset_dat_10_1()$vol_excessofheritage_mobilized,qb_data_energy_subset_dat_10_1()$Date_time_local)})
  
  qb_data_energy_dat_ts_yearly_10 <- reactive({to.yearly(qb_data_energy_dat_ts_10())})
  qb_data_energy_dat_ts_monthly_10 <- reactive({to.monthly(qb_data_energy_subset_ts_10())})
  qb_data_energy_dat_ts_weekly_10 <- reactive({to.weekly(qb_data_energy_subset_ts_10())})
  qb_data_energy_dat_ts_daily_10 <- reactive({to.daily(qb_data_energy_subset_ts_10())})
  qb_data_energy_dat_ts_hourly_10 <- reactive({to.hourly(qb_data_energy_subset_ts_10())})
  
  qb_data_energy_dat_ts_yearly_10_1 <- reactive({to.yearly(qb_data_energy_dat_ts_10_1())})
  qb_data_energy_dat_ts_monthly_10_1 <- reactive({to.monthly(qb_data_energy_subset_ts_10_1())})
  qb_data_energy_dat_ts_weekly_10_1 <- reactive({to.weekly(qb_data_energy_subset_ts_10_1())})
  qb_data_energy_dat_ts_daily_10_1 <- reactive({to.daily(qb_data_energy_subset_ts_10_1())})
  qb_data_energy_dat_ts_hourly_10_1 <- reactive({to.hourly(qb_data_energy_subset_ts_10_1())})
  
  output$QB_ENE_5_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_yearly_10()[,(colnames(qb_data_energy_dat_ts_yearly_10()) %in% c('qb_data_energy_dat_ts_10().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly_10()[,(colnames(qb_data_energy_dat_ts_yearly_10()) %in% c('qb_data_energy_dat_ts_10().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly_10_1()[,(colnames(qb_data_energy_dat_ts_yearly_10_1()) %in% c('qb_data_energy_dat_ts_10_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly_10_1()[,(colnames(qb_data_energy_dat_ts_yearly_10_1()) %in% c('qb_data_energy_dat_ts_10_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_5_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_weekly_10()[,(colnames(qb_data_energy_dat_ts_weekly_10()) %in% c('qb_data_energy_subset_ts_10().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly_10()[,(colnames(qb_data_energy_dat_ts_weekly_10()) %in% c('qb_data_energy_subset_ts_10().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly_10_1()[,(colnames(qb_data_energy_dat_ts_weekly_10_1()) %in% c('qb_data_energy_subset_ts_10_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly_10_1()[,(colnames(qb_data_energy_dat_ts_weekly_10_1()) %in% c('qb_data_energy_subset_ts_10_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_5_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_daily_10()[,(colnames(qb_data_energy_dat_ts_daily_10()) %in% c('qb_data_energy_subset_ts_10().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_daily_10()[,(colnames(qb_data_energy_dat_ts_daily_10()) %in% c('qb_data_energy_subset_ts_10().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_daily_10_1()[,(colnames(qb_data_energy_dat_ts_daily_10_1()) %in% c('qb_data_energy_subset_ts_10_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_daily_10_1()[,(colnames(qb_data_energy_dat_ts_daily_10_1()) %in% c('qb_data_energy_subset_ts_10_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_5_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_hourly_10()[,(colnames(qb_data_energy_dat_ts_hourly_10()) %in% c('qb_data_energy_subset_ts_10().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly_10()[,(colnames(qb_data_energy_dat_ts_hourly_10()) %in% c('qb_data_energy_subset_ts_10().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly_10_1()[,(colnames(qb_data_energy_dat_ts_hourly_10_1()) %in% c('qb_data_energy_subset_ts_10_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly_10_1()[,(colnames(qb_data_energy_dat_ts_hourly_10_1()) %in% c('qb_data_energy_subset_ts_10_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_5_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(qb_data_energy_subset_ts_10(), type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_subset_ts_10_1(), type = "line", name = "High Load: ", color = "green")
  })
  
  
  qb_data_energy_subset_dat_11 <- reactive({subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= qb_date_ind_10_1() & qb_ind_dat_3$Date_time_local <= qb_date_ind_10_2()))})
  qb_data_energy_subset_dat_11_1 <- reactive({subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= qb_date_ind_10_1() & qb_ind_dat_3$Date_time_local <= qb_date_ind_10_2()))})
  qb_data_energy_subset_dat_11_2 <- reactive({subset(qb_ind_dat_2,subset = (qb_ind_dat_2$Date_Time_UTC >= qb_date_ind_12_1() & qb_ind_dat_2$Date_Time_UTC <= qb_date_ind_12_2()))})
  
  qb_data_energy_dat_ts_11 <- reactive({xts(qb_ind_dat_3$total_consumption,qb_ind_dat_3$Date_time_local)})
  qb_data_energy_subset_ts_11 <- reactive({xts(qb_data_energy_subset_dat_11()$total_consumption,qb_data_energy_subset_dat_11()$Date_time_local)})
  
  qb_data_energy_dat_ts_11_1 <- reactive({xts(qb_ind_dat_3$unitcost_excess,qb_ind_dat_3$Date_time_local)})
  qb_data_energy_subset_ts_11_1 <- reactive({xts(qb_data_energy_subset_dat_11_1()$unitcost_excess,qb_data_energy_subset_dat_11_1()$Date_time_local)})
  
  qb_data_energy_dat_ts_11_2 <- reactive({xts(qb_ind_dat_2$total_demand,qb_ind_dat_2$Date_Time_UTC)})
  qb_data_energy_subset_ts_11_2 <- reactive({xts(qb_data_energy_subset_dat_11_2()$total_demand,qb_data_energy_subset_dat_11_2()$Date_Time_UTC)})
  
  qb_data_energy_dat_ts_yearly_11 <- reactive({to.yearly(qb_data_energy_dat_ts_11())})
  qb_data_energy_dat_ts_monthly_11 <- reactive({to.monthly(qb_data_energy_subset_ts_11())})
  qb_data_energy_dat_ts_weekly_11 <- reactive({to.weekly(qb_data_energy_subset_ts_11())})
  qb_data_energy_dat_ts_daily_11 <- reactive({to.daily(qb_data_energy_subset_ts_11())})
  qb_data_energy_dat_ts_hourly_11 <- reactive({to.hourly(qb_data_energy_subset_ts_11())})
  
  qb_data_energy_dat_ts_yearly_11_1 <- reactive({to.yearly(qb_data_energy_dat_ts_11_1())})
  qb_data_energy_dat_ts_monthly_11_1 <- reactive({to.monthly(qb_data_energy_subset_ts_11_1())})
  qb_data_energy_dat_ts_weekly_11_1 <- reactive({to.weekly(qb_data_energy_subset_ts_11_1())})
  qb_data_energy_dat_ts_daily_11_1 <- reactive({to.daily(qb_data_energy_subset_ts_11_1())})
  qb_data_energy_dat_ts_hourly_11_1 <- reactive({to.hourly(qb_data_energy_subset_ts_11_1())})
  
  qb_data_energy_dat_ts_yearly_11_2 <- reactive({to.yearly(qb_data_energy_dat_ts_11_2())})
  qb_data_energy_dat_ts_monthly_11_2 <- reactive({to.monthly(qb_data_energy_subset_ts_11_2())})
  qb_data_energy_dat_ts_weekly_11_2 <- reactive({to.weekly(qb_data_energy_subset_ts_11_2())})
  qb_data_energy_dat_ts_daily_11_2 <- reactive({to.daily(qb_data_energy_subset_ts_11_2())})
  qb_data_energy_dat_ts_hourly_11_2 <- reactive({to.hourly(qb_data_energy_subset_ts_11_2())})
  
  output$QB_ENE_6_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_yearly_11()[,(colnames(qb_data_energy_dat_ts_yearly_11()) %in% c('qb_data_energy_dat_ts_11().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly_11()[,(colnames(qb_data_energy_dat_ts_yearly_11()) %in% c('qb_data_energy_dat_ts_11().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly_11_1()[,(colnames(qb_data_energy_dat_ts_yearly_11_1()) %in% c('qb_data_energy_dat_ts_11_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly_11_1()[,(colnames(qb_data_energy_dat_ts_yearly_11_1()) %in% c('qb_data_energy_dat_ts_11_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_6_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_weekly_11()[,(colnames(qb_data_energy_dat_ts_weekly_11()) %in% c('qb_data_energy_subset_ts_11().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly_11()[,(colnames(qb_data_energy_dat_ts_weekly_11()) %in% c('qb_data_energy_subset_ts_11().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly_11_1()[,(colnames(qb_data_energy_dat_ts_weekly_11_1()) %in% c('qb_data_energy_subset_ts_11_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly_11_1()[,(colnames(qb_data_energy_dat_ts_weekly_11_1()) %in% c('qb_data_energy_subset_ts_11_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_6_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_daily_11()[,(colnames(qb_data_energy_dat_ts_daily_11()) %in% c('qb_data_energy_subset_ts_11().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_daily_11()[,(colnames(qb_data_energy_dat_ts_daily_11()) %in% c('qb_data_energy_subset_ts_11().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_daily_11_1()[,(colnames(qb_data_energy_dat_ts_daily_11_1()) %in% c('qb_data_energy_subset_ts_11_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_daily_11_1()[,(colnames(qb_data_energy_dat_ts_daily_11_1()) %in% c('qb_data_energy_subset_ts_11_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_6_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_hourly_11()[,(colnames(qb_data_energy_dat_ts_hourly_11()) %in% c('qb_data_energy_subset_ts_11().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly_11()[,(colnames(qb_data_energy_dat_ts_hourly_11()) %in% c('qb_data_energy_subset_ts_11().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly_11_1()[,(colnames(qb_data_energy_dat_ts_hourly_11_1()) %in% c('qb_data_energy_subset_ts_11_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly_11_1()[,(colnames(qb_data_energy_dat_ts_hourly_11_1()) %in% c('qb_data_energy_subset_ts_11_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_6_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(qb_data_energy_subset_ts_11(), type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_subset_ts_11_1(), type = "line", name = "High Load: ", color = "green")
  })
  
  output$QB_ENE_8_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_yearly_11_2()[,(colnames(qb_data_energy_dat_ts_yearly_11_2()) %in% c('qb_data_energy_dat_ts_11_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly_11_2()[,(colnames(qb_data_energy_dat_ts_yearly_11_2()) %in% c('qb_data_energy_dat_ts_11_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_8_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_weekly_11_2()[,(colnames(qb_data_energy_dat_ts_weekly_11_2()) %in% c('qb_data_energy_subset_ts_11_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly_11_2()[,(colnames(qb_data_energy_dat_ts_weekly_11_2()) %in% c('qb_data_energy_subset_ts_11_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_8_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_daily_11_2()[,(colnames(qb_data_energy_dat_ts_daily_11_2()) %in% c('qb_data_energy_subset_ts_11_2().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_daily_11_2()[,(colnames(qb_data_energy_dat_ts_daily_11_2()) %in% c('qb_data_energy_subset_ts_11_2().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_8_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_hourly_11_2()[,(colnames(qb_data_energy_dat_ts_hourly_11_2()) %in% c('qb_data_energy_subset_ts_11_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly_11_2()[,(colnames(qb_data_energy_dat_ts_hourly_11_2()) %in% c('qb_data_energy_subset_ts_11_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_8_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(qb_data_energy_subset_ts_11_2(), type = "line", name = "High Load: ", color = "green")
  })
  
  
  qb_data_energy_subset_dat_12 <- reactive({subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= qb_date_ind_11_1() & qb_ind_dat_3$Date_time_local <= qb_date_ind_11_2()))})
  qb_data_energy_subset_dat_12_1 <- reactive({subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= qb_date_ind_11_1() & qb_ind_dat_3$Date_time_local <= qb_date_ind_11_2()))})
  
  
  qb_data_energy_dat_ts_12 <- reactive({xts(qb_ind_dat_3$cost,qb_ind_dat_3$Date_time_local)})
  qb_data_energy_subset_ts_12 <- reactive({xts(qb_data_energy_subset_dat_12()$cost,qb_data_energy_subset_dat_12()$Date_time_local)})
  
  qb_data_energy_dat_ts_12_1 <- reactive({xts(qb_ind_dat_3$cost_hourly,qb_ind_dat_3$Date_time_local)})
  qb_data_energy_subset_ts_12_1 <- reactive({xts(qb_data_energy_subset_dat_12_1()$cost_hourly,qb_data_energy_subset_dat_12_1()$Date_time_local)})
  
  qb_data_energy_dat_ts_yearly_12 <- reactive({to.yearly(qb_data_energy_dat_ts_12())})
  qb_data_energy_dat_ts_monthly_12 <- reactive({to.monthly(qb_data_energy_subset_ts_12())})
  qb_data_energy_dat_ts_weekly_12 <- reactive({to.weekly(qb_data_energy_subset_ts_12())})
  qb_data_energy_dat_ts_daily_12 <- reactive({to.daily(qb_data_energy_subset_ts_12())})
  qb_data_energy_dat_ts_hourly_12 <- reactive({to.hourly(qb_data_energy_subset_ts_12())})
  
  qb_data_energy_dat_ts_yearly_12_1 <- reactive({to.yearly(qb_data_energy_dat_ts_12_1())})
  qb_data_energy_dat_ts_monthly_12_1 <- reactive({to.monthly(qb_data_energy_subset_ts_12_1())})
  qb_data_energy_dat_ts_weekly_12_1 <- reactive({to.weekly(qb_data_energy_subset_ts_12_1())})
  qb_data_energy_dat_ts_daily_12_1 <- reactive({to.daily(qb_data_energy_subset_ts_12_1())})
  qb_data_energy_dat_ts_hourly_12_1 <- reactive({to.hourly(qb_data_energy_subset_ts_12_1())})
  
  output$QB_ENE_7_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_yearly_12()[,(colnames(qb_data_energy_dat_ts_yearly_12()) %in% c('qb_data_energy_dat_ts_12().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly_12()[,(colnames(qb_data_energy_dat_ts_yearly_12()) %in% c('qb_data_energy_dat_ts_12().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly_12_1()[,(colnames(qb_data_energy_dat_ts_yearly_12_1()) %in% c('qb_data_energy_dat_ts_12_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_yearly_12_1()[,(colnames(qb_data_energy_dat_ts_yearly_12_1()) %in% c('qb_data_energy_dat_ts_12_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_7_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_weekly_12()[,(colnames(qb_data_energy_dat_ts_weekly_12()) %in% c('qb_data_energy_subset_ts_12().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly_12()[,(colnames(qb_data_energy_dat_ts_weekly_12()) %in% c('qb_data_energy_subset_ts_12().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly_12_1()[,(colnames(qb_data_energy_dat_ts_weekly_12_1()) %in% c('qb_data_energy_subset_ts_12_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_weekly_12_1()[,(colnames(qb_data_energy_dat_ts_weekly_12_1()) %in% c('qb_data_energy_subset_ts_12_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_7_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_daily_12()[,(colnames(qb_data_energy_dat_ts_daily_12()) %in% c('qb_data_energy_subset_ts_12().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_daily_12()[,(colnames(qb_data_energy_dat_ts_daily_12()) %in% c('qb_data_energy_subset_ts_12().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_daily_12_1()[,(colnames(qb_data_energy_dat_ts_daily_12_1()) %in% c('qb_data_energy_subset_ts_12_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_daily_12_1()[,(colnames(qb_data_energy_dat_ts_daily_12_1()) %in% c('qb_data_energy_subset_ts_12_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_7_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_data_energy_dat_ts_hourly_12()[,(colnames(qb_data_energy_dat_ts_hourly_12()) %in% c('qb_data_energy_subset_ts_12().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly_12()[,(colnames(qb_data_energy_dat_ts_hourly_12()) %in% c('qb_data_energy_subset_ts_12().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly_12_1()[,(colnames(qb_data_energy_dat_ts_hourly_12_1()) %in% c('qb_data_energy_subset_ts_12_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_dat_ts_hourly_12_1()[,(colnames(qb_data_energy_dat_ts_hourly_12_1()) %in% c('qb_data_energy_subset_ts_12_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$QB_ENE_7_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(qb_data_energy_subset_ts_12(), type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(qb_data_energy_subset_ts_12_1(), type = "line", name = "High Load: ", color = "green")
  })
  
  
  
  
  
  
  ab_date_ind_8_1 <- reactive({paste(input$ab_dates_8[1],"00:00:00",sep = " ")})
  ab_date_ind_8_2 <- reactive({paste(input$ab_dates_8[2],"00:00:00",sep = " ")})
  ab_ind_dat_8_asset_filter <- reactive({input$ab_ind_8_ff})
  
  ab_ind_dat_8_sub_1 <- reactive({subset(ab_ind_dat_8,subset = (ab_ind_dat_8$Update_Time >= ab_date_ind_8_1() & ab_ind_dat_8$Update_Time <= ab_date_ind_8_2()))})
  ab_ind_dat_8_sub_2 <- reactive({subset(ab_ind_dat_8_sub_1(),subset = (ab_ind_dat_8_sub_1()$ASSET == ab_ind_dat_8_asset_filter()))})
  
  ab_ind_dat_8_sub_1_xts_1_1 <- reactive({xts(ab_ind_dat_8_sub_2()$MC,ab_ind_dat_8_sub_2()$Update_Time)})
  ab_ind_dat_8_sub_1_xts_1_2 <- reactive({xts(ab_ind_dat_8_sub_2()$TNG,ab_ind_dat_8_sub_2()$Update_Time)})
  ab_ind_dat_8_sub_1_xts_1_3 <- reactive({xts(ab_ind_dat_8_sub_2()$DCR,ab_ind_dat_8_sub_2()$Update_Time)})
  
  ab_ind_dat_8_sub_1_xts_2_1 <- reactive({xts(ab_ind_dat_8_sub_1()$MC,ab_ind_dat_8_sub_1()$Update_Time)})
  ab_ind_dat_8_sub_1_xts_2_2 <- reactive({xts(ab_ind_dat_8_sub_1()$TNG,ab_ind_dat_8_sub_1()$Update_Time)})
  ab_ind_dat_8_sub_1_xts_2_3 <- reactive({xts(ab_ind_dat_8_sub_1()$DCR,ab_ind_dat_8_sub_1()$Update_Time)})
  
  ab_ind_dat_8_sub_1_xts_1_1_yearly_1 <- reactive({to.yearly(ab_ind_dat_8_sub_1_xts_2_1())})
  ab_ind_dat_8_sub_1_xts_1_1_yearly_2 <- reactive({to.yearly(ab_ind_dat_8_sub_1_xts_2_2())})
  ab_ind_dat_8_sub_1_xts_1_1_yearly_3 <- reactive({to.yearly(ab_ind_dat_8_sub_1_xts_2_3())})
  ab_ind_dat_8_sub_1_xts_1_1_monthly_1 <- reactive({to.monthly(ab_ind_dat_8_sub_1_xts_1_1())})
  ab_ind_dat_8_sub_1_xts_1_1_monthly_2 <- reactive({to.monthly(ab_ind_dat_8_sub_1_xts_1_2())})
  ab_ind_dat_8_sub_1_xts_1_1_monthly_3 <- reactive({to.monthly(ab_ind_dat_8_sub_1_xts_1_3())})
  ab_ind_dat_8_sub_1_xts_1_1_weekly_1 <- reactive({to.weekly(ab_ind_dat_8_sub_1_xts_1_1())})
  ab_ind_dat_8_sub_1_xts_1_1_weekly_2 <- reactive({to.weekly(ab_ind_dat_8_sub_1_xts_1_2())})
  ab_ind_dat_8_sub_1_xts_1_1_weekly_3 <- reactive({to.weekly(ab_ind_dat_8_sub_1_xts_1_3())})
  ab_ind_dat_8_sub_1_xts_1_1_daily_1 <- reactive({to.daily(ab_ind_dat_8_sub_1_xts_1_1())})
  ab_ind_dat_8_sub_1_xts_1_1_daily_2 <- reactive({to.daily(ab_ind_dat_8_sub_1_xts_1_2())})
  ab_ind_dat_8_sub_1_xts_1_1_daily_3 <- reactive({to.daily(ab_ind_dat_8_sub_1_xts_1_3())})
  ab_ind_dat_8_sub_1_xts_1_1_hourly_1 <- reactive({to.hourly(ab_ind_dat_8_sub_1_xts_1_1())})
  ab_ind_dat_8_sub_1_xts_1_1_hourly_2 <- reactive({to.hourly(ab_ind_dat_8_sub_1_xts_1_2())})
  ab_ind_dat_8_sub_1_xts_1_1_hourly_3 <- reactive({to.hourly(ab_ind_dat_8_sub_1_xts_1_3())})
  
  output$AB_ENE_8_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_yearly_1()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_yearly_1()) %in% c('ab_ind_dat_8_sub_1_xts_2_1().High'))], type = "line", name = "High MC", color = "green") %>%
      hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_yearly_1()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_yearly_1()) %in% c('ab_ind_dat_8_sub_1_xts_2_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
      hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_yearly_2()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_yearly_2()) %in% c('ab_ind_dat_8_sub_1_xts_2_2().High'))], type = "line", name = "High TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_yearly_2()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_yearly_2()) %in% c('ab_ind_dat_8_sub_1_xts_2_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
      hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_yearly_3()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_yearly_3()) %in% c('ab_ind_dat_8_sub_1_xts_2_3().High'))], type = "line", name = "High DCR", color = "green") %>%
      hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_yearly_3()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_yearly_3()) %in% c('ab_ind_dat_8_sub_1_xts_2_3().Low'))], type = "line", name = "Low DCR ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$AB_ENE_8_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_weekly_1()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_weekly_1()) %in% c('ab_ind_dat_8_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
      hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_weekly_1()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_weekly_1()) %in% c('ab_ind_dat_8_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
      hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_weekly_2()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_weekly_2()) %in% c('ab_ind_dat_8_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_weekly_2()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_weekly_2()) %in% c('ab_ind_dat_8_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
      hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_weekly_3()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_weekly_3()) %in% c('ab_ind_dat_8_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
      hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_weekly_3()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_weekly_3()) %in% c('ab_ind_dat_8_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$AB_ENE_8_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_daily_1()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_daily_1()) %in% c('ab_ind_dat_8_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
      hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_daily_1()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_daily_1()) %in% c('ab_ind_dat_8_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
      hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_daily_2()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_daily_2()) %in% c('ab_ind_dat_8_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_daily_2()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_daily_2()) %in% c('ab_ind_dat_8_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
      hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_daily_3()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_daily_3()) %in% c('ab_ind_dat_8_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
      hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_daily_3()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_daily_3()) %in% c('ab_ind_dat_8_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$AB_ENE_8_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_hourly_1()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_hourly_1()) %in% c('ab_ind_dat_8_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
      hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_hourly_1()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_hourly_1()) %in% c('ab_ind_dat_8_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
      hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_hourly_2()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_hourly_2()) %in% c('ab_ind_dat_8_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_hourly_2()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_hourly_2()) %in% c('ab_ind_dat_8_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
      hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_hourly_3()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_hourly_3()) %in% c('ab_ind_dat_8_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
      hc_add_series(ab_ind_dat_8_sub_1_xts_1_1_hourly_3()[,(colnames(ab_ind_dat_8_sub_1_xts_1_1_hourly_3()) %in% c('ab_ind_dat_8_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$AB_ENE_8_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(ab_ind_dat_8_sub_1_xts_1_1(), type = "line", name = "MC", color = "green") %>%
      hc_add_series(ab_ind_dat_8_sub_1_xts_1_2(), type = "line", name = "TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_8_sub_1_xts_1_3(), type = "line", name = "DCR", color = "green")
  })
  
  ab_date_ind_9_1 <- reactive({paste(input$ab_dates_9[1],"00:00:00",sep = " ")})
  ab_date_ind_9_2 <- reactive({paste(input$ab_dates_9[2],"00:00:00",sep = " ")})
  ab_ind_dat_9_asset_filter <- reactive({input$ab_ind_9_ff})
  
  ab_ind_dat_9_sub_1 <- reactive({subset(ab_ind_dat_9,subset = (ab_ind_dat_9$Update_Time >= ab_date_ind_9_1() & ab_ind_dat_9$Update_Time <= ab_date_ind_9_2()))})
  ab_ind_dat_9_sub_2 <- reactive({subset(ab_ind_dat_9_sub_1(),subset = (ab_ind_dat_9_sub_1()$ASSET == ab_ind_dat_9_asset_filter()))})
  
  ab_ind_dat_9_sub_1_xts_1_1 <- reactive({xts(ab_ind_dat_9_sub_2()$MC,ab_ind_dat_9_sub_2()$Update_Time)})
  ab_ind_dat_9_sub_1_xts_1_2 <- reactive({xts(ab_ind_dat_9_sub_2()$TNG,ab_ind_dat_9_sub_2()$Update_Time)})
  ab_ind_dat_9_sub_1_xts_1_3 <- reactive({xts(ab_ind_dat_9_sub_2()$DCR,ab_ind_dat_9_sub_2()$Update_Time)})
  
  ab_ind_dat_9_sub_1_xts_2_1 <- reactive({xts(ab_ind_dat_9_sub_1()$MC,ab_ind_dat_9_sub_1()$Update_Time)})
  ab_ind_dat_9_sub_1_xts_2_2 <- reactive({xts(ab_ind_dat_9_sub_1()$TNG,ab_ind_dat_9_sub_1()$Update_Time)})
  ab_ind_dat_9_sub_1_xts_2_3 <- reactive({xts(ab_ind_dat_9_sub_1()$DCR,ab_ind_dat_9_sub_1()$Update_Time)})
  
  ab_ind_dat_9_sub_1_xts_1_1_yearly_1 <- reactive({to.yearly(ab_ind_dat_9_sub_1_xts_2_1())})
  ab_ind_dat_9_sub_1_xts_1_1_yearly_2 <- reactive({to.yearly(ab_ind_dat_9_sub_1_xts_2_2())})
  ab_ind_dat_9_sub_1_xts_1_1_yearly_3 <- reactive({to.yearly(ab_ind_dat_9_sub_1_xts_2_3())})
  ab_ind_dat_9_sub_1_xts_1_1_monthly_1 <- reactive({to.monthly(ab_ind_dat_9_sub_1_xts_1_1())})
  ab_ind_dat_9_sub_1_xts_1_1_monthly_2 <- reactive({to.monthly(ab_ind_dat_9_sub_1_xts_1_2())})
  ab_ind_dat_9_sub_1_xts_1_1_monthly_3 <- reactive({to.monthly(ab_ind_dat_9_sub_1_xts_1_3())})
  ab_ind_dat_9_sub_1_xts_1_1_weekly_1 <- reactive({to.weekly(ab_ind_dat_9_sub_1_xts_1_1())})
  ab_ind_dat_9_sub_1_xts_1_1_weekly_2 <- reactive({to.weekly(ab_ind_dat_9_sub_1_xts_1_2())})
  ab_ind_dat_9_sub_1_xts_1_1_weekly_3 <- reactive({to.weekly(ab_ind_dat_9_sub_1_xts_1_3())})
  ab_ind_dat_9_sub_1_xts_1_1_daily_1 <- reactive({to.daily(ab_ind_dat_9_sub_1_xts_1_1())})
  ab_ind_dat_9_sub_1_xts_1_1_daily_2 <- reactive({to.daily(ab_ind_dat_9_sub_1_xts_1_2())})
  ab_ind_dat_9_sub_1_xts_1_1_daily_3 <- reactive({to.daily(ab_ind_dat_9_sub_1_xts_1_3())})
  ab_ind_dat_9_sub_1_xts_1_1_hourly_1 <- reactive({to.hourly(ab_ind_dat_9_sub_1_xts_1_1())})
  ab_ind_dat_9_sub_1_xts_1_1_hourly_2 <- reactive({to.hourly(ab_ind_dat_9_sub_1_xts_1_2())})
  ab_ind_dat_9_sub_1_xts_1_1_hourly_3 <- reactive({to.hourly(ab_ind_dat_9_sub_1_xts_1_3())})
  
  output$AB_ENE_9_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_yearly_1()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_yearly_1()) %in% c('ab_ind_dat_9_sub_1_xts_2_1().High'))], type = "line", name = "High MC", color = "green") %>%
      hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_yearly_1()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_yearly_1()) %in% c('ab_ind_dat_9_sub_1_xts_2_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
      hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_yearly_2()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_yearly_2()) %in% c('ab_ind_dat_9_sub_1_xts_2_2().High'))], type = "line", name = "High TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_yearly_2()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_yearly_2()) %in% c('ab_ind_dat_9_sub_1_xts_2_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
      hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_yearly_3()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_yearly_3()) %in% c('ab_ind_dat_9_sub_1_xts_2_3().High'))], type = "line", name = "High DCR", color = "green") %>%
      hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_yearly_3()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_yearly_3()) %in% c('ab_ind_dat_9_sub_1_xts_2_3().Low'))], type = "line", name = "Low DCR ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$AB_ENE_9_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_weekly_1()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_weekly_1()) %in% c('ab_ind_dat_9_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
      hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_weekly_1()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_weekly_1()) %in% c('ab_ind_dat_9_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
      hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_weekly_2()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_weekly_2()) %in% c('ab_ind_dat_9_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_weekly_2()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_weekly_2()) %in% c('ab_ind_dat_9_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
      hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_weekly_3()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_weekly_3()) %in% c('ab_ind_dat_9_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
      hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_weekly_3()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_weekly_3()) %in% c('ab_ind_dat_9_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$AB_ENE_9_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_daily_1()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_daily_1()) %in% c('ab_ind_dat_9_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
      hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_daily_1()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_daily_1()) %in% c('ab_ind_dat_9_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
      hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_daily_2()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_daily_2()) %in% c('ab_ind_dat_9_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_daily_2()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_daily_2()) %in% c('ab_ind_dat_9_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
      hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_daily_3()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_daily_3()) %in% c('ab_ind_dat_9_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
      hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_daily_3()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_daily_3()) %in% c('ab_ind_dat_9_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$AB_ENE_9_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_hourly_1()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_hourly_1()) %in% c('ab_ind_dat_9_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
      hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_hourly_1()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_hourly_1()) %in% c('ab_ind_dat_9_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
      hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_hourly_2()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_hourly_2()) %in% c('ab_ind_dat_9_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_hourly_2()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_hourly_2()) %in% c('ab_ind_dat_9_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
      hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_hourly_3()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_hourly_3()) %in% c('ab_ind_dat_9_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
      hc_add_series(ab_ind_dat_9_sub_1_xts_1_1_hourly_3()[,(colnames(ab_ind_dat_9_sub_1_xts_1_1_hourly_3()) %in% c('ab_ind_dat_9_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$AB_ENE_9_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(ab_ind_dat_9_sub_1_xts_1_1(), type = "line", name = "MC", color = "green") %>%
      hc_add_series(ab_ind_dat_9_sub_1_xts_1_2(), type = "line", name = "TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_9_sub_1_xts_1_3(), type = "line", name = "DCR", color = "green")
  })
  
  ab_date_ind_10_1 <- reactive({paste(input$ab_dates_10[1],"00:00:00",sep = " ")})
  ab_date_ind_10_2 <- reactive({paste(input$ab_dates_10[2],"00:00:00",sep = " ")})
  ab_ind_dat_10_asset_filter <- reactive({input$ab_ind_10_ff})
  
  ab_ind_dat_10_sub_1 <- reactive({subset(ab_ind_dat_10,subset = (ab_ind_dat_10$Update_Time >= ab_date_ind_10_1() & ab_ind_dat_10$Update_Time <= ab_date_ind_10_2()))})
  ab_ind_dat_10_sub_2 <- reactive({subset(ab_ind_dat_10_sub_1(),subset = (ab_ind_dat_10_sub_1()$ASSET == ab_ind_dat_10_asset_filter()))})
  
  ab_ind_dat_10_sub_1_xts_1_1 <- reactive({xts(ab_ind_dat_10_sub_2()$MC,ab_ind_dat_10_sub_2()$Update_Time)})
  ab_ind_dat_10_sub_1_xts_1_2 <- reactive({xts(ab_ind_dat_10_sub_2()$TNG,ab_ind_dat_10_sub_2()$Update_Time)})
  ab_ind_dat_10_sub_1_xts_1_3 <- reactive({xts(ab_ind_dat_10_sub_2()$DCR,ab_ind_dat_10_sub_2()$Update_Time)})
  
  ab_ind_dat_10_sub_1_xts_2_1 <- reactive({xts(ab_ind_dat_10_sub_1()$MC,ab_ind_dat_10_sub_1()$Update_Time)})
  ab_ind_dat_10_sub_1_xts_2_2 <- reactive({xts(ab_ind_dat_10_sub_1()$TNG,ab_ind_dat_10_sub_1()$Update_Time)})
  ab_ind_dat_10_sub_1_xts_2_3 <- reactive({xts(ab_ind_dat_10_sub_1()$DCR,ab_ind_dat_10_sub_1()$Update_Time)})
  
  ab_ind_dat_10_sub_1_xts_1_1_yearly_1 <- reactive({to.yearly(ab_ind_dat_10_sub_1_xts_2_1())})
  ab_ind_dat_10_sub_1_xts_1_1_yearly_2 <- reactive({to.yearly(ab_ind_dat_10_sub_1_xts_2_2())})
  ab_ind_dat_10_sub_1_xts_1_1_yearly_3 <- reactive({to.yearly(ab_ind_dat_10_sub_1_xts_2_3())})
  ab_ind_dat_10_sub_1_xts_1_1_monthly_1 <- reactive({to.monthly(ab_ind_dat_10_sub_1_xts_1_1())})
  ab_ind_dat_10_sub_1_xts_1_1_monthly_2 <- reactive({to.monthly(ab_ind_dat_10_sub_1_xts_1_2())})
  ab_ind_dat_10_sub_1_xts_1_1_monthly_3 <- reactive({to.monthly(ab_ind_dat_10_sub_1_xts_1_3())})
  ab_ind_dat_10_sub_1_xts_1_1_weekly_1 <- reactive({to.weekly(ab_ind_dat_10_sub_1_xts_1_1())})
  ab_ind_dat_10_sub_1_xts_1_1_weekly_2 <- reactive({to.weekly(ab_ind_dat_10_sub_1_xts_1_2())})
  ab_ind_dat_10_sub_1_xts_1_1_weekly_3 <- reactive({to.weekly(ab_ind_dat_10_sub_1_xts_1_3())})
  ab_ind_dat_10_sub_1_xts_1_1_daily_1 <- reactive({to.daily(ab_ind_dat_10_sub_1_xts_1_1())})
  ab_ind_dat_10_sub_1_xts_1_1_daily_2 <- reactive({to.daily(ab_ind_dat_10_sub_1_xts_1_2())})
  ab_ind_dat_10_sub_1_xts_1_1_daily_3 <- reactive({to.daily(ab_ind_dat_10_sub_1_xts_1_3())})
  ab_ind_dat_10_sub_1_xts_1_1_hourly_1 <- reactive({to.hourly(ab_ind_dat_10_sub_1_xts_1_1())})
  ab_ind_dat_10_sub_1_xts_1_1_hourly_2 <- reactive({to.hourly(ab_ind_dat_10_sub_1_xts_1_2())})
  ab_ind_dat_10_sub_1_xts_1_1_hourly_3 <- reactive({to.hourly(ab_ind_dat_10_sub_1_xts_1_3())})
  
  output$AB_ENE_10_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_yearly_1()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_yearly_1()) %in% c('ab_ind_dat_10_sub_1_xts_2_1().High'))], type = "line", name = "High MC", color = "green") %>%
      hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_yearly_1()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_yearly_1()) %in% c('ab_ind_dat_10_sub_1_xts_2_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
      hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_yearly_2()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_yearly_2()) %in% c('ab_ind_dat_10_sub_1_xts_2_2().High'))], type = "line", name = "High TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_yearly_2()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_yearly_2()) %in% c('ab_ind_dat_10_sub_1_xts_2_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
      hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_yearly_3()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_yearly_3()) %in% c('ab_ind_dat_10_sub_1_xts_2_3().High'))], type = "line", name = "High DCR", color = "green") %>%
      hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_yearly_3()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_yearly_3()) %in% c('ab_ind_dat_10_sub_1_xts_2_3().Low'))], type = "line", name = "Low DCR ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$AB_ENE_10_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_weekly_1()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_weekly_1()) %in% c('ab_ind_dat_10_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
      hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_weekly_1()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_weekly_1()) %in% c('ab_ind_dat_10_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
      hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_weekly_2()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_weekly_2()) %in% c('ab_ind_dat_10_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_weekly_2()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_weekly_2()) %in% c('ab_ind_dat_10_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
      hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_weekly_3()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_weekly_3()) %in% c('ab_ind_dat_10_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
      hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_weekly_3()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_weekly_3()) %in% c('ab_ind_dat_10_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$AB_ENE_10_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_daily_1()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_daily_1()) %in% c('ab_ind_dat_10_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
      hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_daily_1()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_daily_1()) %in% c('ab_ind_dat_10_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
      hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_daily_2()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_daily_2()) %in% c('ab_ind_dat_10_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_daily_2()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_daily_2()) %in% c('ab_ind_dat_10_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
      hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_daily_3()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_daily_3()) %in% c('ab_ind_dat_10_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
      hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_daily_3()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_daily_3()) %in% c('ab_ind_dat_10_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$AB_ENE_10_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_hourly_1()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_hourly_1()) %in% c('ab_ind_dat_10_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
      hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_hourly_1()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_hourly_1()) %in% c('ab_ind_dat_10_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
      hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_hourly_2()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_hourly_2()) %in% c('ab_ind_dat_10_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_hourly_2()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_hourly_2()) %in% c('ab_ind_dat_10_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
      hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_hourly_3()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_hourly_3()) %in% c('ab_ind_dat_10_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
      hc_add_series(ab_ind_dat_10_sub_1_xts_1_1_hourly_3()[,(colnames(ab_ind_dat_10_sub_1_xts_1_1_hourly_3()) %in% c('ab_ind_dat_10_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$AB_ENE_10_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(ab_ind_dat_10_sub_1_xts_1_1(), type = "line", name = "MC", color = "green") %>%
      hc_add_series(ab_ind_dat_10_sub_1_xts_1_2(), type = "line", name = "TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_10_sub_1_xts_1_3(), type = "line", name = "DCR", color = "green")
  })
  
  ab_date_ind_11_1 <- reactive({paste(input$ab_dates_11[1],"00:00:00",sep = " ")})
  ab_date_ind_11_2 <- reactive({paste(input$ab_dates_11[2],"00:00:00",sep = " ")})
  ab_ind_dat_11_asset_filter <- reactive({input$ab_ind_11_ff})
  
  ab_ind_dat_11_sub_1 <- reactive({subset(ab_ind_dat_11,subset = (ab_ind_dat_11$Update_Time >= ab_date_ind_11_1() & ab_ind_dat_11$Update_Time <= ab_date_ind_11_2()))})
  ab_ind_dat_11_sub_2 <- reactive({subset(ab_ind_dat_11_sub_1(),subset = (ab_ind_dat_11_sub_1()$ASSET == ab_ind_dat_11_asset_filter()))})
  
  ab_ind_dat_11_sub_1_xts_1_1 <- reactive({xts(ab_ind_dat_11_sub_2()$MC,ab_ind_dat_11_sub_2()$Update_Time)})
  ab_ind_dat_11_sub_1_xts_1_2 <- reactive({xts(ab_ind_dat_11_sub_2()$TNG,ab_ind_dat_11_sub_2()$Update_Time)})
  ab_ind_dat_11_sub_1_xts_1_3 <- reactive({xts(ab_ind_dat_11_sub_2()$DCR,ab_ind_dat_11_sub_2()$Update_Time)})
  
  ab_ind_dat_11_sub_1_xts_2_1 <- reactive({xts(ab_ind_dat_11_sub_1()$MC,ab_ind_dat_11_sub_1()$Update_Time)})
  ab_ind_dat_11_sub_1_xts_2_2 <- reactive({xts(ab_ind_dat_11_sub_1()$TNG,ab_ind_dat_11_sub_1()$Update_Time)})
  ab_ind_dat_11_sub_1_xts_2_3 <- reactive({xts(ab_ind_dat_11_sub_1()$DCR,ab_ind_dat_11_sub_1()$Update_Time)})
  
  ab_ind_dat_11_sub_1_xts_1_1_yearly_1 <- reactive({to.yearly(ab_ind_dat_11_sub_1_xts_2_1())})
  ab_ind_dat_11_sub_1_xts_1_1_yearly_2 <- reactive({to.yearly(ab_ind_dat_11_sub_1_xts_2_2())})
  ab_ind_dat_11_sub_1_xts_1_1_yearly_3 <- reactive({to.yearly(ab_ind_dat_11_sub_1_xts_2_3())})
  ab_ind_dat_11_sub_1_xts_1_1_monthly_1 <- reactive({to.monthly(ab_ind_dat_11_sub_1_xts_1_1())})
  ab_ind_dat_11_sub_1_xts_1_1_monthly_2 <- reactive({to.monthly(ab_ind_dat_11_sub_1_xts_1_2())})
  ab_ind_dat_11_sub_1_xts_1_1_monthly_3 <- reactive({to.monthly(ab_ind_dat_11_sub_1_xts_1_3())})
  ab_ind_dat_11_sub_1_xts_1_1_weekly_1 <- reactive({to.weekly(ab_ind_dat_11_sub_1_xts_1_1())})
  ab_ind_dat_11_sub_1_xts_1_1_weekly_2 <- reactive({to.weekly(ab_ind_dat_11_sub_1_xts_1_2())})
  ab_ind_dat_11_sub_1_xts_1_1_weekly_3 <- reactive({to.weekly(ab_ind_dat_11_sub_1_xts_1_3())})
  ab_ind_dat_11_sub_1_xts_1_1_daily_1 <- reactive({to.daily(ab_ind_dat_11_sub_1_xts_1_1())})
  ab_ind_dat_11_sub_1_xts_1_1_daily_2 <- reactive({to.daily(ab_ind_dat_11_sub_1_xts_1_2())})
  ab_ind_dat_11_sub_1_xts_1_1_daily_3 <- reactive({to.daily(ab_ind_dat_11_sub_1_xts_1_3())})
  ab_ind_dat_11_sub_1_xts_1_1_hourly_1 <- reactive({to.hourly(ab_ind_dat_11_sub_1_xts_1_1())})
  ab_ind_dat_11_sub_1_xts_1_1_hourly_2 <- reactive({to.hourly(ab_ind_dat_11_sub_1_xts_1_2())})
  ab_ind_dat_11_sub_1_xts_1_1_hourly_3 <- reactive({to.hourly(ab_ind_dat_11_sub_1_xts_1_3())})
  
  output$AB_ENE_11_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_yearly_1()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_yearly_1()) %in% c('ab_ind_dat_11_sub_1_xts_2_1().High'))], type = "line", name = "High MC", color = "green") %>%
      hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_yearly_1()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_yearly_1()) %in% c('ab_ind_dat_11_sub_1_xts_2_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
      hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_yearly_2()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_yearly_2()) %in% c('ab_ind_dat_11_sub_1_xts_2_2().High'))], type = "line", name = "High TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_yearly_2()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_yearly_2()) %in% c('ab_ind_dat_11_sub_1_xts_2_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
      hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_yearly_3()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_yearly_3()) %in% c('ab_ind_dat_11_sub_1_xts_2_3().High'))], type = "line", name = "High DCR", color = "green") %>%
      hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_yearly_3()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_yearly_3()) %in% c('ab_ind_dat_11_sub_1_xts_2_3().Low'))], type = "line", name = "Low DCR ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$AB_ENE_11_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_weekly_1()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_weekly_1()) %in% c('ab_ind_dat_11_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
      hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_weekly_1()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_weekly_1()) %in% c('ab_ind_dat_11_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
      hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_weekly_2()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_weekly_2()) %in% c('ab_ind_dat_11_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_weekly_2()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_weekly_2()) %in% c('ab_ind_dat_11_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
      hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_weekly_3()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_weekly_3()) %in% c('ab_ind_dat_11_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
      hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_weekly_3()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_weekly_3()) %in% c('ab_ind_dat_11_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$AB_ENE_11_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_daily_1()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_daily_1()) %in% c('ab_ind_dat_11_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
      hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_daily_1()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_daily_1()) %in% c('ab_ind_dat_11_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
      hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_daily_2()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_daily_2()) %in% c('ab_ind_dat_11_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_daily_2()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_daily_2()) %in% c('ab_ind_dat_11_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
      hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_daily_3()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_daily_3()) %in% c('ab_ind_dat_11_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
      hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_daily_3()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_daily_3()) %in% c('ab_ind_dat_11_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$AB_ENE_11_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_hourly_1()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_hourly_1()) %in% c('ab_ind_dat_11_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
      hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_hourly_1()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_hourly_1()) %in% c('ab_ind_dat_11_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
      hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_hourly_2()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_hourly_2()) %in% c('ab_ind_dat_11_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_hourly_2()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_hourly_2()) %in% c('ab_ind_dat_11_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
      hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_hourly_3()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_hourly_3()) %in% c('ab_ind_dat_11_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
      hc_add_series(ab_ind_dat_11_sub_1_xts_1_1_hourly_3()[,(colnames(ab_ind_dat_11_sub_1_xts_1_1_hourly_3()) %in% c('ab_ind_dat_11_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$AB_ENE_11_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(ab_ind_dat_11_sub_1_xts_1_1(), type = "line", name = "MC", color = "green") %>%
      hc_add_series(ab_ind_dat_11_sub_1_xts_1_2(), type = "line", name = "TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_11_sub_1_xts_1_3(), type = "line", name = "DCR", color = "green")
  })
  
  ab_date_ind_12_1 <- reactive({paste(input$ab_dates_12[1],"00:00:00",sep = " ")})
  ab_date_ind_12_2 <- reactive({paste(input$ab_dates_12[2],"00:00:00",sep = " ")})
  ab_ind_dat_12_asset_filter <- reactive({input$ab_ind_12_ff})
  
  ab_ind_dat_12_sub_1 <- reactive({subset(ab_ind_dat_12,subset = (ab_ind_dat_12$Update_Time >= ab_date_ind_12_1() & ab_ind_dat_12$Update_Time <= ab_date_ind_12_2()))})
  ab_ind_dat_12_sub_2 <- reactive({subset(ab_ind_dat_12_sub_1(),subset = (ab_ind_dat_12_sub_1()$ASSET == ab_ind_dat_12_asset_filter()))})
  
  ab_ind_dat_12_sub_1_xts_1_1 <- reactive({xts(ab_ind_dat_12_sub_2()$MC,ab_ind_dat_12_sub_2()$Update_Time)})
  ab_ind_dat_12_sub_1_xts_1_2 <- reactive({xts(ab_ind_dat_12_sub_2()$TNG,ab_ind_dat_12_sub_2()$Update_Time)})
  ab_ind_dat_12_sub_1_xts_1_3 <- reactive({xts(ab_ind_dat_12_sub_2()$DCR,ab_ind_dat_12_sub_2()$Update_Time)})
  
  ab_ind_dat_12_sub_1_xts_2_1 <- reactive({xts(ab_ind_dat_12_sub_1()$MC,ab_ind_dat_12_sub_1()$Update_Time)})
  ab_ind_dat_12_sub_1_xts_2_2 <- reactive({xts(ab_ind_dat_12_sub_1()$TNG,ab_ind_dat_12_sub_1()$Update_Time)})
  ab_ind_dat_12_sub_1_xts_2_3 <- reactive({xts(ab_ind_dat_12_sub_1()$DCR,ab_ind_dat_12_sub_1()$Update_Time)})
  
  ab_ind_dat_12_sub_1_xts_1_1_yearly_1 <- reactive({to.yearly(ab_ind_dat_12_sub_1_xts_2_1())})
  ab_ind_dat_12_sub_1_xts_1_1_yearly_2 <- reactive({to.yearly(ab_ind_dat_12_sub_1_xts_2_2())})
  ab_ind_dat_12_sub_1_xts_1_1_yearly_3 <- reactive({to.yearly(ab_ind_dat_12_sub_1_xts_2_3())})
  ab_ind_dat_12_sub_1_xts_1_1_monthly_1 <- reactive({to.monthly(ab_ind_dat_12_sub_1_xts_1_1())})
  ab_ind_dat_12_sub_1_xts_1_1_monthly_2 <- reactive({to.monthly(ab_ind_dat_12_sub_1_xts_1_2())})
  ab_ind_dat_12_sub_1_xts_1_1_monthly_3 <- reactive({to.monthly(ab_ind_dat_12_sub_1_xts_1_3())})
  ab_ind_dat_12_sub_1_xts_1_1_weekly_1 <- reactive({to.weekly(ab_ind_dat_12_sub_1_xts_1_1())})
  ab_ind_dat_12_sub_1_xts_1_1_weekly_2 <- reactive({to.weekly(ab_ind_dat_12_sub_1_xts_1_2())})
  ab_ind_dat_12_sub_1_xts_1_1_weekly_3 <- reactive({to.weekly(ab_ind_dat_12_sub_1_xts_1_3())})
  ab_ind_dat_12_sub_1_xts_1_1_daily_1 <- reactive({to.daily(ab_ind_dat_12_sub_1_xts_1_1())})
  ab_ind_dat_12_sub_1_xts_1_1_daily_2 <- reactive({to.daily(ab_ind_dat_12_sub_1_xts_1_2())})
  ab_ind_dat_12_sub_1_xts_1_1_daily_3 <- reactive({to.daily(ab_ind_dat_12_sub_1_xts_1_3())})
  ab_ind_dat_12_sub_1_xts_1_1_hourly_1 <- reactive({to.hourly(ab_ind_dat_12_sub_1_xts_1_1())})
  ab_ind_dat_12_sub_1_xts_1_1_hourly_2 <- reactive({to.hourly(ab_ind_dat_12_sub_1_xts_1_2())})
  ab_ind_dat_12_sub_1_xts_1_1_hourly_3 <- reactive({to.hourly(ab_ind_dat_12_sub_1_xts_1_3())})
  
  output$AB_ENE_12_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_yearly_1()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_yearly_1()) %in% c('ab_ind_dat_12_sub_1_xts_2_1().High'))], type = "line", name = "High MC", color = "green") %>%
      hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_yearly_1()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_yearly_1()) %in% c('ab_ind_dat_12_sub_1_xts_2_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
      hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_yearly_2()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_yearly_2()) %in% c('ab_ind_dat_12_sub_1_xts_2_2().High'))], type = "line", name = "High TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_yearly_2()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_yearly_2()) %in% c('ab_ind_dat_12_sub_1_xts_2_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
      hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_yearly_3()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_yearly_3()) %in% c('ab_ind_dat_12_sub_1_xts_2_3().High'))], type = "line", name = "High DCR", color = "green") %>%
      hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_yearly_3()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_yearly_3()) %in% c('ab_ind_dat_12_sub_1_xts_2_3().Low'))], type = "line", name = "Low DCR ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$AB_ENE_12_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_weekly_1()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_weekly_1()) %in% c('ab_ind_dat_12_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
      hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_weekly_1()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_weekly_1()) %in% c('ab_ind_dat_12_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
      hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_weekly_2()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_weekly_2()) %in% c('ab_ind_dat_12_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_weekly_2()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_weekly_2()) %in% c('ab_ind_dat_12_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
      hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_weekly_3()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_weekly_3()) %in% c('ab_ind_dat_12_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
      hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_weekly_3()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_weekly_3()) %in% c('ab_ind_dat_12_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$AB_ENE_12_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_daily_1()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_daily_1()) %in% c('ab_ind_dat_12_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
      hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_daily_1()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_daily_1()) %in% c('ab_ind_dat_12_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
      hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_daily_2()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_daily_2()) %in% c('ab_ind_dat_12_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_daily_2()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_daily_2()) %in% c('ab_ind_dat_12_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
      hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_daily_3()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_daily_3()) %in% c('ab_ind_dat_12_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
      hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_daily_3()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_daily_3()) %in% c('ab_ind_dat_12_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$AB_ENE_12_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_hourly_1()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_hourly_1()) %in% c('ab_ind_dat_12_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
      hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_hourly_1()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_hourly_1()) %in% c('ab_ind_dat_12_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
      hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_hourly_2()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_hourly_2()) %in% c('ab_ind_dat_12_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_hourly_2()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_hourly_2()) %in% c('ab_ind_dat_12_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
      hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_hourly_3()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_hourly_3()) %in% c('ab_ind_dat_12_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
      hc_add_series(ab_ind_dat_12_sub_1_xts_1_1_hourly_3()[,(colnames(ab_ind_dat_12_sub_1_xts_1_1_hourly_3()) %in% c('ab_ind_dat_12_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$AB_ENE_12_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(ab_ind_dat_12_sub_1_xts_1_1(), type = "line", name = "MC", color = "green") %>%
      hc_add_series(ab_ind_dat_12_sub_1_xts_1_2(), type = "line", name = "TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_12_sub_1_xts_1_3(), type = "line", name = "DCR", color = "green")
  })
  
  ab_date_ind_13_1 <- reactive({paste(input$ab_dates_13[1],"00:00:00",sep = " ")})
  ab_date_ind_13_2 <- reactive({paste(input$ab_dates_13[2],"00:00:00",sep = " ")})
  ab_ind_dat_13_asset_filter <- reactive({input$ab_ind_13_ff})
  
  ab_ind_dat_13_sub_1 <- reactive({subset(ab_ind_dat_13,subset = (ab_ind_dat_13$Update_Time >= ab_date_ind_13_1() & ab_ind_dat_13$Update_Time <= ab_date_ind_13_2()))})
  ab_ind_dat_13_sub_2 <- reactive({subset(ab_ind_dat_13_sub_1(),subset = (ab_ind_dat_13_sub_1()$ASSET == ab_ind_dat_13_asset_filter()))})
  
  ab_ind_dat_13_sub_1_xts_1_1 <- reactive({xts(ab_ind_dat_13_sub_2()$MC,ab_ind_dat_13_sub_2()$Update_Time)})
  ab_ind_dat_13_sub_1_xts_1_2 <- reactive({xts(ab_ind_dat_13_sub_2()$TNG,ab_ind_dat_13_sub_2()$Update_Time)})
  ab_ind_dat_13_sub_1_xts_1_3 <- reactive({xts(ab_ind_dat_13_sub_2()$DCR,ab_ind_dat_13_sub_2()$Update_Time)})
  
  ab_ind_dat_13_sub_1_xts_2_1 <- reactive({xts(ab_ind_dat_13_sub_1()$MC,ab_ind_dat_13_sub_1()$Update_Time)})
  ab_ind_dat_13_sub_1_xts_2_2 <- reactive({xts(ab_ind_dat_13_sub_1()$TNG,ab_ind_dat_13_sub_1()$Update_Time)})
  ab_ind_dat_13_sub_1_xts_2_3 <- reactive({xts(ab_ind_dat_13_sub_1()$DCR,ab_ind_dat_13_sub_1()$Update_Time)})
  
  ab_ind_dat_13_sub_1_xts_1_1_yearly_1 <- reactive({to.yearly(ab_ind_dat_13_sub_1_xts_2_1())})
  ab_ind_dat_13_sub_1_xts_1_1_yearly_2 <- reactive({to.yearly(ab_ind_dat_13_sub_1_xts_2_2())})
  ab_ind_dat_13_sub_1_xts_1_1_yearly_3 <- reactive({to.yearly(ab_ind_dat_13_sub_1_xts_2_3())})
  ab_ind_dat_13_sub_1_xts_1_1_monthly_1 <- reactive({to.monthly(ab_ind_dat_13_sub_1_xts_1_1())})
  ab_ind_dat_13_sub_1_xts_1_1_monthly_2 <- reactive({to.monthly(ab_ind_dat_13_sub_1_xts_1_2())})
  ab_ind_dat_13_sub_1_xts_1_1_monthly_3 <- reactive({to.monthly(ab_ind_dat_13_sub_1_xts_1_3())})
  ab_ind_dat_13_sub_1_xts_1_1_weekly_1 <- reactive({to.weekly(ab_ind_dat_13_sub_1_xts_1_1())})
  ab_ind_dat_13_sub_1_xts_1_1_weekly_2 <- reactive({to.weekly(ab_ind_dat_13_sub_1_xts_1_2())})
  ab_ind_dat_13_sub_1_xts_1_1_weekly_3 <- reactive({to.weekly(ab_ind_dat_13_sub_1_xts_1_3())})
  ab_ind_dat_13_sub_1_xts_1_1_daily_1 <- reactive({to.daily(ab_ind_dat_13_sub_1_xts_1_1())})
  ab_ind_dat_13_sub_1_xts_1_1_daily_2 <- reactive({to.daily(ab_ind_dat_13_sub_1_xts_1_2())})
  ab_ind_dat_13_sub_1_xts_1_1_daily_3 <- reactive({to.daily(ab_ind_dat_13_sub_1_xts_1_3())})
  ab_ind_dat_13_sub_1_xts_1_1_hourly_1 <- reactive({to.hourly(ab_ind_dat_13_sub_1_xts_1_1())})
  ab_ind_dat_13_sub_1_xts_1_1_hourly_2 <- reactive({to.hourly(ab_ind_dat_13_sub_1_xts_1_2())})
  ab_ind_dat_13_sub_1_xts_1_1_hourly_3 <- reactive({to.hourly(ab_ind_dat_13_sub_1_xts_1_3())})
  
  output$AB_ENE_13_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_yearly_1()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_yearly_1()) %in% c('ab_ind_dat_13_sub_1_xts_2_1().High'))], type = "line", name = "High MC", color = "green") %>%
      hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_yearly_1()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_yearly_1()) %in% c('ab_ind_dat_13_sub_1_xts_2_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
      hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_yearly_2()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_yearly_2()) %in% c('ab_ind_dat_13_sub_1_xts_2_2().High'))], type = "line", name = "High TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_yearly_2()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_yearly_2()) %in% c('ab_ind_dat_13_sub_1_xts_2_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
      hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_yearly_3()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_yearly_3()) %in% c('ab_ind_dat_13_sub_1_xts_2_3().High'))], type = "line", name = "High DCR", color = "green") %>%
      hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_yearly_3()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_yearly_3()) %in% c('ab_ind_dat_13_sub_1_xts_2_3().Low'))], type = "line", name = "Low DCR ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$AB_ENE_13_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_weekly_1()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_weekly_1()) %in% c('ab_ind_dat_13_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
      hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_weekly_1()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_weekly_1()) %in% c('ab_ind_dat_13_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
      hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_weekly_2()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_weekly_2()) %in% c('ab_ind_dat_13_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_weekly_2()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_weekly_2()) %in% c('ab_ind_dat_13_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
      hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_weekly_3()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_weekly_3()) %in% c('ab_ind_dat_13_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
      hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_weekly_3()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_weekly_3()) %in% c('ab_ind_dat_13_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$AB_ENE_13_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_daily_1()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_daily_1()) %in% c('ab_ind_dat_13_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
      hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_daily_1()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_daily_1()) %in% c('ab_ind_dat_13_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
      hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_daily_2()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_daily_2()) %in% c('ab_ind_dat_13_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_daily_2()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_daily_2()) %in% c('ab_ind_dat_13_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
      hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_daily_3()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_daily_3()) %in% c('ab_ind_dat_13_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
      hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_daily_3()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_daily_3()) %in% c('ab_ind_dat_13_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$AB_ENE_13_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_hourly_1()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_hourly_1()) %in% c('ab_ind_dat_13_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
      hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_hourly_1()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_hourly_1()) %in% c('ab_ind_dat_13_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
      hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_hourly_2()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_hourly_2()) %in% c('ab_ind_dat_13_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_hourly_2()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_hourly_2()) %in% c('ab_ind_dat_13_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
      hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_hourly_3()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_hourly_3()) %in% c('ab_ind_dat_13_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
      hc_add_series(ab_ind_dat_13_sub_1_xts_1_1_hourly_3()[,(colnames(ab_ind_dat_13_sub_1_xts_1_1_hourly_3()) %in% c('ab_ind_dat_13_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$AB_ENE_13_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(ab_ind_dat_13_sub_1_xts_1_1(), type = "line", name = "MC", color = "green") %>%
      hc_add_series(ab_ind_dat_13_sub_1_xts_1_2(), type = "line", name = "TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_13_sub_1_xts_1_3(), type = "line", name = "DCR", color = "green")
  })
  
  ab_date_ind_14_1 <- reactive({paste(input$ab_dates_14[1],"00:00:00",sep = " ")})
  ab_date_ind_14_2 <- reactive({paste(input$ab_dates_14[2],"00:00:00",sep = " ")})
  ab_ind_dat_14_asset_filter <- reactive({input$ab_ind_14_ff})
  
  ab_ind_dat_14_sub_1 <- reactive({subset(ab_ind_dat_14,subset = (ab_ind_dat_14$Update_Time >= ab_date_ind_14_1() & ab_ind_dat_14$Update_Time <= ab_date_ind_14_2()))})
  ab_ind_dat_14_sub_2 <- reactive({subset(ab_ind_dat_14_sub_1(),subset = (ab_ind_dat_14_sub_1()$ASSET == ab_ind_dat_14_asset_filter()))})
  
  ab_ind_dat_14_sub_1_xts_1_1 <- reactive({xts(ab_ind_dat_14_sub_2()$MC,ab_ind_dat_14_sub_2()$Update_Time)})
  ab_ind_dat_14_sub_1_xts_1_2 <- reactive({xts(ab_ind_dat_14_sub_2()$TNG,ab_ind_dat_14_sub_2()$Update_Time)})
  ab_ind_dat_14_sub_1_xts_1_3 <- reactive({xts(ab_ind_dat_14_sub_2()$DCR,ab_ind_dat_14_sub_2()$Update_Time)})
  
  ab_ind_dat_14_sub_1_xts_2_1 <- reactive({xts(ab_ind_dat_14_sub_1()$MC,ab_ind_dat_14_sub_1()$Update_Time)})
  ab_ind_dat_14_sub_1_xts_2_2 <- reactive({xts(ab_ind_dat_14_sub_1()$TNG,ab_ind_dat_14_sub_1()$Update_Time)})
  ab_ind_dat_14_sub_1_xts_2_3 <- reactive({xts(ab_ind_dat_14_sub_1()$DCR,ab_ind_dat_14_sub_1()$Update_Time)})
  
  ab_ind_dat_14_sub_1_xts_1_1_yearly_1 <- reactive({to.yearly(ab_ind_dat_14_sub_1_xts_2_1())})
  ab_ind_dat_14_sub_1_xts_1_1_yearly_2 <- reactive({to.yearly(ab_ind_dat_14_sub_1_xts_2_2())})
  ab_ind_dat_14_sub_1_xts_1_1_yearly_3 <- reactive({to.yearly(ab_ind_dat_14_sub_1_xts_2_3())})
  ab_ind_dat_14_sub_1_xts_1_1_monthly_1 <- reactive({to.monthly(ab_ind_dat_14_sub_1_xts_1_1())})
  ab_ind_dat_14_sub_1_xts_1_1_monthly_2 <- reactive({to.monthly(ab_ind_dat_14_sub_1_xts_1_2())})
  ab_ind_dat_14_sub_1_xts_1_1_monthly_3 <- reactive({to.monthly(ab_ind_dat_14_sub_1_xts_1_3())})
  ab_ind_dat_14_sub_1_xts_1_1_weekly_1 <- reactive({to.weekly(ab_ind_dat_14_sub_1_xts_1_1())})
  ab_ind_dat_14_sub_1_xts_1_1_weekly_2 <- reactive({to.weekly(ab_ind_dat_14_sub_1_xts_1_2())})
  ab_ind_dat_14_sub_1_xts_1_1_weekly_3 <- reactive({to.weekly(ab_ind_dat_14_sub_1_xts_1_3())})
  ab_ind_dat_14_sub_1_xts_1_1_daily_1 <- reactive({to.daily(ab_ind_dat_14_sub_1_xts_1_1())})
  ab_ind_dat_14_sub_1_xts_1_1_daily_2 <- reactive({to.daily(ab_ind_dat_14_sub_1_xts_1_2())})
  ab_ind_dat_14_sub_1_xts_1_1_daily_3 <- reactive({to.daily(ab_ind_dat_14_sub_1_xts_1_3())})
  ab_ind_dat_14_sub_1_xts_1_1_hourly_1 <- reactive({to.hourly(ab_ind_dat_14_sub_1_xts_1_1())})
  ab_ind_dat_14_sub_1_xts_1_1_hourly_2 <- reactive({to.hourly(ab_ind_dat_14_sub_1_xts_1_2())})
  ab_ind_dat_14_sub_1_xts_1_1_hourly_3 <- reactive({to.hourly(ab_ind_dat_14_sub_1_xts_1_3())})
  
  output$AB_ENE_14_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_yearly_1()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_yearly_1()) %in% c('ab_ind_dat_14_sub_1_xts_2_1().High'))], type = "line", name = "High MC", color = "green") %>%
      hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_yearly_1()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_yearly_1()) %in% c('ab_ind_dat_14_sub_1_xts_2_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
      hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_yearly_2()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_yearly_2()) %in% c('ab_ind_dat_14_sub_1_xts_2_2().High'))], type = "line", name = "High TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_yearly_2()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_yearly_2()) %in% c('ab_ind_dat_14_sub_1_xts_2_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
      hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_yearly_3()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_yearly_3()) %in% c('ab_ind_dat_14_sub_1_xts_2_3().High'))], type = "line", name = "High DCR", color = "green") %>%
      hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_yearly_3()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_yearly_3()) %in% c('ab_ind_dat_14_sub_1_xts_2_3().Low'))], type = "line", name = "Low DCR ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$AB_ENE_14_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_weekly_1()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_weekly_1()) %in% c('ab_ind_dat_14_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
      hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_weekly_1()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_weekly_1()) %in% c('ab_ind_dat_14_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
      hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_weekly_2()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_weekly_2()) %in% c('ab_ind_dat_14_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_weekly_2()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_weekly_2()) %in% c('ab_ind_dat_14_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
      hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_weekly_3()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_weekly_3()) %in% c('ab_ind_dat_14_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
      hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_weekly_3()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_weekly_3()) %in% c('ab_ind_dat_14_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$AB_ENE_14_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_daily_1()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_daily_1()) %in% c('ab_ind_dat_14_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
      hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_daily_1()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_daily_1()) %in% c('ab_ind_dat_14_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
      hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_daily_2()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_daily_2()) %in% c('ab_ind_dat_14_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_daily_2()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_daily_2()) %in% c('ab_ind_dat_14_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
      hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_daily_3()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_daily_3()) %in% c('ab_ind_dat_14_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
      hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_daily_3()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_daily_3()) %in% c('ab_ind_dat_14_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$AB_ENE_14_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_hourly_1()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_hourly_1()) %in% c('ab_ind_dat_14_sub_1_xts_1_1().High'))], type = "line", name = "High MC", color = "green") %>%
      hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_hourly_1()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_hourly_1()) %in% c('ab_ind_dat_14_sub_1_xts_1_1().Low'))], type = "line", name = "Low MC", color = "red") %>%
      hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_hourly_2()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_hourly_2()) %in% c('ab_ind_dat_14_sub_1_xts_1_2().High'))], type = "line", name = "High TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_hourly_2()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_hourly_2()) %in% c('ab_ind_dat_14_sub_1_xts_1_2().Low'))], type = "line", name = "Low TNG", color = "red") %>%
      hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_hourly_3()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_hourly_3()) %in% c('ab_ind_dat_14_sub_1_xts_1_3().High'))], type = "line", name = "High DCR", color = "green") %>%
      hc_add_series(ab_ind_dat_14_sub_1_xts_1_1_hourly_3()[,(colnames(ab_ind_dat_14_sub_1_xts_1_1_hourly_3()) %in% c('ab_ind_dat_14_sub_1_xts_1_3().Low'))], type = "line", name = "Low DCR", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$AB_ENE_14_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(ab_ind_dat_14_sub_1_xts_1_1(), type = "line", name = "MC", color = "green") %>%
      hc_add_series(ab_ind_dat_14_sub_1_xts_1_2(), type = "line", name = "TNG", color = "green") %>%
      hc_add_series(ab_ind_dat_14_sub_1_xts_1_3(), type = "line", name = "DCR", color = "green")
  })
  
  
  data_dict <- read_xlsx("www/data_dictionay.xlsx", sheet = config$dd_sheet)
  data_dict_df <- as.data.frame(data_dict)
  output$DATA_DICTIONARY_TABLE <- renderDataTable({data_dict_df})
  
  data_dict_nb <- read_xlsx("www/data_dictionary.xlsx",sheet = "New_Brunswick")
  data_dict_df_nb <- as.data.frame(data_dict_nb)
  output$DATA_DICTIONARY_NB <- renderDataTable({data_dict_df_nb})
  
  observeEvent(nsload_data_pre(),{
    #jobs <- jobs_list(workspace = "https://adb-5718177041572308.8.azuredatabricks.net", token = "dapi0eb9c309016768826856d64b9787ce4a")
    #jb_df <- jobs$response_tidy$job_id
    #for (jb_id in jb_df)
    #{
    
    #jb_run <- runs_list(job_id = jb_id,workspace = "https://adb-5718177041572308.8.azuredatabricks.net", token = "dapi0eb9c309016768826856d64b9787ce4a")
    #rn_id <- head(jb_run$response_tidy$runs.run_id,1)
    #rn_stat <- get_run_status(run_id = rn_id,workspace = "https://adb-5718177041572308.8.azuredatabricks.net", token = "dapi0eb9c309016768826856d64b9787ce4a")
    #state <- rn_stat$response_list$state$result_state
    #print(paste(jb_id,state,sep = ":"))
    #}
    
    output$SERVER_STATUS <- renderUI({
      tags$table(
        id="server_stat",
        style = "width:100%",
        tags$tr(
          tags$th("Province"),
          tags$th("Status")
        ),
        tags$tr(
          tags$td("Ontario"),
          tags$td("Ok")
        ),
        tags$tr(
          tags$td("Prince Edward Island"),
          tags$td("Ok")
        )
      )
    })
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
  
  observeEvent(input$button_pei_ind_1,{
    createAlert(session, "NB_1", content = paste("please select this table:",config$provinces$NB$table1, sep = " "), style = "info", dismiss = TRUE)
    Sys.sleep(0.5)
    updateTabsetPanel(session = session, inputId = "rted_menu", selected = "Dwn") 
  })
  
  
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
  
  # File Download System
  
  
  observeEvent(input$rted_menu, {
    if(input$rted_menu == "Dwn")
    {
      updateDateRangeInput(session, "nb_date_range",
                           start = head(nbload_data()$Date_time_local,1),
                           min = head(nbload_data()$Date_time_local,1),
                           end = tail(nbload_data()$Date_time_local,1),
                           max = tail(nbload_data()$Date_time_local,1))
      updateDateRangeInput(session, "ns_date_range",
                           start = head(ns_ind_dat$Date_time_local,1),
                           min = head(ns_ind_dat$Date_time_local,1),
                           end = tail(ns_ind_dat$Date_time_local,1),
                           max = tail(ns_ind_dat$Date_time_local,1))
      updateDateRangeInput(session, "ns_date_range_1",
                           start = head(ns_ind_dat_1$Date_time_local,1),
                           min = head(ns_ind_dat_1$Date_time_local,1),
                           end = tail(ns_ind_dat_1$Date_time_local,1),
                           max = tail(ns_ind_dat_1$Date_time_local,1))
      updateDateRangeInput(session, "ab_date_range",
                           start = head(nbload_data()$Date_time_local,1),
                           min = head(nbload_data()$Date_time_local,1),
                           end = tail(nbload_data()$Date_time_local,1),
                           max = tail(nbload_data()$Date_time_local,1))
      updateDateRangeInput(session, "bc_date_range",
                           start = head(bc_ind_dat$Date_time_local,1),
                           min = head(bc_ind_dat$Date_time_local,1),
                           end = tail(bc_ind_dat$Date_time_local,1),
                           max = tail(bc_ind_dat$Date_time_local,1))
      updateDateRangeInput(session, "bc_date_range_1",
                           start = head(bc_ind_dat_1$date_time_local,1),
                           min = head(bc_ind_dat_1$date_time_local,1),
                           end = tail(bc_ind_dat_1$date_time_local,1),
                           max = tail(bc_ind_dat_1$date_time_local,1))
      
      updateDateRangeInput(session, "on_date_range",
                           start = head(on_ind_dat$date_time_local,1),
                           min = head(on_ind_dat$date_time_local,1),
                           end = tail(on_ind_dat$date_time_local,1),
                           max = tail(on_ind_dat$date_time_local,1))
      updateDateRangeInput(session, "pei_date_range",
                           start = head(pei_ind_dat$Date_time_local,1),
                           min = head(pei_ind_dat$Date_time_local,1),
                           end = tail(pei_ind_dat$Date_time_local,1),
                           max = tail(pei_ind_dat$Date_time_local,1))
      updateDateRangeInput(session, "nfl_date_range",
                           start = head(nfl_ind_dat$Date_time_local,1),
                           min = head(nfl_ind_dat$Date_time_local,1),
                           end = tail(nfl_ind_dat$Date_time_local,1),
                           max = tail(nfl_ind_dat$Date_time_local,1))
      updateDateRangeInput(session, "qb_date_range",
                           start = head(qb_ind_dat_1$Date_time_local,1),
                           min = head(qb_ind_dat_1$Date_time_local,1),
                           end = tail(qb_ind_dat_1$Date_time_local,1),
                           max = tail(qb_ind_dat_1$Date_time_local,1))
      updateDateRangeInput(session, "qb_date_range_1",
                           start = head(qb_ind_dat_2$Date_time_local,1),
                           min = head(qb_ind_dat_2$Date_time_local,1),
                           end = tail(qb_ind_dat_2$Date_time_local,1),
                           max = tail(qb_ind_dat_2$Date_time_local,1))
      updateDateRangeInput(session, "qb_date_range_2",
                           start = head(qb_ind_dat_3$Date_time_local,1),
                           min = head(qb_ind_dat_3$Date_time_local,1),
                           end = tail(qb_ind_dat_3$Date_time_local,1),
                           max = tail(qb_ind_dat_3$Date_time_local,1))
      
      
      
      
      if(!is.null(input$nb_filter))
      {
        shinyjs::show("nb_dwn_1")
        observeEvent(input$nb_filter,{
          output$data_nb <- downloadHandler(
            
            filename = function() {
              paste(config$provinces$NB$table1,"csv",sep = ".")
            },
            content = function(file)
            {
              showNotification("Your File is Downloading, Please wait for some time.", type="message")
              date_1 <- paste(input$nb_date_range[1],"00:00:00",sep = " ")
              date_2 <- paste(input$nb_date_range[2],"00:00:00",sep = " ")
              nbload_subset_download <- subset(nbload_data(),subset = (nbload_data()$Date_time_local >= date_1 & nbload_data()$Date_time_local <= date_2))
              write.csv(nbload_subset_download, file, row.names = FALSE)
            }
          )
        })
        
      }
      if(!is.null(input$ns_filter))
      {
        shinyjs::show("ns_dwn_1")
        observeEvent(input$ns_filter,{
          output$data_ns_1 <- downloadHandler(
            
            filename = function() {
              paste(config$provinces$NS$table1,"csv",sep = ".")
            },
            content = function(file)
            {
              showNotification("Your File is Downloading, Please wait for some time.", type="message")
              date_1 <- paste(input$ns_date_range[1],"00:00:00",sep = " ")
              date_2 <- paste(input$ns_date_range[2],"00:00:00",sep = " ")
              nsload_subset_download <- subset(ns_ind_dat,subset = (ns_ind_dat$Date_time_local >= date_1 & ns_ind_dat$Date_time_local <= date_2))
              write.csv(nsload_subset_download, file, row.names = FALSE)
            }
          )
        })
        
      }
      if(!is.null(input$ns_filter_1))
      {
        shinyjs::show("ns_dwn_1")
        observeEvent(input$ns_filter_1,{
          output$data_ns_2 <- downloadHandler(
            
            filename = function() {
              paste(config$provinces$NS$table2,"csv",sep = ".")
            },
            content = function(file)
            {
              showNotification("Your File is Downloading, Please wait for some time.", type="message")
              date_1 <- paste(input$ns_date_range_1[1],"00:00:00",sep = " ")
              date_2 <- paste(input$ns_date_range_1[2],"00:00:00",sep = " ")
              nsload_subset_download_1 <- subset(ns_ind_dat_1,subset = (ns_ind_dat_1$Date_time_local >= date_1 & ns_ind_dat_1$Date_time_local <= date_2))
              write.csv(nsload_subset_download_1, file, row.names = FALSE)
            }
          )
        })
        
      }
      if(!is.null(input$bc_filter))
      {
        shinyjs::show("ns_dwn_1")
        observeEvent(input$bc_filter,{
          output$data_bc_1 <- downloadHandler(
            
            filename = function() {
              paste(config$provinces$BC$table1,"csv",sep = ".")
            },
            content = function(file)
            {
              showNotification("Your File is Downloading, Please wait for some time.", type="message")
              date_1 <- paste(input$bc_date_range[1],"00:00:00",sep = " ")
              date_2 <- paste(input$bc_date_range[2],"00:00:00",sep = " ")
              bcload_subset_download <- subset(bc_ind_dat,subset = (bc_ind_dat$Date_time_local >= date_1 & bc_ind_dat$Date_time_local <= date_2))
              write.csv(bcload_subset_download, file, row.names = FALSE)
            }
          )
        })
        
      }
      if(!is.null(input$bc_filter_1))
      {
        shinyjs::show("ns_dwn_1")
        observeEvent(input$bc_filter_1,{
          output$data_bc_2 <- downloadHandler(
            
            filename = function() {
              paste(config$provinces$BC$table3,"csv",sep = ".")
            },
            content = function(file)
            {
              showNotification("Your File is Downloading, Please wait for some time.", type="message")
              date_1 <- paste(input$bc_date_range_1[1],"00:00:00",sep = " ")
              date_2 <- paste(input$bc_date_range_1[2],"00:00:00",sep = " ")
              bcload_subset_download_1 <- subset(bc_ind_dat_1,subset = (bc_ind_dat_1$date_time_local >= date_1 & bc_ind_dat_1$date_time_local <= date_2))
              write.csv(bcload_subset_download_1, file, row.names = FALSE)
            }
          )
        })
        
      }
      if(!is.null(input$on_filter))
      {
        shinyjs::show("ns_dwn_1")
        observeEvent(input$on_filter,{
          output$data_on <- downloadHandler(
            
            filename = function() {
              paste(config$provinces$ON$table1,"csv",sep = ".")
            },
            content = function(file)
            {
              showNotification("Your File is Downloading, Please wait for some time.", type="message")
              date_1 <- paste(input$on_date_range[1],"00:00:00",sep = " ")
              date_2 <- paste(input$on_date_range[2],"00:00:00",sep = " ")
              onload_subset_download <- subset(on_ind_dat,subset = (on_ind_dat$date_time_local >= date_1 & on_ind_dat$date_time_local <= date_2))
              write.csv(onload_subset_download, file, row.names = FALSE)
            }
          )
        })
        
      }
      if(!is.null(input$pei_filter))
      {
        shinyjs::show("ns_dwn_1")
        observeEvent(input$pei_filter,{
          output$data_pei_1 <- downloadHandler(
            
            filename = function() {
              paste(config$provinces$PEI$table1,"csv",sep = ".")
            },
            content = function(file)
            {
              showNotification("Your File is Downloading, Please wait for some time.", type="message")
              date_1 <- paste(input$pei_date_range[1],"00:00:00",sep = " ")
              date_2 <- paste(input$pei_date_range[2],"00:00:00",sep = " ")
              peiload_subset_download <- subset(pei_ind_dat,subset = (pei_ind_dat$Date_time_local >= date_1 & pei_ind_dat$Date_time_local <= date_2))
              write.csv(peiload_subset_download, file, row.names = FALSE)
            }
          )
        })
        
      }
      if(!is.null(input$nfl_filter))
      {
        shinyjs::show("ns_dwn_1")
        observeEvent(input$nfl_filter,{
          output$data_nfl <- downloadHandler(
            
            filename = function() {
              paste(config$provinces$NFL$table1,"csv",sep = ".")
            },
            content = function(file)
            {
              showNotification("Your File is Downloading, Please wait for some time.", type="message")
              date_1 <- paste(input$nfl_date_range[1],"00:00:00",sep = " ")
              date_2 <- paste(input$nfl_date_range[2],"00:00:00",sep = " ")
              nflload_subset_download <- subset(nfl_ind_dat,subset = (nfl_ind_dat$Date_time_local >= date_1 & nfl_ind_dat$Date_time_local <= date_2))
              write.csv(nflload_subset_download, file, row.names = FALSE)
            }
          )
        })
        
      }
      if(!is.null(input$qb_filter))
      {
        shinyjs::show("ns_dwn_1")
        observeEvent(input$qb_filter,{
          output$data_qb_1 <- downloadHandler(
            
            filename = function() {
              paste(config$provinces$QUEBEC$table1,"csv",sep = ".")
            },
            content = function(file)
            {
              showNotification("Your File is Downloading, Please wait for some time.", type="message")
              date_1 <- paste(input$qb_date_range[1],"00:00:00",sep = " ")
              date_2 <- paste(input$qb_date_range[2],"00:00:00",sep = " ")
              qbload_subset_download <- subset(qb_ind_dat_1,subset = (qb_ind_dat_1$Date_time_local >= date_1 & qb_ind_dat_1$Date_time_local <= date_2))
              write.csv(qbload_subset_download, file, row.names = FALSE)
            }
          )
        })
        
      }
      if(!is.null(input$qb_filter_1))
      {
        shinyjs::show("ns_dwn_1")
        observeEvent(input$qb_filter_1,{
          output$data_qb_2 <- downloadHandler(
            
            filename = function() {
              paste(config$provinces$QUEBEC$table2,"csv",sep = ".")
            },
            content = function(file)
            {
              showNotification("Your File is Downloading, Please wait for some time.", type="message")
              date_1 <- paste(input$qb_date_range_1[1],"00:00:00",sep = " ")
              date_2 <- paste(input$qb_date_range_1[2],"00:00:00",sep = " ")
              qbload_subset_download_1 <- subset(qb_ind_dat_2,subset = (qb_ind_dat_2$Date_time_local >= date_1 & qb_ind_dat_2$Date_time_local <= date_2))
              write.csv(qbload_subset_download_1, file, row.names = FALSE)
            }
          )
        })
        
      }
      if(!is.null(input$qb_filter_2))
      {
        shinyjs::show("ns_dwn_1")
        observeEvent(input$qb_filter_2,{
          output$data_qb_3 <- downloadHandler(
            
            filename = function() {
              paste(config$provinces$QUEBEC$table3,"csv",sep = ".")
            },
            content = function(file)
            {
              showNotification("Your File is Downloading, Please wait for some time.", type="message")
              date_1 <- paste(input$qb_date_range_2[1],"00:00:00",sep = " ")
              date_2 <- paste(input$qb_date_range_2[2],"00:00:00",sep = " ")
              qbload_subset_download_2 <- subset(qb_ind_dat_3,subset = (qb_ind_dat_3$Date_time_local >= date_1 & qb_ind_dat_3$Date_time_local <= date_2))
              write.csv(qbload_subset_download_2, file, row.names = FALSE)
            }
          )
        })
        
      }
    }
    if(input$rted_menu == "pei")
    {
      #updating the dates after the data loads (PEI)
      pei_dd_1 <- as.Date(Sys.Date())-(7)
      pei_dd_1_1 <- paste(pei_dd_1,"00:00:00",sep=" ")
      pei_dd_2_2 <- as.Date(tail(pei_ind_dat$Date_time_local,1))
      updateSliderInput(session, "pei_dates_1",
                        min = as.Date(head(pei_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(pei_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(pei_dd_1_1),pei_dd_2_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "pei_dates_2",
                        min = as.Date(head(pei_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(pei_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(pei_dd_1_1),pei_dd_2_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "pei_dates_3",
                        min = as.Date(head(pei_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(pei_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(pei_dd_1_1),pei_dd_2_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "pei_dates_4",
                        min = as.Date(head(pei_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(pei_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(pei_dd_1_1),pei_dd_2_2),
                        timeFormat="%Y-%m-%d")
    }
    if(input$rted_menu == "NB"){
      #updating the dates after the data loads (NB)
      nb_dd_1 <- as.Date(Sys.Date())-(7)
      nb_dd_1_1 <- paste(nb_dd_1,"00:00:00",sep=" ")
      nb_dd_2_2 <- as.Date(tail(nb_ind_dat$Date_time_local,1))
      updateSliderInput(session, "nb_dates_1",
                        min = as.Date(head(nb_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(nb_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(nb_dd_1_1),nb_dd_2_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "nb_dates_2",
                        min = as.Date(head(nb_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(nb_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(nb_dd_1_1),nb_dd_2_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "nb_dates_3",
                        min = as.Date(head(nb_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(nb_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(nb_dd_1_1),nb_dd_2_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "nb_dates_4",
                        min = as.Date(head(nb_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(nb_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(nb_dd_1_1),nb_dd_2_2),
                        timeFormat="%Y-%m-%d")
    }
    if(input$rted_menu == "NS"){
      #updating the dates after the data loads (NB)
      ns_dd_1 <- as.Date(Sys.Date())-(7)
      ns_dd_1_1 <- paste(ns_dd_1,"00:00:00",sep=" ")
      ns_dd_2_2 <- as.Date(tail(ns_ind_dat$Date_time_local,1))
      ns_dd_3_2 <- as.Date(tail(ns_ind_dat_1$Date_time_local,1))
      updateSliderInput(session, "ns_dates_1",
                        min = as.Date(head(ns_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(ns_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(ns_dd_1_1),ns_dd_2_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "ns_dates_2",
                        min = as.Date(head(ns_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(ns_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(ns_dd_1_1),ns_dd_3_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "ns_dates_3",
                        min = as.Date(head(ns_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(ns_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(ns_dd_1_1),ns_dd_3_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "ns_dates_4",
                        min = as.Date(head(ns_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(ns_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(ns_dd_1_1),ns_dd_3_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "ns_dates_5",
                        min = as.Date(head(ns_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(ns_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(ns_dd_1_1),ns_dd_3_2),
                        timeFormat="%Y-%m-%d")
    }
    if(input$rted_menu == "NFL"){
      nfl_dd_1 <- as.Date(Sys.Date())-(7)
      nfl_dd_1_1 <- paste(nfl_dd_1,"00:00:00",sep=" ")
      nfl_dd_2_2 <- as.Date(tail(nfl_ind_dat$Date_time_local,1))
      updateSliderInput(session, "nfl_dates_1",
                        min = as.Date(head(nfl_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(nfl_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(nfl_dd_1_1),nfl_dd_2_2),
                        timeFormat="%Y-%m-%d")
    }
    if(input$rted_menu == "AB"){
      ab_dd_8_1 <- as.Date(tail(ab_ind_dat_8$Update_Time,1))-(7)
      ab_dd_8_1_1 <- paste(ab_dd_8_1,"00:00:00",sep=" ")
      ab_dd_8_2_2 <- as.Date(tail(ab_ind_dat_8$Update_Time,1))
      updateSliderInput(session, "ab_dates_8",
                        min = as.Date(head(ab_ind_dat_8$Update_Time,1),"%Y-%m-%d"),
                        max = as.Date(tail(ab_ind_dat_8$Update_Time,1),"%Y-%m-%d"),
                        value=c(as.Date(ab_dd_8_1_1),ab_dd_8_2_2),
                        timeFormat="%Y-%m-%d")
      updateSelectInput(session,"ab_ind_8_ff", choices = unique(ab_ind_dat_8$ASSET))
      ab_dd_9_1 <- as.Date(tail(ab_ind_dat_9$Update_Time,1))-(7)
      ab_dd_9_1_1 <- paste(ab_dd_9_1,"00:00:00",sep=" ")
      ab_dd_9_2_2 <- as.Date(tail(ab_ind_dat_9$Update_Time,1))
      updateSliderInput(session, "ab_dates_9",
                        min = as.Date(head(ab_ind_dat_9$Update_Time,1),"%Y-%m-%d"),
                        max = as.Date(tail(ab_ind_dat_9$Update_Time,1),"%Y-%m-%d"),
                        value=c(as.Date(ab_dd_9_1_1),ab_dd_9_2_2),
                        timeFormat="%Y-%m-%d")
      updateSelectInput(session,"ab_ind_9_ff", choices = unique(ab_ind_dat_9$ASSET))
      ab_dd_10_1 <- as.Date(tail(ab_ind_dat_10$Update_Time,1))-(7)
      ab_dd_10_1_1 <- paste(ab_dd_10_1,"00:00:00",sep=" ")
      ab_dd_10_2_2 <- as.Date(tail(ab_ind_dat_10$Update_Time,1))
      updateSliderInput(session, "ab_dates_10",
                        min = as.Date(head(ab_ind_dat_10$Update_Time,1),"%Y-%m-%d"),
                        max = as.Date(tail(ab_ind_dat_10$Update_Time,1),"%Y-%m-%d"),
                        value=c(as.Date(ab_dd_10_1_1),ab_dd_10_2_2),
                        timeFormat="%Y-%m-%d")
      updateSelectInput(session,"ab_ind_10_ff", choices = unique(ab_ind_dat_10$ASSET))
      ab_dd_11_1 <- as.Date(tail(ab_ind_dat_11$Update_Time,1))-(7)
      ab_dd_11_1_1 <- paste(ab_dd_11_1,"00:00:00",sep=" ")
      ab_dd_11_2_2 <- as.Date(tail(ab_ind_dat_11$Update_Time,1))
      updateSliderInput(session, "ab_dates_11",
                        min = as.Date(head(ab_ind_dat_11$Update_Time,1),"%Y-%m-%d"),
                        max = as.Date(tail(ab_ind_dat_11$Update_Time,1),"%Y-%m-%d"),
                        value=c(as.Date(ab_dd_11_1_1),ab_dd_11_2_2),
                        timeFormat="%Y-%m-%d")
      updateSelectInput(session,"ab_ind_11_ff", choices = unique(ab_ind_dat_11$ASSET))
      ab_dd_12_1 <- as.Date(tail(ab_ind_dat_12$Update_Time,1))-(7)
      ab_dd_12_1_1 <- paste(ab_dd_12_1,"00:00:00",sep=" ")
      ab_dd_12_2_2 <- as.Date(tail(ab_ind_dat_12$Update_Time,1))
      updateSliderInput(session, "ab_dates_12",
                        min = as.Date(head(ab_ind_dat_12$Update_Time,1),"%Y-%m-%d"),
                        max = as.Date(tail(ab_ind_dat_12$Update_Time,1),"%Y-%m-%d"),
                        value=c(as.Date(ab_dd_12_1_1),ab_dd_12_2_2),
                        timeFormat="%Y-%m-%d")
      updateSelectInput(session,"ab_ind_12_ff", choices = unique(ab_ind_dat_12$ASSET))
      ab_dd_13_1 <- as.Date(tail(ab_ind_dat_13$Update_Time,1))-(7)
      ab_dd_13_1_1 <- paste(ab_dd_13_1,"00:00:00",sep=" ")
      ab_dd_13_2_2 <- as.Date(tail(ab_ind_dat_13$Update_Time,1))
      updateSliderInput(session, "ab_dates_13",
                        min = as.Date(head(ab_ind_dat_13$Update_Time,1),"%Y-%m-%d"),
                        max = as.Date(tail(ab_ind_dat_13$Update_Time,1),"%Y-%m-%d"),
                        value=c(as.Date(ab_dd_13_1_1),ab_dd_13_2_2),
                        timeFormat="%Y-%m-%d")
      updateSelectInput(session,"ab_ind_13_ff", choices = unique(ab_ind_dat_13$ASSET))
      ab_dd_14_1 <- as.Date(tail(ab_ind_dat_14$Update_Time,1))-(7)
      ab_dd_14_1_1 <- paste(ab_dd_14_1,"00:00:00",sep=" ")
      ab_dd_14_2_2 <- as.Date(tail(ab_ind_dat_14$Update_Time,1))
      updateSliderInput(session, "ab_dates_14",
                        min = as.Date(head(ab_ind_dat_14$Update_Time,1),"%Y-%m-%d"),
                        max = as.Date(tail(ab_ind_dat_14$Update_Time,1),"%Y-%m-%d"),
                        value=c(as.Date(ab_dd_14_1_1),ab_dd_14_2_2),
                        timeFormat="%Y-%m-%d")
      updateSelectInput(session,"ab_ind_14_ff", choices = unique(ab_ind_dat_14$ASSET))
    }
    if(input$rted_menu == "QB"){
      qb_dd_1 <- as.Date(Sys.Date())-(7)
      qb_dd_1_1 <- paste(qb_dd_1,"00:00:00",sep=" ")
      qb_dd_2_2 <- as.Date(tail(qb_ind_dat_1$Date_time_local,1))
      qb_dd_3_3 <- as.Date(tail(qb_ind_dat_3$Date_time_local,1))
      qb_dd_4_4 <- as.Date(tail(qb_ind_dat_3$Date_time_local,1))-(7)
      qb_dd_5_5 <- as.Date(tail(qb_ind_dat_2$Date_Time_UTC,1))
      updateSliderInput(session, "qb_dates_1",
                        min = as.Date(head(qb_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(qb_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(qb_dd_1_1),qb_dd_2_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "qb_dates_2",
                        min = as.Date(head(qb_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(qb_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(qb_dd_1_1),qb_dd_2_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "qb_dates_3",
                        min = as.Date(head(qb_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(qb_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(qb_dd_1_1),qb_dd_2_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "qb_dates_4",
                        min = as.Date(head(qb_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(qb_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(qb_dd_1_1),qb_dd_2_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "qb_dates_5",
                        min = as.Date(head(qb_ind_dat_3$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(qb_ind_dat_3$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(qb_dd_4_4),qb_dd_3_3),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "qb_dates_6",
                        min = as.Date(head(qb_ind_dat_3$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(qb_ind_dat_3$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(qb_dd_4_4),qb_dd_3_3),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "qb_dates_7",
                        min = as.Date(head(qb_ind_dat_3$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(qb_ind_dat_3$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(qb_dd_4_4),qb_dd_3_3),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "qb_dates_8",
                        min = as.Date(head(qb_ind_dat_3$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(qb_ind_dat_3$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(qb_dd_4_4),qb_dd_3_3),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "qb_dates_9",
                        min = as.Date(head(qb_ind_dat_3$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(qb_ind_dat_3$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(qb_dd_4_4),qb_dd_3_3),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "qb_dates_10",
                        min = as.Date(head(qb_ind_dat_3$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(qb_ind_dat_3$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(qb_dd_4_4),qb_dd_3_3),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "qb_dates_11",
                        min = as.Date(head(qb_ind_dat_3$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(qb_ind_dat_3$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(qb_dd_4_4),qb_dd_3_3),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "qb_dates_12",
                        min = as.Date(head(qb_ind_dat_2$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(qb_ind_dat_2$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(qb_dd_1_1),qb_dd_5_5),
                        timeFormat="%Y-%m-%d")
      
    }
    if(input$rted_menu == "BC"){
      bc_dd_1 <- as.Date(Sys.Date())-(7)
      bc_dd_1_1 <- paste(bc_dd_1,"00:00:00",sep=" ")
      bc_dd_2_2 <- as.Date(tail(bc_ind_dat$Date_time_local,1))
      bc_dd_3_2 <- as.Date(tail(bc_ind_dat_1$Date_time_local,1))
      updateSliderInput(session, "bc_dates_1",
                        min = as.Date(head(bc_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(bc_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(bc_dd_1_1),bc_dd_2_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "bc_dates_2",
                        min = as.Date(head(bc_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(bc_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(bc_dd_1_1),bc_dd_3_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "bc_dates_3",
                        min = as.Date(head(bc_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(bc_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(bc_dd_1_1),bc_dd_3_2),
                        timeFormat="%Y-%m-%d")
    }
    if(input$rted_menu == "ON"){
      #updating the dates after the data loads (NB)
      on_dd_1 <- as.Date(Sys.Date())-(7)
      on_dd_1_1 <- paste(on_dd_1,"00:00:00",sep=" ")
      on_dd_2_2 <- as.Date(tail(on_ind_dat$date_time_local,1))
      updateSliderInput(session, "on_dates_1",
                        min = as.Date(head(on_ind_dat$date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(on_ind_dat$date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(on_dd_1_1),on_dd_2_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "on_dates_2",
                        min = as.Date(head(on_ind_dat$date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(on_ind_dat$date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(on_dd_1_1),on_dd_2_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "on_dates_3",
                        min = as.Date(head(on_ind_dat$date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(on_ind_dat$date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(on_dd_1_1),on_dd_2_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "on_dates_4",
                        min = as.Date(head(on_ind_dat$date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(on_ind_dat$date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(on_dd_1_1),on_dd_2_2),
                        timeFormat="%Y-%m-%d")
    }
  })
  
  
  
  
  
  
  
  
  #individual pages
  
}




# Run the application 
shinyApp(ui = ui, server = server)
