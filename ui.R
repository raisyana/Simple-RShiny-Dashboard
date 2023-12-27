# R-SHINY PROGRAM
# DISTRIBUSI DATA KONTINYU

library(shiny)
library(shinydashboard)
library(rvest)
library(DT)
library(distrEx)
library(RcmdrMisc)
library(RColorBrewer)
library(ggplot2)
library(plotly)
library(glue)

# ==========================================================================================================================

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "MAIN MENU"),
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("HOME", icon = icon("house-user"),
                                     menuSubItem("Diagram",
                                                 tabName = 'diagram',
                                                 icon = icon('signal')),
                                     menuSubItem("Data",
                                                 tabName = 'data',
                                                 icon = icon('list-alt'))
                            ),
                            menuItem("DISTRIBUSI UNIFORM", tabName = "distribusiUniform", icon = icon("bezier-curve")),
                            menuItem("DISTRIBUSI NORMAL", icon = icon("bar-chart-o"),
                                     menuSubItem('Density Function',
                                                 tabName = 'dNorm',
                                                 icon = icon('ellipsis-v')),
                                     menuSubItem('Cumulative Density Function',
                                                 tabName = 'pNorm',
                                                 icon = icon('ellipsis-v')),
                                     menuSubItem('Quantile Function',
                                                 tabName = 'qNorm',
                                                 icon = icon('ellipsis-v')),
                                     menuSubItem('Random Sampling',
                                                 tabName = 'rNorm',
                                                 icon = icon('ellipsis-v'))
                            ),
                            menuItem("DISTRIBUSI EKSPONENSIAL", icon = icon("chart-pie"),
                                     menuSubItem('Density Function',
                                                 tabName = 'dExp',
                                                 icon = icon('ellipsis-v')),
                                     menuSubItem('Cumulative Density Function',
                                                 tabName = 'pExp',
                                                 icon = icon('ellipsis-v')),
                                     menuSubItem('Quantile Function',
                                                 tabName = 'qExp',
                                                 icon = icon('ellipsis-v')),
                                     menuSubItem('Random Sampling',
                                                 tabName = 'rExp',
                                                 icon = icon('ellipsis-v'))
                            ),
                            menuItem("DISTRIBUSI WEIBULL", tabName = "distribusiWeibull", icon = icon("chart-area"))
                        )
                    ),
                    
                    
                    
# ******************************************************************************************************************************************************************************************
                    
                    dashboardBody(
                        tabItems(
                            tabItem("diagram",
                                    fluidPage(
                                      valueBox("TEMPERATURE", "VISUALISASI DATA TEMPERATURE MAKSIMUM PROVISI JAWA TIMUR TAHUN 2020", 
                                               icon = icon("temperature-high"), width = 12),
                                        tabBox(id = "menu1",
                                               tabPanel(title = "JANUARI", plotlyOutput(outputId = "plotJanuari"), width = 9),
                                               tabPanel(title = "FEBRUARI", plotlyOutput(outputId = "plotFebruari"), width = 9),
                                               tabPanel(title = "MARET", plotlyOutput(outputId = "plotMaret"), width = 9),
                                               tabPanel(title = "APRIL", plotlyOutput(outputId = "plotApril"), width = 9),
                                               tabPanel(title = "MEI", plotlyOutput(outputId = "plotMei"), width = 9),
                                               tabPanel(title = "JUNI", plotlyOutput(outputId = "plotJuni"), width = 9)
                                        ),
                                        tabBox(id = "menu2",
                                               tabPanel(title = "JULI", plotlyOutput(outputId = "plotJuli"), width = 9),
                                               tabPanel(title = "AGUSTUS", plotlyOutput(outputId = "plotAgustus"), width = 9),
                                               tabPanel(title = "SEPT", plotlyOutput(outputId = "plotSeptember"), width = 9),
                                               tabPanel(title = "OKT", plotlyOutput(outputId = "plotOktober"), width = 9),
                                               tabPanel(title = "NOV", plotlyOutput(outputId = "plotNovember"), width = 9),
                                               tabPanel(title = "DES", plotlyOutput(outputId = "plotDesember"), width = 9)
                                        ))),
                            
                            tabItem("data",
                                    fluidPage(
                                        box(title = "DATA TEMPERATURE MAKSIMUM PROVINSI JAWA TIMUR TAHUN 2020", status = "primary", 
                                            solidHeader = T, width = 16, dataTableOutput("temperature"))
                                    )),
                            
                            
                            
# ========================UNIFORM============================================================================================================================================================ 
                            
                            tabItem("distribusiUniform", 
                                    fluidRow(
                                        box(title = "APA SIH DISTRIBUSI UNIFORM ITU?", status = "info", solidHeader = T, textOutput("penjelasanUniform", inline = F), width = 6),
                                        box(title = "PENJELASAN", status = "info", solidHeader = T, textOutput("noteUniform", inline = F), width = 6),
                                        box(title = "DATA TEMPERATURE PROVINSI JAWA TIMUR TAHUN 2020", status = "primary",
                                            background = "light-blue", plotOutput("histUniform"), width = 6),
                                        box(numericInput(inputId = "unif_input","Tentukan Sample Data yang Ingin Diuji",value = 1000)),
                                        box(dataTableOutput("unif_hasil"))
                                    )),
                            
                            
                            
# ========================NORMAL============================================================================================================================================================ 
                            
                            tabItem("dNorm", 
                                    fluidRow(
                                        box(title = "APA SIH DISTRIBUSI NORMAL - DENSITY FUNCTION ITU?", status = "info", solidHeader = T, textOutput("penjelasandNorm", inline = F), width = 6),
                                        box(title = "PENJELASAN", status = "info", solidHeader = T,
                                            textOutput("notedNormal", inline = F), width = 6),
                                        tabBox(id = "tabdNormal",
                                               tabPanel(title = "HISTOGRAM", plotOutput(outputId = "histdNormal"), width = 6),
                                               tabPanel(title = "PLOT", plotlyOutput(outputId = "plotdNormal"), width = 6)),
                                        tabBox(id = "dNorm_input",
                                               tabPanel(title = "BATAS BAWAH", numericInput(inputId = "dNorm_batasbawah",
                                                        "Tentukan Batas Bawah Data Sample [minimal 0]", value = 50)),
                                               tabPanel(title = "BATAS ATAS", numericInput(inputId = "dNorm_batasatas",
                                                        "Tentukan Batas Atas Data Sample [maks 353]", value = 250)),
                                               tabPanel(title = "RATA - RATA", numericInput(inputId = "dNorm_mean","Tentukan Nilai Mean yang Ingin Diuji",value = 29)),
                                               tabPanel(title = "SIMPANGAN BAKU", numericInput(inputId = "dNorm_sd","Tentukan Nilai Simpangan Baku yang Ingin Diuji",value = 1.2))),
                                        box(dataTableOutput("dNorm_hasil"))
                                    )),
                            tabItem("pNorm", 
                                    fluidRow(
                                        box(title = "APA SIH DISTRIBUSI NORMAL -  CUMULATIVE DENSITY FUNCTION ITU?", status = "info", solidHeader = T, textOutput("penjelasanpNorm", inline = F), width = 12),
                                        box(title = "PENJELASAN", status = "info", solidHeader = T,
                                            textOutput("notepNormal", inline = F), width = 12),
                                        tabBox(id = "tabpNormal",
                                               tabPanel(title = "HISTOGRAM", plotOutput(outputId = "histpNormal"), width = 6),
                                               tabPanel(title = "PLOT", plotlyOutput(outputId = "plotpNormal"), width = 6)),
                                        tabBox(id = "pNorm_input",
                                               tabPanel(title = "BATAS BAWAH", numericInput(inputId = "pNorm_batasbawah",
                                                                                            "Tentukan Batas Bawah Data Sample [minimal 0]", value = 50)),
                                               tabPanel(title = "BATAS ATAS", numericInput(inputId = "pNorm_batasatas",
                                                                                           "Tentukan Batas Atas Data Sample [maks 353]", value = 250)),
                                               tabPanel(title = "RATA - RATA", numericInput(inputId = "pNorm_mean","Tentukan Nilai Mean yang Ingin Diuji",value = 29)),
                                               tabPanel(title = "SIMPANGAN BAKU", numericInput(inputId = "pNorm_sd","Tentukan Nilai Simpangan Baku yang Ingin Diuji",value = 1.2))),
                                        box(dataTableOutput("pNorm_hasil"))
                                    )),
                            tabItem("qNorm", 
                                    fluidRow(
                                        box(title = "APA SIH DISTRIBUSI NORMAL -  QUANTILE FUNCTION ITU?", status = "info", solidHeader = T, textOutput("penjelasanqNorm", inline = F), width = 6),
                                        box(title = "PENJELASAN", status = "info", solidHeader = T,
                                            textOutput("noteqNormal", inline = F), width = 6),
                                        tabBox(id = "tabqNormal",
                                               tabPanel(title = "HISTOGRAM", plotOutput(outputId = "histqNormal"), width = 6),
                                               tabPanel(title = "PLOT", plotlyOutput(outputId = "plotqNormal"), width = 6)),
                                        tabBox(id = "qNorm_input",
                                               tabPanel(title = "RATA - RATA", numericInput(inputId = "qNorm_mean","Tentukan Nilai Mean yang Ingin Diuji",value = 29)),
                                               tabPanel(title = "SIMPANGAN BAKU", numericInput(inputId = "qNorm_sd","Tentukan Nilai Simpangan Baku yang Ingin Diuji",value = 1.2))),
                                        box(dataTableOutput("qNorm_hasil"))
                                    )),
                            tabItem("rNorm", 
                                    fluidRow(
                                        box(title = "APA SIH DISTRIBUSI NORMAL -  RANDOM FUNCTION ITU?", status = "info", solidHeader = T, textOutput("penjelasanrNorm", inline = F), width = 6),
                                        box(title = "PENJELASAN", status = "info", solidHeader = T,
                                            textOutput("noterNormal", inline = F), width = 6),
                                        box(title = "DATA TEMPERATURE PROVINSI JAWA TIMUR TAHUN 2020", status = "primary",
                                            background = "light-blue", plotOutput("histrNormal"), width = 6),
                                        tabBox(id = "rNorm_input",
                                               tabPanel(title = "RATA - RATA", numericInput(inputId = "rNorm_mean","Tentukan Nilai Mean yang Ingin Diuji",value = 29)),
                                               tabPanel(title = "SIMPANGAN BAKU", numericInput(inputId = "rNorm_sd","Tentukan Nilai Simpangan Baku yang Ingin Diuji",value = 1.2))),
                                        box(dataTableOutput("rNorm_hasil"))
                                    )),
                            
                            
                            
# ========================EKSPONENSIAL============================================================================================================================================================  
                            
                            tabItem("dExp", 
                                    fluidRow(
                                        box(title = "APA SIH DISTRIBUSI EKSPONENSIAL -  DENSITY FUNCTION ITU?", status = "info", solidHeader = T, textOutput("penjelasandExp", inline = F), width = 6),
                                        box(title = "PENJELASAN", status = "info", solidHeader = T,
                                            textOutput("notedExp", inline = F), width = 6),
                                        box(title = "DATA TEMPERATURE PROVINSI JAWA TIMUR TAHUN 2020",status = "primary", solidHeader = T,
                                            background = "light-blue", plotlyOutput(outputId = "plotdExp"), width = 6),
                                        tabBox(id = "dExp_input",
                                               tabPanel(title = "BATAS BAWAH", numericInput(inputId = "dExp_batasbawah",
                                                                                            "Tentukan Batas Bawah Data Sample [minimal 0]", value = 50)),
                                               tabPanel(title = "BATAS ATAS", numericInput(inputId = "dExp_batasatas",
                                                                                           "Tentukan Batas Atas Data Sample [maks 353]", value = 250)),
                                               tabPanel(title = "RATE", numericInput(inputId = "dExp_rate","Tentukan Nilai RATE yang Ingin Diuji",value = 1))),
                                        box(dataTableOutput("dExp_hasil"))
                                    )),
                            tabItem("pExp", 
                                    fluidRow(
                                      box(title = "APA SIH DISTRIBUSI EKSPONENSIAL -  CUMULATIVE DENSITY FUNCTION ITU?", status = "info", solidHeader = T, textOutput("penjelasanpExp", inline = F), width = 6),
                                      box(title = "PENJELASAN", status = "info", solidHeader = T,
                                          textOutput("notepExp", inline = F), width = 6),
                                      tabBox(id = "tabpExp",
                                             tabPanel(title = "HISTOGRAM", plotOutput(outputId = "histpExp"), width = 9),
                                             tabPanel(title = "PLOT", plotlyOutput(outputId = "plotpExp"), width = 9)),
                                      tabBox(id = "pExp_input",
                                             tabPanel(title = "BATAS BAWAH", numericInput(inputId = "pExp_batasbawah",
                                                                                          "Tentukan Batas Bawah Data Sample [minimal 0]", value = 50)),
                                             tabPanel(title = "BATAS ATAS", numericInput(inputId = "pExp_batasatas",
                                                                                         "Tentukan Batas Atas Data Sample [maks 353]", value = 250))),
                                      box(dataTableOutput("pExp_hasil"))
                                    )),

                            tabItem("qExp", 
                                    fluidRow(
                                        box(title = "APA SIH DISTRIBUSI EKSPONENSIAL -  QUANTILE FUNCTION ITU?", status = "info", solidHeader = T, textOutput("penjelasanqExp", inline = F), width = 6),
                                        box(title = "PENJELASAN", status = "info", solidHeader = T,
                                          textOutput("noteqExp", inline = F), width = 6),
                                        box(title = "DATA TEMPERATURE PROVINSI JAWA TIMUR TAHUN 2020",status = "primary", solidHeader = T,
                                            background = "light-blue", plotOutput(outputId = "histqExp"), width = 6),
                                        box(sliderInput("qExp_slider", label = "Slider Input", min = 0, max = 20, value = 6, step = 0,5), width = 6),
                                        box(dataTableOutput("qExp_hasil"))
                                    )),
                            tabItem("rExp", 
                                    fluidRow(
                                        box(title = "APA SIH DISTRIBUSI EKSPONENSIAL -  RANDOM FUNCTION ITU?", status = "info", solidHeader = T, textOutput("penjelasanrExp", inline = F), width = 6),
                                        box(title = "PENJELASAN", status = "info", solidHeader = T,
                                            textOutput("noterExp", inline = F), width = 6),
                                        box(title = "DATA TEMPERATURE PROVINSI JAWA TIMUR TAHUN 2020",status = "primary", solidHeader = T,
                                            background = "light-blue", plotOutput(outputId = "histrExp"), width = 6),
                                        tabBox(id = "rExp_input",
                                               tabPanel(title = "BATAS BAWAH", numericInput(inputId = "rExp_batasbawah",
                                                                                            "Tentukan Batas Bawah Data Sample [minimal 0]", value = 50)),
                                               tabPanel(title = "BATAS ATAS", numericInput(inputId = "rExp_batasatas",
                                                                                           "Tentukan Batas Atas Data Sample [maks 353]", value = 250)),
                                               tabPanel(title = "LAMBDA", numericInput(inputId = "rExp_lambda",
                                                                                           "Tentukan Lambda", value = 10))),
                                        box(dataTableOutput("rExp_hasil"))
                                    )),
                            
                            
                            
# ========================WEIBULL============================================================================================================================================================ 
                            
                            tabItem("distribusiWeibull",
                                    box(title = "APA SIH DISTRIBUSI WEIBULL ITU?", status = "info", solidHeader = T, textOutput("penjelasanWeibull", inline = F), width = 6),
                                    box(title = "PENJELASAN", status = "info", solidHeader = T,
                                        textOutput("noteWeibull")),
                                    box(title = "DATA TEMPERATURE PROVINSI JAWA TIMUR TAHUN 2020", status = "primary", solidHeader = T,
                                        background = "light-blue", plotOutput(outputId = "histWeibull")),
                                    tabBox(id = "weibull_input",
                                           tabPanel(title = "BATAS BAWAH", numericInput(inputId = "weibull_batasbawah",
                                                                                        "Tentukan Batas Bawah Data Sample [minimal 0]", value = 50)),
                                           tabPanel(title = "BATAS ATAS", numericInput(inputId = "weibull_batasatas",
                                                                                       "Tentukan Batas Atas Data Sample [maks 353]", value = 250)),
                                           tabPanel(title = "MINIMAL", numericInput(inputId = "weibull_min","Tentukan Nilai Sequence Minimal",value = 25)),
                                           tabPanel(title = "MAKSIMAL", numericInput(inputId = "weibull_maks","Tentukan Nilai Sequence Maksimal",value = 32))),
                                    box(dataTableOutput("weibull_hasil")))
                        ) # kurung tutup tabItems
                    ) # kurung tutup dashboardBody
) # kurung tutup dashboardPage


