#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(shinycssloaders)
library(markdown)

# Define UI for application that draws a histogram


navbarPage("Análise da Produtividade por Competência", collapsible = TRUE, 
    tabPanel("Análises dos Indicadores", icon = icon("list-alt"),
        fluidPage(theme = shinytheme("cerulean"),

        # Application title
        titlePanel("Análise dos Indicadores por Competência"),
        
        
        # Sidebar with a slider input for number of bins
        sidebarLayout(
            sidebarPanel(width = 3,
            
            pickerInput("grupo",
                        h4("Selecione o grupo:"),
                        choices = list("GRUPO 1" = 1, "GRUPO 2" = 2, "GRUPO 3" = 3,
                                        "GRUPO 4" = 4, "GRUPO 5" = 5, "GRUPO 6" = 6,
                                        "GRUPO 7" = 7, "GRUPO 8" = 8, "GRUPO 9" = 9,
                                        "GRUPO 10" = 10, "GRUPO 11" = 11, "GRUPO 12" = 12,
                                        "GRUPO 13" = 13, "GRUPO 14" = 14, "GRUPO 15" = 15,
                                        "GRUPO 16" = 16, "GRUPO 17" = 17, "GRUPO 18" = 18,
                                        "GRUPO 19" = 19, "GRUPO 20" = 20, "GRUPO 21" = 21,
                                        "GRUPO 22" = 22, "GRUPO 23" = 23, "GRUPO 24" = 24,
                                        "GRUPO 25" = 25, "GRUPO 26" = 26, "GRUPO 27" = 27,
                                        "GRUPO 28" = 28, "GRUPO 29" = 29, "GRUPO 30" = 30,
                                        "GRUPO 31" = 32)),         
                     
                     
            pickerInput("indicador",
                        h4("Selecione o indicador:"),
                        choices = list("ACERVO",
                                       "BAIXADOS",
                                       "DECISÕES",
                                       "DESPACHOS",
                                       "SENTENÇAS",
                                       "AUDIÊNCIAS")),
    
            
            
            radioButtons("ano",
                        h4("Selecione o ano:"),
                        choices = list("2019" = 2019, "2020" = 2020)),
            
            
            uiOutput("periodo"),
            
            uiOutput("unidade")
            
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Comparativo entre Unidades", icon = icon("bar-chart-o"), withSpinner(plotOutput("plotbar"))),
                tabPanel("Evolução Mensal", icon = icon("table"), withSpinner(tableOutput("data"))),
                tabPanel("Evolução Mensal", icon = icon("chart-line"), withSpinner(plotlyOutput("plotline")))
            )
        )    
        )
                )
        ),
    
        tabPanel("Taxa de Congestionamento", icon = icon("list-alt"),
                 titlePanel("Análise da Taxa de Congestionamento por Competência"),
                 
                 sidebarLayout(
                     sidebarPanel(width = 3,
                                  pickerInput("grupo_taxa",
                                              h4("Selecione o grupo:"),
                                              choices = list("GRUPO 1" = 1, "GRUPO 2" = 2, "GRUPO 3" = 3,
                                                             "GRUPO 4" = 4, "GRUPO 5" = 5, "GRUPO 6" = 6,
                                                             "GRUPO 7" = 7, "GRUPO 8" = 8, "GRUPO 9" = 9,
                                                             "GRUPO 10" = 10, "GRUPO 11" = 11, "GRUPO 12" = 12,
                                                             "GRUPO 13" = 13, "GRUPO 14" = 14, "GRUPO 15" = 15,
                                                             "GRUPO 16" = 16, "GRUPO 17" = 17, "GRUPO 18" = 18,
                                                             "GRUPO 19" = 19, "GRUPO 20" = 20, "GRUPO 21" = 21,
                                                             "GRUPO 22" = 22, "GRUPO 23" = 23, "GRUPO 24" = 24,
                                                             "GRUPO 25" = 25, "GRUPO 26" = 26, "GRUPO 27" = 27,
                                                             "GRUPO 28" = 28, "GRUPO 29" = 29, "GRUPO 30" = 30)),
                                  
                                  
                                  uiOutput("periodo_taxa"),
                                  
                                  uiOutput("unidade_taxa")
                                  
                                  
                     ),
                     
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Comparativo entre Unidades", icon = icon("bar-chart-o"), withSpinner(plotOutput("plotbar_taxa"))),
                             tabPanel("Evolução Mensal da Taxa", icon = icon("table"), withSpinner(tableOutput("data_taxa"))),
                             tabPanel("Evolução Mensal da Taxa", icon = icon("chart-line"), withSpinner(plotlyOutput("plotline_taxa")))
                         )
                     )
                 )
        
                 ),
    
        tabPanel("Demonstrativo de Distribuições", icon = icon("list-alt"),
                 titlePanel("Análise dos Distribuídos/Redistribuídos"),
                 sidebarLayout(
                     sidebarPanel(width = 3,
                                  conditionalPanel(condition = "input.tabselected==1",
                                                   radioButtons("ano_dist",
                                                                h4("Selecione o ano:"),
                                                                choices = list("2019" = 2019, "2020" = 2020)),
                                                   
                                                   uiOutput("periodo_dist"),
                                                   
                                                   uiOutput("unidade_dist")
                                                   ),
                                  conditionalPanel(condition = "input.tabselected==2",
                                                   radioButtons("ano_fluxo",
                                                                h4("Selecione o ano:"),
                                                                choices = list("2019" = 2019, "2020" = 2020)),
                                                   
                                                   uiOutput("periodo_fluxo"),
                                                   
                                                   uiOutput("unidade_fluxo")
                                  )
                     ),
                     mainPanel(
                         tabsetPanel(id = "tabselected",
                             tabPanel("Comparativo entre Unidades", value = 1, icon = icon("bar-chart-o"), withSpinner(plotOutput("plotbar_dist"))),
                             tabPanel("Evolução Mensal do Saldo de Processos", value = 1,  icon = icon("chart-line"), withSpinner(plotlyOutput("plotline_dist"))),
                             tabPanel("Análise do Fluxo Processual", value = 2, icon = icon("bar-chart-o"), withSpinner(plotOutput("plotbar_fluxo")))
                         )
                     )
                 )
                 ),
        tabPanel("Sobre", icon = icon("comment", lib = "glyphicon"),
                 includeMarkdown("sobre.md")
                )
)
