#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)
library(ggthemes)
library(stringr)



dados <- readRDS("indicadores.rds")

duplicados <- duplicated(dados,fromLast = TRUE)
which(duplicados)
dados <- dados[!duplicados,] 



dados<- dados %>% mutate_all(replace_na, 0)


#dados[is.na(dados)] <- 0

taxa <- readRDS("taxa.rds")
#taxa[is.na(taxa)] <- 0

taxa <- taxa[-which(is.na(taxa$'TAXA LÍQUIDA')),]

taxa<- taxa %>% mutate_all(replace_na, 0)

distribuidos <- readRDS("distribuidos.rds")

fluxo <- readRDS("fluxo_processual.rds")

duplicados2 <- duplicated(fluxo,fromLast = TRUE)
which(duplicados2)
fluxo <- fluxo[!duplicados2,] 

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    grupo <- reactive({
        dados %>% filter(GRUPO==input$grupo) 
    }) 
    
    ano <- reactive({
        grupo() %>% filter(ANO==input$ano) 
    })
    
    output$periodo <- renderUI(
        if (input$ano == 2019){
        sliderTextInput(
            inputId = "per",
            label = h4("Selecione o período:"), 
            choices = unique(ano()$DATA),
            selected = unique(ano()$DATA)[c(1, 12)]
        )} else if (input$ano == 2020){
            sliderTextInput(
                inputId = "per",
                label = h4("Selecione o período:"), 
                choices = unique(ano()$DATA),
                selected = unique(ano()$DATA)[c(1, 12)]
        )} else if (input$ano == 2021) {
            sliderTextInput(
                inputId = "per",
                label = h4("Selecione o período:"),
                choices = unique(ano()$DATA),
                selected = unique(ano()$DATA)[c(1, 12)]
            )
        }
        
    )
    
    
    mes <- reactive({
        
        if(input$per[1]=="jan" & input$per[2]=="jan"){ano() %>% filter(DATA %in% c("jan"))
        } else if(input$per[1]=="fev" & input$per[2]=="fev"){ano() %>% filter(DATA %in% c("fev"))
        } else if(input$per[1]=="mar" & input$per[2]=="mar"){ano() %>% filter(DATA %in% c("mar"))
        } else if(input$per[1]=="abr" & input$per[2]=="abr"){ano() %>% filter(DATA %in% c("abr"))
        } else if(input$per[1]=="mai" & input$per[2]=="mai"){ano() %>% filter(DATA %in% c("mai"))
        } else if(input$per[1]=="jun" & input$per[2]=="jun"){ano() %>% filter(DATA %in% c("jun"))
        } else if(input$per[1]=="jul" & input$per[2]=="jul"){ano() %>% filter(DATA %in% c("jul"))
        } else if(input$per[1]=="ago" & input$per[2]=="ago"){ano() %>% filter(DATA %in% c("ago"))
        } else if(input$per[1]=="set" & input$per[2]=="set"){ano() %>% filter(DATA %in% c("set"))
        } else if(input$per[1]=="out" & input$per[2]=="out"){ano() %>% filter(DATA %in% c("out"))
        } else if(input$per[1]=="nov" & input$per[2]=="nov"){ano() %>% filter(DATA %in% c("nov"))
        } else if(input$per[1]=="dez" & input$per[2]=="dez"){ano() %>% filter(DATA %in% c("dez"))
        } else if(input$per[1]=="jan" & input$per[2]=="fev"){ano() %>% filter(DATA %in% c("jan","fev"))
        } else if(input$per[1]=="jan" & input$per[2]=="mar"){ano() %>% filter(DATA %in% c("jan","fev","mar"))
        } else if(input$per[1]=="jan" & input$per[2]=="abr"){ano() %>% filter(DATA %in% c("jan","fev","mar","abr"))
        } else if(input$per[1]=="jan" & input$per[2]=="mai"){ano() %>% filter(DATA %in% c("jan","fev","mar","abr","mai"))
        } else if(input$per[1]=="jan" & input$per[2]=="jun"){ano() %>% filter(DATA %in% c("jan","fev","mar","abr","mai", "jun"))
        } else if(input$per[1]=="jan" & input$per[2]=="jul"){ano() %>% filter(DATA %in% c("jan","fev","mar","abr","mai", "jun", "jul"))
        } else if(input$per[1]=="jan" & input$per[2]=="ago"){ano() %>% filter(DATA %in% c("jan","fev","mar","abr","mai", "jun", "jul","ago"))
        } else if(input$per[1]=="jan" & input$per[2]=="set"){ano() %>% filter(DATA %in% c("jan","fev","mar","abr","mai", "jun", "jul","ago","set"))
        } else if(input$per[1]=="jan" & input$per[2]=="out"){ano() %>% filter(DATA %in% c("jan","fev","mar","abr","mai", "jun", "jul","ago","set","out"))
        } else if(input$per[1]=="jan" & input$per[2]=="nov"){ano() %>% filter(DATA %in% c("jan","fev","mar","abr","mai", "jun", "jul","ago","set","out","nov"))
        } else if(input$per[1]=="jan" & input$per[2]=="dez"){ano() %>% filter(DATA %in% c("jan","fev","mar","abr","mai", "jun", "jul","ago","set","out","nov","dez"))    
        } else if(input$per[1]=="fev" & input$per[2]=="mar"){ano() %>% filter(DATA %in% c("fev","mar"))
        } else if(input$per[1]=="fev" & input$per[2]=="abr"){ano() %>% filter(DATA %in% c("fev","mar","abr"))
        } else if(input$per[1]=="fev" & input$per[2]=="mai"){ano() %>% filter(DATA %in% c("fev","mar","abr","mai"))
        } else if(input$per[1]=="fev" & input$per[2]=="jun"){ano() %>% filter(DATA %in% c("fev","mar","abr","mai","jun"))
        } else if(input$per[1]=="fev" & input$per[2]=="jul"){ano() %>% filter(DATA %in% c("fev","mar","abr","mai","jun","jul"))
        } else if(input$per[1]=="fev" & input$per[2]=="ago"){ano() %>% filter(DATA %in% c("fev","mar","abr","mai","jun","jul","ago"))
        } else if(input$per[1]=="fev" & input$per[2]=="set"){ano() %>% filter(DATA %in% c("fev","mar","abr","mai","jun","jul","ago","set"))
        } else if(input$per[1]=="fev" & input$per[2]=="out"){ano() %>% filter(DATA %in% c("fev","mar","abr","mai","jun","jul","ago","set","out"))
        } else if(input$per[1]=="fev" & input$per[2]=="nov"){ano() %>% filter(DATA %in% c("fev","mar","abr","mai","jun","jul","ago","set","out","nov"))
        } else if(input$per[1]=="fev" & input$per[2]=="dez"){ano() %>% filter(DATA %in% c("fev","mar","abr","mai","jun","jul","ago","set","out","nov","dez"))
        } else if(input$per[1]=="mar" & input$per[2]=="abr"){ano() %>% filter(DATA %in% c("mar","abr"))
        } else if(input$per[1]=="mar" & input$per[2]=="mai"){ano() %>% filter(DATA %in% c("mar","abr","mai"))
        } else if(input$per[1]=="mar" & input$per[2]=="jun"){ano() %>% filter(DATA %in% c("mar","abr","mai","jun"))
        } else if(input$per[1]=="mar" & input$per[2]=="jul"){ano() %>% filter(DATA %in% c("mar","abr","mai","jun","jul"))
        } else if(input$per[1]=="mar" & input$per[2]=="ago"){ano() %>% filter(DATA %in% c("mar","abr","mai","jun","jul","ago"))
        } else if(input$per[1]=="mar" & input$per[2]=="set"){ano() %>% filter(DATA %in% c("mar","abr","mai","jun","jul","ago","set"))
        } else if(input$per[1]=="mar" & input$per[2]=="out"){ano() %>% filter(DATA %in% c("mar","abr","mai","jun","jul","ago","set","out"))
        } else if(input$per[1]=="mar" & input$per[2]=="nov"){ano() %>% filter(DATA %in% c("mar","abr","mai","jun","jul","ago","set","out","nov"))
        } else if(input$per[1]=="mar" & input$per[2]=="dez"){ano() %>% filter(DATA %in% c("mar","abr","mai","jun","jul","ago","set","out","nov","dez"))
        } else if(input$per[1]=="abr" & input$per[2]=="mai"){ano() %>% filter(DATA %in% c("abr","mai"))
        } else if(input$per[1]=="abr" & input$per[2]=="jun"){ano() %>% filter(DATA %in% c("abr","mai","jun"))
        } else if(input$per[1]=="abr" & input$per[2]=="jul"){ano() %>% filter(DATA %in% c("abr","mai","jun","jul"))
        } else if(input$per[1]=="abr" & input$per[2]=="ago"){ano() %>% filter(DATA %in% c("abr","mai","jun","jul","ago"))
        } else if(input$per[1]=="abr" & input$per[2]=="set"){ano() %>% filter(DATA %in% c("abr","mai","jun","jul","ago","set"))
        } else if(input$per[1]=="abr" & input$per[2]=="out"){ano() %>% filter(DATA %in% c("abr","mai","jun","jul","ago","set","out"))
        } else if(input$per[1]=="abr" & input$per[2]=="nov"){ano() %>% filter(DATA %in% c("abr","mai","jun","jul","ago","set","out","nov"))
        } else if(input$per[1]=="abr" & input$per[2]=="dez"){ano() %>% filter(DATA %in% c("abr","mai","jun","jul","ago","set","out","nov","dez"))
        } else if(input$per[1]=="mai" & input$per[2]=="jun"){ano() %>% filter(DATA %in% c("mai","jun"))
        } else if(input$per[1]=="mai" & input$per[2]=="jul"){ano() %>% filter(DATA %in% c("mai","jun","jul"))
        } else if(input$per[1]=="mai" & input$per[2]=="ago"){ano() %>% filter(DATA %in% c("mai","jun","jul","ago"))
        } else if(input$per[1]=="mai" & input$per[2]=="set"){ano() %>% filter(DATA %in% c("mai","jun","jul","ago","set"))
        } else if(input$per[1]=="mai" & input$per[2]=="out"){ano() %>% filter(DATA %in% c("mai","jun","jul","ago","set","out"))
        } else if(input$per[1]=="mai" & input$per[2]=="nov"){ano() %>% filter(DATA %in% c("mai","jun","jul","ago","set","out","nov"))
        } else if(input$per[1]=="mai" & input$per[2]=="dez"){ano() %>% filter(DATA %in% c("mai","jun","jul","ago","set","out","nov","dez"))      
        } else if(input$per[1]=="jun" & input$per[2]=="jul"){ano() %>% filter(DATA %in% c("jun","jul"))     
        } else if(input$per[1]=="jun" & input$per[2]=="ago"){ano() %>% filter(DATA %in% c("jun","jul","ago"))    
        } else if(input$per[1]=="jun" & input$per[2]=="set"){ano() %>% filter(DATA %in% c("jun","jul","ago","set"))   
        } else if(input$per[1]=="jun" & input$per[2]=="out"){ano() %>% filter(DATA %in% c("jun","jul","ago","set","out")) 
        } else if(input$per[1]=="jun" & input$per[2]=="nov"){ano() %>% filter(DATA %in% c("jun","jul","ago","set","out","nov"))
        } else if(input$per[1]=="jun" & input$per[2]=="dez"){ano() %>% filter(DATA %in% c("jun","jul","ago","set","out","nov","dez"))
        } else if(input$per[1]=="jul" & input$per[2]=="ago"){ano() %>% filter(DATA %in% c("jul","ago"))
        } else if(input$per[1]=="jul" & input$per[2]=="set"){ano() %>% filter(DATA %in% c("jul","ago","set"))
        } else if(input$per[1]=="jul" & input$per[2]=="out"){ano() %>% filter(DATA %in% c("jul","ago","set","out"))
        } else if(input$per[1]=="jul" & input$per[2]=="nov"){ano() %>% filter(DATA %in% c("jul","ago","set","out","nov"))
        } else if(input$per[1]=="jul" & input$per[2]=="dez"){ano() %>% filter(DATA %in% c("jul","ago","set","out","nov","dez"))
        } else if(input$per[1]=="ago" & input$per[2]=="set"){ano() %>% filter(DATA %in% c("ago","set"))
        } else if(input$per[1]=="ago" & input$per[2]=="out"){ano() %>% filter(DATA %in% c("ago","set","out"))
        } else if(input$per[1]=="ago" & input$per[2]=="nov"){ano() %>% filter(DATA %in% c("ago","set","out","nov"))
        } else if(input$per[1]=="ago" & input$per[2]=="dez"){ano() %>% filter(DATA %in% c("ago","set","out","nov","dez"))
        } else if(input$per[1]=="set" & input$per[2]=="out"){ano() %>% filter(DATA %in% c("set","out"))
        } else if(input$per[1]=="set" & input$per[2]=="nov"){ano() %>% filter(DATA %in% c("set","out","nov"))
        } else if(input$per[1]=="set" & input$per[2]=="dez"){ano() %>% filter(DATA %in% c("set","out","nov","dez"))
        } else if(input$per[1]=="out" & input$per[2]=="nov"){ano() %>% filter(DATA %in% c("out","nov"))
        } else if(input$per[1]=="out" & input$per[2]=="dez"){ano() %>% filter(DATA %in% c("out","nov","dez"))
        } else if(input$per[1]=="nov" & input$per[2]=="dez"){ano() %>% filter(DATA %in% c("nov","dez"))
        }
    
    })
    
    
    output$unidade <- renderUI({
        
        pickerInput("unidade", h4("Selecione a(s) unidade(s) do grupo:"),
                    choices = unique(grupo()$UNIDADE) ,
                    options = list(`actions-box` = TRUE),
                    selected = unique(grupo()$UNIDADE),multiple = T)
        
    })
    
    unidade <- reactive({
        mes() %>% filter(UNIDADE %in% input$unidade)
    
    })
    
    unidade_mes <- reactive(
        
        unidade() %>% filter(DATA == input$per[2])
        
    )
    
    tab <- reactive({
        if (input$indicador == "ACERVO") {
            if (dim(unidade())[1]==0) {} else {
            unidade() %>% select(UNIDADE, as.name(input$indicador), DATA) %>%
            spread(key = DATA, value = as.name(input$indicador)) %>% 
            select(`UNIDADE JUDICIÁRIA` = UNIDADE, as.name(input$per[1]):as.name(input$per[2]))}
        } else { if (dim(unidade())[1]==0) {} else {
            unidade() %>% select(UNIDADE, as.name(input$indicador), DATA) %>%
                spread(key = DATA, value = as.name(input$indicador)) %>%
                mutate(TOTAL = select(., as.name(input$per[1]):as.name(input$per[2])) %>% apply(1, sum, na.rm=TRUE)) %>% 
                arrange(desc(TOTAL)) %>% select(`UNIDADE JUDICIÁRIA` = UNIDADE, as.name(input$per[1]): TOTAL)}}
    })
    
    
    output$data <- renderTable(
        
        tab(), digits = 0, bordered = T, hover = T, striped = T
        
    )
    
    
    output$plotline <- renderPlotly({
        if (dim(unidade())[1]==0) {} else {
        ggplotly(ggplot(data = unidade(), aes(x = DATA, y = get(input$indicador), colour = UNIDADE, text = paste('UNIDADE: ', UNIDADE, '<br>QUANTITATIVO: ', get(input$indicador)))) +
            geom_line(aes(group = UNIDADE)) + geom_point() +
            theme(plot.title = element_text(hjust = 0.5, size=15)) +
            ggtitle(paste("Evolução Mensal", input$indicador, sep = " - ")) +
            xlab("") + ylab("") + theme(legend.position="none"), tooltip = "text", height = 590) %>%
            config(displayModeBar = FALSE)}
    })
    
    soma <- reactive(
        
        unidade() %>%
        group_by(UNIDADE) %>% summarise(Soma=sum(get(input$indicador)))
        
    )
    
    output$plotbar <- renderPlot(
        
        if(input$indicador == "ACERVO"){
            if (dim(unidade())[1]==0) {} 
            else if (length(unique(unidade()$UNIDADE))==1){ }
            else if (input$grupo %in% c(8, 9)) {
                ggplot(data = unidade_mes(), aes(x = reorder(UNIDADE, -get(input$indicador), na.rm = TRUE), y = get(input$indicador))) +
                    geom_col(fill = "dodgerblue") +
                    theme(text = element_text(size=13),
                          axis.text.x = element_text(angle = 90, hjust = 1),
                          plot.title = element_text(hjust = 0.5, size=20)) +
                    ggtitle(paste("Comparativo entre Unidades", input$indicador, sep = " - ")) +
                    xlab(element_blank()) +
                    ylab(element_blank()) +
                    geom_text(aes(label=get(input$indicador)), vjust=1.6, color="white", size=4)}   
            
            else {
            ggplot(data = unidade_mes(), aes(x = reorder(UNIDADE, -get(input$indicador), na.rm = TRUE), y = get(input$indicador))) +
                geom_col(fill = "dodgerblue") +
                theme(text = element_text(size=13),
                      axis.text.x = element_text(angle = 90, hjust = 1),
                      plot.title = element_text(hjust = 0.5, size=20)) +
                ggtitle(paste("Comparativo entre Unidades", input$indicador, sep = " - ")) +
                xlab(element_blank()) +
                ylab(element_blank()) +
                geom_text(aes(label=get(input$indicador)), vjust=1.6, color="white", size=6)}        
            
         }else {
            if (dim(unidade())[1]==0) {}
            else if (length(unique(unidade()$UNIDADE))==1){ }
             else if (input$grupo %in% c(8, 9)) {
                 ggplot(data = soma(), aes(x = reorder(UNIDADE, -Soma, na.rm = TRUE), y = Soma)) +
                     geom_col(fill = "dodgerblue") +
                     theme(text = element_text(size=13),
                           axis.text.x = element_text(angle = 90, hjust = 1),
                           plot.title = element_text(hjust = 0.5, size=20)) +
                     ggtitle(paste("Comparativo entre Unidades", input$indicador, sep = " - ")) +
                     xlab(element_blank()) +
                     ylab(element_blank()) +
                     geom_text(aes(label=Soma), vjust=1.6, color="white", size=4)         
             }
             else {
        ggplot(data = soma(), aes(x = reorder(UNIDADE, -Soma, na.rm = TRUE), y = Soma)) +
            geom_col(fill = "dodgerblue") +
            theme(text = element_text(size=13),
                  axis.text.x = element_text(angle = 90, hjust = 1),
                  plot.title = element_text(hjust = 0.5, size=20)) +
            ggtitle(paste("Comparativo entre Unidades", input$indicador, sep = " - ")) +
            xlab(element_blank()) +
            ylab(element_blank()) +
            geom_text(aes(label=Soma), vjust=1.6, color="white", size=6)}
        }, height = 650
    )

#############################################################################################
# Para a taxa de congestionamento
    
    grupo_taxa <- reactive({
        taxa %>% filter(GRUPO==input$grupo_taxa) 
    }) 
    
    output$periodo_taxa <- renderUI(
        
            sliderTextInput(
                inputId = "per_taxa",
                label = h4("Selecione o período:"), 
                choices = unique(grupo_taxa()$mes_ano)[12:1],
                selected = unique(grupo_taxa()$mes_ano)[c(12, 1)]
            )
        
    )
    
    mes_taxa <- reactive({
      
      if(input$per[1]=="jan/21" & input$per[2]=="jan/21"){ano() %>% filter(DATA %in% c("jan/21"))
      } else if(input$per[1]=="fev/21" & input$per[2]=="fev/21"){ano() %>% filter(DATA %in% c("fev/21"))
      } else if(input$per[1]=="mar/21" & input$per[2]=="mar/21"){ano() %>% filter(DATA %in% c("mar/21"))
      } else if(input$per[1]=="abr/21" & input$per[2]=="abr/21"){ano() %>% filter(DATA %in% c("abr/21"))
      } else if(input$per[1]=="mai/21" & input$per[2]=="mai/21"){ano() %>% filter(DATA %in% c("mai/21"))
      } else if(input$per[1]=="jun/21" & input$per[2]=="jun/21"){ano() %>% filter(DATA %in% c("jun/21"))
      } else if(input$per[1]=="jul/21" & input$per[2]=="jul/21"){ano() %>% filter(DATA %in% c("jul/21"))
      } else if(input$per[1]=="ago/21" & input$per[2]=="ago/21"){ano() %>% filter(DATA %in% c("ago/21"))
      } else if(input$per[1]=="set/21" & input$per[2]=="set/21"){ano() %>% filter(DATA %in% c("set/21"))
      } else if(input$per[1]=="out/21" & input$per[2]=="out/21"){ano() %>% filter(DATA %in% c("out/21"))
      } else if(input$per[1]=="nov/21" & input$per[2]=="nov/21"){ano() %>% filter(DATA %in% c("nov/21"))
      } else if(input$per[1]=="dez/20" & input$per[2]=="dez/20"){ano() %>% filter(DATA %in% c("dez/20"))
      #} else if(input$per_taxa[1]%in%c("nov/20", "dez/20") & input$per_taxa[2]%in%c("nov/20", "dez/20")){grupo_taxa() %>% filter(mes_ano %in% c("nov/20","dez/20"))#
      #} else if(input$per_taxa[1]%in%c("nov/20", "jan/21") & input$per_taxa[2]%in%c("nov/20", "jan/21")){grupo_taxa() %>% filter(mes_ano %in% c("nov/20","dez/20","jan/21"))#
      #} else if(input$per_taxa[1]%in%c("nov/20", "fev/21") & input$per_taxa[2]%in%c("nov/20", "fev/21")){grupo_taxa() %>% filter(mes_ano %in% c("nov/20","dez/20","jan/21","fev/21"))#
      #} else if(input$per_taxa[1]%in%c("nov/20", "mar/21") & input$per_taxa[2]%in%c("nov/20", "mar/21")){grupo_taxa() %>% filter(mes_ano %in% c("nov/20","dez/20","jan/21","fev/21","mar/21"))#
      #} else if(input$per_taxa[1]%in%c("nov/20", "abr/21") & input$per_taxa[2]%in%c("nov/20", "abr/21")){grupo_taxa() %>% filter(mes_ano %in% c("nov/20","dez/20","jan/21","fev/21","mar/21","abr/21"))#
      #} else if(input$per_taxa[1]%in%c("nov/20", "mai/21") & input$per_taxa[2]%in%c("nov/20", "mai/21")){grupo_taxa() %>% filter(mes_ano %in% c("nov/20","dez/20","jan/21","fev/21","mar/21","abr/21","mai/21"))#
      #} else if(input$per_taxa[1]%in%c("nov/20", "jun/21") & input$per_taxa[2]%in%c("nov/20", "jun/21")){grupo_taxa() %>% filter(mes_ano %in% c("nov/20","dez/20","jan/21","fev/21","mar/21","abr/21","mai/21","jun/21"))#
      #} else if(input$per_taxa[1]%in%c("nov/20", "jul/21") & input$per_taxa[2]%in%c("nov/20", "jul/21")){grupo_taxa() %>% filter(mes_ano %in% c("nov/20","dez/20","jan/21","fev/21","mar/21","abr/21","mai/21","jun/21","jul/21"))#
      #} else if(input$per_taxa[1]%in%c("nov/20", "ago/21") & input$per_taxa[2]%in%c("nov/20", "ago/21")){grupo_taxa() %>% filter(mes_ano %in% c("nov/20","dez/20","jan/21","fev/21","mar/21","abr/21","mai/21","jun/21","jul/21","ago/21"))#
      #} else if(input$per_taxa[1]%in%c("nov/20", "set/21") & input$per_taxa[2]%in%c("nov/20", "set/21")){grupo_taxa() %>% filter(mes_ano %in% c("nov/20","dez/20","jan/21","fev/21","mar/21","abr/21","mai/21","jun/21","jul/21","ago/21","set/21"))#
      #} else if(input$per_taxa[1]%in%c("nov/20", "out/21") & input$per_taxa[2]%in%c("nov/20", "out/21")){grupo_taxa() %>% filter(mes_ano %in% c("nov/20","dez/20","jan/21","fev/21","mar/21","abr/21","mai/21","jun/21","jul/21","ago/21","set/21","out/21"))#
      } else if(input$per_taxa[1]%in%c("dez/20", "jan/21") & input$per_taxa[2]%in%c("dez/20", "jan/21")){grupo_taxa() %>% filter(mes_ano %in% c("dez/20","jan/21"))#
      } else if(input$per_taxa[1]%in%c("dez/20", "fev/21") & input$per_taxa[2]%in%c("dez/20", "fev/21")){grupo_taxa() %>% filter(mes_ano %in% c("dez/20","jan/21","fev/21"))#
      } else if(input$per_taxa[1]%in%c("dez/20", "mar/21") & input$per_taxa[2]%in%c("dez/20", "mar/21")){grupo_taxa() %>% filter(mes_ano %in% c("dez/20","jan/21","fev/21","mar/21"))#
      } else if(input$per_taxa[1]%in%c("dez/20", "abr/21") & input$per_taxa[2]%in%c("dez/20", "abr/21")){grupo_taxa() %>% filter(mes_ano %in% c("dez/20","jan/21","fev/21","mar/21","abr/21"))#
      } else if(input$per_taxa[1]%in%c("dez/20", "mai/21") & input$per_taxa[2]%in%c("dez/20", "mai/21")){grupo_taxa() %>% filter(mes_ano %in% c("dez/20","jan/21","fev/21","mar/21","abr/21","mai/21"))#
      } else if(input$per_taxa[1]%in%c("dez/20", "jun/21") & input$per_taxa[2]%in%c("dez/20", "jun/21")){grupo_taxa() %>% filter(mes_ano %in% c("dez/20","jan/21","fev/21","mar/21","abr/21","mai/21","jun/21"))#
      } else if(input$per_taxa[1]%in%c("dez/20", "jul/21") & input$per_taxa[2]%in%c("dez/20", "jul/21")){grupo_taxa() %>% filter(mes_ano %in% c("dez/20","jan/21","fev/21","mar/21","abr/21","mai/21","jun/21","jul/21"))#
      } else if(input$per_taxa[1]%in%c("dez/20", "ago/21") & input$per_taxa[2]%in%c("dez/20", "ago/21")){grupo_taxa() %>% filter(mes_ano %in% c("dez/20","jan/21","fev/21","mar/21","abr/21","mai/21","jun/21","jul/21","ago/21"))#
      } else if(input$per_taxa[1]%in%c("dez/20", "set/21") & input$per_taxa[2]%in%c("dez/20", "set/21")){grupo_taxa() %>% filter(mes_ano %in% c("dez/20","jan/21","fev/21","mar/21","abr/21","mai/21","jun/21","jul/21","ago/21","set/21"))#
      } else if(input$per_taxa[1]%in%c("dez/20", "out/21") & input$per_taxa[2]%in%c("dez/20", "out/21")){grupo_taxa() %>% filter(mes_ano %in% c("dez/20","jan/21","fev/21","mar/21","abr/21","mai/21","jun/21","jul/21","ago/21","set/21","out/21"))#
      } else if(input$per_taxa[1]%in%c("dez/20", "nov/21") & input$per_taxa[2]%in%c("dez/20", "nov/21")){grupo_taxa() %>% filter(mes_ano %in% c("dez/20","jan/21","fev/21","mar/21","abr/21","mai/21","jun/21","jul/21","ago/21","set/21","out/21","nov/21"))#
      } else if(input$per_taxa[1]%in%c("jan/21", "fev/21") & input$per_taxa[2]%in%c("jan/21", "fev/21")){grupo_taxa() %>% filter(mes_ano %in% c("jan/21","fev/21"))#
      } else if(input$per_taxa[1]%in%c("jan/21", "mar/21") & input$per_taxa[2]%in%c("jan/21", "mar/21")){grupo_taxa() %>% filter(mes_ano %in% c("jan/21","fev/21","mar/21"))#
      } else if(input$per_taxa[1]%in%c("jan/21", "abr/21") & input$per_taxa[2]%in%c("jan/21", "abr/21")){grupo_taxa() %>% filter(mes_ano %in% c("jan/21","fev/21","mar/21","abr/21"))#
      } else if(input$per_taxa[1]%in%c("jan/21", "mai/21") & input$per_taxa[2]%in%c("jan/21", "mai/21")){grupo_taxa() %>% filter(mes_ano %in% c("jan/21","fev/21","mar/21","abr/21","mai/21"))#
      } else if(input$per_taxa[1]%in%c("jan/21", "jun/21") & input$per_taxa[2]%in%c("jan/21", "jul/21")){grupo_taxa() %>% filter(mes_ano %in% c("jan/21","fev/21","mar/21","abr/21","mai/21","jun/21"))#
      } else if(input$per_taxa[1]%in%c("jan/21", "jul/21") & input$per_taxa[2]%in%c("jan/21", "jun/21")){grupo_taxa() %>% filter(mes_ano %in% c("jan/21","fev/21","mar/21","abr/21","mai/21","jun/21","jul/21"))#
      } else if(input$per_taxa[1]%in%c("jan/21", "ago/21") & input$per_taxa[2]%in%c("jan/21", "ago/21")){grupo_taxa() %>% filter(mes_ano %in% c("jan/21","fev/21","mar/21","abr/21","mai/21","jun/21","jul/21","ago/21"))#
      } else if(input$per_taxa[1]%in%c("jan/21", "set/21") & input$per_taxa[2]%in%c("jan/21", "set/21")){grupo_taxa() %>% filter(mes_ano %in% c("jan/21","fev/21","mar/21","abr/21","mai/21","jun/21","jul/21","ago/21","set/21"))#
      } else if(input$per_taxa[1]%in%c("jan/21", "out/21") & input$per_taxa[2]%in%c("jan/21", "out/21")){grupo_taxa() %>% filter(mes_ano %in% c("jan/21","fev/21","mar/21","abr/21","mai/21","jun/21","jul/21","ago/21","set/21","out/21"))#
      } else if(input$per_taxa[1]%in%c("jan/21", "nov/21") & input$per_taxa[2]%in%c("jan/21", "nov/21")){grupo_taxa() %>% filter(mes_ano %in% c("jan/21","fev/21","mar/21","abr/21","mai/21","jun/21","jul/21","ago/21","set/21","out/21","nov/21"))#
      } else if(input$per_taxa[1]%in%c("fev/21", "mar/21") & input$per_taxa[2]%in%c("fev/21", "mar/21")){grupo_taxa() %>% filter(mes_ano %in% c("fev/21","mar/21"))#
      } else if(input$per_taxa[1]%in%c("fev/21", "abr/21") & input$per_taxa[2]%in%c("fev/21", "abr/21")){grupo_taxa() %>% filter(mes_ano %in% c("fev/21","mar/21","abr/21"))#
      } else if(input$per_taxa[1]%in%c("fev/21", "mai/21") & input$per_taxa[2]%in%c("fev/21", "mai/21")){grupo_taxa() %>% filter(mes_ano %in% c("fev/21","mar/21","abr/21","mai/21"))#
      } else if(input$per_taxa[1]%in%c("fev/21", "jun/21") & input$per_taxa[2]%in%c("fev/21", "jun/21")){grupo_taxa() %>% filter(mes_ano %in% c("fev/21","mar/21","abr/21","mai/21","jun/21"))#
      } else if(input$per_taxa[1]%in%c("fev/21", "jul/21") & input$per_taxa[2]%in%c("fev/21", "jul/21")){grupo_taxa() %>% filter(mes_ano %in% c("fev/21","mar/21","abr/21","mai/21","jun/21","jul/21"))#
      } else if(input$per_taxa[1]%in%c("fev/21", "ago/21") & input$per_taxa[2]%in%c("fev/21", "ago/21")){grupo_taxa() %>% filter(mes_ano %in% c("fev/21","mar/21","abr/21","mai/21","jun/21","jul/21","ago/21"))#
      } else if(input$per_taxa[1]%in%c("fev/21", "set/21") & input$per_taxa[2]%in%c("fev/21", "set/21")){grupo_taxa() %>% filter(mes_ano %in% c("fev/21","mar/21","abr/21","mai/21","jun/21","jul/21","ago/21","set/21"))#
      } else if(input$per_taxa[1]%in%c("fev/21", "out/21") & input$per_taxa[2]%in%c("fev/21", "out/21")){grupo_taxa() %>% filter(mes_ano %in% c("fev/21","mar/21","abr/21","mai/21","jun/21","jul/21","ago/21","set/21","out/21"))#
      } else if(input$per_taxa[1]%in%c("fev/21", "nov/21") & input$per_taxa[2]%in%c("fev/21", "nov/21")){grupo_taxa() %>% filter(mes_ano %in% c("fev/21","mar/21","abr/21","mai/21","jun/21","jul/21","ago/21","set/21","out/21","nov/21"))#
      } else if(input$per_taxa[1]%in%c("mar/21", "abr/21") & input$per_taxa[2]%in%c("mar/21", "abr/21")){grupo_taxa() %>% filter(mes_ano %in% c("mar/21","abr/21"))#
      } else if(input$per_taxa[1]%in%c("mar/21", "mai/21") & input$per_taxa[2]%in%c("mar/21", "mai/21")){grupo_taxa() %>% filter(mes_ano %in% c("mar/21","abr/21","mai/21"))#
      } else if(input$per_taxa[1]%in%c("mar/21", "jun/21") & input$per_taxa[2]%in%c("mar/21", "jun/21")){grupo_taxa() %>% filter(mes_ano %in% c("mar/21","abr/21","mai/21","jun/21"))#
      } else if(input$per_taxa[1]%in%c("mar/21", "jul/21") & input$per_taxa[2]%in%c("mar/21", "jul/21")){grupo_taxa() %>% filter(mes_ano %in% c("mar/21","abr/21","mai/21","jun/21","jul/21"))#
      } else if(input$per_taxa[1]%in%c("mar/21", "ago/21") & input$per_taxa[2]%in%c("mar/21", "ago/21")){grupo_taxa() %>% filter(mes_ano %in% c("mar/21","abr/21","mai/21","jun/21","jul/21","ago/21"))#
      } else if(input$per_taxa[1]%in%c("mar/21", "set/21") & input$per_taxa[2]%in%c("mar/21", "set/21")){grupo_taxa() %>% filter(mes_ano %in% c("mar/21","abr/21","mai/21","jun/21","jul/21","ago/21","set/21"))#
      } else if(input$per_taxa[1]%in%c("mar/21", "out/21") & input$per_taxa[2]%in%c("mar/21", "out/21")){grupo_taxa() %>% filter(mes_ano %in% c("mar/21","abr/21","mai/21","jun/21","jul/21","ago/21","set/21","out/21"))#
      } else if(input$per_taxa[1]%in%c("mar/21", "nov/21") & input$per_taxa[2]%in%c("mar/21", "nov/21")){grupo_taxa() %>% filter(mes_ano %in% c("mar/21","abr/21","mai/21","jun/21","jul/21","ago/21","set/21","out/21","nov/21"))#
      } else if(input$per_taxa[1]%in%c("abr/21", "mai/21") & input$per_taxa[2]%in%c("abr/21", "mai/21")){grupo_taxa() %>% filter(mes_ano %in% c("abr/21","mai/21"))#
      } else if(input$per_taxa[1]%in%c("abr/21", "jun/21") & input$per_taxa[2]%in%c("abr/21", "jun/21")){grupo_taxa() %>% filter(mes_ano %in% c("abr/21","mai/21","jun/21"))#
      } else if(input$per_taxa[1]%in%c("abr/21", "jul/21") & input$per_taxa[2]%in%c("abr/21", "jul/21")){grupo_taxa() %>% filter(mes_ano %in% c("abr/21","mai/21","jun/21","jul/21"))#
      } else if(input$per_taxa[1]%in%c("abr/21", "ago/21") & input$per_taxa[2]%in%c("abr/21", "ago/21")){grupo_taxa() %>% filter(mes_ano %in% c("abr/21","mai/21","jun/21","jul/21","ago/21"))#
      } else if(input$per_taxa[1]%in%c("abr/21", "set/21") & input$per_taxa[2]%in%c("abr/21", "set/21")){grupo_taxa() %>% filter(mes_ano %in% c("abr/21","mai/21","jun/21","jul/21","ago/21","set/21"))#
      } else if(input$per_taxa[1]%in%c("abr/21", "out/21") & input$per_taxa[2]%in%c("abr/21", "out/21")){grupo_taxa() %>% filter(mes_ano %in% c("abr/21","mai/21","jun/21","jul/21","ago/21","set/21","out/21"))#
      } else if(input$per_taxa[1]%in%c("abr/21", "nov/21") & input$per_taxa[2]%in%c("abr/21", "nov/21")){grupo_taxa() %>% filter(mes_ano %in% c("abr/21","mai/21","jun/21","jul/21","ago/21","set/21","out/21","nov/21"))#
      } else if(input$per_taxa[1]%in%c("mai/21", "jun/21") & input$per_taxa[2]%in%c("mai/21", "jun/21")){grupo_taxa() %>% filter(mes_ano %in% c("mai/21","jun/21"))#
      } else if(input$per_taxa[1]%in%c("mai/21", "jul/21") & input$per_taxa[2]%in%c("mai/21", "jul/21")){grupo_taxa() %>% filter(mes_ano %in% c("mai/21","jun/21","jul/21"))#
      } else if(input$per_taxa[1]%in%c("mai/21", "ago/21") & input$per_taxa[2]%in%c("mai/21", "ago/21")){grupo_taxa() %>% filter(mes_ano %in% c("mai/21","jun/21","jul/21","ago/21"))#
      } else if(input$per_taxa[1]%in%c("mai/21", "set/21") & input$per_taxa[2]%in%c("mai/21", "set/21")){grupo_taxa() %>% filter(mes_ano %in% c("mai/21","jun/21","jul/21","ago/21","set/21"))#
      } else if(input$per_taxa[1]%in%c("mai/21", "out/21") & input$per_taxa[2]%in%c("mai/21", "out/21")){grupo_taxa() %>% filter(mes_ano %in% c("mai/21","jun/21","jul/21","ago/21","set/21","out/21"))#
      } else if(input$per_taxa[1]%in%c("mai/21", "nov/21") & input$per_taxa[2]%in%c("mai/21", "nov/21")){grupo_taxa() %>% filter(mes_ano %in% c("mai/21","jun/21","jul/21","ago/21","set/21","out/21","nov/21"))#
      } else if(input$per_taxa[1]%in%c("jun/21", "jul/21") & input$per_taxa[2]%in%c("jun/21", "jul/21")){grupo_taxa() %>% filter(mes_ano %in% c("jun/21","jul/21"))#
      } else if(input$per_taxa[1]%in%c("jun/21", "ago/21") & input$per_taxa[2]%in%c("jun/21", "ago/21")){grupo_taxa() %>% filter(mes_ano %in% c("jun/21","jul/21","ago/21"))#
      } else if(input$per_taxa[1]%in%c("jun/21", "set/21") & input$per_taxa[2]%in%c("jun/21", "set/21")){grupo_taxa() %>% filter(mes_ano %in% c("jun/21","jul/21","ago/21","set/21"))#
      } else if(input$per_taxa[1]%in%c("jun/21", "out/21") & input$per_taxa[2]%in%c("jun/21", "out/21")){grupo_taxa() %>% filter(mes_ano %in% c("jun/21","jul/21","ago/21","set/21","out/21"))#
      } else if(input$per_taxa[1]%in%c("jun/21", "nov/21") & input$per_taxa[2]%in%c("jun/21", "nov/21")){grupo_taxa() %>% filter(mes_ano %in% c("jun/21","jul/21","ago/21","set/21","out/21","nov/21"))#
      } else if(input$per_taxa[1]%in%c("jul/21", "ago/21") & input$per_taxa[2]%in%c("jul/21", "ago/21")){grupo_taxa() %>% filter(mes_ano %in% c("jul/21","ago/21"))#
      } else if(input$per_taxa[1]%in%c("jul/21", "set/21") & input$per_taxa[2]%in%c("jul/21", "set/21")){grupo_taxa() %>% filter(mes_ano %in% c("jul/21","ago/21","set/21"))#
      } else if(input$per_taxa[1]%in%c("jul/21", "out/21") & input$per_taxa[2]%in%c("jul/21", "out/21")){grupo_taxa() %>% filter(mes_ano %in% c("jul/21","ago/21","set/21","out/21"))#
      } else if(input$per_taxa[1]%in%c("jul/21", "nov/21") & input$per_taxa[2]%in%c("jul/21", "nov/21")){grupo_taxa() %>% filter(mes_ano %in% c("jul/21","ago/21","set/21","out/21","nov/21"))#
      } else if(input$per_taxa[1]%in%c("ago/21", "set/21") & input$per_taxa[2]%in%c("ago/21", "set/21")){grupo_taxa() %>% filter(mes_ano %in% c("ago/21","set/21"))#
      } else if(input$per_taxa[1]%in%c("ago/21", "out/21") & input$per_taxa[2]%in%c("ago/21", "out/21")){grupo_taxa() %>% filter(mes_ano %in% c("ago/21","set/21","out/21"))#
      } else if(input$per_taxa[1]%in%c("ago/21", "nov/21") & input$per_taxa[2]%in%c("ago/21", "nov/21")){grupo_taxa() %>% filter(mes_ano %in% c("ago/21","set/21","out/21","nov/21"))#
      } else if(input$per_taxa[1]%in%c("set/21", "out/21") & input$per_taxa[2]%in%c("set/21", "out/21")){grupo_taxa() %>% filter(mes_ano %in% c("set/21","out/21"))#
      } else if(input$per_taxa[1]%in%c("set/21", "nov/21") & input$per_taxa[2]%in%c("set/21", "nov/21")){grupo_taxa() %>% filter(mes_ano %in% c("set/21","out/21","nov/21"))#
      } else if(input$per_taxa[1]%in%c("out/21", "nov/21") & input$per_taxa[2]%in%c("out/21", "nov/21")){grupo_taxa() %>% filter(mes_ano %in% c("out/21","nov/21"))#
      }
    })
    
    output$unidade_taxa <- renderUI({
        
        pickerInput("unidade_taxa", h4("Selecione a(s) unidade(s) do grupo:"),
                    choices = unique(grupo_taxa()$UNIDADE) ,
                    options = list(`actions-box` = TRUE),
                    selected = unique(grupo_taxa()$UNIDADE),multiple = T)
        
    })
    
    unidade_taxa <- reactive({
        mes_taxa() %>% filter(UNIDADE %in% input$unidade_taxa)
        
    })
    
    unidade_taxa_mes <- reactive(
        
        unidade_taxa() %>% filter(mes_ano == input$per_taxa[2])
        
    )
    
    tab_taxa <- reactive({
        
        if (dim(unidade_taxa())[1]==0){
            
        } else if (length(unique(unidade_taxa()$UNIDADE))==1) {
                unidade_taxa() %>% select(UNIDADE, `TAXA LÍQUIDA`, mes_ano) %>%
                spread(key = mes_ano, value = `TAXA LÍQUIDA`) %>% 
                apply(2, paste, "%", sep = "") %>%
                as.data.frame() %>% 
                t() %>% as.data.frame() %>%
                mutate(UNIDADE = str_sub(UNIDADE, end = -2))
        } else {unidade_taxa() %>% select(UNIDADE, `TAXA LÍQUIDA`, mes_ano) %>%
                spread(key = mes_ano, value = `TAXA LÍQUIDA`) %>%
                apply(2, paste, "%", sep = "") %>%
                as.data.frame() %>%
                mutate(UNIDADE = str_sub(UNIDADE, end = -2))}
        
    })
    
    
    output$data_taxa <- renderTable(
        
        tab_taxa(), digits = 2, bordered = T, hover = T, striped = T
        
    )
    
    
    output$plotline_taxa <- renderPlotly({
        if (dim(unidade_taxa())[1]==0) {} else {
        ggplotly(ggplot(data = unidade_taxa(), aes(x = mes_ano, y = `TAXA LÍQUIDA`, colour = UNIDADE, text = paste('UNIDADE: ', UNIDADE, '<br>TAXA: ', paste(`TAXA LÍQUIDA`, "%", sep='')))) +
                     geom_line(aes(group = UNIDADE)) + geom_point() +
                     theme(plot.title = element_text(hjust = 0.5, size=15)) +
                     ggtitle("Evolução Mensal da Taxa de Congestionamento") +
                     xlab("") + ylab("Percentual %") + scale_y_continuous(limits = c(0, 100)) +
                     theme(legend.position="none"), tooltip = 'text', height = 590) %>%
            config(displayModeBar = FALSE)}
    })
    
    
    
    output$plotbar_taxa <- renderPlot(
        if (dim(unidade_taxa())[1]==0) {}
        else if (length(unique(unidade_taxa()$UNIDADE))==1){ }
        else if (input$grupo_taxa %in% c(8, 9, 10, 31)) {
            ggplot(data = unidade_taxa_mes(), aes(x = reorder(UNIDADE, -`TAXA LÍQUIDA`, na.rm = TRUE), y = `TAXA LÍQUIDA`)) +
                geom_col(fill = "dodgerblue") +
                theme(text = element_text(size=13),
                      axis.text.x = element_text(angle = 90, hjust = 1),
                      plot.title = element_text(hjust = 0.5, size=20)) +
                ggtitle("Comparativo entre Unidades - Taxa de Congestionamento") +
                xlab(element_blank()) + scale_y_continuous(limits = c(0, 100)) +
                ylab("Percentual %") +
                geom_text(aes(label=paste(`TAXA LÍQUIDA`,"%",sep="")), vjust=1.6, color="white",size=3.3)  
        }
        else {
        ggplot(data = unidade_taxa_mes(), aes(x = reorder(UNIDADE, -`TAXA LÍQUIDA`, na.rm = TRUE), y = `TAXA LÍQUIDA`)) +
            geom_col(fill = "dodgerblue") +
            theme(text = element_text(size=13),
                  axis.text.x = element_text(angle = 90, hjust = 1),
                  plot.title = element_text(hjust = 0.5, size=20)) +
            ggtitle("Comparativo entre Unidades - Taxa de Congestionamento") +
            xlab(element_blank()) + scale_y_continuous(limits = c(0, 100)) +
            ylab("Percentual %") +
            geom_text(aes(label=paste(`TAXA LÍQUIDA`,"%",sep="")), vjust=1.6, color="white",size=6)}, height = 650
    )
    
###########################################################################################
# Para os distribuídos
    
    ano_dist <- reactive({
        distribuidos %>% filter(`ANO DE REFERÊNCIA`==input$ano_dist) 
    })
    
    ano_fluxo <- reactive({
        fluxo %>% filter(ANO==input$ano_fluxo) 
    })
    
    output$periodo_dist <- renderUI(
        if (input$ano_dist == 2019){
            sliderTextInput(
                inputId = "per_dist",
                label = h4("Selecione o período:"), 
                choices = unique(ano_dist()$Mes),
                selected = unique(ano_dist()$Mes)[c(1, 12)]
            )} else if (input$ano_dist == 2020){
                sliderTextInput(
                    inputId = "per_dist",
                    label = h4("Selecione o período:"), 
                    choices = unique(ano_dist()$Mes)[1:12],
                    selected = unique(ano_dist()$Mes)[c(1,12)]
                )
            } else if (input$ano_dist == 2021){
              sliderTextInput(
                inputId = "per_dist",
                label = h4("Selecione o período:"), 
                choices = unique(ano_dist()$Mes)[1:12],
                selected = unique(ano_dist()$Mes)[c(1,12)]
              )
            }
        
    )
    
    output$periodo_fluxo <- renderUI(
        if (input$ano_fluxo == 2019){
            sliderTextInput(
                inputId = "per_fluxo",
                label = h4("Selecione o período:"), 
                choices = unique(ano_fluxo()$Mes),
                selected = unique(ano_fluxo()$Mes)[c(1, 12)]
            )}  else if(input$ano_fluxo == 2020) {
                sliderTextInput(
                    inputId = "per_fluxo",
                    label = h4("Selecione o período:"), 
                    choices = unique(ano_fluxo()$Mes)[1:12],
                    selected = unique(ano_fluxo()$Mes)[c(1,12)]
                )
            }  else if(input$ano_fluxo == 2021) {
              sliderTextInput(
                inputId = "per_fluxo",
                label = h4("Selecione o período:"), 
                choices = unique(ano_fluxo()$Mes)[1:12],
                selected = unique(ano_fluxo()$Mes)[c(1,12)]
              )
            }
        
    )
    
    
    mes_dist <- reactive({
        
        if(input$per_dist[1]=="jan" & input$per_dist[2]=="jan"){ano_dist() %>% filter(Mes %in% c("jan"))
        } else if(input$per_dist[1]=="fev" & input$per_dist[2]=="fev"){ano_dist() %>% filter(Mes %in% c("fev"))
        } else if(input$per_dist[1]=="mar" & input$per_dist[2]=="mar"){ano_dist() %>% filter(Mes %in% c("mar"))
        } else if(input$per_dist[1]=="abr" & input$per_dist[2]=="abr"){ano_dist() %>% filter(Mes %in% c("abr"))
        } else if(input$per_dist[1]=="mai" & input$per_dist[2]=="mai"){ano_dist() %>% filter(Mes %in% c("mai"))
        } else if(input$per_dist[1]=="jun" & input$per_dist[2]=="jun"){ano_dist() %>% filter(Mes %in% c("jun"))
        } else if(input$per_dist[1]=="jul" & input$per_dist[2]=="jul"){ano_dist() %>% filter(Mes %in% c("jul"))
        } else if(input$per_dist[1]=="ago" & input$per_dist[2]=="ago"){ano_dist() %>% filter(Mes %in% c("ago"))
        } else if(input$per_dist[1]=="set" & input$per_dist[2]=="set"){ano_dist() %>% filter(Mes %in% c("set"))
        } else if(input$per_dist[1]=="out" & input$per_dist[2]=="out"){ano_dist() %>% filter(Mes %in% c("out"))
        } else if(input$per_dist[1]=="nov" & input$per_dist[2]=="nov"){ano_dist() %>% filter(Mes %in% c("nov"))
        } else if(input$per_dist[1]=="dez" & input$per_dist[2]=="dez"){ano_dist() %>% filter(Mes %in% c("dez"))
        } else if(input$per_dist[1]%in%c("jan", "fev") & input$per_dist[2]%in%c("jan", "fev")){ano_dist() %>% filter(Mes %in% c("jan","fev"))
        } else if(input$per_dist[1]%in%c("jan", "mar") & input$per_dist[2]%in%c("jan", "mar")){ano_dist() %>% filter(Mes %in% c("jan","fev","mar"))
        } else if(input$per_dist[1]%in%c("jan", "abr") & input$per_dist[2]%in%c("jan", "abr")){ano_dist() %>% filter(Mes %in% c("jan","fev","mar","abr"))
        } else if(input$per_dist[1]%in%c("jan", "mai") & input$per_dist[2]%in%c("jan", "mai")){ano_dist() %>% filter(Mes %in% c("jan","fev","mar","abr","mai"))
        } else if(input$per_dist[1]%in%c("jan", "jun") & input$per_dist[2]%in%c("jan", "jun")){ano_dist() %>% filter(Mes %in% c("jan","fev","mar","abr","mai", "jun"))
        } else if(input$per_dist[1]%in%c("jan", "jul") & input$per_dist[2]%in%c("jan", "jul")){ano_dist() %>% filter(Mes %in% c("jan","fev","mar","abr","mai", "jun", "jul"))
        } else if(input$per_dist[1]%in%c("jan", "ago") & input$per_dist[2]%in%c("jan", "ago")){ano_dist() %>% filter(Mes %in% c("jan","fev","mar","abr","mai", "jun", "jul","ago"))
        } else if(input$per_dist[1]%in%c("jan", "set") & input$per_dist[2]%in%c("jan", "set")){ano_dist() %>% filter(Mes %in% c("jan","fev","mar","abr","mai", "jun", "jul","ago","set"))
        } else if(input$per_dist[1]%in%c("jan", "out") & input$per_dist[2]%in%c("jan", "out")){ano_dist() %>% filter(Mes %in% c("jan","fev","mar","abr","mai", "jun", "jul","ago","set","out"))
        } else if(input$per_dist[1]%in%c("jan", "nov") & input$per_dist[2]%in%c("jan", "nov")){ano_dist() %>% filter(Mes %in% c("jan","fev","mar","abr","mai", "jun", "jul","ago","set","out","nov"))
        } else if(input$per_dist[1]%in%c("jan", "dez") & input$per_dist[2]%in%c("jan", "dez")){ano_dist() %>% filter(Mes %in% c("jan","fev","mar","abr","mai", "jun", "jul","ago","set","out","nov","dez"))    
        } else if(input$per_dist[1]%in%c("fev", "mar") & input$per_dist[2]%in%c("fev", "mar")){ano_dist() %>% filter(Mes %in% c("fev","mar"))
        } else if(input$per_dist[1]%in%c("fev", "abr") & input$per_dist[2]%in%c("fev", "abr")){ano_dist() %>% filter(Mes %in% c("fev","mar","abr"))
        } else if(input$per_dist[1]%in%c("fev", "mai") & input$per_dist[2]%in%c("fev", "mai")){ano_dist() %>% filter(Mes %in% c("fev","mar","abr","mai"))
        } else if(input$per_dist[1]%in%c("fev", "jun") & input$per_dist[2]%in%c("fev", "jun")){ano_dist() %>% filter(Mes %in% c("fev","mar","abr","mai","jun"))
        } else if(input$per_dist[1]%in%c("fev", "jul") & input$per_dist[2]%in%c("fev", "jul")){ano_dist() %>% filter(Mes %in% c("fev","mar","abr","mai","jun","jul"))
        } else if(input$per_dist[1]%in%c("fev", "ago") & input$per_dist[2]%in%c("fev", "ago")){ano_dist() %>% filter(Mes %in% c("fev","mar","abr","mai","jun","jul","ago"))
        } else if(input$per_dist[1]%in%c("fev", "set") & input$per_dist[2]%in%c("fev", "set")){ano_dist() %>% filter(Mes %in% c("fev","mar","abr","mai","jun","jul","ago","set"))
        } else if(input$per_dist[1]%in%c("fev", "out") & input$per_dist[2]%in%c("fev", "out")){ano_dist() %>% filter(Mes %in% c("fev","mar","abr","mai","jun","jul","ago","set","out"))
        } else if(input$per_dist[1]%in%c("fev", "nov") & input$per_dist[2]%in%c("fev", "nov")){ano_dist() %>% filter(Mes %in% c("fev","mar","abr","mai","jun","jul","ago","set","out","nov"))
        } else if(input$per_dist[1]%in%c("fev", "dez") & input$per_dist[2]%in%c("fev", "dez")){ano_dist() %>% filter(Mes %in% c("fev","mar","abr","mai","jun","jul","ago","set","out","nov","dez"))
        } else if(input$per_dist[1]%in%c("mar", "abr") & input$per_dist[2]%in%c("mar", "abr")){ano_dist() %>% filter(Mes %in% c("mar","abr"))
        } else if(input$per_dist[1]%in%c("mar", "mai") & input$per_dist[2]%in%c("mar", "mai")){ano_dist() %>% filter(Mes %in% c("mar","abr","mai"))
        } else if(input$per_dist[1]%in%c("mar", "jun") & input$per_dist[2]%in%c("mar", "jun")){ano_dist() %>% filter(Mes %in% c("mar","abr","mai","jun"))
        } else if(input$per_dist[1]%in%c("mar", "jul") & input$per_dist[2]%in%c("mar", "jul")){ano_dist() %>% filter(Mes %in% c("mar","abr","mai","jun","jul"))
        } else if(input$per_dist[1]%in%c("mar", "ago") & input$per_dist[2]%in%c("mar", "ago")){ano_dist() %>% filter(Mes %in% c("mar","abr","mai","jun","jul","ago"))
        } else if(input$per_dist[1]%in%c("mar", "set") & input$per_dist[2]%in%c("mar", "set")){ano_dist() %>% filter(Mes %in% c("mar","abr","mai","jun","jul","ago","set"))
        } else if(input$per_dist[1]%in%c("mar", "out") & input$per_dist[2]%in%c("mar", "out")){ano_dist() %>% filter(Mes %in% c("mar","abr","mai","jun","jul","ago","set","out"))
        } else if(input$per_dist[1]%in%c("mar", "nov") & input$per_dist[2]%in%c("mar", "nov")){ano_dist() %>% filter(Mes %in% c("mar","abr","mai","jun","jul","ago","set","out","nov"))
        } else if(input$per_dist[1]%in%c("mar", "dez") & input$per_dist[2]%in%c("mar", "dez")){ano_dist() %>% filter(Mes %in% c("mar","abr","mai","jun","jul","ago","set","out","nov","dez"))
        } else if(input$per_dist[1]%in%c("abr", "mai") & input$per_dist[2]%in%c("abr", "mai")){ano_dist() %>% filter(Mes %in% c("abr","mai"))
        } else if(input$per_dist[1]%in%c("abr", "jun") & input$per_dist[2]%in%c("abr", "jun")){ano_dist() %>% filter(Mes %in% c("abr","mai","jun"))
        } else if(input$per_dist[1]%in%c("abr", "jul") & input$per_dist[2]%in%c("abr", "jul")){ano_dist() %>% filter(Mes %in% c("abr","mai","jun","jul"))
        } else if(input$per_dist[1]%in%c("abr", "ago") & input$per_dist[2]%in%c("abr", "ago")){ano_dist() %>% filter(Mes %in% c("abr","mai","jun","jul","ago"))
        } else if(input$per_dist[1]%in%c("abr", "set") & input$per_dist[2]%in%c("abr", "set")){ano_dist() %>% filter(Mes %in% c("abr","mai","jun","jul","ago","set"))
        } else if(input$per_dist[1]%in%c("abr", "out") & input$per_dist[2]%in%c("abr", "out")){ano_dist() %>% filter(Mes %in% c("abr","mai","jun","jul","ago","set","out"))
        } else if(input$per_dist[1]%in%c("abr", "nov") & input$per_dist[2]%in%c("abr", "nov")){ano_dist() %>% filter(Mes %in% c("abr","mai","jun","jul","ago","set","out","nov"))
        } else if(input$per_dist[1]%in%c("abr", "dez") & input$per_dist[2]%in%c("abr", "dez")){ano_dist() %>% filter(Mes %in% c("abr","mai","jun","jul","ago","set","out","nov","dez"))
        } else if(input$per_dist[1]%in%c("mai", "jun") & input$per_dist[2]%in%c("mai", "jun")){ano_dist() %>% filter(Mes %in% c("mai","jun"))
        } else if(input$per_dist[1]%in%c("mai", "jul") & input$per_dist[2]%in%c("mai", "jul")){ano_dist() %>% filter(Mes %in% c("mai","jun","jul"))
        } else if(input$per_dist[1]%in%c("mai", "ago") & input$per_dist[2]%in%c("mai", "ago")){ano_dist() %>% filter(Mes %in% c("mai","jun","jul","ago"))
        } else if(input$per_dist[1]%in%c("mai", "set") & input$per_dist[2]%in%c("mai", "set")){ano_dist() %>% filter(Mes %in% c("mai","jun","jul","ago","set"))
        } else if(input$per_dist[1]%in%c("mai", "out") & input$per_dist[2]%in%c("mai", "out")){ano_dist() %>% filter(Mes %in% c("mai","jun","jul","ago","set","out"))
        } else if(input$per_dist[1]%in%c("mai", "nov") & input$per_dist[2]%in%c("mai", "nov")){ano_dist() %>% filter(Mes %in% c("mai","jun","jul","ago","set","out","nov"))
        } else if(input$per_dist[1]%in%c("mai", "dez") & input$per_dist[2]%in%c("mai", "dez")){ano_dist() %>% filter(Mes %in% c("mai","jun","jul","ago","set","out","nov","dez"))      
        } else if(input$per_dist[1]%in%c("jun", "jul") & input$per_dist[2]%in%c("jun", "jul")){ano_dist() %>% filter(Mes %in% c("jun","jul"))     
        } else if(input$per_dist[1]%in%c("jun", "ago") & input$per_dist[2]%in%c("jun", "ago")){ano_dist() %>% filter(Mes %in% c("jun","jul","ago"))    
        } else if(input$per_dist[1]%in%c("jun", "set") & input$per_dist[2]%in%c("jun", "set")){ano_dist() %>% filter(Mes %in% c("jun","jul","ago","set"))   
        } else if(input$per_dist[1]%in%c("jun", "out") & input$per_dist[2]%in%c("jun", "out")){ano_dist() %>% filter(Mes %in% c("jun","jul","ago","set","out")) 
        } else if(input$per_dist[1]%in%c("jun", "nov") & input$per_dist[2]%in%c("jun", "nov")){ano_dist() %>% filter(Mes %in% c("jun","jul","ago","set","out","nov"))
        } else if(input$per_dist[1]%in%c("jun", "dez") & input$per_dist[2]%in%c("jun", "dez")){ano_dist() %>% filter(Mes %in% c("jun","jul","ago","set","out","nov","dez"))
        } else if(input$per_dist[1]%in%c("jul", "ago") & input$per_dist[2]%in%c("jul", "ago")){ano_dist() %>% filter(Mes %in% c("jul","ago"))
        } else if(input$per_dist[1]%in%c("jul", "set") & input$per_dist[2]%in%c("jul", "set")){ano_dist() %>% filter(Mes %in% c("jul","ago","set"))
        } else if(input$per_dist[1]%in%c("jul", "out") & input$per_dist[2]%in%c("jul", "out")){ano_dist() %>% filter(Mes %in% c("jul","ago","set","out"))
        } else if(input$per_dist[1]%in%c("jul", "nov") & input$per_dist[2]%in%c("jul", "nov")){ano_dist() %>% filter(Mes %in% c("jul","ago","set","out","nov"))
        } else if(input$per_dist[1]%in%c("jul", "dez") & input$per_dist[2]%in%c("jul", "dez")){ano_dist() %>% filter(Mes %in% c("jul","ago","set","out","nov","dez"))
        } else if(input$per_dist[1]%in%c("ago", "set") & input$per_dist[2]%in%c("ago", "set")){ano_dist() %>% filter(Mes %in% c("ago","set"))
        } else if(input$per_dist[1]%in%c("ago", "out") & input$per_dist[2]%in%c("ago", "out")){ano_dist() %>% filter(Mes %in% c("ago","set","out"))
        } else if(input$per_dist[1]%in%c("ago", "nov") & input$per_dist[2]%in%c("ago", "nov")){ano_dist() %>% filter(Mes %in% c("ago","set","out","nov"))
        } else if(input$per_dist[1]%in%c("ago", "dez") & input$per_dist[2]%in%c("ago", "dez")){ano_dist() %>% filter(Mes %in% c("ago","set","out","nov","dez"))
        } else if(input$per_dist[1]%in%c("set", "out") & input$per_dist[2]%in%c("set", "out")){ano_dist() %>% filter(Mes %in% c("set","out"))
        } else if(input$per_dist[1]%in%c("set", "nov") & input$per_dist[2]%in%c("set", "nov")){ano_dist() %>% filter(Mes %in% c("set","out","nov"))
        } else if(input$per_dist[1]%in%c("set", "dez") & input$per_dist[2]%in%c("set", "dez")){ano_dist() %>% filter(Mes %in% c("set","out","nov","dez"))
        } else if(input$per_dist[1]%in%c("out", "nov") & input$per_dist[2]%in%c("out", "nov")){ano_dist() %>% filter(Mes %in% c("out","nov"))
        } else if(input$per_dist[1]%in%c("out", "dez") & input$per_dist[2]%in%c("out", "dez")){ano_dist() %>% filter(Mes %in% c("out","nov","dez"))
        } else if(input$per_dist[1]%in%c("nov", "dez") & input$per_dist[2]%in%c("nov", "dez")){ano_dist() %>% filter(Mes %in% c("nov","dez"))
        }
        
    })
    
    
    mes_fluxo <- reactive({
        
        if(input$per_fluxo[1]=="jan" & input$per_fluxo[2]=="jan"){ano_fluxo() %>% filter(Mes %in% c("jan"))
        } else if(input$per_fluxo[1]=="fev" & input$per_fluxo[2]=="fev"){ano_fluxo() %>% filter(Mes %in% c("fev"))
        } else if(input$per_fluxo[1]=="mar" & input$per_fluxo[2]=="mar"){ano_fluxo() %>% filter(Mes %in% c("mar"))
        } else if(input$per_fluxo[1]=="abr" & input$per_fluxo[2]=="abr"){ano_fluxo() %>% filter(Mes %in% c("abr"))
        } else if(input$per_fluxo[1]=="mai" & input$per_fluxo[2]=="mai"){ano_fluxo() %>% filter(Mes %in% c("mai"))
        } else if(input$per_fluxo[1]=="jun" & input$per_fluxo[2]=="jun"){ano_fluxo() %>% filter(Mes %in% c("jun"))
        } else if(input$per_fluxo[1]=="jul" & input$per_fluxo[2]=="jul"){ano_fluxo() %>% filter(Mes %in% c("jul"))
        } else if(input$per_fluxo[1]=="ago" & input$per_fluxo[2]=="ago"){ano_fluxo() %>% filter(Mes %in% c("ago"))
        } else if(input$per_fluxo[1]=="set" & input$per_fluxo[2]=="set"){ano_fluxo() %>% filter(Mes %in% c("set"))
        } else if(input$per_fluxo[1]=="out" & input$per_fluxo[2]=="out"){ano_fluxo() %>% filter(Mes %in% c("out"))
        } else if(input$per_fluxo[1]=="nov" & input$per_fluxo[2]=="nov"){ano_fluxo() %>% filter(Mes %in% c("nov"))
        } else if(input$per_fluxo[1]=="dez" & input$per_fluxo[2]=="dez"){ano_fluxo() %>% filter(Mes %in% c("dez"))
        } else if(input$per_fluxo[1]%in%c("jan", "fev") & input$per_fluxo[2]%in%c("jan", "fev")){ano_fluxo() %>% filter(Mes %in% c("jan","fev"))
        } else if(input$per_fluxo[1]%in%c("jan", "mar") & input$per_fluxo[2]%in%c("jan", "mar")){ano_fluxo() %>% filter(Mes %in% c("jan","fev","mar"))
        } else if(input$per_fluxo[1]%in%c("jan", "abr") & input$per_fluxo[2]%in%c("jan", "abr")){ano_fluxo() %>% filter(Mes %in% c("jan","fev","mar","abr"))
        } else if(input$per_fluxo[1]%in%c("jan", "mai") & input$per_fluxo[2]%in%c("jan", "mai")){ano_fluxo() %>% filter(Mes %in% c("jan","fev","mar","abr","mai"))
        } else if(input$per_fluxo[1]%in%c("jan", "jun") & input$per_fluxo[2]%in%c("jan", "jun")){ano_fluxo() %>% filter(Mes %in% c("jan","fev","mar","abr","mai", "jun"))
        } else if(input$per_fluxo[1]%in%c("jan", "jul") & input$per_fluxo[2]%in%c("jan", "jul")){ano_fluxo() %>% filter(Mes %in% c("jan","fev","mar","abr","mai", "jun", "jul"))
        } else if(input$per_fluxo[1]%in%c("jan", "ago") & input$per_fluxo[2]%in%c("jan", "ago")){ano_fluxo() %>% filter(Mes %in% c("jan","fev","mar","abr","mai", "jun", "jul","ago"))
        } else if(input$per_fluxo[1]%in%c("jan", "set") & input$per_fluxo[2]%in%c("jan", "set")){ano_fluxo() %>% filter(Mes %in% c("jan","fev","mar","abr","mai", "jun", "jul","ago","set"))
        } else if(input$per_fluxo[1]%in%c("jan", "out") & input$per_fluxo[2]%in%c("jan", "out")){ano_fluxo() %>% filter(Mes %in% c("jan","fev","mar","abr","mai", "jun", "jul","ago","set","out"))
        } else if(input$per_fluxo[1]%in%c("jan", "nov") & input$per_fluxo[2]%in%c("jan", "nov")){ano_fluxo() %>% filter(Mes %in% c("jan","fev","mar","abr","mai", "jun", "jul","ago","set","out","nov"))
        } else if(input$per_fluxo[1]%in%c("jan", "dez") & input$per_fluxo[2]%in%c("jan", "dez")){ano_fluxo() %>% filter(Mes %in% c("jan","fev","mar","abr","mai", "jun", "jul","ago","set","out","nov","dez"))    
        } else if(input$per_fluxo[1]%in%c("fev", "mar") & input$per_fluxo[2]%in%c("fev", "mar")){ano_fluxo() %>% filter(Mes %in% c("fev","mar"))
        } else if(input$per_fluxo[1]%in%c("fev", "abr") & input$per_fluxo[2]%in%c("fev", "abr")){ano_fluxo() %>% filter(Mes %in% c("fev","mar","abr"))
        } else if(input$per_fluxo[1]%in%c("fev", "mai") & input$per_fluxo[2]%in%c("fev", "mai")){ano_fluxo() %>% filter(Mes %in% c("fev","mar","abr","mai"))
        } else if(input$per_fluxo[1]%in%c("fev", "jun") & input$per_fluxo[2]%in%c("fev", "jun")){ano_fluxo() %>% filter(Mes %in% c("fev","mar","abr","mai","jun"))
        } else if(input$per_fluxo[1]%in%c("fev", "jul") & input$per_fluxo[2]%in%c("fev", "jul")){ano_fluxo() %>% filter(Mes %in% c("fev","mar","abr","mai","jun","jul"))
        } else if(input$per_fluxo[1]%in%c("fev", "ago") & input$per_fluxo[2]%in%c("fev", "ago")){ano_fluxo() %>% filter(Mes %in% c("fev","mar","abr","mai","jun","jul","ago"))
        } else if(input$per_fluxo[1]%in%c("fev", "set") & input$per_fluxo[2]%in%c("fev", "set")){ano_fluxo() %>% filter(Mes %in% c("fev","mar","abr","mai","jun","jul","ago","set"))
        } else if(input$per_fluxo[1]%in%c("fev", "out") & input$per_fluxo[2]%in%c("fev", "out")){ano_fluxo() %>% filter(Mes %in% c("fev","mar","abr","mai","jun","jul","ago","set","out"))
        } else if(input$per_fluxo[1]%in%c("fev", "nov") & input$per_fluxo[2]%in%c("fev", "nov")){ano_fluxo() %>% filter(Mes %in% c("fev","mar","abr","mai","jun","jul","ago","set","out","nov"))
        } else if(input$per_fluxo[1]%in%c("fev", "dez") & input$per_fluxo[2]%in%c("fev", "dez")){ano_fluxo() %>% filter(Mes %in% c("fev","mar","abr","mai","jun","jul","ago","set","out","nov","dez"))
        } else if(input$per_fluxo[1]%in%c("mar", "abr") & input$per_fluxo[2]%in%c("mar", "abr")){ano_fluxo() %>% filter(Mes %in% c("mar","abr"))
        } else if(input$per_fluxo[1]%in%c("mar", "mai") & input$per_fluxo[2]%in%c("mar", "mai")){ano_fluxo() %>% filter(Mes %in% c("mar","abr","mai"))
        } else if(input$per_fluxo[1]%in%c("mar", "jun") & input$per_fluxo[2]%in%c("mar", "jun")){ano_fluxo() %>% filter(Mes %in% c("mar","abr","mai","jun"))
        } else if(input$per_fluxo[1]%in%c("mar", "jul") & input$per_fluxo[2]%in%c("mar", "jul")){ano_fluxo() %>% filter(Mes %in% c("mar","abr","mai","jun","jul"))
        } else if(input$per_fluxo[1]%in%c("mar", "ago") & input$per_fluxo[2]%in%c("mar", "ago")){ano_fluxo() %>% filter(Mes %in% c("mar","abr","mai","jun","jul","ago"))
        } else if(input$per_fluxo[1]%in%c("mar", "set") & input$per_fluxo[2]%in%c("mar", "set")){ano_fluxo() %>% filter(Mes %in% c("mar","abr","mai","jun","jul","ago","set"))
        } else if(input$per_fluxo[1]%in%c("mar", "out") & input$per_fluxo[2]%in%c("mar", "out")){ano_fluxo() %>% filter(Mes %in% c("mar","abr","mai","jun","jul","ago","set","out"))
        } else if(input$per_fluxo[1]%in%c("mar", "nov") & input$per_fluxo[2]%in%c("mar", "nov")){ano_fluxo() %>% filter(Mes %in% c("mar","abr","mai","jun","jul","ago","set","out","nov"))
        } else if(input$per_fluxo[1]%in%c("mar", "dez") & input$per_fluxo[2]%in%c("mar", "dez")){ano_fluxo() %>% filter(Mes %in% c("mar","abr","mai","jun","jul","ago","set","out","nov","dez"))
        } else if(input$per_fluxo[1]%in%c("abr", "mai") & input$per_fluxo[2]%in%c("abr", "mai")){ano_fluxo() %>% filter(Mes %in% c("abr","mai"))
        } else if(input$per_fluxo[1]%in%c("abr", "jun") & input$per_fluxo[2]%in%c("abr", "jun")){ano_fluxo() %>% filter(Mes %in% c("abr","mai","jun"))
        } else if(input$per_fluxo[1]%in%c("abr", "jul") & input$per_fluxo[2]%in%c("abr", "jul")){ano_fluxo() %>% filter(Mes %in% c("abr","mai","jun","jul"))
        } else if(input$per_fluxo[1]%in%c("abr", "ago") & input$per_fluxo[2]%in%c("abr", "ago")){ano_fluxo() %>% filter(Mes %in% c("abr","mai","jun","jul","ago"))
        } else if(input$per_fluxo[1]%in%c("abr", "set") & input$per_fluxo[2]%in%c("abr", "set")){ano_fluxo() %>% filter(Mes %in% c("abr","mai","jun","jul","ago","set"))
        } else if(input$per_fluxo[1]%in%c("abr", "out") & input$per_fluxo[2]%in%c("abr", "out")){ano_fluxo() %>% filter(Mes %in% c("abr","mai","jun","jul","ago","set","out"))
        } else if(input$per_fluxo[1]%in%c("abr", "nov") & input$per_fluxo[2]%in%c("abr", "nov")){ano_fluxo() %>% filter(Mes %in% c("abr","mai","jun","jul","ago","set","out","nov"))
        } else if(input$per_fluxo[1]%in%c("abr", "dez") & input$per_fluxo[2]%in%c("abr", "dez")){ano_fluxo() %>% filter(Mes %in% c("abr","mai","jun","jul","ago","set","out","nov","dez"))
        } else if(input$per_fluxo[1]%in%c("mai", "jun") & input$per_fluxo[2]%in%c("mai", "jun")){ano_fluxo() %>% filter(Mes %in% c("mai","jun"))
        } else if(input$per_fluxo[1]%in%c("mai", "jul") & input$per_fluxo[2]%in%c("mai", "jul")){ano_fluxo() %>% filter(Mes %in% c("mai","jun","jul"))
        } else if(input$per_fluxo[1]%in%c("mai", "ago") & input$per_fluxo[2]%in%c("mai", "ago")){ano_fluxo() %>% filter(Mes %in% c("mai","jun","jul","ago"))
        } else if(input$per_fluxo[1]%in%c("mai", "set") & input$per_fluxo[2]%in%c("mai", "set")){ano_fluxo() %>% filter(Mes %in% c("mai","jun","jul","ago","set"))
        } else if(input$per_fluxo[1]%in%c("mai", "out") & input$per_fluxo[2]%in%c("mai", "out")){ano_fluxo() %>% filter(Mes %in% c("mai","jun","jul","ago","set","out"))
        } else if(input$per_fluxo[1]%in%c("mai", "nov") & input$per_fluxo[2]%in%c("mai", "nov")){ano_fluxo() %>% filter(Mes %in% c("mai","jun","jul","ago","set","out","nov"))
        } else if(input$per_fluxo[1]%in%c("mai", "dez") & input$per_fluxo[2]%in%c("mai", "dez")){ano_fluxo() %>% filter(Mes %in% c("mai","jun","jul","ago","set","out","nov","dez"))      
        } else if(input$per_fluxo[1]%in%c("jun", "jul") & input$per_fluxo[2]%in%c("jun", "jul")){ano_fluxo() %>% filter(Mes %in% c("jun","jul"))     
        } else if(input$per_fluxo[1]%in%c("jun", "ago") & input$per_fluxo[2]%in%c("jun", "ago")){ano_fluxo() %>% filter(Mes %in% c("jun","jul","ago"))    
        } else if(input$per_fluxo[1]%in%c("jun", "set") & input$per_fluxo[2]%in%c("jun", "set")){ano_fluxo() %>% filter(Mes %in% c("jun","jul","ago","set"))   
        } else if(input$per_fluxo[1]%in%c("jun", "out") & input$per_fluxo[2]%in%c("jun", "out")){ano_fluxo() %>% filter(Mes %in% c("jun","jul","ago","set","out")) 
        } else if(input$per_fluxo[1]%in%c("jun", "nov") & input$per_fluxo[2]%in%c("jun", "nov")){ano_fluxo() %>% filter(Mes %in% c("jun","jul","ago","set","out","nov"))
        } else if(input$per_fluxo[1]%in%c("jun", "dez") & input$per_fluxo[2]%in%c("jun", "dez")){ano_fluxo() %>% filter(Mes %in% c("jun","jul","ago","set","out","nov","dez"))
        } else if(input$per_fluxo[1]%in%c("jul", "ago") & input$per_fluxo[2]%in%c("jul", "ago")){ano_fluxo() %>% filter(Mes %in% c("jul","ago"))
        } else if(input$per_fluxo[1]%in%c("jul", "set") & input$per_fluxo[2]%in%c("jul", "set")){ano_fluxo() %>% filter(Mes %in% c("jul","ago","set"))
        } else if(input$per_fluxo[1]%in%c("jul", "out") & input$per_fluxo[2]%in%c("jul", "out")){ano_fluxo() %>% filter(Mes %in% c("jul","ago","set","out"))
        } else if(input$per_fluxo[1]%in%c("jul", "nov") & input$per_fluxo[2]%in%c("jul", "nov")){ano_fluxo() %>% filter(Mes %in% c("jul","ago","set","out","nov"))
        } else if(input$per_fluxo[1]%in%c("jul", "dez") & input$per_fluxo[2]%in%c("jul", "dez")){ano_fluxo() %>% filter(Mes %in% c("jul","ago","set","out","nov","dez"))
        } else if(input$per_fluxo[1]%in%c("ago", "set") & input$per_fluxo[2]%in%c("ago", "set")){ano_fluxo() %>% filter(Mes %in% c("ago","set"))
        } else if(input$per_fluxo[1]%in%c("ago", "out") & input$per_fluxo[2]%in%c("ago", "out")){ano_fluxo() %>% filter(Mes %in% c("ago","set","out"))
        } else if(input$per_fluxo[1]%in%c("ago", "nov") & input$per_fluxo[2]%in%c("ago", "nov")){ano_fluxo() %>% filter(Mes %in% c("ago","set","out","nov"))
        } else if(input$per_fluxo[1]%in%c("ago", "dez") & input$per_fluxo[2]%in%c("ago", "dez")){ano_fluxo() %>% filter(Mes %in% c("ago","set","out","nov","dez"))
        } else if(input$per_fluxo[1]%in%c("set", "out") & input$per_fluxo[2]%in%c("set", "out")){ano_fluxo() %>% filter(Mes %in% c("set","out"))
        } else if(input$per_fluxo[1]%in%c("set", "nov") & input$per_fluxo[2]%in%c("set", "nov")){ano_fluxo() %>% filter(Mes %in% c("set","out","nov"))
        } else if(input$per_fluxo[1]%in%c("set", "dez") & input$per_fluxo[2]%in%c("set", "dez")){ano_fluxo() %>% filter(Mes %in% c("set","out","nov","dez"))
        } else if(input$per_fluxo[1]%in%c("out", "nov") & input$per_fluxo[2]%in%c("out", "nov")){ano_fluxo() %>% filter(Mes %in% c("out","nov"))
        } else if(input$per_fluxo[1]%in%c("out", "dez") & input$per_fluxo[2]%in%c("out", "dez")){ano_fluxo() %>% filter(Mes %in% c("out","nov","dez"))
        } else if(input$per_fluxo[1]%in%c("nov", "dez") & input$per_fluxo[2]%in%c("nov", "dez")){ano_fluxo() %>% filter(Mes %in% c("nov","dez"))
        }
        
    })
    
    
    output$unidade_dist <- renderUI({
        
        selectInput("unidade_dist", h4("Selecione a(s) unidade(s):"),
                    choices = unique(distribuidos$UNIDADE),
                    selected = unique(distribuidos$UNIDADE)[1] , multiple = T)
        
    })
    
    output$unidade_fluxo <- renderUI({
        
        selectInput("unidade_fluxo", h4("Selecione a(s) unidade(s):"),
                    choices = unique(fluxo$UNIDADE),
                    selected = unique(fluxo$UNIDADE)[1] , multiple = T)
        
    })
    
    unidade_dist <- reactive({
        mes_dist() %>% filter(UNIDADE %in% input$unidade_dist)
        
    })
    
    unidade_fluxo <- reactive({
        mes_fluxo() %>% filter(UNIDADE %in% input$unidade_fluxo)
        
    })
    
    output$plotline_dist <- renderPlotly({
        
        if (dim(unidade_dist())[1]==0) {} else {
            ggplotly(ggplot(data = unidade_dist(), aes(x = Mes, y = SALDO, colour = UNIDADE, text = paste('UNIDADE: ', UNIDADE, '<br>SALDO: ', SALDO))) +
                         geom_line(aes(group = UNIDADE)) + geom_point() +
                         theme(plot.title = element_text(hjust = 0.5, size=15)) +
                         ggtitle("Demonstrativo de Distribuições (Saldo Mensal = Entradas - Saídas)") +
                         xlab("") + ylab("") + 
                         theme(legend.position="none"), tooltip = 'text', height = 590) %>%
                config(displayModeBar = FALSE)}
        
    })
    
    soma_dist <- reactive(
        
        unidade_dist() %>%
            group_by(UNIDADE, Fluxo) %>% summarise(Soma=sum(quant_fluxo))
        
    )
    
    soma_fluxo <- reactive(
        
        unidade_fluxo() %>% filter(Indicador != "ACERVO") %>% 
            group_by(UNIDADE, Indicador) %>% summarise(Soma=sum(valor)) %>%
            as.data.frame()
        
    )
    
    acervo_fluxo <- reactive(
        
        unidade_fluxo() %>%
            filter(Indicador == "ACERVO" & Mes == input$per_fluxo[2]) %>% 
            select(UNIDADE, Indicador, Soma = valor) %>% 
            as.data.frame()
        
    )
    
    tudo_fluxo <- reactive({
       a <- rbind(soma_fluxo(), acervo_fluxo())
      
       a$Indicador <- factor(a$Indicador, levels(as.factor(a$Indicador))[c(1, 3, 2, 4)])
    
       return(a)
    })
    
    output$plotbar_dist <- renderPlot(
        if (dim(unidade_dist())[1]==0) {}
        else {
            ggplot(data = soma_dist(), aes(x = UNIDADE, y = Soma, fill = Fluxo)) +
                geom_bar(stat="identity", position=position_dodge()) +
                theme(text = element_text(size=13),
                      axis.text.x = element_text(angle = 90, hjust = 1),
                      plot.title = element_text(hjust = 0.5, size=20)) +
                ggtitle("Demonstrativo de Distribuições/Redistribuições") +
                xlab(element_blank()) +
                ylab(element_blank()) +
                geom_text(aes(label=Soma), vjust=-0.3, color="black",
                          position = position_dodge(0.9), size=4)
                }, height = 650
    )

    output$plotbar_fluxo <- renderPlot(
        if (dim(unidade_fluxo())[1]==0) {}
        else {
            ggplot(data = tudo_fluxo(), aes(x = UNIDADE, y = Soma, fill = Indicador)) +
                geom_bar(stat="identity", position=position_dodge()) +
                scale_fill_manual(values = c("#FA8072", "#FFA07A", "#00CED1", "#96CDCD")) +
                theme(text = element_text(size=13),
                      axis.text.x = element_text(angle = 90, hjust = 1),
                      plot.title = element_text(hjust = 0.5, size=20)) +
                ggtitle("Fluxo Processual") +
                xlab(element_blank()) +
                ylab(element_blank()) +
                geom_text(aes(label=Soma), vjust=-0.3, color="black",
                          position = position_dodge(0.9), size=4)
        }, height = 650
    )
    
})
