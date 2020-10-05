library(readxl)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

rm(list = ls())

dados_atu <- read_excel("Indicadores - Atualizado em 2020-07-29 08-04-55.xls")

dados_atu <- dados_atu %>% filter(str_detect(`MÊS DE REFERÊNCIA`,pattern = "20$")==FALSE)

dados_2020 <- read_excel("Indicadores - Atualizado em 2020-10-05 08-02-31.xls") ####

dados_atu <- rbind(dados_atu, dados_2020)

dados_atu <- dados_atu %>% select(!ID_UNIDADE)

dados_atu <- dados_atu %>% mutate(DATA = str_sub(`MÊS DE REFERÊNCIA`, end = 3))

dados_atu <- dados_atu %>% mutate(ANO = str_sub(`MÊS DE REFERÊNCIA`, start = 5) %>% as.numeric)

dados_atu$DATA<- factor(dados_atu$DATA, levels(as.factor(dados_atu$DATA))[c(5, 4, 9, 1, 8, 7, 6, 2, 12, 11, 10, 3)])

dados_atu$UNIDADE[which(dados_atu$UNIDADE %in%
                          c("NATAL - JUIZADO ESPECIAL CRIMINAL"))] <- 
  "NATAL - JUIZADO ESPECIAL CRIMINAL CENTRAL"

dados_atu$UNIDADE[which(dados_atu$UNIDADE %in%
                          c("NATAL - 14º JUIZADO ESPECIAL CÍVEL"))] <- 
  "NATAL - 14º JUIZADO ESPECIAL CÍVEL CENTRAL"

dados_atu$UNIDADE[which(dados_atu$UNIDADE %in%
                          c("NATAL - 15º JUIZADO ESPECIAL CÍVEL"))] <- 
  "NATAL - 15º JUIZADO ESPECIAL CÍVEL CENTRAL"

dados_atu$UNIDADE[which(dados_atu$UNIDADE %in%
                          c("NATAL - 16º JUIZADO ESPECIAL CÍVEL"))] <- 
  "NATAL - 16º JUIZADO ESPECIAL CÍVEL CENTRAL"


for (i in 1:dim(dados_atu)[1]) {
  if (dados_atu$ANO[i] == 18){
    dados_atu$ANO[i] <- 2018
  } else if (dados_atu$ANO[i] == 19){
    dados_atu$ANO[i] <- 2019
  } else if (dados_atu$ANO[i] == 20){
    dados_atu$ANO[i] <- 2020
  }
}

# Acrescentando o acervo ao banco

acervo <- read_xlsx("acervo.xlsx")

acervo$`Comarca - Unidade`[which(acervo$`Comarca - Unidade` %in%
                                   c("NATAL - 1º JUIZADO ESPECIAL CÍVEL",
                                     "NATAL - 2º JUIZADO ESPECIAL CÍVEL",
                                     "NATAL - 3º JUIZADO ESPECIAL CÍVEL",
                                     "NATAL - 4º JUIZADO ESPECIAL CÍVEL",
                                     "NATAL - 5º JUIZADO ESPECIAL CÍVEL",
                                     "NATAL - 6º JUIZADO ESPECIAL CÍVEL",
                                     "NATAL - 7º JUIZADO ESPECIAL CÍVEL",
                                     "NATAL - 8º JUIZADO ESPECIAL CÍVEL",
                                     "NATAL - 9º JUIZADO ESPECIAL CÍVEL",
                                     "NATAL - 10º JUIZADO ESPECIAL CÍVEL",
                                     "NATAL - 11º JUIZADO ESPECIAL CÍVEL",
                                     "NATAL - 12º JUIZADO ESPECIAL CÍVEL",
                                     "NATAL - 13º JUIZADO ESPECIAL CÍVEL",
                                     "NATAL - 14º JUIZADO ESPECIAL CÍVEL",
                                     "NATAL - 15º JUIZADO ESPECIAL CÍVEL",
                                     "NATAL - 16º JUIZADO ESPECIAL CÍVEL"))] <-
  c("NATAL - 1º JUIZADO ESPECIAL CÍVEL CENTRAL",
    "NATAL - 2º JUIZADO ESPECIAL CÍVEL CENTRAL",
    "NATAL - 3º JUIZADO ESPECIAL CÍVEL CENTRAL",
    "NATAL - 4º JUIZADO ESPECIAL CÍVEL CENTRAL",
    "NATAL - 5º JUIZADO ESPECIAL CÍVEL CENTRAL",
    "NATAL - 6º JUIZADO ESPECIAL CÍVEL CENTRAL",
    "NATAL - 7º JUIZADO ESPECIAL CÍVEL CENTRAL",
    "NATAL - 8º JUIZADO ESPECIAL CÍVEL CENTRAL",
    "NATAL - 9º JUIZADO ESPECIAL CÍVEL CENTRAL",
    "NATAL - 10º JUIZADO ESPECIAL CÍVEL CENTRAL",
    "NATAL - 11º JUIZADO ESPECIAL CÍVEL CENTRAL",
    "NATAL - 12º JUIZADO ESPECIAL CÍVEL CENTRAL",
    "NATAL - 13º JUIZADO ESPECIAL CÍVEL CENTRAL",
    "NATAL - 14º JUIZADO ESPECIAL CÍVEL CENTRAL",
    "NATAL - 15º JUIZADO ESPECIAL CÍVEL CENTRAL",
    "NATAL - 16º JUIZADO ESPECIAL CÍVEL CENTRAL")

# acrescentando maio de 2020 ao acervo

acervo_maio_20 <- read_xlsx("Acervo em 31-05-2020 às 22-22-52.xlsx")

acervo_maio_20$UNIDADE[which(acervo_maio_20$UNIDADE %in%
                               c("NATAL - JUIZADO ESPECIAL CRIMINAL"))] <-
  "NATAL - JUIZADO ESPECIAL CRIMINAL CENTRAL"

acervo <- left_join(acervo, acervo_maio_20[,c(1,4)], by = c("Comarca - Unidade" = "UNIDADE"))

names(acervo)[19] <- "mai2"

# acrescentando junho de 2020 ao acervo 

acervo_junho_20 <- read_excel("Acervo - Atualizado em 2020-07-01 08-00-20.xls")

acervo_junho_20$UNIDADE[which(acervo_junho_20$UNIDADE %in%
                                c("NATAL - JUIZADO ESPECIAL CRIMINAL"))] <-
  "NATAL - JUIZADO ESPECIAL CRIMINAL CENTRAL"

acervo_junho_20$UNIDADE[which(acervo_junho_20$UNIDADE %in%
                                c("NATAL - 14º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 14º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo_junho_20$UNIDADE[which(acervo_junho_20$UNIDADE %in%
                                c("NATAL - 15º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 15º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo_junho_20$UNIDADE[which(acervo_junho_20$UNIDADE %in%
                                c("NATAL - 16º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 16º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo <- left_join(acervo, acervo_junho_20[,c(1,4)], by = c("Comarca - Unidade" = "UNIDADE"))

names(acervo)[20] <- "jun2"

# acrescentando julho até 06 de 2020 ao acervo 

acervo_julho_20 <- read_excel("Acervo - Atualizado em 2020-08-01 08-00-24.xls")

acervo_julho_20$UNIDADE[which(acervo_julho_20$UNIDADE %in%
                                c("NATAL - JUIZADO ESPECIAL CRIMINAL"))] <-
  "NATAL - JUIZADO ESPECIAL CRIMINAL CENTRAL"

acervo_julho_20$UNIDADE[which(acervo_julho_20$UNIDADE %in%
                                c("NATAL - 14º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 14º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo_julho_20$UNIDADE[which(acervo_julho_20$UNIDADE %in%
                                c("NATAL - 15º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 15º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo_julho_20$UNIDADE[which(acervo_julho_20$UNIDADE %in%
                                c("NATAL - 16º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 16º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo <- left_join(acervo, acervo_julho_20[,c(1,4)], by = c("Comarca - Unidade" = "UNIDADE"))

names(acervo)[21] <- "jul2"

# acrescentando agosto ao acervo 

acervo_agosto_20 <- read_excel("Acervo - Atualizado em 2020-08-31 08-00-21.xls") 

acervo_agosto_20$UNIDADE[which(acervo_agosto_20$UNIDADE %in%
                                c("NATAL - JUIZADO ESPECIAL CRIMINAL"))] <-
  "NATAL - JUIZADO ESPECIAL CRIMINAL CENTRAL"

acervo_agosto_20$UNIDADE[which(acervo_agosto_20$UNIDADE %in%
                                c("NATAL - 14º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 14º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo_agosto_20$UNIDADE[which(acervo_agosto_20$UNIDADE %in%
                                c("NATAL - 15º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 15º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo_agosto_20$UNIDADE[which(acervo_agosto_20$UNIDADE %in%
                                c("NATAL - 16º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 16º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo <- left_join(acervo, acervo_agosto_20[,c(1,4)], by = c("Comarca - Unidade" = "UNIDADE"))

names(acervo)[22] <- "ago2"


# acrescentando setembro ao acervo 

acervo_setembro_20 <- read_excel("Acervo - Atualizado em 2020-09-28 08-00-22.xls") ###

acervo_setembro_20$UNIDADE[which(acervo_setembro_20$UNIDADE %in%
                                 c("NATAL - JUIZADO ESPECIAL CRIMINAL"))] <-
  "NATAL - JUIZADO ESPECIAL CRIMINAL CENTRAL"

acervo_setembro_20$UNIDADE[which(acervo_setembro_20$UNIDADE %in%
                                 c("NATAL - 14º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 14º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo_setembro_20$UNIDADE[which(acervo_setembro_20$UNIDADE %in%
                                 c("NATAL - 15º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 15º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo_setembro_20$UNIDADE[which(acervo_setembro_20$UNIDADE %in%
                                 c("NATAL - 16º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 16º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo <- left_join(acervo, acervo_setembro_20[,c(1,4)], by = c("Comarca - Unidade" = "UNIDADE"))

names(acervo)[23] <- "set2"



# acrescentando outubro ao acervo 

acervo_outubro_20 <- read_excel("Acervo - Atualizado em 2020-10-05 08-00-22.xls") ###

acervo_outubro_20$UNIDADE[which(acervo_outubro_20$UNIDADE %in%
                                   c("NATAL - JUIZADO ESPECIAL CRIMINAL"))] <-
  "NATAL - JUIZADO ESPECIAL CRIMINAL CENTRAL"

acervo_outubro_20$UNIDADE[which(acervo_outubro_20$UNIDADE %in%
                                   c("NATAL - 14º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 14º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo_outubro_20$UNIDADE[which(acervo_outubro_20$UNIDADE %in%
                                   c("NATAL - 15º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 15º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo_outubro_20$UNIDADE[which(acervo_outubro_20$UNIDADE %in%
                                   c("NATAL - 16º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 16º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo <- left_join(acervo, acervo_outubro_20[,c(1,4)], by = c("Comarca - Unidade" = "UNIDADE"))

names(acervo)[24] <- "out2"



# Preparando todo o acervo

acervo <- acervo %>% gather("MÊS", "ACERVO", 3:24)

for(i in 1:dim(acervo)[1]){
  if(acervo$MÊS[i] %in% c("jan2", "fev2", "mar2", "abr2", "mai2", "jun2", "jul2", "ago2","set2","out2")){acervo$ANO[i] <- 2020
  } else{acervo$ANO[i] <- 2019}
}

for(i in 1:dim(acervo)[1]){
  if(acervo$MÊS[i] == "jan2"){acervo$MÊS[i] <- "jan"
  } else if(acervo$MÊS[i] == "fev2"){acervo$MÊS[i] <- "fev"
  } else if(acervo$MÊS[i] == "mar2"){acervo$MÊS[i] <- "mar"
  } else if(acervo$MÊS[i] == "abr2"){acervo$MÊS[i] <- "abr"
  } else if(acervo$MÊS[i] == "mai2"){acervo$MÊS[i] <- "mai"
  } else if(acervo$MÊS[i] == "jun2"){acervo$MÊS[i] <- "jun"
  } else if(acervo$MÊS[i] == "jul2"){acervo$MÊS[i] <- "jul"
  } else if(acervo$MÊS[i] == "ago2"){acervo$MÊS[i] <- "ago"
  } else if(acervo$MÊS[i] == "set2"){acervo$MÊS[i] <- "set"
  } else if(acervo$MÊS[i] == "out2"){acervo$MÊS[i] <- "out"
  }
}

acervo$MÊS <- factor(acervo$MÊS, levels(as.factor(acervo$MÊS))[c(5, 4, 9, 1, 8, 7, 6, 2, 12, 11, 10, 3)])


todos <- left_join(dados_atu, acervo, by = c("UNIDADE" = "Comarca - Unidade", "DATA" = "MÊS", "ANO" = "ANO"))

# Taxa de congestionamento

taxa <- read_excel("Taxa de congestionamento - Atualizado em  2020-10-05 08-00-21.xls") ###

for (i in 1:nrow(taxa)) {
  if (taxa$MÊS[i] == 1) {taxa$mes[i] <- "jan"}
  else if (taxa$MÊS[i] == 2) {taxa$mes[i] <- "fev"}
  else if (taxa$MÊS[i] == 3) {taxa$mes[i] <- "mar"}
  else if (taxa$MÊS[i] == 4) {taxa$mes[i] <- "abr"}
  else if (taxa$MÊS[i] == 5) {taxa$mes[i] <- "mai"}
  else if (taxa$MÊS[i] == 6) {taxa$mes[i] <- "jun"}
  else if (taxa$MÊS[i] == 7) {taxa$mes[i] <- "jul"}
  else if (taxa$MÊS[i] == 8) {taxa$mes[i] <- "ago"}
  else if (taxa$MÊS[i] == 9) {taxa$mes[i] <- "set"}
  else if (taxa$MÊS[i] == 10) {taxa$mes[i] <- "out"}
  else if (taxa$MÊS[i] == 11) {taxa$mes[i] <- "nov"}
  else if (taxa$MÊS[i] == 12) {taxa$mes[i] <- "dez"}
}

taxa <- taxa %>% mutate(mes_ano = paste(mes, str_sub(ANO, start = 3), sep = "/"))

# Mudar a ordem quando acrescentar um mês
taxa$mes_ano <- factor(taxa$mes_ano, levels(as.factor(taxa$mes_ano))[c(11, 10, 3, 5, 4, 9, 1, 8, 7,6,2,12)])

taxa$`TAXA LÍQUIDA` <- round(taxa$`TAXA LÍQUIDA`,2)

# Distribuídos

distribuidos <- read_excel("Distribuições - Atualizado em 2020-10-05 08-01-37.xls") ####

distribuidos <- distribuidos %>%
  mutate(Mes = str_sub(`MÊS DE REFERÊNCIA_TEXTO`, end = 3)%>% str_to_lower())

distribuidos$Mes <- factor(distribuidos$Mes, levels(as.factor(distribuidos$Mes))[c(5, 4, 9, 1, 8, 7, 6, 2, 12, 11, 10, 3)])

distribuidos <- distribuidos %>% mutate(`Distribuição/Redistribuição (Entradas)` = `DISTRIBUIÇÕES - ENTRADAS` + `REDISTRIBUIÇÕES - ENTRADAS`, `Redistribuídos (Saídas)` = `REDISTRIBUIÇÕES - SAÍDAS`)

distribuidos <- distribuidos %>% gather(Fluxo, quant_fluxo, `Distribuição/Redistribuição (Entradas)`:`Redistribuídos (Saídas)`)

distribuidos$UNIDADE[which(distribuidos$UNIDADE %in%
                                c("NATAL - JUIZADO ESPECIAL CRIMINAL"))] <-
  "NATAL - JUIZADO ESPECIAL CRIMINAL CENTRAL"

distribuidos$UNIDADE[which(distribuidos$UNIDADE %in%
                                c("NATAL - 14º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 14º JUIZADO ESPECIAL CÍVEL CENTRAL"

distribuidos$UNIDADE[which(distribuidos$UNIDADE %in%
                                c("NATAL - 15º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 15º JUIZADO ESPECIAL CÍVEL CENTRAL"

distribuidos$UNIDADE[which(distribuidos$UNIDADE %in%
                                c("NATAL - 16º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 16º JUIZADO ESPECIAL CÍVEL CENTRAL"

distribuidos <- left_join(distribuidos, todos %>% select(UNIDADE, GRUPO) %>% unique(),
                  by = c("UNIDADE" = "UNIDADE"))

distribuidos <- distribuidos %>% filter(is.na(GRUPO.y)==FALSE)

# Fazendo as quebras de linhas das unidades com nome muito grande

# Indicadores

todos$UNIDADE[which(todos$UNIDADE %in% c("MOSSORÓ - 1º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                         "MOSSORÓ - 2º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                         "MOSSORÓ - 3º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                         "MOSSORÓ - 4º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                         "PARNAMIRIM - 1º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",          
                                         "PARNAMIRIM - 2º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",          
                                         "PARNAMIRIM - 3º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",          
                                         "PARNAMIRIM - 4º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",          
                                         "APODI - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                    
                                         "AREIA BRANCA - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                         "ASSÚ - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                     
                                         "CAICÓ - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                    
                                         "CEARÁ-MIRIM - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",              
                                         "CURRAIS NOVOS - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",            
                                         "JOÃO CÂMARA - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",              
                                         "MACAU - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                    
                                         "MACAÍBA - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                  
                                         "NOVA CRUZ - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                
                                         "PAU DOS FERROS - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",
                                         "SANTA CRUZ - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",
                                         "SÃO GONÇALO DO AMARANTE - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA"))] <-
  str_replace_all(todos$UNIDADE[which(todos$UNIDADE %in% c("MOSSORÓ - 1º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                                           "MOSSORÓ - 2º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                                           "MOSSORÓ - 3º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                                           "MOSSORÓ - 4º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                                           "PARNAMIRIM - 1º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",          
                                                           "PARNAMIRIM - 2º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",          
                                                           "PARNAMIRIM - 3º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",          
                                                           "PARNAMIRIM - 4º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",          
                                                           "APODI - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                    
                                                           "AREIA BRANCA - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                                           "ASSÚ - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                     
                                                           "CAICÓ - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                    
                                                           "CEARÁ-MIRIM - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",              
                                                           "CURRAIS NOVOS - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",            
                                                           "JOÃO CÂMARA - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",              
                                                           "MACAU - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                    
                                                           "MACAÍBA - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                  
                                                           "NOVA CRUZ - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                
                                                           "PAU DOS FERROS - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",
                                                           "SANTA CRUZ - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",
                                                           "SÃO GONÇALO DO AMARANTE - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA"))],
                  pattern = ", ",
                  replacement =",\n")

todos$UNIDADE[which(todos$UNIDADE %in% c("MOSSORÓ - VARA DO JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER",     
                                         "NATAL - 1º JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER",            
                                         "NATAL - 2º JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER",            
                                         "NATAL - 3º JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER",            
                                         "PARNAMIRIM - JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER"))] <- 
  str_replace_all(todos$UNIDADE[which(todos$UNIDADE %in% c("MOSSORÓ - VARA DO JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER",     
                                                           "NATAL - 1º JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER",            
                                                           "NATAL - 2º JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER",            
                                                           "NATAL - 3º JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER",            
                                                           "PARNAMIRIM - JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER"))],
                  pattern = " E ",
                  replacement = "\nE ")

# Taxa de congestionamento

taxa$UNIDADE[which(taxa$UNIDADE %in% c("MOSSORÓ - 1º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                       "MOSSORÓ - 2º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                       "MOSSORÓ - 3º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                       "MOSSORÓ - 4º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                       "PARNAMIRIM - 1º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",          
                                       "PARNAMIRIM - 2º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",          
                                       "PARNAMIRIM - 3º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",          
                                       "PARNAMIRIM - 4º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",          
                                       "APODI - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                    
                                       "AREIA BRANCA - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                       "ASSÚ - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                     
                                       "CAICÓ - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                    
                                       "CEARÁ-MIRIM - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",              
                                       "CURRAIS NOVOS - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",            
                                       "JOÃO CÂMARA - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",              
                                       "MACAU - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                    
                                       "MACAÍBA - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                  
                                       "NOVA CRUZ - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                
                                       "PAU DOS FERROS - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",
                                       "SANTA CRUZ - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",
                                       "SÃO GONÇALO DO AMARANTE - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA"))] <-
  str_replace_all(taxa$UNIDADE[which(taxa$UNIDADE %in% c("MOSSORÓ - 1º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                                         "MOSSORÓ - 2º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                                         "MOSSORÓ - 3º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                                         "MOSSORÓ - 4º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                                         "PARNAMIRIM - 1º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",          
                                                         "PARNAMIRIM - 2º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",          
                                                         "PARNAMIRIM - 3º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",          
                                                         "PARNAMIRIM - 4º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",          
                                                         "APODI - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                    
                                                         "AREIA BRANCA - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                                         "ASSÚ - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                     
                                                         "CAICÓ - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                    
                                                         "CEARÁ-MIRIM - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",              
                                                         "CURRAIS NOVOS - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",            
                                                         "JOÃO CÂMARA - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",              
                                                         "MACAU - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                    
                                                         "MACAÍBA - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                  
                                                         "NOVA CRUZ - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                
                                                         "PAU DOS FERROS - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",
                                                         "SANTA CRUZ - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",
                                                         "SÃO GONÇALO DO AMARANTE - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA"))],
                  pattern = ", ",
                  replacement =",\n")

taxa$UNIDADE[which(taxa$UNIDADE %in% c("MOSSORÓ - VARA DO JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER",     
                                       "NATAL - 1º JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER",            
                                       "NATAL - 2º JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER",            
                                       "NATAL - 3º JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER",            
                                       "PARNAMIRIM - JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER"))] <- 
  str_replace_all(taxa$UNIDADE[which(taxa$UNIDADE %in% c("MOSSORÓ - VARA DO JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER",     
                                                         "NATAL - 1º JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER",            
                                                         "NATAL - 2º JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER",            
                                                         "NATAL - 3º JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER",            
                                                         "PARNAMIRIM - JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER"))],
                  pattern = " E ",
                  replacement = "\nE ")


# Distribuídos

distribuidos$UNIDADE[which(distribuidos$UNIDADE %in% c("MOSSORÓ - 1º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                       "MOSSORÓ - 2º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                       "MOSSORÓ - 3º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                       "MOSSORÓ - 4º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                       "PARNAMIRIM - 1º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",          
                                       "PARNAMIRIM - 2º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",          
                                       "PARNAMIRIM - 3º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",          
                                       "PARNAMIRIM - 4º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",          
                                       "APODI - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                    
                                       "AREIA BRANCA - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                       "ASSÚ - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                     
                                       "CAICÓ - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                    
                                       "CEARÁ-MIRIM - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",              
                                       "CURRAIS NOVOS - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",            
                                       "JOÃO CÂMARA - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",              
                                       "MACAU - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                    
                                       "MACAÍBA - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                  
                                       "NOVA CRUZ - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                
                                       "PAU DOS FERROS - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",
                                       "SANTA CRUZ - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",
                                       "SÃO GONÇALO DO AMARANTE - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA"))] <-
  str_replace_all(distribuidos$UNIDADE[which(distribuidos$UNIDADE %in% c("MOSSORÓ - 1º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                                         "MOSSORÓ - 2º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                                         "MOSSORÓ - 3º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                                         "MOSSORÓ - 4º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                                         "PARNAMIRIM - 1º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",          
                                                         "PARNAMIRIM - 2º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",          
                                                         "PARNAMIRIM - 3º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",          
                                                         "PARNAMIRIM - 4º JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",          
                                                         "APODI - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                    
                                                         "AREIA BRANCA - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",             
                                                         "ASSÚ - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                     
                                                         "CAICÓ - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                    
                                                         "CEARÁ-MIRIM - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",              
                                                         "CURRAIS NOVOS - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",            
                                                         "JOÃO CÂMARA - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",              
                                                         "MACAU - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                    
                                                         "MACAÍBA - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                  
                                                         "NOVA CRUZ - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",                
                                                         "PAU DOS FERROS - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",
                                                         "SANTA CRUZ - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA",
                                                         "SÃO GONÇALO DO AMARANTE - JUIZADO ESPECIAL CÍVEL, CRIMINAL E DA FAZENDA PÚBLICA"))],
                  pattern = ", ",
                  replacement =",\n")

distribuidos$UNIDADE[which(distribuidos$UNIDADE %in% c("MOSSORÓ - VARA DO JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER",     
                                       "NATAL - 1º JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER",            
                                       "NATAL - 2º JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER",            
                                       "NATAL - 3º JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER",            
                                       "PARNAMIRIM - JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER"))] <- 
  str_replace_all(distribuidos$UNIDADE[which(distribuidos$UNIDADE %in% c("MOSSORÓ - VARA DO JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER",     
                                                         "NATAL - 1º JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER",            
                                                         "NATAL - 2º JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER",            
                                                         "NATAL - 3º JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER",            
                                                         "PARNAMIRIM - JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER"))],
                  pattern = " E ",
                  replacement = "\nE ")


# Criando o arquivo para a análise de fluxo

saldo <- distribuidos %>% filter(`ANO DE REFERÊNCIA` %in% c(2019, 2020) & 
                                   Fluxo == "Distribuição/Redistribuição (Entradas)") %>% 
  select(UNIDADE, ANO = `ANO DE REFERÊNCIA`, Mes, valor = SALDO) %>%
  mutate(Indicador = "SALDO DE DISTRIBUIÇÕES")

senteca <- todos %>% filter(ANO %in% c(2019, 2020)) %>% 
  select(UNIDADE, ANO, Mes = DATA, valor = SENTENÇAS) %>% 
  mutate(Indicador = "SENTENÇAS")

baixado <- todos %>% filter(ANO %in% c(2019, 2020)) %>% 
  select(UNIDADE, ANO, Mes = DATA, valor = BAIXADOS) %>% 
  mutate(Indicador = "BAIXADOS")

acervo_out <- todos %>% filter(ANO %in% c(2019, 2020)) %>% 
  select(UNIDADE, ANO, Mes = DATA, valor = ACERVO) %>% 
  mutate(Indicador = "ACERVO")

fluxo_processual <- rbind(saldo, senteca, baixado, acervo_out)

# Dando a saída dos arquivos

saveRDS(todos, file = "Produtividade/indicadores.rds")

saveRDS(taxa, file = "Produtividade/taxa.rds")

saveRDS(distribuidos, file = "Produtividade/distribuidos.rds")

saveRDS(fluxo_processual, file = "Produtividade/fluxo_processual.rds")
