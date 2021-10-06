library(readxl)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

rm(list = ls())

#git config --tratamento user.email "kainho424@gmail.com"
#git config --tratamento user.name "kaiobreno424"

dados_atu <- read_excel("Indicadores - Atualizado em 2020-07-29 08-04-55.xls")

dados_atu <- dados_atu %>% filter(str_detect(`MÊS DE REFERÊNCIA`,pattern = "20$")==FALSE)

dados_2020 <- read_excel("Indicadores - Atualizado em 2020-12-29 11-14-26.xls") 

dados_2021 <- read_excel("Indicadores - Atualizado em 2021-10-05 09-29-09.xls") ####

dados_2020 <- dados_2020 %>% select(!GRUPO)

dados_atu <- dados_atu %>% select(!GRUPO)

a <- dados_2021 %>% select(UNIDADE, GRUPO) %>% unique()

dados_2020 <- left_join(dados_2020, a, by =  "UNIDADE")

dados_atu <- left_join(dados_atu, a, by =  "UNIDADE")

dados_atu <- rbind(dados_atu, dados_2020, dados_2021)

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
  } else if (dados_atu$ANO[i] == 21){
    dados_atu$ANO[i] <- 2021
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

acervo_setembro_20 <- read_excel("Acervo - Atualizado em 2020-09-28 08-00-22.xls") 

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

acervo_outubro_20 <- read_excel("Acervo - Atualizado em 2020-10-26 08-00-22.xls") ###

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


# acrescentando novembro ao acervo 

acervo_novembro_20 <- read_excel("Acervo - Atualizado em 2020-11-30 08-00-20.xls") ###

acervo_novembro_20$UNIDADE[which(acervo_novembro_20$UNIDADE %in%
                                   c("NATAL - JUIZADO ESPECIAL CRIMINAL"))] <-
  "NATAL - JUIZADO ESPECIAL CRIMINAL CENTRAL"

acervo_novembro_20$UNIDADE[which(acervo_novembro_20$UNIDADE %in%
                                   c("NATAL - 14º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 14º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo_novembro_20$UNIDADE[which(acervo_novembro_20$UNIDADE %in%
                                   c("NATAL - 15º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 15º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo_novembro_20$UNIDADE[which(acervo_novembro_20$UNIDADE %in%
                                   c("NATAL - 16º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 16º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo <- left_join(acervo, acervo_novembro_20[,c(1,4)], by = c("Comarca - Unidade" = "UNIDADE"))

names(acervo)[25] <- "nov2"



# acrescentando dezembro ao acervo 

acervo_dezembro_20 <- read_excel("Acervo - Atualizado em 2020-12-29 11-14-12.xls") ###

acervo_dezembro_20$UNIDADE[which(acervo_dezembro_20$UNIDADE %in%
                                   c("NATAL - JUIZADO ESPECIAL CRIMINAL"))] <-
  "NATAL - JUIZADO ESPECIAL CRIMINAL CENTRAL"

acervo_dezembro_20$UNIDADE[which(acervo_dezembro_20$UNIDADE %in%
                                   c("NATAL - 14º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 14º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo_dezembro_20$UNIDADE[which(acervo_dezembro_20$UNIDADE %in%
                                   c("NATAL - 15º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 15º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo_dezembro_20$UNIDADE[which(acervo_dezembro_20$UNIDADE %in%
                                   c("NATAL - 16º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 16º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo <- left_join(acervo, acervo_dezembro_20[,c(1,4)], by = c("Comarca - Unidade" = "UNIDADE"))

names(acervo)[26] <- "dez2"

# acrescentando janeiro ao acervo 

acervo_janfev <- read_excel("Acervos Janeiro e fevereiro de 2021 - 1º Grau.xlsx")

acervo_janeiro_21 <- acervo_janfev %>% filter(mes == "Janeiro") %>% select(!mes)

acervo_janeiro_21$UNIDADE[which(acervo_janeiro_21$UNIDADE %in%
                                   c("NATAL - JUIZADO ESPECIAL CRIMINAL"))] <-
  "NATAL - JUIZADO ESPECIAL CRIMINAL CENTRAL"

acervo_janeiro_21$UNIDADE[which(acervo_janeiro_21$UNIDADE %in%
                                   c("NATAL - 14º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 14º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo_janeiro_21$UNIDADE[which(acervo_janeiro_21$UNIDADE %in%
                                   c("NATAL - 15º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 15º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo_janeiro_21$UNIDADE[which(acervo_janeiro_21$UNIDADE %in%
                                   c("NATAL - 16º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 16º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo <- left_join(acervo, acervo_janeiro_21[,c(1,4)], by = c("Comarca - Unidade" = "UNIDADE"))

names(acervo)[27] <- "jan3"

# acrescentando fevereiro ao acervo 

acervo_fevereiro_21 <- acervo_janfev %>% filter(mes == "Fevereiro") %>% select(!mes)

acervo_fevereiro_21$UNIDADE[which(acervo_fevereiro_21$UNIDADE %in%
                                  c("NATAL - JUIZADO ESPECIAL CRIMINAL"))] <-
  "NATAL - JUIZADO ESPECIAL CRIMINAL CENTRAL"

acervo_fevereiro_21$UNIDADE[which(acervo_fevereiro_21$UNIDADE %in%
                                  c("NATAL - 14º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 14º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo_fevereiro_21$UNIDADE[which(acervo_fevereiro_21$UNIDADE %in%
                                  c("NATAL - 15º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 15º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo_fevereiro_21$UNIDADE[which(acervo_fevereiro_21$UNIDADE %in%
                                  c("NATAL - 16º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 16º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo <- left_join(acervo, acervo_fevereiro_21[,c(1,4)], by = c("Comarca - Unidade" = "UNIDADE"))

names(acervo)[28] <- "fev3"

# acrescentando marco ao acervo 

acervo_marco_20 <- read_excel("Acervo - Atualizado em 2021-04-01 08-00-21.xls") ###

acervo_marco_20$UNIDADE[which(acervo_marco_20$UNIDADE %in%
                                    c("NATAL - JUIZADO ESPECIAL CRIMINAL"))] <-
  "NATAL - JUIZADO ESPECIAL CRIMINAL CENTRAL"

acervo_marco_20$UNIDADE[which(acervo_marco_20$UNIDADE %in%
                                    c("NATAL - 14º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 14º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo_marco_20$UNIDADE[which(acervo_marco_20$UNIDADE %in%
                                    c("NATAL - 15º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 15º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo_marco_20$UNIDADE[which(acervo_marco_20$UNIDADE %in%
                                    c("NATAL - 16º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 16º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo <- left_join(acervo, acervo_marco_20[,c(1,4)], by = c("Comarca - Unidade" = "UNIDADE"))

names(acervo)[29] <- "mar3"


# acrescentando abril ao acervo 

acervo_abril_20 <- read_excel("Acervo - Atualizado em 2021-05-01 08-00-25.xls") ###

acervo_abril_20$UNIDADE[which(acervo_abril_20$UNIDADE %in%
                                c("NATAL - JUIZADO ESPECIAL CRIMINAL"))] <-
  "NATAL - JUIZADO ESPECIAL CRIMINAL CENTRAL"

acervo_abril_20$UNIDADE[which(acervo_abril_20$UNIDADE %in%
                                c("NATAL - 14º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 14º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo_abril_20$UNIDADE[which(acervo_abril_20$UNIDADE %in%
                                c("NATAL - 15º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 15º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo_abril_20$UNIDADE[which(acervo_abril_20$UNIDADE %in%
                                c("NATAL - 16º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 16º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo <- left_join(acervo, acervo_abril_20[,c(1,4)], by = c("Comarca - Unidade" = "UNIDADE"))

names(acervo)[30] <- "abr3"


# acrescentando maio ao acervo 

acervo_maio_20 <- read_excel("Acervo - Atualizado em 2021-05-31 08-00-22.xls") ###

acervo_maio_20$UNIDADE[which(acervo_maio_20$UNIDADE %in%
                                c("NATAL - JUIZADO ESPECIAL CRIMINAL"))] <-
  "NATAL - JUIZADO ESPECIAL CRIMINAL CENTRAL"

acervo_maio_20$UNIDADE[which(acervo_maio_20$UNIDADE %in%
                                c("NATAL - 14º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 14º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo_maio_20$UNIDADE[which(acervo_maio_20$UNIDADE %in%
                                c("NATAL - 15º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 15º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo_maio_20$UNIDADE[which(acervo_maio_20$UNIDADE %in%
                                c("NATAL - 16º JUIZADO ESPECIAL CÍVEL"))] <-
  "NATAL - 16º JUIZADO ESPECIAL CÍVEL CENTRAL"

acervo <- left_join(acervo, acervo_maio_20[,c(1,4)], by = c("Comarca - Unidade" = "UNIDADE"))

names(acervo)[31] <- "mai3"



# acrescentando junho ao acervo 

acervo_junho_20 <- read_excel("Acervo - Atualizado em 2021-07-01 08-00-26.xls") ###

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

names(acervo)[32] <- "jun3"


# acrescentando julho ao acervo 

acervo_julho_20 <- read_excel("Acervo - Atualizado em 2021-08-02 08-00-30.xls") ###

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

names(acervo)[33] <- "jul3"


# acrescentando agosto ao acervo 

acervo_agosto_20 <- read_excel("Acervo - Atualizado em 2021-08-31 08-55-32.xls") ###

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

names(acervo)[34] <- "ago3"


# acrescentando setembro ao acervo 

acervo_setembro_20 <- read_excel("Acervo - Atualizado em 2021-09-21 14-08-34.xls") ###

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

names(acervo)[35] <- "set3"


# acrescentando outubro ao acervo 

acervo_outubro_20 <- read_excel("Acervo - Atualizado em 2021-10-05 09-00-49.xls") ###

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

names(acervo)[36] <- "out3"


# Preparando todo o acervo

acervo <- acervo %>% gather("MÊS", "ACERVO", 3:36)

acervo<- unique(acervo)

#aa1<- dim(acervo)[1]
ano2019<-0
ano2020<-0
ano2021<-0

ano2019[1:(225*12)] <- 2019
ano2020[1:(225*12)] <- 2020
ano2021[1:(225*10)] <- 2021  ####################################

acervo$ANO<- c(ano2019,ano2020,ano2021)



#for(i in 1:aa1){
#  if(acervo$MÊS[i] %in% c("jan2", "fev2", "mar2", "abr2", "mai2", "jun2", "jul2", "ago2","set2","out2", "nov2", "dez2")){acervo$ANO[i] <- 2020} 
#  else if(acervo$MÊS[i] %in% c("jan3", "fev3", "mar3", "abr3", "mai3", "jun3", "jul3", "ago3")){acervo$ANO[i] <- 2021} 
#  else{acervo$ANO[i] <- 2019}
#  print(i)
#}

janeiro<-"jan"
fevereiro<-"fev"
marco<-"mar"
abril<-"abr"
maio<-"mai"
junho<-"jun"
julho<-"jul"
agosto<-"ago"
setembro<-"set"
outubro<-"out"
novembro<-"nov"
dezembro<-"dez"


janeiro[1:225]<- "jan"
fevereiro[1:225]<- "fev"
marco[1:225]<- "mar"
abril[1:225]<- "abr"
maio[1:225]<- "mai"
junho[1:225]<- "jun"
julho[1:225]<- "jul"
agosto[1:225]<- "ago"
setembro[1:225]<- "set"
outubro[1:225]<- "out"
novembro[1:225]<- "nov"
dezembro[1:225]<- "dez"

AnoCompleto<- c(janeiro,fevereiro,marco,abril,maio,junho,julho,agosto,setembro,outubro,novembro,dezembro)


AnoAtual<-c(janeiro,fevereiro,marco,abril,maio,junho,julho,agosto,setembro,outubro)  ########################


acervo$MÊS <- c(AnoCompleto,AnoCompleto,AnoAtual)

#for(i in 1:dim(acervo)[1]){
#  if(acervo$MÊS[i] == "jan2"){acervo$MÊS[i] <- "jan"
#  } else if(acervo$MÊS[i] == "fev2"){acervo$MÊS[i] <- "fev"
#  } else if(acervo$MÊS[i] == "mar2"){acervo$MÊS[i] <- "mar"
#  } else if(acervo$MÊS[i] == "abr2"){acervo$MÊS[i] <- "abr"
#  } else if(acervo$MÊS[i] == "mai2"){acervo$MÊS[i] <- "mai"
#  } else if(acervo$MÊS[i] == "jun2"){acervo$MÊS[i] <- "jun"
#  } else if(acervo$MÊS[i] == "jul2"){acervo$MÊS[i] <- "jul"
#  } else if(acervo$MÊS[i] == "ago2"){acervo$MÊS[i] <- "ago"
#  } else if(acervo$MÊS[i] == "set2"){acervo$MÊS[i] <- "set"
#  } else if(acervo$MÊS[i] == "out2"){acervo$MÊS[i] <- "out"
#  } else if(acervo$MÊS[i] == "nov2"){acervo$MÊS[i] <- "nov"
#  } else if(acervo$MÊS[i] == "dez2"){acervo$MÊS[i] <- "dez"
#  } else if(acervo$MÊS[i] == "jan3"){acervo$MÊS[i] <- "jan"
#  } else if(acervo$MÊS[i] == "fev3"){acervo$MÊS[i] <- "fev"
#  } else if(acervo$MÊS[i] == "mar3"){acervo$MÊS[i] <- "mar"
#  } else if(acervo$MÊS[i] == "abr3"){acervo$MÊS[i] <- "abr"
#  } else if(acervo$MÊS[i] == "mai3"){acervo$MÊS[i] <- "mai"
#  } else if(acervo$MÊS[i] == "jun3"){acervo$MÊS[i] <- "jun"
#  } else if(acervo$MÊS[i] == "jul3"){acervo$MÊS[i] <- "jul"
#  } else if(acervo$MÊS[i] == "ago3"){acervo$MÊS[i] <- "ago"
#  }
#  print(i)
#}

acervo$MÊS <- factor(acervo$MÊS, levels(as.factor(acervo$MÊS))[c(5, 4, 9, 1, 8, 7, 6, 2, 12, 11, 10, 3)])



todos <- left_join(dados_atu, acervo, by = c("UNIDADE" = "Comarca - Unidade", "DATA" = "MÊS", "ANO" = "ANO"))

# Taxa de congestionamento

taxa <- read_excel("Taxa de congestionamento - Atualizado em  2021-10-05 09-00-49.xls") ###

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
taxa$mes_ano <- factor(taxa$mes_ano, levels(as.factor(taxa$mes_ano))[c(11, 10, 3, 5, 4, 9,1,8,7,6,2,12)])

taxa$`TAXA LÍQUIDA` <- round(taxa$`TAXA LÍQUIDA`,2)

# Distribuídos

distribuidos <- read_excel("Distribuições - Atualizado em 2021-10-05 09-13-53.xls") ####

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


#todos$UNIDADE[which(todos$UNIDADE %in%
#                      c("MOSSORÓ - VARA DO JUIZADO DE VIOLÊNCIA DOMÉSTICA\nE FAMILIAR CONTRA A MULHER"))] <-
#  "MOSSORÓ - JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER"

todos[which(todos$UNIDADE == "MOSSORÓ - VARA DO JUIZADO DE VIOLÊNCIA DOMÉSTICA\nE FAMILIAR CONTRA A MULHER"),][,9] <- 21

todos[which(todos$UNIDADE == "MOSSORÓ - JUIZADO DE VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER"),][,13]<- c(2244,2816,2852,2840,2873, 2947, 3041,3119,3163,3205)

# Criando o arquivo para a análise de fluxo

saldo <- distribuidos %>% filter(`ANO DE REFERÊNCIA` %in% c(2019, 2020, 2021) & 
                                   Fluxo == "Distribuição/Redistribuição (Entradas)") %>% 
  select(UNIDADE, ANO = `ANO DE REFERÊNCIA`, Mes, valor = SALDO) %>%
  mutate(Indicador = "SALDO DE DISTRIBUIÇÕES")

senteca <- todos %>% filter(ANO %in% c(2019, 2020, 2021)) %>% 
  select(UNIDADE, ANO, Mes = DATA, valor = SENTENÇAS) %>% 
  mutate(Indicador = "SENTENÇAS")

baixado <- todos %>% filter(ANO %in% c(2019, 2020, 2021)) %>% 
  select(UNIDADE, ANO, Mes = DATA, valor = BAIXADOS) %>% 
  mutate(Indicador = "BAIXADOS")


acervo_out <- todos %>% filter(ANO %in% c(2019, 2020, 2021)) %>% 
  select(UNIDADE, ANO, Mes = DATA, valor = ACERVO) %>% 
  mutate(Indicador = "ACERVO")

fluxo_processual <- rbind(saldo, senteca, baixado, acervo_out)

# Dando a saída dos arquivos

saveRDS(todos, file = "PRODUTIVIDADE/indicadores.rds")

saveRDS(taxa, file = "PRODUTIVIDADE/taxa.rds")

saveRDS(distribuidos, file = "PRODUTIVIDADE/distribuidos.rds")

saveRDS(fluxo_processual, file = "PRODUTIVIDADE/fluxo_processual.rds")

