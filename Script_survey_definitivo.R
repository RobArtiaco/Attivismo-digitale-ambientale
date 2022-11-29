library(rio)
library(tidyverse)
library(skimr)
library(dplyr)
library(ggplot2)
library(moments)
library(scales)







#DIVISIONE VARIABILE PROBLEMATICHE
attivismo_new = separate(attivismo_progetto, col= problematiche, into=c('problematica1', 'problematica2'), sep=';') 

#DIVISIONE VARIABILE SOCIAL UTILIZZATI
attivismo_new1 = separate(attivismo_new, col=quale_social, into=c('social1', 'social2','social3','social4','social5','social6'), sep=';')

#DIVISIONE VARIABILE ATTIVITA' ONLINE
attivismo_new2 = attivismo_new1 %>%
  mutate(fruizione_contenuti = 
           ifelse(str_detect(attivismo_new1$attività_online, 
                             'Fruito di contenuti multimediali'),1,0)) %>%
  mutate(seguito_pagine = 
           ifelse(str_detect(attivismo_new1$attività_online, 
                             'Seguito pagine di attivisti o associazioni'),1,0)) %>%
  mutate(info_proteste = 
           ifelse(str_detect(attivismo_new1$attività_online, 
                             'Cercato informazioni su raduni e/o proteste'),1,0)) %>%
  mutate(iscrizione_gruppi = 
           ifelse(str_detect(attivismo_new1$attività_online, 
                             'Iscritto a un gruppo su un social network'),1,0)) %>%
  mutate(hashtags = 
           ifelse(str_detect(attivismo_new1$attività_online, 
                             'Utilizzato hashtag'),1,0)) %>%
  mutate(condiviso_contenuti = 
           ifelse(str_detect(attivismo_new1$attività_online, 
                             'Condiviso contenuti sul tema'),1,0)) %>%
  mutate(firma_petizioni = 
           ifelse(str_detect(attivismo_new1$attività_online, 
                             'Firmato petizioni online su piattaforme apposite'),1,0)) %>%
  mutate(denuncia_abusi = 
           ifelse(str_detect(attivismo_new1$attività_online, 
                             'Segnalato o denunciato abusi ambientali sui social networks'),1,0)) %>%
  mutate(compra_prodotti = 
           ifelse(str_detect(attivismo_new1$attività_online, 
                             "Comprato prodotti online di un'azienda green"),1,0)) %>%
  mutate(creazione_contenuti = 
           ifelse(str_detect(attivismo_new1$attività_online, 
                             'Creato contenuti'),1,0)) %>%
  mutate(donazioni = 
           ifelse(str_detect(attivismo_new1$attività_online, 
                             "Effettuato donazioni online"),1,0)) %>%
  mutate(nessuna = 
           ifelse(str_detect(attivismo_new1$attività_online, 
                             'Nessuna delle precedenti'),1,0))

# RICODIFICA ORE SOCIAL

attivismo_new3 = attivismo_new2 %>%
  mutate(ore_social_ric = case_when(ore_social == "Meno di un'ora" ~ 1,
                                    ore_social == "Da 1 a 2 ore" ~ 2,
                                    ore_social == "Da 2 a 3 ore" ~ 3,
                                    ore_social == "Da 3 a 4 ore" ~ 4,
                                    ore_social == "Oltre 4" ~ 5))



#CREAZIONE VARIABILE PER MACRO-AREE
attivismo_new3 = attivismo_new3 %>%
  mutate(m_area = case_when(CAP >= "10010" & CAP <= "10156" | #Torino
                              CAP>= "12010" & CAP <= "12100" | #Cuneo
                              CAP>= "13010" & CAP <= "13100" | #Vercelli
                              CAP>= "14010" & CAP <= "14100" | #Asti
                              CAP>= "15010" & CAP <= "15100" | #Alessandria
                              CAP>= "13811" & CAP <= "13900" | #Biella
                              CAP>= "28010" & CAP <= "28100" | #Novara
                              CAP>= "28801" & CAP <= "28925" | #Verbano-Cusio-Orsolo 
                              #Lombardia
                              CAP>= "20010" & CAP <= "27100" | #Tutti comuni lombardia tranne Mantova
                              CAP>= "46010" & CAP <= "46100" | #Mantova
                              #Trentino Alto-Adige
                              CAP>= "38010" & CAP <= "39100" |
                              #Valle d'Aosta
                              CAP>= "11010" & CAP <= "11100" |
                              #Veneto
                              CAP>= "30010" & CAP <= "37142" | #Veneto tranne Rovigo
                              CAP>= "45010" & CAP <= "45100" | #Rovigo
                              #Friuli Venezia-Giulia
                              CAP>= "33010" & CAP <= "34170" | 
                              #Liguria
                              CAP>= "16010" & CAP <= "19137" |
                              #Emilia Romagna
                              CAP>= "29010" & CAP <= "29100" | #Piacenza
                              CAP>= "40010" & CAP <= "44100" | #Bologna,Reggio Emilia,Parma,Ferrara
                              CAP>= "47010" & CAP <= "48100" |
                              CAP>= "50010" & CAP <= "59100" #Toscana 
                            ~ 'Nord',
                            #Marche        
                            CAP>= "60010" & CAP <= "63900" |
                              #Umbria 
                              CAP>= "05010" & CAP <= "05100" | 
                              CAP>= "06010" & CAP <= "06135" | #Perugia
                              #Lazio    
                              CAP>= "00010" & CAP <= "00199" | #Roma
                              CAP>= "01010" & CAP <= "04100"  ~ 'Centro',
                            CAP>= "80010" & CAP <= "84135" |
                              #Puglia
                              CAP>= "70010" & CAP <= "74100" |
                              #Basilicata
                              CAP>= "75010" & CAP <= "75100" | #Matera
                              CAP>= "85010" & CAP <= "85100" | #Potenza
                              #Abruzzo
                              CAP>= "64010" & CAP <= "67100" |
                              #Calabria
                              CAP>= "87010" & CAP <= "89900" |
                              #Molise
                              CAP>= "86010" & CAP <= "86170" |
                              CAP>= "90010" & CAP <= "98168" |
                              #Sardegna
                              CAP>= "07010" & CAP <= "09170" ~ 'sud_isole')) %>%
  mutate(m_area = factor(m_area,levels = c('Nord',
                                           'Centro',
                                           'sud_isole')))


#RICODIFICA VARIABILE CLASSI ETA'
attivismo_new3 = attivismo_new3 %>%
  mutate(eta_classe_ric = case_when(età >= 18 & età <= 24  ~ '18-24',
                                    età >= 25 & età <= 35 ~ '25-35',
                                    età >= 36 & età <= 45 ~ '36-45',
                                    età >= 46 & età <= 55 ~ '46-55',
                                    età >= 56 & età <= 65 ~ '56-65',
                                    età >= 66 ~ 'over 66')) %>%
  
  mutate(eta_classe_ric = factor(eta_classe_ric,levels = c('18-24','25-35','36-45','46-55','56-65','over 66'), ordered = T))

#RICODIFICA PROBLEMATICHE
attivismo_new3 = attivismo_new3 %>%
  mutate(problematica_r = case_when(problematica1 == "Cambiamento climatico" ~ problematica2, 
                                    problematica1 != "Cambiamento climatico" ~ problematica1))

attivismo_new3 = attivismo_new3 %>%
  mutate(problematica_o = case_when(problematica_r == " Utilizzo di prodotti chimici che usiamo quotidianamente (detersivi" | 
                                      problematica_r == "Utilizzo di prodotti chimici che usiamo quotidianamente (detersivi" ~ 
                                      "utilizzo prodotti chimici",
                                    problematica_r == "Aumento dei rifiuti" | 
                                      problematica_r == " Aumento dei rifiuti" ~ 
                                      "Aumento rifiuti",
                                    problematica_r == "Impatto ambientale dei mezzi di trasporto (traffico" | 
                                      problematica_r == " Impatto ambientale dei mezzi di trasporto (traffico" ~ 
                                      "Mezzi di trasporto",
                                    problematica_r == "Impoverimento delle risorse naturali" | 
                                      problematica_r == " Impoverimento delle risorse naturali" ~ 
                                      "risorse naturali",
                                    problematica_r == "Inquinamento dei prodotti agricoli" | 
                                      problematica_r == " Inquinamento dei prodotti agricoli" ~ 
                                      "Inquinamento prodotti agricoli",
                                    problematica_r == "Inquinamento dell'aria" | 
                                      problematica_r == " Inquinamento dell'aria" ~ 
                                      "Inquinamento aria",
                                    problematica_r == "Nostre abitudini consumistiche" | 
                                      problematica_r == " Nostre abitudini consumistiche" ~ 
                                      "Abitudini di consumo",
                                    problematica_r == "Deforestazione" | 
                                      problematica_r == " Deforestazione"  ~ 
                                      "Deforestazione",
                                    problematica_r == "Inquinamento delle acque" |
                                      problematica_r == " Inquinamento delle acque" ~ 
                                      "Inquinamento acque"))

#RICODIFICA VARIABILE ORIENTAMENTO POLITICO
attivismo_new3 = attivismo_new3 %>%
  mutate(orientamento_politico = if_else(str_detect(orientamento_politico, "estra"), "Destra", as.character(orientamento_politico))) %>%
  mutate(orientamento_politico = if_else(str_detect(orientamento_politico, "inistra"), "Sinistra", as.character(orientamento_politico))) %>%
  mutate(orientamento_politico = fct_lump_min(orientamento_politico,3))

attivismo_new3 = attivismo_new3 %>%
  mutate(orientamento_politico = if_else(str_detect(orientamento_politico, "Other"), "Altro", as.character(orientamento_politico)))



#DATI NORD-ITALIA
nord_italia = attivismo_new3 %>%
  filter(CAP>=     "10010" & CAP <= "10156"| #Torino
           CAP>= "12010" & CAP <= "12100" | #Cuneo
           CAP>= "13010" & CAP <= "13100" | #Vercelli
           CAP>= "14010" & CAP <= "14100" | #Asti
           CAP>= "15010" & CAP <= "15100" | #Alessandria
           CAP>= "13811" & CAP <= "13900" | #Biella
           CAP>= "28010" & CAP <= "28100" | #Novara
           CAP>= "28801" & CAP <= "28925" | #Verbano-Cusio-Orsola
           #Lombardia
           CAP>= "20010" & CAP <= "27100" | #Tutti comuni lombardia tranne Mantova
           CAP>= "46010" & CAP <= "46100" | #Mantova
           #Trentino Alto-Adige
           CAP>= "38010" & CAP <= "39100" | 
           #Valle d'Aosta
           CAP>= "11010" & CAP <= "11100" | 
           #Veneto
           CAP>= "30010" & CAP <= "37142" | #Veneto tranne Rovigo
           CAP>= "45010" & CAP <= "45100" | #Rovigo
           #Friuli Venezia-Giulia
           CAP>= "33010" & CAP <= "34170" | 
           #Liguria
           CAP>= "16010" & CAP <= "19137" | 
           #Emilia Romagna
           CAP>= "29010" & CAP <= "29100" | #Piacenza
           CAP>= "40010" & CAP <= "44100" | #Bologna,Reggio Emilia,Parma,Ferrara
           CAP>= "47010" & CAP <= "48100" ) #restanti

view(nord_italia)

#DATI CENTRO ITALIA
centro_italia = attivismo_n %>%
  #Toscana
  filter(CAP>= "50010" & CAP <= "59100"| 
           #Marche        
           CAP>= "60010" & CAP <= "63900"| 
           #Umbria 
           CAP>= "05010" & CAP <= "05100"| 
           CAP>= "06010" & CAP <= "06135"| #Perugia 
           #Lazio    
           CAP>= "00010" & CAP <= "00199"| #Roma
           CAP>= "01010" & CAP <= "04100") #restanti 

#DATI SUD ITALIA
sud_italia = attivismo2 %>%
  #Campania
  filter(CAP>= "80010" & CAP <= "84135"|
           #Puglia
           CAP>= "70010" & CAP <= "74100"|
           #Basilicata
           CAP>= "75010" & CAP <= "75100"| #Matera
           CAP>= "85010" & CAP <= "85100"| #Potenza
           #Abruzzo
           CAP>= "64010" & CAP <= "67100"|
           #Calabria
           CAP>= "87010" & CAP <= "89900"|
           #Molise
           CAP>= "86010" & CAP <= "86170")

isole_italia = attivismo2 %>%
  #Sicilia
  filter(CAP>= "90010" & CAP <= "98168"|
           #Sardegna
           CAP>= "07010" & CAP <= "09170")



prova_ponderazione = prova_ponderazione %>%
  mutate(fruizione_contenuti = case_when(
    fruizione_contenuti == 1 ~ 
      round(1-sum(fruizione_contenuti)/748, digits = 2),
    fruizione_contenuti == 0 ~ 0))%>%
  
  mutate(seguito_pagine =case_when(
    seguito_pagine == 1 ~
      round(1-sum(seguito_pagine)/748, digits = 2),
    seguito_pagine == 0 ~ 0)) %>%
  
  mutate(info_proteste =case_when(
    info_proteste == 1 ~
      round(1-sum(info_proteste)/748,digits = 2),
    info_proteste == 0 ~ 0)) %>%
  
  mutate(iscrizione_gruppi =case_when(
    iscrizione_gruppi == 1 ~
      round (1-sum(iscrizione_gruppi)/748,digits = 2),
    iscrizione_gruppi == 0 ~ 0)) %>%
  
  mutate(hashtags =
           case_when(
             hashtags == 1 ~round (1-sum(hashtags)/748,digits = 2),
             hashtags == 0 ~ 0)) %>%
  
  mutate(condiviso_contenuti =case_when(
    condiviso_contenuti == 1 ~
      round (1-sum(condiviso_contenuti)/748,digits = 2),
    condiviso_contenuti == 0 ~ 0)) %>%
  
  mutate(firma_petizioni =case_when(
    firma_petizioni == 1 ~
      round (1-sum(firma_petizioni)/748,digits = 2),
    firma_petizioni == 0 ~ 0)) %>%
  
  mutate(denuncia_abusi =case_when(
    denuncia_abusi == 1 ~
      round (1-sum(denuncia_abusi)/748,digits = 2),
    denuncia_abusi == 0 ~ 0)) %>%
  
  mutate(compra_prodotti =case_when(
    compra_prodotti == 1 ~
      round (1-sum(compra_prodotti)/748,digits = 2),
    compra_prodotti == 0 ~ 0)) %>%
  
  mutate(creazione_contenuti =case_when(
    creazione_contenuti == 1 ~
      round (1-sum(creazione_contenuti)/748,digits = 2),
    creazione_contenuti == 0 ~ 0)) %>%
  
  mutate(donazioni = case_when(
    donazioni == 1 ~
      round(1-sum(donazioni)/748,2),
    donazioni == 0 ~ 0))

prova_ponderazione = prova_ponderazione %>% 
  rowwise()%>%
  mutate(indice_attivismo = round(sum(c(fruizione_contenuti, 
                                        seguito_pagine,
                                        info_proteste,
                                        iscrizione_gruppi,
                                        hashtags,
                                        condiviso_contenuti,
                                        firma_petizioni,
                                        denuncia_abusi,
                                        compra_prodotti,
                                        creazione_contenuti,
                                        donazioni))/11, digits = 2))




