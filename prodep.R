setwd("C:/Users/Claudio/Dropbox/PesquisaProdep")

Base1 <- import("BaseGeralUnico_V2.csv")

library(foreign)
library(polycor)
library(foreign)
library(tidyverse) # pacote base para mexer no R
library(janitor) # pacote para tirar tabulacoes 
library(rio) # pacote para abrir bases
library(dplyr)
library(haven)
library(bit64)
library(mirt)
library(openxlsx)
library(inspectdf)
library(corrplot)

t.test(Base2$Matriculas_em_IES_Total_2010,Base2$IDHM_2010)

mean(Base2$IMPOSTOS_2000)
mean(Base2$PIB_M_1999)

median(Base$POP_2018)

str(Base)

mean(Base$POP_2018)



Base2<-read.xlsx("BaseGeralUnico_V2.xlsx")

mean(Base2$POP_2018)

Base3<-read.csv("BaseGeralUnico_V2.csv",
                header = TRUE,
                sep = ";",
                quote = "",
)

mean(Base3$POP_2018)
mean(Base3$VAB_SERV_1999)
mean(Base3$Escolaridade_2010Total)



Base2010 <- Base1  %>%  
     select(VAB_SERV_2010,
            IMPOSTOS_2010,
            PIB_M_2010,
            VAR_PIB_20102009,
            VAR_PIB_2011_2010,
            Matriculas_em_IES_Total_2010,
            Possui_IES_20002010,
            IDEB_2009,
            IDEB_2011,
            Escolaridade_2010Sem_instru??o_1_ciclo_fundamental_incompleto,
            Escolaridade_20101_ciclo_fundamental_completo_2_ciclo_incompleto,
            Escolaridade_20102_ciclo_fundamental_completo_ou_mais,
            Escolaridade_2010N?o_determinada,
            Escolaridade_2010Total,
            IDHM_2000,
            IDHM_2010
     )

glimpse(Base2)



matriz2010 <- cor(Base2010)
matrizteste <- cor(Base2010$VB2010,Base2010$IMP)
matrizteste2 <- cor(Base2010$VB2010,Base2010$IMP,Base2010$PIB_M_2010)

corrplot(matriz2010, type = "upper")
corrplot(matriz2010, method = "number", type = "upper")

sum(Base$POP_2018)

corrplot(matriz2010, method = "number")

Base2 %>% 
     select(Matriculas_em_IES_Total_2010,IDHM_2010) %>% 
     cor(.)

Base2010 <- Base2010 %>% rename(VB2010=VAB_SERV_2010, IMP=IMPOSTOS_2010)
Base2010 <- Base2010 %>% rename(V1=VB2010,
                                V2=IMP,
                                V3=PIB_M_2010,
                                V4=VAR_PIB_20102009,
                                V5=VAR_PIB_2011_2010,
                                V6=Matriculas_em_IES_Total_2010,
                                V7=Possui_IES_20002010,
                                V8=IDEB_2009,
                                V9=IDEB_2011,
                                V10=Escolaridade_2010Sem_instru??o_1_ciclo_fundamental_incompleto,
                                V11=Escolaridade_20101_ciclo_fundamental_completo_2_ciclo_incompleto,
                                V12=Escolaridade_20102_ciclo_fundamental_completo_ou_mais,
                                V13=Escolaridade_2010N?o_determinada,
                                V14=Escolaridade_2010Total,
                                V15=IDHM_2000,
                                V16=IDHM_2010
)
names(Base2010)

Base2010 %>% 
     tabyl(V10)

Base2010 %>% 
     tabyl(V14)

Base2010 %>%
     summarise(mediav10=mean(V10))

Base2010 %>%
     summarise(mediav10=mean(V3))

correlacao <- cor(Base2010$V3,Base2010$V7)
