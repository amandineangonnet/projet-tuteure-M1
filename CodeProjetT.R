#Librairie :
library(tidyverse)
library(readxl) # lire les fichiers excel
library(dplyr)  #L'opérateur "pipe" %>%, fonctions "mutate", "filter"...
library(tidyquant) # données financières
library(ggfortify) # package "autoplot"
library(ggplot2) # graphiques jolies
library(tseries) # test de Phillips-Perron, test KPSS, Test de Jarque-Bera
library(urca) # test de ADF
library(forecast) # ggAcf, ggPacf
library(gridExtra) # poser ensemble les graphiques (dans ce code ggAcf, ggPacf)
library(broom) # Tableaux des résultats pour ARIMA
library(lmtest) # test de DW
library(FinTS) # test ARCH
library(rugarch) # Garch modele
library(plm) # panel modele
library(ggcorrplot) ## test corrélation
library(stargazer)
library(lubridate)
library(modelsummary)
####################################################
####################################################


#Chemin pour les bases de données :
setwd("C:/Users/alixv/OneDrive/Bureau")

####################################################
# Importation bases et transformation du df en df panel pour la suite de l'analyse
####################################################
df = read.csv2("BaseFinale.csv")
paneldf =pdata.frame(df, index = c("Zone", "Annee"))

####################################################
##### Représentation graphique des rendements ######
####################################################


## Graphique des rendements pour chaque pays au cours du temps
ggplot(df, aes(x = Annee, y = Rendement, fill = Zone)) +
  geom_col(width = 0.7) + #Graphique en colonne
  facet_wrap(~ Zone) + #Un graphique par pays
  theme_minimal() + #Theme minimal
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    legend.position = "none"
  ) + #Permet d'orienter le texte de la meilleure des manières
  scale_x_continuous(
    breaks = seq(1995, 2023, by = 5) #Met une date tous les 5 ans pour plus de lisibilité
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 4)
  ) +
  labs(
    title = "Évolution des rendements du blé entre 1995 et 2023 par pays",
    x = "Année",
    y = "Rendement en kg/ha"
  ) 

######################################################################
######################################################################
####### Création de la matrice de corrélation en les variables #######
######################################################################

sapply(df, class) ## observation type de chiffrage (vérifier si numeric ou non)
df_num <- df[, sapply(df, is.numeric)] ## transformation des données en numéric
df_num = df_num[,-1] # retrait de la colonne année pour le calcul de la matrice de corrélation

# Calcul de la matrice de corrélation (Spearman)
corr <- round(cor(df_num, method = "spearman", use = "pairwise.complete.obs"), 1)

# Calcul des p-valeurs
pvalue <- cor_pmat(df_num)

# Représentation de la matrice
ggcorrplot(corr,
           lab = TRUE,                     
           lab_size = 4,                   
           p.mat = pvalue,                 
           colors = c("tomato2", "white", "green3"),  
           title = "",
           ggtheme = ggthemes::theme_clean(base_size=5)) +  # Thème épuré
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),                        
    legend.position = "none"                                     
  )

##########################################################################
##########################################################################
############################ ANALYSE DE PANEL ############################
##########################################################################
##########################################################################

## Pooling : OLS classique pas de prise en compte des effets individuels
Reg_Pooling = plm(Rendement~ Surface + Precipitation + Jourfroid
                  +Irrigation+Jourchaud+Insecticides+Herbicides+Fongicides+FBCF+Azote+Potasse+Phosphate,
                  data = paneldf,index=c("Zone", "Annee"), model="pooling")
summary(Reg_Pooling)

## BP Test : On cherche à savoir s'il est pertient de construire un autre modèle qu'un simple OLS
plmtest(Reg_Pooling, type = "bp")
### Le test montre qu'il y a davantage d'information à tirer d'un modèle de panel
####################################################

## Within : Effets fixes prise en compte des différences selon le pays
Reg_Within = plm(Rendement~ Surface + Precipitation + Jourfroid
                 +Irrigation+Jourchaud+Insecticides+Herbicides+Fongicides+FBCF+Azote+Potasse+Phosphate,
                 data = paneldf,index=c("Zone", "Annee"), model="within")
summary(Reg_Within)

## OLS effets fixes par pays (Within fait différement)

Reg_OLS = lm(Rendement~ Surface + Precipitation + Jourfroid
             +Irrigation+Jourchaud+Insecticides+Herbicides+Fongicides+FBCF+Azote+Potasse+Phosphate+as.factor(Zone),
             data = paneldf)
summary(Reg_OLS)

## Between : Modèle académique sans grand intérêt très rarement utilisé 
Reg_Betw = plm(Rendement~ Surface + Precipitation + Jourfroid
               +Irrigation+Jourchaud+Insecticides+Herbicides+Fongicides+FBCF+Azote+Potasse+Phosphate,
               data = paneldf,index=c("Zone", "Annee"), model="between")
summary(Reg_Betw)

## Random : Effets aléatoires, les effets individuels sont considérés comme un tirage d'un distribution
###           le modèle cherche à estimer cette distribution

Reg_Random = plm(Rendement~ Surface + Precipitation + Jourfroid
                 +Irrigation+Jourchaud+Insecticides+Herbicides+Fongicides+FBCF+Azote+Potasse+Phosphate,
               data = paneldf,index=c("Zone", "Annee"), model="random", random.method="amemiya")
summary(Reg_Random)

####################################################

## Hausman Test : test la présence d'un problème d'endogénéité dans les modèles liées à l'omission de variables
###               aux caractéristiques individuelles 

phtest(Reg_Within,Reg_Random)

#On ne rejette pas H0. Les deux sont convergents, on va préférer random effect model car plus efficace.

####################################################
## Modèle (1) chosi :

Reg_Random = plm(Rendement~ Surface + Precipitation + Jourfroid
                 +Irrigation+Jourchaud+Insecticides+Herbicides+Fongicides+FBCF+Azote+Potasse+Phosphate,
                 data = paneldf,index=c("Zone", "Annee"), model="random", random.method="amemiya")
summary(Reg_Random)

####################################################

## Erreurs Robustes : correction des p-value avec des erreurs standard robustes
coeftest(Reg_Random, vcov = vcovHC(Reg_Random, type="HC1", cluster="group"))

###############################################################
###### Changement de plan temporel : 3 premiers trimestres ####
###############################################################

##Modèle pour les trois premiers trimestres

Reg_Pooling3 = plm(Rendement~ Surface + Precipitation_3Tri + JourFroid_3Tri
                  +Irrigation+JourChaud_3Tri+Insecticides+Herbicides+Fongicides+FBCF+Azote+Potasse+Phosphate,
                  data = paneldf,index=c("Zone", "Annee"), model="pooling")
summary(Reg_Pooling3)

plmtest(Reg_Pooling3, type = "bp")

Reg_Within3 = plm(Rendement~ Surface + Precipitation_3Tri + JourFroid_3Tri
                 +Irrigation+JourChaud_3Tri+Insecticides+Herbicides+Fongicides+FBCF+Azote+Potasse+Phosphate,
                 data = paneldf,index=c("Zone", "Annee"), model="within")
summary(Reg_Within3)

Reg_Random3 = plm(Rendement~ Surface + Precipitation_3Tri + JourFroid_3Tri
                 +Irrigation+JourChaud_3Tri+Insecticides+Herbicides+Fongicides+FBCF+Azote+Potasse+Phosphate,
                 data = paneldf,index=c("Zone", "Annee"), model="random", random.method="amemiya")
summary(Reg_Random3)

## Hausman Test : Test la convergence des estimateurs Within et Random
phtest(Reg_Within3,Reg_Random3)

### Choix : Random effect

###############################################################
###### Changement de plan temporel : 2 premiers trimestres ####
###############################################################

## Modèle pour les deux premiers trimestres 

Reg_Pooling2 = plm(Rendement~ Surface + Precipitation_2Tri + JourFroid_2Tri
                  +Irrigation+JourChaud_2Tri+Insecticides+Herbicides+Fongicides+FBCF+Azote+Potasse+Phosphate,
                  data = paneldf,index=c("Zone", "Annee"), model="pooling")
summary(Reg_Pooling2)

plmtest(Reg_Pooling2, type = "bp")

Reg_Within2 = plm(Rendement~ Surface + Precipitation_2Tri + JourFroid_2Tri
                 +Irrigation+JourChaud_2Tri+Insecticides+Herbicides+Fongicides+FBCF+Azote+Potasse+Phosphate,
                 data = paneldf,index=c("Zone", "Annee"), model="within")
summary(Reg_Within2)

Reg_Random2 = plm(Rendement~ Surface + Precipitation_2Tri + JourFroid_2Tri
                 +Irrigation+JourChaud_2Tri+Insecticides+Herbicides+Fongicides+FBCF+Azote+Potasse+Phosphate,
                 data = paneldf,index=c("Zone", "Annee"), model="random", random.method="amemiya")
summary(Reg_Random2)


## Hausman Test : Test la convergence des estimateurs Within et Random

phtest(Reg_Within2,Reg_Random2)

### Choix : Random effect

##########################################################################
##########################################################################
##########################################################################




##########################################################################
######################## Modèles pour l'analyse ##########################
##########################################################################

## Modele Random pour données Annuelles
Reg_Random = plm(Rendement~ Surface + Precipitation + Jourfroid
                 +Jourchaud+Irrigation+Insecticides+Herbicides+Fongicides+FBCF+Azote+Potasse+Phosphate,
                 data = paneldf,index=c("Zone", "Annee"), model="random", random.method="amemiya")
summary(Reg_Random)

coeftest(Reg_Random,vcov = vcovHC(Reg_Random, type = "HC1", cluster = "group"))

## Modele Random pour données 3tri

Reg_Random3 = plm(Rendement~ Surface + Precipitation_3Tri + JourFroid_3Tri
                  +JourChaud_3Tri+Irrigation+Insecticides+Herbicides+Fongicides+FBCF+Azote+Potasse+Phosphate,
                  data = paneldf,index=c("Zone", "Annee"), model="random", random.method="amemiya")
summary(Reg_Random3)

coeftest(Reg_Random3,vcov = vcovHC(Reg_Random3, type = "HC1", cluster = "group"))

## Modele Random pour données 2tri
Reg_Random2 = plm(Rendement~ Surface + Precipitation_2Tri + JourFroid_2Tri
                  +JourChaud_2Tri+Irrigation+Insecticides+Herbicides+Fongicides+FBCF+Azote+Potasse+Phosphate,
                  data = paneldf,index=c("Zone", "Annee"), model="random", random.method="amemiya")
summary(Reg_Random2)

coeftest(Reg_Random2,vcov = vcovHC(Reg_Random2, type = "HC1", cluster = "group"))



###Tableau de statistiques descriptives
# 1. Je prend un dataframe auxquels je retire la colonne Année, Zone
dt2=df%>%
  select(Rendement, 
         Precipitation, Jourfroid, Jourchaud,
         Precipitation_2Tri, JourFroid_2Tri, JourChaud_2Tri, 
         Precipitation_3Tri, JourFroid_3Tri, JourChaud_3Tri,
         FBCF, Irrigation, Surface, 
         Fongicides, Herbicides, Insecticides,  
         Azote, Phosphate)

# 2. On sélectionne ces colonnes et on génère le tableau
options(
  OutDec = ",",     # virgule décimale
  scipen = 999      # pas de notation scientifique
)
datasummary(
  All(dt2) ~ 
    N + Mean + SD + Min + Max,
  data = dt2,
  fmt = function(x) format(x, digits = 3, nsmall = 0, decimal.mark = ","),
  output = "latex",
  title = "Statistiques descriptives des variables."
)

##Faire un dataframe pour engrais potasse car il ne possède que 289 observations
dt3=df%>%
  select(Potasse)%>%
  drop_na()
options(
  OutDec = ",",     # virgule décimale
  scipen = 999      # pas de notation scientifique
)
datasummary(
  All(dt3) ~ 
    N + Mean + SD + Min + Max,
  data = dt3,
  fmt = function(x) format(x, digits = 3, nsmall = 0, decimal.mark = ","),
  output = "latex",
  title = "Statistiques descriptives des variables."
)






