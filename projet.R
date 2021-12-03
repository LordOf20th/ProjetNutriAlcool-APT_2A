rm(list=ls())
library(knitr) #pour avoir un format table dans les sorties
library(ggplot2) #pour avoir de 'beaux'graphiques
library(FactoMineR) #pour effectuer l'ACP
library(factoextra) #pour extraire et visualiser les résultats issus de FactoMineR
library(tidyverse)
library(corrplot) #pour avoir une représentation des corrélations


## Import et formattage des données
inca2 <- read.csv('inca2_alcool.csv', header = TRUE)


inca2$nomen = as.factor(inca2$nomen)
inca2$sexe_ps = as.factor(inca2$sexe_ps)
levels(inca2$sexe_ps) <- c("homme", "femme")
inca2$reg = as.factor(inca2$reg)
levels(inca2$reg) <- c('Nord Ouest', 'Est', 'Ile de France',
                       'Ouest', 'Centre', 'Centre Est',
                       'Sud Ouest', 'Sud Est')


colnames(inca2)[19:54] <- c("energie_totale","proteines", "glucides_dispos", 
                            "lipides","alcool","ag_monoinsatures", 
                            "ag_polyinsatures", "ag_satures", "amidon",
                            "glucides_simples","fibres","cholesterol","eau",
                            "calcium", "fer", "sodium", "magnesium", 
                            "manganese", "phosphore", "potassium", 
                            "cuivre", "zinc", "selenium", "iode", 
                            "retinol", "beta-carotene", "vitamineC", 
                            "vitamineD", "vitamineE", "thiamine-B1", 
                            "riboflavine-B2", "niacine-B3", 
                            "acide_panthothenique-B5", "pyridoxine-B6", 
                            "folates-B9", "colobamine-B12")

summary(inca2)
str(inca2)

conso <- read.csv(file = 'conso_alcool.csv')

conso$nomen = as.factor(conso$nomen)
conso$nomjour = as.factor(conso$nomjour)
conso$nomgr = as.factor(conso$nomgr)

summary(conso)

## Analyse en composante principale
inca2_numerique = inca2[-c(1,2,4,5)]

correlation_inca2 = cor(inca2_numerique)
corrplot(correlation_inca2)
inca2_pca = PCA(inca2_numerique, scale.unit = TRUE)
kable(get_eigenvalue(inca2_pca))
fviz_eig(inca2_pca)

### Analyse en sous blocs
#### Sous bloc 1 : Par semaine
inca2_sem <- inca2_numerique[c(1:14)]
correlation_inca2_sem = cor(inca2_sem)
corrplot(correlation_inca2_sem)
inca2_sem_pca = PCA(inca2_sem, graph = FALSE, scale.unit = TRUE)
