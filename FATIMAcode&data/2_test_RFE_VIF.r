# Importer les bibliothèques
library(ggplot2)
library(carData) 
library(caret)
library(dplyr) 
library(randomForest)
library(Hmisc)
library(corrplot)
library(openxlsx)
library(usdm)

#Etape 1: chercher les variables performants

#Importation du tableau de donnees
data_sentinel_sites <- read.csv("C:/Users/HP/OneDrive - Université Cheikh Anta DIOP de DAKAR/Stage CSE_CIRAD/data/exemplerfevif/VI_M2021_herbaceous.csv", header = TRUE)
names(data_sentinel_sites)
data_sentinel_sites[,11]
ms_columns <- data_sentinel_sites[,11]
# s'assurer que les resultats sont reproductibles
set.seed(1)
# fixer les lignes et colonnes
attach(data_sentinel_sites)

#### POUR LE RFE
# define the control using a random forest selection function 
control <- rfeControl(functions=rfFuncs, method="cv", number=5)
# run the RFE algorithm / "data_sentinel_sites[12:19]" = tous les indices de vegetation / "data_sentinel_sites[,4]" = choisi
# la variable a expliquer , ici j'ai choisi le MAT qui se trouve a la 4eme colonne.
results_RFE_BH <- rfe(data_sentinel_sites[1:8], data_sentinel_sites[,11], sizes=c(1:8), rfeControl=control)
# list of the chosen predictors apres RFE, c'est la liste des variables performantes que tu exporteras
predictors_RFE_BH <-predictors(results_RFE_BH)
BD_predictors_RFE_BH<-data_sentinel_sites %>% dplyr::select(all_of(predictors_RFE_BH)) 
write.csv(BD_predictors_RFE_BH, 
          file="C:/Users/HP/OneDrive - Université Cheikh Anta DIOP de DAKAR/Stage CSE_CIRAD/data/exemplerfevif/RFE_BH_SMS.csv", quote=TRUE,
           dec=".", row.names=TRUE, col.names=TRUE)


#2: Application VIF pour la detecter la colinearite entre les variables
#Dans RFE_BH.csv que tu avais exporte, il est necessaire de supprimer la premiere colonne avant de passer aux etapes suivantes
RFE_sentinel_sites_BH<-read.csv("C:/Users/HP/OneDrive - Université Cheikh Anta DIOP de DAKAR/Stage CSE_CIRAD/data/exemplerfevif/RFE_BH_SMS.csv", header = TRUE)
RFE_sentinel_sites_BH
vif<-vifstep(RFE_sentinel_sites_BH[2:7], th=10) # identifier les variables colineaires qui doivent etre exclues
vif
BD_Indices_vif  <-  exclude (RFE_sentinel_sites_BH[2:7] , vif )  # exclure les variables colineaires qui ont ete identifiees 
BD_Indices_vif

#Exporter ce tableau final (MAT avec les variables performantes et non colineaires)

write.csv(BD_Indices_vif, 
          file="C:/Users/HP/OneDrive - Université Cheikh Anta DIOP de DAKAR/Stage CSE_CIRAD/data/exemplerfevif/VIF_BH_SMS.csv", quote=TRUE,
           dec=".", row.names=TRUE, col.names=TRUE)

BD_Indices_CBW_vif<-cbind(BD_Indices_vif,ms_columns)
BD_Indices_CBW_vif
names(BD_Indices_CBW_vif)[7]<-'MAT'
BD_Indices_CBW_vif
write.csv(BD_Indices_CBW_vif, 
          file="C:/Users/HP/OneDrive - Université Cheikh Anta DIOP de DAKAR/Stage CSE_CIRAD/data/exemplerfevif/results_VIF/bd_bh_sms.csv", quote=TRUE,
          dec=".", row.names=TRUE, col.names=TRUE)
# refaire le tout en utilisant les autres variables telles que ADF, NDF, MAT
