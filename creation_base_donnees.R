### TER L3 MIASHS
### Création de la base de données du projet 
### à partir du dossier 'Data' (Eurostat) et des données de University of Gothenburg

#######################################################################################################

# Repertoire
setwd(dir='E:/TER')


# fichiers du dossier "Data"
filenames <- list.files("Data", pattern="*.csv", full.names=FALSE)

# modele du dataframe (donnees Eurostat)
df <- read.table(file="Data/tauxChomage.csv", header= TRUE, sep=",", dec= ".")
df <- subset(df, select = geo)

# merge des variables des donnees Eurostat
for (file in filenames){
  var <- substr(file,1,nchar(file)-4)
  df_ajout <- read.table(file=paste0("Data/",file), header= TRUE, sep=",", dec= ".")
  df_ajout <- subset(df_ajout, select = c(geo, OBS_VALUE))
  names(df_ajout)[names(df_ajout) == "OBS_VALUE"] <- var
  df <- merge(df,df_ajout,by="geo",all.x = TRUE)
}

# donnees EQI (University of Gothenburg)
dfg<- read.csv("EQI_goth.csv")

# ajout region : Ouest/Est/Nord/Sud
europe_nord <- c("Denmark","Finland","Ireland","Lithuania","Sweden")
europe_ouest <- c("Austria","Belgium","Germany","France","Netherlands")
europe_sud <- c("Greece","Spain","Croatia","Italy","Slovenia","Portugal")
europe_est <- c("Bulgaria","Czech Republic","Hungary","Poland","Romania","Slovakia")
dfg$region[dfg$cname%in%europe_nord] <- "Europe du Nord"
dfg$region[dfg$cname%in%europe_ouest] <- "Europe de l'Ouest"
dfg$region[dfg$cname%in%europe_sud] <- "Europe du Sud"
dfg$region[dfg$cname%in%europe_est] <- "Europe de l'Est"

dfg <- subset(dfg, select = c(region_code,name,cname,region,EQIregionN2,eqi21_n2,eqi21_n2_lowME,eqi21_n2_highME,qualityp21_n2,impartialityp21_n2,corruptionp21_n2))

# merge final ( Donnees Gothenburg + Eurostat)
names(dfg)[1] <- "geo"
dfg <- merge(dfg,df,by="geo",all.x = TRUE)

dfg$pib_hab <- (dfg$pib_par_reg/dfg$pop_densite)*10
dfg$log_pib_hab <- log((dfg$pib_par_reg/dfg$pop_densite)*10)
dfg$lit_hop_par_hab <- dfg$NbLitHopitaux/dfg$pop_densite

# exportation de la base de donnees finale
write.csv(dfg,"data_TER.csv", row.names = FALSE)


