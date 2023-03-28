### TER L3 MIASHS
## Méthode des k-means

###############################################################################################################

## Definition de l'emplacement du repertoire de travail
setwd(dir='C:/Users/lebre/OneDrive/Bureau/TER')
getwd()

## Packages
library(ggplot2)
library(stats)

## Transformation des données ################################################################################

df <- read.table(file="data_TER.csv",header=TRUE,sep=",")

# variables gardées
var_garde <- c("name","cname","region","eqi21_n2","jeuneSansEmploiFormation","mort_infantile","nbPop_1erJanv","pib_par_reg","pop_densite","popNivParEtude25_64","tauxChomage")  
df <- df[,var_garde]
nrow(df) # 238 lignes
df <- na.omit(df)
nrow(df) # 228 lignes sans valeurs manquantes

# Variables numériques 
df_num <- df[,c("eqi21_n2","jeuneSansEmploiFormation","mort_infantile","nbPop_1erJanv","pib_par_reg","pop_densite","popNivParEtude25_64","tauxChomage")]

## Méthode des k-means #######################################################################################

set.seed(123)  # pour rendre les résultats reproductibles

# Elbow method pour déterminer le nombre clusters à choisir

max_k <- 10 # nombre maximum de clusters à tester
sse <- c() # vecteur de SSE : somme des carrés des écarts 

for (k in 1:max_k) {
  km <- kmeans(df_num, centers=k, nstart=25) # nstart : nombre de centres de clusters initiaux différents
  sse[k] <- km$tot.withinss # on ajoute le SSE de chaque kmeans
}
# représentation des SSE 
plot(1:max_k, sse, type="b", xlab="Nombre de clusters", ylab="SSE") # on choisit 4 clusters

# Méthode des k-means avec 4 clusters
kmeans_result <- kmeans(df_num, centers = 4) 

kmeans_result$centers # afficher les centres de chaque cluster
table(kmeans_result$cluster) # nombre d'observations par clusters

df$cluster <- as.factor(kmeans_result$cluster) # ajout des clusters au df de base

nuts2_clusters <- split(df$name, kmeans_result$cluster) # Régions NUTS 2 par cluster
nuts2_clusters
pays_clusters <- split(df$cname, kmeans_result$cluster) # Pays des régions  par cluster
lapply(pays_clusters,table)
zones_clusters <- split(df$region, kmeans_result$cluster) # Zone de l'Europe des régions  par cluster
lapply(zones_clusters,table)

# Ajout des clusters au data frame : "data_TER.csv"
df_clusters <- subset(df, select = c("name", "cluster"))
data_TER <- read.csv("data_TER.csv")
data_TER <- merge(data_TER,df_clusters,by="name",all.x=TRUE)

# exportation de la base de donnees finale
write.csv(data_TER,"data_TER.csv", row.names = FALSE)
