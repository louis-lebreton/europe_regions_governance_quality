### TER L3 MIASHS
### Cartographie à partir des fichiers "data_TER.csv" et "nuts2.geojson"
### Choix d'une variable à représenter

#######################################################################################################

# Repertoire
setwd(dir='C:/Users/lebre/OneDrive/Bureau/TER')

# Packages 
library(geojsonio) # pour ouverture des fichiers geojson
library(broom) # pour utiliser la fonction tidy
library(ggplot2)
library(dplyr)
library(viridis) # pour palettes de couleurs

#######################################################################################################

# df : représentation spatiale (latitude + longitude) des régions NUTS2
geo <- geojson_read("nuts2.geojson",  what = "sp")

geo_tidy <- tidy(geo,region="id")
geo_tidy <- geo_tidy[(geo_tidy$long>(-13))&(geo_tidy$long<40)&(geo_tidy$lat<70)&(geo_tidy$lat>30),]
geo_tidy <- geo_tidy[nchar(geo_tidy$id)==4,] # seulement NUTS2 

# fond de carte obtenu
ggplot() +
  geom_polygon(data = geo_tidy, aes( x = long, y = lat, group = group), fill="white", color="grey") +
  theme_void() +
  coord_map()

# join: geo tidy + la base de donnees
df<- read.csv("data_TER.csv")

geo_tidy = geo_tidy %>%
  left_join(. , df, by=c("id"="geo"))

write.csv(geo_tidy,"geo_tidy.csv", row.names = FALSE)

#######################################################################################################
geo_tidy <- read.csv("geo_tidy.csv")
geo_tidy$cluster <- factor(geo_tidy$cluster) # faire les k-means avant pour que ça marche


# carte : en choisissant une variable en particulier
carte <- ggplot() +
  geom_polygon(data = geo_tidy, aes_string(fill = "res_abs", x = "long", y = "lat", group = "group"),size=1, alpha=1) +
  theme_void()+
  coord_map()+
  # choix de couleurs
  #scale_discrete_manual("fill", values = c("#FFB200", "#EEACF0", "#B8F0AC","#2DB2BB"),na.value="#555555")+
  #scale_fill_viridis(name="Taux pour \n1 000 enfants \nnés vivants",option = "turbo",direction = 1) + # ou direction = -1
  scale_fill_gradient2(name="Taux (en %)",low = "blue", mid = "#F7F9A8", high = "brown", midpoint = mean(df$res_abs,na.rm=TRUE))+
  # legende
  labs(
    title = "Taux de mortalité infantile",
    subtitle = "Régions NUTS 2 - Année 2021",
    caption = "Données: Eurostat")+  # University of Gothenburg ou Eurostat
  # theme de legende
  theme(text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 18, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 15, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=10, color = "#4e4d47", margin = margin(b = 0.3, r=0, unit = "cm") ),
    
    legend.position = c(0.9, 0.35))
carte

# exportation de la carte dans le dossier 'Cartes'
ggsave(carte, file="Cartes/taux_mort_infant_2021.jpg", width = 12, height = 7)
