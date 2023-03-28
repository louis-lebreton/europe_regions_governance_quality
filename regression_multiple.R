### TER L3 MIASHS
## Regression multiple

######################################################################################################

## Definition de l'emplacement du repertoire de travail
setwd(dir='C:/Users/lebre/OneDrive/Bureau/TER')
getwd()

## Packages
library(ggplot2)
library(ggcorrplot)

## Transformation des données #######################################################################

df <- read.table(file="data_TER.csv",header=TRUE,sep=",")
View(df)

sum(is.na(df)) # 1802 valeurs manquantes
vm <- sort(colMeans(is.na(df)) * 100) # % de valeurs manquantes par variable
vm
# Graphique des valeurs manquantes
vm_df <- data.frame(variable = names(vm), vm)

# Créer le graphique de Pareto
ggplot(data = vm_df, aes(x = reorder(variable, vm), y = vm)) +
  geom_bar(stat="identity", fill='#157989') +
  coord_flip()+
  xlab("Variable du jeu de données de base") +
  ylab("Pourcentage de valeurs manquantes")+
  labs(title = "Pourcentage de valeurs manquantes par variables")+
  theme(axis.text.x = element_text(size=15),axis.text.y = element_text(size=13),
        axis.title.x = element_text(size=16),axis.title.y = element_text(size=16),
        title=element_text(size=15))


# On ne garde que les variables qui ont moins de 17% de valeurs manquantes
var_garde <- colnames(df)[as.vector(which(colMeans(is.na(df) * 100)<17))]
var_garde <- var_garde[5:length(var_garde)] # on ne garde que les vars numériques
# on ne garde pas les var qui composent l'indicateur EQI et celles qui ne nous servent pas
var_garde <- setdiff(var_garde, c("EQIregionN2","eqi21_n2_lowME","eqi21_n2_highME","qualityp21_n2","impartialityp21_n2","corruptionp21_n2","pib_hab","pib_par_reg")) 
length(var_garde) # 8 variables finales

# création d'un data frame avec ces 10 variables numériques
df_num <- df[,var_garde]
sum(is.na(df_num)) # 53 valeurs manquantes
nrow(df_num) # 238 lignes
df_num <- na.omit(df_num)
nrow(df_num) # 228 lignes sans valeurs manquantes
View(df_num)
# Matrice de corrélation ######################################################################

cor_mat <- cor(df_num)
ggcorrplot(cor_mat, hc.order = FALSE, type = "lower",
           lab = TRUE, lab_size = 3,
           title="Matrice de corrélation",
           ggtheme = ggplot2::theme_gray,
           colors = c("red", "white", "darkgreen"))

# Régression linéaire multiple #################################################################

# regression multiple avec 7 variables explicatives
var_expl <- setdiff(var_garde, "eqi21_n2") # on enlève la variable à expliquer des 8 variables
formula_str <- paste("eqi21_n2 ~", paste(var_expl, collapse = "+"))
reg_multiple <- lm(as.formula(formula_str), data = df_num)
summary(reg_multiple)

## Pour sélectionner les variables les plus pertinentes,
## on utilise la Méthode forward stepwise 

######## Avec le critère AIC : Critère d'information d'Akaike ########

# modèle vide
model <- lm(eqi21_n2 ~ 1, data = df_num)

# variables explicatives
var_list <- var_expl 

while (length(var_list) > 0) { # tant que var_list est non vide
  AIC_vec <- c() # vecteur de critères AIC
  for (i in 1:length(var_list)) {
    # formule : eqi21_n2 ~ i eme var
    formule <- as.formula(paste("eqi21_n2 ~", paste(c(attr(terms(model), "term"), var_list[i]), collapse = "+")))
    
    modele_temporaire <- lm(formule , data = df_num) # régression pour chaque variable
    AIC_vec[i] <- AIC(modele_temporaire) # ajout du AIC pour chaque regression
  }
  
  min_AIC_indice <- which.min(AIC_vec) # quelle regression a le plus petit AIC ?
  
  if (AIC_vec[min_AIC_indice] < AIC(model)) { # cette AIC minimum est-il inferieur au AIC de notre modèle actuel ? 
    # si oui: on remplace notre ancien modèle par le nouveau modèle qui est meilleur
    model <- update(model, formula = as.formula(paste("eqi21_n2 ~", paste(c(attr(terms(model), "term"), var_list[min_AIC_indice]), collapse = "+"))))
    var_list <- var_list[-min_AIC_indice] # supprimons donc la variable utilisée du vecteur var_list
  } else {
    # si non: fin de la boucle et on garde le dernier modèle actualisé
    break
  }
}

length(coef(model))-1 # 6 variables
coef(model)
model # modèle final
summary(model)

######## Avec le critère BIC : Critère d'information bayésien ########

# modèle vide
model <- lm(eqi21_n2 ~ 1, data = df_num)

# variables explicatives
var_list <- var_expl 

while (length(var_list) > 0) { # tant que var_list est non vide
  BIC_vec <- c() # vecteur de critères BIC
  for (i in 1:length(var_list)) {
    # formule : eqi21_n2 ~ i eme var
    formule <- as.formula(paste("eqi21_n2 ~", paste(c(attr(terms(model), "term"), var_list[i]), collapse = "+")))
    
    modele_temporaire <- lm(formule , data = df_num) # régression pour chaque variable
    BIC_vec[i] <- BIC(modele_temporaire) # ajout du BIC pour chaque regression
  }
  
  min_BIC_indice <- which.min(BIC_vec) # quelle regression a le plus petit BIC ?
  
  if (BIC_vec[min_BIC_indice] < BIC(model)) { # cette BIC minimum est-il inferieur au BIC de notre modèle actuel ? 
    # si oui: on remplace notre ancien modèle par le nouveau modèle qui est meilleur
    model <- update(model, formula = as.formula(paste("eqi21_n2 ~", paste(c(attr(terms(model), "term"), var_list[min_BIC_indice]), collapse = "+"))))
    var_list <- var_list[-min_BIC_indice] # supprimons donc la variable utilisée du vecteur var_list
  } else {
    # si non: fin de la boucle et on garde le dernier modèle actualisé
    break
  }
}

length(coef(model))-1 # 6 variables
coef(model)
model # modèle final
summary(model)
model$coefficients


var_expl <- setdiff(var_garde, c("eqi21_n2"))
formula_str <- paste("eqi21_n2 ~", paste(var_expl, collapse = "+"))
reg_multiple <- lm(as.formula(formula_str), data = df_num)
summary(reg_multiple)
summary_model2 <- summary(reg_multiple)


summary(model)$adj.r.squared # R2 ajust : 0.611
summary_model2$adj.r.squared  # R2 ajust : 0.609

# Modèle final : model

# Predictions

df <- read.table(file="data_TER.csv",header=TRUE,sep=",")
predict(model,df)



