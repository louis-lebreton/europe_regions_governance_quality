### Application R Shiny
### Comparaison géographique de deux variables 
### à partir des fichiers "geo_tidy.csv" et "description_var.csv"
### Choix de 2 variables à représenter

#######################################################################################################

## Repertoire
setwd(dir='C:/Users/lebre/OneDrive/Bureau/TER')

## Packages
library(shiny)
library(shinythemes)
library(ggplot2)
library(viridis)
library(dplyr)
library(rsconnect)

#######################################################################################################

## Data
geo_tidy <- read.csv("geo_tidy.csv")
geo_tidy$cluster <- factor(geo_tidy$cluster)
# choix des variables à supprimer
geo_tidy <- geo_tidy[, !(names(geo_tidy) %in% c("persSantePour1000Habs","pauvreteExclusion","pib_par_reg","pop_densite","tauxRisquePauvrete","lit_hop_par_hab","coutTravail",
                                                "crime_cambriolage","crime_homicide_intentionnel","crime_vol_qualifie","crime_vol_vehicule","EQIregionN2","eqi21_n2_lowME",
                                                "eqi21_n2_highME","dureeMoySejHopitaux","mort_infantile","corruptionp21_n2","NbLitHopitaux"))]

data_TER <- read.csv("data_TER.csv")
description_var <- read.csv("description_var.csv",sep=";")

# variables observées
var <- colnames(geo_tidy)[11:length(colnames(geo_tidy))]

#######################################################################################################

## creation de l'interface Shiny
ui <- fluidPage(
  
  sidebarPanel(
    titlePanel("Régions européennes\nComparaison géographique\nde deux variables"),
    fluidRow(column(6,
            selectInput(inputId = "Var1",
                        label = "Choix de la première variable",
                        choices = var,
                        selected = "eqi21_n2",
                        width = "220px")),
    column(6,
           selectInput(inputId = "Var2",
                       label = "Choix de la seconde variable",
                       choices = var,
                       selected = "accesInternetMenage",
                       width = "220px"))),
    
      checkboxGroupInput(inputId = "Region",
                        label = "Choix des régions:",
                        choices = c("Europe du Nord","Europe du Sud","Europe de l'Ouest","Europe de l'Est"),
                        selected = c("Europe du Nord","Europe du Sud","Europe de l'Ouest","Europe de l'Est"),
                        inline=FALSE),
      fluidRow(column(6,
                      selectInput(inputId = "Palette",
                                  label = "Choix de la première palette de couleurs",
                                  choices = c("Dégradé bleu","Dégradé bleu inversé","Viridis","Viridis inversé","Rouge-Bleu","4 clusters K-Means"),
                                  selected = "Rouge-Bleu",
                                  width = "220px")),
             column(6,
                    selectInput(inputId = "Palette2",
                                label = "Choix de la seconde palette de couleurs",
                                choices = c("Dégradé bleu","Dégradé bleu inversé","Viridis","Viridis inversé","Rouge-Bleu","4 clusters K-Means"),
                                selected = "Rouge-Bleu",
                                width = "220px")))),
  tabPanel("Carte", fluidRow(
    column(3, mainPanel(plotOutput("carte1"))),
    column(3, mainPanel(plotOutput("carte2"))))
      ),
  textOutput("text_var1"),
  textOutput("text_var2"),
  textOutput("text_space"),
  textOutput("text_titre"),
  textOutput("text_coeff"),
  textOutput("text_r2"),
  textOutput("text_pvalue")
  )

## server Shiny
server <- function(input, output, session){
  
  # transformation reactive du data frame
  # choix des régions : Europe du Sud, Europe du Nord, etc
  geo_tidy_finder <- reactive({
    req(input$Region)
    filter(geo_tidy, region %in% input$Region)
  })
  
  data_TER_finder <- reactive({
    req(input$Region)
    filter(data_TER, region %in% input$Region)
  })
  
  # choix des variables à décrire / texte à afficher
  description_var_finder1 <- reactive({
    req(input$Var1)
    filter(description_var, var==input$Var1)
  })
  
  description_var_finder2 <- reactive({
    req(input$Var2)
    filter(description_var, var==input$Var2)
  })
  
  
  # Texte à afficher en bas à gauche de l'application
  output$text_var1 <- renderText({paste(input$Var1,":",description_var_finder1()[1,3])})
  output$text_var2 <- renderText({paste(input$Var2,":",description_var_finder2()[1,3])})
  output$text_space <- renderText({paste("--------------------------------------------------------------------------------------------------------")})
  output$text_titre <- renderText({paste("Régression linéaire simple :",input$Var1, "~", input$Var2)})
  output$text_coeff <- renderText({
    # Regression linéaire simple entre les deux variables
    formula <- as.formula(paste(input$Var1, "~", input$Var2))
    lm_model <- lm(formula, data = data_TER_finder())
    summary_lm <- summary(lm_model)
    # intervalles de confiance à 95%
    confidence1 <- round(as.numeric(confint(lm_model)[2]),2)
    confidence2 <- round(as.numeric(confint(lm_model)[4]),2)
    paste("> Coefficients : B0: ",round(as.numeric(coef(lm_model)[1],2)),"; B1: ",round(as.numeric(coef(lm_model)[2]),2),"+/- [",confidence1,";",confidence2,"] (95%)")
  })
  output$text_r2 <- renderText({
    # Regression linéaire simple entre les deux variables
    formula <- as.formula(paste(input$Var1, "~", input$Var2))
    summary_lm <- summary(lm(formula, data = data_TER_finder()))
    paste("> R2 : ", round(summary_lm$r.squared, 2))
  })
  output$text_pvalue <- renderText({
    # Regression linéaire simple entre les deux variables
    formula <- as.formula(paste(input$Var1, "~", input$Var2))
    summary_lm <- summary(lm(formula, data = data_TER_finder()))
    paste("> P-value : ", paste(round(summary_lm$coefficients[8],3), collapse = ", "))
  })
  
  
  # carte 1 : en choisissant une variable en particulier
  output$carte1 <- renderPlot(
    width = 350,
    height = 600,
    {
    
    # insertion : reactive 
    input$Palette
    input$Var1
    # carte
    ggplot() +
    # insertion de la variable 1
    geom_polygon(data = geo_tidy_finder(), aes_string(fill = input$Var1, x = "long", y = "lat", group = "group"),size=1, alpha=1) +
    theme_void()+
    coord_map()+
    # choix de couleurs
    {if(input$Palette == "Dégradé bleu") scale_fill_viridis(name="Index",option = "mako")}+
    {if(input$Palette == "Dégradé bleu inversé") scale_fill_viridis(name="Index",option = "mako",direction = -1)}+
    {if(input$Palette == "Viridis") scale_fill_viridis(name="Index",option = "viridis")}+
    {if(input$Palette == "Viridis inversé") scale_fill_viridis(name="Index",option = "viridis",direction = -1)}+
    {if(input$Palette == "Rouge-Bleu") scale_fill_gradient2(name="Index",low = "brown", mid = "#F7F9A8", high = "blue", midpoint = mean(geo_tidy_finder()[, input$Var1],na.rm = TRUE))}+
    {if(input$Palette == "4 clusters K-Means") scale_discrete_manual("fill", values = c("#FFB200", "#EEACF0", "#B8F0AC","#2DB2BB"),na.value="#555555")}+
    # legende
    labs(
      title = input$Var1 , #input$Var1
      subtitle = "Régions NUTS 2 - Année 2021",
      caption = paste0("Données: ",description_var_finder1()[1,2],"           "))+
    # theme de legende
    theme(text = element_text(color = "#22211d"),
          plot.background = element_rect(fill = "#f5f5f2", color = NA),
          panel.background = element_rect(fill = "#f5f5f2", color = NA),
          legend.background = element_rect(fill = "#f5f5f2", color = NA),
          
          plot.title = element_text(size= 18, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
          plot.subtitle = element_text(size= 15, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
          plot.caption = element_text( size=10, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
          
          legend.position = c(0.86, 0.35))})
  # carte 2 : en choisissant une variable en particulier
  output$carte2 <- renderPlot(
    width = 350,
    height = 600,
    {
    # insertion : reactive 
    input$Palette
    input$Var2
    # carte
    ggplot() +
      # insertion de la variable 2
      geom_polygon(data = geo_tidy_finder(), aes_string(fill = input$Var2, x = "long", y = "lat", group = "group"),size=1, alpha=1) +
      theme_void()+
      coord_map()+
      # choix de couleurs
      {if(input$Palette2 == "Dégradé bleu") scale_fill_viridis(name="Index",option = "mako")}+
      {if(input$Palette2 == "Dégradé bleu inversé") scale_fill_viridis(name="Index",option = "mako",direction = -1)}+
      {if(input$Palette2 == "Viridis") scale_fill_viridis(name="Index",option = "viridis")}+
      {if(input$Palette2 == "Viridis inversé") scale_fill_viridis(name="Index",option = "viridis",direction = -1)}+
      {if(input$Palette2 == "Rouge-Bleu") scale_fill_gradient2(name="Index",low = "brown", mid = "#F7F9A8", high = "blue", midpoint = mean(geo_tidy_finder()[, input$Var2],na.rm = TRUE))}+
      {if(input$Palette == "4 clusters K-Means") scale_discrete_manual("fill", values = c("#FFB200", "#EEACF0", "#B8F0AC","#2DB2BB"),na.value="#555555")}+
      # legende
      labs(
        title = input$Var2 , #input$Var2
        subtitle = "Régions NUTS 2 - Année 2021",
        caption = paste0("Données: ",description_var_finder2()[1,2],"           "))+
      # theme de legende
      theme(text = element_text(color = "#22211d"),
            plot.background = element_rect(fill = "#f5f5f2", color = NA),
            panel.background = element_rect(fill = "#f5f5f2", color = NA),
            legend.background = element_rect(fill = "#f5f5f2", color = NA),
            
            plot.title = element_text(size= 18, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
            plot.subtitle = element_text(size= 15, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
            plot.caption = element_text( size=10, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
            
            legend.position = c(0.9, 0.35))})
}

## execution de l'application Shiny
shinyApp(ui, server)

