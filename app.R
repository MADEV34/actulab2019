# Développement d'une solution de retraite - ACTU-LAB

library(shiny)


## Interface
ui <- fluidPage(navbarPage("Actulab 2019 - iA", tabPanel("Problématique",fluidRow(column(6, includeMarkdown("about.Rmd")))),
                           
                           
                           tabPanel("Exemple dynamique",
  
  ## Titre
  titlePanel("Développement d'une solution de retraite"),
  
  helpText("Exemple dynamique permettant de mieux illustrer l'idée quantitativement"),
  
  ## Barre latérale avec les contrôles
  sidebarLayout(
    sidebarPanel(
      numericInput("amount", "Montant annuel de la rente :", value = 25000, min = 0, max = 100000, step = 1000),  
      sliderInput("pourcentage", "Pourcentage de la rente utilisé :", value = 75, min = 0, max = 100, step = 2),
      sliderInput("extern", "Contribution annuelle supplémentaire au fonds (en % de la rente annuelle) :", value = 0, min = 0, max = 10, step = 0.2)
    ),
    
    ## Graphique et résultat en format texte
    mainPanel(plotOutput("init"), plotOutput("final"), textOutput("som"))
  )
)))


## Serveur
server <- function(input, output) 
{
  ## Préparation des données
  amount <- reactive({input$amount})
  pourcentageused <- reactive({input$pourcentage/100})
  unusedamount <- reactive({(1 - pourcentageused())*amount()})
  amountextern <- reactive({input$extern/100 * amount()})
  bonification <- reactive({0.05 * (amountextern() + unusedamount())})
  
  
  ## Création des graphiques
  output$init <- renderPlot({pie(c(pourcentageused(), 1-pourcentageused()), labels = paste(c("Rente Utilisé", "Somme restant au fonds"), c((pourcentageused())*amount(),(1-pourcentageused())*amount()), "$"), main = "Montants au fonds durant l'année", col = c("white", "dodgerblue3"))})
  output$final <- renderPlot({text(x = barplot(c(unusedamount(), amountextern(), bonification()), xlab="", ylab="", yaxt='n', main = "Répartition du fonds en fin d'année", names.arg=c("Montant de rente non utilisé","Contribution externe","Bonification"), col = c("dodgerblue3", "dimgrey", "firebrick1")), y = c(unusedamount(), amountextern(), bonification()), label = c(unusedamount(), amountextern(), bonification()), pos = 1, cex = 1.3, col = "white" )})
  
  ## Sommes du fonds en fin d'année
  output$som <- renderText({paste("Somme du fonds en fin d'année :", format(unusedamount()+amountextern()+bonification(), big.mark =" "), "$")})

}

## Application
shinyApp(ui = ui, server = server)
