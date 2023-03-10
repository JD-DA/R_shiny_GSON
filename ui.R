
library(shiny)
library(shinyjs)

shinyUI(
        navbarPage("Perceptron",
                   id="mainNavBar",
                   useShinyjs(),
                   tabPanel("Menu",
                            value = "menu",
                            #menu with two big buttons
                            fluidRow(
                              column(6,wellPanel(
                                h3("1. C\'est quoi un réseau de neurones ?"),
                                #description
                                p("Réseau de neuronnes, deep learning, machine learning, 
                perceptrons... Vous saurez tout sur ces concepts et bien
                 plus encore !"),
                                p("Un réseau de neurones est un modèle mathématique qui 
                permet de modéliser des processus complexes. Il est 
                constitué de neurones artificiels, qui sont des fonctions
                 mathématiques simples, mais qui peuvent être combinées 
                 de manière complexe pour modéliser des processus complexes. 
                 Les réseaux de neurones sont utilisés dans de nombreux 
                 domaines, comme la reconnaissance d\'images, la traduction 
                 automatique, la prédiction de séries temporelles, etc."),
                                #image
                                img(src="./neuron.png",width="50%"),
                                actionButton('cours1','Je veux savoir !')
                              )),
                              column(6,wellPanel(
                                h3('Entrainer un réseau de neurones'),
                                actionButton('train','Je veux entrainer un réseau !')
                              ))
                            )
                   ),
                   navbarMenu("Cours",
                              tabPanel("Perceptron",
                                       value="perceptron",
                                       fluidRow(
                                         column(10,wellPanel(
                                           h3("1. Perceptron"),
                                           p("La brique de base d'un réseau de neuronne est... le neurone."),
                                           p("Comme souvent en Intelligence Artificielle on cherche à mimer le fonctionnement du raisonnement humain. C'est donc tout naturellement que l'on cherche à moddéliser la brique la plus élémentaire: le neurone"),
                                           img(src="./neuronOrga.jpg",width="20%",style="display: block; margin-left: auto; margin-right: auto;"),
                                           p("Un neurone est composé d'un noyau auxquels sont reliés des dentrites. Ceux ci vont recevoir des signaux éléctrochimique, les transmettre au noyau qui va a son tour envoyer des signaux."),
                                           p("Un neurone en informatique est donc une simple modélisation de cette agrégation de signaux d'entrée et d'un calcul d'un sortie. Il suit cette formule :"),
                                           uiOutput("formuleNeurone"),
                                           img(src="./neuron.png",width="20%",style="display: block; margin-left: auto; margin-right: auto;"),
                                           p("On peut utiliser ce neurone, seul, en qualité de perceptron. Un perceptron est un algorithme d'apprentissage supervisé de clasifeur binaire. Il permet de séparer des données linérairement séparables. En prenant en entrée chaque dimension il va chercher un hyperplan qui va séparer les deux classes de nos données dans la perspectives de pouvoir classifier les prochains exemples à leurs tour."),
                                           actionButton('goToHandtrain','Séparer des données')
                                         ))
                                       )
                              ),
                              tabPanel("Entrainer à la main",
                                       value = "handtrain",
                                       sidebarLayout(
                                         sidebarPanel(
                                           
                                           sliderInput("a_hand","A:",min = -1,max = 2,value = -1,step=0.1),
                                           sliderInput("b_hand","B:",min = -10,max = 10,value = 8,step=0.1),
                                           verbatimTextOutput("value"),
                                           actionButton('handtrainNext','Continuer')
                                         ),
                                         
                                         # Show a plot of the generated distribution
                                         mainPanel(
                                           plotOutput("handPlot"),
                                           

                                         )
                                         
                                       )
                                       ),
                              tabPanel("Auto-entrainement",
                                       value = "autotrain",
                                       sidebarLayout(
                                         sidebarPanel(
                                           selectInput(inputId='auto_dataset', label='Choisissez un set de données :',
                                                       choices=c('Exemple 1','Exemple 2','AND','XOR','Iris') ,multiple = FALSE, selected='N(0,1)'),
                                           fileInput('importAuto','Select a txt file to import'),
                                           sliderInput("a_auto","A:",min = -10,max = 10,value = 3),
                                           sliderInput("b_auto","B:",min = -10,max = 10,value = -3),
                                           actionButton('launchPerceptron','Lancer le perceptron'),
                                           verbatimTextOutput("outputPerceptron"),
                                           
                                         ),
                                         
                                         # Show a plot of the generated distribution
                                         mainPanel(
                                           plotOutput("autoPlot"),
                                           downloadButton('launchPerceptronReport','Générer un rapport'),
                                         )
                                       )
                                      ),
                              #tabPanel("Réseaux de neuronnes")),
                   
                   
                   
))
