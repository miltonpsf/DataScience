library(shiny)

# Define UI for random distribution application 
shinyUI(fluidPage( 
    
  # Application title
  titlePanel("Inferência em Grafos Aleatórios Exponenciais : Bayesiano X ABC"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel( width = 2,
        selectInput("selecionaVertices", label = h3("Vértices"), width = "100px",
        choices = list("50" = 50
                       ), selected = 1) ,
        selectInput("selecionaCenario", label = h3("Cenários"), width = "175px",
                    
        choices = list("theta1=0,80;  theta2=0,02" = 802,
                       "theta1=0,20;  theta2=0,20"  = 22,
                       "theta1=0,50;  theta2=0,35" = 535,
                       "theta1=0,90;  theta2=0,02" = 902,
                       "theta1=0,60;  theta2=0,10"  = 61,
                       "theta1=0,70;  theta2=0,30"  = 73
                       ),  selected = 1) 

    ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(
        type = "tabs", 
        tabPanel("ABC Sequencial-Lenormand.", 
                 plotOutput("Lenormand"),
                 tabPanel("Bayesiano",
                          tags$h4("Bayesiano"),
                          plotOutput(("BergmG")))
                 )

      )
    )
  )
))