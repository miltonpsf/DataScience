library(shiny)

# Define UI for random distribution application 
shinyUI(fluidPage( 
    
  # Application title
  titlePanel("Inferência em Grafos Aleatórios Bernoulli : Bayesiano X ABC"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel( width = 2,
        selectInput("selecionaVertices", label = h3("Vértices"), width = "100px",
        choices = list("50" = 50,
                       "100" = 100,
                       "200" = 200,
                       "500" = 500,
                       "750" = 750,
                       "1000" = 1000,
                       "1250" = 1250,
                       "1500" = 1500,
                       "1750" = 1750,
                       "2000" = 2000,
                       "2250" = 2250
                       ), selected = 1)
#       ,
#       br(),
# 
#       radioButtons("modelo", "Modelos ABC ",  
#                    c("ABC Sequencial : Delmoral" = "Delmoral",
#                      "ABC Sequencial : Beaumont" = "Beaumont",
#                      "ABC Sequencial : Lenormand" = "Lenormand",
#                      "ABC Sequencial : Drovandi" = "Drovandi",
#                      "ABC Rejeicao" = "Rejeicao",
#                      "ABC MCMC       : Marjoram" = "Marjoram")),
#       
#       radioButtons("modelos", "Modelos ABC ",  
#                    c("Todos" = "Todos"))
      

    ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(
        type = "tabs", 
        tabPanel("Bayesiano x Todos os métodos ABC. Média a posteriori = 0.731", 
                 plotOutput("BergmG"),
                 tabPanel("ABC Sequencial-Delmoral",
                          tags$h4("ABC Sequencial-Delmoral"),
                          plotOutput(("Delmoral"))),
                 tabPanel("ABC Sequencial-Beaumont",
                          tags$h4("ABC Sequencial-Beaumont"),
                          plotOutput(("Beaumont"))),
                 tabPanel("ABC Sequencial-Lenormand",
                          tags$h4("ABC Sequencial-Lenormand"),
                          plotOutput(("Lenormand"))),
                 tabPanel("ABC Sequencial-Drovandi",
                          tags$h4("ABC Sequencial-Drovandi"),
                          plotOutput(("Drovandi"))),
                 tabPanel("ABC Rejeicao",
                          tags$h4("ABC Rejeicao"),
                          plotOutput(("Rejeicao"))),
                 tabPanel("ABC MCMC-Marjoram",
                          tags$h4("ABC MCMC-Marjoram"),
                          plotOutput(("Marjoram")))
                 ), 
#         tabPanel("Bergm", 
#                  plotOutput("BergmI"),
#                  tabPanel("ABC",
#                           tags$h4("MÃ©todo ABC"),
#                           plotOutput(("ABC")))
#                  ), 
        tabPanel("Tempos de Processamento", 
#                  plotOutput("GraficoTempoEspecifico"),
                 plotOutput("GraficoTempoGeral"),
                 plotOutput("GraficoTempoGeralSemRejeicao")
                 ),
        tabPanel("Estatísticas : KS e KL", 
                 dataTableOutput('estatistica50') ,
                 dataTableOutput('estatistica100'),
                 dataTableOutput('estatistica200'),
                 dataTableOutput('estatistica500'),
                 dataTableOutput('estatistica750'),
                 dataTableOutput('estatistica1000'),
                 dataTableOutput('estatistica1250'),
                 dataTableOutput('estatistica1500'),
                 dataTableOutput('estatistica1750'),
                 dataTableOutput('estatistica2000'),
                 dataTableOutput('estatistica2250')
                 )
      )
    )
  )
))