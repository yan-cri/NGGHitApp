## This App is developed by Yan Li for requested project (CRI-BIO-300), last update on August, 2015



header <- dashboardHeader(
  title = "NGG Genome Search"
)

sidebar <- dashboardSidebar(
  h3("Sequence Data Input",
     style="padding-left: 0.5em"),
  textInput(inputId="seq", 
            label = "Please provide your interested 20bp sequence here",
            value = "GGGACTTTCCGGGACTTTCC"),
  br(),
  h3("Genome Options",
     style="padding-left: 0.5em"),
  uiOutput("choose_genome"),
  
  br(),
  radioButtons(inputId = "mismatchNo",
               label = "Please choose the maximun number of mismatch allowed in the genome search",
               choices = c("No mismatch" = "0", "1 mismatch" = "1",
                           "2 mismatch" = "2", "3 mismatch" = "3", "4 mismatch" = "4"),
               selected = "0"),
  
  br(),
  actionButton("submit", label="Submit"),
  tags$style("button#submit {margin-left: 1em; }")

  )

body <- dashboardBody(
  
  fluidRow(
    column(width = 2,
                     
           box(title = textOutput("genomeInfo"), 
               width = NULL, 
               solidHeader = T, status = "info",
               uiOutput("genome_options"),
               br(),
               actionButton("chrSubmit", label="Submit")
               )
           
           ),
    
    
    column(width = 10,
           box(title = "NGG sequence search Results",
               width = NULL, 
               solidHeader = T, status= "info",
               textOutput("genomeAndChrInfo"),
               br(),
               plotOutput("hitSearchPlot")               
               ),
           fluidRow(
             box(title = "Genome Search Results",
                 width = 6, 
                 solidHeader = T, status= "info",
                 collapsible = T, collapsed = F,
                 DT::dataTableOutput("tabRes"),
                 br(),
                 downloadButton("resDownload", 
                                label = "Download",
                                class = NULL)
                 ),
             box(title = "Genome Search Summary",
                 width = 6, 
                 solidHeader = T, status= "info",
                 collapsible = T, collapsed = F,
                 tableOutput("chrSummary")
                 
             )
             
             )
           
           )
    ) 

  
  )

ui <- dashboardPage(header, sidebar, body, skin = "red")
