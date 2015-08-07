## This App is developed by Yan Li for requested project (CRI-BIO-300), last update on August, 2015

header <- dashboardHeader(
  title = "NGG Genome Search" 
)

sidebar <- dashboardSidebar(
  h3("Sequence Data Input",
     style="padding-left: 0.5em"),
  textInput(inputId="seq", 
            label = "Please provide input sequence here",
            value = "AGGAGGAGGAGGAGGAGGAG"),
  br(),
  h3("Genome Search Options",
     style="padding-left: 0.5em"),
  uiOutput("choose_genome"),
  
  br(),
  radioButtons(inputId = "mismatchNo",
               label = "Please choose the maximun number of mismatch allowed in the search",
               choices = c("No mismatch" = "0", "1 mismatch" = "1",
                           "2 mismatch" = "2", "3 mismatch" = "3", "4 mismatch" = "4"),
               selected = "0"),
  
  br(),
  actionButton("submit", label="Submit"),
  tags$style("button#submit {margin-left: 1em; }")
  #img(src="CRI_Logo_Text.png", style="padding-left:0.7em;margin-left:auto;margin-right:auto;display:block;position:absolute; bottom:1em;")
  
  #,br()
  #,br()
  #,br()
  #,br()
  #,helpText("Developed by bioinformatics core, Center for Research Informatics (CRI), University of Chicago", style="padding-left:1em; padding-right:1em")
  ,helpText("Developed by bioinformatics core, Center for Research Informatics (CRI), University of Chicago", style="padding-left:1em; padding-right:1em;position:absolute; bottom:1em; ")
  
  )

body <- dashboardBody(
  
  fluidRow(
    column(width = 2,
                     
           box(title = textOutput("genomeInfo"), 
               width = NULL, 
               solidHeader = T, status = "info",
               uiOutput("genome_options")
               )
           
           ),
    
    
    column(width = 10,
           box(title = textOutput("genomeAndChrInfo"),
               width = NULL, 
               solidHeader = T, status= "info",
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
                 tableOutput("chrSummary"),
                 tags$style("#chrSummary table {border: 1px solid black; align: center; margin: auto}","#chrSummary th {border: 1px solid black;}","#chrSummary td {border: 1px solid black;}"),
                 
                 br(),
                 downloadButton("countresDownload", 
                                label = "Download",
                                class = NULL)
                 
             )
             
             )
           
           )
    ) 

  
  )

ui <- dashboardPage(header, sidebar, body, skin = "red")
