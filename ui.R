## ui.R ##
library(DT)
library(shiny)
library(plotly)
library(shinydashboard)
library(colourpicker)
header <- dashboardHeader(title = "Community Visualization v2.0", titleWidth = 450)

sidebar <- dashboardSidebar(
  p("Communities are ", span("blue", style = "color:#2A9FD6")), 
  p("Entities are ",  span("green", style = "color:#FF8800")),
 # p("Chemicals are ", span("orange", style = "color:#FF8800")), 
  #p(" are ", span("red", style = "color:#CC0000")),
 selectInput("select", label = h5("Select algorithm"), 
             choices = list("Louvain" = "lv", "Walktrap" = "wk", "Fast Greedy" = "fg","Infomap" = "imap","Edge betweeness" = "ebetweens","Label Propagation"="lp","Spinglass"="sg"), 
             selected = "lv"),
 hr(),
  actionButton("back_button", "Back"),
  actionButton("reset_button", "Reset"),  
 radioButtons("interactions", "Show interactions:",
              c("All" = "all",
                "Protein-Protein" = "Protein-Protein",
                "Protein-Disease" = "Protein-Disease",
                "Protein-Chemical" = "Protein-Chemical",
                "Chemical-Disease" = "Chemical-Disease")),
 

 #fluidRow(column(3, verbatimTextOutput("value"))),
 # checkboxGroupInput("node_types", "Entities:",
#                     choices = c("Protein" , "Disease", "Chemical"),
#                     selected = c("Protein" , "Disease", "Chemical")),
  
  textInput("searchentitiy","Search Entity"),
  actionButton("search_button","Search")
)

body <- dashboardBody(
  tags$head(
    tags$script(src='lib/sigma.min.js'),
    tags$script(src='lib/sigma.layout.forceAtlas2.min.js'),
    tags$script(src='lib/sigma.parsers.json.min.js'),
    tags$script(src='rendergraph.js'),
    tags$link(rel = "stylesheet", type = "text/css", href = "graph.css")
  ),
  
  fluidRow(  
    box(     textOutput("name"), 
             uiOutput("graph_with_sigma"),
             title = "Network",
             header = TRUE,
             tags$canvas(id="graph", # graphical output area
                         width="1000",
                         height="800"),tags$div(id="graph2")
    ),
    
    tabBox( title = "Details", 
            id = "details",
            selected = "Entities",
            tabPanel("Entities", DT::dataTableOutput("degree_table")),
            tabPanel("Degrees", plotlyOutput("degree_distribution")),
            tabPanel("PageRanks", plotlyOutput("pagerank_distribution")),
            tabPanel("Disease Pathway Info.", 
                     tabBox(width=500,title="",id="pathinfo",
                            tabPanel("Data",fluidRow(splitLayout(cellWidths = c("100%", "0%"), DT::dataTableOutput("plotgraph1"))
                            )),
                            tabPanel("Heatmap",plotlyOutput("plotgraph2"))
                     )
    ),
    tabPanel("App. Options,",
             tabBox(width=500,title="",id="fileinfo",
                    tabPanel("EdgeListFile",
                          fileInput('file1', 'Choose file to upload',
                                accept = c(
                                           'text/csv',
                                           'text/comma-separated-values',
                                           'text/tab-separated-values',
                                           'text/plain',
                                           '.csv',
                                           '.tsv'
                                         )
                               ),
             tags$hr(),
             checkboxInput('header', 'Header', TRUE),
             radioButtons('sep', 'Separator',
                          c(Comma=',',
                            Semicolon=';',
                            Tab='\t'),
                          ','),
             radioButtons('quote', 'Quote',
                          c(None='',
                            'Double Quote'='"',
                            'Single Quote'="'"),
                          '"'),
             tags$hr(),
             p('File size limit is 100MB')
                    ),
             tabPanel("Entity definitions",
                      selectInput("entity1","Entity1 Column:",c()),
                      selectInput("entity2","Entity2 Column:",c()),
                      selectInput("type1","Entitiy1 Type Column:",c()),
                      selectInput("type2","Entity2 Type Column:",c()),
                      actionButton("entitymapping_button", "Done")
                      ),
             tabPanel("Entity colors",
                      selectInput("entcolors","Select entity",choices=c()),
                      colourInput("entcol","Select entity color"),
                      actionButton("entdone","Assign color"),
                      tableOutput("enttable")
                      
                      ),
             tabPanel("Community parameters",
                      colourInput("community_col","Community Color","#2ADDDD"),
                      numericInput("comm_size","Max. community size:",value = 400)
                      )
             
             ),
             actionButton("saveoptionscsv","Save"),
             mainPanel(
               tableOutput('contents')
             )
             
             )
  )
  
)
)

dashboardPage(header, sidebar, body, skin = "blue")
