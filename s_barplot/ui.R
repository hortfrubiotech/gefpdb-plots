#ui.R
#Load library for PgSQL connection
library("RPostgreSQL")
#Connect to the database
drv<-dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="drupal", host="10.0.0.17", user="drupal", port ="5432")
#Set the search path to chado, public in the database
dbSendQuery(con, "SET search_path TO chado, public;")
#Query the DB to get all the attributes that are in the phenotype table
#The query returns a dataframe with 1 column
attr_df<-dbGetQuery(con, 'SELECT DISTINCT c.name AS attribute FROM cvterm c 
JOIN phenotype p ON  c.cvterm_id = p.attr_id ORDER BY c.name ASC')
#Transform the #1 column to a vector, this is necessary since the 'selectInput'
# widget needs a vector for the 'choices' parameter
attr_names<-attr_df[,1]
#start UI
shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
#Two select inputs, one for attributes and the other for seasons and a 'Run' button.
#The attribute input is  fixed and the values are in the attr_names vector.
#The season input is created depending on the attribute selection and it's defined in the server.R file.
#The 'Run' button prevents shiny for starting before any option is selected. 
      selectInput("attribute",
                  label="Choose a phenotypic attribute",
                  choices = attr_names,
                  selected = NULL),
      uiOutput("select.collection"),
      uiOutput("select.season"),
      checkboxInput("mult", 
                    label = "Select individual stocks (choose more than one)",
                    value = FALSE),
      uiOutput("select.stk"),
      actionButton("go","Run")
      
  ),
  
  mainPanel( 
    #Multiple tabs
    tabsetPanel(id="tabs",
                tabPanel("Plot", plotOutput("plot")),
                tabPanel("Normality",  plotOutput("histo"), verbatimTextOutput("norm")),
                tabPanel("Homocedasticity", verbatimTextOutput("homo")),
                tabPanel("Differences", verbatimTextOutput("aov")),
                tabPanel("Post-hoc", verbatimTextOutput("groups")),
                tabPanel("Download data", downloadButton("downloadData"))
                ),   
    #Avoid error messages to appear in the page, not working
    tags$style(type="text/css",
               "#map.shiny-output-error { color: inherit;}",
               "#map.shiny-output-error:before { content: ; }"
   )
   )
  )
  ))