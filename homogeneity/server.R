#server.R, homogeneity
#Load libraries for shiny, plots and PgSQL connection
library(shiny)
library(ggplot2)
library("RPostgreSQL")
library(agricolae)
library(car)
library(data.table)
library(plyr)
#Connect to the database
drv<-dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="drupal", host="10.0.0.17", user="drupal", port ="5432")
#Set the search path to chado, public in the database
dbSendQuery(con, "SET search_path TO chado, public;")
#Query the DB to get all the phenotypics values from all attributes and store it in a dataframe
bulkdata<- dbGetQuery(con, "SELECT s3.uniquename AS collection, s2.uniquename AS stock, esp.value AS season,
                      c1.name AS attribute, p.value AS value, epr.value AS date, cvp.value AS unit 
                      FROM stock s 
                      JOIN stock_relationship sr ON sr.subject_id = s.stock_id
                      JOIN stock s2 ON sr.object_id=s2.stock_id
                      JOIN stock_relationship sr2 ON sr2.subject_id = s2.stock_id
                      JOIN stock s3 ON sr2.object_id=s3.stock_id
                      JOIN nd_experiment_stock es ON s.stock_id = es.stock_id
                      JOIN nd_experiment_stockprop esp ON es.nd_experiment_stock_id = esp.nd_experiment_stock_id         
                      LEFT JOIN chado_stock ck ON s.stock_id = ck.stock_id         
                      JOIN nd_experiment_phenotype ep ON es.nd_experiment_id = ep.nd_experiment_id             
                      JOIN nd_experimentprop epr ON ep.nd_experiment_id = epr.nd_experiment_id            
                      JOIN phenotype p ON ep.phenotype_id = p.phenotype_id          
                      JOIN cvterm c1 ON p.attr_id = c1.cvterm_id            
                      JOIN cvtermprop cvp ON cvp.cvterm_id = c1.cvterm_id
                      WHERE cvp.type_id = (SELECT c.cvterm_id FROM cvterm c WHERE c.name = 'unit')")
bulkdata$value<-as.numeric(bulkdata$value)
postgresqlCloseConnection(con)

shinyServer(function(input, output) {
  
  #Reactive object to subset the bulkdata depending on the attribute input. It will be used to determine which seasons are available for a given attribute.
  options<-reactive({
    subset(bulkdata, attribute==input$attribute)
  })
  
  
  #Create the selectInput for collections
  output$select.collection<-renderUI({
    if (input$attribute == ""){
       choices<-""# If no attribute is selected, return a empty object. 
    }else{
      options<- options()
      choices<-as.vector(options[["collection"]])
    }
      selectInput("collection",
                label = "Choose a collection",
                choices = choices,
                selected = NULL)
    
  })
  
  op2<-reactive({
    if (is.null(input$collection) == TRUE){ #If op1 is empty, do nothing. Prevents for errors at start the app
      NULL
    }else{
#       validate(#Avoid red error message to be shown when the user changes the attribute. Meanwhile, print the message "waiting for your selection"
#         need(length(input$collection)>0, "Waiting for your selection")
#       )
      subset(options(), collection==input$collection)
    }
  })
  
  
  #Create the selectInput for seasons. 
  output$select.season<-renderUI({
    if (input$collection == ""){ # If no attribute is selected, return a empty object.
      seas<-""
    }else{
    options<-op2()  
#     validate(#Avoid red error message to be shown when the user changes the attribute. Meanwhile, print the message "waiting for your selection"
#       need(nrow(options)>1, "Waiting for your selection")
#     )
#       
      seas<-as.vector(options[["season"]])
    }
      selectInput("season",
                  label = "Choose a season",
                  choices = seas,
                  selected = NULL)
        
  })
  
  #Get the dataset which will be ploted latter, subset bulkdata with the selected inputs, attribute and season
  data<-reactive({
    if (input$go  ==0){ # if the 'Run' button is not clicked, return nothing.
      return(NULL)
    }else{
      subset(op2(), season==input$season)
    }
  })

  ############
  ##Boxplot###
  ############
  output$map<-renderPlot({
    validate(#Avoid red error message to be shown when the user changes the attribute. Meanwhile, print the message "waiting for your selection"
      need(input$go>0, "Waiting for your selection")
    )
      data<-data()
      validate(#Avoid red error message to be shown when the user changes the attribute. Meanwhile, print the message "waiting for your selection"
        need(nrow(data)>1, "Waiting for your selection")
      )
      p<-ggplot(data, aes(factor(stock), value)) +
        xlab("Stock") +
        ylab(paste0(input$attribute, " ", "(", unique(data$unit), ")")) + 
        scale_fill_discrete(guide=FALSE) +
        geom_boxplot(aes(fill = factor(stock)))
      print(p)
     
  })
  
  ###################
  ###Bartlett test###
  ###################
  output$bart<- renderPrint({
    validate(#Avoid red error message to be shown when the user changes the attribute. Meanwhile, print the message "waiting for your selection"
      need(input$go>0, "Waiting for your selection")
    )
    data<-data()
    validate(#Avoid red error message to be shown when the user changes the attribute. Meanwhile, print the message "waiting for your selection"
      need(nrow(data)>1, "Waiting for your selection")
    )
    bart.res<-bartlett.test(value ~ stock, data=data)
    bart.res
    
  })
  
  #################
  ###Levene test###
  #################
  output$lev<- renderPrint({
    validate(#Avoid red error message to be shown when the user changes the attribute. Meanwhile, print the message "waiting for your selection"
      need(input$go>0, "Waiting for your selection")
    )
    data<-data()
    validate(#Avoid red error message to be shown when the user changes the attribute. Meanwhile, print the message "waiting for your selection"      
      need(nrow(data)>1, "Waiting for your selection")
    )
    lev.res<-leveneTest(value ~ as.factor(stock), data=data)
    lev.res
    })
  
  ##################
  ###Fligner test###
  ##################
  output$flig<- renderPrint({
    validate(#Avoid red error message to be shown when the user changes the attribute. Meanwhile, print the message "waiting for your selection"
      need(input$go>0, "Waiting for your selection")
    )
    data<-data()
    validate(#Avoid red error message to be shown when the user changes the attribute. Meanwhile, print the message "waiting for your selection"      
      need(nrow(data)>1, "Waiting for your selection")
    )
    flig.res<-fligner.test(data$value, as.factor(data$stock))
    flig.res
    
  })
  
})
