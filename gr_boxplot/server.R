#server.R, grouped boxplot
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

#Define an empty list to store the user selected stock names
stk.ls<-list()
coll.ls<-list()
shinyServer(function(input, output) {
  #Reactive object to subset the bulkdata depending on the attribute input. It will be used to determine which stocks are available for a given attribute.
  options<-reactive({
    subset(bulkdata, attribute==input$attribute)
  })
  
  #Create the selectInput for collections
  output$select.collection<-renderUI({
    if (input$attribute == ""){
      seas<-""
    }else{
      options<- options()
      seas<-as.vector(options[["collection"]])
    }
    selectInput("collection",
                label = "Choose a collection",
                choices = seas,
                selected = NULL,
                multiple=TRUE)
    
  })
  
  op2<-reactive({
    if (is.null(input$collection) == TRUE){ #If op1 is empty, do nothing. Prevents for errors at start the app
      NULL
    }else{
      coll<-input$collection
#       stk.ls[stock]<-stock #Create a list from the multiple selectInput, each selected stock is appended to the list
#       if (input$control == T){ # If the checkbox of control values is clicked, add the control stock name to the list of stocks
#         stk.ls["control"]<-"chaendler" 
#       }
      #Loop over the list of stocks, subset the bulkdata df with the user input for attributes and stock
      #Lapply create a list, in this case a list of df, each one corresponding to the subsetted df for the combiation of stock and attribute
      coll.ls[coll]<-coll
      data.ls<-lapply(coll.ls, function(x){
        subset(options(), collection==x)  
      })
      rbindlist(data.ls) #Bind all the df stored in the data.ls list of df
      
    }
  })
  
  
  
  #Create the selectInput for a list of stocks. 
  output$select.stk<-renderUI({
    if(input$all == TRUE){
      return()
    }else{
    if (is.null(input$collection) == TRUE){#Returns nothing if there is no attribute selected
      stk.opt<-""
    }else{
      options<-op2()
      stk.opt<-as.vector(options[["stock"]])
    }
      selectInput("stock",
                  label="Choose a stock",
                  choices = stk.opt,
                  selected = NULL,
                  multiple=TRUE)                  
    }
  })
  #Create a df containing the user selected values.        
  op3<-reactive({
    if (input$go==0){
      return(NULL)
    }else{
      if (input$all == TRUE){
        op2()
      }else{
      stock<-input$stock
      stk.ls[stock]<-stock #Create a list from the multiple selectInput, each selected stock is appended to the list
      #Loop over the list of stocks, subset the bulkdata df with the user input for attributes and stock
      #Lapply create a list, in this case a list of df, each one corresponding to the subsetted df for the combiation of stock and attribute
      data.ls<-lapply(stk.ls, function(x){
        subset(op2(), stock==x)  
      })
      rbindlist(data.ls) #Bind all the df stored in the data.ls list of df
    }
    }
  })



#Create the selectInput for collections
output$select.season<-renderUI({
  if (input$check.season == FALSE){
    return()
  }else{
    options<- op3()
    seas<-as.vector(options[["season"]])
    selectInput("season",
              label = "Choose one or more seasons",
              choices = seas,
              selected = NULL,
              multiple=TRUE)
  }
})

#Create a df containing the user selected values.        
data<-reactive({
  if (input$go==0){
    return(NULL)
  }else{
    if (input$check.season == FALSE){
      op3()
    }else{
      season<-input$season
      stk.ls[season]<-season #Create a list from the multiple selectInput, each selected stock is appended to the list
      #Loop over the list of stocks, subset the bulkdata df with the user input for attributes and stock
      #Lapply create a list, in this case a list of df, each one corresponding to the subsetted df for the combiation of stock and attribute
      data.ls<-lapply(stk.ls, function(x){
        subset(op3(), season==x)  
      })
      rbindlist(data.ls) #Bind all the df stored in the data.ls list of df
    }
  }
})

    #############
    ###Boxplot###
    #############
    output$plot<-renderPlot({
      if (input$go==0){
        return(NULL)
      }else{
        data<-data()
        validate(#Avoid red error message to be shown when the user changes the attribute. Meanwhile, print the message "waiting for your selection"
          need(nrow(data)>1, "Waiting for your selection")
        )
        p<-ggplot(data, aes(factor(season), value)) +
        labs(x="year", y = paste0(input$attribute, " ", "(", unique(data$unit), ")")) +
        scale_fill_discrete(name="stock") #legend title

        p <- p  +  geom_boxplot(aes(fill = factor(stock))) #Map color to stock
        print(p)
      }
  })
})
