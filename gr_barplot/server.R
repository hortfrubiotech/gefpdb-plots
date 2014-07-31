#server.R, gr_barplot

#Load libraries for shiny, plots and PgSQL connection
library(shiny)
library(ggplot2)
library("RPostgreSQL")
library(agricolae)
library(car)
library(data.table)
library(plyr)
library(RColorBrewer)
library("dplyr")
#Connect to the database
drv<-dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="drupal", host="10.0.0.17", user="drupal", port ="5432")
#Set the search path to chado, public in the database
dbSendQuery(con, "SET search_path TO chado, public;")
#Query the DB to get all the phenotypics values from all attributes and store it in a dataframe
bulkdata<- dbGetQuery(con, "SELECT s3.uniquename AS collection, s2.uniquename AS stock, c2.name AS stock_type, esp.value AS season,
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
                      JOIN cvterm c2 ON s2.type_id = c2.cvterm_id            
                      JOIN cvtermprop cvp ON cvp.cvterm_id = c1.cvterm_id
                      WHERE cvp.type_id = (SELECT c.cvterm_id FROM cvterm c WHERE c.name = 'unit')")
bulkdata$value<-as.numeric(bulkdata$value)
postgresqlCloseConnection(con)
bulkdata<-na.omit(bulkdata)
#Script where the dataplot function is defined
source("functions.R")
stk.ls<-list()
coll.ls<-list()
shinyServer(function(input, output) {
  #Reactive object to subset the bulkdata depending on the attribute input. It will be used to determine which stocks are available for a given attribute.
  options<-reactive({
    filter(bulkdata, attribute==input$attribute)
  })
  
  #Create the selectInput for collections.
  
  output$collection<-renderUI({
    if (is.null(input$attribute) == TRUE){ #Returns nothing if there is no attribute selected.
      return()
    }else{
      options<-options()
      col.opt<-as.vector(options[["collection"]])#Get the stock names from the subsetted df, transforms it in a vector. 
      
      selectInput("collection",
                  label="Select one or more collections",
                  choices = col.opt,
                  selected = NULL,
                  multiple= TRUE)                  
    }
  })
  
  op2<-reactive({
    if(is.null(input$attribute) == TRUE){
      return()
    }else{
      coll<-input$collection
      
      coll.ls[coll]<-coll
      #Loops over the stk.ls list and subset for the given stock and the selected attribute. Creates a list of df, each one corresponding to one stock and one attribute
      data.ls<-lapply(coll.ls, function(x){
        subdata<-filter(options(), collection==x)
        validate(#Avoid red error message to be shown when the user changes the attribute. Meanwhile, print the message "waiting for your selection"
          need(nrow(subdata)>0, "Waiting for your selection")
        )
        subdata
        
      })
      #Bind all the df in the data.ls list, creates a unique df
      rbindlist(data.ls)
    }
  })
  #Create the selectInput for one stock.
  output$stk<-renderUI({
    if (is.null(input$attribute) == TRUE | input$mult == TRUE | input$all == TRUE ){ #Returns nothing if there is no attribute selected or the user has checked the multiple-stock checkbox.
      return()
    }else{
      options<-op2()
      stk.opt<-as.vector(options[["stock"]])#Get the stock names from the subsetted df, transforms it in a vector. 
      
      selectInput("stock",
                  label="Select an individual stock",
                  choices = stk.opt,
                  selected = NULL)                  
    }
  })
  
  
  #Create the selectInput for a list of stocks. 
  output$list<-renderUI({
    if (input$mult == FALSE){ #if the check button is not selected, don't show this box
      return()
    }else{
      options<-op2()
      stk.opt<-as.vector(options[["stock"]])
      selectInput("stkList",
                  label="Choose a stock",
                  choices = stk.opt,
                  selected = NULL,
                  multiple=TRUE)
    }
  })
  
  output$all.stk<-renderUI({
#     if (input$mult == FALSE){ #if the check button is not selected, don't show this box
#       return()
#     }else{
      checkboxInput("all", 
                    label = "Select all stocks (no statistical analysis possible)",
                    value = FALSE)
#     }
  })
  
  stk<-reactive({
    op2<-op2()
    if (input$go==0){ # if the 'Run' button is not clicked, returns nothing.
      return(NULL)
    }else{
      if(input$all == TRUE){
        stk.ls<-list()
        stk.ls<-as.list(levels(as.factor(op2$stock)))
      }else{      
        if (input$mult == FALSE){ #if the checkbox is not selected, get the stock from input$stock (unique stock selection)
          stock<-input$stock
          control.sbs <- op2[which(op2$stock_type=='control'), ]
          cnt.name<-unique(control.sbs$stock)
          stk.ls[stock]<-stock
          stk.ls["control"]<-cnt.name
          
          validate( #Show a message if the user selects chaendler stock (control stock)
            need(stock != cnt.name,"You have selected the control stock, please select other stock to compare with the control." )
          )
        }else{ # Else, get the stocks from the input$stkList (multiple stock selection)
          stock<-input$stkList
          stk.ls[stock]<-stock
        }
      }
      stk.ls
    }
  })
  
  #Creates a df containing the user selected values.       
  data<-reactive({
    if (input$go==0){ # if the 'Run' button is not clicked, returns nothing.
      return(NULL)
    }else{
      d<-op2()
      stk.ls<-stk()    
      #Loops over the stk.ls list and subset for the given stock and the selected attribute. Creates a list of df, each one corresponding to one stock and one attribute
      data.ls<-lapply(stk.ls, function(x){
        subdata<-filter(d, stock==x)
        validate(#Avoid red error message to be shown when the user changes the attribute. Meanwhile, print the message "waiting for your selection"
          need(nrow(subdata)>0, "Waiting for your selection")
        )
        subdata
        
      })
      #Bind all the df in the data.ls list, creates a unique df
      rbindlist(data.ls)
    }
  })
  
  output$table<-renderTable({
    data()
  })
  
  
  
  output$plot<-renderPlot({  
    if (input$go==0){ # if the 'Run' button is not clicked, returns nothing.
      return(NULL)
    }else{  
      data<-data()
      stk.ls<-stk()
      attribute <- input$attribute  
      #Execute the function defined in barplot.R or plot_no_stat.R depending on the checkbox
      if (input$all == TRUE  | input$mult == TRUE){
        p<-s.plot(data, stk=stk.ls)
        
      }else{
        p<-printplot(data, stk=stk.ls)
      }
      print(p)
    }
  })
  
  
})