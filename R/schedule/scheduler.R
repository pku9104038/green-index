## Task scheduler for green index data processing and report generation


## flag for source loaded checking
sheduler.loaded <- TRUE


## define GreenIndexScheduler
library(methods)

GreenIndexScheduler <- setRefClass(
  "GreenIndexScheduler",
  contains = "GreenIndexBase",
  
  #fields = list(
  #  config = "GreenIndexConfig",
  #  module = "character"
  #),
  
  
  methods = list(
    
    test = function(){
      df <- data.frame()
      df2 <- data.frame()
      df[1,"id"] <- 345
      gio.database$WriteTable(df, "test_table")
      df2[1,"id"] <- 234
      gio.database$AppendTable(df2, "test_table")
      df <- gio.database$ReadTable("test_table")
      print(df)
      gio.database$RemoveTable("test_table")
    },
    
    Run = function(){
      
      LogInfo("Start running!")
      
      gio.yaml$ConsolidateYaml()
      
      gio.loaddata$LoadData()
      
      

    }
    
  )
) 