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
    
    DataPrep = function(){
      
      # gio.loaddata$LoadData()
      
      gio.createtable$CreateTable()
      
      
      
      gio.dictionary$Dictionary()
      
      # gio.checkdata$CheckData()
      
      gio.joindata$JoinData()
      
      gio.splitdata$SplitData()
      
      #gio.cleandata$CleanData()
    },
    
    DataTransform = function(){
      gio.assignpoint$AssignPoint()
      
      gio.transform$TransformData()
      
      gio.dataready$DataReady()
      
      gio.statistics$StatisticsData()
      
      gio.indexation$IndexationData()
      
      gio.converge$ConvergeTable()
    },
    
    Report = function() {
      gio.R$Report()
    },
    
    Run = function(){
      
      LogInfo("Start running!")
      #gio.config$InitJobs()
      
      DataPrep()
      
      DataTransform()
      
      Report()
      
    },
    
    TestQueryData = function(){
      
      jobs <- config$GetConfigJob()$dataquery
      test <- jobs$test
      for (i in 1:length(test)) {
        df <- gio.querydata$QueryData(test[i])
        print(df)
      }
    },
    
    TestPlotFigure = function(){
      
      jobs <- config$GetConfigJob()$plotfigure
      test <- jobs$test
      for (i in 1:length(test)) {
        figure <- gio.plotfigure$PlotFigure("report.out/2018sh/fig/", test[[i]])
        
      }
    }
    
  )
) 