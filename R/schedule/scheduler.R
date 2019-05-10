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
      
      gio.loaddata$LoadAttribute()
      
      gio.loaddata$LoadData()
      
      # gio.createtable$CreateTable()
      
      gio.dictionary$Dictionary()
      
      # gio.checkdata$CheckData()
      
      gio.joindata$JoinData()
      
      gio.splitdata$SplitData()
      
      #gio.cleandata$CleanData()
      
      
    },
    
    
    ProcessSurvey = function(reload) {
      
      if (reload)
        gio.loaddata$LoadAttribute()
      
      gio.assignpoint$AssignPoint()
      
      gio.transform$TransformSurvey()
      
      # gio.dataready$DataReady()
      
      gio.statistics$StatisticsSurvey()
      
    },
    
    ProcessScore = function(reload) {
      
      if (reload)
        gio.loaddata$LoadAttribute()
      
      gio.transform$TransformScore()
      
      gio.joindata$ScoreMerge()
      
      gio.transform$TransformMerged()
      
      gio.statistics$StatisticsMerged()
      # gio.dataready$DataReady()
      
      gio.statistics$StatisticsScore()
      
      gio.scoreconverge$ScoreConverge()
      
      
    },
    
    Indexation = function(reload) {
      if (reload)
        gio.loaddata$LoadAttribute()
      gio.indexation$IndexationData()
      gio.converge$ConvergeTable()
    },
    
    
    Report = function(reload) {
      if (reload)
        gio.loaddata$LoadAttribute()
      gio.R$PrepareDataframe()
      
      gio.R$Report()
    },
    
    Run = function(){
      
      LogInfo("Start running!")
      #gio.config$InitJobs()
      
      DataPrep()
      
      ProcessSurvey(FALSE)
      
      ProcessScore(FALSE)
      
      Indexation(FALSE)
      
      Report(FALSE)
      
    },
    
    Survey = function(){
      
      LogInfo("Start running survey!")
      #gio.config$InitJobs()
      
      DataPrep()
      
      ProcessSurvey()
      
      indexation()
      
    },
    
    Score = function(){
      
      LogInfo("Start running survey!")
      #gio.config$InitJobs()
      
      DataPrep()
      
      ProcessScore()
      
      indexation()
      
    },
    
    TestQueryData = function(){
      gio.querydata$PrepareDataframe()
      jobs <- config$GetConfigJob()$dataquery
      test <- jobs$test
      for (i in 1:length(test)) {
        df <- gio.querydata$QueryData(test[i])
        print(df)
      }
    },
    
    TestPlotFigure = function(){
      
      gio.plotfigure$PrepareDataframe()
      jobs <- config$GetConfigJob()$plotfigure
      test <- jobs$test
      for (i in 1:length(test)) {
        figure <- gio.plotfigure$PlotFigure("report.out/2018sh/fig/", test[[i]])
        
      }
    }
    
  )
) 