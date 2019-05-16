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
      
      gio.loaddata$LoadData()
      
      
      # gio.createtable$CreateTable()
      
      gio.dictionary$Dictionary()
      
      # gio.checkdata$CheckData()
      
      gio.joindata$JoinData()
      
      gio.splitdata$SplitData()
      
      #gio.cleandata$CleanData()
      
      # gio.loaddata$LoadAttribute()
      
      gio.assignpoint$AssignPoint()
      
    },
    
    
    ProcessSurvey = function(reload) {
      
      if (reload)
        gio.loaddata$LoadAttribute()
      
      
      gio.transform$TransformSurvey()
      
      gio.statistics$StatisticsSurvey()
      
    },
    
    ProcessScore = function(reload) {
      
      if (reload)
        gio.loaddata$LoadAttribute()
      
      gio.transform$TransformScore()    # 成绩转化
      
      # gio.joindata$ScoreMerge()         # 转换成绩合并（多学科合并计算）
      
      # gio.transform$TransformMerged()   # 合并转换 （多学科合并计算）
      
      # gio.statistics$StatisticsMerged() # 合并统计
      
      gio.statistics$StatisticsScore()  # 成绩统计
      
      gio.scoreconverge$ScoreConverge() # 成绩综合（在统计的基础上再次合并统计）
      
      
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
      # gio.config$InitJobs()
      
      # DataPrep()
      
      ProcessSurvey(TRUE)
      
      ProcessScore(FALSE)
      
      Indexation(FALSE)
      
      # Report(FALSE)
      
    },
    
    Survey = function(){
      
      LogInfo("Start running survey!")
      #gio.config$InitJobs()
      
      DataPrep()
      
      ProcessSurvey()
      
      Indexation()
      
    },
    
    Score = function(){
      
      LogInfo("Start running survey!")
      #gio.config$InitJobs()
      
      DataPrep()
      
      ProcessScore()
      
      Indexation()
      
    },
    
    TestQueryData = function(){
      gio.loaddata$LoadAttribute()
      gio.querydata$PrepareDataframe()
      jobs <- config$GetConfigJob()$dataquery
      test <- jobs$test
      for (i in 1:length(test)) {
        df <- gio.querydata$QueryData(test[i])
        print(df)
      }
    },
    
    TestPlotFigure = function(){
      gio.loaddata$LoadAttribute()
      gio.plotfigure$PrepareDataframe()
      jobs <- config$GetConfigJob()$plotfigure
      test <- jobs$test
      for (i in 1:length(test)) {
        figure <- gio.plotfigure$PlotFigure("report.out/2018sh/fig/", test[[i]])
        
      }
    },
    
    EchoInfo = function(info) {
      
      command <- paste("echo", info, ">> time.txt")
      system(command = command)
      command <- "date >> time.txt"
      system(command = command)
    }
    
  )
) 