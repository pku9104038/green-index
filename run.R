## Entry script for green index data analyse

# pg_dump -h localhost -p 5432 -U xxxx -w -d dbname -f filename
# psql -h localhost -p 5432 -U xxxx -w -f filename dbname

# install.packages("~/github/ggthemes_4.2.0.tar.gz", repos = NULL, type = "Source")

setwd("~/OneDrive/zuoyue/project/2018绿色指标分析/green-index")

# global setting rmd
# \graphicspath{ {/Users/wangpeifeng/OneDrive/zuoyue/project/2018绿色指标分析/green-index/report.out/2018sh/fig/} }

options(java.parameters = "-Xmx8g")  # set java heap size before run.R


# options(tinytex.verbose = TRUE)

## set instance config for global.R
gi.dir.script <- "R/pipeline/"
gi.config.yaml <- "yaml/config.yaml"
gi.db.user <- "wangpeifeng"
gi.db.pwd <- ""


## source global.R
# this will create and init all global objects
if (!exists("global.loaded", mode = "variable")){
  source(paste0(gi.dir.script,"global.R"))
}

######################################

## source scheduler.R, this must sourced after global.R
gi.dir.schedule <- "R/schedule/"
if(!exists("scheduler.loaded", mode = "variable")) {
  source(paste0(gi.dir.schedule,"scheduler.R"))
}

## init scheduler
gio.scheduler <- GreenIndexScheduler$new()
gio.scheduler$Init("Scheduler", gio.config)


gio.scheduler$EchoInfo("Start!")
#######################################


## Running

# gio.scheduler$DataPrep()
# gio.loaddata$LoadData()
# gio.joindata$JoinData()
# gio.splitdata$SplitData()
# gio.assignpoint$AssignPoint()

# gio.scheduler$Run()
# gio.loaddata$LoadAttribute()

# gio.scheduler$ProcessSurvey(TRUE)
# gio.transform$TransformSurvey()
# gio.statistics$StatisticsSurvey()


# gio.scheduler$ProcessScore(TRUE)
# gio.transform$TransformScore()    # 成绩转化
# gio.statistics$StatisticsScore()  # 成绩统计
# gio.scoreconverge$ScoreConverge() # 成绩综合（在统计的基础上再次合并统计）

# gio.scheduler$Indexation(TRUE)
# gio.loadindex$LoadIndex()
# gio.converge$ConvergeTable()

gio.scheduler$Report(TRUE)
# gio.loaddata$LoadParams()

# gio.scheduler$TestQueryData(TRUE)
# gio.scheduler$TestPlotFigure(TRUE)
# gio.scheduler$TestMultiPlotFigure(TRUE)

gio.scheduler$EchoInfo("Stop!")