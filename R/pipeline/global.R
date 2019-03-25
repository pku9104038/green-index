## Global variables, objects, constants 
## for green index data processing and report generation
## MUST source global.R before any other green index .R script


## flag for source loaded checking
global.loaded <- TRUE


## define global constants
# column name of the output data table
kAssessment <- "评测项目"
kGrade <- "年级"
kSubject <- "学科"  # or name of respondent
kTier <- "统计层级"
kScope <- "统计范围"
kPerspective <- "统计视角"
kSample <- "统计样本"
kDomain <- "领域"
kDimention <- "维度"
kGroup <- "群组"  # new column for tier3 index from 2018sh
kAttribute <- "属性"  # new column for tier4 of science, mathematics from 2018sh
kTopic <- "主题"
kVariable <- "变量"  # rename 变量 to 统计变量 from 2018sh
kAlgorithm <- "算法"  # rename 统计 to 计算方法 from 2018sh
kKey <- "键"
kValue <- "值"
kWeight <- "权重"  # rename 加权 to 权重 from 2018sh

# constants for subject
kSubjectCN <- "语文"
kSubjectMA <- "数学"
kSubjectEN <- "英语"
kSubjectSC <- "科学"
kSubjectAR <- "艺术"
kRespondentSTU <- "学生"
kRespondentPAR <- "家长"
kRespondentTEA <- "教师"
kRespondentPRI <- "校长"

kSubjectSet <- c(
  kSubjectCN,
  kSubjectMA,
  kSubjectEN,
  kSubjectSC,
  kSubjectAR,
  kRespondentSTU,
  kRespondentPAR,
  kRespondentTEA,
  kRespondentPRI
)

# constants for table suffix
kTableRaw <- "原始数据"

# constants for choice
kInvalidSet <- list("", "(跳过)")    # 无效选项
kSkipSet <- list("(跳过)")           # 未作答选项
kNullStr <- ""
kColumnMultipleChoice <- "_多选_"    # 多选项答题结果拆分



## basicConfig of logger for every modules
library(logging)
basicConfig()

## source all scripts, create and init objects
# source config.R, this should be the first script to be loaded
if (!exists("config.loaded", mode = "variable")){
  source(paste0(gi.dir.script, "config.R"))
}

# source base.R, this should be the second script to be loaded
# because it is parent of other class
if (!exists("base.loaded", mode = "variable")){
  source(paste0(gi.dir.script, "base.R"))
}

# source yaml.R
if (!exists("yaml.loaded", mode = "variable")){
  source(paste0(gi.dir.script,"yaml.R"))
}

# source database.R
if (!exists("database.loaded", mode = "variable")){
  source(paste0(gi.dir.script,"database.R"))
}

# source xlsx.R
if (!exists("xlsx.loaded", mode = "variable")){
  source(paste0(gi.dir.script,"xlsx.R"))
}

# source loaddata.R
if (!exists("loaddata.loaded", mode = "variable")){
  source(paste0(gi.dir.script,"loaddata.R"))
}

# source checkdata.R
if (!exists("checkdata.loaded", mode = "variable")){
  source(paste0(gi.dir.script,"checkdata.R"))
}

# source joindata.R
if (!exists("joindata.loaded", mode = "variable")){
  source(paste0(gi.dir.script,"joindata.R"))
}

# source splitdata.R
if (!exists("splitdata.loaded", mode = "variable")){
  source(paste0(gi.dir.script,"splitdata.R"))
}

# source cleandata.R
if (!exists("cleandata.loaded", mode = "variable")){
  source(paste0(gi.dir.script,"cleandata.R"))
}

# source assignment.R
if (!exists("assignpoint.loaded", mode = "variable")){
  source(paste0(gi.dir.script,"assignment.R"))
}

####################################

## create global objects

gio.config <- GreenIndexConfig$new()
gio.yaml <- GreenIndexYaml$new()
gio.xlsx <- GreenIndexXlsx$new()
gio.database <- GreenIndexDatabase$new()
gio.loaddata <- GreenIndexLoadData$new()
gio.joindata <- GreenIndexJoinData$new()
gio.splitdata <- GreenIndexSplitData$new()
gio.checkdata <- GreenIndexCheckData$new()
gio.cleandata <- GreenIndexCleanData$new()
gio.assignpoint <- GreenIndexAssignPoint$new()

## Init global objects
gio.config$Init("Config", gi.config.yaml)
gio.yaml$Init("Yaml", gio.config)
gio.xlsx$Init("Xlsx", gio.config)
gio.database$Init("Database", gio.config, gi.db.user, gi.db.pwd)
gio.loaddata$Init("LoadData", gio.config, gio.database, gio.xlsx)
gio.joindata$Init("JoinData", gio.config, gio.database)
gio.splitdata$Init("SplitData", gio.config, gio.database)
gio.checkdata$Init("CheckData", gio.config, gio.database)
gio.cleandata$Init("CleanData", gio.config, gio.database)
gio.assignpoint$Init("AssignPoint", gio.config, gio.database)
