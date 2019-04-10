## Global variables, objects, constants 
## for green index data processing and report generation
## MUST source global.R before any other green index .R script


## flag for source loaded checking
global.loaded <- TRUE


## define global constants

# constants for subject
kSubjectCN <- "语文"
kSubjectMA <- "数学"
kSubjectEN <- "英语"
kSubjectSC <- "科学"
kSubjectAR <- "艺术"
kRespondentST <- "学生"
kRespondentPA <- "家长"
kRespondentTE <- "教师"
kRespondentPR <- "校长"

kSubjectSet <- c(
  kSubjectCN,
  kSubjectMA,
  kSubjectEN,
  kSubjectSC,
  kSubjectAR,
  kRespondentST,
  kRespondentPA,
  kRespondentTE,
  kRespondentPR
)

# constants for table suffix
# kTableRaw <- "原始数据"

# constants for choice
kInvalidSet <- list("", "(跳过)")    # 无效选项
kSkipSet <- list("(跳过)")           # 忽略选项
kNullStr <- ""                       # 未作答选项
kColumnMultipleChoice <- "_多选_"    # 多选项答题结果拆分
kSeparator <- "┋"
kColumnSuffixPoint <- "_分值"


kTRUE <- "是"
kFALSE <- "否"
kNumericNA <- -1
kHashDigestDefault <- "27f4468f070e6f28b58e39fda7293bf8c3fa6fb7"
kFilterALL <- "ALL"
kPilotRun <- "PILOT"
kAutoRun <- "AUTO"

# column name of the output data table
kColumnHashDigest <- "哈希值"
kColumnAssessment <- "评测项目"
kColumnGrade <- "年级"
kColumnSubject <- "学科"
kColumnDomain <- "领域"
kColumnDimention <- "维度"
kColumnGroup <- "群组"
kColumnAttribute <- "属性"
kColumnTopic <- "主题"
kColumnStatisticsTier <- "统计层级"
kColumnStatisticsScope <- "统计范围"
kColumnStatisticsPerspective <- "统计视角"
kColumnStatisticsSample <- "统计样本"
kColumnStatisticsVariable <- "统计变量"
kColumnStatisticsAlgorithm <- "统计算法"
kColumnStatisticsOutcome <- "指标类型"
kColumnValueType <- "数值类型"
kColumnKey <- "键"
kColumnValue <- "值"


# constants for transform
kColumnTableName <- "表名称"
kColumnTableSuffix <- "表后缀"
kColumnColumnName <- "列名称"
kColumnColumnSuffix <- "列后缀"
kColumnVariableName <- "变量名称"
kColumnVariableSuffix <- "变量后缀"
kColumnVariableType <- "变量类型"
kColumnAlgorithm <- "算法"
kColumnParameter <- "参数"
kColumnTODO <- "TODO"
kColumnCount <- "计数"
kColumnFilterName <- "过滤变量"
kColumnFilterType <- "过滤类型"
kColumnFilterValue <- "过滤值"

# constants for data type
kDataTypeCharacter <- "character"
kDataTypeNumeric <- "numeric"
kDataTypeBoolean <- "boolean"

# constsants for tier
kTierCity <- "市"
kTierDistrict <- "区"
kTierSchool <- "学校"

# constants for perspective
kPerspectiveTotal <- "总体"

# constants for algorithm
kAlgorithmConstant <- "常量赋值"
kAlgorithmSigmaBinary <- "求和阈值比较"
kAlgorithmSingleChoicePercent <- "单选百分比"
kAlgorithmMultipleChoicePercent <- "多选百分比"
kAlgorithmValueSpacePercent <- "值域百分比"
kAlgorithmTenLevelIndex <- "十级指数化"

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

# source createtable.R
if (!exists("createtable.loaded", mode = "variable")){
  source(paste0(gi.dir.script,"createtable.R"))
}

# source loaddata.R
if (!exists("loaddata.loaded", mode = "variable")){
  source(paste0(gi.dir.script,"loaddata.R"))
}

# source dictionary.R
if (!exists("dictionary.loaded", mode = "variable")){
  source(paste0(gi.dir.script,"dictionary.R"))
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

# source transform.R
if (!exists("transform.loaded", mode = "variable")){
  source(paste0(gi.dir.script,"transform.R"))
}

# source dataready.R
if (!exists("dataready.loaded", mode = "variable")){
  source(paste0(gi.dir.script,"dataready.R"))
}

# source statistics.R
if (!exists("statistics.loaded", mode = "variable")){
  source(paste0(gi.dir.script,"statistics.R"))
}

# source indexation.R
if (!exists("indexation.loaded", mode = "variable")){
  source(paste0(gi.dir.script,"indexation.R"))
}

# source convergetable.R
if (!exists("convergetable.loaded", mode = "variable")){
  source(paste0(gi.dir.script,"convergetable.R"))
}

####################################

## create global objects

gio.config <- GreenIndexConfig$new()
gio.yaml <- GreenIndexYaml$new()
gio.xlsx <- GreenIndexXlsx$new()
gio.database <- GreenIndexDatabase$new()
gio.createtable <- GreenIndexCreateTable$new()
gio.loaddata <- GreenIndexLoadData$new()
gio.dictionary <- GreenIndexDictionary$new()
gio.joindata <- GreenIndexJoinData$new()
gio.splitdata <- GreenIndexSplitData$new()
gio.checkdata <- GreenIndexCheckData$new()
gio.cleandata <- GreenIndexCleanData$new()
gio.assignpoint <- GreenIndexAssignPoint$new()
gio.transform <- GreenIndexTransformData$new()
gio.dataready <- GreenIndexDataReady$new()
gio.statistics <- GreenIndexStatisticsData$new()
gio.indexation <- GreenIndexIndexationData$new()
gio.converge <- GreenIndexConvergeTable$new()

## Init global objects
gio.config$Init("Config", gi.config.yaml)
gio.yaml$Init("Yaml", gio.config)
gio.xlsx$Init("Xlsx", gio.config)
gio.database$Init("Database", gio.config, gi.db.user, gi.db.pwd)
gio.createtable$Init("CreateTable", gio.config, gio.database)
gio.loaddata$Init("LoadData", gio.config, gio.database, gio.xlsx)
gio.dictionary$Init("Dictionary", gio.config, gio.database)
gio.joindata$Init("JoinData", gio.config, gio.database)
gio.splitdata$Init("SplitData", gio.config, gio.database)
gio.checkdata$Init("CheckData", gio.config, gio.database)
gio.cleandata$Init("CleanData", gio.config, gio.database)
gio.assignpoint$Init("AssignPoint", gio.config, gio.database)
gio.transform$Init("TransformData", gio.config, gio.database)
gio.dataready$Init("DataReady", gio.config, gio.database)
gio.statistics$Init("StatisticsData", gio.config, gio.database)
gio.indexation$Init("IndexationData", gio.config, gio.database)
gio.converge$Init("ConvergeTable", gio.config, gio.database)


