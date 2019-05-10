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
kSegmentConnector <- "-"
kColumnSuffixPoint <- "_分值"

# constants for data type
kDataTypeCharacter <- "character"
kDataTypeNumeric <- "numeric"
kDataTypeBoolean <- "boolean"

# constsants for tier
kTierCity <- "市"
kTierDistrict <- "区"
kTierSchool <- "学校"
kTierRegion <- "区域"

# constants for perspective
kPerspectiveTotal <- "总体"

kTRUE <- "是"
kFALSE <- "否"
kCoefficient <- "系数"
kIndex <- "指数"
kStatistics <- "统计"
kNumericNA <- -1
kAxisXTextLimit <- 25
kHashDigestDefault <- "27f4468f070e6f28b58e39fda7293bf8c3fa6fb7"
kStringAll <- "ALL"
kStringNone <- "NONE"
kStringNull<- "NULL"
kPilotRun <- "PILOT"        # city total only
kAgileRun <- "AGILE"        # city all, district total
kMileStone <- "MILESTONE"   # city, district all  and one district school total
kAutoRun <- "AUTO"          # all tier, all perspective 
kValueTypeInteger <- "整数"
kValueTypePercent <- "百分数"
kPercentDigits <- 1
kSortAsc <- "ASC"
kSortAscAll <- "ASC_ALL"
kSortDesc <- "DESC"
kSortDescAll <- "DESC_ALL"
kPositionDodge <- "dodge"
kPositionStack <- "stack"
kCoordFlip <- "flip"
kCoordPolar <- "polar"
kMinPercent <- "5%"
kMin <- "min"
kLowerPercent <- "25%"
kLower <- "lower"
kMedianPercent <- "50%"
kMedian<- "median"
kUpperPercent <- "75%"
kUpper  <- "upper"
kMaxPercent <- "95%"
kMax <- "max"
kMean <- "mean"
kSigma <- "标准差"
kCoefVar <- "离散系数"
kCoefBalanceIndivadual <- "个体均衡系数"
kIndexBalanceIndividual <- "个体均衡指数"

kPrefixPlot <- "plot"
kPrefixMultiPlot <- "multiplot"
kPrefixQueryData <- "query"
kPrefixConnector <- "\\."

# constants for algorithm
kAlgorithmConstant <- "常量赋值"
kAlgorithmSigmaBinary <- "求和阈值比较"
kAlgorithmSigmaMean <- "求和平均值"
kAlgorithmSigmaValue <- "求和"
kAlgorithmCleanChoice <- "单选题清洗"
kAlgorithmGreatThanMean <- "大于平均值"
kAlgorithmGroupSigma <- "属性组求和"
kAlgorithmSigmaTotalScore <- "计算总分"
kAlgorithmStandardization <- "标准化"
kAlgorithmScoreSegment <- "分数段切分"
kAlgorithmScoreRank <- "分数等级"
kAlgorithmValueMapping <- "值映射"
kAlgorithmSingleChoicePercent <- "单选百分比"
kAlgorithmMultipleChoicePercent <- "多选百分比"
kAlgorithmValueSpacePercent <- "值域百分比"
kAlgorithmQuantile <- "四分位"
kAlgorithmMean <- "平均值"
kAlgorithmTenLevelIndex <- "十级指数化"

# constants for plot
kPlotGeomBarDodge <- "并列条形图"
kPlotGeomBarStack <- "堆叠条形图"
kPlotGeomScatter <- "散点图"
kPlotGeomBox <- "盒须图"

# table for plot
kTablePlotParameter <- "属性表绘图参数"
kColumnPlotCode <- "plot_code"
kColumnPlotTitle <- "plot_title"
kColumnPlotWidth <- "plot_width"
kColumnPlotHeight <- "plot_height"
kColumnPlotCoord <- "plot_coord"
kColumnPlotTheme <- "plot_theme"
kColumnPlotGeom <- "plot_geom"
kColumnPlotBarPosition <- "plot_bar_position"
kColumnPlotBarWidth <- "plot_bar_width"
kColumnAxisX <- "plot_x"
kColumnSortX <- "plot_sort_x"
kColumnOrderX <- "plot_order_x"
kColumnLabelX <- "plot_label_x"
kColumnTextAngleX <- "plot_text_angle_x"
kColumnTextSizeX <- "plot_text_size_x"
kColumnAxisY <- "plot_y"
kColumnLabelY <- "plot_label_y"
kColumnDiscreteY <- "plot_discrete_y"
kColumnLimitY <- "plot_limit_y"
kColumnLabel <- "plot_label"
kColumnLabelPosition <- "plot_label_position"
kColumnLabelColour <- "plot_label_colour"
kColumnLabelSize <- "plot_label_size"
kColumnLabelVjust <- "plot_label_vjust"
kColumnLabelHjust <- "plot_label_hjust"
kColumnFill <- "plot_fill" 
kColumnFillOrder <- "plot_fill_order"
kColumnFillAlpha <- "plot_fill_alpha"
kColumnFillSkip <- "plot_fill_skip"
kColumnLegendWidth <- "plot_legend_width"
kColumnLegendHeight <- "plot_legend_height"
kColumnLegendPosition <- "plot_legend_position"
kColumnLegendDirection <- "plot_legend_direction"
kColumnLegendFontSize <- "plot_legend_font_size"
kColumnLegendOrder <- "plot_legend_order"
kColumnFacet <- "plot_facet"
kColumnFacetPosition <- "plot_facet_position"
kColumnFacetScale <- "plot_facet_scale"
kColumnFacetOrder <- "plot_facet_order"



# table for data query
kTableDataQuery <- "属性表数据查询"
kColumnDataset <- "数据集编码"
kColumnAliasType <- "别名类型"
kColumnAlias <- "别名"
kColumnName <- "名称"
kColumnDrop <- "忽略值"
kColumnKeep <- "保留值"

kAliasTypeTotal <- "统称"
kAliasTypeName <- "名称"
kAliasTypeAnonymous <- "匿名"

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
kColumnCity <- "市"
kColumnDistrict <- "区"
kColumnSchool <- "学校"
kColumnStatisticsPerspective <- "统计视角"
kColumnStatisticsSample <- "统计样本"
kColumnStatisticsVariable <- "统计变量"
kColumnStatisticsAlgorithm <- "统计算法"
kColumnStatisticsIndexType <- "指标类型"
kColumnValueType <- "数值类型"
kColumnKey <- "键"
kColumnValue <- "值"
kColumnTimeStamp <- "时间戳"
kColumnSubjectConverge <- "学科综合"
kColumnQuestionGroup <- "题组"

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
kColumnRegion <- "区域"

# constants for report
kColumnReport <- "报告"
kColumnTier <- "层级"

# constants for transform
kColumnSN <- "序号"
kColumnQuestionCode <- "题号"
kColumnPointValue <- "分值"
kColumnQuestionType <- "题型"
kQuestionTypeSingleChoice <- "单选"


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

# source scoreconverge
if (!exists("scoreconverge.loaded", mode = "variable")){
  source(paste0(gi.dir.script,"scoreconverge.R"))
}

# source indexation.R
if (!exists("indexation.loaded", mode = "variable")){
  source(paste0(gi.dir.script,"indexation.R"))
}

# source convergetable.R
if (!exists("convergetable.loaded", mode = "variable")){
  source(paste0(gi.dir.script,"convergetable.R"))
}

# source querydata.R
if (!exists("querydata.loaded", mode = "variable")){
  source(paste0(gi.dir.script,"querydata.R"))
}

# source plotfigure.R
if (!exists("plotfigure.loaded", mode = "variable")){
  source(paste0(gi.dir.script,"plotfigure.R"))
}

# source report.R
if (!exists("report.loaded", mode = "variable")){
  source(paste0(gi.dir.script,"report.R"))
}

####################################

## create global green index objects

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
gio.scoreconverge <- GreenIndexScoreConverge$new()
gio.indexation <- GreenIndexIndexationData$new()
gio.converge <- GreenIndexConvergeTable$new()
gio.querydata <- GreenIndexQueryData$new()
gio.plotfigure <- GreenIndexPlotFigure$new()
gio.R <- GreenIndexReport$new()

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
gio.scoreconverge$Init("ScoreConverge", gio.config, gio.database)
gio.indexation$Init("IndexationData", gio.config, gio.database)
gio.converge$Init("ConvergeTable", gio.config, gio.database)
gio.querydata$Init("QueryData", gio.config, gio.database)
gio.plotfigure$Init("PlotFigure", gio.config, gio.database)
gio.R$Init("Report", gio.config, gio.database)

