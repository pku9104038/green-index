# load libray
library(yaml)

# init global configurations
conf <- yaml.load_file("yaml/conf.yaml")
yaml <- yaml.load_file(paste0(conf$dir$yaml,conf$yaml$survey))
g.dir <- conf$dir
g.yaml <- conf$yaml
g.var <- yaml$global$stat$var
g.tier <- yaml$global$stat$def$tier
source(paste0(g.dir$R,"report/plot.R"))
source(paste0(g.dir$R,"ETL/db.R"))
#######################################################
plot.name <- "psychological_renjiguanxi.bar"

reports <- yaml.load_file(paste0(g.dir$yaml,g.yaml$survey))$report
############
# 1 base, 2 green, 3 cn, 4 ma, 5 county_base 6 county_green, 7 county_cn, 8 county_ma, 9 school_report
############

i <- 5

report <-  reports$report[[i]]

report$scope <- "浦东新区" #上海市崇明区新海学校(海桥校区）"  #"上海奉贤区民办青溪小学"  #上海市浦东新区第二中心小学（巨野校区）
report$school <- report$scope
report$county <- "浦东新区"
report$province  <- "上海市"
report$brake <- FALSE
report$dummy.fig <- "reports.jpg"

report$plot.out  <- paste0(getwd(),"/",g.dir$report.out.plot)

if(is.null(report$table)){
  report$table <-  reports$table
}
if(is.null(report$output)){
  report$output  <- reports$output
}
if(is.null(report$geom)){
  report$geom  <- reports$geom
}
if(is.null(report$aliasTable)){
  report$aliasTable <-  reports$aliasTable
  print(report$aliasTable)
}
if(is.null(report$aliasFlag)){
  
  report$aliasFlag <-  reports$aliasFlag
  print(report$aliasFlag)
}

timestamp()
print(paste("Read Table:",report$table))
report$data <- db.ReadTable(report$table)
timestamp()
  print(paste("Read Table:",report$aliasTable))
  report$aliasdata <- db.ReadTable(report$aliasTable)
  
  if(report$tier == g.tier$county){
    report$county <- report$scope
    report$aliasdata[report$aliasdata[,g.var$scope]==report$county, g.var$alias] = "本区"
    
  }else if(report$tier == g.tier$school){
    report$school  <- report$scope
    #print(report$aliasdata)
    #print(g.var$scope)
    #print(g.tier$county)
    #print(g.var$scope)
    #print(g.tier$county)
    report$county <- report$aliasdata[report$aliasdata[,g.var$scope]==report$school, g.tier$county]
    report$aliasdata[report$aliasdata[,g.var$scope]==report$county, g.var$alias] = "本区"
    report$aliasdata[report$aliasdata[,g.var$scope]==report$school, g.var$alias] = "本校"
    
  }



plot <- report$plot[[plot.name]]

#plot$output <-  report$output[[1]]

#plot$title <- report$title
#plot$dir  <- paste0(getwd(),"/",g.dir$report.out.plot)
#plot$data$keep <- list(list(var = "统计范围", value=list(report$school)))
#plot$data$keep <- list(list(var = "统计范围", value=list(report$scope)))
plot$data$keep <- list(list(var = "统计范围", value=list(report$county,report$province)))
#plot$data$keep <-list(list(var = "统计范围", value=list(report$scope,report$county)))
#plot$data$keep <- list(list(var = "统计范围", value=list(report$county,report$scope)),
#                       list(var = "学科", value=list("学业成绩")),
#                       list(var = "主题", value=list("高层次思维能力指数")))
#plot$data$keep <- list(list(var = "统计范围", value=list(report$scope)))
#plot$data$postkeep <- list(list(var = "x", value=list("本市")))
#plot$$data$keep <- list(list(var = "评测项目", value=list("2016年度上海市中小学学业质量绿色指标","2014年度上海市中小学学业质量绿色指标",)))
#plot$data$order <- list(list(var = "x", list = list("0-5","5.5-9","9.5-12","12.5-15")))
#plot$data$postkeep <- list(list(var = "变量", value=list("C3AO031")))
#plot$data$postkeep <- list(list(var = "x", value=list(report$county,report$province)))
timestamp()
print(report$scope)
print(report$county)
print(report$province)
plot.figure( report = report, plot.in = plot,fig_name = plot$fig.name)
timestamp()