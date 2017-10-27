# load libray
library(yaml)

# init global configurations
conf <- yaml.load_file("yaml/conf.yaml")
g.dir <- conf$dir
g.yaml <- conf$yaml
source(paste0(g.dir$R,"ETL/db.R"))
source(paste0(g.dir$R,"report/dataframe.R"))
#######################################################
param.name <- "point.rate.table.cn"
row <- list(var = "变量", value="C3AO011")
col <- list(var = "键", value="随迁子女民办")

reports <- yaml.load_file(paste0(g.dir$yaml,g.yaml$survey))$report
############
# 1 base, 2 green, 3 cn, 4 ma, 5 county_base 6 county_green, 7 county_cn, 8 county_ma, 9 school_report
############

i <- 7
report <-  reports$report[[i]]

report$scope <- "黄浦区"  #"上海奉贤区民办青溪小学"  #
report$school <- report$scope
report$county <- "黄浦区"
report$province  <- "上海市"
report$brake <- TRUE
report$dummy.fig <- "reports.jpg"

var.variable <- "变量" 
var.key <- "键"
var.param <- "标注值"
var.sample <- "统计样本"
var.scope <- "统计范围"
var.dimention <-  "维度"
  
  
if(is.null(report$table)){
  report$table <-  reports$table
}

timestamp()
print(paste("Read Table:",report$table))
report$data <- db.ReadTable(report$table)
print(report$param)
param <- report$param[[param.name]]

#param.entry <- "jilei.point.rate.table.cn"
#jilei.param  <- dataframe.table( report = report, param = report$param[[param.entry]])
#print(jilei.param)
#data.table  <- jilei.param

data.table <- dataframe.table( report = report, param = param)
print(data.table)

point.rate  <- data.table[data.table[,var.scope]==report$county & data.table[,var.dimention]=="积累",]
row <- point.rate[point.rate[,var.variable]=="C3AO011_X",]
print(row)
element  <- row[row[,var.key]=="随迁子女民办",var.param]

print(element)
print(typeof(element))
print(null.check(element))

#param$data$selector <- list(row,col)
#data.element <- dataframe.element(data.table, param$data$selector)
#print(data.element)
#param$data$selector <- list(row)
#data.row <- dataframe.row(data.table, param$data$selector)
#print(data.row)
#param$data$selector <- list(col)
#data.element <- dataframe.element(data.row, param$data$selector)
#print(data.element)
