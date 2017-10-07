# load libray
library(yaml)

# init global configurations
conf <- yaml.load_file("yaml/conf.yaml")
g.dir <- conf$dir
g.yaml <- conf$yaml
source(paste0(g.dir$R,"ETL/db.R"))
source(paste0(g.dir$R,"report/dataframe.R"))
#######################################################
param.name <- "jilei.point.rate.table.cn"
row <- list(var = "变量", value="C3AO011_X")
col <- list(var = "键", value="中心城区")

reports <- yaml.load_file(paste0(g.dir$yaml,g.yaml$survey))$report
############
# 1 base, 2 green 3,cn
############
i <- 3
report <-  reports$report[[i]]

if(is.null(report$table)){
  report$table <-  reports$table
}

timestamp()
print(paste("Read Table:",report$table))
report$data <- db.ReadTable(report$table)
print(report$param)
param <- report$param[[param.name]]

param.entry <- "jilei.point.rate.table.cn"
jilei.param  <- dataframe.table( report = report, param = report$param[[param.entry]])
print(jilei.param)
data.table  <- jilei.param
#data.table <- dataframe.table( report = report, param = param)
print(data.table)
param$data$selector <- list(row,col)
data.element <- dataframe.element(data.table, param$data$selector)
print(data.element)
param$data$selector <- list(row)
data.row <- dataframe.row(data.table, param$data$selector)
print(data.row)
param$data$selector <- list(col)
data.element <- dataframe.element(data.row, param$data$selector)
print(data.element)
