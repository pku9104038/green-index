# load libray
library(yaml)

# init global configurations
conf <- yaml.load_file("yaml/conf.yaml")
g.dir <- conf$dir
g.yaml <- conf$yaml

source(paste0(g.dir$R,"report/plot.R"))
#######################################################
plot.name <- "answer.segment.bar.ma"


reports <- yaml.load_file(paste0(g.dir$yaml,g.yaml$survey))$report
############
# 1 base, 2 green, 3 cn, 4 ma
############
i <- 4
report <-  reports$report[[i]]

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
print(report$aliasFlag)
if(!is.null(report$aliasFlag)){
  report$aliasdata <- db.ReadTable(report$aliasTable)
}

plot <- report$plot[[plot.name]]

#plot$output <-  report$output[[1]]

#plot$title <- report$title
#plot$dir  <- paste0(getwd(),"/",g.dir$report.out.plot)
#plot$data$keep <- list(list(var = "主题", value=list("数与运算得分分布","数与运算平均分")))
plot$data$keep <- list(list(var = "变量", value=list("M3AS131")))
plot.figure( report = report, plot.in = plot,fig_name = plot$fig.name)
