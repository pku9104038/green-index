# load libray
library(yaml)
# init global configurations
conf <- yaml.load_file("yaml/conf.yaml")
g.dir <- conf$dir
g.yaml <- conf$yaml

source(paste0(g.dir$R,"report/report.R"))
reports <- yaml.load_file(paste0(g.dir$yaml,g.yaml$survey))$report
n <- length(reports$report)
for(i in 1:n){
  report <-  reports$report[[i]]
  if(is.null(report$output)){
    report$output  <- reports$output
  }
  report$input_file <- paste0(g.dir$report.in.rmd,report$markdown)
  output_dir <- paste0(g.dir$report.out,report$dir_output)
  for(j in 1:length(report$output)){
    output <-  report$output[[j]]
    report$datetime  <- report.datetime()
    report$file <- paste0(report$datetime,output$ext)
    print(paste("Render",report$file,Sys.time()))
    
    render(input = report$input_file, 
           output_format = output$format, 
           output_dir = output_dir,
           output_file = report$file
    )
    
  }
  
  
}