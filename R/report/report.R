#############################
library(yaml)
library(rmarkdown)
conf <- yaml.load_file("yaml/conf.yaml")
g.dir <- conf$dir
g.var <- yaml.load_file(paste0(g.dir$yaml,g.yaml$survey))$global$stat$var
g.tier <- yaml.load_file(paste0(g.dir$yaml,g.yaml$survey))$global$def$tier
# source scripts
source(paste0(g.dir$R,"ETL/db.R"))

###############################
report.datetime  <- function(){
  return(sub(" ", "_",sub(":","",sub(":","",Sys.time()))))
}
##############################
report.render <- function(
  report.in
){
  report <-report.in
  timestamp()
  print(paste("Read Table:",report$table))
  report$data <- db.ReadTable(report$table)
  if(!is.null(report$aliasFlag)){
    report$aliasdata <- db.ReadTable(report$aliasTable)
  }
  
  scopes <- levels(factor(report$data[report$data[,g.var$tier]==report$tier,g.var$scope]))
  n <- length(scopes)
  if(report$skip){
    n  <- 1
  }
  for(i in 1:n){
    report$scope  <- scopes[[i]]
    report$datetime  <- report.datetime()
    report$title <- paste0(report$year,report$scope,report$project,report$report)
    report$header  <- paste0(report$year,report$scope,report$project,
                             report$subject,report$report)
    
    report$input_file <- paste0(g.dir$report.in.rmd,report$markdown)
    output_dir <- paste0(g.dir$report.out,report$dir_output)
    for(j in 1:length(report$output)){
      output <-  report$output[[j]]
      report$file <- paste0(report$title,"_",report$datetime,output$ext)
      print(paste("Render",report$file,Sys.time()))
      
      render(input = report$input_file, 
             output_format = output$format, 
             output_dir = output_dir,
             output_file = report$file,
             params = list(report = report,
                           dummy.fig  = report$dummy.fig)
             )
      timestamp()
      #command <- paste(report$copy$command, 
      #                 paste0('"',g.dir$report.in.rmd,report$file,'"'),
      #                 paste0('"',g.dir$report.out,report$copy$to,report$file,'"'))
      #print(paste(command,Sys.time()))
      #system(command = command)
    }
  }
  

  
  #rmarkdown::
}