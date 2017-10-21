#############################
library(yaml)
library(rmarkdown)
#conf <- yaml.load_file("yaml/conf.yaml")

# source scripts


###############################
report.datetime  <- function(){
  return(sub(" ", "_",sub(":","",sub(":","",Sys.time()))))
}

##############################
pre.render <- function(
  report,
  conf,
  yaml
){
  source(paste0(g.dir$R,"report/plot.R"))
  
  for(i in 1:length(report$pre.render)){
    plot.entry  <- report$pre.render[[i]]
    plot <- (report$plot)[[plot.entry]]
    if(!is.null(plot)){
      plot.figure(report,plot.in = plot)
    }
    
  }
  
  frmd <- paste0(getwd(),"/",report$input_file)
  input_file <- paste0(g.dir$report.in.rmd,report$markdown,".Rmd")
  frmd.rmd  <- paste0(getwd(),"/",input_file)
  
  rmd.data <- readLines(frmd)
  rmd.rmd.data <- rmd.data 
  l <- 1
  for(i in 1:length(rmd.data)){
    
    if(is.element(rmd.data[i],unlist(report$pre.render))){
      
      for(j in 1:length(report$pre.render)){
        
        plot.entry  <- report$pre.render[[j]]
        if(plot.entry == rmd.data[i] ){
          
          tex.file <- paste0(report$plot.out,paste0(plot.entry,".tex"))
          tex.data <- readLines(tex.file)
          for(k in 1:length(tex.data)){
            rmd.rmd.data[l] <- tex.data[k] 
            l  <- l+1
          }
          
        }
        
      } 
      
    }
    else{
      rmd.rmd.data[l] <- rmd.data[i] 
      l  <- l+1
    }
  }
  
  fo <- file(frmd.rmd)
  writeLines(rmd.rmd.data,fo)
  close(fo)
  
  return(input_file)
  
}
##############################
report.render <- function(
  report.in,
  conf,
  yaml
){
  
  g.dir <- conf$dir
  g.yaml <- conf$yaml
  g.var <- yaml$global$stat$var
  g.tier <- yaml$global$stat$def$tier
  source(paste0(g.dir$R,"ETL/db.R"))
  source(paste0(g.dir$R,"report/dataframe.R"))
  
  report <-report.in
  timestamp()
  print(paste("Read Table:",report$table))
  report$data <- db.ReadTable(report$table)
  
  data.assesment  <- report$data[report$data[,g.var$assesment]==report$assesment,]
  scopes <- levels(factor(data.assesment[data.assesment[,g.var$tier]==report$tier,g.var$scope]))
  n <- length(scopes)
  if(report$skip){
    n  <- 1
  }
  for(i in 1:n){
    if(report$skip & report$tier != g.tier$province){
      if(report$tier == g.tier$county){
        report$scope  <- report$test$county
        #report$scope  <- scopes[[i]]
        #report$scope = "黄浦区"
        #report$scope = "浦东新区"
      }
      else if(report$tier == g.tier$school){
        report$scope <- report$test$school
        #report$scope  <- scopes[[i]]
        #report$scope = "上海浦东新区民办联营小学"
        #report$scope = "上海市浦东新区第二中心小学(张江校区)"
      }
    }
    else{
      report$scope  <- scopes[[i]]
    }
    
    
   
      print(paste("Read Table:",report$aliasTable))
      report$aliasdata <- db.ReadTable(report$aliasTable)
      #print(report$aliasdata)
      
      print(paste("alias:",report$tier))
      if(report$tier == g.tier$county){
        report$county <- report$scope
        report$aliasdata[report$aliasdata[,g.var$scope]==report$county, g.var$alias] = "本区"
        
      }
      else if(report$tier == g.tier$school){
        report$school  <- report$scope
        report$county <- report$aliasdata[report$aliasdata[,g.var$scope]==report$school, g.tier$county]
        
        report$aliasdata[report$aliasdata[,g.var$scope]==report$county, g.var$alias] = "本区"
        report$aliasdata[report$aliasdata[,g.var$scope]==report$school, g.var$alias] = "本校"
        report$school.hq  <- report$aliasdata[report$aliasdata[,g.var$scope]==report$school, g.var$school.hq]
        report$school.camp  <- report$aliasdata[report$aliasdata[,g.var$scope]==report$school, g.var$school.camp]
        
      }
    
    
    report$datetime  <- report.datetime()
    report$title <- paste0(report$year,report$scope,report$project,report$subject,report$report)
    report$header  <- paste0(report$year,report$scope,report$project,
                             report$subject,report$report)
    print(paste("report ",report$header))
  
    report$input_file <- paste0(g.dir$report.in.rmd,report$markdown)
    output_dir <- paste0(g.dir$report.out,report$dir_output)
    for(j in 1:length(report$output)){
      output <-  report$output[[j]]
      report$file <- paste0(report$title,"_",report$datetime,output$ext)
      if(report$tier == g.tier$school){
        report$file <- paste0(report$county,"_",report$file)
      }
      zipfile <- paste0(report$title,"_",report$datetime,".zip")
      sqlfile <- paste0(report$title,"_",report$datetime,".sql")
      csvfile <- paste0(report$title,"_",report$datetime,".csv")
      
      # copy pictures
      for(k in 1:length(report$command$copy$from)){
        command <- paste(report$command$copy$command, 
                         report$command$copy$from[[k]],
                         report$command$copy$to)
        system(command = command)
      }
      ##################################################
      
      if(!is.null(report$pre.render)){
        report$input_file <- pre.render(report, conf, yaml)
      }
      
      ##################################################
      print(paste("Render",report$input_file,Sys.time()))
      render(input = report$input_file, 
             output_format = output$format, 
             output_dir = output_dir,
             output_file = report$file,
             params = list(report = report,
                           dummy.fig  = report$dummy.fig,
                           conf = conf)
             )
      timestamp()
      ####################################################
      #  dump csv and sql
      if(report$tier == g.tier$province && report$dumpFlag){
        db.DumpTable(report$table,paste0(report$command$dump$to,csvfile))
        
        command <- paste(report$command$dump$command, 
                         "-t", report$table,
                         "-f", 
                         paste0(report$command$dump$to,sqlfile),
                         report$command$dump$dbase)
        print(command)
        system(command = command)
      
      
      # zip csv, sql, plot
      
        command <- paste(report$command$zip$command, 
                         paste0(output_dir,"/",zipfile),
                         report$command$zip$from)
        print(command)
        system(command = command)
      }
      if(report$cleanFlag){
        # remove plot and dump files
        command <- paste(report$command$remove$command, 
                         report$command$remove$files)
        
        print(command)
        system(command = command)
      }
      
      
    }
  }

  if(!report$skip & report$tier != g.tier$province){
    if(report$tier != g.tier$county){
      reporttitle <- paste0(report$year,report$project,report$subject,report$report,"_区级")
      zipfile <- paste0(reporttitle,"_",report.datetime(),".zip")
      command <- paste(report$command$zipcounty$command, 
                       paste0(output_dir,"/",zipfile),
                       report$command$zipcounty$from)
      print(command)
      system(command = command)
    }
    else if(report$tier != g.tier$school){
      reporttitle <- paste0(report$year,report$project,report$subject,report$report,"_校级")
      zipfile <- paste0(reporttitle,"_",report.datetime(),".zip")
      command <- paste(report$command$zipschool$command, 
                       paste0(output_dir,"/",zipfile),
                       report$command$zipschool$from)
      print(command)
      system(command = command)
    }
    
  }
  

  
  #rmarkdown::
}