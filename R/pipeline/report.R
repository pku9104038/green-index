## Report class for green index data processing and report generation


## flag for source loaded checking
report.loaded <- TRUE

## define 
library(methods)
library(rmarkdown)
#library(tinytex)

GreenIndexReport <- setRefClass(
  "GreenIndexReport",
  contains = "GreenIndexPlotFigure",
  
  fields = list(
    
    table.df = "data.frame",
    district.df = "data.frame",
    school.df = "data.frame",
    
    report.plotfig = "logical",
    report.tier = "character",
    report.scope = "character",
    report.unit = "list",
    report.copyright = "list",
    report.name = "character",
    report.subject = "character",
    report.title = "character",
    report.header = "character",
    report.pilot = "list",
    report.plot.multiple = "character",
    report.query.data = "character",
    report.output = "list",
    report.subdir = "character",
    report.rmarkdown = "character",
    report.plot = "character",
    
    report.fig.name = "character",
    report.fig.path = "character"
  ),
  
  methods = list(
    PrepareDataframe = function() {
      callSuper()
      
      jobs <- config$GetConfigJob()$report
      dataframe <- jobs$dataframe
      
      # table row prerender data
      table.table <- paste0(dataframe$table$table, 
                             dataframe$table$suffix)
      table.df <<- database$ReadTable(table.table)
      
      school.table <- paste0(dataframe$school$table, 
                           dataframe$school$suffix)
      school.df <<- database$ReadTable(school.table)
      
      district.table <- paste0(dataframe$district$table, 
                            dataframe$district$suffix)
      district.df <<- database$ReadTable(district.table)
      
      report.copyright <<- jobs$copyright
        
      report.pilot <<- jobs$pilot
      
    },
    
    CopyFigureDir = function(subdir) {
      command <- paste0("rm ", config$GetDirReportOut(),
                        config$GetDirFigure(), "*")
      system(command = command)
      command <- paste0("cp ", config$GetDirReportIn(),
                       config$GetDirFigure(), config$GetDirCommon(),
                       "* ", 
                       config$GetDirReportOut(),
                       config$GetDirFigure())
      system(command = command)
      # for (i in 1:length(subdir)) {
        command <- paste0("cp ", config$GetDirReportIn(),
                          config$GetDirFigure(), subdir,
                          "/* ", 
                          config$GetDirReportOut(),
                          config$GetDirFigure())
        system(command = command)
      # }
      
    },
    
    CopyRmarkdownDir = function(subdir) {
      command <- paste0("rm ", config$GetDirReportOut(),
                        config$GetDirRmarkdown(), "*")
      system(command = command)
      command <- paste0("cp ", config$GetDirReportIn(),
                        config$GetDirRmarkdown(), config$GetDirCommon(),
                        "* ", 
                        config$GetDirReportOut(),
                        config$GetDirRmarkdown())
      system(command = command)
      
      # for (i in 1:length(subdir)) {
        command <- paste0("cp ", config$GetDirReportIn(),
                          config$GetDirRmarkdown(), subdir,
                          "/* ", 
                          config$GetDirReportOut(),
                          config$GetDirRmarkdown())
        system(command = command)
        
      # }
      
      command <- paste0("mkdir ", GetReportOutRmdDir(), 
                        config$config$dir$log)
      system(command = command)
      command <- paste0("mkdir ", GetReportOutRmdDir(), config$config$dir$log, 
                        "/", config$GetAssessmentJob())
      system(command = command)
    },
    
    SpliceRmarkdown = function(rmarkdown.list) {
      
      dir <- paste0(config$GetDirReportOut(),
                    config$GetDirRmarkdown())
      splice.out <- paste0(dir, report.output$splice)
      
      outputfile <- file(splice.out, 'w+')
      for (i in 1:length(rmarkdown.list)) {
        inputfile <- file(paste0(dir, rmarkdown.list[[i]]), 'rt')
        text <- readLines(inputfile)
        LogDebug(paste("Consolidate", rmarkdown.list[[i]]))
        writeLines(text, outputfile)
        close(inputfile)
      }
      close(outputfile)
      LogDebug(splice.out)
      
    },
    
    GetReportOutFigDir = function() {
      return(paste0(config$GetDirReportOut(), config$GetDirFigure()))
    },
    
    GetReportOutFigDirAbs = function() {
      return(paste0(getwd(), "/", config$GetDirReportIn(), 
                    config$GetDirFigure()))
    },
    
    GetReportOutRmdDir = function() {
      return(paste0(config$GetDirReportOut(), config$GetDirRmarkdown()))
    },
    
    GetReportOutDir = function() {
      if (report.tier == kTierCity) {
        path <- paste0(config$GetDirReportOut(), kTierCity, "/")
      } else if(report.tier == kTierDistrict) {
        path <- paste0(config$GetDirReportOut(), kTierDistrict, "/",
                       report.unit$district, "/")
      } else if(report.tier == kTierSchool) {
        path <- paste0(config$GetDirReportOut(), kTierSchool, "/",
                       report.unit$district, "/")
      }
      return(path)
    },
    
    SetReportTitle = function() {
      subtitle <- ""
      if (report.unit$scope != "" || report.subject != "") {
        subtitle <- paste0("（", report.unit$scope, report.subject, "）")
      }
      report.title <<- paste0(report.copyright$year, 
                      report.copyright$project, 
                      report.name,
                      subtitle)
      return(report.title)
    },
    
    GetReportOutFile = function() {
      return(paste0(report.title, gsub("\\:+", "_", as.character(Sys.time())), 
                    report.output$ext))
    },
    
    ReportPlotFigure = function(plot.code) {
      plot.dir <- GetReportOutFigDir()
      return(Plotfigure(plot.dir, plot.code))
    },
    
    GetPlotTitle = function(plot.code) {
      plot.dir <- GetReportOutFigDir()
      return(GetPlotFigure(plot.dir, plot.code)$name)
    },
    
    OpenChunk = function(chunk.code, input.data) {
      output.data <- input.data
      output.data[length(output.data) + 1] <- 
        paste0("```{r ", chunk.code, 
              ", echo=FALSE, message=FALSE, include = FALSE, result=\"hide\"}")
      return(output.data)
    },
    
    CloseChunk = function(input.data) {
      output.data <- input.data
      output.data[length(output.data) + 1] <- "```"
      return(output.data)
    },
    
    AddChunk = function(chunk.code, input.data) {
      output.data <- OpenChunk(chunk.code, input.data)
      return(CloseChunk(output.data))
    },
    
    PrerenderReport = function(input.file, output.file) {
      
      input.data <- readLines(input.file)
      
      output.data <- "" 
      # prerender.set <- unlist(unique(plot.df[, kColumnPlotCode]))
      preplot.set <- unlist(unique(plot.df[, kColumnPlotCode]))
      prequery.set <- unlist(unique(dataset.df[, kColumnDataset]))
      pretable.set <- unlist(unique(table.df[, kColumnTableRowCode]))
      for (i in 1:length(input.data)) {
        
        input.line <- input.data[i]
        if (is.element(trimws(input.line), preplot.set) || 
            is.element(trimws(input.line), prequery.set) || 
            is.element(trimws(input.line), pretable.set) ){
          prefix <- unlist(strsplit(trimws(input.line), kPrefixConnector))[1]
          if (prefix == kPrefixPlot) {
            if (report.plotfig == TRUE){
              report.fig <- PlotFigure (GetReportOutFigDir(), trimws(input.line))
              
              # output.data <- OpenChunk(input.line, output.data)
              # output.data[length(output.data) + 1] <- 
              #  paste0("plot.fig <- gio.R$GetPlotFigure(gio.R$GetReportOutFigDir(), \"",
              #        input.line, "\")")
              # output.data <- CloseChunk(output.data)
              
              output.data[length(output.data) + 1] <-  " "
              output.data[length(output.data) + 1] <- "\\begin{figure}[H]"
              output.data[length(output.data) + 1] <-
                paste0("\\includegraphics[width=\\textwidth]{", report.fig$path, "}") 
              # paste0("\\includegraphics[width=\\textwidth]{","`r plot.fig$path`","}") 
              output.data[length(output.data) + 1] <- 
                paste0("\\caption{", report.fig$name, "}") 
              output.data[length(output.data) + 1] <- 
                paste0("\\label{fig: ", report.fig$name, "}") 
              output.data[length(output.data) + 1] <- "\\end{figure}"
              output.data[length(output.data) + 1] <-  " "
              
            } else {
              output.data[length(output.data) + 1] <- input.line 
            }
            
            
          } else if (prefix == kPrefixMultiPlot) {
            
            report.fig <- MultiPlotFigure (GetReportOutFigDir(), trimws(input.line))
            
            for (j in 1:length(report.fig$path)) {
              
              output.data[length(output.data) + 1] <-  " "
              output.data[length(output.data) + 1] <- "\\begin{figure}[H]"
              output.data[length(output.data) + 1] <-
                paste0("\\includegraphics[width=\\textwidth]{", report.fig$path[j], "}") 
              # paste0("\\includegraphics[width=\\textwidth]{","`r plot.fig$path`","}") 
              output.data[length(output.data) + 1] <- 
                paste0("\\caption{", report.fig$name[j], "}") 
              output.data[length(output.data) + 1] <- 
                paste0("\\label{fig: ", report.fig$name[j], "}") 
              output.data[length(output.data) + 1] <- "\\end{figure}"
              output.data[length(output.data) + 1] <-  " "
            }
            
          } else if (prefix == kPrefixQuery2Data) {
            output.data <- OpenChunk(input.line, output.data)
            output.data[length(output.data) + 1] <-
              paste0("query2.data <- gio.R$QueryData(\"", 
                     trimws(input.line), "\")") 
            output.data <- CloseChunk(output.data)
            
          } else if (prefix == kPrefixQueryData) {
            output.data <- OpenChunk(trimws(input.line), output.data)
            output.data[length(output.data) + 1] <-
              paste0("query.data <- gio.R$QueryData(\"", 
                     input.line, "\")") 
            output.data <- CloseChunk(output.data)
            
          } else if (prefix == kPrefixTableRow) {
            table.row <- table.df[table.df[, kColumnTableRowCode] == trimws(input.line), ]
            if (nrow(table.row) > 0) {
              table.cols.sample <- 
                unlist(strsplit(table.row[1, kColumnTableDataSample], kSeparator))
              table.cols.tier <- 
                unlist(strsplit(table.row[1, kColumnTableDataTier], kSeparator))
              for (j in 1:length(table.cols.sample)) {
                row.line <-
                  paste0(" & `r ",
                         table.row[1, kColumnTableDataQuery], "(",
                         table.row[1, kColumnTableDataFrame], ", ",
                         "\"", table.row[1, kColumnTableDataVariable], "\", ",
                         "\"", table.cols.tier[min(j,length(table.cols.tier))], "\", ",
                         "\"", table.cols.sample[j], "\", ",
                         "\"", table.row[1, kColumnTableDataKey], "\", ",
                         table.row[1, kColumnTableDataDigit], ")", 
                         "` "
                  )
                output.data[length(output.data) + 1] <- row.line
              }
            }
            
          } else {
            output.data[length(output.data) + 1] <- input.line 
          }
          
        } else {
          output.data[length(output.data) + 1] <- input.line 
        }
        
      }
      
      fo <- file(output.file)
      writeLines(output.data, fo)
      close(fo)
      
    },
    
    LabPers = function(data.set, perspective) {
      return(data.set[data.set[, kColumnStatisticsPerspective] == perspective,
                      kColumnLabel])
    },
    LabPersKey = function(data.set, perspective, key) {
      return(data.set[data.set[, kColumnStatisticsPerspective] == perspective &
                        data.set[, kColumnKey] == key,
                      kColumnLabel])
    },
    
    ScopeReport = function(){
      
      CopyFigureDir(report.subdir)
      CopyRmarkdownDir(report.subdir)
      
      SpliceRmarkdown(report.rmarkdown)
      SetReportTitle()
      
      PrerenderReport( paste0(GetReportOutRmdDir(), report.output$splice), 
                       paste0(GetReportOutRmdDir(), report.output$prerender))
      
      rmd.file <- paste0(GetReportOutRmdDir(), report.output$prerender)
      LogInfo(paste("Start Rendering!", report.output$prerende))
      render(input = rmd.file, 
             output_format = report.output$format, 
             output_dir = GetReportOutDir(),
             output_file = GetReportOutFile(),
             params = list(report = report.name,
                           dummy.fig  = "dummy.fig",
                           conf = "conf")
      )
    },
    
    Report = function(){
      LogInfo("Report!")
      
      # get job configuration
      jobs <- config$GetConfigJob()$report
      reworkall <- config$IsReworkAll()
      reworkjobs <- jobs$TODO
      RUN <- jobs$RUN
      report.plotfig <<- jobs$PLOTFIG
      report.pilot <<- jobs$pilot
      report.output <<- jobs$output
      
      
      PrepareDataframe()
      
      for (i in 1:length(jobs$report)){
        job <- jobs$report[[i]]
        TODO <- job$TODO
        if (TODO || reworkjobs ) {
          
          report.tier <<- job$tier
          report.name <<- job$name
          report.subdir <<- job$subdir
          report.subject <<- paste0(job$subject$name, job$subject$suffix)
          report.rmarkdown <<- job$rmarkdown
          # report.plot <<- job$plot
          # report.plot.multiple <<- job$plot.multiple
          # report.query.data <<- job$query.data

          if (report.tier == kTierCity) {
            
            scopes <- unique(school.df[, kTierCity])
            if (RUN == kPilotRun) {
              scopes <- list(report.pilot$scope$city)
            }
            for (i in 1:length(scopes)) {
              report.scope <<- scopes[[i]]
              report.unit$city <<- report.scope
              report.unit$district <<- ""
              report.unit$shool <<- ""
              report.unit$scope <<- paste0("", #report.unit$city, 
                                           "", 
                                           "")
              ScopeReport()
            }
          } else if (report.tier == kTierDistrict) {
            scopes <- unique(district.df[, kTierDistrict])
            if (RUN == kPilotRun) {
              scopes <- list(report.pilot$scope$district)
            }
            for (i in 1:length(scopes)) {
              report.scope <<- scopes[[i]]
              report.unit$city <<- district.df[district.df[, kTierDistrict] == 
                                                report.scope, kTierCity]
              report.unit$district <<- report.scope
              report.unit$school <<- ""
              report.unit$scope <<- paste0("", # report.unit$city, 
                                           report.unit$district, 
                                           "")
              LogInfo(paste(report.scope, report.unit$district))
              SetDataScope(report.unit)
              ScopeReport()
            }
          } else if (report.tier == kTierSchool) {
            districts <- unique(district.df[, kTierDistrict])
            if (RUN == kPilotRun) {
              districts <- list(report.pilot$scope$district)
            }
            for (i in 1:length(districts)) {
              district <- districts[[i]]
              scopes <- unique(school.df[school.df[, kTierDistrict] == 
                                         district, kTierSchool])
              if (RUN == kPilotRun) {
                scopes <- list(report.pilot$scope$school)
              }
              for (j in 1:length(scopes)) {
                report.scope <<- scopes[[j]]
                report.unit$school <<- report.scope
                report.unit$district <<- district
                report.unit$city <<- district.df[
                  district.df[, kTierDistrict] == district, kTierCity]
                report.unit$scope <<- paste0("", 
                                             "", 
                                             report.unit$school)
                SetDataScope(report.unit)
                LogInfo(paste(report.scope, report.unit$school, report.unit$district, report.unit$city))
                ScopeReport()
              }
            }
            
          } 
          
        }
        
      }
      
    },
    
    GetReportYear = function() {
      return(report.copyright$year)
    },
    GetReportScope = function() {
      return(report.scope)
    },
    GetReportUnitScope = function() {
      return(report.unit$scope)
    },
    GetReportProject = function() {
      return(report.copyright$project)
    },
    GetReportName = function() {
      return(report.name)
    },
    GetReportSubject = function() {
      return(report.subject)
    },
    GetReportCopyRight = function() {
      return(report.copyright)
    },
    GetReportHeader = function() {
      return(report.title)
    }
    
    
  
  )
)
