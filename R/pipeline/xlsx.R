## XlsxClass for green index data processing and report generation


## flag for source loaded checking
xlsx.loaded <- TRUE

## define GreenIndexXlsx
library(methods)
library(xlsx)
library(readxl)

GreenIndexXlsx <- setRefClass(
  "GreenIndexXlsx",
  contains = "GreenIndexBase",
  
  fields = list(
    
  ),
  
  methods = list(
    
    ReadXlsxSheet = function(file, sheet.name = "Sheet1"){
      path <- paste0(config$GetDirDataIn(), file)
      LogDebug(paste("Read data.frame from", path, sheet.name))
      #df <- read.xlsx2(path, sheetName = sheet.name)
      df <- read_xlsx(path, sheet = sheet.name, guess_max = 80000)
      return(df)
    },
    
    WriteXlsxSheet = function(x, file, sheet.name = "Sheet1"){
      LogDebug(paste("Write data.frame to", file, sheet.name))
      write.xlsx2(x, file, sheetName = sheet.name, row.names = FALSE, 
                  showNA = FALSE)
      
    },
    AppendXlsxSheet = function(x, file, sheet.name = "Sheet1"){
      LogDebug(paste("Append data.frame to", file, sheet.name))
      write.xlsx2(x, file, sheetName = sheet.name, row.names = FALSE, 
                  showNA = FALSE, append = TRUE)
      
    },
    RemoveXlsxSheet = function(x, file, sheet.name){
      
    }
  )
)
  
  