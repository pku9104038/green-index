##############################################

library(yaml)

conf <- yaml.load_file("yaml/conf.yaml")
g.dir <- conf$dir
g.yaml <- conf$yaml
dbname <- conf$db$dbname
# source scripts
source(paste0(g.dir$R,"ETL/db.R"))

####################################
table <- "校长问卷变量"
data <- db.ReadTable(table = table)