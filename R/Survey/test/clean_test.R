library(yaml)
# init global configurations
g.dir <- yaml.load_file("yaml/conf.yaml")$dir
stat <- yaml.load_file(paste0(g.dir$yaml,"survey.yaml"))$define$statistics
surveys <- stat$survey


# source scripts
source(paste0(g.dir$R,"ETL/db.R"))
source(paste0(g.dir$R,"Survey/clean.R"))


# merge survey table with student, school table
survey.merge(surveys = surveys, dryrun = FALSE)

# calculate variables
survey.variables(stat, surveys, dryrun  = FALSE)

# add weight for school level
survey.weight(surveys, dryrun  = FALSE)



library(yaml)
# init global configurations
g.dir <- yaml.load_file("yaml/conf.yaml")$dir
stat <- yaml.load_file(paste0(g.dir$yaml,"score.yaml"))$define$statistics
surveys <- stat$test

# source scripts
source(paste0(g.dir$R,"ETL/db.R"))
source(paste0(g.dir$R,"Survey/clean.R"))

# merge survey table with student, school table
survey.merge(surveys = surveys, dryrun = FALSE)

# calculate variables
survey.variables(stat, surveys, dryrun  = FALSE)

# add weight for school level
survey.weight(surveys, dryrun  = FALSE)


