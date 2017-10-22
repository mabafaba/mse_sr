setwd("/Users/m/paper_january17")
src<-"./code/data/all_LU.shp"

# init does:
# load all dependencies and other source files
# set analysis parameters

source("./code/scripts/init.R")

results<-readRDS("./results_april_15.RDS")
nullmod<-readRDS("./nullmods_april_15.RDS")
results_synthetic<-readRDS("./_april_15.RDS")
parameters_loaded<-readRDS("./parameters_april_15.RDS")
plots_synthetic_from_saved_files(results = results,prefix = "test",synthetic_many_results = synthetic_results)


plots_synthetic(results_synthetic,"testsynthetic",synthetic_many_results)








