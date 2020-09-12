#!
# SAVE DATA
source("./funs-ipca-dl.R")
# ipca.dl.csv("../data/")
ipca.save.rds("../data/")

# load IPCA
ipca <- readRDS("../data/ipca.rds")
tail(ipca)




