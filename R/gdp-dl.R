#!
# LOAD FUNCTIONS
source("./funs-gdp-dl.R")

# DOWNLOAD and SAVE DATA
gdp.dl.save("../data/")

# DOWNLOAD DATA
gdp.dl.csv("../data/")
gdp.dl.json("../data/")

# SAVE DATA
gdp.save.rds("../data/")











