source("../../R/funs-macro.R")
library(xts)
# gdp.dl()

gdp.transform()
gdp.df  <- gdp.load.df() ; head(gdp.df)
gdp.xts <- gdp.load.xts(); head(gdp.xts)

