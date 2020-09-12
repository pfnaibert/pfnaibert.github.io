# load data
# levels
gdp.nominal   <- readRDS("../data/gdp-nominal.rds");   head(gdp.nominal)
gdp.real.NSA  <- readRDS("../data/gdp-real-NSA.rds");  head(gdp.real.NSA)
gdp.real.SA   <- readRDS("../data/gdp-real-SA.rds");   head(gdp.real.SA)
gdp.index.NSA <- readRDS("../data/gdp-index-NSA.rds"); head(gdp.index.NSA)
gdp.index.SA  <- readRDS("../data/gdp-index-SA.rds");  head(gdp.index.SA)

# rets
gdp.ret4    <- readRDS("../data/gdp-ret4.rds");    head(gdp.ret4)
gdp.ret1    <- readRDS("../data/gdp-ret1.rds");    head(gdp.ret1)
gdp.retsum4 <- readRDS("../data/gdp-retsum4.rds"); head(gdp.retsum4)
gdp.retyear <- readRDS("../data/gdp-retyear.rds"); head(gdp.retyear)

