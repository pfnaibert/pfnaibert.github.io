################################################
# load libraries
library(xts)

################################################
# Download GDP long series
gdp.dl <- function()
{

# urls
url1 <- "https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela1846.csv&terr=N&rank=-&query=t/1846/n1/all/v/all/p/all/c11255/90707/d/v585%204/l/t%2Bc11255%2Bv,,p"
url2 <- "https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela6612.csv&terr=N&rank=-&query=t/6612/n1/all/v/all/p/all/c11255/90707/d/v9318%204/l/t%2Bc11255%2Bv,,p"
url3 <- "https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela6613.csv&terr=N&rank=-&query=t/6613/n1/all/v/all/p/all/c11255/90707/d/v9319%204/l/v%2Bt,c11255,p"

# Download Data CSV
download.file(url1, "../../data/CNT-SIDRA-1846.csv") # current 10^6 BRL
download.file(url2, "../../data/CNT-SIDRA-6612.csv") # chained 10^6 BRL from 1995 NSA
download.file(url3, "../../data/CNT-SIDRA-6613.csv") # chained 10^6 BRL from 1995 SA

# success message
return(1)
}

####################################################
# LOAD files
load.trim <- function(filename, nhead, ntail)
{
# load trimmed files without the first nhead lines and the last ntail lines

# trim head
data <- read.csv(filename, stringsAsFactors = F, skip = nhead, header = F, sep = ",")
# trim tail
newdata <- head(data, -ntail)
return(newdata)
}

################################################
# transform gdp
gdp.transform <- function()
{
# load trim CSV
x1 <- load.trim("../../data/CNT-SIDRA-1846.csv", 5, 9) # current 10^6 BRL
x2 <- load.trim("../../data/CNT-SIDRA-6612.csv", 5, 9) # chained 10^6 BRL from 1995 NSA
x3 <- load.trim("../../data/CNT-SIDRA-6613.csv", 5, 9) # chained 10^6 BRL from 1995 SA

# dates
dates <- x1[,1]

# data
data1 <- as.numeric( x1[,2] )
data2 <- as.numeric( x2[,2] )
data3 <- as.numeric( x3[,2] )

# names as dates
# rownames are HUMAN readable `YYYY:QX`
Q <- substring(dates, 1,1)
Y <- substring(dates, 14,17)
dates <- names(data1) <- names(data2) <- names(data3) <- paste0(Y, ":","Q", Q)

# gdp.list <- list("nominal"=data1, "real.NSA"=data2, "real.SA"=data3)
gdp.df <- data.frame("dates"=qtr2date(dates), "nominal"=data1, "real.NSA"=data2, "real.SA"=data3)

# add some transformed data
gdp.df$ret1      <- ret1(gdp.df$real.SA)  # t/t-1 SA
gdp.df$ret4      <- ret4(gdp.df$real.NSA) # t/t-4 NSA

gdp.df$sum4.nomi <- sum4(gdp.df$nominal)  # sum 4q
gdp.df$sum4.real <- sum4(gdp.df$real.NSA) # sum 4q

gdp.df$ret.ac4q.nomi <- ret4(sum4(gdp.df$nominal))  # t/t-4 of sums
gdp.df$ret.ac4q.real <- ret4(sum4(gdp.df$real.NSA)) # t/t-4 of sums

# SAVE DF in RDS
saveRDS(gdp.df,  "../../data/gdp-df.rds" )

# transform df into xts
gdp.xts <- xts(gdp.df[,-1], order.by=qtr2date(dates) )

# SAVE xts in RDS
saveRDS(gdp.xts,  "../../data/gdp-xts.rds" )

# success message
return(1)
}

####################################################
gdp.load.df <- function()
{
# READ in folder and in RDS format
data <- readRDS("../../data/gdp-df.rds" )
return(data)
}

####################################################
gdp.load.xts <- function()
{
# READ in folder and in RDS format
data <- readRDS("../../data/gdp-xts.rds" )
return(data)
}

####################################################
# ret (t/t-1)
ret1 <- function(data)
{
T <- NROW(data)
tmp1 <- data[-1]; tmp2 <- data[-T]
ret  <- c(NA, 100*(tmp1/tmp2 - 1) )
return(ret)
}

####################################################
# ret (t/t-4)
ret4 <- function(data)
{
T    <- NROW(data)
id1  <- seq(1,4); id2 <- seq(T-3,T)
tmp1 <- data[-id1]; tmp2 <- data[-id2]
ret  <- c( rep(NA, 4), 100*(tmp1/tmp2 - 1) )
return(ret)
}

####################################################
# sum last 4 quarters
sum4 <- function(data)
{
T <- NROW(data);  dates <- names(data)
tmp <- rep(NA, (T-3)); names(tmp) <- dates[-seq(1:3)]
for(i in 1:(T-3))
{
    w1     <- seq(i,i+4-1)
    tmp[i] <- sum(data[w1])
}
return( c(rep(NA, 3), tmp) )
}

####################################################
qtr2date <- function(data)
{
data <- gsub(":Q1", "-03-31", data)
data <- gsub(":Q2", "-06-30", data)
data <- gsub(":Q3", "-09-30", data)
data <- gsub(":Q4", "-12-31", data)
return( as.Date(data) )
}

####################################################
qtr2num <- function(data) return( as.numeric(substring(data, 1, 4)) - 1/4 + as.numeric( substring(data, 7) )/4 )

####################################################
normalize <- function(data, date) return( 100*data / rep( data[date], NROW(data) ) )

####################################################
normalize.yr <- function(data, date) return( 100*data/mean(data[date]) )
