################################################
# Download GDP long series
gdp.dl <- function()
{

# urls for site download CSV
# 1846 -- Valores a preços correntes (Milhões de Reais)
url1 <- "https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela1846.csv&terr=N&rank=-&query=t/1846/n1/all/v/all/p/all/c11255/all/d/v585%204/l/v%2Bt,c11255,p"

# 6612 -- Valores encadeados a preços de 1995 (Milhões de Reais)
url2 <- "https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela6612.csv&terr=N&rank=-&query=t/6612/n1/all/v/all/p/all/c11255/all/d/v9318%204/l/v%2Bt,c11255,p"

# 6613 -- Valores encadeados a preços de 1995 com ajuste sazonal (Milhões de Reais)
url3 <- "https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela6613.csv&terr=N&rank=-&query=t/6613/n1/all/v/all/p/all/c11255/all/d/v9319%204/l/v%2Bt,c11255,p"

# Download Data json via API
# download.file("https://apisidra.ibge.gov.br/values/t/1846/n1/all/v/all/p/all/c11255/all/d/v585%204", "CNT-SIDRA-1846.json")
# download.file("https://apisidra.ibge.gov.br/values/t/6612/n1/all/v/all/p/all/c11255/all/d/v9318%204", "CNT-SIDRA-6612.json")
# download.file("https://apisidra.ibge.gov.br/values/t/6613/n1/all/v/all/p/all/c11255/all/d/v9319%204", "CNT-SIDRA-6613.json")

# Download Data CSV VIA SITE URL
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
# load gdp raw
gdp.save.df <- function()
{

# NOMINAL
# load trim CSV
data <- load.trim("../../data/CNT-SIDRA-1846.csv", 5, 9) # current 10^6 BRL

# dates # just do it once
dates <- data[,1]
# rownames are HUMAN readable `YYYY:QX`
Q <- substring(dates, 1,1)
Y <- substring(dates, 14,17)
dates <- paste0(Y, ":","Q", Q)

tmp1 <- qtr2date(dates)
tmp2 <- substring(dates, 1,4)
tmp3 <- substring(dates, 7)

# data in data frame and colnames
newdata <- data.frame(tmp1, tmp2, tmp3, sapply(data[,-1], as.numeric) )
colnames(newdata) <- c("date", "year", "qtr", "AGR", "IND", "IND.EXT", "IND.TRANS", "ELEC.ETC", "CONST", "SER", "COMM", "TRANS", "INFO.COM", "FIN", "REAL.ESTATE", "SERV.ETC", "ADM", "VAB", "TAX", "GDP", "C", "G", "FBKF", "DE", "X", "M")
saveRDS(newdata,  "../../data/gdp-nominal.rds" )

# REAL
# load trim CSV
data <- load.trim("../../data/CNT-SIDRA-6612.csv", 5, 9) # chained 10^6 BRL from 1995 NSA
# data in data frame and colnames
newdata <- data.frame(tmp1, tmp2, tmp3, sapply(data[,-1], as.numeric) )
colnames(newdata) <- c("date", "year", "qtr", "AGR", "IND", "IND.EXT", "IND.TRANS", "ELEC.ETC", "CONST", "SER", "COMM", "TRANS", "INFO.COM", "FIN", "REAL.ESTATE", "SERV.ETC", "ADM", "VAB", "TAX", "GDP", "C", "G", "FBKF", "X", "M")
saveRDS(newdata,  "../../data/gdp-real-NSA.rds" )

# REAL with SESONAL ADJ
# load trim CSV
data <- load.trim("../../data/CNT-SIDRA-6613.csv", 5, 9) # chained 10^6 BRL from 1995 SA
# data in data frame and colnames
newdata <- data.frame(tmp1, tmp2, tmp3, sapply(data[,-1], as.numeric) )
colnames(newdata) <- c("date", "year", "qtr", "AGR", "IND", "IND.EXT", "IND.TRANS", "ELEC.ETC", "CONST", "SER", "COMM", "TRANS", "INFO.COM", "FIN", "REAL.ESTATE", "SERV.ETC", "ADM", "VAB", "GDP", "C", "G", "FBKF", "X", "M")
# SAVE DF in RDS
saveRDS(newdata,  "../../data/gdp-real-SA.rds" )

return(1)
}

################################################
# transform gdp
gdp.transform <- function()
{

gdp.nom <- readRDS("../../data/gdp-nominal.rds")
gdp.rea <- readRDS("../../data/gdp-real-NSA.rds")
gdp.rsa <- readRDS("../../data/gdp-real-SA.rds")

# T/T-1
gdp.ret1 <- cbind( gdp.rsa[,c(1,2,3)], apply(gdp.rsa[,-c(1,2,3)], 2, ret1) );
saveRDS(gdp.ret1[-1,],  "../../data/gdp-ret1.rds" )

# T/T-4
gdp.ret4 <- cbind( gdp.rea[,c(1,2,3)], apply(gdp.rea[,-c(1,2,3)], 2, ret4) );
saveRDS(gdp.ret4[-c(1,2,3,4),],  "../../data/gdp-ret4.rds" )

# AC4Q
gdp.ac4  <- cbind( gdp.rea[,c(1,2,3)], apply(gdp.rea[,-c(1,2,3)], 2, sum4) );
saveRDS(gdp.ac4[-c(1,2,3),],  "../../data/gdp-ac4.rds" )

# T/T-4 AC4Q
gdp.rac  <- cbind( gdp.rea[,c(1,2,3)], apply(gdp.rea[,-c(1,2,3)], 2, retsum4) );
saveRDS(gdp.rac[-seq(1,7),],  "../../data/gdp-ret-ac4.rds" )

# # share of GDP
# gdp.shr  <- cbind( gdp.nom[,c(1,2,3)], 100*gdp.nom[,-c(1,2,3)]/gdp.nom$GDP );
# saveRDS(gdp.shr[-seq(1,7),],  "../../data/gdp-share.rds" )
#
# # share of GDP
# vab.shr  <- cbind( gdp.nom[,c(1,2,3)], 100*gdp.nom[,-c(1,2,3)]/gdp.nom$VAB );
# saveRDS(vab.shr[-seq(1,7),],  "../../data/vab-share.rds" )

return(1)
}

####################################################
gdp.dl.save <- function()
{
gdp.dl()
gdp.save.df()
gdp.transform()
return(1)
}

####################################################
gdp.load.df <- function(type)
{
# READ in folder and in RDS format
types <- c("nominal", "real", "real.NSA", "real.SA")
if(!type %in% types) stop("\nYour requested type: \"", type, "\" is NOT valid.\n Valid options are:\n \"nominal\", \"real\", \"real.NSA\", \"real.SA\"")

# nominal
if(type == "nominal")
{
data <- readRDS("../../data/gdp-nominal.rds")
} else
# real
if(type %in% c("real", "real.NSA") )
{
data <- readRDS("../../data/gdp-real-NSA.rds")
} else
# real with Seasonl ADJ
if(type == "real.SA")
{
data <- readRDS("../../data/gdp-real-SA.rds")
}

return(data)
}

####################################################
df2xts <- function(df)
{

# require xts
# if(!require(xts)) stop("\nInstall xts package:\nHere use the following line:\ninstall.packages(\"xts\")")
# require data.frame
if(!class(df) == "data.frame") stop("\nYour data is not a dataframe.\nConvert your data")

df.xts <- xts(df[,-1], order.by=df[,1] )
return( df.xts )
}

####################################################
gdp.load.xts <- function(type)
{
# require xts
if(!require(xts)) stop("\nInstall xts package.\nHere use the following line:\ninstall.packages(\"xts\")")
data <- gdp.load(type)
newdata <- gdp.df2xts(data)
return(newdata)
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
# ret (t/t-4) of the sum of last 4 quarters
retsum4 <- function(data) return( ret4( sum4( data ) ) )

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

