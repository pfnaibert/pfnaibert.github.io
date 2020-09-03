################################################
gdp.dl.csv <- function()
{
# Download GDP long series with csv

# urls for site download CSV

# 1846 -- Valores a preços correntes (Milhões de Reais)
url1 <- "https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela1846.csv&terr=N&rank=-&query=t/1846/n1/all/v/all/p/all/c11255/all/d/v585%204/l/v%2Bt,c11255,p"

# 6612 -- Valores encadeados a preços de 1995 (Milhões de Reais)
url2 <- "https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela6612.csv&terr=N&rank=-&query=t/6612/n1/all/v/all/p/all/c11255/all/d/v9318%204/l/v%2Bt,c11255,p"

# 6613 -- Valores encadeados a preços de 1995 com ajuste sazonal (Milhões de Reais)
url3 <- "https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela6613.csv&terr=N&rank=-&query=t/6613/n1/all/v/all/p/all/c11255/all/d/v9319%204/l/v%2Bt,c11255,p"

# Tabela 1620 - Série encadeada do índice de volume trimestral (Base: média 1995 = 100)
url4 <- "https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela1620.csv&terr=N&rank=-&query=t/1620/n1/all/v/all/p/all/c11255/all/d/v583%204/l/t%2Bv,c11255,p"

# Tabela 5932 - Taxa de variação do índice de volume trimestral
url5 <- "https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela5932.csv&terr=N&rank=-&query=t/5932/n1/all/v/6561/p/all/c11255/all/d/v6561%201/l/t%2Bv,c11255,p" # QoQ-4
url6 <- "https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela5932.csv&terr=N&rank=-&query=t/5932/n1/all/v/6562/p/all/c11255/all/d/v6562%201/l/t%2Bv,c11255,p" # 4Qo4Q
url7 <- "https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela5932.csv&terr=N&rank=-&query=t/5932/n1/all/v/6563/p/all/c11255/all/d/v6563%201/l/t%2Bv,c11255,p" # Ac. Year
url8 <- "https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela5932.csv&terr=N&rank=-&query=t/5932/n1/all/v/6564/p/all/c11255/all/d/v6564%201/l/t%2Bv,c11255,p" # QoQ-1

# Download Data CSV VIA SITE URL
download.file(url1, "../../data/CNT-SIDRA-1846.csv") # current 10^6 BRL
download.file(url2, "../../data/CNT-SIDRA-6612.csv") # chained 10^6 BRL from 1995 NSA
download.file(url3, "../../data/CNT-SIDRA-6613.csv") # chained 10^6 BRL from 1995 SA
download.file(url4, "../../data/CNT-SIDRA-1620.csv") # chained series of quarterly volume (1995=100)
download.file(url5, "../../data/CNT-SIDRA-5932-ret4.csv") #
download.file(url6, "../../data/CNT-SIDRA-5932-retsum4.csv") #
download.file(url7, "../../data/CNT-SIDRA-5932-retyear.csv") #
download.file(url8, "../../data/CNT-SIDRA-5932-ret1.csv") #

# success message
return(1)
}

####################################################
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
# gdp dates
gdp.dates <- function(dates)
{
# rownames are HUMAN readable `YYYY:QX`
Q <- substring(dates, 1,1)
Y <- substring(dates, 14,17)
dates <- paste0(Y, ":","Q", Q)

newdates <- qtr2date(dates)
year     <- substring(dates, 1,4)
quarter  <- substring(dates, 7)
date.df  <- data.frame(newdates, year, quarter)
return(date.df)
}

################################################
gdp.csv2df <- function(data)
{
# transform gdp data into df
dates  <- gdp.dates(data[,1])
gdp.df <- data.frame(dates, sapply(data[,-1], as.numeric) )
return(gdp.df)
}

################################################
gdp.vars <- function(nvars)
{
# gdp variables

# 21 vars plus dates (in the csv file)
vars.22 <- c("date", "year", "qtr", "AGR", "IND", "IND.EXT", "IND.TRANS", "ELEC.ETC", "CONST", "SER", "COMM", "TRANS", "INFO.COM", "FIN", "REAL.ESTATE", "SERV.ETC", "ADM", "VAB", "GDP", "C", "G", "FBKF", "X", "M")

# 22 vars plus dates (in the csv file)
vars.23 <- c("date", "year", "qtr", "AGR", "IND", "IND.EXT", "IND.TRANS", "ELEC.ETC", "CONST", "SER", "COMM", "TRANS", "INFO.COM", "FIN", "REAL.ESTATE", "SERV.ETC", "ADM", "VAB", "TAX", "GDP", "C", "G", "FBKF", "X", "M")

# 23 vars plus dates (in the csv file)
vars.24 <- c("date", "year", "qtr", "AGR", "IND", "IND.EXT", "IND.TRANS", "ELEC.ETC", "CONST", "SER", "COMM", "TRANS", "INFO.COM", "FIN", "REAL.ESTATE", "SERV.ETC", "ADM", "VAB", "TAX", "GDP", "C", "G", "FBKF", "DE", "X", "M")

if (!(nvars %in% c(22,23,24)) ) stop("invalid number of variales")
else
if(nvars==24) vars <- vars.24
else
if(nvars==23) vars <- vars.23
else
if(nvars==22) vars <- vars.22

return(vars)
}

################################################
gdp.save.rets <- function()
{
# Save gdp series as data.frame in rds format

# CHANGE -- ret4
data              <- load.trim("../../data/CNT-SIDRA-5932-ret4.csv", 5, 9) # load trim CSV
newdata           <- gdp.csv2df(data)                                      # data into data frame
colnames(newdata) <- gdp.vars(23)                                          # put colnames
saveRDS(newdata,  "../../data/gdp-ret4.rds" )                              # save in RDS

# CHANGE -- retsum4
data              <- load.trim("../../data/CNT-SIDRA-5932-retsum4.csv", 5, 9) # load trim CSV
newdata           <- gdp.csv2df(data)                                         # data into data frame
colnames(newdata) <- gdp.vars(23)                                             # put colnames
saveRDS(newdata,  "../../data/gdp-retsum4.rds" )                              # save in RDS

# CHANGE -- retyear
data              <- load.trim("../../data/CNT-SIDRA-5932-retyear.csv", 5, 9) # load trim CSV
newdata           <- gdp.csv2df(data)                                         # data into data frame
colnames(newdata) <- gdp.vars(23)                                             # put colnames
saveRDS(newdata,  "../../data/gdp-retyear.rds" )                              # save in RDS

# CHANGE -- ret1
data              <- load.trim("../../data/CNT-SIDRA-5932-ret1.csv", 5, 9) # load trim CSV
newdata           <- gdp.csv2df(data)                                      # data into data frame
colnames(newdata) <- gdp.vars(23)                                          # put colnames
saveRDS(newdata,  "../../data/gdp-ret1.rds" )                              # save in RDS

return(1)
}

################################################
gdp.save.levels <- function()
{
# Save gdp series as data.frame in rds format

# NOMINAL -- value in current 10^6 BRL
data              <- load.trim("../../data/CNT-SIDRA-1846.csv", 5, 9) # load trim CSV
newdata           <- gdp.csv2df(data)                                 # data into data frame
colnames(newdata) <- gdp.vars(24)                                     # put colnames
saveRDS(newdata,  "../../data/gdp-nominal.rds" )                      # save in RDS

# Volume index
data              <- load.trim("../../data/CNT-SIDRA-1620.csv", 5, 9) # load trim CSV
newdata           <- gdp.csv2df(data)                                 # data into data frame
colnames(newdata) <- gdp.vars(23)                                     # put colnames
saveRDS(newdata,  "../../data/gdp-index-NSA.rds" )                    # save in RDS

# REAL -- value in chained 10^6 BRL from 1995 NSA
data              <- load.trim("../../data/CNT-SIDRA-6612.csv", 5, 9) # load trim CSV
newdata           <- gdp.csv2df(data)                                 # data into data frame
colnames(newdata) <- gdp.vars(23)                                     # put colnames
saveRDS(newdata,  "../../data/gdp-real-NSA.rds" )                     # save in RDS

# REAL Seasonally Adjusted -- value in chained 10^6 BRL from 1995 SA
data              <- load.trim("../../data/CNT-SIDRA-6613.csv", 5, 9) # load trim CSV
newdata           <- gdp.csv2df(data)                                 # data into data frame
colnames(newdata) <- gdp.vars(22)                                     # put colnames
saveRDS(newdata,  "../../data/gdp-real-SA.rds" )                      # save in RDS

return(1)
}

####################################################
df2ts <- function(df)
{
# require data.frame
if(!class(df) == "data.frame") stop("\nYour data is not a dataframe.\nConvert your data")

df.ts <- ts(df[,-c(1,2,3)], start=c(1996,1), freq=4 )
return( df.ts )
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
date2qtr <- function(data)
{
newdata <- as.character(data)
newdata <- gsub("-03-31", ":Q1", newdata)
newdata <- gsub("-06-30", ":Q2", newdata)
newdata <- gsub("-09-30", ":Q3", newdata)
newdata <- gsub("-12-31", ":Q4", newdata)
return( newdata )
}

####################################################
normalize <- function(data, date)
{
if(!class(data)=="ts") stop("The function can only be performed with a `ts` object")
# only accepts ts object
tmp <- window(data, start=date, end=date)
den <- rep( tmp, NROW(data) )
newdata <- 100*data / den
return(newdata)
}

####################################################
normalize.yr <- function(data, year)
{
if(!class(data)=="ts") stop("The function can only be performed with a `ts` object")
tmp <- window(data, start=c(year,1), end=c(year,4))
den <- mean(tmp)
newdata <- 100*data/den
return( newdata )
}

####################################################
sum4 <- function(data)
{
# sum last 4 quarters
T <- NROW(data);  dates <- names(data)
tmp <- rep(NA, (T-3)); names(tmp) <- dates[-seq(1:3)]

for(i in 1:(T-3))
{
	w1     <- seq(i,i+4-1)
	tmp[i] <- sum(data[w1])
}

return( c(rep(NA, 3), tmp) )
}


