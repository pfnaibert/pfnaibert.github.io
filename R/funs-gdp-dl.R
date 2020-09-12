################################################
gdp.dl.save <- function(folderpath = "./")
{
gdp.dl.csv(folderpath)
gdp.save.rds(folderpath)
}


################################################
gdp.dl.csv <- function(folderpath = "./")
{
# Download GDP long series with csv

# Tabela 1846 - Valores a preços correntes (Milhões de Reais)
# Tabela 6612 - Valores encadeados a preços de 1995 (Milhões de Reais)
# Tabela 6613 - Valores encadeados a preços de 1995 com ajuste sazonal (Milhões de Reais)
# Tabela 1620 - Série encadeada do índice de volume trimestral (Base: média 1995 = 100)
# Tabela 1621 - Série encadeada do índice de volume trimestral com ajuste sazonal (Base: média 1995 = 100)
# Tabela 5932 - Taxa de variação do índice de volume trimestral

# urls for site download CSV
urls <- c("https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela1846.csv&terr=N&rank=-&query=t/1846/n1/all/v/all/p/all/c11255/all/d/v585%204/l/v%2Bt,c11255,p",
"https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela6612.csv&terr=N&rank=-&query=t/6612/n1/all/v/all/p/all/c11255/all/d/v9318%204/l/v%2Bt,c11255,p",
"https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela6613.csv&terr=N&rank=-&query=t/6613/n1/all/v/all/p/all/c11255/all/d/v9319%204/l/v%2Bt,c11255,p",
"https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela1620.csv&terr=N&rank=-&query=t/1620/n1/all/v/all/p/all/c11255/all/d/v583%204/l/t%2Bv,c11255,p" ,
"https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela1621.csv&terr=N&rank=-&query=t/1621/n1/all/v/all/p/all/c11255/all/d/v584%204/l/t%2Bv,c11255,p" ,
"https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela5932.csv&terr=N&rank=-&query=t/5932/n1/all/v/6561/p/all/c11255/all/d/v6561%201/l/t%2Bv,c11255,p", # QoQ-4
"https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela5932.csv&terr=N&rank=-&query=t/5932/n1/all/v/6562/p/all/c11255/all/d/v6562%201/l/t%2Bv,c11255,p", # 4Qo4Q
"https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela5932.csv&terr=N&rank=-&query=t/5932/n1/all/v/6563/p/all/c11255/all/d/v6563%201/l/t%2Bv,c11255,p", # Ac. Year
"https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela5932.csv&terr=N&rank=-&query=t/5932/n1/all/v/6564/p/all/c11255/all/d/v6564%201/l/t%2Bv,c11255,p") # QoQ-1

tmp <- c("1846", "6612", "6613", "1620", "1621", "5932-ret4", "5932-retsum4", "5932-retyear", "5932-ret1")
table.names <- paste0("CNT-SIDRA-", tmp, ".csv")

# Download Data CSV VIA SITE URL
for(i in seq_along(table.names)) download.file( urls[i], paste0(folderpath, table.names[i] ) ) # current 10^6 BRL

# success message
return(1)
}

################################################
gdp.dl.json <- function(folderpath = "./")
{
# Download GDP long series with json via API

# Tabela 1846 - Valores a preços correntes (Milhões de Reais)
# Tabela 6612 - Valores encadeados a preços de 1995 (Milhões de Reais)
# Tabela 6613 - Valores encadeados a preços de 1995 com ajuste sazonal (Milhões de Reais)
# Tabela 1620 - Série encadeada do índice de volume trimestral (Base: média 1995 = 100)
# Tabela 1621 - Série encadeada do índice de volume trimestral com ajuste sazonal (Base: média 1995 = 100)
# Tabela 5932 - Taxa de variação do índice de volume trimestral

# urls
urls <- c( "https://apisidra.ibge.gov.br/values/t/1846/n1/all/v/all/p/all/c11255/all/d/v585%204",
		  "https://apisidra.ibge.gov.br/values/t/6612/n1/all/v/all/p/all/c11255/all/d/v9318%204",
		  "https://apisidra.ibge.gov.br/values/t/6613/n1/all/v/all/p/all/c11255/all/d/v9319%204",
		  "https://apisidra.ibge.gov.br/values/t/1620/n1/all/v/all/p/all/c11255/all/d/v583%204",
		  "https://apisidra.ibge.gov.br/values/t/1621/n1/all/v/all/p/all/c11255/all/d/v584%204",
		  "https://apisidra.ibge.gov.br/values/t/5932/n1/all/v/all/p/all/c11255/all/d/v6561%201,v6562%201,v6563%201,v6564%201" )

tmp <- c("1846", "6612", "6613", "1620", "1621", "5932")
table.names <- paste0("CNT-SIDRA-", tmp, ".json")

# Download Data json via API
for(i in seq_along(table.names)) download.file( urls[i], paste0(folderpath, table.names[i] ) ) # current 10^6 BRL

# success message
return(1)
}

################################################
gdp.save.rds <- function(folderpath = "./")
{
# Save gdp series as data.frame in rds format

tmp1 <- c("1846", "1620", "1621", "6612", "6613", "5932-ret4", "5932-ret1", "5932-retsum4", "5932-retyear")
tmp2 <- c("nominal", "index-NSA", "index-SA", "real-NSA", "real-SA", "ret4", "ret1", "retsum4", "retyear")

table.names  <- paste0( "CNT-SIDRA-", tmp1, ".csv" )
series.names <- paste0("gdp-", tmp2, ".rds")

for(i in seq_along(series.names))
{
data              <- load.trim(paste0(folderpath, table.names[i]), 5, 9) # load trim CSV
newdata           <- gdp.csv2df(data)                                    # data into data frame
colnames(newdata) <- gdp.vars(NCOL(data))                                # put colnames
saveRDS(newdata,  paste0(folderpath, series.names[i]) )                  # save in RDS
}

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
gdp.csv2df <- function(data)
{
# transform gdp data into df
dates  <- gdp.dates(data[,1])
gdp.df <- data.frame(dates, sapply(data[,-1], as.numeric) )
return(gdp.df)
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

####################################################
qtr2date <- function(data)
{
data <- gsub(":Q1", "-03-31", data)
data <- gsub(":Q2", "-06-30", data)
data <- gsub(":Q3", "-09-30", data)
data <- gsub(":Q4", "-12-31", data)
return( as.Date(data) )
}
