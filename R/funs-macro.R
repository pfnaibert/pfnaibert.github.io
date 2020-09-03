# load data
# levels
gdp.nominal <- readRDS("../../data/gdp-nominal.rds")
gdp.index   <- readRDS("../../data/gdp-index-NSA.rds")
gdp.real    <- readRDS("../../data/gdp-real-NSA.rds")

# rets
gdp.ret4    <- readRDS("../../data/gdp-ret4.rds")
gdp.ret1    <- readRDS("../../data/gdp-ret1.rds")
gdp.retsum4 <- readRDS("../../data/gdp-retsum4.rds")
gdp.retyear <- readRDS("../../data/gdp-retyear.rds")

# load libraries
library(reshape2)
library(ggplot2)
library(ggthemes)

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

################################################
gdp.dl.json <- function()
{
# Download GDP long series with json via API
# Download Data json via API

# 1846 -- Valores a preços correntes (Milhões de Reais)
download.file("https://apisidra.ibge.gov.br/values/t/1846/n1/all/v/all/p/all/c11255/all/d/v585%204", "CNT-SIDRA-1846.json")

# 6612 -- Valores encadeados a preços de 1995 (Milhões de Reais)
download.file("https://apisidra.ibge.gov.br/values/t/6612/n1/all/v/all/p/all/c11255/all/d/v9318%204", "CNT-SIDRA-6612.json")

# 6613 -- Valores encadeados a preços de 1995 com ajuste sazonal (Milhões de Reais)
download.file("https://apisidra.ibge.gov.br/values/t/6613/n1/all/v/all/p/all/c11255/all/d/v9319%204", "CNT-SIDRA-6613.json")

# Tabela 1620 - Série encadeada do índice de volume trimestral (Base: média 1995 = 100)
download.file("https://apisidra.ibge.gov.br/values/t/1620/n1/all/v/all/p/all/c11255/all/d/v583%204", "CNT-SIDRA-1620.json")

# Tabela 5932 - Taxa de variação do índice de volume trimestral
download.file("https://apisidra.ibge.gov.br/values/t/5932/n1/all/v/all/p/all/c11255/all/d/v6561%201,v6562%201,v6563%201,v6564%201", "CNT-SIDRA-5932.json")

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
gdp.load.xts <- function(type)
{
# require xts
# if(!require(xts)) stop("\nInstall xts package.\nHere use the following line:\ninstall.packages(\"xts\")")
data <- gdp.load.df(type)
newdata <- gdp.df2xts(data)
return(newdata)
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
qtr2num <- function(data) return( as.numeric(substring(data, 1, 4)) - 1/4 + as.numeric( substring(data, 7) )/4 )

####################################################
normalize <- function(data, date) return( 100*data / rep( data[date], NROW(data) ) )

####################################################
normalize.yr <- function(data, date) return( 100*data/mean(data[date]) )

####################################################
# gdp table function
gdp.table <- function(data, type="level" , period="qtr", digits=0)
{
# TODO: ERROR CATCHING

# subset vars by type
if(type=="level") vars <- c("AGR", "IND", "SER", "VAB", "TAX", "GDP", "C", "G", "FBKF", "DE", "X", "M")
else if(type=="ret4") vars <- c("AGR", "IND", "SER", "VAB", "TAX", "GDP", "C", "G", "FBKF", "X", "M")
else if(type=="ret1") vars <- c("AGR", "IND", "SER", "VAB", "GDP", "C", "G", "FBKF", "X", "M")

# subset time by period
if(period =="year") data <- data[c(grepl("*-12-*", data$date)[-NROW(data)], TRUE),]

# trasnform data
newdata <- data.frame( t( apply( data[,vars], 2 , rev) ) )

# attribute colnames
if(period =="qtr") colnames(newdata) <- rev( paste0( data[,"year"], ":Q", data[,"qtr"] ) )
else if(period =="year") colnames(newdata) <- rev( c( paste0( data[-NROW(data),"year"] ), paste0( data[NROW(data),"year"], ":Q", data[NROW(data),"qtr"] ) ) )

# return table
return( round(newdata, digits) )
}

####################################################
ret1 <- function(data)
{
# ret (t/t-1)

T <- NROW(data)
tmp1 <- data[-1]; tmp2 <- data[-T]
ret  <- c(NA, 100*(tmp1/tmp2 - 1) )
return(ret)
}

####################################################
ret4 <- function(data)
{
# ret (t/t-4)

T    <- NROW(data)
id1  <- seq(1,4); id2 <- seq(T-3,T)
tmp1 <- data[-id1]; tmp2 <- data[-id2]
ret  <- c( rep(NA, 4), (tmp1/tmp2 - 1) )
return(ret)
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

####################################################
# ret (t/t-4) of the sum of last 4 quarters
retsum4 <- function(data) return( ret4( sum4( data ) ) )

# ####################################################
# # contribution
# contrib <- function(data, l=4, var.num, vars.den)
# {
# T    <- NROW(data)
# id1  <- seq(1,l); id2 <- seq(T-(l-1),T)
# tmp1 <- data[-id1,]; tmp2 <- data[-id2,]
#
# den <- apply(tmp1[,vars.den], 1, sum)
# num <- tmp1[,var.num] - tmp2[,var.num]
#
# contrib  <- c( rep(NA, l), 100*num/den )
# return(contrib)
# }

#  ####################################################
#  # contribution
#  contrib <- function(data, l=4, var.num, var.den)
#  {
#  T    <- NROW(data)
#  id1  <- seq(1,l); id2 <- seq(T-(l-1),T)
#  tmp1 <- data[-id1,]; tmp2 <- data[-id2,]
#
#  den <- tmp1[,var.den]
#  num <- tmp1[,var.num] - tmp2[,var.num]
#
#  contrib  <- c( rep(NA, l), 100*num/den )
#  return(contrib)
#  }

####################################################

####################################################
gdp.ggplot.subsectors <- function(data, type)
{
# function to ggplot Growth by SECTORS

# TODO: ERROR CATCHING

vars <- c("AGR", "IND.EXT", "IND.TRANS", "ELEC.ETC", "CONST", "COMM", "TRANS", "INFO.COM", "FIN", "REAL.ESTATE", "SERV.ETC", "ADM", "GDP")
# varnames <- c("Agropecuária", "Ind. Extrativa", "Ind. de transformação", "Eletricidades e gás, esgoto, ativ. de gestão de resíduos", "Construção", "Comércio", "Transporte, armazenagem e correio", "Informação e Comunicação", "Ativ. financeiras de seguros e serviços realacionados", "Ativ. Imobiliárias", "Outras atividades de serviços", "Adm., defesa, saúde e educação públicas e seguridade social", "PIB")
varnames <- c("Agropecuária", "Ind. Extrativa", "Ind. de transformação",
			  "Eletricidades e gás, esgoto, \n ativ. de gestão de resíduos",
			  "Construção", "Comércio", "Transporte, armazenagem e correio", "Informação e Comunicação",
			  "Ativ. financeiras de seguros \n e serviços realacionados",
			  "Ativ. Imobiliárias", "Outras atividades de serviços",
			  "Adm., defesa, saúde e educação \n públicas e seguridade social",
			  "PIB")

newdata <- tail( data[, vars], 1); colnames(newdata) <- varnames
newdata <- melt( sort(newdata, decreasing=T) )

# plot
ggplot(newdata, aes( x = variable, y = value, fill = value > 0 ) ) +
geom_bar( stat="identity" ) +
scale_fill_manual(values=c("red", "blue") ) +
guides(fill=FALSE) +
geom_hline(yintercept=0 ) +
geom_text( aes(label=value, vjust=ifelse(value > 0, "bottom", "top") ) )+
theme(axis.text.x = element_text(color="black", size=10, angle=90, hjust=1, vjust=.33),
  	  plot.title = element_text(face = "bold", hjust=.5),
  	  plot.subtitle = element_text(hjust = .5),
  	  plot.caption = element_text(size=11,hjust = 0) ) +
xlab("") + ylab("") +
labs(title="GDP by Subsectors",
	 subtitle=type,
	 caption="Data Source: IBGE.")
}

####################################################
gdp.ggplot.prod.growth <- function(data, type, ndates)
{
# function to ggplot Growth by SECTORS

# TODO: ERROR CATCHING

vars <- c("date", "GDP", "AGR", "IND", "SER")

newdata      <- tail(data[, vars], ndates)
newdata$date <- date2qtr( newdata$date )
newdata      <- melt( newdata, id="date" )

#plot
ggplot(newdata, aes( x = date, y = value, group=variable, color=variable) ) +
geom_line( size=1 ) +
geom_hline(yintercept=0 ) +
theme(legend.position = "top",
	  legend.title = element_blank(),
	  legend.text  = element_text(size = 11, face = "bold"),
	  axis.text.x = element_text(color = "black", size = 11),
  	  plot.title = element_text(face = "bold", hjust = .5),
  	  plot.subtitle = element_text(hjust = .5),
  	  plot.caption = element_text(size=11, hjust = 0) ) +
xlab("") + ylab("") +
labs(title="GDP Growth by Production Variables",
	 subtitle=type,
	 caption="Data Source: IBGE.")
}

####################################################
gdp.ggplot.demand.growth <- function(data, ndates,type)
{
# function to ggplot Growth by SECTORS

# TODO: ERROR CATCHING
vars <- c("date", "GDP", "C", "G", "FBKF", "X", "M")

newdata      <- tail(data[, vars], ndates)
newdata$date <- date2qtr( newdata$date )
newdata      <- melt( newdata, id="date" )

#plot
ggplot(newdata, aes( x = date, y = value, group=variable, color=variable) ) +
geom_line( size=1 ) +
geom_hline(yintercept=0 ) +
theme(legend.position = "top",
	  legend.title = element_blank(),
	  legend.text  = element_text(size = 11, face = "bold"),
	  axis.text.x = element_text(color = "black", size = 11),
  	  plot.title = element_text(face = "bold", hjust = .5),
  	  plot.subtitle = element_text(hjust = .5),
  	  plot.caption = element_text(size=11,hjust = 0) ) +
xlab("") + ylab("") +
labs(title="GDP Growth by Demand Variables",
	 subtitle=type,
	 caption="Data Source: IBGE.")
}
