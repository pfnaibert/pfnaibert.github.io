################################################
# Download ipca long series
download.ipca <- function()
{

# urls
url1 <- "https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela1737.csv&terr=N&rank=-&query=t/1737/n1/all/v/2266/p/all/d/v2266%2013/l/v%2Bt,,p"

# Download Data CSV
download.file(url1, "../data/IPCA-SIDRA-1737.csv")

# Load Data into R
ipca  <- load.trim("../data/IPCA-SIDRA-1737.csv", 4, 11)
ipca1 <- date.month.SIDRA(ipca)

# SAVE in RDS
saveRDS(ipca1,  "../data/ipca.rds" )

return(1)
}

################################################
# Download industry long series
download.pimpfbr <- function()
{

#
filename <- "../data/PIMPFBR-SIDRA-3653.csv"

# urls
url1 <- "https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela3653.csv&terr=N&rank=-&query=t/3653/n1/all/v/3134,3135,3138/p/all/c544/129314/d/v3134%201,v3135%201,v3138%201/l/t%2Bc544,v,p"

# Download Data CSV
download.file(url1, filename)

# Load Data into R
data <- load.trim(filename,nhead=5,ntail=9)

ind1  <- fun.month.SIDRA(data[,c(1,2)])
ind2  <- fun.month.SIDRA(data[,c(1,3)])
ind3  <- fun.month.SIDRA(data[-seq(1,23),c(1,4)]) - 100

ind   <- list(ind1, ind2, ind3); names(ind) <- c("NSA", "SA", "ac12")

# SAVE in RDS
saveRDS(ind,  "../data/ind.rds" )

return(1)
}

################################################
# Download commerce long series
download.pmc <- function()
{

#
filename <- "../data/PMC-SIDRA-3416.csv"

# urls
url1 <- "https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela3416.csv&terr=N&rank=-&query=t/3416/n1/all/v/564/p/all/c11046/40311,40312/d/v564%201/l/v%2Bt,c11046,p"

# Download Data CSV
download.file(url1, filename)

# Load Data into R
data <- load.trim(filename,nhead=5,ntail=9)

data1  <- fun.month.SIDRA(data[,c(1,2)])
data2  <- fun.month.SIDRA(data[,c(1,3)])

newdata <- list(data1, data2); names(newdata) <- c("NSA", "SA")

# SAVE in RDS
saveRDS(newdata,  "../data/pmc.rds" )

return(1)
}

################################################
# Download services long series
download.pms <- function()
{

#
filename <- "../data/PMS-SIDRA-6442.csv"

# urls
url1 <- "https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela6442.csv&terr=N&rank=-&query=t/6442/n1/all/v/8677/p/all/c11046/40311,40312/d/v8677%201/l/t%2Bv,c11046,p"

# Download Data CSV
download.file(url1, filename)

# Load Data into R
data <- load.trim(filename,nhead=5,ntail=9)

data1  <- fun.month.SIDRA(data[,c(1,2)])
data2  <- fun.month.SIDRA(data[,c(1,3)])

newdata <- list(data1, data2); names(newdata) <- c("NSA", "SA")

# SAVE in RDS
saveRDS(newdata,  "../data/pms.rds" )

return(1)
}

################################################
# Download IBC long series
download.ibc <- function()
{

#
file1 <- "../data/IBC-SGS-24363.csv"
file2 <- "../data/IBC-SGS-24364.csv"

# urs
url1 <- "http://api.bcb.gov.br/dados/serie/bcdata.sgs.24363/dados?formato=csv"
url2 <- "http://api.bcb.gov.br/dados/serie/bcdata.sgs.24364/dados?formato=csv"

# Download Data CSV
download.file(url1, file1)  # NSA
download.file(url2, file2)  # SA

# Load Data into R
x1 <- read.csv(file1, stringsAsFactors = F,skip=1,header=F, sep=";", dec=",")
x2 <- read.csv(file2, stringsAsFactors = F,skip=1,header=F, sep=";", dec=",")
y  <- list(x1, x2)

# Adjust names and dates
x1 <- fun.month.sgs(x1)
x2 <- fun.month.sgs(x2)
y  <- list(x1, x2)
names(y) <- c("NSA", "SA")

# SAVE in RDS
saveRDS(y,  "../data/IBC.rds" )

return(1)
}

################################################
# Download SELIC
download.selic <- function()
{

#
file1 <- "../data/SELIC-SGS-11.csv"
file2 <- "../data/SELIC-SGS-1178.csv"
file3 <- "../data/SELIC-SGS-432.csv"
file4 <- "../data/SELIC-SGS-4189.csv"

# urls
url1 <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.11/dados?formato=csv"   # selic diaria
url2 <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.1178/dados?formato=csv" # selic.an diaria
url3 <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.432/dados?formato=csv"  # selic.meta diaria
url4 <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.4189/dados?formato=csv" # selic.an ac mes

# Download Data CSV
download.file(url1, file1)  # selic
download.file(url2, file2)  # selic.an
download.file(url3, file3)  # selic.meta
download.file(url4, file4)  # selic.meta

return(1)
}

####################################################
# DATES

####################################################
# quarter
date.quarter <- function(data)
{
T <- NROW(data); dates <- names(data)

# dates as numeric for TS
Y <- as.numeric(substring(dates, 1,4))
Q <- as.numeric(substring(dates, 7,7))

# Time Series
newdata <- ts(data, start = c(Y[1], Q[1]), end = c(Y[T], Q[T]), frequency = 4)
return(newdata)
}

####################################################
# month
date.month <- function(data)
{
T <- NROW(data); dates <- names(data)
Y <- as.numeric(substring(dates, 1,4))
M <- as.numeric(substring(dates, 6,7))
newdata <- ts(data, start = c(Y[1], Q[1]), end = c(Y[T], Q[T]), frequency = 12)
return(newdata)
}

####################################################
date.month.SIDRA <- function(data)
{
# IPCA DATES (%B %Y) to %Y:%m
# format(Sys.Date(), "%B/%Y")
# format(Sys.Date(), "%Y:%m")

# numeric values
data2 <- as.numeric(as.character(data[,-1]))
K     <- NROW(data)

# dates
dates <- data[,1]
m1 <- c("janeiro", "fevereiro", "março", "abril", "maio", "junho", "julho", "agosto", "setembro", "outubro", "novembro", "dezembro")
m2 <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

for(i in 1:12)
{
dates <- gsub(m1[i], m2[i], dates )
}

M <- substring(dates, 1,2); M1 <- as.numeric(M)
Y <- substring(dates, 4,7); Y1 <- as.numeric(Y)
names(data2) <- paste0(Y, ":", M)

# newdata
newdata <- ts(data2, start=c(Y1[1], M1[1]), end=c(Y1[K], M1[K]), frequency=12 )

return(newdata)
}

####################################################
date.month.sgs <- function(data)
{
# SGS DATES ("%d/%m/%Y") to  "%Y:%m"
# format(Sys.Date(), "%d/%m/%Y")
# format(Sys.Date(), "%Y:%m")

data1 <- data[,1]
data2 <- as.numeric(data[,2]); T <- NROW(data2)

M <- substring(data1, 4,5); M1 <- as.numeric(M)
Y <- substring(data1, 7,10); Y1 <- as.numeric(Y)
names(data2) <- paste0(Y, ":", M)

newdata <- ts(data2, start=c(Y1[1], M1[1]), end=c(Y1[T], M1[T]), frequency=12 )

return(newdata)
}

####################################################
# SUMS and Returns


####################################################
# ac4Q # QUARTER
sum4 <- function(data)
{
T <- NROW(data); dates <- names(data)
tmp <- rep(NA, (T-3)); names(tmp) <- dates[-seq(1:3)]

for(i in 1:(T-3))
{
    w1     <- seq(i,i+4-1)
    tmp[i] <- sum(as.numeric(data[w1]))
}

return(date.quarter(tmp))
}

####################################################
# ret (t/t-4) # QUARTER
ret4 <- function(data)
{
T    <- NROW(data); dates <- names(data)

id1  <- seq(1,4); id2 <- seq(T-3,T)
tmp1 <- as.numeric(data[-id1]); tmp2 <- as.numeric(data[-id2])
ret  <- 100*(tmp1/tmp2 - 1)
names(ret) <- dates[-id1]

return(date.quarter(ret))
}

####################################################
# y(t) - y(t-4) # QUARTER
dif4 <- function(data)
{
T    <- NROW(data); dates <- names(data)

id1  <- seq(1,4); id2 <- seq(T-3,T)
tmp1 <- data[-id1]; tmp2 <- data[-id2]
dif  <- as.numeric(tmp1) - as.numeric(tmp2)
names(dif) <- dates[-id1]

return(date.quarter(dif))
# return(list("values"=ret, "dates"=dates[-id1]))
}

####################################################
ret1.q <- function(data)
{
T <- NROW(data); dates <- names(data)
tmp1 <- as.numeric(data[-1]); tmp2 <- as.numeric(data[-T])
ret  <- 100*(tmp1/tmp2 - 1)
names(ret) <- dates[-1]

return(date.quarter(ret))
}

####################################################
# sum 12 meses # MONTH
sum12 <- function(data)
{
T     <- NROW(data); dates <- names(data)
sum.12 <- rep(NA, T-11)

for(i in 1:(T-11))
{
    w1       <- seq(i,i+12-1)
    sum.12[i] <- sum(data[w1])/12
}
names(sum.12) <- dates[-c(1:11)]

return(date.month(sum.12) )
}

####################################################
# ret t/t-12 # MONTH
ret12 <- function(data)
{
T    <- NROW(data); dates <- names(data)

id1  <- seq(1:12); id2  <- seq(T-11, T)
tmp1 <- data[-id1]; tmp2 <- data[-id2]
ret  <- 100*(tmp1/tmp2 - 1)
names(ret) <- dates[-id1]

return(date.month(ret) )
}

####################################################
# dif y(t) - y(t-12) # MONTH
dif12 <- function(data)
{
T    <- NROW(data); dates <- names(data)

id1  <- seq(1:12); id2  <- seq(T-11, T)
tmp1 <- data[-id1]; tmp2 <- data[-id2]
dif  <- tmp1 - tmp2
names(dif) <- dates[-id1]

return(date.month(dif) )
}

####################################################
# ret t/t-1 # MONTH
ret1.m <- function(data)
{
T <- NROW(data); dates <- names(data)

tmp1 <- data[-1]; tmp2 <- data[-T]
ret  <- 100*(tmp1/tmp2 - 1)
names(ret) <- dates[-1]

return(date.month(ret))
}

####################################################
# ret anualizado (ret)^per
ret.an <- function(ret, per)
{
ret.an <- ((1+ret/100)^per - 1)*100
return(ret.an)
}

#######################################################
# AC ANO
ac.yr <- function(index, year)
{
id <- grep(year, names(index) )
return(100*(index[tail(id,1)]/index[(head(id,1)-1)] -1 ) )
}

####################################################
normalize <- function(data, date)
{
T <- NROW(data)
den <- rep(data[date], T)
newdata <- 100*data/den
return(newdata)
}

####################################################
standard <- function(data)
{
return((data-mean(data))/sd(data))
}

####################################################
#
fun.subserie <- function(data, pat)
{
id      <- grep(pat, names(data))
newdata <- data[id]
return(newdata)
}

####################################################
coef.lin <- function(data)
{
# data is NOT in LOG
T <- NROW(data); t1 <- seq(1, T)

# But the regression IS in LOG
reg <- lm(log(data)~t1)
return(coef(reg))
}

####################################################
trend.lin <- function(data)
{
# data is NOT in LOG
T <- NROW(data); t1 <- seq(1, T);

# But the regression IS in LOG
reg   <- lm(log(data)~t1)    # log linear

# answers are NOT in LOG
t1 <- exp(date.quarter(reg$fitted.values))
c1 <- data-t1
c2 <- c1/t1

# Growth Gap
g  <- ret1.q(t1)
gg <- ret1.q(data) - g

return(list("fit"=t1, "dif"=c1, "pct"=c2, "g"=g, "gg"=gg))
}

####################################################
trend.quad <- function(data)
{

# data is NOT in LOG
T <- NROW(data); t1 <- seq(1, T); t2 <- t1^2

# But the regression IS in LOG
reg <- lm(log(data)~t1+t2) # quad

# answers are NOT in LOG
t1 <- exp(date.quarter(reg$fitted.values))
c1 <- data-t1
c2 <- c1/t1

# Growth Gap
g  <- ret1.q(t1)
gg <- ret1.q(data) - g

return(list("fit"=t1, "dif"=c1, "pct"=c2, "g"=g, "gg"=gg))
}

#############################################
# HF # Hamilton Filter
trend.hf <- function(data, h=8, p=4)

{
# data is NOT in LOG
T <- NROW(data); y <- 100*log(data)

# But the regression IS in LOG
y.00 <- y[(1+h+p-1):(T-0)] # y(t+h) or y(t)

x <- matrix(NA, nrow = length(y.00), ncol = p)
for(i in 1:p)
{
x[,i] <- y[i:(T-h-(p-i))]  # y(t) or y(t-h-(p-i)) for i=1, ..., p
}

reg <- lm(y.00 ~ x)

# answers are NOT in LOG
t1 <- date.quarter(reg$fitted.values)
c1 <- date.quarter(reg$residuals)

#
u <- date.quarter(y.00 - x[,p]) # y(t) - y(t-h)

# Growth Gap
g  <- ret1.q(exp(t1/100))
gg <- ret1.q(data) - g

return(list("fit"=t1, "c1"=c1, "u"=u, "g"=g, "gg"=gg))
}

####################################################
trend.hpf <- function(data, lambda)
{
require(mFilter)

# data is NOT in LOG
T <- NROW(data); t1 <- seq(1, T); t2 <- t1^2

# But the filter IS in LOG
hpf <- hpfilter(log(data), freq = lambda)
names(hpf$trend) <- names(data)

# answers are NOT in LOG
t1 <- exp(hpf$trend)
c1 <- data-t1
c2 <- c1/t1

# Growth Gap
g  <- ret1.q(t1)
gg <- ret1.q(data) - g

return(list("fit"=t1, "dif"=c1, "pct"=c2, "g"=g, "gg"=gg))
}
####################################################
