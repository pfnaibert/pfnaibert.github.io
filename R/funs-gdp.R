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

#	####################################################
#	gdp.dl.save <- function()
#	{
#	gdp.dl()
#	gdp.save.df()
#	gdp.transform()
#	return(1)
#	}
