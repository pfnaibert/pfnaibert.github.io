import_gdp <- function(filename)
{

# import csv
data  <- read.csv(filename)

# delete last 2 lines
data  <- head(data, -2)

# save dates
dates <- data[,1]

# delete first row
data  <- data[,-1]

# rearrange strings in dates
dates <- gsub(" T", ":Q", dates)

# give names to values
names(data) <- dates

# `ts` object
data.ts <- ts(data, start=c(1980, 1), end=c(2014, 3), freq=4 )

return(data.ts)
}

qtr2date <- function(data)
{
data <- gsub(":Q1", "-03-31", data)
data <- gsub(":Q2", "-06-30", data)
data <- gsub(":Q3", "-09-30", data)
data <- gsub(":Q4", "-12-31", data)
return(data)
}


date2num <- function(data) return( as.numeric(substring(data, 1, 4)) + as.numeric( substring(data, 7) )/4 )
