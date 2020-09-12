################################################
# Download ipca long series
ipca.dl.csv <- function(folderpath="./")
{

# urls
url1 <- "https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela1737.csv&terr=N&rank=-&query=t/1737/n1/all/v/all/p/all/d/v63%202,v69%202,v2263%202,v2264%202,v2265%202,v2266%2013/l/,t%2Bv,p"

# Download Data CSV
download.file(url1, paste0( folderpath, "IPCA-SIDRA-1737.csv"))

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

####################################################
ipca.dates <- function(dates)
{
# IPCA DATES (%B %Y) to %Y:%m
# format(Sys.Date(), "%B/%Y")
# format(Sys.Date(), "%Y:%m")

# dates
m1 <- c("janeiro", "fevereiro", "março", "abril", "maio", "junho", "julho", "agosto", "setembro", "outubro", "novembro", "dezembro")
m2 <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

for(i in seq_along(m1)) dates <- gsub(m1[i], m2[i], dates )

# month    <- as.numeric(substring(dates, 1,2))
# year     <- as.numeric(substring(dates, 4,7))
month    <- substring(dates, 1,2)
year     <- substring(dates, 4,7)
newdates <- as.Date(paste(year, month, "15", sep="-"))

dates.df <- data.frame(newdates, year, month)

return(dates.df)
}

####################################################
ipca.csv2df <- function(data)
{
dates   <- ipca.dates(data[,1])
newdata <- data.frame( dates, sapply( data[, -1], as.numeric ) )
colnames(newdata) <- c("date", "year", "month", "index", "ret1", "retac3", "retac6", "retacyr", "retac12")

# preallocate meta, banda, lb, ub
newdata$meta <- NA; newdata$banda <- NA; newdata$lb <- NA; newdata$ub <- NA

# load meta.mat
meta <-readRDS("../data/ipca-meta.rds")

# bind meta.mat to ipca.df
years <- seq(1999, tail(newdata$year,1))
for(i in seq_along(years)) newdata[which(newdata$year==meta$year[i]), c("meta", "banda", "lb", "ub")] <- meta[i,-1]

return(newdata)
}

####################################################
ipca.save.rds <- function(folderpath="./")
{
data    <- load.trim( paste0(folderpath, "IPCA-SIDRA-1737.csv"), 4, 11)
newdata <- ipca.csv2df(data)
saveRDS(newdata, paste0(folderpath, "ipca.rds"))
return(1)
}
