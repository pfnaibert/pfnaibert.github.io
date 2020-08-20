# https://www.neonscience.org/dc-time-series-plot-ggplot-r
# https://www.tutorialspoint.com/ggplot2/ggplot2_time_series.htm
# http://www.cookbook-r.com/Graphs/Titles_(ggplot2)/
# https://plotly.com/ggplot2/geom_rect/

###########
# center title
# https://stackoverflow.com/questions/40675778/center-plot-title-in-ggplot2

###########
# import funs
source("funs.R")

# libraries
library(ggplot2)
library(ggthemes)

# import data
y     <- import_gdp("../../_resources/data/PIB_BASE_2000.csv")
recs  <- readRDS("../../_resources/data/recessions.rds")
nrecs <- length(recs)/2
recs.mat <- as.data.frame( t( matrix( date2num(recs), nrow=2 ) ) ); colnames(recs.mat) <- c("start", "end")

# transform dates
dates <- date2num(names(y))

y <- as.data.frame(cbind(dates, "GDP"=y) )
class(y)
head(y)

###########
# quick plot
qplot(x=dates, y=GDP, data=y,
	  geom="line", size=I(1), color = I("blue"),
	  xlab="", ylab="",
	  main = "Cronologia" )

###########
# basic
ggplot(data = y, aes( x = dates, y = GDP ) ) +
	theme( plot.title = element_text(hjust = 0.5, face = "bold" ) ) +
	ggtitle( "Cronologia" ) +
	xlab("") + ylab("") +
   	geom_line( color = "blue", size=1 )

###########
# theme economist
ggplot(data = y, aes( x = dates, y = GDP ) ) +
	theme_economist() +
	theme(plot.title = element_text(hjust = 0.5)) +
	ggtitle( "Cronologia" ) +
	xlab("") + ylab("") +
	geom_line( color = "blue", size=1 )

###########
# theme bw
ggplot(data = y, aes( x = dates, y = GDP ) ) +
	theme_bw() +
	theme(plot.title = element_text(hjust = 0.5)) +
	ggtitle( "Cronologia" ) +
	xlab("") + ylab("") +
	geom_line( color = "blue", size=1 )

###########
# abline
ggplot(data = y, aes( x = dates, y = GDP ) ) +
	theme_bw() +
	theme( plot.title = element_text(hjust = 0.5, face = "bold" ) ) +
	ggtitle( "Cronologia" ) +
	xlab("") + ylab("") +
	geom_line( color = "blue", size=1 ) +
	geom_vline( color = "red", linetype="dotted", size=.5, aes( xintercept=1990 ) ) +
	geom_hline( color = "red", linetype="dashed", size=.5, aes( yintercept=100 ) )

###########
# rect
ggplot(data = y, aes( x = dates, y = GDP ) ) +
	theme_bw() +
	theme( plot.title = element_text(hjust = 0.5, face = "bold" ) ) +
	ggtitle( "Cronologia" ) +
	xlab("") + ylab("") +
	geom_rect( data=recs.mat, aes( NULL, NULL, xmin = start, xmax = end ), ymin = 0, ymax = 200 ) +
   	geom_line( color = "blue", size=1 )

###########
# save plots
gdp_plot  <-
ggplot(data = y, aes( x = dates, y = GDP ) ) +
	ggtitle( "Cronologia" ) +
	xlab("") + ylab("") +
	geom_line( color = "blue", size=1 )

###########
gdp_plot
gdp_plot + theme_economist()
gdp_plot + theme_bw()

###########
sessionInfo()
