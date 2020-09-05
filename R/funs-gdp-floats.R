# GDP FIGS

####################################################
gdp.ggplot.growth <- function(data, year)
{
idx <- (data$year >= year)
newdata                <- data[idx, -c(2,3)]
newdata$date           <- date2qtr( newdata$date )
colnames(newdata)[2:5] <- c("T/T-4", "T/T-1 with Seasonal Adjustment", "T/T-4 AC4Q", "AC. Year")
d.ends                 <- melt(tail(newdata, 1), id="date")
d.ends
newdata                <- melt( newdata, id="date" )

#plot
ggplot(newdata, aes( x = date, y = value, group=variable, color=variable) ) +
theme_bw() +
geom_line( size=1 ) +
geom_hline(yintercept=0 ) +
# geom_label(label=d.ends$value, x=d.ends$date, y=d.ends$value , hjust=1 ) +
# scale_y_continuous(sec.axis = sec_axis(data$variable ~. , breaks = d.ends$value)) +
facet_wrap( variable ~ ., ncol=1) +
theme(legend.position = "none",
	  axis.text.x = element_text(color = "black", size = 9, angle=90, vjust=.5),
	  panel.grid.major.x = element_blank(),
  	  plot.title = element_text(face = "bold", hjust = .5),
  	  plot.caption = element_text(size=11, hjust = 0) ) +
xlab("") + ylab("") +
labs(title="GDP Growth (%)",
	 caption="Data Source: IBGE.")
}

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
theme_bw() +
geom_bar( stat="identity" ) +
scale_fill_manual(values=c("red", "blue") ) +
# scale_x_discrete(breaks = NULL) +
guides(fill=FALSE) +
geom_hline(yintercept=0 ) +
geom_text( aes(label=value, vjust=ifelse(value > 0, "bottom", "top") ) )+
theme(axis.text.x = element_text(color="black", size=10, angle=90, hjust=1, vjust=.33),
	  panel.grid.major.x = element_blank(),
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

#######################################################
# PLOT GDP AC 4Q

fig.gdp.ac4 <- function(data)
{
dygraph(data, main = "Index GDP in Reais of 1995 AC 4Q 2018:Q4=100") %>%
    dySeries(label = "AC t/t-4") %>%
    dyEvent("2002-01-1", "Lula",  labelLoc = "bottom") %>%
    dyEvent("2008-10-1", "Crise", labelLoc = "bottom") %>%
    dyEvent("2010-01-1", "Dilma", labelLoc = "bottom") %>%
    dyEvent("2016-09-1", "Temer", labelLoc = "bottom") %>%
    dyEvent("2019-01-1", "Bolsonaro", labelLoc = "bottom") %>%
    dyBarChart() %>%
    dyOptions(includeZero = TRUE)
}

#######################################################
# PLOT GDP INDEX SA

fig.gdp.t1 <- function(gdp, ret, date)
{
tmp <- normalize(gdp, date)
tmp <- cbind(tmp, ret); colnames(tmp) <- c("gdp","t/t-1")

dygraph(tmp,
        main = paste0("Index GDP in Reais of 1995 SA ", date, "=100") ) %>%
    dyLimit(0,   color = "black") %>%
    dyLimit(100, color = "black") %>%
    dyEvent("2002-01-1", "Lula",  labelLoc = "bottom") %>%
    dyEvent("2008-10-1", "Crise", labelLoc = "bottom") %>%
    dyEvent("2010-01-1", "Dilma", labelLoc = "bottom") %>%
    dyEvent("2016-09-1", "Temer", labelLoc = "bottom") %>%
    dyEvent("2019-01-1", "Bolsonaro", labelLoc = "bottom") %>%
    dyBarSeries("t/t-1") %>%
    dyOptions(includeZero = TRUE)
}

#######################################################
# PLOT GDP AC 4Q

fig.gdp.t4 <- function(gdp, ret, date)
{
tmp <- normalize(gdp, date)
tmp <- cbind(tmp, ret); colnames(tmp) <- c("gdp","t/t-4")

dygraph(tmp,
        main = paste0("Index GDP in Reais of 1995 AC 4Q ", date, "=100") ) %>%
    dyLimit(0,   color = "black") %>%
    dyLimit(100, color = "black") %>%
    dyEvent("2002-01-1", "Lula",  labelLoc = "bottom") %>%
    dyEvent("2008-10-1", "Crise", labelLoc = "bottom") %>%
    dyEvent("2010-01-1", "Dilma", labelLoc = "bottom") %>%
    dyEvent("2016-09-1", "Temer", labelLoc = "bottom") %>%
    dyEvent("2019-01-1", "Bolsonaro", labelLoc = "bottom") %>%
    dyBarSeries("t/t-4") %>%
    dyOptions(includeZero = TRUE)
}

#######################################################

####################################################
####################################################
# gdp table function
gdp.tab.1 <- function(data, caption=NULL)
{
# TODO: ERROR CATCHING

newdata <- cbind("GDP"=data$GDP, "Agriculture"=data$AGR, "Industry"=data$IND, "Services"=data$SER,
						  "Household Consumption"=data$C, "Government Expenditure"=data$G, "Fixed Investment"=data$FBKF, "Exports"=data$X, "Imports"=data$M )
rownames(newdata) <- date2qtr(data$date)

mytab <- kbl( t(tail(newdata, 5)), caption=caption ) %>%
	kable_classic("hover", full_width = T, font_size=12) %>%
    row_spec(1, bold =T, background = "lightblue") %>%
	column_spec(6, bold = T) %>%
	pack_rows("Production", 2, 4) %>%
	pack_rows("Demand", 5, 9) %>%
	footnote(general = "IBGE", general_title = "Data Source: ",
			 footnote_as_chunk = T)

return(mytab)
}

####################################################
# gdp table function
gdp.tab.2 <- function(data, caption=NULL)
{
# TODO: ERROR CATCHING

vars     <- c( "AGR", "IND", "SER", "VAB", "TAX", "GDP", "C", "G", "FBKF", "X", "M", "DE" )
varnames <- c("Agropecuária", "Indústria", "Serviços", "VAB a Preços Básicos", "Impostos sobre Produtos", "PIB a Preços de Mercado", "Despesa de Consumo das Famílias", "Despesa de Consumo do Governo", "Formação Bruta de Capital Fixo", "Exportações de Bens e Serviços", "Importações de Bens e Serviços (-)", "Variação de Estoque")

newdata <- data[,vars]; colnames(newdata) <- varnames; rownames(newdata) <- date2qtr(data$date)
idx     <- (data$year == 2019); tmp <- apply(data[idx, vars], 2, sum)
newdata <- cbind( t(tail(newdata, 5)), "2019"=tmp)[,c(1,2,3,6,4,5)]


mytab <- kable( newdata , caption=caption, digits=0, format.args=list(decimal.mark=",", big.mark=".") ) %>%
	kable_classic("hover", full_width = T, font_size=12) %>%
    row_spec(4, bold =T) %>%
    row_spec(6, bold =T, background = "lightblue") %>%
	# column_spec(6, bold = T) %>%
	footnote(general = "IBGE",
			 general_title = "Data Source: ",
			 footnote_as_chunk = T)
return(mytab)
}

####################################################
# gdp vab share table function
gdp.tab.vab.share <- function(data, caption=NULL)
{
# TODO: ERROR CATCHING

varnames <- c("Agropecuária", "Indústria", "Extrativa Mineral", "Transformação", "Prod. e distrib. de eletricidade, gás, água e esgoto", "Construção", "Serviços", "Comércio", "Transporte, armazenagem e correio", "Serviços de informação", "Intermed. financeira, seguros, prev. complem. e serv. rel.", "Atividades Imobiliárias", "Outros serviços", "Adm., saúde e educação públicas", "VAB a Preços Básicos", "Impostos sobre Produtos", "PIB a Preços de Mercado")

varsl   <- colnames(data) %in% c("C", "G", "FBKF", "X", "M", "DE")
idx     <- (data$qtr == 4 & (data$year==2000 | data$year==2005 | data$year>=2010) );
newdata <- data[idx, !varsl]; rownames(newdata) <- newdata$year
newdata <- t(newdata[, -c(1,2,3)]); rownames(newdata) <- varnames

mytab <- kable( newdata , caption=caption, digits=1, format.args=list(decimal.mark=",", big.mark=".") ) %>%
	kable_classic("hover", full_width = T, font_size=12) %>%
    row_spec(1, bold =T, background = "lightblue") %>%
    row_spec(2, bold =T, background = "lightblue") %>%
    row_spec(7, bold =T, background = "lightblue") %>%
    row_spec(15, bold =T, background = "lightblue") %>%
    row_spec(17, bold =T, background = "lightblue") %>%
	footnote(general = "IBGE",
			 general_title = "Data Source: ",
			 footnote_as_chunk = T)
return(mytab)
}

####################################################
# gdp vab share table function
gdp.tab.demand.share <- function(data, caption=NULL)
{
# TODO: ERROR CATCHING

varnames  <- c("Despesa de Consumo das Famílias", "Despesa de Consumo do Governo", "FBCF + Variação de Estoque", "Exportações de Bens e Serviços", "Importações de Bens e Serviços (-)", "PIB a Preços de Mercado")
varsl     <- colnames(data) %in% c("date", "year", "qtr", "C", "G", "FBKF", "DE", "X", "M", "GDP")
idx       <- (data$qtr == 4 & (data$year==2000 | data$year==2005 | data$year>=2010) );
newdata   <- data[idx, varsl]; rownames(newdata) <- newdata$year
newdata$I <- newdata$FBKF + newdata$DE
newdata   <- t( newdata[,c(5,6,11,9,10,4)] ); rownames(newdata) <- varnames

mytab <- kable( newdata , caption=caption, digits=1, format.args=list(decimal.mark=",", big.mark=".") ) %>%
	kable_classic("hover", full_width = T, font_size=12) %>%
	footnote(general = "IBGE",
			 general_title = "Data Source: ",
			 footnote_as_chunk = T)
return(mytab)
}

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
