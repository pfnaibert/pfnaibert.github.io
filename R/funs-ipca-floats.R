####################################################
# GDP FIGS
####################################################

####################################################
plot.ipca.index <- function(data, year, month, title, subtitle, data.source="IBGE.")
{
idx     <- which(data$year>=year)
newdata <- melt(data[idx,c(1,4)], id="date")

idx2 <- which(data$year >= year & data$month==month); tmp <- data$date[idx2]

#plot
ggplot(newdata, aes( x = date, y = value ) ) +
theme_bw() +
geom_line(size=1, color="blue" ) +
scale_x_continuous(labels=date2month(tmp), breaks=tmp) +
theme(legend.position = "none",
	  legend.title=element_blank(),
	  axis.text.x = element_text(color = "black", size = 9, angle=90, vjust=.5),
	  # panel.grid.major.x = element_blank(),
	  panel.grid.minor.x = element_blank(),
  	  plot.title = element_text(face = "bold", hjust = .5),
  	  plot.subtitle = element_text(hjust = .5),
  	  plot.caption = element_text(size=11, hjust = 0) ) +
xlab("") + ylab("") +
labs(title=title,
	 subtitle=subtitle,
	 caption=paste("Data Source:", data.source ) )
}

####################################################
plot.ipca.ac12 <- function(data, year, month, title, subtitle, data.source="IBGE.")
{

idx  <- which(data$year>=year); newdata <- data[idx,c("date", "retac12", "meta", "lb", "ub" )]
idx2 <- which(data$year >= year & data$month==month); tmp <- data$date[idx2]

# http://sape.inf.usi.ch/quick-reference/ggplot2/linetype
# https://www.datanovia.com/en/blog/how-to-create-a-ggplot-with-multiple-lines/
# cond.line <- ifelse(newdata$variable=="retac12", "solid", "dashed")
# cond.size <- ifelse(newdata$variable=="retac12", 1, .5)
# cond.col  <- ifelse(newdata$variable=="retac12", "blue", "black")

#plot
ggplot(newdata, aes(x = date) ) +
theme_bw() +
# geom_line(aes( y = value, group=variable),
# 		  size=cond.size,
# 		  linetype=cond.line,
# 		  color=cond.col ) +
geom_line(aes( y = retac12), color="blue", size=1) +
geom_line(aes( y = meta), color="black", size=.75, linetype="solid") +
geom_line(aes( y = lb), color="black", size=.5, linetype="longdash") +
geom_line(aes( y = ub), color="black", size=.5, linetype="longdash") +
geom_hline(yintercept=0) +
# geom_text( aes(x=tail(date,1), y=tail(value,1), label=tail(value,1), vjust=.5, hjust=0 ) ) +
scale_x_continuous(labels=date2month(tmp), breaks=tmp) +
theme(legend.position = "none",
	  legend.title=element_blank(),
	  axis.text.x = element_text(color = "black", size = 9, angle=90, vjust=.5),
	  # panel.grid.major.x = element_blank(),
	  panel.grid.minor = element_blank(),
  	  plot.title = element_text(face = "bold", hjust = .5),
  	  plot.subtitle = element_text(hjust = .5),
  	  plot.caption = element_text(size=11, hjust = 0) ) +
xlab("") + ylab("") +
labs(title=title,
	 subtitle=subtitle,
	 caption=paste("Data Source:", data.source ) )
}


####################################################
plot.ipca.ret <- function(data, var, year, month, title, subtitle, data.source="IBGE.")
{

id.var  <- which(colnames(data)==var); idx <- which(data$year>=year)
newdata <- melt(data[idx,c(1, id.var)], id="date")

idx2 <- which(data$year >= year & data$month==month); tmp <- data$date[idx2]

#plot
ggplot(newdata, aes( x = date, y = value) ) +
theme_bw() +
geom_line(size=1, color="blue") +
geom_hline(yintercept=0) +
geom_text( aes(x=tail(date,1), y=tail(value,1), label=tail(value,1), vjust=.5, hjust=0 ) ) +
scale_x_continuous(labels=date2month(tmp), breaks=tmp) +
theme(legend.position = "none",
	  legend.title=element_blank(),
	  axis.text.x = element_text(color = "black", size = 9, angle=90, vjust=.5),
	  # panel.grid.major.x = element_blank(),
	  panel.grid.minor = element_blank(),
  	  plot.title = element_text(face = "bold", hjust = .5),
  	  plot.subtitle = element_text(hjust = .5),
  	  plot.caption = element_text(size=11, hjust = 0) ) +
xlab("") + ylab("") +
labs(title=title,
	 subtitle=subtitle,
	 caption=paste("Data Source:", data.source ) )
}

####################################################
plot.ipca.rets <- function(data, ndates, title, subtitle, data.source="IBGE.")
{
# TODO: ERROR CATCHING
varnames <- c("date", "ret1", "retyear", "retac12")
vars     <- c("date", "ret1", "retyear", "retac12")

newdata      <- tail(data[, vars], ndates); colnames(newdata) <- varnames
newdata      <- melt( newdata, id="date" )

tmp <- date2month( newdata$date )
# return(tmp)

#plot
ggplot(newdata, aes( x = date, y = value, fill=variable) ) +
theme_bw() +
geom_bar( stat="identity", position=position_dodge2(width = 1) ) +
# geom_text(aes(x=date, y=value, label=value), position=position_dodge(width = 1), vjust=ifelse(newdata$value>0, "bottom", "top"), hjust=.5) +
# scale_x_continuous(labels=tmp, breaks=tmp) +
# scale_x_discrete(labels=tmp, breaks=tmp) +
theme(legend.position = "bottom",
	  legend.title = element_blank(),
	  legend.text  = element_text(size = 11, face = "bold"),
	  panel.grid.major.x = element_blank(),
	  panel.grid.minor.x = element_blank(),
	  axis.text.x = element_text(color = "black", size = 11),
  	  plot.title = element_text(face = "bold", hjust = .5),
  	  plot.subtitle = element_text(hjust = .5),
  	  plot.caption = element_text(size=11,hjust = 0) ) +
xlab("") + ylab("") +
labs(title=title,
	 subtitle=subtitle,
	 caption=paste("Data Source:", data.source ) )
}

####################################################
gdp.ggplot.demand.growth <- function(data, subtitle, ndates)
{
# function to ggplot Growth by SECTORS

# TODO: ERROR CATCHING
varnames <- c("date", "PIB", "Consumo das \n Famílias", "Consumo do \n Governo", "FBCF", "Exportação de \n Bens e Serviços", "Importação de \n Bens e Serviços")
vars     <- c("date", "GDP", "C", "G", "FBKF", "X", "M")

newdata      <- tail(data[, vars], ndates); colnames(newdata) <- varnames
newdata$date <- date2qtr( newdata$date )
newdata      <- melt( newdata, id="date" )

#plot
ggplot(newdata, aes( x = variable, y = value, fill=date) ) +
theme_bw() +
geom_bar( stat="identity", position=position_dodge2(width = 1) ) +
geom_text(aes(x=variable, y=value, label=value), position=position_dodge(width = 1), vjust=ifelse(newdata$value>0, "bottom", "top"), hjust=.5) +
#	geom_text( aes(x=variable, y=value, label=value, vjust=ifelse(value>0, "bottom", "top"), hjust=0.5 ),
#			  position=position_dodge(width = 1) ) +
theme(legend.position = "bottom",
	  legend.title = element_blank(),
	  legend.text  = element_text(size = 11, face = "bold"),
	  panel.grid.major.x = element_blank(),
	  panel.grid.minor.x = element_blank(),
	  axis.text.x = element_text(color = "black", size = 11),
  	  plot.title = element_text(face = "bold", hjust = .5),
  	  plot.subtitle = element_text(hjust = .5),
  	  plot.caption = element_text(size=11,hjust = 0) ) +
xlab("") + ylab("") +
labs(title="GDP Growth by Demand Variables",
	 subtitle=subtitle,
	 caption="Data Source: IBGE.")
}


#######################################################
# TABLES By kableExtra
#######################################################

####################################################
# gdp table function
gdp.tab.1 <- function(data, caption=NULL)
{
# TODO: ERROR CATCHING

newdata <- t(tail(data[,-c(1,2,3)],5))
colnames(newdata) <- date2month(tail(data$date, 5))
rownames(newdata) <- c("T/T-4", "T/T-1 com Ajuste Sazonal", "T/T-4 AC4Q",  "Ac. Ano")
newdata <- newdata[c(4,3,1,2),]

mytab <- kable( newdata, caption=caption) %>%
	kable_classic("hover", full_width = T, font_size=12) %>%
	column_spec(6, bold = T) %>%
	footnote(general = "IBGE", general_title = "Data Source: ", footnote_as_chunk = T)
return(mytab)
}

####################################################
# gdp table function
gdp.tab.2 <- function(data, caption=NULL)
{
# TODO: ERROR CATCHING

vars     <- c("GDP", "AGR", "IND", "SER", "C", "G", "FBKF", "X", "M")
varnames <- c("PIB a preços de mercado", "Agropecuária", "Indústria", "Serviços", "Despesa de consumo das famílias", "Despesa de consumo do governo", "Formação Bruta de Capital Fixo", "Exportação de bens e serviços", "Importação d bens e serviços (-)")
newdata  <- data[, vars]; colnames(newdata) <- varnames; rownames(newdata) <- date2month(data$date)

mytab <- kbl( t(tail(newdata, 5)), caption=caption ) %>%
	kable_classic("hover", full_width = T, font_size=12) %>%
    row_spec(1, bold =T, background = "lightblue") %>%
	column_spec(6, bold = T) %>%
	pack_rows("Ótica da Produção", 2, 4) %>%
	pack_rows("Ótica da Demanda", 5, 9) %>%
	footnote(general = "IBGE", general_title = "Data Source: ",
			 footnote_as_chunk = T)
return(mytab)
}

####################################################
# gdp vab share table function
gdp.tab.2.full <- function(data, caption=NULL)
{
# TODO: ERROR CATCHING

varnames <- c("Agropecuária", "Indústria", "Extrativa Mineral", "Transformação", "Prod. e distrib. de eletricidade, gás, água e esgoto", "Construção", "Serviços", "Comércio", "Transporte, armazenagem e correio", "Serviços de informação", "Intermed. financeira, seguros, prev. complem. e serv. rel.", "Atividades Imobiliárias", "Outros serviços", "Adm., saúde e educação públicas", "VAB a Preços Básicos", "Impostos sobre Produtos", "PIB a Preços de Mercado", "Despesa de consumo das famílias", "Despesa de consumo da administração pública", "Formação bruta de capital fixo", "Exportação de bens e serviços", "Importação de bens e serviços (-)")

newdata <- t( tail(data[, -c(1,2,3)], 5) )
colnames(newdata) <- date2month( tail(data$date, 5) )
rownames(newdata) <- varnames

mytab <- kable( newdata , caption=caption, digits=1, format.args=list(decimal.mark=",", big.mark=".") ) %>%
	kable_classic("hover", full_width = T, font_size=12) %>%
    row_spec(c(1,2,7,15,16,18,19,20,21,22) , bold =T) %>%
	add_indent(c(3:6,8:14)) %>%
    row_spec(17, bold =T, background = "lightblue") %>%
	footnote(general = "IBGE",
			 general_title = "Data Source: ",
			 footnote_as_chunk = T)
return(mytab)
}

####################################################
# gdp table function
gdp.tab.level <- function(data, caption=NULL)
{
# TODO: ERROR CATCHING

vars     <- c( "AGR", "IND", "SER", "VAB", "TAX", "GDP", "C", "G", "FBKF", "X", "M", "DE" )
varnames <- c("Agropecuária", "Indústria", "Serviços", "VAB a Preços Básicos", "Impostos sobre Produtos", "PIB a Preços de Mercado", "Despesa de Consumo das Famílias", "Despesa de Consumo do Governo", "Formação Bruta de Capital Fixo", "Exportações de Bens e Serviços", "Importações de Bens e Serviços (-)", "Variação de Estoque")

newdata <- data[,vars]; colnames(newdata) <- varnames; rownames(newdata) <- date2month(data$date)
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
    row_spec(c(1,2,7,15,17), bold =T, background = "lightblue") %>%
	add_indent(c(3:6,8:14)) %>%
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
	kable_classic(c("hover", "striped"), full_width = T, font_size=12) %>%
	footnote(general = "IBGE",
			 general_title = "Data Source: ",
			 footnote_as_chunk = T)
return(mytab)
}

#######################################################
# TABLE RAW
#######################################################

#######################################################
# dates
#######################################################

####################################################
date2month <- function(data)
{
newdata <- as.character(data)

m1 <- paste0( c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), "-15")
m2 <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
for(i in seq_along(m1)) newdata <- gsub(m1[i], m2[i], newdata )
return(newdata)
}

####################################################
month2date <- function(data)
{

m1 <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
m2 <- paste0( c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), "-15")
for(i in seq_along(m1)) data <- gsub(m1[i], m2[i], data )
return(as.Date(data))
}

