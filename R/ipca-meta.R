# https://www.bcb.gov.br/controleinflacao/historicometas
# years <- seq(1999, 2023)

year <- c(1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)
meta  <- c(8, 6, 4, 3.5, 4, 5.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.25, 4, 3.75, 3.5, 3.25) 
banda <- c(2, 2, 2, 2, 2.5, 2.5, 2.5, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5) 
meta.mat <- data.frame( cbind(year, meta, banda, "LB"=meta-banda, "UB"=meta+banda) )
meta.mat

saveRDS(meta.mat, "../data/ipca-meta.rds")

# UPDATE meta.mat
# tmp <- readRDS("../data/ipca-meta.rds")
# rbind(tmp, c(2024, meta, banda, lb, ub))



