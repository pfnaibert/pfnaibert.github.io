# source
# https://portalibre.fgv.br/sites/default/files/2020-06/comunicado-do-comite-de-datacao-de-ciclos-economicos-29_06_2020-1.pdf

recs <- c("1981:Q1", "1983:Q1",
 "1987:Q3", "1988:Q3",
 "1989:Q3", "1992:Q1",
 "1995:Q2", "1995:Q3",
 "1998:Q1", "1999:Q1",
 "2001:Q2", "2001:Q4",
 "2003:Q1", "2003:Q2",
 "2008:Q4", "2009:Q1" )

saveRDS(recs, "../../_resources/data/recessions.rds")

exps <- c( "1983:Q2", "1987:Q2",
"1989:Q1", "1989:Q2",
"1992:Q2", "1995:Q1",
"1995:Q4", "1997:Q4",
"1999:Q2", "2001:Q1",
"2002:Q1", "2002:Q4",
"2003:Q3", "2008:Q3",
"2009:Q2", "2014:Q1",
"2017:Q1", "2019:Q4" )

saveRDS(exps, "../../_resources/data/expansions.rds")

