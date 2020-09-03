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
