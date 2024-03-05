l1score <-
function(x){
	stopifnot(is.vector(x) & is.numeric(x))
	if(sum(is.na(x)) == length(x)){return(rep(1, length(x)))}
	stopifnot(is.numeric(x))
	x <- abs(x)
	1 - x / max(c(1, x), na.rm = TRUE)
}
