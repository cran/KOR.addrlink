split_number <-
function(x, debug = FALSE){ 
	stopifnot(is.character(x), is.vector(x))
	x <- trimws(x)
	x[which(x == "0" | x == "NULL" | x == "")] <- NA
	x_ready <- data.frame(Hausnummer = suppressWarnings(as.numeric(x)), 
		Hausnummernzusatz = NA)
	ids <- which(!is.na(x) & is.na(x_ready$Hausnummer))
	vec_split_hnr <- Vectorize(helper_split_number, vectorize.args = "x", 
		USE.NAMES = FALSE, SIMPLIFY = FALSE)
	res <- vec_split_hnr(x[ids], debug = debug)
	res <- data.frame(Hausnummer = unlist(lapply(res, '[[', 1)), 
		Hausnummernzusatz = unlist(lapply(res, '[[', 2)))
	x_ready[ids,] <- res
	return(x_ready)
}
