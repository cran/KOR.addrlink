split_address <-
function(x, debug = FALSE) { 
	stopifnot(is.character(x), is.vector(x))
	vec_split_address <- Vectorize(helper_split_address, vectorize.args = "x", 
		USE.NAMES = FALSE, SIMPLIFY = FALSE)
	res <- vec_split_address(x, debug = debug)
	return(data.frame(Strasse = unlist(lapply(res, '[[', 1)), 
	Hausnummer = unlist(lapply(res, '[[', 2)), 
	Hausnummernzusatz = unlist(lapply(res, '[[', 3))))
}
