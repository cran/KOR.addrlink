sanitize_street <-
function(x){
	stopifnot(is.character(x), is.vector(x))
	x <- tolower(x)
	pattern <- c("\u00e4", "\u00fc", "\u00f6", "\u00df", "-", " ", "\\.", "str$")
	replacement <- c("ae", "ue", "oe", "ss", "", "", "", "strasse")
	x <- stringi::stri_replace_all_regex(str = x, 
		pattern = pattern, replace = replacement, 
		vectorize_all = FALSE)
	x <- stringi::stri_trans_general(str = x, id = "Any-Latin;Latin-ASCII")
	x <- stringi::stri_trans_nfc(str = x)
	x <- stringi::stri_replace_all_regex(str = x, 
		pattern = "[[:punct:]]", replace = "", vectorize_all = FALSE)
	return(x)
}
