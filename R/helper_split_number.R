helper_split_number <-
function(x, debug = FALSE){
	if(debug) print(x)
	x <- stringi::stri_replace_all_regex(str = x, 
		pattern = c("-", "/", "\\s+"), replace = c(" ", " ", " "), 
		vectorize_all = FALSE)
	x <- trimws(x)
	if(nchar(x) == 0){ return(data.frame(Hausnummer = NA, Zusatz = NA)) }
	x_split <- unlist(strsplit(x, ''))
	x_start <- head(which(x_split %in% as.character(0:9)), 1)
	x <- substr(x, x_start, nchar(x))
	x_split <- unlist(strsplit(x, ''))
	if(" " %in% x_split){
		x <- strsplit(x, ' ')[[1]][1]
		x_split <- unlist(strsplit(x, ''))
	}
	idx <- suppressWarnings(as.numeric(x_split))
	idx <- !is.na(idx)
	idx_rle <- rle(idx)
	hausnr <- as.numeric(substr(x, 1, head(idx_rle$length, 1)))
	if(hausnr == ""){ hausnr <- NA }
	zusatz <- substr(x, head(idx_rle$length, 1) + 1, head(idx_rle$length, 1) + 1)
	if(zusatz == ""){ zusatz <- NA }
	return(data.frame(Hausnummer = hausnr, Zusatz = zusatz))
}
