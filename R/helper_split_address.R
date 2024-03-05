helper_split_address <-
function(x, debug = FALSE){
	if(debug) print(x)
	x <- trimws(x)
	x_split <- unlist(strsplit(x, ''))
	num.idx <- which(x_split %in% as.character(0:9))
	if(length(num.idx) == 0){ return(list(strasse = x, hnr = NA, hnrz = NA)) }
	max.num.idx <- max(num.idx)
	num.extra <- which(x_split %in% c(" ", "-", "/"))
	num.extra <- num.extra[min(num.idx) < num.extra & max.num.idx > num.extra]
	num.idx <- c(num.idx, num.extra)
	idx.subs <- lapply(min(1, (max.num.idx - 7)):max.num.idx, function(x) x:max.num.idx)
	idx.hnr <- idx.subs[[min(which(unlist(lapply(idx.subs, function(y) all(y %in% num.idx)))))]]
	hnr <- trimws(substr(x, min(idx.hnr), max(idx.hnr)))
	if(grepl("-", hnr)){ 
		hnr <- as.numeric(unlist(strsplit(hnr, "-")))
		hnr <- head(hnr[!is.na(hnr)], 1)
	}
	if(grepl(" ", hnr)){ 
		hnr <- as.numeric(unlist(strsplit(hnr, " ")))
		hnr <- head(hnr[!is.na(hnr)], 1)
	}
	if(grepl("/", hnr)){ 
		hnr <- as.numeric(unlist(strsplit(hnr, "/")))
		hnr <- head(hnr[!is.na(hnr)], 1)
	} else {
	hnr <- as.numeric(hnr)}
	hnrz <- toupper(substr(trimws(substr(x, max(idx.hnr) + 1, nchar(x))), 1, 1))
	if(nchar(hnrz) == 0) hnrz <- NA
	strasse <- trimws(substr(x, 1, min(idx.hnr) - 1))
	strasse <- sub("[[:digit:]]+[a-zA-Z]$", "", strasse)
	return(list(strasse = strasse, hnr = hnr, hnrz = hnrz))
}
