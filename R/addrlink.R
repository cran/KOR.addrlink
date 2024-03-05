addrlink <-
function(df_ref, df_match, 
		col_ref = c("Strasse", "Hausnummer", "Hausnummernzusatz"), 
		col_match = c("Strasse", "Hausnummer", "Hausnummernzusatz"), 
		fuzzy_threshold = .9, seed = 1234){
	
	stopifnot(is.data.frame(df_ref), is.data.frame(df_match), 
		is.vector(col_ref), is.character(col_ref), 
		is.vector(col_match), is.character(col_match))
	stopifnot(unique(colnames(df_ref)) == colnames(df_ref))
	stopifnot(unique(colnames(df_match)) == colnames(df_match))
	stopifnot(any(!is.na(col_ref), !is.na(col_match)))
	stopifnot(length(col_ref) == 3, length(col_match) == 3)
	stopifnot(col_ref %in% colnames(df_ref))
	stopifnot(col_match %in% colnames(df_match))
	stopifnot(fuzzy_threshold < 1, fuzzy_threshold > 0)
	stopifnot(seed > 0)
	set.seed(seed)
	
	Adressen <- df_ref[, col_ref]
	stopifnot(is.character(Adressen[,1]), is.numeric(Adressen[,2]), is.character(Adressen[,3]))
	stopifnot(nrow(unique(Adressen)) == nrow(Adressen))
	stopifnot(nrow(Adressen) > 0)
	colnames(Adressen) <- c("Strasse", "Hausnummer", "Hausnummernzusatz")
	Adressen$id.addr <- 1:nrow(Adressen)
	Adressen$Strasse <- sanitize_street(Adressen$Strasse)
	Adressen$Hausnummernzusatz <- tolower(Adressen$Hausnummernzusatz)
	
	df <- df_match[, col_match]
	stopifnot(is.character(df[,1]), is.numeric(df[,2]), 
		is.character(df[,3]))
	stopifnot(nrow(df) > 0)
	colnames(df) <- c("Strasse", "Hausnummer", "Hausnummernzusatz")
	df$id.df <- 1:nrow(df)
	df$Strasse <- sanitize_street(df$Strasse)
	df$Hausnummernzusatz <- tolower(df$Hausnummernzusatz)
	
	# first pass (direct matches)
	fp <- merge(x = df, y = Adressen, 
		by.x = c("Strasse", "Hausnummer", "Hausnummernzusatz"), 
		by.y = c("Strasse", "Hausnummer", "Hausnummernzusatz"), 
		incomparables = NULL)
	if(nrow(fp) > 0){
	fp$qAddress <- 1
	fp$qscore <- 1
	} else fp <- data.frame(Strasse = character(), Hausnummer = numeric(), 
		Hausnummernzusatz = character(), id.addr = numeric(), id.df = numeric(), 
		qAddress = numeric(), qscore = numeric())
	
	# second pass (street correct)
	tmp <- df[!(df$id.df %in% fp$id.df) & !is.na(df$Strasse),]
	sp <- merge(x = tmp, y = unique(Adressen[, "Strasse", drop = FALSE]), 
		by.x = c("Strasse"), 
		by.y = c("Strasse"), 
		incomparables = NULL)
	if(nrow(sp) > 0){
	sp <- cbind(id.df = sp$id.df, 
		do.call(rbind, apply(X = sp, MARGIN = 1, 
		FUN = match_number, Adressen = Adressen)))
	sp$qAddress <- 2
	sp <- merge(x = sp, y = Adressen, 
		by.x = c("Strasse", "Hausnummer", "Hausnummernzusatz"), 
		by.y = c("Strasse", "Hausnummer", "Hausnummernzusatz"), 
		incomparables = NULL)
	} else sp <- data.frame(Strasse = character(), Hausnummer = numeric(), 
		Hausnummernzusatz = character(), id.addr = numeric(), id.df = numeric(), 
		qAddress = numeric(), qscore = numeric())
	
	## third pass (fuzzy matches)
	tp <- df[!(df$id.df %in% c(fp$id.df, sp$id.df)) & !is.na(df$Strasse),]
	uSTR <- unique(Adressen$Strasse)
	tmp <- stringdist::stringsimmatrix(a = tp$Strasse, b = uSTR, method = "jw", 
		nthread = max(1, floor(parallel::detectCores() / 2)))
	threshold <- which(apply(tmp, MARGIN = 1, FUN = max) > fuzzy_threshold)
	if(length(threshold) > 0){
	tp$Strasse[threshold] <- uSTR[unlist(apply(tmp, MARGIN = 1, FUN = which.max))[threshold]]
	tp <- merge(x = tp, y = unique(Adressen[, "Strasse", drop = FALSE]), 
		by.x = c("Strasse"), 
		by.y = c("Strasse"), 
		incomparables = NULL)
	tp <- cbind(id.df = tp$id.df, 
		do.call(rbind, apply(X = tp, MARGIN = 1, 
		FUN = match_number, Adressen = Adressen)))
	tp$qscore <- tp$qscore * apply((tmp[threshold, , drop = FALSE]), 1, max)
	tp$qAddress <- 3
	tp <- merge(x = tp, y = Adressen, 
		by.x = c("Strasse", "Hausnummer", "Hausnummernzusatz"), 
		by.y = c("Strasse", "Hausnummer", "Hausnummernzusatz"), 
		incomparables = NULL)
	} else tp <- data.frame(Strasse = character(), Hausnummer = numeric(), 
		Hausnummernzusatz = character(), id.addr = numeric(), id.df = numeric(), 
		qAddress = numeric(), qscore = numeric())
	
	# no match
	nomatch <- df[!(df$id.df %in% c(fp$id.df, sp$id.df, tp$id.df)),]
	if(nrow(nomatch) > 0){
	nomatch$Strasse <- NA
	nomatch$Hausnummer <- NA
	nomatch$Hausnummernzusatz <- NA
	nomatch$id.addr <- NA
	nomatch$qAddress <- 4
	nomatch$qscore <- 0
	} else nomatch <- data.frame(Strasse = character(), Hausnummer = numeric(), 
		Hausnummernzusatz = character(), id.addr = numeric(), id.df = numeric(), 
		qAddress = numeric(), qscore = numeric())

	# results 
	res <- rbind(fp, sp, tp, nomatch)
	ret <- cbind(df_match[res$id.df,], df_ref[res$id.addr,])
	return(list(ret = ret, QA = res[, c("qAddress", "qscore")]))
}
