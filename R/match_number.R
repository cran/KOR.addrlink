match_number <- 
function(record, Adressen, weights = c(.9, .1)){
	valid <- Adressen[Adressen$Strasse == record[["Strasse"]], c("Hausnummer", "Hausnummernzusatz")]
	if(is.na(record[["Hausnummer"]]) & is.na(record[["Hausnummernzusatz"]])){ #no info
		return(cbind(qscore = 0, Strasse = record[["Strasse"]], valid[sample(1:nrow(valid), 1),]))
	}
	if(is.na(record[["Hausnummer"]]) & !is.na(record[["Hausnummernzusatz"]])){ #no hnr, but hnrz
		zusatz <- l1score(match(record[["Hausnummernzusatz"]], LETTERS) - 
			match(valid$Hausnummernzusatz, LETTERS)) * weights[2]
		val <- max(zusatz, na.rm = TRUE)
		ids <- which(zusatz == val)
		if(length(ids) == 0){
			return(cbind(qscore = 0, Strasse = record[["Strasse"]], valid[sample(1:nrow(valid), 1),]))
		}
		if(length(ids) == 1){
			return(cbind(qscore = 0.05, Strasse = record[["Strasse"]], valid[ids,]))}
		return(cbind(qscore = 0.05, Strasse = record[["Strasse"]], valid[sample(ids, 1),]))
	}
	if(!is.na(record[["Hausnummer"]]) & is.na(record[["Hausnummernzusatz"]])){ #hnr, no hnrz
		hausnr_diff <- as.numeric(record[["Hausnummer"]]) - as.numeric(valid$Hausnummer)
		hausnr <- l1score(hausnr_diff) * weights[1]
		if(min(abs(hausnr_diff), na.rm = TRUE) > 4){
			return(cbind(qscore = 0, Strasse = record[["Strasse"]], valid[sample(1:nrow(valid), 1),]))
		}
		val <- max(hausnr, na.rm = TRUE)
		ids <- which(hausnr == val)
		if(length(ids) == 1){
			return(cbind(qscore = val, Strasse = record[["Strasse"]], valid[ids,]))}
		return(cbind(qscore = val, Strasse = record[["Strasse"]], valid[sample(ids, 1),]))
	}
	hausnr_diff <- as.numeric(record[["Hausnummer"]]) - as.numeric(valid$Hausnummer)
	hausnr <- l1score(hausnr_diff) * weights[1]
	zusatz <- l1score(match(record[["Hausnummernzusatz"]], LETTERS) - 
		match(valid$Hausnummernzusatz, LETTERS)) * weights[2]
	if(min(abs(hausnr_diff), na.rm = TRUE) > 4){#no hnr, but hnrz
		val <- max(zusatz, na.rm = TRUE)
		ids <- which(zusatz == val)
		if(length(ids) == 0){
			return(cbind(qscore = 0, Strasse = record[["Strasse"]], valid[sample(1:nrow(valid), 1),]))
		}
		if(length(ids) == 1){
			return(cbind(qscore = 0.05, Strasse = record[["Strasse"]], valid[ids,]))}
		return(cbind(qscore = 0.05, Strasse = record[["Strasse"]], valid[sample(ids, 1),]))
	}
	zusatz[is.na(zusatz)] <- - 0.05
	score <- hausnr + zusatz
	val <- max(score, na.rm = TRUE)
	ids <- which(score == val)
	if(length(ids) == 1){
		return(cbind(qscore = val, Strasse = record[["Strasse"]], valid[ids,]))}
	return(cbind(qscore = val, Strasse = record[["Strasse"]], valid[sample(ids, 1),]))
}