### R code from vignette source 'Example.Rnw'

###################################################
### code chunk number 1: Example.Rnw:25-28
###################################################
library(KOR.addrlink)
Adressen[c(sample(which(is.na(Adressen$HNRZ)), 4), 
	sample(which(!is.na(Adressen$HNRZ)), 2)),]


###################################################
### code chunk number 2: Example.Rnw:41-42
###################################################
df1[1180:(1183+6),]


###################################################
### code chunk number 3: Example.Rnw:48-50
###################################################
df1 <- cbind(df1, split_number(df1$hausnr))
df1[1180:(1183+6),]


###################################################
### code chunk number 4: Example.Rnw:57-63
###################################################
# column hausnr is no longer needed
df1 <- within(df1, rm(hausnr))
df1_matched <- addrlink(df_ref = Adressen, 
	col_ref = c("STRNAME", "HNR", "HNRZ"), 
	df_match = df1, 
	col_match = c("gross_strasse", "Hausnummer", "Hausnummernzusatz"))


###################################################
### code chunk number 5: Example.Rnw:72-74
###################################################
head(df1_matched$ret)
table(df1_matched$QA$qAddress)


###################################################
### code chunk number 6: Example.Rnw:88-89
###################################################
head(within(df2, Adresse <- trimws(Adresse)))


###################################################
### code chunk number 7: Example.Rnw:96-98
###################################################
df2 <- cbind(df2, split_address(df2$Adresse))
within(df2, Adresse <- trimws(Adresse))[23:(23+6),]


###################################################
### code chunk number 8: Example.Rnw:108-115
###################################################
# column Adresse is no longer needed
df2 <- within(df2, rm(Adresse))
df2_matched <- addrlink(df_ref = Adressen, 
	col_ref = c("STRNAME", "HNR", "HNRZ"), 
	df_match = df2, 
	col_match = c("Strasse", "Hausnummer", "Hausnummernzusatz"), 
	fuzzy_threshold = .9, seed = 1234)


###################################################
### code chunk number 9: Example.Rnw:118-120
###################################################
head(df2_matched$ret)
table(df2_matched$QA$qAddress)


###################################################
### code chunk number 10: Example.Rnw:126-129
###################################################
id <- which(df2_matched$QA$qAddress == 3) 
df2_matched$ret[id,]
df2_matched$QA[id,]


###################################################
### code chunk number 11: Example.Rnw:138-139
###################################################
sum(df2_matched$QA$qscore == 0) 


