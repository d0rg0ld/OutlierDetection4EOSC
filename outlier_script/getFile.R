

#This work is co-funded by the EOSC-hub project (Horizon 2020) under Grant number 777536. 

getFile <- function(username, password, dav) {
  pkgTest("curl")
  pkgTest("stringdist")

  h <- new_handle()
  handle_setopt(h, username = username)
  handle_setopt(h, password = password)

  tmpzip=tempfile(, fileext="zip")
  download.file(dav, destfile=tmpzip, mode="wb")
  files=unzip(tmpzip, list=TRUE)
  resultat=NULL
  cnames=NULL
  print (paste("Extract Files from", tmpzip))
  for (file in files[files$Length>0,]$Name) {
    print (file)
    tab=read.table(unz(tmpzip,file), stringsAsFactors = F, sep=";")
    resultat=rbind(resultat,tab[-1,])
    cnames=append(cnames,paste(tab[1,], collapse=";"))
  }
  print ("Done extracting")

  unlink(tmpzip)

  if (sum(stringdistmatrix(cnames)) > 0) {
        #different colnames encountered, bail out
        return (0)
  }

  colnames(resultat)=unlist(strsplit(cnames[1], ";"))

  print (paste("Set colnames to", colnames(resultat)))

  vals=as.numeric(gsub(",",".",resultat$VALUE))
  #write(vals, stderr())
  resultat$VALUE=vals
  tsdata <- resultat



  #swap alternative aliases
  if (!("SITECODE" %in% colnames(resultat)) & ("SITE_CODE" %in% colnames(resultat))) 
	tsdata$SITECODE=tsdata$SITE_CODE
  if (!("FIELDNAME" %in% colnames(resultat)) & ("SUBST" %in% colnames(resultat))) 
	tsdata$FIELDNAME=tsdata$SUBST
  if (!("VALUE" %in% colnames(resultat)) & ("VAL" %in% colnames(resultat))) 
	tsdata$VALUE=tsdata$VAL
  if (!("HOUR" %in% colnames(resultat)) & ("SHOUR" %in% colnames(resultat))) 
	tsdata$HOUR=tsdata$SHOUR
  if (!("DAY" %in% colnames(resultat)) & ("SDAY" %in% colnames(resultat))) 
	tsdata$DAY=tsdata$SDAY
  if (!("MONTH" %in% colnames(resultat)) & ("SMONTH" %in% colnames(resultat))) 
	tsdata$MONTH=tsdata$SMONTH
  if (!("YEAR" %in% colnames(resultat)) & ("SYEAR" %in% colnames(resultat))) 
	tsdata$YEAR=tsdata$SYEAR
  if (!("MINUTE" %in% colnames(resultat)) & ("SMINUTE" %in% colnames(resultat))) 
	tsdata$MINUTE=tsdata$SMINUTE
  if (!("SECOND" %in% colnames(resultat)) & ("SSECOND" %in% colnames(resultat))) 
	tsdata$SECOND=tsdata$SSECOND
  if (("DATE" %in%  colnames(resultat)) & (!("DAY" %in% colnames(resultat))) & (!("MONTH" %in% colnames(resultat))) & (!("YEAR" %in% colnames(resultat)))) {
	dt1=as.POSIXlt(tsdata$DATE, format="%Y%m%d%H%M%S", tz="UTC")
	if ((sum(is.na(dt1))>0))	
		dt1=as.POSIXlt(tsdata$DATE, format="%Y%m%d", tz="UTC")
		if ((sum(is.na(dt1))>0))	
			dt1=as.POSIXlt(tsdata$DATE, format="%Y-%m-%d %H:%M:%S", tz="UTC")
	if (sum(is.na(dt1))==0) {
		tsdata$YEAR=dt1$year+1900
		tsdata$MONTH=dt1$mon+1
		tsdata$DAY=dt1$mday
		tsdata$HOUR=dt1$hour
		tsdata$MINUTE=dt1$min
		tsdata$SECOND=dt1$sec
	}
  }
  	
  if (!("YEAR" %in% colnames(resultat)) || !("MONTH" %in% colnames(resultat)) || !("DAY" %in% colnames(resultat)))
	return(NULL)


  if (!("HOUR" %in% colnames(resultat)))
        tsdata$HOUR=0
  if (!("MINUTE" %in% colnames(resultat)))
        tsdata$MINUTE=0
  if (!("SECOND" %in% colnames(resultat)))
        tsdata$SECOND=0
  if (!("RID" %in% colnames(resultat)))
        tsdata$RID=""

  tsdata=tsdata[,c("SITECODE","VALUE","FIELDNAME","RID","DAY","MONTH","YEAR", "HOUR", "MINUTE", "SECOND")]
  
 # colnames(tsdata) <- c("SITECODE","VALUE","FIELDNAME","RID","DAY","MONTH","YEAR", "HOUR", "MIN", "SEC")
  rownames(tsdata) <- seq(1,nrow(tsdata),1)
  tsdata$RID <- NULL
  tsdata$FIELDNAME <- paste(tsdata$SITECODE, tsdata$FIELDNAME, sep="_")
  tsdata$SITECODE <- NULL 
  return(tsdata)
}
