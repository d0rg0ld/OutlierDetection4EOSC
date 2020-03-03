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
  for (file in files[files$Length>0,]$Name) {
    print (file)
    tab=read.table(unz(tmpzip,file), stringsAsFactors = F, sep=";")
    resultat=rbind(resultat,tab[-1,])
    cnames=append(cnames,paste(tab[1,], collapse=";"))
  }

  unlink(tmpzip)

  if (sum(stringdistmatrix(cnames)) > 0) {
        #different colnames encountered, bail out
        return (0)
  }

  colnames(resultat)=unlist(strsplit(cnames[1], ";"))

  vals=as.numeric(gsub(",",".",resultat$VALUE))
  #write(vals, stderr())
  resultat$VALUE=vals
  tsdata <- resultat

  # stay compatible with old shit
  cols=c("SITECODE","VALUE","FIELDNAME","RID","DAY","MONTH","YEAR")
  if (!("HOUR" %in% cols))
        tsdata$HOUR=0
  if (!("MIN" %in% cols))
        tsdata$MIN=0
  if (!("SEC" %in% cols))
        tsdata$SEC=0
  colnames(tsdata) <- c("SITECODE","VALUE","FIELDNAME","RID","DAY","MONTH","YEAR", "HOUR", "MIN", "SEC")
  rownames(tsdata) <- seq(1,nrow(tsdata),1)
  tsdata$RID <- NULL
  tsdata$OBSERVEDPROPERTY=""

}
