#This work is co-funded by the EOSC-hub project (Horizon 2020) under Grant number 777536.  

options(encoding = "UTF-8")
# from
#}
#addTaskCallback(function(expr, value, ok, visible) {
#	cat('\n',as.character(Sys.time()), '\t') 
#	TRUE 
#})

# from 
# https://stackoverflow.com/questions/9341635/check-for-installed-packages-before-running-install-packages

pkgTest <- function(x, mode="regular", extra="")
{
  if (!require(x,character.only = TRUE))
  	if(mode == "regular") {
  	    install.packages(x,dep=TRUE)
  	    if(!require(x,character.only = TRUE)) stop("Package not found")
  	} else {
  		if (mode=="github") {
    			pkgTest("remotes")
    			install_github(x, extra)
  		}
	}
}



###########################
# R-Skript für Ausreißeranalyse von Messzeitreihen 
###########################

# parameter, die in der url definiert werden müssen
# daten entweder von sos tsm (1) oder next cloud (2)
##type <- 1
# wenn 1, dann ..
# site/station im tsm 
##sos_site <- "ZOE_0551P00"
# parameter im tsm   
##sos_parameter <- "SurfaceWaterConcentration_DOC_0.2m"
# anfang und ende der untersuchungsperiode
##sos_startperiod <- "2018-01-01TT00:00:00+01:00"
##sos_endperiod <- "2018-01-31T23:59:59+01:00"
# breite des moving windows (z.b. in dem fall 40 tage)
##stat_movingwindows <- 10
# sprung zwischen den einzelnen moving windows (alle 2 wochen ein moving window mit 40 tage)
##stat_interval <- 10
# pfad wo die resultate exportiert werden

cachepath <- "./qs/cache/" 

library("optparse")
 
option_list = list(
	make_option(c("-e", "--endpoint"), type="character", default="http://ltercwn01.umweltbundesamt.at:8080/cwn.all.sos2/service", 
              help="URL SOS endpoint [default= %default]", metavar="character"),
	make_option(c("-r", "--repourl"), type="character", default="", 
              help="URL DAV REPO endpoint", metavar="character"),
	make_option(c("-u", "--username"), type="character", default="", 
              help="URL DAV REPO username", metavar="character"),
	make_option(c("-c", "--credential"), type="character", default="", 
              help="URL DAV REPO password", metavar="character"),
	make_option(c("-s", "--site"), type="character", default="ZOE_0551P00", 
              help="SOS name of Site", metavar="character"),
	make_option(c("-p", "--parameter"), type="character", default="SurfaceWaterConcentration_DOC_0.2m", 
              help="SOS name of Parameter", metavar="character"),
	make_option(c("-f", "--from"), type="character", default="2018-01-01T00:00:00+01:00", 
              help="Period Start - Datetime", metavar="character"),
	make_option(c("-t", "--to"), type="character", default="2018-01-31T23:59:59+01:00", 
              help="Period End - Datetime", metavar="character"),
	make_option(c("-w", "--window"), type="integer", default=10, 
              help="Width of moving window in datapoints (integer) [default= %default]", metavar="integer"),
	make_option(c("-i", "--interval"), type="integer", default=10, 
              help="Distance between windows in datapoints (integer) [default= %default]", metavar="integer"),
	make_option(c("-m", "--mode"), type="integer", default=1, 
              help="SOS (1) or File based (2) [default= %default]", metavar="integer"),
	make_option(c("-l", "--logfile"), type="character", default="log.txt", 
              help="Log output to file [default= %default]", metavar="character"),
	make_option(c("-o", "--outpathbase"), type="character", default="./qs/results/", 
              help="result directory [default= %default]", metavar="character"),
	make_option(c("-z", "--timestamp"), type="character", default="", 
              help="Init timestamp", metavar="character"),
	make_option(c("-q", "--quiet"), type="logical", default=FALSE, 
              help="Be quiet [default= %default]", metavar="logical")
) 
 
opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)


#http://stackoverflow.com/questions/7096989/how-to-save-all-console-output-to-file-in-rfrom https://stackoverflow.com/questions/7096989/how-to-save-all-console-output-to-file-in-r
con <- file(opt$logfile, "a")
devnull <- file("/dev/null", "a")
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")

print (paste("------------ BEGIN ---",Sys.time(),"----------------", sep=""))

#parameter passing
.sos <- opt$endpoint
sos_site	<- opt$site
sos_parameter	<- opt$parameter
sos_startperiod	<- opt$from
sos_endperiod	<- opt$to
stat_movingwindows <- opt$window
stat_interval <- opt$interval
type <- opt$mode
dav <- opt$repourl
username = opt$username
password = opt$credential
respath=opt$outpathbase
myts=opt$timestamp

#DEBUG OVERRIDE
.sos <- "https://sensorweb.demo.52north.org/sensorwebtestbed/service" 
sos_site	<- "ELV-WS2500"
sos_parameter	<- "AirTemperature"
sos_startperiod	<- "2015-06-01T00:00:00+01:00"
sos_endperiod	<- "2015-06-05T23:59:59+01:00"

.sos <- "https://lter-at-sos.sos.cdn.lter-europe.net/service"
sos_site        <- "LTER_EU_AT_003"
sos_parameter	<- "http://vocabs.lter-europe.net/EnvThes/22035"
sos_startperiod	<- "2012-06-01T00:00:00+01:00"
sos_endperiod	<- "2012-06-30T23:59:59+01:00"

# <sos:procedure>/1579860990023/LTER_AT/LTER_EU_AT_003/ATZOEAM0939_TEMPMEAN_TEST3/AIR</sos:procedure>

stat_movingwindows <- 40
stat_interval <- 14
type <- 1
dav <- ""
username = ""
password = ""
respath="./qs/results/"
myts="20200122_143300"

.loadCache=FALSE

#DEBUG OVERRIDE

#stat_movingwindows <- 40
#stat_interval <- 14
#type <-0 
#dav <- "https://docs.umweltbundesamt.at/s/W4Fq5JWxG4QXQzs/download"
#username = ""
#password = ""
#respath="./qs/results/"
#myts="20200122_105051"

mypid=Sys.getpid()
mydir=paste(mypid, myts, sep="_")
respath <- paste(respath,"/",mydir, "/", sep="")

dir.create(respath)

write("Paras: ", stderr())
for ( k in seq(1,length(opt))) {
	# do not disclose theusername
	listkey=names(opt)[k]
	if (listkey!="username" & listkey!="credential") {
		print (paste(listkey, opt[[k]], sep=": "))
	}
}


#browser()
if (type==1) {
  pkgTest("remotes")
  pkgTest("52North/sos4R", mode="github", extra="feature/0.4")
  pkgTest("sos4R")
  pkgTest("lubridate")
  
.verbose <- FALSE 
.saveOriginal <- FALSE
.version <- sos200_version
.binding <- "KVP"
.responseFormat <- "http://www.opengis.net/om/2.0"

sos=NULL
try(sos <- SOS(url = .sos, version = .version, verboseOutput = .verbose, binding = .binding))

.site <- sos_site
 
myoffering=NULL 
try(myoffering <- sosOfferings(sos)[[sos_site]])

.observedProperty <- list(sos_parameter)

sos_startperiod <- as_datetime(sos_startperiod)
sos_endperiod <- as_datetime(sos_endperiod)

sos_startperiod_utc <- floor_date(sos_startperiod, unit="month")
#print(unclass(sos_endperiod))
#browser()
sos_endperiod_utc <- (ceiling_date(sos_endperiod, unit="month")-seconds(1))

print (paste("sos_startperiod : ",	sos_startperiod,	" sos_endperiod : ",	sos_endperiod, 		sep=""))
print (paste("sos_startperiod_utc : ",	sos_startperiod_utc,	" sos_endperiod_utc : ",sos_endperiod_utc, 	sep=""))
print (paste("sos_startperiod_utc : ",	sos_startperiod_utc,	" sos_endperiod_utc : ",sos_endperiod_utc-seconds(1), 	sep=""))

#browser()
source <- sos_startperiod_utc
target <- source+months(1)-seconds(1)

restab00 <- t(data.frame(rep(NA,10)))
colnames(restab00) <- c("SITECODE", "OBSERVEDPROPERTY", "YEAR","MONTH", "DAY", "HOUR", "MIN", "SEC", "FIELDNAME","VALUE")

print (paste("source : ",source," target : ",target, sep=""))
done=FALSE
while (!done) {
  #browser()
  print (source)
  print (target)
  periodstart <- paste(sprintf("%04d",year(source)),sprintf("%02d",month(source)),sprintf("%02d",day(source)),sep="")
  periodend <- paste(sprintf("%04d",year(target)), sprintf("%02d",month(target)),sprintf("%02d",day(target)),sep="")

  cache_filename=paste(gsub(" ", "_", sos_site) ,"_",sos_parameter,"_",periodstart,"_",periodend,sep="")

  cache_filename=paste(cachepath,gsub("[://]","_", cache_filename), sep="")

  loaded_object=NULL
  print (cache_filename)
  try ((loaded_object=load(cache_filename)), silent=TRUE) 
  print (loaded_object)
  if (.loadCache==FALSE || is.null(loaded_object) || loaded_object!="restab") { 
	      period <- sosCreateTimePeriod(sos = sos,
					    begin = source,
					    end = target)
	      .eventTime <- sosCreateEventTimeList(period)
	    	print(.eventTime) 
	      print (paste("Loading observations from SOS : ", cache_filename, sep="")) 


	      myGetObservation <- getObservation(sos = sos,
						 offering = sosOfferings(sos),
						 featureOfInterest = .site,
						 observedProperty = .observedProperty,
						 #responseFormat = .responseFormat,
						 eventTime = .eventTime,
						 verbose = FALSE,
						 #verbose = .verbose,
						 saveOriginal = .saveOriginal)
	      save(myGetObservation, file=paste(cache_filename, "myGetObs", sep="_"))
	      print (paste("Finished loading observations from SOS : ", "", sep="")) 
	  
	      restab <- t(data.frame(rep(NA,32)))
	      colnames(restab) <- c(	"foi",
					"procedure",
					"phentime_start_year",
					"phentime_start_month",
					"phentime_start_day",
					"phentime_start_hour",
					"phentime_start_min",
					"phentime_start_sec",
					"phentime_end_year",
					"phentime_end_month",
					"phentime_end_day",
					"phentime_end_hour",
					"phentime_end_min",
					"phentime_end_sec",
					"obsprop", 
					"result", 
					"resultUnit", 
					"restime_start_year",
					"restime_start_month",
					"restime_start_day",
					"restime_start_hour",
					"restime_start_min",
					"restime_start_sec",
					"restime_end_year",
					"restime_end_month",
					"restime_end_day",
					"restime_end_hour",
					"restime_end_min",
					"restime_end_sec",
					"resQual",
					"parameter",
					"metadata")
	      browser() 
	      for (k in c(1:dim(summary(myGetObservation))[1])) {
			
			res00 <- data.frame(foi=toString(myGetObservation[[k]]@featureOfInterest@feature@identifier))

			res00$procedure=toString(myGetObservation[[k]]@procedure)		

			if (class(myGetObservation[[k]]@phenomenonTime) == "GmlTimePeriod") {
				timestart=unclass(as.POSIXlt(myGetObservation[[k]]@phenomenonTime@begin@timePosition@time))
				timeend=unclass(as.POSIXlt(myGetObservation[[k]]@phenomenonTime@end@timePosition@time))
			} else {
				timestart=unclass(as.POSIXlt(myGetObservation[[k]]@phenomenonTime@timePosition@time))
				timeend=timestart
			}
			#res00$phentime <- toString(myGetObservation[[k]]@phenomenonTime)
			#print(timestart)
			res00$phentime_start_year	<- timestart$year+1900
			res00$phentime_end_year		<- timeend$year+1900
			res00$phentime_start_month	<- timestart$mon+1
			res00$phentime_end_month	<- timeend$mon+1
			res00$phentime_start_day 	<- timestart$mday
			res00$phentime_end_day  	<- timeend$mday
			res00$phentime_start_hour 	<- timestart$hour
			res00$phentime_end_hour  	<- timeend$hour
			res00$phentime_start_min 	<- timestart$min
			res00$phentime_end_min  	<- timeend$min
			res00$phentime_start_sec 	<- timestart$sec
			res00$phentime_end_sec  	<- timeend$sec

			res00$obsprop <-  unlist(myGetObservation[[k]]@observedProperty@href)
			res00$result <- toString(myGetObservation[[k]]@result)
			res00$resultUnit <- toString(names(myGetObservation[[k]]@result))
			#res00$resTime <- toString(myGetObservation[[k]]@resultTime)
			if (class(myGetObservation[[k]]@resultTime) == "GmlTimePeriod") {
				timestart=unclass(as.POSIXlt(myGetObservation[[k]]@resultTime@begin@timePosition@time))
				timeend=unclass(as.POSIXlt(myGetObservation[[k]]@resultTime@end@timePosition@time))
			} else {
				timestart=unclass(as.POSIXlt(myGetObservation[[k]]@resultTime@timePosition@time))
				timeend=timestart
			}
			res00$restime_start_year	<- timestart$year+1900
			res00$restime_end_year		<- timeend$year+1900
			res00$restime_start_month	<- timestart$mon+1
			res00$restime_end_month		<- timeend$mon+1
			res00$restime_start_day 	<- timestart$mday
			res00$restime_end_day		<- timeend$mday
			res00$restime_start_hour 	<- timestart$hour
			res00$restime_end_hour  	<- timeend$hour
			res00$restime_start_min 	<- timestart$min
			res00$restime_end_min		<- timeend$min
			res00$restime_start_sec 	<- timestart$sec
			res00$restime_end_sec		<- timeend$sec
			res00$resQual <- toString(myGetObservation[[k]]@resultQuality)
			res00$parameter <- toString(myGetObservation[[k]]@parameter)
			res00$metadata <- toString(myGetObservation[[k]]@metadata)
			#browser()	
			restab <- rbind(restab,res00)
	      }
	      #browser()
	      restab <- restab[-1,]
	      restab <- restab[,c(	"foi",
					"obsprop",
					"phentime_start_year",
					"phentime_start_month",
					"phentime_start_day",
					"phentime_start_hour",
					"phentime_start_min",
					"phentime_start_sec",
					"procedure","result")]
		# treat procedure as fieldname, dirty workaround
	      colnames(restab) <- c("SITECODE","OBSERVEDPROPERTY","YEAR","MONTH", "DAY", "HOUR", "MIN", "SEC", "FIELDNAME","VALUE")
	      #resultat$duplicated <- duplicated(resultat$date)
	      #nrow(resultat)
	      #resultat <- resultat[resultat$duplicated==F,]
	      #nrow(resultat)
	      #resultat$duplicated <- NULL
	      #browser()
	      
	      save(restab,file=cache_filename)
      }	
      restab00 <- rbind(restab00,restab)
      print(Sys.time())
      source=target + seconds(1)
      print (target)
      target=target %m+% months(1)
      print (source)
      print (target)
      #browser()
      if (source> sos_endperiod_utc)
	done=TRUE
}
restab00 <- restab00[-1,]
#resultat <- restab00[,c("foi","phentime","obsprop","result")]
#colnames(resultat) <- c("SITECODE","date","FIELDNAME","VALUE")
#resultat$duplicated <- duplicated(resultat$date)
#nrow(resultat)
#resultat <- resultat[resultat$duplicated==F,]
#nrow(resultat)
#resultat$duplicated <- NULL
#datetemp <- data.frame(data=unlist(strsplit(resultat$date," "),recursive=T))
#datetemp$check <- grepl("-",datetemp$data)
#datetemp <- datetemp[datetemp$check==TRUE,]
#datetemp$check <- NULL
#resultat <- cbind(resultat,datetemp)
#resultat$DAY <- substr(resultat$data,9,10)
#resultat$MONTH <- substr(resultat$data,6,7)
#resultat$YEAR <- substr(resultat$data,1,4)
#resultat$data <- NULL
#resultat$date <- NULL

#tsdata <- resultat[,c("SITECODE","VALUE","FIELDNAME","DAY","MONTH","YEAR")]
tsdata <- restab00
rownames(tsdata) <- seq(1,nrow(tsdata),1)
tsdata$VALUE <- as.numeric(tsdata$VALUE)

} else {
  pkgTest("curl")
  pkgTest("stringdist")


  #Downloading and Unzipping files in R
  #https://rpubs.com/otienodominic/398952
  
  # download eines bestimmten files
  #dav = "https://docs.umweltbundesamt.at/s/PFK3eHkHNSMETg8/download"
  # download des ganzen ordner inhalts mit curl_download auf definierten ordner
  #dav <- "https://docs.umweltbundesamt.at/s/W4Fq5JWxG4QXQzs/download"


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


  #rownames(resultat) <- 1
  #temp <- seq(1,ncol(resultat),1)
  #colnames(resultat) <- paste("par_",temp,sep="")

	
 # write(colnames(resultat), stderr())
    
#  for (xy in c(2:nrow(res))) {
#    resultat01 <- t(data.frame(strsplit(res[xy,"data"],";")))
#    rownames(resultat01) <- 1
#    temp <- seq(1,ncol(resultat01),1)
#    colnames(resultat01) <- paste("par_",temp,sep="")
#    resultat <- rbind(resultat,resultat01)
#    rm(temp)
#  }
#resultat <- data.frame(resultat)

vals=as.numeric(gsub(",",".",resultat$VALUE))
#write(vals, stderr())
resultat$VALUE=vals
#resultat$DAY=as.numeric(gsub(",",".",resultat$DAY))
#resultat$MONTH=as.numeric(gsub(",",".",resultat$MONTH))
#resultat$YEAR=as.numeric(gsub(",",".",resultat$YEAR))

#  for (xy in c(1:ncol(resultat))) {
#temp <- as.numeric(gsub(",",".",as.character(resultat[,xy])))
#if (is.na(mean(temp))==TRUE) { resultat[,xy] <- resultat[,xy] } else { resultat[,xy] <- as.numeric(gsub(",",".",as.character(resultat[,xy]))) } 
#}

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

#write(tsdata$VALUE, stderro))

#browser()

#############
# Ausreisseranalyse
#############

pkgTest("chron")
pkgTest("GmAMisc")
pkgTest("extremevalues")
pkgTest("univOutl")
pkgTest("OutlierDetection")
pkgTest("DMwR")
pkgTest("outliers")
#pkgTest("forecast")
#pkgTest("EnvStats")
#pkgTest("RCurl")

#homepath <- "//umweltbundesamt.at/Projekte/3000/3135_IntMon/Intern/ARCHneu/UBA_IM/Themen_nach_Projektbereichen/aaPB_uebergreifende/sos4R_tsm/daten/"

# import des lims export files (von christian schütz - ausgangsfile für die aufbereitung der chemiedaten vor upload auf trans_lab)
#tsdata <- read.csv2(paste(homepath,"eLTER_CDN_Daten_20191029.csv",sep=""),header=T,quote="",sep=";",dec=",",stringsAsFactors=FALSE)
#str(tsdata)

tsdata$SITECODE <- as.character(tsdata$SITECODE)
tsdata$FIELDNAME <- as.character(tsdata$FIELDNAME)
tsdata$DAY <- as.numeric(tsdata$DAY)
tsdata$MONTH <- as.numeric(tsdata$MONTH)
tsdata$YEAR <- as.numeric(tsdata$YEAR)
tsdata$VALUE <- as.numeric(tsdata$VALUE)
tsdata$ID <- paste(tsdata$SITECODE, tsdata$FIELDNAME, tsdata$OBSERVEDPROPERTY, sep="_")

paraloop <- unique(tsdata$ID)

resxx <- data.frame(t(rep(NA,12)))
colnames(resxx) <- c("merge","SITECODE", "OBSERVEDPROPERTY","FIELDNAME","VALUE","date","statistik","period","mean","startperiod","endperiod","grubbstest")
resxx$date <- as.POSIXct("1970-01-01 00:00:00", tz="UTC")
#resxx$date <- as.Date("1970-01-01 00:00:00",format = c("%Y-%m-%d %h:%m:%s"))

#Suppress Graphics output
pdf(file=NULL)

for (i in paraloop) {
	#browser()
	pardata <- tsdata[tsdata$ID==i,]

	print(i)

	pardata$MONTH <- ifelse(nchar(pardata$MONTH)==1,paste("0",pardata$MONTH,sep=""),pardata$MONTH)
	pardata$DAY <- ifelse(nchar(pardata$DAY)==1,paste("0",pardata$DAY,sep=""),pardata$DAY)
	pardata$HOUR <- ifelse(nchar(pardata$HOUR)==1,paste("0",pardata$HOUR,sep=""),pardata$HOUR)
	pardata$MIN <- ifelse(nchar(pardata$MIN)==1,paste("0",pardata$MIN,sep=""),pardata$MIN)
	pardata$SEC <- ifelse(nchar(pardata$SEC)==1,paste("0",pardata$SEC,sep=""),pardata$SEC)
	pardata$merge <- paste(	pardata$YEAR,"-", pardata$MONTH,"-", pardata$DAY," ",
				pardata$HOUR,":",pardata$MIN,":",pardata$SEC,sep="")

	pardata02 <- pardata[,c("merge", "SITECODE", "OBSERVEDPROPERTY","FIELDNAME","VALUE")]

	# introduce three "outliers"
	idx=sample(nrow(pardata02), 3)
	pardata02[idx,]$VALUE=pardata02[idx,]$VALUE*2.0
	#pardata02$date <- as.Date(pardata02$merge,format = c("%Y-%m-%d %h:%m:%s"))
	pardata02$date <- as.POSIXct(pardata02$merge,tz="UTC")
	#brute force cut re timespan
	
	#check sos or file
	if (type==1) {
		#sos case -> we take the actual period passed via arguments
		realstart=as.POSIXct(sos_startperiod)
		realend=as.POSIXct(sos_endperiod)
	} else {
		realstart=min(as.POSIXct(pardata$merge))
		realend=max(as.POSIXct(pardata$merge))
	}

	pardata02=pardata02[pardata02$date>=realstart & pardata02$date<=realend,]

	pardata02 <- pardata02[order(pardata02$date,decreasing=F),]
	rownames(pardata02) <- seq(1,nrow(pardata02),1)

	#get start and end dates wrt moving window
	startd <- sort(unique(pardata02$date))[(stat_movingwindows/2)+1]
	endd <- sort(unique(pardata02$date), decreasing=T)[(stat_movingwindows/2)+1]
	#startd <- min(pardata02$date)+(stat_movingwindows/2)
	#endd <- max(pardata02$date)-(stat_movingwindows/2)

	# Tabelle mit dem wirklichen Start und Endzeitpunkt
	tempdata <- pardata02[pardata02$date>=startd&pardata02$date<=endd,]
	# Anzahl der Zeitabschnitte, die auf Ausreisser untersucht werden
	uniquetimestamps <- unique(sort(pardata02$date))
	uniquetimestamps_window <- uniquetimestamps[uniquetimestamps>=startd&uniquetimestamps<=endd]
	reps <- length(uniquetimestamps_window)/stat_interval

	print (paste(length(uniquetimestamps), " in total"))
	print (paste(length(uniquetimestamps_window), " datapoints make ", reps, " intervals"))
	#cv <- ifelse(cv<5,cv,5)

	res00 <- data.frame(t(rep(NA,12)))
	colnames(res00) <- c("merge","SITECODE", "OBSERVEDPROPERTY","FIELDNAME","VALUE","date","statistik","period","mean","startperiod","endperiod","grubbstest")
	#res00$date <- as.Date("1970-01-01 00:00:00",format = c("%Y-%m-%d %h:%m:%s"))
	res00$date <- as.POSIXct("1970-01-01 00:00:00", tz="UTC")



	for (cv in seq(0,reps+1,1)) {
	  print(cv)
	  res01 <- data.frame(t(rep(NA,7)))
	  colnames(res01) <- c("merge","SITECODE", "OBSERVEDPROPERTY","FIELDNAME","VALUE","date","statistik")
	  #res01$date <- as.Date("1970-01-01 00:00:00",format = c("%Y-%m-%d %h:%m:%s"))
	  res01$date <- as.POSIXct("1970-01-01 00:00:00",tz="UTC")
		#>> Potential Problem	  
	  #startperiod <- (startd+stat_interval*cv)-(stat_movingwindows/2)
	  #endperiod <- (startd+stat_interval*cv)+(stat_movingwindows/2)
	  startperiod <- uniquetimestamps[1+stat_interval*cv]
	  endidx=1+stat_interval*cv+stat_movingwindows
	  if (endidx > length(uniquetimestamps)) {
		endidx=length(uniquetimestamps)
		
	  }		
	  endperiod   <- uniquetimestamps[endidx]
		# FIXME
	  if (startperiod > endd)
		break
	  if (endperiod > endd)
		endperiod=endd
	  
	  print (paste(startperiod, " to ", endperiod))

	  outlierdata <- pardata02[pardata02$date>=startperiod&pardata02$date<=endperiod,]
	  	temp01 <- data.frame(GmAMisc::outlier(outlierdata$VALUE,method="mean",addthres=TRUE)$flaggedData)
	  temp02 <- "TRUE" %in% temp01$outlier
	  temp03 <- temp01[temp01$outlier==TRUE,"ID"]
	  if (temp02==TRUE) { 
		res02 <- outlierdata[temp03,] 
		res02$statistik <- "GmAMisc_outlier_mean"
	  }
	  if (exists("res02")==T) { 
		res01 <- rbind(res01,res02)
		rm(temp01,temp02,temp03,res02) 
	  } else { 
		rm(temp01,temp02,temp03)
	  }
	 
	  	temp01 <- data.frame(GmAMisc::outlier(outlierdata$VALUE,method="median",addthres=TRUE)$flaggedData)
	  
	  temp02 <- "TRUE" %in% temp01$outlier
	  temp03 <- temp01[temp01$outlier==TRUE,"ID"]
	  if (temp02==TRUE) { 
		res02 <- outlierdata[temp03,] 
		res02$statistik <- "GmAMisc_outlier_median"
	  }
	  if (exists("res02")==T) { 
		res01 <- rbind(res01,res02)
		rm(temp01,temp02,temp03,res02) 
	  } else { 
		rm(temp01,temp02,temp03)
	  }
	 
	  	temp01 <- data.frame(GmAMisc::outlier(outlierdata$VALUE,method="boxplot",addthres=TRUE)$flaggedData)
	  temp02 <- "TRUE" %in% temp01$outlier
	  temp03 <- temp01[temp01$outlier==TRUE,"ID"]
	  if (temp02==TRUE) { 
		res02 <- outlierdata[temp03,] 
		res02$statistik <- "GmAMisc_outlier_boxplot"
	  }
	  if (exists("res02")==T) { 
		res01 <- rbind(res01,res02)
		rm(temp01,temp02,temp03,res02) 
	  } else { 
		rm(temp01,temp02,temp03)
	  }
	 
	  #write(outlierdata$VALUE, stderr())
 
	  shapirotemp <- shapiro.test(outlierdata$VALUE)$p.val

	  	if (shapirotemp<0.05) { 
			temp01 <- extremevalues::getOutliers(outlierdata$VALUE,method="I") 
		  } else { 
			temp01 <- extremevalues::getOutliers(outlierdata$VALUE,method="I")
		  }

	  res02 <- outlierdata[temp01$iRight,]
	  if (nrow(res02)>0) { 
		res02$statistik <- "extremevalues_getoutliers_I"
	  }
	  res01 <- rbind(res01,res02)
	  rm(res02)
	  
	  res02 <- outlierdata[temp01$iLeft,]
	  if (nrow(res02)>0) { 
		res02$statistik <- "extremevalues_getoutliers_I"
	  }
	  res01 <- rbind(res01,res02)
	  rm(temp01,res02)
	  
	  # package: extremevalues, function: getoutliers, method: II
	 	 if (shapirotemp<0.05) { 
			temp01 <- extremevalues::getOutliers(outlierdata$VALUE,method="II") 
		  } else { 
			temp01 <- getOutliers(outlierdata$VALUE,method="II") 
		  }
 
	  res02 <- outlierdata[temp01$iRight,]
	  if (nrow(res02)>0) { 
		res02$statistik <- "extremevalues_getoutliers_II"
	  }
	  res01 <- rbind(res01,res02)
	  rm(res02)
	  
	  res02 <- outlierdata[temp01$iLeft,] 
	  if (nrow(res02)>0) { 
		res02$statistik <- "extremevalues_getoutliers_II"
	  }
	  res01 <- rbind(res01,res02)
	  rm(res02)
	  rm(shapirotemp)
	  # package: univOutl, function: LocScaleB, method: MAD; k: 2
	  for (vb in c("IQR","IDR","MAD","Gini","ScaleTau2","Qn","Sn")) {
	  	  sink()
		  sink(devnull)
		  sink(devnull, type="message")
		  	res02 <- univOutl::LocScaleB(outlierdata$VALUE,k=2,method=vb,weights=NULL,id=NULL,exclude=NA,logt=FALSE,return.dataframe=TRUE)$data
		  sink()
		  sink(con, append=TRUE)
		  sink(con, append=TRUE, type="message")
		  res02 <- res02[res02$outliers==1,]
		  if (nrow(res02)>0) { res02 <- outlierdata[res02$id,] 
		    res02$statistik <- paste("univOutl_LocScaleB_",vb,"_k2",sep="")
		    }
		  res01 <- rbind(res01,res02)
		  rm(res02)
		  
		  # package: univOutl, function: LocScaleB, method: MAD; k: 2
		  sink()
		  sink(devnull)
		  sink(devnull, type="message")
		 	res02 <- univOutl::LocScaleB(outlierdata$VALUE,k=3,method=vb,weights=NULL,id=NULL,exclude=NA,logt=FALSE,return.dataframe=TRUE)$data
		  sink()
		  sink(con, append=TRUE)
		  sink(con, append=TRUE, type="message")
		  res02 <- res02[res02$outliers==1,]
		  if (nrow(res02)>0) { res02 <- outlierdata[res02$id,] 
			res02$statistik <- paste("univOutl_LocScaleB_",vb,"_k3",sep="")
		  }
		  res01 <- rbind(res01,res02)
		  rm(res02)
	  }
	  
	  # package: OutlierDetection, function: UnivariateOutlierDetection, method: dist
	  	 res02 <- OutlierDetection::UnivariateOutlierDetection(outlierdata$VALUE,dist=F,dens=F,depth=F,Method="euclidean")$`Location of Outlier`
	  if (length(res02)>0) { 
		res02 <- outlierdata[res02,] 
		res02$statistik <- paste("OutlierDetection_UnivariateOutlierDetection_","dist",sep="")
	  }
	  res01 <- rbind(res01,res02)
	  rm(res02)  
	  
	  # http://www.rdatamining.com/examples/outlier-detection
	    outlierdata$lofactors <- DMwR::lofactor(outlierdata$VALUE,k=2)

	  outlierdata <- outlierdata[order(outlierdata$lofactors,decreasing=T),]
	  res02 <- outlierdata[c(1:5),]
	  res02$lofactors <- NULL
	  res02$statistik <- paste("DMwR_lofactor_2",sep="")
	  
	  res01 <- rbind(res01,res02)
	  rm(res02)
	  
	  res01$period <- cv
	  res01$mean <- round(mean(outlierdata$VALUE),digits=2)
	  res01$startperiod <- as.character(startperiod) 
	  res01$endperiod <- as.character(endperiod)
	  
	  if (dim(res01)[1]>1) {
		  res01 <- res01[-1,]
		  res01$grubbstest <- NA
	  
		  temp01 <- mean(outlierdata$VALUE)
		  
		  temp02 <- res01[res01$VALUE>temp01,]
		  temp03 <- res01[res01$VALUE<temp01,]
		  rm(temp01)
		  if (exists("temp02")) {
		    temp02 <- temp02[order(temp02$VALUE,decreasing=F),]
		    grubbsdata <- outlierdata
		    for (xy in unique(temp02$merge)) {
		      	  res34 <- outliers::grubbs.test(grubbsdata$VALUE,opposite=FALSE)$p.val
		      res01$grubbstest[res01$merge==xy] <- res34
		      grubbsdata <- grubbsdata[grubbsdata$merge!=xy,]
		    }
		    rm(temp02,grubbsdata)
		  }
	 
		  if (exists("temp03")) {
			  temp03 <- temp03[order(temp03$VALUE,decreasing=T),]
			  grubbsdata <- outlierdata
			  for (xy in unique(temp03$merge)) {
			   	res34 <- outliers::grubbs.test(grubbsdata$VALUE,opposite=TRUE)$p.val
			    res01$grubbstest[res01$merge==xy] <- res34
			    grubbsdata <- grubbsdata[grubbsdata$merge!=xy,]
			  }
			  rm(temp03,grubbsdata)
		  }
	  }
	  res00 <- rbind(res00,res01)
	  rm(startperiod)
	  rm(endperiod)
	  rm(outlierdata)
	  rm(res01)
	}

  res00 <- res00[-1,]
  resxx <- rbind(resxx,res00)
  rm(res00)
}

resxx <- resxx[-1,]

filelist=list()

write.table(resxx,paste(respath,"xoutlier_overall.dat",sep=""),sep=";",dec=".",row.names=F)

filelist=append(filelist, paste(respath,"xoutlier_overall.dat",sep=""))

resultat00 <- data.frame(date=as.character(unique(resxx$date)))

#tsdata$ID <- paste(tsdata$SITECODE, tsdata$FIELDNAME, tsdata$OBSERVEDPROPERTY, sep="_")
resxx$ID= paste(resxx$SITECODE, resxx$FIELDNAME, resxx$OBSERVEDPROPERTY, sep="_")
para1loop <- unique(resxx$ID)

for (i in para1loop) {
  loopdata00 <- resxx[resxx$ID==i,]
  loopdata01 <- aggregate(as.character(loopdata00$date),list(as.character(loopdata00$date)),length)
  colnames(loopdata01) <- c("datum",paste(i,"frequency",sep="_"))
  loopdata01 <- loopdata01[order(loopdata01[,2],decreasing=T),]
  loopdata01$datum <- as.character(loopdata01$datum) 
  resultat00 <- merge(resultat00,loopdata01,by.x="date",by.y="datum",all.x=T) 
}

write.table(resultat00,paste(respath,"xpar_freq.dat",sep=""),sep=";",dec=".",row.names=F)
filelist=append(filelist, paste(respath,"xpar_freq.dat",sep=""))
rm(i,loopdata00,loopdata01,resultat00)

for (i in para1loop) {
  loopdata00 <- resxx[resxx$ID==i,]

  resultat00 <- data.frame(statistik=as.character(unique(resxx$statistik)))
  
  for (y in unique(loopdata00$merge)) {
	loopdata01 <- loopdata00[loopdata00$merge==y,]
	loopdata02 <- aggregate(as.character(loopdata01$statistik),list(as.character(loopdata01$statistik)),length)
	colnames(loopdata02) <- c("statistik","frequency")
	loopdata02$ratio <- round((loopdata02$frequency/(sum(loopdata02$frequency)))*100,digits=1)
	loopdata02$string <- paste(loopdata02$frequency," (",loopdata02$ratio,"%)",sep="")
	loopdata02$frequency <- NULL
	loopdata02$ratio <- NULL
	colnames(loopdata02) <- c("statistik",y)
	  
	resultat00 <- merge(resultat00,loopdata02,by.x="statistik",by.y="statistik",all.x=T)
  }
  resultat00$statistik <- as.character(resultat00$statistik)
  resultat01 <- data.frame(t(resultat00[,c(2:ncol(resultat00))]))
  resultat01$datum <- rownames(resultat01)
  colnames(resultat01)  <- c(resultat00$statistik,"datum")
  write.table(resultat01,paste(respath,"x",gsub(":", "_",gsub("\\/", "", i)),"_statistik_freq.dat",sep=""),sep=";",dec=".",row.names=F)
  filelist=append(filelist, paste(respath,"x",gsub(":", "_",gsub("\\/", "", i)),"_statistik_freq.dat",sep=""))
  rm(resultat00,resultat01,i,y,loopdata00,loopdata01,loopdata02)
}

for (i in para1loop) {
  loopdata00 <- resxx[resxx$ID==i,]
  
  resultat00 <- data.frame(period=as.character(unique(resxx$period)))
  
  for (y in unique(loopdata00$merge)) {
    loopdata01 <- loopdata00[loopdata00$merge==y,]
    loopdata02 <- aggregate(as.character(loopdata01$period),list(as.character(loopdata01$period)),length)
    colnames(loopdata02) <- c("period","frequency")
    loopdata02$ratio <- round((loopdata02$frequency/(sum(loopdata02$frequency)))*100,digits=1)
    loopdata02$string <- paste(loopdata02$frequency," (",loopdata02$ratio,"%)",sep="")
    loopdata02$frequency <- NULL
    loopdata02$ratio <- NULL
    colnames(loopdata02) <- c("period",y)
    
    resultat00 <- merge(resultat00,loopdata02,by.x="period",by.y="period",all.x=T)
  }
  resultat00$period <- as.character(resultat00$period)
  resultat01 <- data.frame(t(resultat00[,c(2:ncol(resultat00))]))
  resultat01$datum <- rownames(resultat01)
  colnames(resultat01)  <- c(resultat00$period,"datum")
  write.table(resultat01,paste(respath,"x",gsub(":", "_",gsub("\\/", "", i)),"_period_freq.dat",sep=""),sep=";",dec=".",row.names=F)
  filelist=append(filelist, paste(respath,"x",gsub(":", "_",gsub("\\/", "", i)),"_period_freq.dat",sep=""))
  rm(resultat00,resultat01,i,y,loopdata00,loopdata01,loopdata02)

}

warnings()

pkgTest("zip")
zipfile=paste(sos_site, "_", gsub("\\/", "", sos_parameter), "_", gsub(" ", "_", sos_startperiod), "_", gsub(" ", "_", sos_endperiod), "_", stat_movingwindows, "_", stat_interval,sep="")

zipfile=gsub("[:\\.-]", "_", zipfile)

zipfile=paste(respath, zipfile, ".zip", sep="")

#print(zipfile)
#print(filelist)
#print (unlist(filelist))
zipr(zipfile, unlist(filelist))

print (paste("ARE WE QUIET ", opt$quiet, sep=""))
print (paste("------------ END ---",Sys.time(),"----------------", sep=""))
file.create(paste(respath, mypid, ".done", sep=""))
sink()
sink(type="message")
if (!opt$quiet) {
	cat(zipfile)
	flush.console()
}
