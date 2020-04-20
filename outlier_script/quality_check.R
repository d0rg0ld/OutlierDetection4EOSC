#This work is co-funded by the EOSC-hub project (Horizon 2020) under Grant number 777536.

options(encoding = "UTF-8")


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



library("optparse")
 
option_list = list(
	make_option(c("-e", "--endpoint"), type="character", 
              default="http://ltercwn01.umweltbundesamt.at:8080/cwn.all.sos2/service", 
              help="URL SOS endpoint [default= %default]", metavar="character"),
	make_option(c("-r", "--repourl"), type="character", default="", 
              help="URL DAV REPO endpoint", metavar="character"),
	make_option(c("-u", "--username"), type="character", default="", 
              help="URL DAV REPO username", metavar="character"),
	make_option(c("-c", "--credential"), type="character", default="", 
              help="URL DAV REPO password", metavar="character"),
	make_option(c("-s", "--site"), type="character", default=NULL, 
              help="SOS name of Site", metavar="character"),
	make_option(c("-p", "--parameter"), type="character", default=NULL, 
              help="SOS name of Parameter", metavar="character"),
	make_option(c("-P", "--procedure"), type="character", default=NULL, 
              help="SOS name of Procedure", metavar="character"),
	make_option(c("-O", "--offering"), type="character", default=NULL, 
              help="SOS name of Offering", metavar="character"),
	make_option(c("-y", "--offeringonly"), action="store_true", default=FALSE, 
              help="Use only offering parameter for SOS retrieval"),
	make_option(c("-f", "--from"), type="character", default="2018-01-01T00:00:00+01:00", 
              help="Period Start - Datetime", metavar="character"),
	make_option(c("-t", "--to"), type="character", default="2018-01-31T23:59:59+01:00", 
              help="Period End - Datetime", metavar="character"),
	make_option(c("-w", "--window"), type="integer", default=10, 
              help="Width of moving window in datapoints (integer) [default= %default]", metavar="integer"),
	make_option(c("-i", "--interval"), type="integer", default=10, 
              help="Distance between windows in datapoints (integer) [default= %default]", metavar="integer"),
	make_option(c("-S", "--span"), type="double", default=0.06, 
              help="Span [default= %default]", metavar="double"),
	make_option(c("-m", "--mode"), type="integer", default=1, 
              help="SOS (1) or File based (2) [default= %default]", metavar="integer"),
	make_option(c("-l", "--logfile"), type="character", default="log.txt", 
              help="Log output to file [default= %default]", metavar="character"),
	make_option(c("-o", "--outpathbase"), type="character", default="../results/", 
              help="result directory [default= %default]", metavar="character"),
	make_option(c("-b", "--bufferdir"), type="character", default="../cache/", 
              help="cache directory [default= %default]", metavar="character"),
	make_option(c("-z", "--timestamp"), type="character", default="", 
              help="Init timestamp", metavar="character"),
	make_option(c("-q", "--quiet"), action="store_true", default=FALSE, 
              help="Be quiet [default= %default]"),
	make_option(c("-d", "--directory"), type="character", default=getwd(), 
              help="Working directory of script", metavar="character")
) 
 
opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)


#http://stackoverflow.com/questions/7096989/how-to-save-all-console-output-to-file-in-rfrom https://stackoverflow.com/questions/7096989/how-to-save-all-console-output-to-file-in-r
con <- file(opt$logfile, "a")
devnull <- file("/dev/null", "a")
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")

print (paste("------------ BEGIN ---",Sys.time(),"----------------", sep=""))

setwd(opt$directory)
source(paste(getwd(),"pkgtest.R", sep="/"))

#parameter passing
.sos <- opt$endpoint
sos_site	<- opt$site
sos_procedure	<- opt$procedure
sos_offering	<- opt$offering
sos_parameter	<- opt$parameter
sos_offeringonly	<- opt$offeringonly
sos_startperiod	<- opt$from
sos_endperiod	<- opt$to
stat_movingwindows	<- opt$window
stat_interval		<- opt$interval
stat_span		<- opt$span
type <- opt$mode
dav <- opt$repourl
username = opt$username
password = opt$credential
respath=opt$outpathbase
cachepath=opt$bufferdir
myts=opt$timestamp

##DEBUG OVERRIDE
#.sos <- "https://sensorweb.demo.52north.org/sensorwebtestbed/service" 
#sos_site	<- "ELV-WS2500"
#sos_parameter	<- "AirTemperature"
#sos_startperiod	<- "2015-06-01T00:00:00+01:00"
#sos_endperiod	<- "2015-06-05T23:59:59+01:00"
#
#.sos <- "https://lter-at-sos.sos.cdn.lter-europe.net/service"
#sos_site        <- "LTER_EU_AT_003"
#sos_parameter	<- "http://vocabs.lter-europe.net/EnvThes/22035"
#sos_startperiod	<- "2012-06-01T00:00:00+01:00"
#sos_endperiod	<- "2012-06-30T23:59:59+01:00"
#
## <sos:procedure>/1579860990023/LTER_AT/LTER_EU_AT_003/ATZOEAM0939_TEMPMEAN_TEST3/AIR</sos:procedure>
#
#stat_movingwindows <- 40
#stat_interval <- 14
#type <- 1
#dav <- ""
#username = ""
#password = ""
#myts="20200122_143300"

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

tsdata=""
#browser()
if (type==1) {

  source(paste(getwd(),"getSos.R", sep="/"))
  tsdata=getSos( FALSE, FALSE, sos200_version, "KVP", "http://www.opengis.net/om/2.0",
		.sos, sos_offering, sos_site, sos_parameter, sos_procedure, sos_startperiod, sos_endperiod,
		cachepath, .loadCache, sos_offeringonly, FALSE)
} else {

  source(paste(getwd(),"getFile.R", sep="/"))
  tsdata=getFile(username, password, dav)
}
print(colnames(tsdata))

write.table(tsdata, paste("DUMMYDUMP_", mypid,".tsv", sep=""), sep="\t", row.names=F, col.names=TRUE)

source(paste(getwd(), "outlierAnalysis.R", sep="/"))
filelist=outlierAnalysis(tsdata, stat_movingwindows, stat_interval, stat_span, respath)
print(strsplit(filelist, ";"))
#sys.exit(1)
#warnings()

pkgTest("zip")
zipfile=paste(sos_site, "_", gsub("\\/", "", sos_parameter), "_", gsub(" ", "_", sos_startperiod), "_", gsub(" ", "_", sos_endperiod), "_", stat_movingwindows, "_", stat_interval,sep="")

zipfile=gsub("[:\\.-]", "_", zipfile)

zipfile=paste(respath, zipfile, ".zip", sep="")

#print(zipfile)
#print(filelist)
#print (unlist(filelist))
zipr(zipfile, unlist(strsplit(filelist, ";")))

print (paste("ARE WE QUIET ", opt$quiet, sep=""))
print (paste("------------ END ---",Sys.time(),"----------------", sep=""))
file.create(paste(respath, mypid, ".done", sep=""))
sink()
sink(type="message")
if (!opt$quiet) {
	cat(zipfile)
	flush.console()
}
