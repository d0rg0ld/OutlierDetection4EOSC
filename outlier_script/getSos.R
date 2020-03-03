options(encoding = "UTF-8")

getSos <- function(verbose, saveOriginal, sosVersion, binding, responseFormat,
			sosUrl, sos_site, sos_parameter, sos_startperiod, sos_endperiod,
			cachepath, loadCache) {

	pkgTest("remotes")
	pkgTest("52North/sos4R", mode="github", extra="feature/0.4")
	pkgTest("sos4R")
	pkgTest("lubridate")
	
	sos=NULL
	try(sos <- SOS(url = sosUrl, version = sosVersion, verboseOutput = verbose, binding = binding))

	myoffering=NULL
	try(myoffering <- sosOfferings(sos)[[sos_site]])

	sos_startperiod <- as_datetime(sos_startperiod)
	sos_endperiod <- as_datetime(sos_endperiod)

	sos_startperiod_utc <- floor_date(sos_startperiod, unit="month")
	#print(unclass(sos_endperiod))
	#browser()
	sos_endperiod_utc <- (ceiling_date(sos_endperiod, unit="month")-seconds(1))

	#print (paste("sos_startperiod : ",sos_startperiod," sos_endperiod : ",sos_endperiod,sep=""))
	#print (paste("sos_startperiod_utc : ",sos_startperiod_utc," sos_endperiod_utc : ",sos_endperiod_utc,sep=""))
	#print (paste("sos_startperiod_utc : ",sos_startperiod_utc," sos_endperiod_utc : ",sos_endperiod_utc-seconds(1), sep=""))

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
							 featureOfInterest = sos_site,
							 observedProperty = list(sos_parameter),
							 #responseFormat = .responseFormat,
							 eventTime = .eventTime,
							 verbose = FALSE,
							 #verbose = .verbose,
							 saveOriginal = .saveOriginal)
		      save(myGetObservation, file=paste(cache_filename, "myGetObs", sep="_"))
		      print (paste("Finished loading observations from SOS : ", "", sep=""))

		      restab <- t(data.frame(rep(NA,32)))
		      colnames(restab) <- c(    "foi",
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
				res00$phentime_start_year       <- timestart$year+1900
				res00$phentime_end_year         <- timeend$year+1900
				res00$phentime_start_month      <- timestart$mon+1
				res00$phentime_end_month        <- timeend$mon+1
				res00$phentime_start_day        <- timestart$mday
				res00$phentime_end_day          <- timeend$mday
				res00$phentime_start_hour       <- timestart$hour
				res00$phentime_end_hour         <- timeend$hour
				res00$phentime_start_min        <- timestart$min
				res00$phentime_end_min          <- timeend$min
				res00$phentime_start_sec        <- timestart$sec
				res00$phentime_end_sec          <- timeend$sec

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
				res00$restime_start_year        <- timestart$year+1900
				res00$restime_end_year          <- timeend$year+1900
				res00$restime_start_month       <- timestart$mon+1
				res00$restime_end_month         <- timeend$mon+1
				res00$restime_start_day         <- timestart$mday
				res00$restime_end_day           <- timeend$mday
				res00$restime_start_hour        <- timestart$hour
				res00$restime_end_hour          <- timeend$hour
				res00$restime_start_min         <- timestart$min
				res00$restime_end_min           <- timeend$min
				res00$restime_start_sec         <- timestart$sec
				res00$restime_end_sec           <- timeend$sec
				res00$resQual <- toString(myGetObservation[[k]]@resultQuality)
				res00$parameter <- toString(myGetObservation[[k]]@parameter)
				res00$metadata <- toString(myGetObservation[[k]]@metadata)
				#browser()
				restab <- rbind(restab,res00)
		      }
		      #browser()
		      restab <- restab[-1,]
		      restab <- restab[,c(      "foi",
						"obsprop",
						"phentime_start_year",
						"phentime_start_month",
						"phentime_start_day",
						"phentime_start_hour",
						"phentime_start_min",
						"phentime_start_sec",
						"procedure","result")]
			# treat procedure as fieldname, dirty workaround
		      colnames(restab) <- c("SITECODE","OBSERVEDPROPERTY",
						"YEAR","MONTH", "DAY", "HOUR", "MIN", "SEC", 
						"FIELDNAME","VALUE")

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

	tsdata <- restab00
	rownames(tsdata) <- seq(1,nrow(tsdata),1)
	tsdata$VALUE <- as.numeric(tsdata$VALUE)
	return(tsdata)
		
}
