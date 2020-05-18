#This work is co-funded by the EOSC-hub project (Horizon 2020) under Grant number 777536.

outlierAnalysis <- function(tsdata, stat_movingwindows, stat_overlap, span_num, respath_final) {

	pkgTest("chron")
	pkgTest("univOutl")
	pkgTest("OutlierDetection")
	pkgTest("DMwR")
	pkgTest("EnvStats")		
	# parameter für die eine ausreißeranalyse durchgeführt werden kann
	paraloop <- unique(tsdata$FIELDNAME)

	# vektor mit den pfaden der ergebnisfiles
	pathres <- NULL

	# tabelle an die die ergebnisse der kommenden schleifen angehängt werden
	#resi00 <- data.frame(t(rep(NA,38)))
	#resi00 <- data.frame(matrix(ncol=38), nrow=0)
	resi00 <- NULL
	
	#colnames(resi00) <- c("timestamp","FIELDNAME","VALUE","temp_resolution","slopediffback","slopediffforward","Nas2","Nas7","slopediffback_grp","slopediffforward_grp",
	#		      "EnvStats_rosnerTest","univOutl_LocScaleB_IDR_k2","univOutl_LocScaleB_IDR_k5","univOutl_LocScaleB_IQR_k2","univOutl_LocScaleB_IQR_k5",
	#		      "univOutl_LocScaleB_Gini_k2","univOutl_LocScaleB_Gini_k5","univOutl_LocScaleB_Qn_k2","univOutl_LocScaleB_Qn_k5",
	#		      "univOutl_LocScaleB_ScaleTau2_k2","univOutl_LocScaleB_ScaleTau2_k5","univOutl_LocScaleB_Sn_k2","univOutl_LocScaleB_Sn_k5",
	#		      "univOutl_LocScaleB_MAD_k2","univOutl_LocScaleB_MAD_k5","DMwR_lofactor_univar_2","DMwR_lofactor_univar_4","DMwR_lofactor_bivar_2",
	#		      "DMwR_lofactor_bivar_4","OutlierDetection_dens_univar","OutlierDetection_dens_bivar","OutlierDetection_depthout_bivar","OutlierDetection_maha_univar",
	#		      "OutlierDetection_maha_bivar","OutlierDetection_nn_univar","OutlierDetection_nn_bivar","OutlierDetection_nnk_univ", "OutlierDetection_nnk_bivar")
	#resi00$timestamp <- chron("01-01-1970","12:00:00",format=c(dates="d-m-y",times="h:m:s"))
	parcnt=0
	for (para in paraloop) {
		# Percentile threshold used in the outlier analysis, default value is 0.05 
		parcnt=parcnt+1
		print (paste("Analysing outliers for parameter", para, ",", parcnt, "out of", length(paraloop)))

		ct_num <- 0.99

		# tabelle mit den werten des definierten parameters
		pardata00 <- tsdata[tsdata$FIELDNAME==para,]
		# aufbereitung des zeitstempels
		pardata00$MONTH <- ifelse(nchar(pardata00$MONTH)==1,paste("0",pardata00$MONTH,sep=""),pardata00$MONTH)
		pardata00$DAY <- ifelse(nchar(pardata00$DAY)==1,paste("0",pardata00$DAY,sep=""),pardata00$DAY)
		pardata00$HOUR <- ifelse(nchar(pardata00$HOUR)==1,paste("0",pardata00$HOUR,sep=""),pardata00$HOUR)
		pardata00$MINUTE <- ifelse(nchar(pardata00$MINUTE)==1,paste("0",pardata00$MINUTE,sep=""),pardata00$MINUTE)
		pardata00$SECOND <- ifelse(nchar(pardata00$SECOND)==1,paste("0",pardata00$SECOND,sep=""),pardata00$SECOND)
		# umwandeln des zeitstempels in chron zeitformat (package: chron)
		pardata00$timestamp <- chron(paste(pardata00$DAY,"-",pardata00$MONTH,"-",pardata00$YEAR,sep=""),paste(pardata00$HOUR,":",pardata00$MINUTE,":",pardata00$SECOND,sep=""),
						 format=c(dates="d-m-y",times="h:m:s"))

		# tabelle mit den spalten die benötigt werden 
		pardata01 <- pardata00[,c("timestamp","FIELDNAME","VALUE")]
		# löschen der NAs
		pardata01 <- pardata01[is.na(pardata01$VALUE)==F,]
		# ordnen nach zeit aufsteigend
		pardata01 <- pardata01[order(pardata01$timestamp,decreasing=F),]
		# nach ordnen laufende nummer als zeilenbeschriftung
		
		rownames(pardata01) <- seq(1,nrow(pardata01),1)

		# berechnung der zeitlichen auflösung der messungen (= messintervall) -> neue spalte mit zeitstempel aber um einen 
		# eintrag verschoben (erster eintrag wird gelöscht)
		pardata01$timeperiod <- c(pardata01$timestamp[-1],NA)
		# zeitliche differenz zwischen 2 aufeinanderfolgende zeitstempeln  (= zeitliche auflösung der messreihe) 
		pardata01$timediff <- as.numeric(pardata01$timestamp-pardata01$timeperiod)
		# zeitliche auflösung umgerechnet in minuten und gerundet auf volle "ganze" minuten
		pardata01$timediff <- round((24*60)*abs(pardata01$timediff),digits=0)
		# umwandeln in faktoren (="gruppen"), bestimmung der häufigkeiten der gruppen und sortieren hinsichtlich der häufigkeiten der gruppen 
		quanttemp <- aggregate(as.character(pardata01$timediff),list(as.character(pardata01$timediff)),length)
		colnames(quanttemp) <- c("timediff_min","freq")
		# ordnen nach häufigkeit (absteigend)
		quanttemp <- quanttemp[order(quanttemp$freq,decreasing=T),]
		# umwandeln der zeitangabe in stunden
		quanttemp$timediff_h <- as.numeric(quanttemp$timediff)/60
		# bestimmung der gruppe mit der größten häufigkeit (= das zeitintervall mit der 
		# größten häufigkeit = zeitliche auflösung der messungen)
		#quanttemp <- quanttemp[length(quanttemp)]
		# löschen der spalten timediff und timeperiod (= werden nicht mehr benötigt)
		pardata01$timediff <- NULL
		pardata01$timeperiod <- NULL

		# damit die analyse nicht zu lange dauert werden nur messwerte der jahre 2012, 2013, 2014 und 2015 verwendet
		pardata01$years <- years(pardata01$timestamp)
		#pardata01 <- pardata01[pardata01$years==2012,]
		nrow(pardata01)
		pardata01$years <- NULL
		# löschen der objekte die nicht mehr benötigt werden
		rm(pardata00)

		# generierung einer vollständige zeitreihe
		# anfang der untersuchten zeitreihe
		start_ts <- min(pardata01$timestamp)
		# ende der untersuchten zeitreihe
		end_ts <- max(pardata01$timestamp)
		# potentielle länge der zeitreihe definiert durch anfang und ende und der jeweiligen zeitlichen auflösung
		# der zeitreihe (= messintervall), welche oben bestimmt wurde

		#print ("ORDER ")
		#print (pardata01$timestamp)
		#print (start_ts)
		#print (end_ts)
		#print (quanttemp[quanttemp$freq==max(quanttemp$freq),"timediff_h"]/24)
		#browser()
		
		ts_full <- data.frame(timestamp=seq.dates(start_ts,end_ts,by=quanttemp[quanttemp$freq==max(quanttemp$freq),"timediff_h"]/24))
		if (sum(hours(ts_full$timestamp)) == 0 & sum(minutes(ts_full$timestamp)) == 0 & sum(seconds(ts_full$timestamp)) == 0)
		  ts_full$timestamp <- chron(ts_full$timestamp, times="00:00:00", format=c(dates="d-m-y",times="h:m:s"))

		#print ("ORDER done")
		# "merge" -> spalte mit der datensatz mit anderem verbunden wird (= primärschlüssel)
		ts_full$merge <- as.character(ts_full$timestamp)
		# löschen nicht mehr benötigter objekte
		rm(start_ts,end_ts)

		# anhängen der messwerte an die gesamte zeitreihe
		pardata02 <- pardata01
		# "merge" -> spalte mit der der datensatz mit anderem verbunden wird (= primärschlüssel)
		pardata02$merge <- as.character(pardata02$timestamp)
		# löschen der spalte (= wird nicht mehr benötigt)
		pardata02$timestamp <- NULL
		# verbinden der potentiellen mit der tatsächlichen zeitreihe
		ts_full <- merge(ts_full,pardata02,by.x="merge",by.y="merge",all.x=T)
		# ordnen der gesamten zeitreihe
		ts_full <- ts_full[order(ts_full$timestamp,decreasing=F),]
		# löschen der spalte (= wird nicht mehr benötigt)
		ts_full$merge <- NULL
		# löschen unnötiger objekte
		rm(pardata02)

		# gesamte zeitreihe (halbstundenwerte)
		ts_full$temp_resolution <- quanttemp[1,"timediff_min"]

		# 1. analyseergebnis: absolute veränderung zum davorliegenden als auch nächsten messwert und ermittlung ob die veränderungen 
		# auffällig sind (95er bzw. 75er perzentile) und ermittlung der anzahl der NAs innerhalb 2er moving windows (+- 2 messwerte bzw- +- 7 messwerte)
		loopres <- data.frame(t(rep(NA,10)))
		colnames(loopres) <- c("timestamp","FIELDNAME","VALUE","temp_resolution","slopediffback","slopediffforward","Nas2","Nas7",
				       "slopediffback_grp","slopediffforward_grp")
		loopres$timestamp <- chron("01-01-70","12:00:00",format=c(dates="d-m-y",times="h:m:s"))
		# vektor mit den zeitlichen auflösungen
		temp_res_loop <- unique(ts_full$temp_resolution)
		# das gehört noch dokumentiert

		for (df in temp_res_loop) {
			# tabelle mit der jeweiligen zeitlichen auflösung
			looptab <- ts_full[ts_full$temp_resolution==df,]
			# ordnen der gesamten zeitreihe
			looptab <- looptab[order(looptab$timestamp,decreasing=F),]
			# vergeben einer fortlaufenden nummer
			rownames(looptab) <- seq(1,nrow(looptab),1)
			# objekte an die die ergebnisse der schleife angehängt werden
			res01 <- NA
			res02 <- NA
			res03 <- NA
			res04 <- NA
			res05 <- NA

			for (nm in seq(2,nrow(looptab),1)) {
			  # zeitliche differenz zwischen den beiden messungen (=zeitliche auflösung) 
			  timedifftemp <- round(looptab[nm,"timestamp"]-looptab[nm-1,"timestamp"],digits=1)
			  # anhängen des ergebnis an res01
			  res01 <- c(res01,timedifftemp)
			  # absolute differenz zum zeitlich davorliegenden messwert
			  slopediffback <- round(looptab[nm,"VALUE"]-looptab[nm-1,"VALUE"],digits=3)
			  # anhängen des ergebnis an res02
			  res02 <- c(res02,slopediffback)
			  # absolute differenz zum zeitlich darauffolgenden messwert
			  slopediffforward <- round(looptab[nm+1,"VALUE"]-looptab[nm,"VALUE"],digits=3)
			  # anhängen des ergebnis an res03
			  res03 <- c(res03,slopediffforward)
			  # ermittlung des relativen anteils der NAs in einem window - bzw. +2 messwerte vom aktuellen messwert (dh messwert +/- 2 messwerte) 
			  if ((nm-2)<0) { low <- 0 } else { low <- nm-2 }
			  NAs2temp <- looptab[c((low):(nm+2)),] 
			  # berechnung des ratios
			  NAs2ratio <- (length(na.exclude(NAs2temp[,"VALUE"])))/nrow(NAs2temp)
			  # anhängen des ergebnis an res04
			  res04 <- c(res04,NAs2ratio)
			  # ermittlung des relativen anteils der NAs in einem window - bzw. +7 messwerte vom aktuellen messwert (dh messwert +/- 7 messwerte) 
			  if ((nm-7)<0) { low <- 0 } else { low <- nm-7 }
			  NAs7temp <- looptab[c((low):(nm+7)),] 
			  # berechnung des ratios
			  NAs7ratio <- (length(na.exclude(NAs7temp[,"VALUE"])))/nrow(NAs7temp)
			  # anhängen des ergebnis an res05
			  res05 <- c(res05,NAs7ratio)
			}
			# check ob zeitliche intervalle zwischen den messungen stimmen
			#print(unique(na.exclude(res01)))
			looptab$slopediffback <- res02
			looptab$slopediffforward <- res03
			looptab$Nas2 <- res04
			looptab$Nas7 <- res05
			#print(nrow(looptab))

			rm(res01,res02,res03,res04,res05,low,NAs2temp,NAs2ratio,NAs7temp,NAs7ratio,
			   timedifftemp,slopediffback,slopediffforward,nm)

			# gruppierung der obsoluten veränderungen zum davorliegenden messwert getrennt nach zu- bzw. abnahme - 5/95er 
			# extreme abweichung 25/75 deutliche abweichung sonst normal und 0 = keine veränderung
			quantile_back <- looptab[is.na(looptab$slopediffback)==F,]
			# klassifizierung der davorliegenden veränderung (getrennt nach positiven und negativen veränderungen) 
			# zuerst davorliegende wert war höher -> dh veränderung ist eine abnahme -> messwert - veränderung = davorliegender messwert
			quantile_back_neg <- quantile_back[quantile_back$slopediffback<0,]
			# 5er perzentile
			neg_05 <- quantile(quantile_back_neg$slopediffback,probs=seq(0,1,0.05))[2]
			# 25er perzentile 
			neg_25 <- quantile(quantile_back_neg$slopediffback,probs=seq(0,1,0.05))[6]
			# zuerst davorliegende wert war niedriger -> dh veränderung ist eine zunahme -> messwert - veränderung = davorliegender messwert
			quantile_back_pos <- quantile_back[quantile_back$slopediffback>0,]
			# 95er perzentile
			pos_95 <- quantile(quantile_back_pos$slopediffback,probs=seq(0,1,0.05))[20]
			# 75er perzentile 
			pos_75 <- quantile(quantile_back_pos$slopediffback,probs=seq(0,1,0.05))[16]
			# hier: gruppierung
			# wenn veränderung 0, dann bleibt alles gleich 
			looptab$slopediffback_grp <- ifelse(looptab$slopediffback==0,"zero change",NA)
			# 3 gruppen: ... extreme >/< 95 perzentil bzw ..... high: zwischen 75 and 95 perzentil, wenn 0 = keine veränderung, sonst normal 
			looptab$slopediffback_grp <- ifelse(looptab$slopediffback<=neg_05,"decrease extreme",looptab$slopediffback_grp)
			looptab$slopediffback_grp <- ifelse(looptab$slopediffback>neg_05&looptab$slopediffback<=neg_25,"decrease high",looptab$slopediffback_grp)
			looptab$slopediffback_grp <- ifelse(looptab$slopediffback>=pos_95,"increase extreme",looptab$slopediffback_grp)
			looptab$slopediffback_grp <- ifelse(looptab$slopediffback<pos_95&looptab$slopediffback>=pos_75,"increase high",looptab$slopediffback_grp)
			looptab$slopediffback_grp <- ifelse(is.na(looptab$slopediffback_grp)==T,"normal",looptab$slopediffback_grp)

			# gruppierung der obsoluten veränderungen zum folgenden messwert getrennt nach zu- bzw. abnahme - 5/95er 
			# extreme abweichung 25/75 deutliche abweichung sonst normal und 0 = keine veränderung
			quantile_forward <- looptab[is.na(looptab$slopediffforward)==F,]
			# klassifizierung der folgenden veränderung (getrennt nach positiven und negativen veränderungen) 
			# zuerst nächster wert war niedriger -> dh veränderung ist eine abnahme -> messwert + veränderung = folgender messwert
			quantile_forward_neg <- quantile_forward[quantile_forward$slopediffforward<0,]
			# 5er perzentile 
			neg_05 <- quantile(quantile_forward_neg$slopediffforward,probs=seq(0,1,0.05))[2]
			# 25er perzentile
			neg_25 <- quantile(quantile_forward_neg$slopediffforward,probs=seq(0,1,0.05))[6]
			# dann nächster wert war höher -> dh veränderung ist eine zunahme -> messwert + veränderung = folgender messwert
			quantile_forward_pos <- quantile_forward[quantile_forward$slopediffforward>0,]
			# 95er perzentile
			pos_95 <- quantile(quantile_forward_pos$slopediffforward,probs=seq(0,1,0.05))[20]
			# 75er perzentile
			pos_75 <- quantile(quantile_forward_pos$slopediffforward,probs=seq(0,1,0.05))[16]
			# wenn veränderung 0, dann bleibt alles gleich 
			looptab$slopediffforward_grp <- ifelse(looptab$slopediffforward==0,"zero change",NA)
			# 3 gruppen: ... extreme >/< 95 perzentil bzw ..... high: zwischen 75 and 95 perzentil, wenn 0 = keine veränderung, sonst normal 
			looptab$slopediffforward_grp <- ifelse(looptab$slopediffforward<=neg_05,"decrease extreme",looptab$slopediffforward_grp)
			looptab$slopediffforward_grp <- ifelse(looptab$slopediffforward>neg_05&looptab$slopediffforward<=neg_25,"decrease high",looptab$slopediffforward_grp)
			looptab$slopediffforward_grp <- ifelse(looptab$slopediffforward>=pos_95,"increase extreme",looptab$slopediffforward_grp)
			looptab$slopediffforward_grp <- ifelse(looptab$slopediffforward<pos_95&looptab$slopediffforward>=pos_75,"increase high",looptab$slopediffforward_grp)
			looptab$slopediffforward_grp <- ifelse(is.na(looptab$slopediffforward_grp)==T,"normal",looptab$slopediffforward_grp)
			#looptab$runnr <- NULL
			#looptab$FIELDNAME <- NULL
			loopres <- rbind(loopres,looptab)
		}

		# löchen der 1. zeile mit NAs
		ts_full <- loopres[-1,]
		# löschen von objekten die nicht mehr benötigt werden
		rm(neg_05,neg_25,pos_75,pos_95,quantile_back,quantile_back_neg,
		   quantile_back_pos,quantile_forward,quantile_forward_neg,quantile_forward_pos,loopres,looptab,df,temp_res_loop)

		# ausreißeranalyse  
		# resultat der analyse: metadaten für jedes zeitfenster (start, ende, pot. anzahl der
		# messwerte, tatsächlich vorhandene anzahl der messwerte, ...)!!! 
		qs_res <- data.frame(t(rep(NA,11)))
		colnames(qs_res) <- c("para","period","start","end","tempres","potlength","actlength","rat",
				      "shapiro_overall","shapiro_red","cov")
		qs_res$start <- chron("01-01-1970","12:00:00",format=c(dates="d-m-y",times="h:m:s"))
		qs_res$end <- chron("01-01-1970","12:00:00",format=c(dates="d-m-y",times="h:m:s"))

		# resultat der analyse: alle ausreißer (definiert durch parameter, wert, zeitstempel und methode)
		res00 <- data.frame(t(rep(NA,9)))
		colnames(res00) <- c("FIELDNAME","VALUE","timestamp","statistik","temp_resolution","period","mean","startperiod","endperiod")
		res00$timestamp <- chron("01-01-1970","12:00:00",format=c(dates="d-m-y",times="h:m:s"))
		res00$startperiod <- chron("01-01-1970","12:00:00",format=c(dates="d-m-y",times="h:m:s"))
		res00$endperiod <- chron("01-01-1970","12:00:00",format=c(dates="d-m-y",times="h:m:s"))

		# resultat der analyse: alle moving windows in denen eine bestimmte methode nicht durchgeührt werden konnte
		res9999 <- data.frame(t(rep(NA,5)))
		colnames(res9999) <- c("FIELDNAME","statistik","period","startperiod","endperiod")
		res9999$startperiod <- chron("01-01-1970","12:00:00",format=c(dates="d-m-y",times="h:m:s"))
		res9999$endperiod <- chron("01-01-1970","12:00:00",format=c(dates="d-m-y",times="h:m:s"))

		# 
		taba <- ts_full[,c("timestamp","FIELDNAME","VALUE")]
		# ordnen nach zeitstempel
		taba <- taba[order(taba$timestamp,decreasing=F),]
		# vergabe einer laufenden nummer
		rownames(taba) <- seq(1,nrow(taba),1)

		# Anzahl der moving windows unter berücksichtigung der zeitlichen überlappung
		# vekor mit allen zeiteinheiten (dh zeitstempel)
		veca <- unique(substr(as.character(taba$timestamp),2,9))
		# berechnung der anzahl der möglichen moving winsows: anzahl der zeitstempel durch länge des moving windows minus der zeitlichen überlappung
		vecb <- length(veca)/(stat_movingwindows-stat_overlap)
		# neuberechung der länge der moving windows, damit sich eine ganze zahl an moving windows ergibt 
		# unterschied ist minimal
		stat_movingwindows_mod <- length(veca)/ceil(vecb)
		# vecb <- length(veca)/(stat_movingwindows_mod)

		# vector mit der laufenden nummern der moving windows 
		sample <- seq(1,ceil(vecb),1)
		# zum testen der analyse werden nicht alle moving windows analysiert
		# zufällige auswahl einer definierten anzahl an moving windows
		rm(veca,vecb)

		for (runb in sample) {
		  print(paste("Processing window", runb, "out of", max(sample)))
		  #print(runb)
		  # tabelle an die die messwerte die als ausreißer bestimmt wurden 
		  # angehängt werden + die methode mit der die analyse durchführt wurde (="statistik")
		  res01 <- data.frame(t(rep(NA,4)))
		  colnames(res01) <- c("FIELDNAME","VALUE","timestamp","statistik")
		  res01$timestamp <- chron("01-01-1970","12:00:00",format=c(dates="d-m-y",times="h:m:s"))

		  # tabelle an die die methoden inkl moving windows, die nicht angewendet werden können angehängt werden
		  res99 <- data.frame(t(rep(NA,2)))
		  colnames(res99) <- c("FIELDNAME","statistik")
		 
		  # anfang des jeweiligen moving windows
		  endperiod <- (min(taba$timestamp)+stat_movingwindows_mod*runb)
		  # ende des jeweiligen moving windows
		  startperiod <- endperiod-stat_movingwindows
			    
		  # potentielle länge des jeweiligen moving windows (dh wenn keine NAs in der zeitreihe sind) definiert durch anfang und ende und
		  # der jeweiligen zeitlichen auflösung der zeitreihe (= messintervall), welche oben bestimmt wurde
		  # da noch anpassen
		  pottimeperiod <- length(seq.dates(startperiod,endperiod,by=quanttemp[1,"timediff_h"]/24))
		  
		  # tabelle mit den werten des jeweiligen moving windows 
		  outlierdata <- taba[taba$timestamp>=startperiod&taba$timestamp<endperiod,]
    
		  if (sum(!is.na(unique(outlierdata$VALUE)))==0) { 
			outlierdata$VALUE <- 9999 
			outlierdata$FIELDNAME <- para
		  }


		  # ornden nach zeitstempels
		  outlierdata <- outlierdata[order(outlierdata$timestamp,decreasing=F),]
		  # vergabe einer fortlaufenden nummer
		  rownames(outlierdata) <- seq(1,nrow(outlierdata),1)
		  # anzahl der zeilen der tabelle (= anzahl der tatsächlich vorhandenen messwerte)
		  acttimeperiod <- length(na.exclude(outlierdata$VALUE))
		  # verhältnis zwischen tatsächlichen und potentiellen messwerten
		  ratio <- round(acttimeperiod/pottimeperiod,digits=2)
		  
		  # entfernen der ausreißer (alle methoden werden ohne ausreißer durchgeführt)
		  outlierdata <- na.exclude(outlierdata)

		  # test auf normalverteilung der messwerte (1. gesamter datensatz mit ausreißern, 2.
		  # reduzierter datensatz (= an den beiden enden werden jeweils 10% der werte entfernt))
		  # damit der einfluss der potentiellen ausreißer im datensatz gering gehalten wird, 
		  # werden die 10% niedrigsten und 10% höchsten werte (=potentielle ausreißer) vom datensatz entfernt
		  number <- round((nrow(outlierdata)*(20/100)/2),digits=0)
		  # sortieren der werte
		  shap_temp <- sort(outlierdata$VALUE,decreasing=T)
		  # entfernen der höchsten werte (10% der gesamten anzahl der werte)
		  shap_temp <- shap_temp[-c(1:number)]
		  # entfernen der niedrigsten werte (10% der gesamten anzahl der werte)
		  shap_temp <- shap_temp[-c((length(shap_temp)-number):length(shap_temp))]
		  # shapiro test auf normalverteilung
		  res991 <- tryCatch(
		    #expression die ausgeführt werden soll
		    { shap_p_overall <- round(shapiro.test(na.exclude(outlierdata$VALUE))$p.value,digits=5) },
		    error = function(cond) { # message(paste("Error shapiro test full data", cond, sep="\n"))
		      res991=data.frame(FIELDNAME="No outliers")
		      if(nrow(outlierdata)>0)
		      	res991 <- data.frame(FIELDNAME=unique(outlierdata$FIELDNAME))
		      res991$statistik <- "Error shapiro test full data"
		      return(res991)
		    }
		  )
		  res99 <- rbind(res99,res991)
		  rm(res991)

		#    shap_p_overall <- round(shapiro.test(na.exclude(outlierdata$VALUE))$p.value,digits=5)
		  
		  res991 <- tryCatch(
		    #expression die ausgeführt werden soll
		    { shap_p_red <- round(shapiro.test(na.exclude(shap_temp))$p.value,digits=5) },
		    error = function(cond) { # message(paste("Error shapiro test reduced data", cond, sep="\n")) 
		      res991=data.frame(FIELDNAME="No outliers")
		      if(nrow(outlierdata)>0)
		      	res991 <- data.frame(FIELDNAME=unique(outlierdata$FIELDNAME))
		      res991$statistik <- "Error shapiro test reduced data"
		      shap_p_red <- 9999
		      return(res991)
		    }
		  )
		  res99 <- rbind(res99,res991)
		  rm(res991,shap_temp)
		  # berechnung des cov
		  cov <- round(sd(na.exclude(outlierdata$VALUE))/abs(mean(na.exclude(outlierdata$VALUE))),digits=2)
		  if (exists("shap_p_overall")==F) { 
			shap_p_overall <- 999999 }
		  if (exists("shap_p_red")==F) { 
			shap_p_red <- 999999 }
 
 		  shap_p_overall_dummy=NA
		  if (exists("shap_p_overall")) 
 		  	shap_p_overall_dummy=shap_p_overall
 		  shap_p_red_dummy=NA
		  if (exists("shap_p_red")) 
 		  	shap_p_red_dummy=shap_p_red
 
		  # !!!resultat der ausreißeranalyse: metadaten des jeweiligen moving windows!!!!
		  qs_01 <- data.frame(para=para,period=runb,start=startperiod,end=endperiod,
				      tempres=quanttemp[1,"timediff_min"],potlength=pottimeperiod,actlength=acttimeperiod,rat=ratio,
				      shapiro_overall=shap_p_overall_dummy,shapiro_red=shap_p_red_dummy,cov=cov)
		  # anhängen an die oben definierte tabelle
		  qs_res <- rbind(qs_res,qs_01)
		  rm(pottimeperiod,acttimeperiod,shap_p_overall,shap_p_red,cov,number,qs_01)
		  
		  # vorausetzung der datensätze: ratio tat/pot werte >=0.5 und anzahl unterschiedlicher
		  # werte >= 40 -> das vor allem weil es zu datensätze mit nur 0 werten kommen kann (zb niederschlag)
		  # und dann hängen sich viele funktionen auf
		   if (ratio<=0.5|(length(unique(outlierdata$VALUE))<=5)) { # print("alles gleich") 
		     } else { 
		     # loess = Fit a polynomial surface determined by one or more numerical predictors, using local fitting
		     # analyse wird sowohl univariate als auch bivariate durchgeführt -
		     # !!!!ACHTUNG: span muss an die anzahl der werte in der tabelle angepaßt werden
		     outlierdata$timestamp_numeric <- as.numeric(outlierdata$timestamp)
		     #print(head(outlierdata$timestamp_numeric))
		     #print(span_num)
		     #span_num=2
			# FIX ME
		     loesstemp <- loess(outlierdata$VALUE~outlierdata$timestamp_numeric,span=span_num)
		     #print(head(loesstemp))
		     outlierdata$timestamp_num <- predict(loesstemp,outlierdata$timestamp_numeric)
		     #print("AFTER LOESS")
		     outlierdata$timestamp_numeric <- NULL

		     #write.table(outlierdata, paste("outlierdata", runb,  sep="_"))

		     rm(loesstemp)
		     
		     # 2. methode: rosner test (=ESD) (univariat)
		     # rosner test -> getestet wird wieviele einer definierte anzahl an messwerten ausreißer sind
		     # k = die anzahl der messwerte die getestet wird - k reicht von 1 bis zur hälfte der messwerte
		     # - k wird durch schleife verändert
		     # ergebnis = anzahl der ausreißer
		     # ergebnis ist immer die anzahl der ausreißer bei einem bestimmten k
		     rosres <- data.frame(t(rep(NA,2)))
		     colnames(rosres) <- c("k","outlier")
		     #rosi = k = anzahl der werte die uf ausreißer getestet wird
		     for (rosi in c(1:floor(nrow(outlierdata)/2))) {
		     rosnertemp <- EnvStats::rosnerTest(outlierdata$VALUE,k=rosi,warn=F)
		     # anzahl der werte die getestet werden
		     resloop <- data.frame(k=rosi)
		     # anzahl der ausreißer
		     resloop$outlier <- rosnertemp$n.outliers
		     rosres <- rbind(rosres,resloop)
		     }
		     rosres <- rosres[-1,]
		     # tabelle mit allen ks (= durchläufen), bei denen mindestens 1 ausreißer gefunden wurde 
		     rosres <- rosres[rosres$outlier>0,]
		     # wenn mindestens 1 ausreißer gefunden wurde, dann wird die maximale anzahl an 
		     # ausreißern gesucht (k nimmt zu, aber ab einem bestimmten k bleibt die anzahl 
		     # der ausreißer gleich) 
		     if (nrow(rosres)>0) { 
		     # alle ks mit der maximale anzahl an ausreißern
		     rosi <- rosres[rosres$outlier==max(rosres$outlier),]
		     # von den maximalen ausreißern das niedigste k
		     rosi <- min(rosi$k)
		     # druchführen des rosnertest mit dem gerade ermittelten ks
		     rosnertemp <- EnvStats::rosnerTest(outlierdata$VALUE,k=rosi,warn=F)
		     # selektion der messwerte die als ausreißer ermittelt wurden
		     rosnertemp <- rosnertemp$all.stats
		     res02 <- outlierdata[rosnertemp$Obs.Num,]
		     res02$statistik <- paste("EnvStats_rosnerTest")
		     res02$timestamp_num <- NULL
		     res01 <- rbind(res01,res02)
		     }
		     rm(rosi,rosnertemp,resloop,rosres)

			# 3. methode: MAD +/- Streuungsparameter
			# package: univOutl, function: LocScaleB, method: alle die möglich sind, k: 2 und 5
			# berechnung ist mittelwert +/- einem streuungsparameter - außerhalb dieses bereichs werden die werte als ausreißer definiert
			# als mittelwert wird der median verwendet weil er robust gegenüber ausreißer ist und dann gibt es die aufgelisteten streuungsparameter 
			# k ist der wert mit dem der jeweilige streuungsparameter multipliziert wird und hat damit einen direkten einfluss auf das 
			# untere und obere limit ab dem ein wert ein ausreißer ist

			for (vb in c("IQR","IDR","MAD","Gini","ScaleTau2","Qn","Sn")) {
			   res02 <- vector()
			   res991 <- tryCatch(
			      { res02 <- univOutl::LocScaleB(outlierdata$VALUE,k=2,method=vb,weights=NULL,id=NULL,exclude=NA,logt=FALSE,return.dataframe=TRUE)$data 
			        res02 <- res02[res02$outliers==1,]
			        if (nrow(res02)>0) { 
				  res02 <- outlierdata[res02$id,] 
			          res02$statistik <- paste("univOutl_LocScaleB_",vb,"_k2",sep="")
			        }
			        res02$timestamp_num <- NULL
			        res01 <- rbind(res01,res02)
			        rm(res02)  
			      },
			      error = function(cond) { #message(paste("Error Error UnivOutl_LocScaleB", cond, sep="\n")) 
				res991 <- data.frame(FIELDNAME=unique(outlierdata$FIELDNAME))
				res991$statistik <- paste("univOutl_LocScaleB_",vb,"_k2",sep="")
				return(res991)
			      }
			    )
			    res99 <- rbind(res99,res991)
			    rm(res991)

			    res02 = vector()
			    res991 <- tryCatch(
			      { res02 <- univOutl::LocScaleB(outlierdata$VALUE,k=5,method=vb,weights=NULL,id=NULL,exclude=NA,logt=FALSE,return.dataframe=TRUE)$data 
			        res02 <- res02[res02$outliers==1,]
			        if (nrow(res02)>0) { 
				  res02 <- outlierdata[res02$id,] 
			          res02$statistik <- paste("univOutl_LocScaleB_",vb,"_k5",sep="")
			        }
			        res02$timestamp_num <- NULL
			        res01 <- rbind(res01,res02)
			        rm(res02) 
			      },
			      error = function(cond) { #message(paste("Error Error UnivOutl_LocScaleB", cond, sep="\n")) 
				res991 <- data.frame(FIELDNAME=unique(outlierdata$FIELDNAME))
				res991$statistik <- paste("univOutl_LocScaleB_",vb,"_k5",sep="")
				return(res991)
			      }
			    )
			    res99 <- rbind(res99,res991)
			    rm(res991)
			 }
			  rm(vb)


		  # 4. methode: This function obtain local outlier factors using the LOF algorithm. 
		  # ... given a data set it produces a vector of local outlier factors (=value shows "outlierness" of each value) for each case
		  # analyse wird mit unterschiedlichen ks durchgeführt 
		  # (k = The number of neighbours that will be used in the calculation of the local outlier factors.)
		  # eine klassifikation in ausreißer oder nicht-ausreißer (0/1) wird nicht durchgeführt
		  # die 5 messwerte mit den höchsten werten wird ausgewählt -> das könnten wir noch verbessern
		  # http://www.rdatamining.com/examples/outlier-detection
		  
		  outlierdata <- outlierdata[order(outlierdata$timestamp,decreasing=F),]
		  outlierdata$lofactors <- DMwR::lofactor(outlierdata[,c("VALUE")],k=2)
		  outlierdata <- outlierdata[order(outlierdata$lofactors,decreasing=T),]
		  res02 <- outlierdata[outlierdata$lofactors!=Inf,]
		  res02 <- outlierdata[c(1:5),]
		  res02$lofactors <- NULL
		  res02$statistik <- paste("DMwR_lofactor_univar_2",sep="")
		  outlierdata$lofactors <- NULL
		  res02$timestamp_num <- NULL
		  res01 <- rbind(res01,res02)
		  rm(res02)
		  
		  outlierdata <- outlierdata[order(outlierdata$timestamp,decreasing=F),]
		  outlierdata$lofactors <- DMwR::lofactor(outlierdata[,c("timestamp_num","VALUE")],k=2)
		  outlierdata <- outlierdata[order(outlierdata$lofactors,decreasing=T),]
		  res02 <- outlierdata[outlierdata$lofactors!=Inf,]
		  res02 <- outlierdata[c(1:5),]
		  res02$lofactors <- NULL
		  res02$statistik <- paste("DMwR_lofactor_bivar_2",sep="")
		  outlierdata$lofactors <- NULL
		  res02$timestamp_num <- NULL
		  res01 <- rbind(res01,res02)
		  rm(res02)

		  outlierdata <- outlierdata[order(outlierdata$timestamp,decreasing=F),]
		  outlierdata$lofactors <- DMwR::lofactor(outlierdata[,c("VALUE")],k=4)
		  outlierdata <- outlierdata[order(outlierdata$lofactors,decreasing=T),]
		  res02 <- outlierdata[outlierdata$lofactors!=Inf,]
		  res02 <- outlierdata[c(1:5),]
		  res02$lofactors <- NULL
		  res02$statistik <- paste("DMwR_lofactor_univar_4",sep="")
		  outlierdata$lofactors <- NULL
		  res02$timestamp_num <- NULL
		  res01 <- rbind(res01,res02)
		  rm(res02)
		  
		  outlierdata <- outlierdata[order(outlierdata$timestamp,decreasing=F),]
		  outlierdata$lofactors <- DMwR::lofactor(outlierdata[,c("timestamp_num","VALUE")],k=4)
		  outlierdata <- outlierdata[order(outlierdata$lofactors,decreasing=T),]
		  res02 <- outlierdata[outlierdata$lofactors!=Inf,]
		  res02 <- outlierdata[c(1:5),]
		  res02$lofactors <- NULL
		  res02$statistik <- paste("DMwR_lofactor_bivar_4",sep="")
		  outlierdata$lofactors <- NULL
		  res02$timestamp_num <- NULL
		  res01 <- rbind(res01,res02)
		  #print("lofactor")
		  rm(res02)

		  # 5. methode: OutlierDetection::dens - Takes a dataset and finds its outliers using Robust Kernal-based Outlier Factor(RKOF) algorithm
		  # auch hier gibt ein k das man definieren kann -> default einstellungen
		  # univariate
		  #windows(40,30)
		  outlierdata <- outlierdata[order(outlierdata$timestamp,decreasing=F),]
		  res02 = vector()
		  res991 <- tryCatch(
		    { res02 <- OutlierDetection::dens(outlierdata["VALUE"],rnames=T,cutoff=ct_num)$`Location of Outlier` },
		    error = function(cond) { #message(paste("Error OutlierDetection_dens_univar", cond, sep="\n")) 
		      res991=data.frame(FIELDNAME="No outliers")
		      if(nrow(outlierdata)>0)
		        res991 <- data.frame(FIELDNAME=unique(outlierdata$FIELDNAME))
		      res991$statistik <- "Error OutlierDetection_dens_univar"
		      return(res991)
		    }
		  )
		  res99 <- rbind(res99,res991)
		  rm(res991)
		  # figure
		  #plot(outlierdata[,"timestamp"],outlierdata[,"VALUE"],type="p",pch=16,col="black")
		  #points(outlierdata[res02,"timestamp"],outlierdata[res02,"VALUE"],type="p",pch=16,col="red")
		  #lines(outlierdata[,"timestamp"],outlierdata[,"timestamp_num"],col="red")
		  #savePlot(paste(respath,para,runb,"OutlierDetection_dens_univar",".pdf",sep=""),type="pdf")
		  #dev.off()
		  # anhängen an schleifentabelle
		  if (length(res02)>0) { res02 <- outlierdata[res02,] 
		  res02$statistik <- paste("OutlierDetection_dens_univar",sep="")
		  res02$timestamp_num <- NULL
		   }
		  res01 <- rbind(res01,res02)
		  rm(res02)

		  # bivariate
		  #windows(40,30)
		  outlierdata <- outlierdata[order(outlierdata$timestamp,decreasing=F),]
		  res02 = vector()
		  res991 <- tryCatch(
		    { res02 <- OutlierDetection::dens(outlierdata[,c("timestamp_num","VALUE")],rnames=T,cutoff=ct_num)$`Location of Outlier` },
		    error = function(cond) {  #message(paste("Error OutlierDetection_dens_bivar", cond, sep="\n")) 
		      res991=data.frame(FIELDNAME="No outliers")
		      if(nrow(outlierdata)>0)
		        res991 <- data.frame(FIELDNAME=unique(outlierdata$FIELDNAME))
		      res991$statistik <- "Error OutlierDetection_dens_bivar"
		      return(res991)
		    }
		  )
		  res99 <- rbind(res99,res991)
		  rm(res991)
		  # figure
		  #plot(outlierdata[,"timestamp"],outlierdata[,"VALUE"],type="p",pch=16,col="black")
		  #points(outlierdata[res02,"timestamp"],outlierdata[res02,"VALUE"],type="p",pch=16,col="red")
		  #lines(outlierdata[,"timestamp"],outlierdata[,"timestamp_num"],col="red")
		  #savePlot(paste(respath,para,runb,"OutlierDetection_dens_bivar",".pdf",sep=""),type="pdf")
		  #dev.off()
		  # anhängen an schleifentabelle
		  if (length(res02)>0) { res02 <- outlierdata[res02,] 
		  res02$statistik <- paste("OutlierDetection_dens_bivar",sep="")
		  res02$timestamp_num <- NULL
		  }
		  res01 <- rbind(res01,res02)
		  #print("dens")
		  rm(res02)

		  # 6. methode: OutlierDetection::depthout - Takes a dataset and finds its outliers using depth-based method
		  # nur bivariat
		  # bivariate
		  #windows(40,30)
		  outlierdata <- outlierdata[order(outlierdata$timestamp,decreasing=F),]
		  res02 = vector()
		  res991 <- tryCatch(
		    { res02 <- OutlierDetection::depthout(outlierdata[,c("timestamp_num","VALUE")],rnames=T,cutoff=1-ct_num)$`Location of Outlier` },
		    error = function(cond) { #message(paste("Error OutlierDetection_depthout_bivar", cond, sep="\n")) 
		      res991=data.frame(FIELDNAME="No outliers")
		      if(nrow(outlierdata)>0)
		        res991 <- data.frame(FIELDNAME=unique(outlierdata$FIELDNAME))
		      res991$statistik <- "Error OutlierDetection_depthout_bivar"
		      return(res991)
		    }
		  )
		  dev.off()
		  res99 <- rbind(res99,res991)
		  rm(res991)
		  # figure
		  #plot(outlierdata[,"timestamp"],outlierdata[,"VALUE"],type="p",pch=16,col="black")
		  #points(outlierdata[res02,"timestamp"],outlierdata[res02,"VALUE"],type="p",pch=16,col="red")
		  #lines(outlierdata[,"timestamp"],outlierdata[,"timestamp_num"],col="red")
		  #savePlot(paste(respath,para,runb,"OutlierDetection_depthout_bivar",".pdf",sep=""),type="pdf")
		  #dev.off()
		  # anhängen an schleifentabelle
		  if (length(res02)>0) { res02 <- outlierdata[res02,] 
		  res02$statistik <- paste("OutlierDetection_depthout_bivar",sep="")
		  res02$timestamp_num <- NULL
		  }
		  res01 <- rbind(res01,res02)
		  #print("depthout")
		  rm(res02)

		  # 7. ausreißeranalyse: OutlierDetection::maha - Takes a dataset and finds its outliers using modelbased method
		  
		  # univariate
		  #windows(40,30)
		  outlierdata <- outlierdata[order(outlierdata$timestamp,decreasing=F),]
		  res02 = vector()
		  res991 <- tryCatch(
		    { res02 <- OutlierDetection::maha(outlierdata["VALUE"],rnames=F,cutoff=ct_num)$`Location of Outlier` },
		    error = function(cond) { #message(paste("Error OutlierDetection_maha_univar", cond, sep="\n")) 
		      res991=data.frame(FIELDNAME="No outliers")
		      if(nrow(outlierdata)>0)
		        res991 <- data.frame(FIELDNAME=unique(outlierdata$FIELDNAME))
		      res991$statistik <- "Error OutlierDetection_maha_univar"
		      return(res991)
		    }
		  )
		  res99 <- rbind(res99,res991)
		  rm(res991)
		  # figure
		  #plot(outlierdata[,"timestamp"],outlierdata[,"VALUE"],type="p",pch=16,col="black")
		  #points(outlierdata[res02,"timestamp"],outlierdata[res02,"VALUE"],type="p",pch=16,col="red")
		  #lines(outlierdata[,"timestamp"],outlierdata[,"timestamp_num"],col="red")
		  #savePlot(paste(respath,para,runb,"OutlierDetection_maha_univar",".pdf",sep=""),type="pdf")
		  #dev.off()
		  # anhängen an schleifentabelle
		  if (length(res02)>0) { res02 <- outlierdata[res02,] 
		  res02$statistik <- paste("OutlierDetection_maha_univar",sep="")
		  res02$timestamp_num <- NULL
		  }
		  res01 <- rbind(res01,res02)
		  rm(res02)
		  
		  # bivariate
		  #windows(40,30)
		  outlierdata <- outlierdata[order(outlierdata$timestamp,decreasing=F),]
		  res02 = vector()
		  res991 <- tryCatch(
		    { res02 <- OutlierDetection::maha(outlierdata[,c("timestamp_num","VALUE")],rnames=T,cutoff=ct_num)$`Location of Outlier` },
		    error = function(cond) { #message(paste("Error OutlierDetection_maha_bivar", cond, sep="\n")) 
		      res991=data.frame(FIELDNAME="No outliers")
		      if(nrow(outlierdata)>0)
		        res991 <- data.frame(FIELDNAME=unique(outlierdata$FIELDNAME))
		      res991$statistik <- "Error OutlierDetection_maha_bivar"
		      return(res991)
		    }
		  )
		  res99 <- rbind(res99,res991)
		  rm(res991)
		  # figure
		  #plot(outlierdata[,"timestamp"],outlierdata[,"VALUE"],type="p",pch=16,col="black")
		  #points(outlierdata[res02,"timestamp"],outlierdata[res02,"VALUE"],type="p",pch=16,col="red")
		  #lines(outlierdata[,"timestamp"],outlierdata[,"timestamp_num"],col="red")
		  #savePlot(paste(respath,para,runb,"OutlierDetection_maha_bivar",".pdf",sep=""),type="pdf")
		  #dev.off()
		  # anhängen an schleifentabelle
		  if (length(res02)>0) { res02 <- outlierdata[res02,] 
		  res02$statistik <- paste("OutlierDetection_maha_bivar",sep="")
		  res02$timestamp_num <- NULL
		  }
		  res01 <- rbind(res01,res02)
		  #print("maha")
		  rm(res02)
		  
		  # 8. ausreißeranalyse: OutlierDetection::nn 
		  
		  # univariate
		  #windows(40,30)
		  outlierdata <- outlierdata[order(outlierdata$timestamp,decreasing=F),]
		  res02 = vector()
		  res991 <- tryCatch(
		    { res02 <- OutlierDetection::nn(outlierdata["VALUE"],rnames=F,cutoff=ct_num)$`Location of Outlier` },
		    error = function(cond) { #message(paste("Error OutlierDetection_nn_univar", cond, sep="\n")) 
		      res991=data.frame(FIELDNAME="No outliers")
		      if(nrow(outlierdata)>0)
		        res991 <- data.frame(FIELDNAME=unique(outlierdata$FIELDNAME))
		      res991$statistik <- "Error OutlierDetection_nn_univar"
		      return(res991)
		    }
		  )
		  res99 <- rbind(res99,res991)
		  rm(res991)
		  # figure
		  #plot(outlierdata[,"timestamp"],outlierdata[,"VALUE"],type="p",pch=16,col="black")
		  #points(outlierdata[res02,"timestamp"],outlierdata[res02,"VALUE"],type="p",pch=16,col="red")
		  #lines(outlierdata[,"timestamp"],outlierdata[,"timestamp_num"],col="red")
		  #savePlot(paste(respath,para,runb,"OutlierDetection_nn_univar",".pdf",sep=""),type="pdf")
		  #dev.off()
		  # anhängen an schleifentabelle
		  if (length(res02)>0) { res02 <- outlierdata[res02,] 
		  res02$statistik <- paste("OutlierDetection_nn_univar",sep="")
		  res02$timestamp_num <- NULL
		  }
		  res01 <- rbind(res01,res02)
		  rm(res02)
		  
		  # bivariate
		  #windows(40,30)
		  outlierdata <- outlierdata[order(outlierdata$timestamp,decreasing=F),]
		  res02 = vector()
		  res991 <- tryCatch(
		    { res02 <- OutlierDetection::nn(outlierdata[,c("timestamp_num","VALUE")],rnames=F,cutoff=ct_num)$`Location of Outlier` },
		    error = function(cond) { #message(paste("Error OutlierDetection_nn_bivar", cond, sep="\n")) 
		      res991=data.frame(FIELDNAME="No outliers")
		      if(nrow(outlierdata)>0)
		        res991 <- data.frame(FIELDNAME=unique(outlierdata$FIELDNAME))
		      res991$statistik <- "Error OutlierDetection_nn_bivar"
		      return(res991)
		    }
		  )
		  res99 <- rbind(res99,res991)
		  rm(res991)
		  # figure
		  #plot(outlierdata[,"timestamp"],outlierdata[,"VALUE"],type="p",pch=16,col="black")
		  #points(outlierdata[res02,"timestamp"],outlierdata[res02,"VALUE"],type="p",pch=16,col="red")
		  #lines(outlierdata[,"timestamp"],outlierdata[,"timestamp_num"],col="red")
		  #savePlot(paste(respath,para,runb,"OutlierDetection_nn_bivar",".pdf",sep=""),type="pdf")
		  #dev.off()
		  # anhängen an schleifentabelle
		  if (length(res02)>0) { res02 <- outlierdata[res02,] 
		  res02$statistik <- paste("OutlierDetection_nn_bivar",sep="")
		  res02$timestamp_num <- NULL
		  }
		  res01 <- rbind(res01,res02)
		  #print("nn")
		  rm(res02)
		  
		  # 9. methode: OutlierDetection::nnk 
		  
		  # univariate
		  #windows(40,30)
		  outlierdata <- outlierdata[order(outlierdata$timestamp,decreasing=F),]
		  res02 = vector()
		  res991 <- tryCatch(
		    { res02 <- OutlierDetection::nnk(outlierdata[c("VALUE")],rnames=T,cutoff=ct_num)$`Location of Outlier` },
		    error = function(cond) { #message(paste("Error OutlierDetection_nnk_univ", cond, sep="\n")) 
		      res991=data.frame(FIELDNAME="No outliers")
		      if(nrow(outlierdata)>0)
		        res991 <- data.frame(FIELDNAME=unique(outlierdata$FIELDNAME))
		      res991$statistik <- "Error OutlierDetection_nnk_univ"
		      return(res991)
		    }
		  )
		  res99 <- rbind(res99,res991)
		  rm(res991)
		  # figure
		  #plot(outlierdata[,"timestamp"],outlierdata[,"VALUE"],type="p",pch=16,col="black")
		  #points(outlierdata[res02,"timestamp"],outlierdata[res02,"VALUE"],type="p",pch=16,col="red")
		  #lines(outlierdata[,"timestamp"],outlierdata[,"timestamp_num"],col="red")
		  #savePlot(paste(respath,para,runb,"OutlierDetection_nnk_univar",".pdf",sep=""),type="pdf")
		  #dev.off()
		  # anhängen an schleifentabelle
		  if (length(res02)>0) { res02 <- outlierdata[res02,] 
		  res02$statistik <- paste("OutlierDetection_nnk_univ",sep="")
		  res02$timestamp_num <- NULL
		  }
		  res01 <- rbind(res01,res02)
		  rm(res02)  
		  
		  # bivariate
		  #windows(40,30)
		  outlierdata <- outlierdata[order(outlierdata$timestamp,decreasing=F),]
		  res02 = vector()
		  res991 <- tryCatch(
		    { res02 <- OutlierDetection::nnk(outlierdata[,c("timestamp_num","VALUE")],rnames=F,cutoff=ct_num)$`Location of Outlier` },
		    error = function(cond) { #message(paste("Error OutlierDetection_nnk_bivar", cond, sep="\n")) 
		      res991=data.frame(FIELDNAME="No outliers")
		      if(nrow(outlierdata)>0)
		        res991 <- data.frame(FIELDNAME=unique(outlierdata$FIELDNAME))
		      res991$statistik <- "Error OutlierDetection_nnk_bivar"
		      return(res991)
		    }
		  )
		  res99 <- rbind(res99,res991)
		  rm(res991)
		  # figure
		  #plot(outlierdata[,"timestamp"],outlierdata[,"VALUE"],type="p",pch=16,col="black")
		  #points(outlierdata[res02,"timestamp"],outlierdata[res02,"VALUE"],type="p",pch=16,col="red")
		  #lines(outlierdata[,"timestamp"],outlierdata[,"timestamp_num"],col="red")
		  #savePlot(paste(respath,para,runb,"OutlierDetection_nnk_bivar",".pdf",sep=""),type="pdf")
		  #dev.off()
		  # anhängen an schleifentabelle
		  if (length(res02)>0) { res02 <- outlierdata[res02,] 
		  res02$statistik <- paste("OutlierDetection_nnk_bivar",sep="")
		  res02$timestamp_num <- NULL
		  }
		  res01 <- rbind(res01,res02)
		  #print("nnk")
		  rm(res02)  

		  # zusätzlich ,metainformation (zeitliche auflösung, laufende nummer des moving windows, mittelwert des moving windows,
		  # beginn und ende des moving windows)
		  res01$temp_resolution <- quanttemp[1,"timediff_min"]
		  res01$period <- runb
		  res01$mean <- round(mean(outlierdata$VALUE),digits=2)
		  res01$startperiod <- startperiod 
		  res01$endperiod <- endperiod
		  res00 <- rbind(res00,res01)
		  
		  # anhängen der info welche methoden nicht druchgeführt werden konnten an die schleifentabelle
		  if (nrow(res99)>1) {
		  res99$period <- runb
		  res99$startperiod <- startperiod 
		  res99$endperiod <- endperiod
		  res99 <- res99[substr(res99$statistik,1,5)=="Error",]
		  res9999 <- rbind(res9999,res99)
		  }
		   }
		  # löschen von objekten die nicht mehr benötigt werden
		  rm(runb)
		  rm(ratio)
		  rm(startperiod)
		  rm(endperiod)
		  rm(outlierdata)
		  rm(res01)
		  rm(res99)
		}
		res00 <- res00[-1,]
		res9999 <- res9999[is.na(res9999$FIELDNAME)==F,]
		qs_res <- qs_res[-1,]
		
		para4file <- gsub("[ /:\\.;]", "_", para)

		# exportieren der ergebnisse
		write.table(res00,paste(respath_final,para4file,"_outliers.dat",sep=""),sep=";",dec=".",row.names=F)
		write.table(res9999,paste(respath_final,para4file,"_method_notworking.dat",sep=""),sep=";",dec=".",row.names=F)
		write.table(qs_res,paste(respath_final,para4file,"_movingwindows_metadata.dat",sep=""),sep=";",dec=".",row.names=F)

		ts_full_temp <- ts_full

		tempx=NULL
		# erstellung einer tabelle mit den häufigkeiten  
		if (nrow(res9999)>0) {
		  errorloop <- data.frame(t(rep(NA,3)))
		  colnames(errorloop) <- c("timestamp","FIELDNAME","statistik")
		  errorloop$timestamp <- chron("01-01-1970","12:00:00",format=c(dates="d-m-y",times="h:m:s"))

		  for (err in c(1:nrow(res9999))) {
		    errstart <- res9999[err,"startperiod"]
		    errend <- res9999[err,"endperiod"]
		    errortemp <- ts_full_temp[ts_full_temp$timestamp>=errstart&ts_full_temp$timestamp<=errend,]
		    errortemp <- errortemp[,c("timestamp","FIELDNAME")]
		    errortemp$statistik <- res9999[err,"statistik"]
		    errorloop <- rbind(errorloop,errortemp)
		  }
		  errorloop <- errorloop[-1,]
		  errorloop$statistik <- substr(errorloop$statistik,7,99)
		  tempx <- aggregate(errorloop$timestamp,list(paste(errorloop$timestamp,errorloop$statistik,sep="_")),length)
		  colnames(tempx) <- c("timestamp_statistik","N")
		  tempx$N <- tempx$N*-1
		  tempx$timestamp <- substr(tempx$timestamp_statistik,1,19)
		  tempx$statistik <- substr(tempx$timestamp_statistik,21,99)
		  tempx$timestamp_statistik <- NULL
		}
			
		rm(errortemp,err,errorloop,errstart,errend) 

		# anhängen der ermittelten ausreißer an die gesamte zeitreihe - getrennt nach den angewandten methoden
		ts_full_temp$merge <- as.character(ts_full_temp$timestamp)
		res00 <- res00[is.na(res00$statistik)==F,]
		meth_loop <- c("EnvStats_rosnerTest","univOutl_LocScaleB_IDR_k2","univOutl_LocScaleB_IDR_k5","univOutl_LocScaleB_IQR_k2","univOutl_LocScaleB_IQR_k5",
			       "univOutl_LocScaleB_Gini_k2","univOutl_LocScaleB_Gini_k5","univOutl_LocScaleB_Qn_k2","univOutl_LocScaleB_Qn_k5",
			       "univOutl_LocScaleB_ScaleTau2_k2","univOutl_LocScaleB_ScaleTau2_k5","univOutl_LocScaleB_Sn_k2","univOutl_LocScaleB_Sn_k5",
			       "univOutl_LocScaleB_MAD_k2","univOutl_LocScaleB_MAD_k5","DMwR_lofactor_univar_2","DMwR_lofactor_univar_4","DMwR_lofactor_bivar_2",
			       "DMwR_lofactor_bivar_4","OutlierDetection_dens_univar","OutlierDetection_dens_bivar","OutlierDetection_depthout_bivar","OutlierDetection_maha_univar",
			       "OutlierDetection_maha_bivar","OutlierDetection_nn_univar","OutlierDetection_nn_bivar","OutlierDetection_nnk_univ", "OutlierDetection_nnk_bivar")                       

		for (jh in meth_loop) {
		  temp <- res00[res00$statistik==jh,]
		  temp <- temp[,c("timestamp","statistik")]
		  if (nrow(temp)==0) { 
		    ts_full_temp$temp <- NA
		    colnames(ts_full_temp)[ncol(ts_full_temp)] <- jh 
		    } else {
		  temp$timestamp <- as.character(temp$timestamp)
		  tempo <- aggregate(temp$timestamp,list(temp$timestamp),length)
		  colnames(tempo) <- c("timestamp",jh)
		  ts_full_temp <- merge(ts_full_temp,tempo,by.x="merge",by.y="timestamp",all.x=T)
		rm(tempo,temp)
		}
		}
		rm(jh,meth_loop)

		# anhängen der information welche methoden nicht durchgefhrt werden konnten an die gesamte zeitreihe
		if (!is.null(tempx) && nrow(tempx)>0) {
		errorloop <- unique(tempx$statistik)
    #browser()
		for (err in errorloop) {
		  errortemp <- tempx[tempx$statistik==err,]
		  ts_full_temp <- merge(ts_full_temp,errortemp,by.x="merge",by.y="timestamp",all.x=T)
		  ts_full_temp[,unique(na.exclude(ts_full_temp$statistik))] <- ifelse(is.na(ts_full_temp$N)==F,ts_full_temp$N,ts_full_temp[,unique(na.exclude(ts_full_temp$statistik))])
		  ts_full_temp$N <- NULL
		  ts_full_temp$statistik <- NULL
		  rm(errortemp)
		  }
		}
		rm(errorloop,err,tempx)
		ts_full_temp$merge <- NULL
		ts_full_temp$N <- NULL
		ts_full_temp$statistik <- NULL
		# exportieren
		write.table(ts_full_temp,paste(respath_final,para4file,"_timeseries_plusoutlier.dat",sep=""),sep=";",dec=".",row.names=F,na = "")
		# exportieren der gesamten zeitreihe aller parameter
		#print("BEFORE RESI00 rbind")
		if (is.null(resi00)) {
			resi00=ts_full_temp
		} else {
			resi00 <- rbind(resi00,ts_full_temp)
		}
		#resi00 <- rbind(resi00,ts_full_temp)
		#print("AFTER RESI00 rbind")
		# erstellen eines vektors mit den pfaden aller ergebnisdateien 
		#pathvector <- paste(paste(respath,para4file,"_outliers.dat",sep=""),";",paste(respath,para4file,"_method_notworking.dat",sep=""),";",
		#		    paste(respath,para4file,"_movingwindows_metadata.dat",sep=""),";",paste(respath,para4file,"_timeseries_plusoutlier.dat",sep=""),sep="")
		pathvector <- c(paste(respath_final,para4file,"_outliers.dat",sep=""),paste(respath_final,para4file,"_method_notworking.dat",sep=""),
				    paste(respath_final,para4file,"_movingwindows_metadata.dat",sep=""),paste(respath_final,para4file,"_timeseries_plusoutlier.dat",sep=""))
		pathres <- c(pathres,pathvector)
		# löschen nicht mehr benötigter objekte
		rm(res00,res9999,qs_res,ts_full_temp,para,quanttemp,ts_full,taba,pardata01)
	}
	print(warnings())
	total_outfile=paste(respath_final,"total_timeseries_plusoutlier.dat",sep="")
	write.table(resi00, total_outfile, sep=";",dec=".",row.names=F,na = "")
	rm(sample,pathvector,resi00,paraloop,homepath,respath_final,ct_num,span_num,stat_movingwindows,stat_movingwindows_mod,stat_overlap)
	pathres=c(total_outfile, pathres)
	return(pathres)
}

