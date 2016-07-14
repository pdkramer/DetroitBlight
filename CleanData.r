#Read data files and determine factors
dParcels <- read.csv("..\\data\\Parcel-Points.csv", stringsAsFactors = F)
dParcels <- dplyr::filter(dParcels, !is.na(dParcels$Ward))
dParcels$SalePrice <- as.numeric(gsub("\\$|,","",dParcels$SalePrice))
dParcels$SEV <- as.numeric(gsub("\\$|,","",dParcels$SEV))
dParcels$AV <- as.numeric(gsub("\\$|,","",dParcels$AV))
dParcels$TV <- as.numeric(gsub("\\$|,","",dParcels$TV))
dParcels$Latitude <- as.numeric(dParcels$Latitude)
dParcels$Longitude <- as.numeric(dParcels$Longitude)
dParcels$TaxStatus <- as.factor(dParcels$TaxStatus)
dParcels$IsImproved <- as.factor(dParcels$IsImproved)

d311 <- read.csv("..\\data\\311.csv", stringsAsFactors = F)
d311$acknowledged_at <- as.Date(d311$acknowledged_at, "%m/%d/%Y" )
d311$issue_type <- as.factor(d311$issue_type)
d311$ticket_status <- as.factor(d311$ticket_status)
d311$rating <- as.factor(d311$rating)

#Blight violations downloaded from Detroit Open Data to get valid dates
dViol <- read.csv("..\\data\\Blight-Violations.csv", stringsAsFactors = F)
dViol$TicketIssuedDT <- as.Date(dViol$TicketIssuedDT, "%m/%d/%Y" )
dViol <- dViol %>% filter(TicketIssuedDT > "2010-12-31") #remove data outside other data timeframes
dViol$Lat <- as.numeric(Get_Lat_Lon(dViol$ViolationLocation)[,1]) #Get_Lat_Lon is in ParseLatLon.r
dViol$Lon <- as.numeric(Get_Lat_Lon(dViol$ViolationLocation)[,2])
dViol <- dViol %>% filter(!is.na(dViol$Lat)) #now remove violations we cannot locate
dViol$AgencyName <- as.factor(dViol$AgencyName)
dViol$ViolationCode <- as.factor(dViol$ViolationCode)
dViol$Disposition <- as.factor(dViol$Disposition)
dViol$PaymentStatus <- as.factor(dViol$PaymentStatus)
dViol$ViolationCategory <- as.factor(dViol$ViolationCategory)
dViol$HearingDT <- as.Date(dViol$HearingDT, "%m/%d/%Y" )
dViol$FineAmt <- as.numeric(gsub("\\$|,","",dViol$FineAmt))
dViol$AdminFee <- as.numeric(gsub("\\$|,","",dViol$AdminFee))
dViol$StateFee <- as.numeric(gsub("\\$|,","",dViol$StateFee))
dViol$LateFee <- as.numeric(gsub("\\$|,","",dViol$LateFee))
dViol$CleanUpCost <- as.numeric(gsub("\\$|,","",dViol$CleanUpCost))
dViol$LienFilingFee <- as.numeric(gsub("\\$|,","",dViol$LienFilingFee))
dViol$JudgmentAmt <- as.numeric(gsub("\\$|,","",dViol$JudgmentAmt))

dCrime <- read.csv("..\\data\\Crime-Incidents.csv")
dCrime$INCIDENTDATE <- as.Date(dCrime$INCIDENTDATE,"%m/%d/%Y")
dCrime <- dCrime %>% filter(INCIDENTDATE > "2010-12-31") #remove data outside other data timeframes
dCrime$ADDRESS <- as.character(dCrime$ADDRESS)
dCrime$LOCATION <- as.character(dCrime$LOCATION)

dPermits <- read.csv("..\\data\\Building-Permits.csv", stringsAsFactors = F)
dPermits$CASE_TYPE <- as.factor(dPermits$CASE_TYPE)
dPermits$CASE_DESCRIPTION <- as.factor(dPermits$CASE_DESCRIPTION)
dPermits$LEGAL_USE <- as.factor(dPermits$LEGAL_USE)
dPermits$BLD_PERMIT_TYPE <- as.factor(dPermits$BLD_PERMIT_TYPE)
dPermits$BLD_TYPE_USE <- as.factor(dPermits$BLD_TYPE_USE)
dPermits$FEE_TYPE <- as.factor(dPermits$FEE_TYPE)
dPermits$PERMIT_ISSUED <- as.Date(dPermits$PERMIT_ISSUED, "%m/%d/%Y" )
dPermits$PCF_AMT_DUE <- as.numeric(gsub("\\$|,","",dPermits$PCF_AMT_DUE))

dPermitParcels <- read.csv("..\\data\\PermitParcels.csv", stringsAsFactors = F)
dPermitParcels <- unique(dPermitParcels) #there are duplicates due to duplicate "Dismantle" permits
dDismantle <- dPermits %>% filter(BLD_PERMIT_TYPE == "Dismantle")
dDismantle <- dDismantle %>% inner_join(dPermitParcels) #Add Assessor's ParcelNo
dDismantle <- dDismantle %>% inner_join(dParcels)
summary(dDismantle$Latitude)


