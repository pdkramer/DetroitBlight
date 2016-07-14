library("fastmatch")

dPermits$PossibleMatches <- 0

parcels <- dParcels$ParcelNo

for (i in 1:nrow(dPermits)) {
  permit <- dPermits[i,]
  parcel <- dPermits[i,"PARCEL_NO"]
  
  if (!is.na(fmatch(gsub(" ","",parcel),parcels)))  dPermits[i,"PossibleMatches"] <- dPermits[i,"PossibleMatches"] + 1
  if (!is.na(fmatch(gsub(" ","0",parcel),parcels))) dPermits[i,"PossibleMatches"] <- dPermits[i,"PossibleMatches"] + 1
  if (!is.na(fmatch(gsub(" ","1",parcel),parcels))) dPermits[i,"PossibleMatches"] <- dPermits[i,"PossibleMatches"] + 1
  if (!is.na(fmatch(gsub(" ","2",parcel),parcels))) dPermits[i,"PossibleMatches"] <- dPermits[i,"PossibleMatches"] + 1
  if (!is.na(fmatch(gsub(" ","3",parcel),parcels))) dPermits[i,"PossibleMatches"] <- dPermits[i,"PossibleMatches"] + 1
  if (!is.na(fmatch(gsub(" ","4",parcel),parcels))) dPermits[i,"PossibleMatches"] <- dPermits[i,"PossibleMatches"] + 1
  if (!is.na(fmatch(gsub(" ","5",parcel),parcels))) dPermits[i,"PossibleMatches"] <- dPermits[i,"PossibleMatches"] + 1
  if (!is.na(fmatch(gsub(" ","6",parcel),parcels))) dPermits[i,"PossibleMatches"] <- dPermits[i,"PossibleMatches"] + 1
  if (!is.na(fmatch(gsub(" ","7",parcel),parcels))) dPermits[i,"PossibleMatches"] <- dPermits[i,"PossibleMatches"] + 1
  if (!is.na(fmatch(gsub(" ","8",parcel),parcels))) dPermits[i,"PossibleMatches"] <- dPermits[i,"PossibleMatches"] + 1
  if (!is.na(fmatch(gsub(" ","9",parcel),parcels))) dPermits[i,"PossibleMatches"] <- dPermits[i,"PossibleMatches"] + 1

  if (substr(parcel,2,2) == " ") {
    parcel <- paste0("0",substr(parcel,1,1),substr(parcel,3,99))

    if (!is.na(fmatch(gsub(" ","",parcel),parcels)))  dPermits[i,"PossibleMatches"] <- dPermits[i,"PossibleMatches"] + 1
    if (!is.na(fmatch(gsub(" ","0",parcel),parcels))) dPermits[i,"PossibleMatches"] <- dPermits[i,"PossibleMatches"] + 1
    if (!is.na(fmatch(gsub(" ","1",parcel),parcels))) dPermits[i,"PossibleMatches"] <- dPermits[i,"PossibleMatches"] + 1
    if (!is.na(fmatch(gsub(" ","2",parcel),parcels))) dPermits[i,"PossibleMatches"] <- dPermits[i,"PossibleMatches"] + 1
    if (!is.na(fmatch(gsub(" ","3",parcel),parcels))) dPermits[i,"PossibleMatches"] <- dPermits[i,"PossibleMatches"] + 1
    if (!is.na(fmatch(gsub(" ","4",parcel),parcels))) dPermits[i,"PossibleMatches"] <- dPermits[i,"PossibleMatches"] + 1
    if (!is.na(fmatch(gsub(" ","5",parcel),parcels))) dPermits[i,"PossibleMatches"] <- dPermits[i,"PossibleMatches"] + 1
    if (!is.na(fmatch(gsub(" ","6",parcel),parcels))) dPermits[i,"PossibleMatches"] <- dPermits[i,"PossibleMatches"] + 1
    if (!is.na(fmatch(gsub(" ","7",parcel),parcels))) dPermits[i,"PossibleMatches"] <- dPermits[i,"PossibleMatches"] + 1
    if (!is.na(fmatch(gsub(" ","8",parcel),parcels))) dPermits[i,"PossibleMatches"] <- dPermits[i,"PossibleMatches"] + 1
    if (!is.na(fmatch(gsub(" ","9",parcel),parcels))) dPermits[i,"PossibleMatches"] <- dPermits[i,"PossibleMatches"] + 1
  }
}