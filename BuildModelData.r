#Helper function to find nearby "incidents"
#Radius is in miles
#TODO:  Consider implementing a more accurate measurement of distance, using "d" from
# dlon = lon2 - lon1 
# dlat = lat2 - lat1 
# a = (sin(dlat/2))^2 + cos(lat1) * cos(lat2) * (sin(dlon/2))^2 
# c = 2 * atan2( sqrt(a), sqrt(1-a) ) 
# d = 3961 * c (where 3961 approximates the radius of the Earth in miles)  

nearby_count <- function(x=c(0.0, 0.0), df, radius=1, cutoffdate) {
  degree_bounding <- radius / 51 #approximately 51 miles per degree of longitude at this latitude
  return(nrow(df[which(abs(df[,"Latitude"] - x[1]) < degree_bounding &
                       abs(df[,"Longitude"] - x[2]) < degree_bounding &
                       as.Date(df[,"TestDate"]) <= cutoffdate),])) 
}

#Start with blighted properties
blighted_properties <-  dDismantle %>%
                        select(ParcelNo, SaleDate, SalePrice, Latitude, Longitude, 
                               TaxStatus, IsImproved, PermitIssued = PERMIT_ISSUED) %>%
                        distinct_() %>%
                        mutate(BlightStatus = "Blighted")

#Add blighted neighbor counts
work.df <- blighted_properties %>% select(Latitude=Latitude, Longitude=Longitude, TestDate=PermitIssued)
blighted_properties$BlightNeighborCount <- 0 #initialize for performance
blighted_properties$BlightNeighborCount <- as.numeric(apply(as.matrix(blighted_properties[,c("Latitude","Longitude")]), 
                                                   1, nearby_count, df=work.df, radius=1, 
                                                   cutoffdate=blighted_properties$PermitIssued))

#Add crime counts
work.df <- dCrime %>% sample_n(10000) %>% select(Latitude=LAT, Longitude=LON, TestDate=INCIDENTDATE)
blighted_properties$CrimeCount <- 0 #initialize for performance
blighted_properties$CrimeCount <- as.numeric(apply(as.matrix(blighted_properties[,c("Latitude","Longitude")]), 
                                                   1, nearby_count, df=work.df, radius=5, 
                                                   cutoffdate=blighted_properties$PermitIssued))

#Add blight violations
work.df <- dViol %>% sample_n(10000) %>% select(Latitude=Lat, Longitude=Lon, TestDate=TicketIssuedDT)
blighted_properties$BlightCount <- 0 #initialize for performance
blighted_properties$BlightCount <- as.numeric(apply(as.matrix(blighted_properties[,c("Latitude","Longitude")]), 
                                                   1, nearby_count, df=work.df, radius=2, 
                                                   cutoffdate=blighted_properties$PermitIssued))

#Add 311 calls
work.df <- d311 %>% sample_n(10000) %>% select(Latitude=lat, Longitude=lng, TestDate=acknowledged_at)
blighted_properties$Call311Count <- 0 #initialize for performance
blighted_properties$Call311Count <- as.numeric(apply(as.matrix(blighted_properties[,c("Latitude","Longitude")]), 
                                                    1, nearby_count, df=work.df, radius=2, 
                                                    cutoffdate=blighted_properties$PermitIssued))

#Build non-blighted properties of the same size (as per class instructions)
other_properties <-   dParcels %>%
                      anti_join(dDismantle, by=c("ParcelNo" = "ParcelNo")) %>%
                      sample_n(nrow(blighted_properties)) %>%
                      select(ParcelNo, SaleDate, SalePrice, Latitude, Longitude, 
                             TaxStatus, IsImproved) %>%
                      distinct_() %>%
                      mutate(PermitIssued = NA, BlightStatus = "Non-Blighted")

#Add blighted neighbor counts
work.df <- blighted_properties %>% select(Latitude=Latitude, Longitude=Longitude, TestDate=PermitIssued)
other_properties$BlightNeighborCount <- 0 #initialize for performance
other_properties$BlightNeighborCount <- as.numeric(apply(as.matrix(blighted_properties[,c("Latitude","Longitude")]), 
                                                            1, nearby_count, df=work.df, radius=1, 
                                                            cutoffdate=today()))

#Add crime counts
work.df <- dCrime %>% sample_n(10000) %>% select(Latitude=LAT, Longitude=LON, TestDate=INCIDENTDATE)
other_properties$CrimeCount <- 0 #initialize for performance
other_properties$CrimeCount <- as.numeric(apply(as.matrix(other_properties[,c("Latitude","Longitude")]), 
                                                   1, nearby_count, df=work.df, radius=5, 
                                                   cutoffdate=today()))

#Add blight violations
work.df <- dViol %>% sample_n(10000) %>% select(Latitude=Lat, Longitude=Lon, TestDate=TicketIssuedDT)
other_properties$BlightCount <- 0 #initialize for performance
other_properties$BlightCount <- as.numeric(apply(as.matrix(other_properties[,c("Latitude","Longitude")]), 
                                                    1, nearby_count, df=work.df, radius=2, 
                                                    cutoffdate=today()))

#Add 311 calls
work.df <- d311 %>% sample_n(10000) %>% select(Latitude=lat, Longitude=lng, TestDate=acknowledged_at)
other_properties$Call311Count <- 0 #initialize for performance
other_properties$Call311Count <- as.numeric(apply(as.matrix(other_properties[,c("Latitude","Longitude")]), 
                                                     1, nearby_count, df=work.df, radius=2, 
                                                     cutoffdate=today()))

#Here is our Model Data!
Model_Data <- rbind(blighted_properties, other_properties)

#Features are similar in magnitude but we will scale them anyway since the magnitudes are not relevant in any case
#Also remove final unecessary data
Model_Data$Latitude <- NULL
Model_Data$Longitude <- NULL
Model_Data$BlightNeighborCount <- (Model_Data$BlightNeighborCount - min(Model_Data$BlightNeighborCount)) /
                                  (max(Model_Data$BlightNeighborCount) - min(Model_Data$BlightNeighborCount))
Model_Data$CrimeCount <- (Model_Data$CrimeCount - min(Model_Data$CrimeCount)) /
                         (max(Model_Data$CrimeCount) - min(Model_Data$CrimeCount))
Model_Data$BlightCount <- (Model_Data$BlightCount - min(Model_Data$BlightCount)) /
                          (max(Model_Data$BlightCount) - min(Model_Data$BlightCount))
Model_Data$Call311Count <- (Model_Data$Call311Count - min(Model_Data$Call311Count)) /
                           (max(Model_Data$Call311Count) - min(Model_Data$Call311Count))



