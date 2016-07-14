#Create PNG maps of Dismantle Permits and Blight Violations.  These will be stiched into animated GIFs using ImageMagick

dismantle_area = get_map(c(-83.3,42.27,-82.92,42.46), maptype = "roadmap", zoom = 11)
map <- ggmap(dismantle_area)

for (currentYear in 2011:2015) {
  for (currentMonth in 1:12) {
    currentDate <- ymd(paste(currentYear, currentMonth, 01, sep = "-"))
    cutoffDate <- currentDate %m+% months(1)
    
    dMonthlyDismantle <- dDismantle %>%
      filter(PERMIT_ISSUED < cutoffDate) %>%
      mutate(InCurrentMonth = ifelse(
        year(PERMIT_ISSUED) == currentYear
        &
          month(PERMIT_ISSUED) == currentMonth,
        paste(currentYear, currentMonth, sep = "-"),
        "Prior"
      ))
    
    png(paste0("..\\data\\D", currentDate, ".png"), bg="transparent", width=1024, height=1024)
    
    print(map + geom_point(data = dMonthlyDismantle,
                     aes(x = Longitude, y = Latitude, colour = InCurrentMonth)) + 
      labs(colour = "Dismantle Permit"))
    
    dev.off()
  }
}

for (currentYear in 2011:2015) {
  for (currentMonth in 1:12) {
    currentDate <- ymd(paste(currentYear, currentMonth, 01, sep = "-"))
    cutoffDate <- currentDate %m+% months(1)
    
    dMonthlyviolation <- dViol %>%
      filter(TicketIssuedDT < cutoffDate) %>%
      mutate(InCurrentMonth = ifelse(
        year(TicketIssuedDT) == currentYear
        &
          month(TicketIssuedDT) == currentMonth,
        paste(currentYear, currentMonth, sep = "-"),
        "Prior"
      ))
    
    png(paste0("..\\data\\V", currentDate, ".png"), bg="transparent", width=1024, height=1024)
    
    print(map + geom_point(data = dMonthlyviolation,
                           aes(x = Lon, y = Lat, colour = InCurrentMonth)) + 
            labs(colour = "Blight violation"))
    
    dev.off()
  }
}