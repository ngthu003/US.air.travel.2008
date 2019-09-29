library(tidyverse)
library(ggplot2)


# Data loading ------------------------------------------------------
airtraffic2008 <- read.csv('2008.csv')
data <- airtraffic2008
airport.info <- read.csv('airport-info-08.csv')

airport.info

# 1. Rename Month & DayofWeek ---------------------------------------
daynames <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday',
              'Saturday', 'Sunday')
for (i in 1:7) {
  data$DayN[data$DayOfWeek == i] <- daynames[i]
}
data$DayN <- factor(data$DayN, levels = daynames)
# table(data$DayN)
monthnames <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
for (i in 1:12) {
  data$MonthN[data$Month == i] <- monthnames[i]
}
data$MonthN <- factor(data$MonthN, levels = monthnames)
# table(data$MonthN)
rm(i, daynames, monthnames)
str(data)



# 2. Add Binary var: Holidays ---------------------------------------
holidays <- data.frame()
holidays <- rbind(holidays, 
                  c("New Year's Day", 'Jan', 1),
                  c("MLK' Day", 'Jan', 21),
                  c("Presidents' Day", 'Feb', 18),
                  c('Memorial Day', 'May', 16),
                  c('Independence Day', 'Jul', 4),
                  c('Labor Day', 'Sep', 1),
                  c('Columbus Day', 'Oct', 13),
                  c('Veterans Day', 'Nov', 11),
                  c('Thanksgiving Day', 'Nov', 27),
                  c('Christmas Day', 'Dec', 25))
colnames(holidays) <- c('Name', 'Month', 'Day')
holidays$Month <- as.character(holidays$Month)
# holidays
data$Holiday <- 'no.holiday'
for (i in 1:nrow(holidays)) {
  data$Holiday[data$MonthN == holidays[i,2] & data$DayofMonth == holidays[i,3]] <- 'holiday'
}
data$Holiday <- factor(data$Holiday, levels = c('no.holiday', 'holiday'))
# table(data$Holiday)
rm(i, holidays)
str(data)



# 3. Block CRSDepTime & CRSArrTime ----------------------------------
block <- function(x) {
  if (x <= 600) { x <- 'Block 1' } 
  else if (x <= 1200) { x <- 'Block 2' }
  else if (x <= 1800) { x <- 'Block 3' }
  else { x <- 'Block 4' }
}
# CRS Departure Time
data$CRSDepT.Block <- sapply(data$CRSDepTime, block)
data$CRSDepT.Block <- factor(data$CRSDepT.Block)
str(data$CRSDepT.Block)
# CRS Arrival Time
data$CRSArrT.Block <- sapply(data$CRSArrTime, block)
data$CRSArrT.Block <- factor(data$CRSDepT.Block)
str(data$CRSArrT.Block)
str(data)







# 4. Classify Airports into 4 tiers ---------------------------------
# Classify according to Percentage of Annual Passenger Boarding:
# https://www.faa.gov/airports/planning_capacity/passenger_allcargo_stats/categories/
#     < .05: Non hub
# .05 - .25: Small hub
# .25 -   1: Medium hub
#     >   1: Large hub
from <- data %>%       #------------------------------- Departing flights
  dplyr::select(Origin) %>%
  group_by(Origin) %>%
  summarize(From = n(),
            Share = From/nrow(data),
            Hub = ifelse(Share > .01, 'Large', 
                         ifelse(Share > .0025, 'Medium', 
                                ifelse(Share > .0005, 'Small', 'Non')))) %>%
  arrange(desc(From))

# Origin Airports
data$Origin.Hub <- rep('Non', length(data$Origin))
data$Origin.Hub[data$Origin %in% from$Origin[from$Hub == 'Large']] <- 'Large'
data$Origin.Hub[data$Origin %in% from$Origin[from$Hub == 'Medium']] <- 'Medium'
data$Origin.Hub[data$Origin %in% from$Origin[from$Hub == 'Small']] <- 'Small'
data$Origin.Hub <- factor(data$Origin.Hub, levels = c('Large', 'Medium', 'Small', 'Non'))
# table(data$Origin.Hub)

# Destination Airports
data$Dest.Hub <- rep('Non', length(data$Dest))
data$Dest.Hub[data$Dest %in% from$Origin[from$Hub == 'Large']] <- 'Large'
data$Dest.Hub[data$Dest %in% from$Origin[from$Hub == 'Medium']] <- 'Medium'
data$Dest.Hub[data$Dest %in% from$Origin[from$Hub == 'Small']] <- 'Small'
data$Dest.Hub <- factor(data$Dest.Hub, levels = c('Large', 'Medium', 'Small', 'Non'))
# table(data$Dest.Hub)
str(data)
rm(from)



# 5. Convert to Factors appropriate variables -----------------------
data$UniqueCarrier <- factor(data$UniqueCarrier)

# Convert Cancelled: Interger => Factor
data$CancelledF <- ifelse(data$Cancelled == 1, 'cancelled', 'notcancelled')
data$CancelledF <- factor(data$CancelledF,
                          levels = c('cancelled', 'notcancelled'))
# table(data$CancelledF)
str(data)




# 6. Add Airport's city info ----------------------------------------
airport.info <- airports %>%
  select(iata, city, state, lat, long, Population, Housing.units, Area.sq.m, Pop.Density, Housing.Density, Capital)

data <- merge(data, airport.info, by.x = 'Origin', by.y = 'iata', all.x = TRUE)
colnames(data)[38:47]
for (i in 38:47) {
  colnames(data)[i] <- paste0('Origin.', colnames(data)[i])
}
data <- merge(data, airport.info, by.x = 'Dest', by.y = 'iata', all.x = TRUE)
colnames(data)[48:57]
for (i in 48:57) {
  colnames(data)[i] <- paste0('Dest.', colnames(data)[i])
}

colnames(data)



# 7. Filter out relevant variables ----------------------------------

# 7.1. For EDA ------------------------------------------------------
tmp <- data %>%
  dplyr::select(-Year, -Month, -DayOfWeek, -FlightNum, -TailNum, -TaxiIn, -TaxiOut,
                -Origin.city, -Origin.lat, -Origin.long, -Origin.Housing.units, -Origin.Housing.Density,
                -Dest.city, -Dest.lat, -Dest.long, -Dest.Housing.units, -Dest.Housing.Density) %>%
  dplyr::select(-Holiday, -CRSDepT.Block, -CRSArrT.Block, -CancelledF, 
                -Origin.Population, -Origin.Pop.Density, -Origin.Area.sq.m, -Origin.Capital,
                -Dest.Population, -Dest.Pop.Density, -Dest.Area.sq.m, -Dest.Capital)


# Label airport city, state location --------------------------------
airport.info <- airport.info %>%
  mutate(City.State = paste0(city, ', ', state))

tmp.airport.label <- airport.info[,c('iata', 'City.State')]
head(tmp.airport.label)
head(data)

tmp <- merge(tmp, tmp.airport.label, by.x = 'Origin', by.y = 'iata', all.x = TRUE)
colnames(data)[ncol(data)] <- 'Origin.City.State'
tmp <- merge(tmp, tmp.airport.label, by.x = 'Dest', by.y = 'iata', all.x = TRUE)
colnames(tmp)[ncol(tmp)] <- 'Dest.City.State'
# Remove all files but tmp and save
# save.image(file = 'airtraffic08-airportinfo-EDA.RData')



# 7.2. For Machine Learning: Classification -------------------------
tmp <- data %>%
  dplyr::select(#-Year, -Month, -DayOfWeek, -FlightNum, -TailNum, -TaxiIn, -TaxiOut,
                -DayofMonth, -CRSDepTime, -CRSArrTime, -Cancelled, 
                -DepTime, -ArrTime, -ActualElapsedTime, -AirTime, -ArrDelay, -DepDelay,
                -CancellationCode, -Diverted, -CarrierDelay, -WeatherDelay, -NASDelay, 
                -SecurityDelay, -LateAircraftDelay
                # -Origin.city, -Origin.lat, -Origin.long, -Origin.Housing.units, -Origin.Housing.Density,
                # -Dest.city, -Dest.lat, -Dest.long, -Dest.Housing.units, -Dest.Housing.Density
                -Origin.Area.sq.m, -Dest.Area.sq.m,
                -Origin.state, -Dest.state,
                -Origin.Population, -Dest.Population,
                -Origin.Pop.Density, -Dest.Pop.Density
                )
# Remove all files but tmp and save
# save.image(file = 'airtraffic08-airportinfo-MachineLearning.RData')
















