libs <- c('tidyverse', 'lubridate', 'ggplot2', 'ggthemes', 'ggpubr', 'xtable', 'reshape2', 
          'ggrepel', 'ggthemes', 'RColorBrewer',
          'ggmap')
lapply(libs, require, character.only = TRUE)
rm(libs)


# Load prepared dataset ---------------------------------------------
load("D:/Data Science Projects/US Air Travel in 2008/Air traffic 2008 - 1 - EDA/airtraffic08-airportinfo-EDA.RData")
airport.info <- read.csv('airport-info-08.csv')


# Base US Map -------------------------------------------------------
usa <- map_data('usa')
us.map <- ggplot() + 
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = 'white', color = 'gray') + 
  coord_fixed(1.3) +
  theme_nothing()
# us.map
rm(usa)




# Airline codes -----------------------------------------------------
airline.code <- read.csv('airline_codes.csv')
airline.code[,] <- lapply(airline.code[,], as.character)
str(airline.code)
airline.code <- rbind(airline.code,
                      c('AQ', 'Aloha Airlines'),
                      c('CO', 'Continental Airlines'),
                      c('FL', 'AirTran Airways'),
                      c('NW', 'Northwest Airlines'),
                      c('OH', 'Comair'),
                      c('US', 'US Airways'),
                      c('XE', 'ExpressJet'),
                      c('YV', 'Mesa Airlines'),
                      c('9E', 'Pinnacle Airlines'))

top.4.airlines <- c('Southwest Airlines', 'American Airlines', 
                    'United Airlines', 'Delta Airlines')





# 1.1. By Airlines --------------------------------------------------
tmp <- data %>%
  select(UniqueCarrier) %>%
  group_by(UniqueCarrier) %>%
  summarize(Flights = n()) %>%
  mutate(Share = round(Flights/sum(Flights) * 100, 2))
tmp <- merge(tmp, airline.code, by.x = 'UniqueCarrier', by.y = 'Airline.Code', all.x = TRUE)
tmp <- tmp %>%
  select(UniqueCarrier, Airline.Name, Flights, Share) %>%
  arrange(desc(Flights))

# Table summary by Airlines
print(xtable(tmp, digits = c(0,0,0,0,2), type = 'latex'),
      file = 'EDA - Airlines - Market Share.tex')

# Airlines' total flights in 2008 - Barplot
tmp.Col <- tmp %>%
  arrange(Flights) %>% select(Airline.Name)
tmp.Col = ifelse(tmp.Col$Airline.Name %in% top.4.airlines, 'Blue', 'Black')
tmp %>%
  ggplot(aes(x = reorder(Airline.Name, Flights), y = Flights)) +
  geom_bar(stat = 'identity', width=.5, position = 'dodge', fill = '#336699') +
  geom_text(aes(label=Flights), position=position_dodge(width=0.9), 
            vjust=-0.05, hjust = -.25, size = 5, color = '#336699') +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0, 1300000)) +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(axis.text.y = element_text(color = tmp.Col),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'))




# 1.2. By Airports --------------------------------------------------
tmp <- data %>%
  # select(Origin) %>%
  group_by(Origin) %>%
  summarize(Flights = n()) %>%
  mutate(Share = round(Flights/sum(Flights) * 100, 2))

# To get the number of airlines serving each airport
tmp.2 <- data %>%
  group_by(Origin, UniqueCarrier) %>% 
  tally() %>%
  select(Origin, UniqueCarrier) %>%
  group_by(Origin) %>%
  summarize(Number.Airlines = n()) %>%
  arrange(desc(Number.Airlines))
tmp <- merge(tmp, tmp.2)
# To get name of city
tmp.2 <- data %>% select(Origin, Origin.City.State) %>% group_by(Origin) %>% filter(row_number()==1)
tmp <- merge(tmp, tmp.2)
# To get the number of neighboring airports
tmp.2 <- data %>%
  select(Origin, Dest) %>%
  group_by(Origin, Dest) %>%
  filter(row_number()==1) %>%
  group_by(Origin) %>%
  summarise(Number.Neighbors = n()) %>%
  arrange(desc(Number.Neighbors))
tmp <- merge(tmp, tmp.2)
# Final summary table
tmp <- tmp %>% 
  select(Origin, Origin.City.State, Number.Neighbors, Number.Airlines, Flights, Share) %>%
  arrange(desc(Share))
head(tmp)


# Table summary by Airports
print(xtable(tmp, digits = c(rep(0,6),2), type = 'latex'),
      file = 'EDA - Airports - Market Share.tex')


library(corrplot)

cor(tmp$Share, tmp$Number.Airlines)
cor.table <- tmp %>% 
  select(Number.Neighbors, Number.Airlines, Flights) %>% 
  cor() %>% round(.,2)
print(xtable(cor.table, digits = c(0, rep(2,3)), type = 'latex'),
      file = 'EDA - Airports - Correlation Matrix.tex')



tmp %>%
  mutate(Full.Name = paste0(Origin, ' (', Origin.City.State, ')')) %>%
  filter(Share >= 1) %>%
  ggplot(aes(x = reorder(Full.Name, Flights), y = Flights)) +
  geom_bar(stat = 'identity', width=.5, position = 'dodge', fill = '#336699') +
  geom_text(aes(label=Flights), position=position_dodge(width=0.9), 
            vjust=-0.05, hjust = -.25, size = 4, color = '#336699') +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0, 500000)) +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(axis.text.y = element_text(),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'))

# To get airport's coordinates on map
tmp.2 <- airport.info[,c('iata', 'lat', 'long')]
tmp <- merge(tmp, tmp.2, by.x = 'Origin', by.y = 'iata')

# Map of Major Hubs
tmp.Airports <- tmp %>% 
  filter(Share >= 1) %>% 
  select(Origin.City.State, lat, long, Flights)
us.map +
  geom_point(data = tmp.Airports, aes(x = long, y = lat, size = Flights), color = '#336699') +
  geom_text_repel(data = tmp.Airports, aes(x = long, y = lat, label = Origin.City.State),
                  hjust = .5, nudge_y = 1, color = '#336699', size = 4)
rm(tmp.2, tmp.Airports, usa, cor.table, res)




# 1.3. By Air Routes ------------------------------------------------
tmp <- data %>%
  group_by(Origin, Dest) %>% 
  summarise(Flights = n()) %>%
  arrange(desc(Flights))
head(tmp)
  
# To get the number of airlines serving each air route
tmp.2 <- data %>%
  group_by(Origin, Dest, UniqueCarrier) %>% 
  tally() %>%
  select(Origin, Dest, UniqueCarrier) %>%
  group_by(Origin, Dest) %>%
  summarize(Number.Airlines = n()) %>%
  arrange(desc(Number.Airlines))
tmp <- merge(tmp, tmp.2)
tmp %>%
  arrange(desc(Flights)) %>%
  head(10)


# To get list of air routes
air.t <- data %>%
  group_by(Origin, Dest) %>%
  mutate(count = n()) %>%
  distinct(Origin, Dest, count) %>%
  arrange(desc(count))
head(air.t)

library(igraph)
air.dig <- graph.edgelist(as.matrix(air.t[,1:2]))
air.und <- as.undirected(air.dig, mode = 'mutual')
summary(air.und)
routes <- get.edgelist(air.und)

routes <- as.data.frame(routes)
colnames(routes) <- c('Airport.1', 'Airport.2')
head(routes)
routes$A1.to.A2 <- rep(0,nrow(routes))
routes$A2.to.A1 <- rep(0,nrow(routes))
routes$Airlines.1.to.2 <- rep(0,nrow(routes))
routes$Airlines.2.to.1 <- rep(0,nrow(routes))

# To get number of flights in each direction
for (i in 1:nrow(routes)) {
  # Get current pair of airports
  a.1 <- routes$Airport.1[i]
  a.2 <- routes$Airport.2[i]
  
  # Airport 1 => Airport 2
  idx <- which(a.1 == tmp$Origin & a.2 == tmp$Dest)
  routes$A1.to.A2[i] <- tmp$Flights[idx]
  routes$Airlines.1.to.2[i] <- tmp$Number.Airlines[idx]
  # Airport 2 => Airport 1
  idx <- which(a.2 == tmp$Origin & a.1 == tmp$Dest)
  routes$A2.to.A1[i] <- tmp$Flights[idx]
  routes$Airlines.2.to.1[i] <- tmp$Number.Airlines[idx]
}

# To get full city, state names for airports
tmp.airports <- airport.info %>%
  mutate(City.State = paste0(city, ', ', state),
         Full.Name = paste0(iata, ' (', city, ', ', state, ')')) %>%
  select(iata, City.State, Full.Name)

routes <- merge(routes, tmp.airports, by.x = 'Airport.1', by.y = 'iata')
colnames(routes)[c(7,8)] <- paste0('Airport.1.', colnames(routes)[c(7,8)])
routes <- merge(routes, tmp.airports, by.x = 'Airport.2', by.y = 'iata')
colnames(routes)[c(9,10)] <- paste0('Airport.2.', colnames(routes)[c(9,10)])
head(routes)
# To add some more variables
routes <- routes %>%
  mutate(Total.Flights = A1.to.A2 + A2.to.A1,
         Difference = abs(A1.to.A2 - A2.to.A1),
         Number.Airlines = Airlines.2.to.1) %>%
  mutate(Ratio.Diff = round(Difference / Total.Flights,3),
         Share = round(Total.Flights/sum(Total.Flights) * 100, 3)) %>%
  arrange(desc(Total.Flights))


tmp <- routes %>%
  select(Airport.1, Airport.1.City.State, Airport.2, Airport.2.City.State,
         Number.Airlines,
         A1.to.A2, A2.to.A1, Total.Flights, Difference, Ratio.Diff, Share) %>%
  head(100)
tmp$A1.to.A2 <- prettyNum(tmp$A1.to.A2, big.mark = ',')
tmp$A2.to.A1 <- prettyNum(tmp$A2.to.A1, big.mark = ',')
tmp$Total.Flights <- prettyNum(tmp$Total.Flights, big.mark = ',')
str(tmp)
head(tmp)
print(xtable(tmp, digits = c(rep(0,10), 3,3), type = 'latex'),
      file = 'EDA - Air Routes - Market Share.tex')



# Air Routes' Total FLights - Barplot
tmp <- routes %>%
  mutate(Routes = paste0(Airport.1, '-', Airport.2)) %>%
  head(30) %>%
  select(Routes, Total.Flights, Number.Airlines)

tmp.Col <- tmp %>%
  arrange(Total.Flights) %>% select(Number.Airlines)
tmp.Col = ifelse(tmp.Col >=7, 'Blue', 
                 ifelse(tmp.Col >=4, 'Brown', 'Black'))
tmp %>%
  ggplot(aes(x = reorder(Routes, Total.Flights), y = Total.Flights)) +
  geom_bar(stat = 'identity', width = .5, position = 'dodge', fill = '#336699') +
  geom_text(aes(label=Total.Flights), position=position_dodge(width=0.9), 
            vjust=-0.05, hjust = -.25, size = 5, color = '#336699') +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0, 30000)) +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(axis.text.y = element_text(color = tmp.Col),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'))


tmp <- routes %>%
  arrange(desc(Ratio.Diff)) %>%
  filter(Total.Flights >= 366) %>%
  select(Airport.1, Airport.1.City.State, Airport.2, Airport.2.City.State,
         A1.to.A2, A2.to.A1, Total.Flights, Difference, Ratio.Diff) %>%
  head(10)
print(xtable(tmp, digits = c(rep(0,9), 3), type = 'latex'),
      file = 'EDA - Air Routes - Unbalanced Directions.tex')





# 1.4. By Time ------------------------------------------------------
tmp <- data %>%
  select(MonthN, DayofMonth, DayN) %>%
  group_by(MonthN, DayofMonth, DayN) %>%
  summarize(count = n())
tmp %>%
  ggplot(aes(x = 1:366, y = count, color = factor(DayN))) +
  geom_point(size = 2) +
  # labs(x = '', y = '', color = 'Day of the Week') +
  scale_y_continuous(labels = scales::comma, limits = c(0, 22000)) +
  theme_fivethirtyeight() +
  theme(plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = 'white')) +
  guides(color = guide_legend(override.aes = list(size=4),
                              title = 'Day of Week'))

# By Day of Week
tmp.2 <- tmp %>%
  group_by(DayN) %>%
  summarize(total = sum(count))
tmp.2$DayN <- as.character(tmp.2$DayN)
tmp.2 <- as.data.frame(t(rbind(tmp.2, c('Average', round(mean(tmp.2$total),0)))))
tmp.2[,] <- lapply(tmp.2[,], as.character)
tmp.2[2,] <- prettyNum(tmp.2[2,], big.mark = ',')
print(xtable(tmp.2, digits = c(rep(0,9)), type = 'latex'),
      file = 'EDA - Time - Day of Week.tex')


# By Month
tmp.2 <- tmp %>%
  group_by(MonthN) %>%
  summarize(total = sum(count))
tmp.2$MonthN <- as.character(tmp.2$MonthN)
tmp.2 <- as.data.frame(t(rbind(tmp.2, c('Average', round(mean(tmp.2$total),0)))))
tmp.2[,] <- lapply(tmp.2[,], as.character)
tmp.2[2,] <- prettyNum(tmp.2[2,], big.mark = ',')
print(xtable(tmp.2, digits = c(rep(0,14)), type = 'latex'),
      file = 'EDA - Time - Month.tex')





# 2. Delays and Cancels ---------------------------------------------




# 2.1. Delays -------------------------------------------------------
tmp <- subset(data, DepDelay > 0)
# nrow(tmp)/nrow(data)


# 2.1.1. Delays: Categories -----------------------------------------
# Number of Delays per Category
Delay.Carrier <- nrow(subset(data, CarrierDelay > 0))
Delay.Weather <- nrow(subset(data, WeatherDelay > 0))
Delay.NAS <- nrow(subset(data, NASDelay > 0))
Delay.Security <- nrow(subset(data, SecurityDelay > 0))
Delay.LateAircraft <- nrow(subset(data, LateAircraftDelay > 0))
type.Delay <- c('Carrier', 'Weather', 'National Air System', 'Security', 'Late Aircraft')
tmp <- data.frame(Type = type.Delay,
                  Flights = c(Delay.Carrier, Delay.Weather, Delay.NAS, Delay.Security, Delay.LateAircraft))
t(tmp)
tmp <- data.frame(CarrierDelay = c(Delay.Carrier, round(100*Delay.Carrier/nrow(data),2)),
                  WeatherDelay = c(Delay.Weather, round(100*Delay.Weather/nrow(data),2)),
                  NASDelay = c(Delay.NAS, round(100*Delay.NAS/nrow(data),2)),
                  SecurityDelay = c(Delay.Security, round(100*Delay.Security/nrow(data),2)),
                  LateAircraftDelay = c(Delay.LateAircraft, round(100*Delay.LateAircraft/nrow(data),2)))

tmp <- tmp %>%
  mutate(Total = CarrierDelay + WeatherDelay + NASDelay + SecurityDelay + LateAircraftDelay)
rownames(tmp) <- c('Flights', 'Percentage')
print(xtable(tmp, digits = c(1,rep(2,6)), type = 'latex'),
      file = 'EDA - Delay - Overall Statistics.tex')
tmp
rm(Delay.Carrier, Delay.Weather, Delay.NAS, Delay.Security, Delay.LateAircraft)


# Severity of Delays per Category
bins <- c(0,30,60,90,120,150,180,Inf)
tmp.delays <- subset(data, CarrierDelay > 0)$CarrierDelay
tmp.2 <- data.frame(tmp.delays, bin=cut(tmp.delays, bins, include.lowest=TRUE))
tmp <- table(tmp.2$bin)
tmp.delays <- subset(data, WeatherDelay > 0)$WeatherDelay
tmp.2 <- data.frame(tmp.delays, bin=cut(tmp.delays, bins, include.lowest=TRUE))
tmp <- rbind(tmp, table(tmp.2$bin))
tmp.delays <- subset(data, NASDelay > 0)$NASDelay
tmp.2 <- data.frame(tmp.delays, bin=cut(tmp.delays, bins, include.lowest=TRUE))
tmp <- rbind(tmp, table(tmp.2$bin))
tmp.delays <- subset(data, SecurityDelay > 0)$SecurityDelay
tmp.2 <- data.frame(tmp.delays, bin=cut(tmp.delays, bins, include.lowest=TRUE))
tmp <- rbind(tmp, table(tmp.2$bin))
tmp.delays <- subset(data, LateAircraftDelay > 0)$LateAircraftDelay
tmp.2 <- data.frame(tmp.delays, bin=cut(tmp.delays, bins, include.lowest=TRUE))
tmp <- rbind(tmp, table(tmp.2$bin))

tmp <- as.data.frame(t(tmp))
colnames(tmp) <- c('Carrier', 'Weather', 'National Air System', 'Security', 'Late Aircraft')
tmp.2 <- rownames(tmp)
tmp <- tmp %>%
  mutate(Total = Carrier + Weather + `National Air System` + Security + `Late Aircraft`) %>%
  mutate(Share = round(100* Total / nrow(data), 2))
rownames(tmp) <- tmp.2
for (i in 1:6) {
  tmp[,i] <- prettyNum(tmp[,i], big.mark = ',')
}
# print(xtable(tmp, digits = c(rep(0,7),2), type = 'latex'),
#       file = 'EDA - Delay - Severity per Category.tex')
rm(bins, i, tmp.2, tmp.delays, type.Delay)



# 2.1.2. Delays: Airlines -------------------------------------------
# Delay Statistics per Airlines
tmp <- subset(data, DepDelay > 0)
tmp <- tmp %>%
  select(UniqueCarrier) %>%
  group_by(UniqueCarrier) %>%
  summarize(Delayed.Flights = n())
tmp.2 <- data %>%
  select(UniqueCarrier) %>%
  group_by(UniqueCarrier) %>%
  summarize(Flights = n())
tmp <- merge(tmp, tmp.2)
tmp <- merge(tmp, airline.code, by.x = 'UniqueCarrier', by.y = 'Airline.Code', all.x = TRUE)
tmp <- tmp %>%
  mutate(Share = round(100 * Delayed.Flights / Flights, 2)) %>%
  select(UniqueCarrier, Airline.Name, Delayed.Flights, Flights, Share) %>%
  arrange(desc(Share))

# Plot - Barplot
# Airlines' total flights in 2008 - Barplot
tmp.Col <- tmp %>%
  arrange(Share) %>% select(Airline.Name)
tmp.Col = ifelse(tmp.Col$Airline.Name %in% top.4.airlines, 'Blue', 'Black')
tmp %>%
  ggplot(aes(x = reorder(Airline.Name, Share), y = Share)) +
  geom_bar(stat = 'identity', width=.5, position = 'dodge', fill = '#336699') +
  geom_text(aes(label = paste0(tmp$Share, ' %')), position=position_dodge(width=0.9), 
            vjust=-0.05, hjust = -.25, size = 5, color = '#336699') +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0, 100)) +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(axis.text.y = element_text(color = tmp.Col),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'))

# Summary Table
tmp$Delayed.Flights <- prettyNum(tmp$Delayed.Flights, big.mark = ',')
tmp$Flights <- prettyNum(tmp$Flights, big.mark = ',')
print(xtable(tmp, digits = c(rep(0,5),2), type = 'latex'),
      file = 'EDA - Delay - by Airlines.tex')



# 2.1.3. Delays: Airports -------------------------------------------
# Delay Statistics per Airports
tmp <- subset(data, DepDelay > 0)
tmp <- tmp %>%
  select(Origin) %>%
  group_by(Origin) %>%
  summarize(Delayed.Flights = n())
tmp.2 <- data %>%
  select(Origin) %>%
  group_by(Origin) %>%
  summarize(Flights = n())
tmp <- merge(tmp, tmp.2)
tmp.2 <- airport.info[,c('iata', 'city', 'state')]
tmp <- merge(tmp, tmp.2, by.x = 'Origin', by.y = 'iata', all.x = TRUE)
tmp <- tmp %>%
  mutate(Share = round(100 * Delayed.Flights / Flights, 2),
         City.State = paste0(city, ', ', state)) %>%
  filter(Flights >= 366) %>%
  select(Origin, City.State, Delayed.Flights, Flights, Share) %>%
  arrange(desc(Share))


# Plot: Delay percentage by Airports: Barplot
tmp %>%
  mutate(Full.Name = paste0(Origin, ' (', City.State, ')')) %>%
  arrange(desc(Share)) %>%
  head(20) %>%
  ggplot(aes(x = reorder(Full.Name, Share), y = Share)) +
  geom_bar(stat = 'identity', width=.5, position = 'dodge', fill = '#336699') +
  geom_text(aes(label = paste0(Share, ' %')), position=position_dodge(width=0.9), 
            vjust=-0.05, hjust = -.25, size = 5, color = '#336699') +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0, 100)) +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'))


# Summary table
# summary(tmp$Share)
# By Share/Percentage
tmp.2 <- tmp %>% arrange(desc(Share)) %>% head(30)
tmp.2$Delayed.Flights <- prettyNum(tmp.2$Delayed.Flights, big.mark = ',')
tmp.2$Flights <- prettyNum(tmp.2$Flights, big.mark = ',')
print(xtable(tmp.2, digits = c(rep(0,5),2), type = 'latex'),
      file = 'EDA - Delay - by Airports - by Share.tex')

# By #(flights)
tmp.2 <- tmp %>% arrange(desc(Flights)) %>% head(30)
tmp.2$Delayed.Flights <- prettyNum(tmp.2$Delayed.Flights, big.mark = ',')
tmp.2$Flights <- prettyNum(tmp.2$Flights, big.mark = ',')
print(xtable(tmp.2, digits = c(rep(0,5),2), type = 'latex'),
      file = 'EDA - Delay - by Airports - by Flights.tex')





# 2.2. Cancels ------------------------------------------------------
tmp <- subset(data, Cancelled == 1)
# nrow(tmp)/nrow(data)


# 2.2.1. Cancels: Airports ------------------------------------------
# Cancel Statistics per Airports
tmp <- tmp %>%
  select(Origin) %>%
  group_by(Origin) %>%
  summarize(Cancelled.Flights = n())
tmp.2 <- data %>%
  select(Origin) %>%
  group_by(Origin) %>%
  summarize(Flights = n())
tmp <- merge(tmp, tmp.2)
tmp.2 <- airport.info[,c('iata', 'city', 'state')]
tmp <- merge(tmp, tmp.2, by.x = 'Origin', by.y = 'iata', all.x = TRUE)
tmp <- tmp %>%
  mutate(Share = round(100 * Cancelled.Flights / Flights, 2),
         City.State = paste0(city, ', ', state)) %>%
  filter(Flights >= 366) %>%
  select(Origin, City.State, Cancelled.Flights, Flights, Share) %>%
  arrange(desc(Share))

# Plot: Cacnel percentage by Airports: Barplot
tmp %>%
  mutate(Full.Name = paste0(Origin, ' (', City.State, ')')) %>%
  arrange(desc(Flights)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(Full.Name, Flights), y = Share)) +
  geom_bar(stat = 'identity', width=.5, position = 'dodge', fill = '#336699') +
  geom_text(aes(label = paste0(Share, ' %')), position=position_dodge(width=0.9), 
            vjust=-0.05, hjust = -.25, size = 5, color = '#336699') +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0, 25)) +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'))

# Summary table
# By Share/Percentage
tmp.2 <- tmp %>% arrange(desc(Share)) %>% head(30)
tmp.2$Cancelled.Flights <- prettyNum(tmp.2$Cancelled.Flights, big.mark = ',')
tmp.2$Flights <- prettyNum(tmp.2$Flights, big.mark = ',')
print(xtable(tmp.2, digits = c(rep(0,5),2), type = 'latex'),
      file = 'EDA - Cancels - by Airports - by Share.tex')

# By #(flights)
tmp.2 <- tmp %>% arrange(desc(Flights)) %>% head(30)
tmp.2$Cancelled.Flights <- prettyNum(tmp.2$Cancelled.Flights, big.mark = ',')
tmp.2$Flights <- prettyNum(tmp.2$Flights, big.mark = ',')
print(xtable(tmp.2, digits = c(rep(0,5),2), type = 'latex'),
      file = 'EDA - Cancels - by Airports - by Flights.tex')




# 2.2.2. Cancels: Airlines ------------------------------------------
# Cancel Statistics per Airlines
tmp <- subset(data, Cancelled == 1)
tmp <- tmp %>%
  select(UniqueCarrier) %>%
  group_by(UniqueCarrier) %>%
  summarize(Cancelled.Flights = n())
tmp.2 <- data %>%
  select(UniqueCarrier) %>%
  group_by(UniqueCarrier) %>%
  summarize(Flights = n())
tmp <- merge(tmp, tmp.2)
tmp <- merge(tmp, airline.code, by.x = 'UniqueCarrier', by.y = 'Airline.Code', all.x = TRUE)
tmp <- tmp %>%
  mutate(Share = round(100 * Cancelled.Flights / Flights, 2)) %>%
  select(UniqueCarrier, Airline.Name, Cancelled.Flights, Flights, Share) %>%
  arrange(desc(Share))


# Plot - Barplot
# Airlines' total flights in 2008 - Barplot
tmp.Col <- tmp %>%
  arrange(Share) %>% select(Airline.Name)
tmp.Col = ifelse(tmp.Col$Airline.Name %in% top.4.airlines, 'Blue', 'Black')
tmp %>%
  ggplot(aes(x = reorder(Airline.Name, Share), y = Share)) +
  geom_bar(stat = 'identity', width=.5, position = 'dodge', fill = '#336699') +
  geom_text(aes(label = paste0(tmp$Share, ' %')), position=position_dodge(width=0.9), 
            vjust=-0.05, hjust = -.25, size = 5, color = '#336699') +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0, 10)) +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(axis.text.y = element_text(color = tmp.Col),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'))

# Summary Table
tmp$Cancelled.Flights <- prettyNum(tmp$Cancelled.Flights, big.mark = ',')
tmp$Flights <- prettyNum(tmp$Flights, big.mark = ',')
print(xtable(tmp, digits = c(rep(0,5),2), type = 'latex'),
      file = 'EDA - Cancels - by Airlines.tex')











# SCRATCH -----------------------------------------------------------














