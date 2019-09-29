libs <- c('statnet', 'degreenet', 'intergraph', 'igraph', 'network', 'sna', 
          'tidyverse', 'ggplot2', 'ape', 'reshape2', 'ggthemes', 'expm', 'ROCR', 'gridExtra',
          'xtable', 'ggmap')
lapply(libs, require, character.only = TRUE)
rm(libs)


# --------------------------------------------------------------------------
# RUN AFTER the 'Air traffic 2008 - 0 - Data Preparation - 2008 Data.R' file
# --------------------------------------------------------------------------


# Load Data ----------------------------------------------------------------
load("D:/Data Science Projects/US Air Travel in 2008/airtraffic08-airportinfo-EDA.RData")
airport.info <- read.csv('airport-info-08.csv')


# ' ' ' Network Analysis ---------------------------------------------------
names(data)
unique(data$Origin.state)
state.rm <- c('VI', 'PR', 'HI', 'AK')
k <- which(data$Origin.state %in% state.rm | data$Dest.state %in% state.rm)
data <- data[-k,]
rm(k, state.rm)







# 1-A. All routes dataset --------------------------------------------------
air.t <- data %>%
  #filter(UniqueCarrier == 'WN') %>%
  select(Origin, Dest, Distance, Origin.Hub, Dest.Hub) %>%
  group_by(Origin, Dest) %>%
  mutate(count = n()) %>%
  distinct(Origin, Dest, Distance, count, Origin.Hub, Dest.Hub) %>%
  arrange(desc(count))
head(air.t)


# 1-B. All routes igraph ---------------------------------------------------
###################################################################### START
# 1-B. 1. igraph 
air.dig <- graph.edgelist(as.matrix(air.t[,1:2]))
summary(air.dig)
######################################################################## END



###################################################################### START
# 1-B. 2. Vertex Attributes
# Airport Hub 
tmp <- air.t %>%
  group_by(Origin) %>%
  distinct(Origin, Origin.Hub)
vertex.attr.df <- data.frame(Index = 1:vcount(air.dig),
                             Airport = V(air.dig)$name)
vertex.attr.df <- merge(vertex.attr.df, tmp, 
                        by.x = 'Airport', by.y = 'Origin', all.x = TRUE)
vertex.attr.df <- vertex.attr.df[order(vertex.attr.df$Index),]
V(air.dig)$Hub <- vertex.attr.df$Origin.Hub

# Total number of Departing flights
tmp <- air.t %>%
  group_by(Origin) %>%
  mutate(Departing.flights = sum(count)) %>%
  distinct(Origin, Departing.flights)
vertex.attr.df <- merge(vertex.attr.df, tmp, 
                        by.x = 'Airport', by.y = 'Origin', all.x = TRUE)
vertex.attr.df <- vertex.attr.df[order(vertex.attr.df$Index),]
V(air.dig)$Departing.flights <- vertex.attr.df$Departing.flights

# Total number of Arriving flights
tmp <- air.t %>%
  group_by(Dest) %>%
  mutate(Arriving.flights = sum(count)) %>%
  distinct(Dest, Arriving.flights)
vertex.attr.df <- merge(vertex.attr.df, tmp, 
                        by.x = 'Airport', by.y = 'Dest', all.x = TRUE)
vertex.attr.df <- vertex.attr.df[order(vertex.attr.df$Index),]
V(air.dig)$Arriving.flights <- vertex.attr.df$Arriving.flights


summary(air.dig)
# rm(tmp, vertex.attr.df)


# Airport's City Info ------------------------------------------------------
# ------------------- State, Population, Area.sq.m, Pop.Density, Housing.Density, Capital
# ------------------- Prepare info
tmp <- airport.info %>%
  select(iata, state, Population, Area.sq.m, Pop.Density, Capital)
vertex.attr.df <- merge(vertex.attr.df, tmp, 
                        by.x = 'Airport', by.y = 'iata', all.x = TRUE)
vertex.attr.df <- vertex.attr.df[order(vertex.attr.df$Index),]
names(vertex.attr.df)
# ------------------- Add vertex attributes: City info
V(air.dig)$State <- vertex.attr.df$state
V(air.dig)$Population <- vertex.attr.df$Population
V(air.dig)$Area.sq.m <- vertex.attr.df$Area.sq.m
V(air.dig)$Pop.Density <- vertex.attr.df$Pop.Density
V(air.dig)$Capital <- vertex.attr.df$Capital

summary(air.dig)
######################################################################## END



###################################################################### START
# 1-B. 3. Edge Attributes
# Number of flights
E(air.dig)$Flights <- air.t$count
# Route distance
E(air.dig)$Distance <- air.t$Distance
summary(air.dig)
######################################################################## END

print(object.size(air.dig), units = 'Mb')



# ' ' ' Undirected graph ------------------------------------###############
E(air.dig)$weight <- E(air.dig)$Flights                      ############# !
air.und <- as.undirected(air.dig, mode = 'mutual',           ############# !
                         edge.attr.comb = 'sum')             ############# !
air.und <- delete_edge_attr(air.und, 'Distance')             ############# !
summary(air.und)                                             ############# !
# Get the biggest component                                  ############# !
air.und <- decompose.graph(air.und)[[1]]                     ############# !
######################################################################## END
E(air.und)$Distance <- E(air.und)$weight
air.und <- delete_edge_attr(air.und, 'weight')
summary(air.und)


# Files for Network Analysis
save(air.dig, air.und, file = '[Stock] --- SNA - US flight network.RData')