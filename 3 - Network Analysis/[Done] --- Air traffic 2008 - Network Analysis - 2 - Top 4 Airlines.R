libs <- c('statnet', 'degreenet', 'intergraph', 'igraph', 'network', 'sna', 
          'tidyverse', 'ggplot2', 'ape', 'reshape2', 'ggthemes', 'expm', 'ROCR', 'gridExtra', 'ggpubr',
          'xtable',
          'ggmap', 'maps', 'mapdata')
lapply(libs, require, character.only = TRUE)
rm(libs)



load("D:/Data Science Projects/US Air Travel in 2008/Air traffic 2008 - 3 - Network Analysis/[Stock] --- SNA - US flight network.RData")
load("D:/Data Science Projects/US Air Travel in 2008/Air traffic 2008 - 1 - EDA/airtraffic08-airportinfo-EDA.RData")
airport.info <- read.csv('airport-info-08.csv')




# ' ' ' Network Analysis ---------------------------------------------------
names(data)
unique(data$Origin.state)
state.rm <- c('VI', 'PR', 'HI', 'AK')
k <- which(data$Origin.state %in% state.rm | data$Dest.state %in% state.rm)
data <- data[-k,]


# Base US map
usa <- map_data('usa')
us.map <- ggplot() + 
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = 'white', color = 'gray') + 
  coord_fixed(1.3) +
  theme_void()
us.map




# ' ' ' Focus: Airlines ----------------------------------------------------




get.airline.graph <- function(airline, df, city.df) {                       # Helper fn 
  # 1-A. Prepare dataset
  airline.data <- df %>%
    filter(UniqueCarrier == airline) %>%
    select(Origin, Dest, Distance, Origin.Hub, Dest.Hub, UniqueCarrier) %>%
    group_by(Origin, Dest) %>%
    mutate(count = n()) %>%
    distinct(Origin, Dest, Distance, count, Origin.Hub, Dest.Hub, UniqueCarrier) %>%
    arrange(desc(count))
  
  # Convert to igraph object
  airline.graph <- graph.edgelist(as.matrix(airline.data[,1:2]))
  
  # 1-B. Vertex Attributes
  # Airport Hub 
  tmp <- airline.data %>%
    group_by(Origin) %>%
    distinct(Origin, Origin.Hub)
  vertex.attr.df <- data.frame(Index = 1:vcount(airline.graph),
                               Airport = V(airline.graph)$name)
  vertex.attr.df <- merge(vertex.attr.df, tmp, 
                          by.x = 'Airport', by.y = 'Origin', all.x = TRUE)
  vertex.attr.df <- vertex.attr.df[order(vertex.attr.df$Index),]
  V(airline.graph)$Hub <- vertex.attr.df$Origin.Hub
  
  # Total number of Departing flights
  tmp <- airline.data %>%
    group_by(Origin) %>%
    mutate(Departing.flights = sum(count)) %>%
    distinct(Origin, Departing.flights)
  vertex.attr.df <- merge(vertex.attr.df, tmp, 
                          by.x = 'Airport', by.y = 'Origin', all.x = TRUE)
  vertex.attr.df <- vertex.attr.df[order(vertex.attr.df$Index),]
  V(airline.graph)$Departing.flights <- vertex.attr.df$Departing.flights
  
  # Total number of Arriving flights
  tmp <- airline.data %>%
    group_by(Dest) %>%
    mutate(Arriving.flights = sum(count)) %>%
    distinct(Dest, Arriving.flights)
  vertex.attr.df <- merge(vertex.attr.df, tmp, 
                          by.x = 'Airport', by.y = 'Dest', all.x = TRUE)
  vertex.attr.df <- vertex.attr.df[order(vertex.attr.df$Index),]
  V(airline.graph)$Arriving.flights <- vertex.attr.df$Arriving.flights
  
  # ------------------- State, Population, Area.sq.m, Pop.Density, Housing.Density, Capital
  # ------------------- Prepare info
  tmp <- city.df %>%
    select(iata, state, Population, Area.sq.m, Pop.Density, Capital)
  vertex.attr.df <- merge(vertex.attr.df, tmp, 
                          by.x = 'Airport', by.y = 'iata', all.x = TRUE)
  vertex.attr.df <- vertex.attr.df[order(vertex.attr.df$Index),]
  # ------------------- Add vertex attributes: City info
  V(airline.graph)$State <- vertex.attr.df$state
  V(airline.graph)$Population <- vertex.attr.df$Population
  V(airline.graph)$Area.sq.m <- vertex.attr.df$Area.sq.m
  V(airline.graph)$Pop.Density <- vertex.attr.df$Pop.Density
  V(airline.graph)$Capital <- vertex.attr.df$Capital
  
  # 1-B. Edge Attributes
  # Number of flights
  E(airline.graph)$Flights <- airline.data$count
  # Route distance
  E(airline.graph)$Distance <- airline.data$Distance
  
  return(airline.graph)
}



get.airline.v.centrality <- function(graph, city.df) {
  # Vertex centrality table
  airline.v.cen <- data.frame(Airport = V(graph)$name,
                              Hub = V(graph)$Hub,
                              Degree = degree(graph),
                              Closeness = round(closeness(graph, normalized = TRUE),6),
                              Betweenness = round(betweenness(graph, normalized = FALSE), 0),
                              Eigenvector = round(evcent(graph)$vector,3))
  # Airport's city info
  tmp <- city.df %>%
    select(iata, city, state)
  airline.v.cen <- airline.v.cen %>%
    merge(., tmp, by.x = 'Airport', by.y = 'iata') %>%
    select(Airport, city, state, Hub, Degree, Closeness, Betweenness, Eigenvector) %>%
    arrange(desc(Degree))
  return(airline.v.cen)
}



print.vertex.centrality <- function(v.cen.table, filename) {
  print(xtable(v.cen.table,
               digits = c(rep(0,6), 6, 0, 3), type = 'latex'),
        file = paste0(filename, '.tex'))
  write.csv(v.cen.table,
            file = paste0(filename, '.csv'))
}



get.airline.centrality.rank <- function(v.cen.table) {
  v.cen.table <- v.cen.table %>%
    mutate(Airport.City.State = paste0(Airport, ' (', city, ', ', state, ')'))
  # Ranking of airport 
  per.Deg <- v.cen.table %>%
    arrange(desc(Degree)) %>% head(10) %>% select(Airport.City.State)
  per.Cls <- v.cen.table %>%
    arrange(desc(Closeness)) %>% head(10) %>% select(Airport.City.State)
  per.Btw <- v.cen.table %>%
    arrange(desc(Betweenness)) %>% head(10) %>% select(Airport.City.State)
  per.Eig <- v.cen.table %>%
    arrange(desc(Eigenvector)) %>% head(10) %>% select(Airport.City.State)
  
  airline.centrality.rank <- data.frame(Degree = per.Deg[,1],
                                        Closeness = per.Cls[,1],
                                        Betweenness = per.Btw[,1],
                                        Eigenvector = per.Eig[,1])
  return(airline.centrality.rank)
}


print.centrality.ranking <- function(centrality.rank, filename) {
  print(xtable(centrality.rank, type = 'latex'),
        file = paste0(filename, '.tex'))
  write.csv(centrality.rank,
            file = paste0(filename, '.csv'))
}





# 1. Southwest -------------------------------------------------------------
sw.dig <- get.airline.graph('WN', data, airport.info)
summary(sw.dig)
########################################################### Undirected graph
E(sw.dig)$weight <- E(sw.dig)$Flights                        ############# !
sw.und <- as.undirected(sw.dig, mode = 'collapse',           ############# !
                        edge.attr.comb = 'sum')              ############# !
sw.und <- delete_edge_attr(sw.und, 'Distance')               ############# !
summary(sw.und)                                              ############# !
######################################################################## END

# Get Airline Vertex Centrality measures
sw.v.cen <- get.airline.v.centrality(sw.und, airport.info)
# Latex Report 
# print.vertex.centrality(sw.v.cen, 'Airlines - Southwest - Vertex Centralities')

# Get Airport's centrality ranking
sw.centrality.rank <- get.airline.centrality.rank(sw.v.cen)
# Latex Report 
# print.centrality.ranking(sw.centrality.rank, 'Southwest - Airport Centrality Ranking')







# 2. American --------------------------------------------------------------
aa.dig <- get.airline.graph('AA', data, airport.info)
summary(aa.dig)
########################################################### Undirected graph
E(aa.dig)$weight <- E(aa.dig)$Flights                        ############# !
aa.und <- as.undirected(aa.dig, mode = 'collapse',           ############# !
                        edge.attr.comb = 'sum')              ############# !
aa.und <- delete_edge_attr(aa.und, 'Distance')               ############# !
summary(aa.und)                                              ############# !
######################################################################## END

# Get Airline Vertex Centrality measures
aa.v.cen <- get.airline.v.centrality(aa.und, airport.info)
# Latex Report 
# print.vertex.centrality(aa.v.cen, 'Airlines - American - Vertex Centralities')

# Get Airport's centrality ranking
aa.centrality.rank <- get.airline.centrality.rank(aa.v.cen)
# Latex Report 
# print.centrality.ranking(aa.centrality.rank, 'American - Airport Centrality Ranking')





# 3. United ----------------------------------------------------------------
ua.dig <- get.airline.graph('UA', data, airport.info)
summary(ua.dig)
########################################################### Undirected graph
E(ua.dig)$weight <- E(ua.dig)$Flights                        ############# !
ua.und <- as.undirected(ua.dig, mode = 'collapse',           ############# !
                        edge.attr.comb = 'sum')              ############# !
ua.und <- delete_edge_attr(ua.und, 'Distance')               ############# !
summary(ua.und)                                              ############# !
######################################################################## END

# Get Airline Vertex Centrality measures
ua.v.cen <- get.airline.v.centrality(ua.und, airport.info)
# Latex Report 
# print.vertex.centrality(ua.v.cen, 'Airlines - United - Vertex Centralities')

# Get Airport's centrality ranking
ua.centrality.rank <- get.airline.centrality.rank(ua.v.cen)
# Latex Report 
# print.centrality.ranking(ua.centrality.rank, 'United - Airport Centrality Ranking')






# 4. Delta -----------------------------------------------------------------
dl.dig <- get.airline.graph('DL', data, airport.info)
summary(dl.dig)
########################################################### Undirected graph
E(dl.dig)$weight <- E(dl.dig)$Flights                        ############# !
dl.und <- as.undirected(dl.dig, mode = 'collapse',           ############# !
                        edge.attr.comb = 'sum')              ############# !
dl.und <- delete_edge_attr(dl.und, 'Distance')               ############# !
summary(dl.und)                                              ############# !
######################################################################## END

# Get Airline Vertex Centrality measures
dl.v.cen <- get.airline.v.centrality(dl.und, airport.info)
# Latex Report 
# print.vertex.centrality(dl.v.cen, 'Airlines - Delta - Vertex Centralities')

# Get Airport's centrality ranking
dl.centrality.rank <- get.airline.centrality.rank(dl.v.cen)
# Latex Report 
# print.centrality.ranking(dl.centrality.rank, 'Delta - Airport Centrality Ranking')







# ' ' ' Plot: Hub-&-Spoke --------------------------------------------------
plot.airline.network <- function(graph, label, title) {
  # Prepare geographic layout
  airline.layout <- airport.info %>%
    select(iata, lat, long)
  v.name <- data.frame(iata = V(graph)$name,
                       Index = 1:vcount(graph))
  v.name <- merge(v.name, airline.layout)
  v.name <- v.name[order(v.name$Index),]
  airline.layout <- v.name %>%
    select(lat, long) %>%
    as.matrix()
  # Plot
  set.seed(1)
  plot(graph, layout = layout.davidson.harel,
       vertex.label = labels, vertex.label.color = 'blue', vertex.label.cex = 1.5,
       vertex.frame.color = 'blue', vertex.color = 'white',
       vertex.cex = .5, vertex.size = degree(graph)/2,
       edge.arrow.size = .1, edge.color = 'lightblue')
  title(title, cex.main = 1.5)
}



# Auto layout: best for Hub-and-spoke
par(mfrow = c(2,2), mar = c(1,0,1,0))
# Southwest
labels <- rep('', vcount(sw.und))
labels[degree(sw.und) >= 40] <- V(sw.und)[degree(sw.und) >= 40]$name
plot.airline.network(sw.und, labels, 'Southwest Airlines')
# American
labels <- rep('', vcount(aa.und))
labels[degree(aa.und) >= 40] <- V(aa.und)[degree(aa.und) >= 40]$name
plot.airline.network(aa.und, labels, 'American Airlines')
# United
labels <- rep('', vcount(ua.und))
labels[degree(ua.und) >= 50] <- V(ua.und)[degree(ua.und) >= 50]$name
plot.airline.network(ua.und, labels, 'United Airlines')
# Delta
labels <- rep('', vcount(dl.und))
labels[degree(dl.und) >= 50] <- V(dl.und)[degree(dl.und) >= 50]$name
plot.airline.network(dl.und, labels, 'Delta Air Lines')




# ' ' ' Plot airports on map -----------------------------------------------
plot.airline.map <- function(graph, labels, titles) {
  # Prepare geographic layout
  v.name <- data.frame(iata = V(graph)$name,
                       Degree = degree(graph)/5,
                       Index = 1:vcount(graph),
                       Labels = labels)
  v.name <- merge(v.name, airline.layout)
  # Plot
  p <- us.map +
    geom_point(data = v.name, aes(x = long, y = lat, size = Degree), color = '#336699') +
    geom_text_repel(data = v.name, aes(x = long, y = lat, label = Labels),
                    hjust = .5, nudge_y = 2, color = '#336699', size = 5) +
    labs(title = titles) +
    theme(legend.position = 'none')
  return(p)
}



# Southwest
labels <- rep('', vcount(sw.und))
labels[degree(sw.und) >= 40] <- V(sw.und)[degree(sw.und) >= 40]$name
p.1 <- plot.airline.map(sw.und, labels, 'Southwest Airlines')
# American
labels <- rep('', vcount(aa.und))
labels[degree(aa.und) >= 40] <- V(aa.und)[degree(aa.und) >= 40]$name
p.2 <- plot.airline.map(aa.und, labels, 'American Airlines')
# United
labels <- rep('', vcount(ua.und))
labels[degree(ua.und) >= 50] <- V(ua.und)[degree(ua.und) >= 50]$name
p.3 <- plot.airline.map(ua.und, labels, 'United Airlines')
# Delta
labels <- rep('', vcount(dl.und))
labels[degree(dl.und) >= 50] <- V(dl.und)[degree(dl.und) >= 50]$name
p.4 <- plot.airline.map(dl.und, labels, 'Delta Air Lines')

ggarrange(p.1, p.2, p.3, p.4, ncol = 2, nrow = 2)  




# ' ' ' Statistical Summary ------------------------------------------------
airlines <- c('Southwest', 'American', 'United', 'Delta')
graphs <- list(sw.und, aa.und, ua.und, dl.und)
top4.desc <- data.frame(Airline = airlines,
                        Airports = unlist(lapply(graphs, vcount)),
                        Routes = unlist(lapply(graphs, ecount)))
top4.desc <- top4.desc %>%
  mutate(`Density (%)` = round(Routes / (Airports * (Airports - 1)) * 200, 2))
top4.desc



# print(xtable(top4.desc, digits = c(rep(0,4), 2), type = 'latex'),
#       file = 'Top 4 Airlines - Network Description.tex')
# write.csv(top4.desc, file = 'Top 4 Airlines - Network Description.csv')




# ' ' ' Airline rankings ---------------------------------------------------
airline.v.cen <- function(graph) {
  v.cen <- data.frame(Airport = V(graph)$name,
                      Hub = V(graph)$Hub,
                      Degree = degree(graph),
                      Closeness = round(closeness(graph, normalized = TRUE),6),
                      Betweenness = round(betweenness(graph, normalized = FALSE), 0),
                      Eigenvector = round(evcent(graph)$vector,3))
  tmp <- airport.info %>%
    select(iata, city, state)
  v.cen <- v.cen %>%
    merge(., tmp, by.x = 'Airport', by.y = 'iata') %>%
    mutate(City.State = paste0(city, ', ', state),
           Airport.City.State = paste0(Airport, ' (', city, ', ', state, ')')) %>%
    select(Airport, City.State, Airport.City.State, Degree, Closeness, Betweenness, Eigenvector) %>%
    arrange(desc(Degree))
  # Airport ranking
  per.Deg <- v.cen %>%
    arrange(desc(Degree)) %>% head(5) %>% select(Airport.City.State)
  per.Cls <- v.cen %>%
    arrange(desc(Closeness)) %>% head(5) %>% select(Airport.City.State)
  per.Btw <- v.cen %>%
    arrange(desc(Betweenness)) %>% head(5) %>% select(Airport.City.State)
  per.Eig <- v.cen %>%
    arrange(desc(Eigenvector)) %>% head(5) %>% select(Airport.City.State)
  
  airline.v.cen.table <- data.frame(Degree = per.Deg[,1],
                                    Closeness = per.Cls[,1],
                                    Betweenness = per.Btw[,1],
                                    Eigenvector = per.Eig[,1])
  return(airline.v.cen.table)
}

sw.v.cen <- airline.v.cen(sw.und)
# print(xtable(sw.v.cen, digits = rep(0,5), type = 'latex'),
#       file = 'Airlines - Southwest Statistics.tex')
aa.v.cen <- airline.v.cen(aa.und)
# print(xtable(aa.v.cen, digits = rep(0,5), type = 'latex'),
#       file = 'Airlines - American Statistics.tex')
ua.v.cen <- airline.v.cen(ua.und)
# print(xtable(ua.v.cen, digits = rep(0,5), type = 'latex'),
#       file = 'Airlines - United Statistics.tex')
dl.v.cen <- airline.v.cen(dl.und)
# print(xtable(dl.v.cen, digits = rep(0,5), type = 'latex'),
#       file = 'Airlines - Delta Statistics.tex')







# SCRATCH ------------------------------------------------------------------



