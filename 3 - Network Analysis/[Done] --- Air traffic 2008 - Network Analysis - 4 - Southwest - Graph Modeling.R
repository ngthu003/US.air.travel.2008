libs <- c('statnet', 'degreenet', 'intergraph', 'igraph', 'network', 'sna', 
          'tidyverse', 'ape', 'reshape2',  'expm', 'ROCR', 
          'xtable', 'latex2exp',
          'ggmap', 'ggrepel', 'ggplot2', 'ggthemes', 'gridExtra', 'directlabels')
lapply(libs, require, character.only = TRUE)
rm(libs)


# load("D:/Kaggle Datasets/Airport/Air traffic 2008 - 3 - Network Analysis/[Stock] --- SNA - US flight network.RData")
load("D:/Kaggle Datasets/Airport/airtraffic08-airportinfo-EDA.RData")
airport.info <- read.csv('airport-info-08.csv')



state.rm <- c('VI', 'PR', 'HI', 'AK')
k <- which(data$Origin.state %in% state.rm | data$Dest.state %in% state.rm)
data <- data[-k,]
rm(k, state.rm)




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







# ' ' ' Southwest ----------------------------------------------------------
sw.dig <- get.airline.graph('WN', data, airport.info)
summary(sw.dig)
########################################################### Undirected graph
E(sw.dig)$weight <- E(sw.dig)$Flights                        ############# !
sw.und <- as.undirected(sw.dig, mode = 'collapse',           ############# !
                        edge.attr.comb = 'sum')              ############# !
sw.und <- delete_edge_attr(sw.und, 'weight')                 ############# !
summary(sw.und)                                              ############# !
######################################################################## END




# ' ' ' Fitting Exponential Random Graph -----------------------------------


# 4. Prepare Network object ------------------------------------------------
A <- get.adjacency(sw.und)
v.attrs <- get.data.frame(sw.dig, what = 'vertices')
# Clean up some vertex attributes
# v.attrs$Hub <- as.character(V(sw.und)$Hub)
# v.attrs$State <- as.character(vertex.attr.df$state)
# v.attrs$Capital <- vertex.attr.df$Capital
head(v.attrs)
str(v.attrs)
# Available vertex attributes
# --------------------------- Hub, Departing.flights, Arriving.flights, State,
# --------------------------- Population, Area.sq.m, Pop.Density, Capital
names(v.attrs)
sw.sn <- network::as.network(as.matrix(A), directed = FALSE)
network::set.vertex.attribute(sw.sn, 'Hub', v.attrs$Hub)
network::set.vertex.attribute(sw.sn, 'State', v.attrs$State)
network::set.vertex.attribute(sw.sn, 'Population', v.attrs$Population)
network::set.vertex.attribute(sw.sn, 'Area.sq.m', v.attrs$Area.sq.m)
network::set.vertex.attribute(sw.sn, 'Pop.Density', v.attrs$Pop.Density)
network::set.vertex.attribute(sw.sn, 'Capital', v.attrs$Capital)
(sw.sn)

# 4-B. Baseline model ------------------------------------------------------
ergm.base <- ergm(sw.sn ~ edges, control = control.ergm(seed = 1))
summary(ergm.base)

# Interpretation
# ----------- Edge: binary var => Use LogIt model
# ----------- Prob. of having an edge
exp(coef(ergm.base)[[1]]) / (1 + exp(coef(ergm.base)[[1]]))
# ----------- which is the same as
ecount(sw.und) / choose(vcount(sw.und), 2)


# Model 1: edges + 'Hub' ----------------------------------
ergm.model.1 <- ergm(sw.sn ~ edges + nodefactor('Hub') + nodematch('Hub'), 
                     control = control.ergm(seed = 1))
summary(ergm.model.1)
set.seed(1)
gof.model.1 <- gof(ergm.model.1)
par(mfrow=c(1,4))
plot(gof.model.1, cex.axis = 1.5)


# Model 2: edges + 'State' --------------------------------
ergm.model.2 <- ergm(sw.sn ~ edges + nodefactor('State') + nodematch('State'), 
                     control = control.ergm(seed = 1))
summary(ergm.model.2)


# Model 3: edges + Population -----------------------------
ergm.model.3 <- ergm(sw.sn ~ edges + nodecov('Population'), 
                     control = control.ergm(seed = 1))
summary(ergm.model.3)


# Model 4: edges + Pop.Density ----------------------------
ergm.model.4 <- ergm(sw.sn ~ edges + nodecov('Pop.Density'), 
                     control = control.ergm(seed = 1))
summary(ergm.model.4)


# Model 5: edges + Population + Pop.Density ---------------
ergm.model.5 <- ergm(sw.sn ~ edges + nodecov('Population') + nodecov('Pop.Density'), 
                     control = control.ergm(seed = 1))
summary(ergm.model.5)


# Model 6: edges + Capital --------------------------------
ergm.model.6 <- ergm(sw.sn ~ edges + nodecov('Capital'), 
                     control = control.ergm(seed = 1))
summary(ergm.model.6)


# Model 7: edges + Area.sq.m --------------------------------
ergm.model.7 <- ergm(sw.sn ~ edges + nodecov('Area.sq.m'), 
                     control = control.ergm(seed = 1))
summary(ergm.model.7)


# Model 8: edges + Everything --------------------------------
ergm.model.8 <- ergm(sw.sn ~ edges + nodefactor('Hub') + nodematch('Hub')
                     + nodefactor('State') + nodematch('State')
                     + nodecov('Population') + nodecov('Pop.Density')
                     + nodecov('Capital') + nodecov('Area.sq.m'), 
                     control = control.ergm(seed = 1))
summary(ergm.model.8)


# Model 9: edges + Everything - Pop.Density -Area.sq.m -------
ergm.model.9 <- ergm(sw.sn ~ edges + nodefactor('Hub') + nodematch('Hub')
                     + nodefactor('State') + nodematch('State')
                     + nodecov('Population') + nodecov('Capital'), 
                     control = control.ergm(seed = 1))
summary(ergm.model.9)
set.seed(1)
gof.model.9 <- gof(ergm.model.9)
par(mfrow=c(2,2))
plot(gof.model.1, cex.axis = 1.5)





model.summary <- as.data.frame(matrix(ncol = 2))
colnames(model.summary) <- c('Model', 'AIC')
for (i in 1:9) {
  model <- eval(parse(text = paste0('ergm.model.', i)))
  model.aic <- round(summary(model)$aic,0)
  model.summary[1+i,] <- c(paste0('ergm.model.', i), model.aic)
}
model.summary[1,] <- c('Baseline', round(summary(ergm.base)$aic,0))
model.summary




















