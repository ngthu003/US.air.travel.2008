libs <- c('statnet', 'degreenet', 'intergraph', 'igraph', 'network', 'sna', 
          'tidyverse', 'ape', 'reshape2',  'expm', 'ROCR', 
          'xtable', 'latex2exp',
          'ggmap', 'ggrepel', 'ggplot2', 'ggthemes', 'gridExtra', 'directlabels')
lapply(libs, require, character.only = TRUE)
rm(libs)


load("D:/Data Science Projects/US Air Travel in 2008/Air traffic 2008 - 3 - Network Analysis/[Stock] --- SNA - US flight network.RData")
airport.info <- read.csv('airport-info-08.csv')


# Base US map
usa <- map_data('usa')
us.map <- ggplot() + 
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = 'white', color = 'gray') + 
  coord_fixed(1.3) +
  theme_nothing()
us.map



# 1. Descriptive Statistics ------------------------------------------------

# 1.1. Diameter
# ----------- unweighted
diameter(air.und)
get.diameter(air.und)
# ----------- weighted: weights = distance
diameter(air.und, weights = E(air.und)$Distance)
get.diameter(air.und, weights = E(air.und)$Distance)

# 1.2. Average path lengths
average.path.length(air.und)

# 1.3 Density
graph.density(air.und)

# Summary table
tmp <- data.frame(Diameter = c(diameter(air.und)),
                  Average.path.length = average.path.length(air.und),
                  Density = graph.density(air.und))
xtable(tmp)




# 2. Vertex Centrality: Airports -------------------------------------------
v.cen <- data.frame(Airport = V(air.und)$name,
                    Hub = V(air.und)$Hub,
                    Degree = degree(air.und),
                    Closeness = round(closeness(air.und, normalized = TRUE),6),
                    Betweenness = round(betweenness(air.und, normalized = FALSE), 0),
                    Eigenvector = round(evcent(air.und)$vector,3))
tmp <- airport.info %>%
  select(iata, city, state)
v.cen <- v.cen %>%
  merge(., tmp, by.x = 'Airport', by.y = 'iata') %>%
  select(Airport, city, state, Hub, Degree, Closeness, Betweenness, Eigenvector) %>%
  mutate(City.State = paste0(city, ', ', state),
         Airport.City.State = paste0(Airport, ' (', city, ', ', state, ')')) %>%
  arrange(desc(Degree))
View(v.cen)


# Ranking of airport 
# ------------------ Degree
# ------------------ Closeness
# ------------------ Betweenness
# ------------------ Eigenvalue
per.Deg <- v.cen %>%
  arrange(desc(Degree)) %>% head(10) %>% select(Airport.City.State)
per.Cls <- v.cen %>%
  arrange(desc(Closeness)) %>% head(10) %>% select(Airport.City.State)
per.Btw <- v.cen %>%
  arrange(desc(Betweenness)) %>% head(10) %>% select(Airport.City.State)
per.Eig <- v.cen %>%
  arrange(desc(Eigenvector)) %>% head(10) %>% select(Airport.City.State)

airport.centrality.rank <- data.frame(Degree = per.Deg[,1],
                                      Closeness = per.Cls[,1],
                                      Betweenness = per.Btw[,1],
                                      Eigenvector = per.Eig[,1])
View(airport.centrality.rank)


unique(c(per.Deg[,1], per.Cls[,1], per.Btw[,1], per.Eig[,1]))

# ' ' ' Latex Report -------------------------------------------------------
# Vertex centrality
# print(xtable(v.cen, 
#              digits = c(rep(0,6), 6, 0, 3), type = 'latex'), 
#       file = 'All routes - Airports - Vertex Centralities.tex')
# write.csv(v.cen, 
#           file = 'All routes - Airports - Vertex Centralities.csv')
#
# print(xtable(v.cen %>%
#                select(Airport, City.State, Degree) %>% arrange(desc(Degree)) %>% head(10),
#              digits = rep(0, 4)),
#       file = 'USAN - Vertex Centrality - Degree.tex')
# print(xtable(v.cen %>%
#                select(Airport, City.State, Closeness) %>% arrange(desc(Closeness)) %>% head(10),
#              digits = c(rep(0, 3), 3)),
#       file = 'USAN - Vertex Centrality - Closeness.tex')
# print(xtable(v.cen %>%
#                select(Airport, City.State, Betweenness) %>% arrange(desc(Betweenness)) %>% head(10),
#              digits = rep(0, 4)),
#       file = 'USAN - Vertex Centrality - Betweenness.tex')
# print(xtable(v.cen %>%
#                select(Airport, City.State, Eigenvector) %>% arrange(desc(Eigenvector)) %>% head(10),
#              digits = c(rep(0, 3), 3)),
#       file = 'USAN - Vertex Centrality - Eigenvector.tex')
#
# Airport ranking - vertex centrality
# Latex Report 
# print(xtable(airport.centrality.rank, type = 'latex'),
#       file = 'USAN - Airport Centrality Ranking.tex')
# write.csv(airport.centrality.rank,
#           file = 'All routes - Airport Centrality Ranking.csv')



# ' ' ' Plot: Airports' locations ------------------------------------------
# All airports in ranking
tmp <- sapply(unique(c(per.Deg[,1], per.Cls[,1], per.Btw[,1], per.Eig[,1])), function(x) substring(x,0,3))
tmp.ranking <- airport.info %>%
  mutate(Airport.City.State = paste0(iata, ' (', city, ', ', state, ')')) %>%
  filter(iata %in% tmp) %>%
  select(Airport.City.State, lat, long)
us.map +
  geom_point(data = tmp.ranking, aes(x = long, y = lat), size = 5, color = '#336699') +
  geom_text_repel(data = tmp.ranking, aes(x = long, y = lat, label = Airport.City.State),
                  hjust = .5, nudge_y = 1, color = '#336699', size = 5)
rm(per.Deg, per.Cls, per.Btw, per.Eig)




# 3. Small-World & Preferential Attachment Properties ----------------------
# 3.1. Small-World
# ------------------ High Clustering Coefficient
# ------------------ Low Average Path Length

# Clustering coefficient & Average path length
cl.air  <- transitivity(air.und)
apl.air <- average.path.length(air.und)

ntrials <- 10000
nv <- vcount(air.und); ne <- ecount(air.und)
cl.rg  <- numeric(ntrials); apl.rg <- numeric(ntrials)

for (i in 1:ntrials) {
  g.rg <- erdos.renyi.game(nv, ne, type = 'gnm', directed = FALSE)
  cl.rg[i] <- transitivity(g.rg)
  apl.rg[i] <- average.path.length(g.rg)
}

pnorm(cl.air, mean(cl.rg), sd(cl.rg), lower = F)
pnorm(apl.air, mean(apl.rg), sd(apl.rg), lower = F)
air.cl.apl <- data.frame(Attributes = c('Clustering Coef.', 'Average Path Length'),
                         Observed = round(c(cl.air, apl.air),4),
                         Simulation.Mean = round(c(mean(cl.rg), mean(apl.rg)),4),
                         Simulation.SD   = round(c(sd(cl.rg), sd(apl.rg)),4))
air.cl.apl <- air.cl.apl %>%
  mutate(Outside.2.SD = ifelse(Observed >= Simulation.Mean + 2*Simulation.SD 
                               | Observed <= Simulation.Mean - 2*Simulation.SD, 'Yes', 'No'),
         Outside.3.SD = ifelse(Observed >= Simulation.Mean + 3*Simulation.SD 
                               | Observed <= Simulation.Mean - 3*Simulation.SD, 'Yes', 'No'))
print(xtable(air.cl.apl, digits = c(0,0,rep(3,3),0,0)), type = 'latex',
      file = 'Small-world property simulation.tex')

# rm(cl.air, apl.air, ntrials, nv, ne, cl.rg, apl.rg, g.rg, i)

# Observation
# ----------- Observed Clustering Coef. is statistically significantly HIGHER
# ----------- Observed Average Path Length is also statistically significantly HIGHER
# Conclusion
# ----------- Reasons to believe it models the Small-World Model



# 3.2. Preferential Attachment Model
# Fit the power law degree distribution
k <- 5
fit <- fit_power_law(degree(air.und), xmin = k)
alpha <- fit$alpha
tmp <- data.frame(Degree = 1:max(v.cen$Degree),
                  Cum.Den = ecdf(v.cen$Degree)(1:168))
Power.law.Fit <- c(rep(0,k-1), (tmp$Degree[k:168])^(-alpha))
Power.law.Fit.N <- Power.law.Fit/sum(Power.law.Fit)
cum.Power.law <- cumsum(Power.law.Fit.N)
tmp %>%
  mutate(Power.law.Fit = cum.Power.law) %>%
  ggplot() +
  geom_point(aes(x = Degree, y = Cum.Den)) +
  scale_y_reverse(lim = c(1,0)) +
  scale_x_continuous(limits = c(0, 170)) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  xlab('Degrees') + ylab('Frequency') +
  geom_smooth(aes(x = Degree, y = Cum.Den), method = 'lm', formula = y ~ log(x), color = 'red') +
  geom_line(aes(x = Degree, y = Power.law.Fit), color = 'blue') 

  
# Exponential fit
exp.fit <- lm(ecdf(v.cen$Degree)(1:168) ~ log(1:max(v.cen$Degree)))
exp.pred <- predict(exp.fit, newdata = data.frame(grid = 1:168))
exp.pred.N <- exp.pred/max(exp.pred)
# Power law fit
k <- 5
alpha <- fit_power_law(degree(air.und), xmin = k)$alpha
Power.law.Fit <- c(rep(0,k-1), (tmp$Degree[k:168])^(-alpha))
Power.law.Fit.N <- Power.law.Fit/sum(Power.law.Fit)
cum.Power.law <- cumsum(Power.law.Fit.N)

tmp <- data.frame(Degree = rep(1:max(v.cen$Degree), 2),
                  Cum.Density = rep(ecdf(v.cen$Degree)(1:168), 2),
                  Cum.Fit = c(exp.pred.N, cum.Power.law),
                  Fit = c(rep('Exponential fit', 168), rep('Power law: alpha = 5', 168)))
tmp <- data.frame(Degree = 1:max(v.cen$Degree),
                  Cum.Density = ecdf(v.cen$Degree)(1:168),
                  Exp.fit = exp.pred.N,
                  Power.law.fit = cum.Power.law)
tmp %>%
  ggplot() +
  geom_point(aes(x = Degree, y = Cum.Density)) +
  geom_text_repel(aes(x = Degree, y = Cum.Density,
                      label = ifelse(Degree == 20, 'Observed degree distribution', '')),
                  hjust = -.2, nudge_y = .1, color = 'black', size = 5) +
  # geom_line(aes(x = Degree, y = Exp.fit), color = 'red', size = 1) +
  # geom_text_repel(aes(x = Degree, y = Exp.fit,
  #                     label = ifelse(Degree == 50, 'Exponential fit', '')),
  #                 hjust = -.2, nudge_y = .06, color = 'red', size = 5) +
  geom_line(aes(x = Degree, y = Power.law.fit), color = 'blue', size = 1) +
  geom_text_repel(aes(x = Degree, y = Power.law.fit,
                      label = ifelse(Degree == 8, 'Power law fit', '')),
                  hjust = -.2, nudge_y = .06, color = 'blue', size = 5) +
  geom_vline(xintercept = 30, color = 'lightblue') + geom_text(aes(29, .025, label = 30, hjust = -.5), color = 'lightblue') +
  scale_y_reverse(lim = c(1,0)) +
  scale_x_continuous(limits = c(0, 170)) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white')) +
  xlab('Degrees') + 
  ylab('Cumulative Frequency') +
  labs(title = 'Degree distribution of flight network')



  


# 4. Graph Partitioning ----------------------------------------------------
cl.fGr <- cluster_fast_greedy(air.und, weights = E(air.und)$Flights)
cl.Inf <- cluster_infomap(air.und, e.weights = E(air.und)$Flights)
cl.Lou <- cluster_louvain(air.und, weights = E(air.und)$Flights)
cl.Spi <- cluster_spinglass(air.und, weights = E(air.und)$Flights)
cl.lEi <- cluster_leading_eigen(air.und, weights = E(air.und)$Flights)
cl.Wkt <- cluster_walktrap(air.und, weights = E(air.und)$Flights)

clus.list <- list(cl.fGr, cl.Inf, cl.Lou, cl.Spi, cl.lEi, cl.Wkt)
clus.name <- c('Fast Greedy', 'InfoMAP', 'Louvain', 'Spinglass', 'Leading Eigenvector', 'Walktrap')
clus.alg.table <- data.frame(Algorithms = clus.name,
                             Modularity = round(unlist(lapply(clus.list, modularity)),3),
                             Number.of.clusters = unlist(lapply(clus.list, length)))
# Latex report
# print(xtable(clus.alg.table, digits = c(0,0,3,0), type = 'latex'),
#       file = 'USAN - Clustering Algorithm Summary.tex')


# Plot of Louvain algorithm
communities(cl.Lou)
tmp <- airport.info %>%
  filter(iata %in% V(air.und)$name)
tmp$Cluster <- rep(0, nrow(tmp))
for (i in 1:length(cl.Lou)) {
  tmp.clus <- cl.Lou[[i]]
  tmp$Cluster[tmp$iata %in% tmp.clus] <- as.character(i)
}
tmp$Cluster <- as.factor(tmp$Cluster)
table(tmp$Cluster)

us.map +
  geom_point(data = tmp, aes(x = long, y = lat, color = Cluster), size = 2) +
  scale_color_manual(values = c('#984ea3', '#377eb8', '#4daf4a', '#e41a1c'))

  























