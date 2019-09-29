library(dplyr)

# Data sets ----------------------------------------------------------------------------
# 1-A. Airport codes & names -----------------------------------------------------------
airports <- read.csv('airports.csv')

tmp1 <- unique(dataNetSA$Origin);
tmp2 <- unique(dataNetSA$Dest)
airports.2008 <- union(tmp1, tmp2)
airports.08 <- data.frame(iata = airports.2008)
airports.08 <- merge(airports.08, airports)
rm(tmp1, tmp2, dataNetSA, airports.2008, airports)



# 1-B. City Census Info ----------------------------------------------------------------
census.all <- read.csv('DEC_10_SF1_GCTPH1.US13PR_with_ann.csv')
census <- census.all %>%
  select(c(6:10, 13:14)) %>%
  slice(2:n())
colnames(census) <- c('State.City', 'City',
                      'Population', 'Housing.units',
                      'Area.sq.m',
                      'Pop.Density', 'Housing.Density')
rm(census.all)




# 1-C. Clean up some city name mismatch ------------------------------------------------
airports.08$city <- sapply(airports.08$city, function(x) gsub('Ft.', 'Fort', x))
airports.08$city <- sapply(airports.08$city, function(x) gsub('St', 'St.', x))
airports.08$city <- sapply(airports.08$city, function(x) gsub('St.ation', 'Station', x))

# Clean up census info
census$City <- (as.character(census$City))
census[census$City == 'Juneau city and borough',][1,2] <- 
  gsub('Juneau city and borough', 'Juneau city', census[census$City == 'Juneau city and borough',][1,2])
census[census$City == 'Sitka city and borough',][1,2] <- 
  gsub('Sitka city and borough', 'Sitka city', census[census$City == 'Sitka city and borough',][1,2])
census[census$City == 'Wrangell city and borough',][1,2] <- 
  gsub('Wrangell city and borough', 'Wrangell city', census[census$City == 'Wrangell city and borough',][1,2])



# Fill out missing airport names
airports.08$city <- as.character(airports.08$city)
airports.08$state <- as.character(airports.08$state)

update.census <- function(iata.code, city.name, state.name, airports.08) {
  tmp.idx <- which(airports.08$iata == iata.code)
  airports.08[tmp.idx, 'city'] <- city.name
  airports.08[tmp.idx,'state'] <- state.name
  return(airports.08)
}

tmp.df <- data.frame(iata = c('CLD', 'HHH', 'MQT', 'SCE', 'SCC'),
                     city = c('Carlsbad', 'Hilton Head Island', 'Marquette', 'State College', 'Prudhoe Bay'),
                     state = c('CA', 'SC', 'MI', 'PA', 'AK'))

for (i in 1:nrow(tmp.df)) {
  tmp <- as.character(unlist(tmp.df[i,]))
  airports.08 <- update.census(tmp[1], tmp[2], tmp[3], airports.08)
}
# View(airports.08)

rm(tmp.df, i, tmp)










# Data Explanation ---------------------------------------------------------------------

# airports.08
# ----------- list of airports having US domestic flights in 2008
# ----------- dimension: 305 x 7



# State Df -----------------------------------------------------------------------------
all.states <- as.character(na.omit(unique(airports.08$state)))
state.names <- c('Pennsylvania', 'Texas', 'New Mexico', 'Georgia', 'Massachusetts', 'California',
  'New Jersey', 'Alaska', 'Louisiana', 'New York', 'Iowa', 'Colorado', 'Wisconsin',
  'North Carolina', 'Michigan', 'Connecticut', 'Maine', 'Alabama', 'Montana',
  'North Dakota', 'Minnesota', 'Washington', 'Illinois', 'Tennessee', 'Idaho',
  'Puerto Rico', 'Vermont', 'Maryland', 'South Carolina', 'Ohio', 'Utah', 'Virginia',
  'Wyoming', 'West Virginia', 'Kentucky', 'Florida', 'Nevada', 'Oregon', 'Indiana', 
  'Arizona', 'South Dakota', 'Arkansas', 'Mississippi', 'Hawaii', 'Kansas', 'Oklahoma',
  'Nebraska', 'Missouri', 'New Hampshire', 'Rhode Island', 'Virgin Islands')
state.capitals <- c('Harrisburg', 'Austin', 'Santa Fe', 'Atlanta', 'Boston', 'Sacramento', 
                    'Trenton', 'Juneau', 'Baton Rouge', 'Albany', 'Des Moines', 'Denver', 'Madison',
                    'Raleigh', 'Lansing', 'Hartford', 'Augusta', 'Montgomery', 'Helena',
                    'Bismarck', 'St Paul', 'Olympia', 'Springfield', 'Nashville', 'Boise',
                    'San Juan', 'Montpelier', 'Annapolis', 'Columbia', 'Columbus', 'Salt Lake City',
                    'Richmond', 'Cheyenne', 'Charleston', 'Frankfort', 'Tallahassee', 'Carson City',
                    'Salem', 'Indianapolis', 'Phoenix', 'Pierre', 'Little Rock', 'Jackson',
                    'Honolulu', 'Topeka', 'Oklahoma City', 'Lincoln', 'Jefferson City', 'Concord',
                    'Providence', 'Charlotte Amalie')
states.df <- data.frame(state.codes = all.states,
                        state.names = state.names,
                        state.capitals = state.capitals)
rm(all.states, state.names, state.capitals)
#################################################################################### End


# 1. Complicated city names ------------------------------------------------------------
# fn to get cities per state
state.airports <- function(st) {
  cities <- airports.08 %>%
    filter(state == st) %>% select(city) %>% unique()
  cities <- as.character(cities$city)
  return(cities)
}




# 2. Complicated city names ------------------------------------------------------------
complicated.city.names <- list()
simple.city.names <- list()
k <- 1

for (i in 1:length(states.df$state.codes)) {
  # Get the current state code
  state.tmp <- state.airports(as.character(states.df$state.codes[i]))
  # Initialize the current state index of simple/complicated city names
  idx.sim <- c()
  idx.com <- c()
  
  # Check for which names are complicated, if yes, put in complicated.city.names
  # loop over current state's cities having airports in 08
  for (j in 1:length(state.tmp)) {
    if (grepl('-', state.tmp[j]) | grepl('/', state.tmp[j])) {
      idx.com <- c(idx.com, j)
    } else {
      idx.sim <- c(idx.sim, j)
    }
  }
  
  # Update list
  # Simple city names
  simple.city.names[[i]] <- unique(idx.sim)
  names(simple.city.names)[i] <- as.character(states.df$state.codes[i])
  
  # Complicated city names
  
  if (is.null(idx.com) == FALSE) {
    complicated.city.names[[k]] <- unique(idx.com)
    names(complicated.city.names)[k] <- as.character(states.df$state.codes[i])
    k <- k + 1
  }
}

# simple.city.names
# complicated.city.names

rm(i, j, k, idx.com, idx.sim, state.tmp)




# 3-A. Initiate airport.info df w/ dim: 305 x 14 ---------------------------------------
airport.info <- airports.08
for (i in 1:ncol(census)) {
  airport.info <- cbind(airport.info, rep(NA, nrow(airport.info)))
}
colnames(airport.info)[(ncol(airports.08)+1):ncol(airport.info)] <- colnames(census)
# dim(airport.info)
# head(airport.info)
rm(i)




# 3-B. fn to Update airport.info -------------------------------------------------------
update.info <- function(idx, city.name, code, census, info.df) {
  tmp.row <- census[idx,]            #---------------- current city census row
  # look for appropriate row in airport.info
  tmp.idx <- rownames(subset(airport.info,
                             city == city.name & state == code))
  # Convert to character
  tmp.row.2 <- unname(sapply(tmp.row[1, ], as.character))
  for (z in 3:4) {
    tmp.row.2[z] <- as.numeric(strsplit(tmp.row.2[z], split = '(', fixed = TRUE)[[1]][1])  
  }
  # Updata airport.info
  for (k in nk:ncol(info.df)) {
    info.df[tmp.idx, k] <- tmp.row.2[k-ncol(airports.08)]
  }
  return(info.df)
}





# 3-B. Udate airport.info: simple names ------------------------------------------------
nk <- ncol(airports.08) + 1

# Loop for each state having airports having flights in 2008
for (i in 1:length(states.df$state.codes)) {
  # Current state code & name
  tmp.code <- names(simple.city.names)[i]
  tmp.idx <- simple.city.names[[i]]
  tmp.cities <- state.airports(tmp.code)[tmp.idx]     # Simple city names
  # Current state census
  tmp.idx <- grepl(states.df$state.names[i], census$State.City)
  tmp.census <- census[tmp.idx,]
  
  for (j in 1:length(tmp.cities)) {
    # Indicator if current name is a city
    city.flag <- TRUE
    city.flag <- any(grepl(paste(tmp.cities[j], 'city'), tmp.census$City))
    
    # Check for if the current name is not a city
    if (city.flag == FALSE) {                    #--------------------- name is NOT a city
      tmp.idx <- which(grepl(tmp.cities[j], tmp.census$City) == TRUE)
      airport.info <- update.info(tmp.idx, tmp.cities[j], tmp.code, tmp.census, airport.info)
    } 
    else {                                       #--------------------- the name is a city
      tmp.idx <- which((paste(tmp.cities[j], 'city') == tmp.census$City) == TRUE)
      airport.info <- update.info(tmp.idx, tmp.cities[j], tmp.code, tmp.census, airport.info)
    }
  }
}





rm(i, j, k, tmp.cities, tmp.code, tmp.idx, tmp.idx.2, city.flag, tmp.census, tmp.row)
# View(airport.info)




# 3-C. Complicated city names ----------------------------------------------------------
# ..... aggregation fn -----------------------------------------------------------------
agg.split.city <- function(tmp.df) {
  # Convert to character
  tmp.df[, 1:7] <- sapply(tmp.df[, 1:7], as.character)
  for (y in 1:nrow(tmp.df)) {
    tmp.df[y,3] <- as.numeric(strsplit(tmp.df[y, 3], split = '(', fixed = TRUE)[[1]][1])
    for (z in 4:ncol(tmp.df)) {
      tmp.df[y, z] <- as.numeric(tmp.df[y, z])
    }  
  }
  
  # Convert appropriate columns to numeric
  tmp.df[, 3:7] <- sapply(tmp.df[, 3:7], as.numeric)
  
  # Get the aggregate info for all split cities
  tmp.agg <- tmp.df %>%
    select(-State.City, -City) %>%
    mutate(sum.Pop = cumsum(Population),
           sum.Housing = cumsum(Housing.units),
           sum.Area = cumsum(Area.sq.m)) %>%
    select(sum.Pop, sum.Housing, sum.Area) %>%
    filter(row_number() == n())
  tmp.agg <- as.numeric(tmp.agg[1,])
  tmp.agg[4] <- round(tmp.agg[1]/tmp.agg[3],1)
  tmp.agg[5] <- round(tmp.agg[2]/tmp.agg[3],1)
  row.agg <- c(rep(tmp.cities[j], 2), tmp.agg)
  
  # return the vector of aggregate info
  return(row.agg)
}



# ..... Update airport.info ------------------------------------------------------------
for (i in 1:length(complicated.city.names)) {
  tmp.code <- names(complicated.city.names)[i]
  tmp.idx <- complicated.city.names[[i]]
  tmp.cities <- state.airports(tmp.code)[tmp.idx]     # Complicated city names
  tmp.name <- states.df$state.names[states.df$state.codes == tmp.code]
  tmp.idx <- grepl(tmp.name, census$State.City)
  tmp.census <- census[tmp.idx,]
  
  for (j in 1:length(tmp.cities)) {
    
    # Split complicated city names into individual cities
    if (grepl('/', tmp.cities[j])) {
      tmp.cities.split <- strsplit(tmp.cities[j], '/')[[1]]
    } else {
      tmp.cities.split <- strsplit(tmp.cities[j], '-')[[1]]
    }
    
    # Temporary df. to store individual city info
    tmp <- data.frame()
    for (x in 1:length(tmp.cities.split)) {
      # Indicator if current name is a city
      city.flag <- TRUE
      city.flag <- any(grepl(paste(tmp.cities.split[x], 'city'), tmp.census$City))
      
      # Check for if the current name is not a city
      if (city.flag == FALSE) {                    #--------------------- name is NOT a city
        tmp.idx <- which(grepl(tmp.cities.split[j], tmp.census$City) == TRUE)
        tmp.row <- tmp.census[tmp.idx,]            #---------------- current city census row
        # look for appropriate row in airport.info
        tmp.idx.2 <- rownames(subset(airport.info,
                                     city == tmp.cities[i] & state == tmp.code))
        tmp <- rbind(tmp, tmp.row)
      } 
      else {                                       #--------------------- the name is a city
        tmp.idx <- which((paste(tmp.cities.split[x], 'city') == tmp.census$City) == TRUE)
        tmp.row <- tmp.census[tmp.idx,]            #---------------- current city census row
        # look for appropriate row in airport.info
        tmp.idx.2 <- rownames(subset(airport.info,
                                     city == tmp.cities[i] & state == tmp.code))
        tmp <- rbind(tmp, tmp.row)
      }
    }
    
    # Get the aggregate city info
    tmp.row.agg <- agg.split.city(tmp)
    # Update the complicated city info
    tmp.idx <- which(airport.info$city == tmp.cities[j])
    airport.info[tmp.idx,8:14] <- tmp.row.agg
  }
}

rm(i, j, k, x, nk, tmp.cities, tmp.cities.split, tmp.code,
   tmp.idx, tmp.idx.2, tmp.name, tmp.row.agg, tmp.row,
   city.flag, tmp, tmp.census)


# View(airport.info)





# ..... Update State Capital -----------------------------------------------------------
airport.info$Capital <- rep(0, nrow(airport.info))
for (i in 1:nrow(states.df)) {
  tmp.state <- states.df[i,]
  tmp.idx <- which(airport.info$state == tmp.state[1,1] & airport.info$city == tmp.state[1,3])
  airport.info$Capital[tmp.idx] <- 1
}

rm(i, tmp.state, tmp.idx)

View(airport.info)

print(object.size(airport.info), units = 'Mb')

write.csv(file = 'airport-info-08.csv', x = airport.info)

