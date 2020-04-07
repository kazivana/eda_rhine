
library(data.table)
library(ggplot2)

# 1.

runoff_stations <- fread('./data/raw/runoff_stations.csv')

head(runoff_stations)


runoff_stations[, sname := abbreviate(station)]
runoff_stations[, altitude := round(altitude, 0)]

runoff_stations_aas <- runoff_stations[, 6:8]
runoff_stations_lls<- runoff_stations[, 4:8]
runoff_stations_sname <- runoff_stations[, 8]


head(runoff_stations)
head(runoff_stations_aas)
head(runoff_stations_lls)


# 2. 

ggplot(data = runoff_stations_aas)+
  geom_point(aes(x = area, y = altitude))

# 3.

# attempt1
ggplot(data = runoff_stations_aas, aes(x = area, y = altitude)) +
  geom_point() +
  geom_text(
    label=rownames(runoff_stations_aas[,3]),
    nudge_x = 0.25, nudge_y = 0.25,
    check_overlap = T
  )

# attempt2 
ggplot(data = runoff_stations_aas, aes(x = area, y= altitude, color='blue', label=sname))+
  geom_point() + geom_text(aes(label=sname), hjust=0, vjust=0) + theme(legend.position = 'right')

ggplot(data = runoff_stations_lls, aes(x = lon, y = lat, color = 'green', label=sname))+
  geom_point() + geom_text(aes(label=sname), hjust=0, vjust=0) + theme(legend.position = 'right')


# 3. 

runoff_day <- readRDS('./data/runoff_day_raw.rds')
head(runoff_day)

station_time <- runoff_day[, .(start = min(year(date)),
                               end = max(year(date))),
                           by = sname]
table(station_time$end)


## Explorer questions

# 1a: Units for area: m^2
# 1b: Units for runoff: m^3/s

#2: avg. catchment area and runoff

avg_catchment <- mean(runoff_stations_aas$area)
avg_catchment
# 67069.5 m^2

avg_runoff <- mean(runoff_day$value)
avg_runoff
# 2028.642 m^3

#3: avg. runoff per station

head(runoff_day)

runoff_day[, list(avg=mean(value)), by=sname]

#4: They have a somewhat inverse relationship. Higher altitudes would
# generally indicate 'peaks', hills or mountains, which by definition are
# not wide enough to have a large area the way plains at lower altitudes can.