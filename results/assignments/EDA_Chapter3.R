
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


