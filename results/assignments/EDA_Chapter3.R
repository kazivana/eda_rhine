
library(data.table)
library(ggplot2)

# 1.

runoff_stations <- fread('./data/raw/runoff_stations.csv')

head(runoff_stations)


runoff_stations[, sname := abbreviate(station)]
runoff_stations[, altitude := round(altitude, 0)]
runoff_stations[, id := factor(id)]

runoff_area_alt <- runoff_stations[, .(sname, area, altitude)]

# 2. 

ggplot(data = runoff_stations)+
  geom_point(aes(x = area, y = altitude))

# 3.

ggplot(data = runoff_stations, aes(x = area, y = altitude, col = size)) +
  geom_point() +
  geom_text(label = runoff_stations$sname, 
            position = position_jitter(width = 50, height = 50)) +
  theme_minimal()

ggplot(data = runoff_stations, aes(x = lon, y = lat, col = altitude)) +
  geom_point() +
  geom_text(label = runoff_stations$sname, 
            position = position_jitter(width = 0.2, height = 0.2)) +
  scale_color_gradient(low = 'dark green', high = 'brown') +
  theme_bw()
    


# 4. 

to_plot <- runoff_stations[, .(sname, start, end)]
to_plot <- melt(to_plot, id.vars = 'sname')

ggplot(data = to_plot, aes(x = value, y = sname)) +
  geom_line(lwd = 2, col = 'dark red') +
  theme_bw()


## Explorer questions

# 1a: Units for area: km^2
# 1b: Units for runoff: m^3/s

#2: avg. catchment area and runoff, data.frame

runoff_day <- readRDS('./data/runoff_day.rds')

avg_catchment <- mean(runoff_stations_aas$area) 
avg_catchment
# 67069.5 km^2

avg_runoff <- mean(runoff_day$value)
avg_runoff
# 2028.642 m^3

#2: avg. catchment area and runoff, data.table
runoff_stations[, mean(area)]
runoff_day[, mean(value)]

#3: avg. runoff per station

head(runoff_day)

runoff_day[, list(avg=mean(value)), by=sname]

avg_runoff_stations <- runoff_day[, .(mean_runoff = mean(value)), by = sname]
avg_runoff_stations <- avg_runoff_stations[, mean_runoff := round(mean_runoff, 3)]
sname_plot <- ggplot(aver_runoff_stations, aes(x = sname, y = mean_runoff)) +
  geom_bar(stat = "identity", width = 0.4)

print(sname_plot + labs(title = "Average runoff per station", x = "Station", y = "Mean runoff (m³)"))

#4: The catchment area refers to the area where rainwater is collected by the river. 
# At higher altitudes such as the beginning of the Rhine at the Alps, the river has
# only just begun, meaning there is not a lot of catchment area. In contrast, by the
# time it reaches the ocean and the elevation is minimal/nonexistent, the entire
# river is behind this point, meaning the catchment area is very large here where 
# the entirety of the river converges with the open water. 