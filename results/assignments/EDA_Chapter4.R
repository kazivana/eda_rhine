library(data.table)
library(ggplot2)
library(mapview) 
library(sf)
library(moments)

runoff_summary <- readRDS('./data/runoff_summary.rds')
runoff_stats <- readRDS('./data/runoff_stats.rds')
runoff_day <- readRDS('./data/runoff_day.rds')
runoff_month <- readRDS('./data/runoff_month.rds')
runoff_summer <- readRDS('./data/runoff_summer.rds')
runoff_winter <- readRDS('./data/runoff_winter.rds')
runoff_year <- readRDS('./data/runoff_year.rds')
runoff_stations <- readRDS('./data/runoff_stations.rds')
n_stations <- nrow(runoff_summary)
colset_4 <- c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")

# Navigator tasks ---------------

#1: 

median_runoff <- runoff_day[, .(median = median(value)), sname]
runoff_stats <- median_runoff[runoff_stats, on = 'sname']
runoff_stats_tidy <- melt(runoff_stats, id.vars = 'sname')
to_plot <- runoff_stats_tidy[variable != 'sd_day']

ggplot(to_plot, aes(x = sname, y = value, size = 0.5)) +
  geom_point(aes(col = variable, shape = variable))


#2a:

runoff_day_skew <- runoff_day[, .(skew = moments::skewness(value)), by = sname]
runoff_stats <- runoff_day_skew[runoff_stats, on = 'sname']
runoff_stats[, cv_day := sd_day / mean_day]


runoff_skew_cv <- runoff_stats[, c('sname', 'skew', 'cv_day')]

runoff_skew_cv


#3:

to_merge <- runoff_summary[, .(sname, runoff_class)]
to_plot <- runoff_month[to_merge, on = 'sname']

ggplot(to_plot, aes(x = factor(month), y = value, fill = runoff_class)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free') +
  theme_bw()


#5:

#area
runoff_summary[, area_class := factor('small')]
runoff_summary[area >= 10000 & area < 100000, area_class := factor('medium')]
runoff_summary[area >= 100000, area_class := factor('large')]

#altitude
runoff_summary[, alt_class := factor('low')]
runoff_summary[altitude >= 100 & altitude < 500, alt_class := factor('medium')]
runoff_summary[altitude >= 500, alt_class := factor('high')]

runoff_summary$mean_day <- runoff_stats$mean_day

ggplot(runoff_summary, aes(x = mean_day, y = area,
                           col = area_class, cex = alt_class)) +
  geom_point()


# the outliers for lowest values (Reki, Dier, Doma) are all in Switzerland. 
# meanwhile the outliers for highest daily outflow are in the Netherlands and nearby
# Germany, indicating since the catchment area is larger towards the end of the river
# the outflow is also larger. 


# Explorer's questions ----------------------

#1: The 0.5 quantile is the median

mean(runoff_stats$mean_day)
runoff_stats[, quantile(mean_day)]

# median: 1276
# mean: 1305.471
#2:  The mean is less reliable due to being more influenced by outliers. 

#3: They both have the highest maximums of runoff. Their elevation is similar and
# low. They are both surrounded by a lot of smaller water bodies as well, which collect
# water from the main Rhine river, which might be a factor in explaining the high maximums.



#4: 

runoff_summer_TM1 <- runoff_summer[, min(value), by = sname]
runoff_summer_TM2 <- runoff_summer[, max(value), by = sname]
runoff_summer_to_merge <- merge(runoff_summer_TM1, runoff_summer_TM2, by = 'sname')
colnames(runoff_summer_to_merge) <- c('sname', 'min', 'max')
runoff_summer_to_merge
runoff_summer
runoff_summer_minmax <- merge(runoff_summer, runoff_summer_to_merge, by = 'sname')
runoff_summer_minmax
runoff_summer_max_final <- runoff_summer_minmax[runoff_summer_minmax$value == runoff_summer_minmax$max] 
runoff_summer_min_final <- runoff_summer_minmax[runoff_summer_minmax$value == runoff_summer_minmax$min] 

ggplot(data = runoff_summer_max_final, aes(x = sname, y = max, label = year)) +
  geom_point() +
  geom_text(aes(label = year), hjust = 0, vjust = 0) 

ggplot(data = runoff_summer_min_final, aes(x = sname, y = min, label = year)) +
  geom_point() +
  geom_text(aes(label = year), hjust = 0, vjust = 0) 

runoff_w_TM1 <- runoff_winter[, min(value), by = sname]
runoff_w_TM2 <- runoff_winter[, max(value), by = sname]
runoff_w_to_merge <- merge(runoff_w_TM1, runoff_w_TM2, by = 'sname')
colnames(runoff_w_to_merge) <- c('sname', 'min', 'max')

runoff_w_minmax <- merge(runoff_winter, runoff_w_to_merge, by = 'sname')

runoff_w_max_final <- runoff_w_minmax[runoff_w_minmax$value == runoff_w_minmax$max] 
runoff_w_min_final <- runoff_w_minmax[runoff_w_minmax$value == runoff_w_minmax$min] 

ggplot(data = runoff_w_max_final, aes(x = sname, y = max, label = year)) +
  geom_point() +
  geom_text(aes(label = year), hjust = 0, vjust = 0) 

ggplot(data = runoff_w_min_final, aes(x = sname, y = min, label = year)) +
  geom_point() +
  geom_text(aes(label = year),hjust = 0, vjust = 0) 

runoff_m_TM1 <- runoff_month[,min(value),by = sname]
runoff_m_TM2 <- runoff_month[,max(value),by = sname]
runoff_m_to_merge <- merge(runoff_m_TM1, runoff_m_TM2, by = 'sname')
colnames(runoff_m_to_merge) <- c('sname', 'min', 'max')

runoff_m_minmax <- merge(runoff_month, runoff_m_to_merge, by = 'sname')

runoff_m_max_final <- runoff_m_minmax[runoff_m_minmax$value == runoff_m_minmax$max] 
runoff_m_min_final <- runoff_m_minmax[runoff_m_minmax$value == runoff_m_minmax$min] 

ggplot(data = runoff_m_max_final, aes(x = sname, y = max, label = year)) +
  geom_point() +
  geom_text(aes(label = year), hjust = 0, vjust=0) 

ggplot(data = runoff_m_min_final, aes(x = sname, y = min, label = year)) +
  geom_point() +
  geom_text(aes(label = year), hjust = 0, vjust=0) 

runoff_y_TM1 <- runoff_year[, min(value), by = sname]
runoff_y_TM2 <- runoff_year[, max(value), by = sname]
runoff_y_to_merge <- merge(runoff_y_TM1, runoff_y_TM2, by = 'sname')
colnames(runoff_y_to_merge) <- c('sname', 'min', 'max')

runoff_y_minmax <- merge(runoff_year, runoff_y_to_merge, by = 'sname')

runoff_y_max_final <- runoff_y_minmax[runoff_y_minmax$value == runoff_y_minmax$max] 
runoff_y_min_final <- runoff_y_minmax[runoff_y_minmax$value == runoff_y_minmax$min] 
ggplot(data = runoff_y_max_final, aes(x = sname, y = max, label = year)) +
  geom_point() +
  geom_text(aes(label = year), hjust = 0, vjust = 0) 

ggplot(data = runoff_y_min_final, aes(x = sname, y = min, label = year)) +
  geom_point() +
  geom_text(aes(label = year), hjust = 0, vjust = 0) 
