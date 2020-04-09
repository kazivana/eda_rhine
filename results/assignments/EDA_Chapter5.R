library(data.table)
library(ggplot2)

runoff_summary <- readRDS('data/runoff_summary.rds')
runoff_summary_key <- readRDS('data/runoff_summary_key.rds')
runoff_stats <- readRDS('data/runoff_stats.rds')
runoff_month_key <- readRDS('data/runoff_month_key.rds')
runoff_summer_key <- readRDS('data/runoff_summer_key.rds')
runoff_winter_key <- readRDS('data/runoff_winter_key.rds')
runoff_year_key <- readRDS('data/runoff_year_key.rds')
runoff_summer <- readRDS('data/runoff_summer.rds')
runoff_winter <- readRDS('data/runoff_winter.rds')

colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")
theme_set(theme_bw())

# Navigator tasks --------
 
#1: Yearly and monthly boxplots

year_thres <- 1950

runoff_year_key[year < year_thres, period := factor('pre_2000')]
runoff_year_key[year >= year_thres, period := factor('aft_2000')]


ggplot(runoff_year_key, aes(year, value, fill = period)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

runoff_month_key[year < year_thres, period := factor('pre_2000')]
runoff_month_key[year >= year_thres, period := factor('aft_2000')]

ggplot(runoff_month_key, aes(month, value, fill = period)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Month") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

#2: 

runoff_month <- readRDS('./data/runoff_month.rds')
runoff_month[, quantile := cut(value,
                               breaks = quantile(value, probs = seq(0, 1, by = 1/10)),
                               labels = 1:10, right = FALSE)]
runoff_month

low_runoff_month <- runoff_month[, quantile == 1]
high_runoff_month <- runoff_month[, quantile == 9]

runoff_month[quantile == 1, by = sname, .N]
runoff_month[quantile == 9, by = sname, .N]

runoff_month[quantile == 1, by = month, .N]
runoff_month[quantile == 9, by = month, .N]

runoff_day <- readRDS('./data/runoff_day.rds')
runoff_day[, quantile := cut(value,
                             breaks = quantile(value, probs = seq(0, 1, by = 1/10)),
                             labels = 1:10, right = FALSE)]

low_runoff_day <- runoff_day[, quantile == 1]
high_runoff_day <- runoff_day[, quantile ==9]

runoff_day[quantile == 1, by = sname, .N]
runoff_day[quantile == 9, by = sname, .N]



#3:

ggplot(runoff_summer_key[year <= 2010], aes(x = year, y = value)) +
  geom_line(col = colset_4[3])+
  geom_point(col = colset_4[3])+
  facet_wrap(~sname, scales = 'free') +
  geom_smooth(method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  geom_smooth(method = 'loess', formula = y~x, se = 0, col = colset_4[4]) +
  scale_color_manual(values = colset_4[c(1, 2, 4)]) +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

ggplot(runoff_winter_key[year <= 2010], aes(x = year, y = value)) +
  geom_line(col = colset_4[3])+
  geom_point(col = colset_4[3])+
  facet_wrap(~sname, scales = 'free') +
  geom_smooth(method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  geom_smooth(method = 'loess', formula = y~x, se = 0, col = colset_4[4]) +
  scale_color_manual(values = colset_4[c(1, 2, 4)]) +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()


runoff_winter[, value_norm := scale(value), sname]
runoff_summer[, value_norm := scale(value), sname]
n_stations <- nrow(runoff_summary)

# loess

ggplot(runoff_winter[year > 1950 & year < 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Winter runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

# here we see that adding this change made the graph different by removing the upwards trends which
# show up at the ends of the lines of the original graph. 

ggplot(runoff_summer[year > 1950 & year < 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Summer runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

# the same occurs for the summer plot - originally there are upturns in the end of the graph, but by
# limiting it to pre-2010 we have a stronger downwards trend.

# linear regression

ggplot(runoff_winter[year > 1950 & year < 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'lm', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Winter runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

ggplot(runoff_summer[year > 1950 & year < 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'lm', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Summer runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

# The linear regression plots show much more straight-forward trends, with upwards lines for
# increasing winter runoff and downwards for decreasing summer runoff. This indicates that the 
# results of the linear regression analysis might be oversimplified and overlooking variations. 

# Explorer's questions ----------

#1: DOMA: As our graphs showcase, DOMA (Domat/Ems) is the first station
# on the river Rhine. Due to it being the first and upstream, it doesn't have a large
# catchment area. While mid and downstream stations BARS and KOEL have runoff in the 
# thousands of m^3/s, DOMA's remain in the hundreds, as shown in the boxplot.
# Our regression analysis shows vaiations in the changes to outflow to the DOMA station
# as well, particularly in winter. 
# On one hand, DOMA is the first station which means its data is less influenced by other stations
# of the river. It's outflow could be one of the indicators for what's comming to the river.
# However, DOMA might not be the most representative station for the entire river, 
# due to the low levels of runoff and correlation to the other stations. 


#2: 

precip_day <- readRDS('./data/precip_day.rds')
precip_day <- precip_day[date >= '1814-11-01']
precip_day[, .(precipitation = value), (date=date)]

runoff_day
setkey(precip_day, date)
setkey(runoff_day, date)
precip_runoff_day <- precip_day[runoff_day]

precip_runoff_day[, .(runoff = i.value)]

ggplot(precip_runoff_day, aes(year, value, fill = i.value)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()


ggplot() +
  geom_point(data = to_plot, aes(x = mean_day, y = area, col = category), cex = 3) +
  geom_smooth(data = to_plot[c(1:7)], aes(x = mean_day, y = area), 
              method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  geom_smooth(data = to_plot[c(8:11)], aes(x = mean_day, y = area), 
              method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  geom_smooth(data = to_plot[c(12:17)], aes(x = mean_day, y = area), 
              method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  scale_color_manual(values = colset_4[c(2, 3, 4)]) +
  xlab(label = "Area (km3)") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()


ggplot(precip_runoff_day, aes(x = mean(i.value), y = value)) +
  geom_point(aes(col = season), cex = 3) +
  geom_smooth(method = 'lm', formula = y ~ poly(x, 2), se = 0, col = colset_4[1]) +
  scale_color_manual(values = colset_4[c(2, 3, 4)]) +
  xlab(label = "Area (km3)") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()


ggplot(precip_runoff_day, aes(season, value, fill = i.value)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Season") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

ggplot(precip_runoff_day, aes(x = year, y = value)) +
  geom_line(col = colset_4[3])+
  geom_point(col = colset_4[3])+
  facet_wrap(~sname, scales = 'free') +
  geom_smooth(method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  geom_smooth(method = 'loess', formula = y~x, se = 0, col = colset_4[4]) +
  scale_color_manual(values = colset_4[c(1, 2, 4)]) +
  xlab(label = "Year") +
  ylab(label = "Precipitation") +
  theme_bw()

#3: I was somewhat surprised to have seen the variations on the Rhine outflow in the above graph. 
# Whereas I was expecting things to be a somewhat clear correlation between rising temperatures and lower
# runoff, and to some extent this is shown, we also see there was variation in the Rhine outflow in the past,
# indicating there is more natural variation (or due to other unaddressed causes) than was expected.
# If trends continue in the same path, we risk witnessing what the Middelkoop and colleagues study predicted,
# which is dangerously low river flow in the summer, and too high in the winter. 

#4: I would be interested in examining the diversity of life along the Rhine river. Are there changes in the 
# numbers or kinds of fish that live in the river over time? What about the wildlife around the Rhine? How about 
# deforestation, are the natural resources around the river being perserved to help renew the river and keep 
# natural cycles flowing? I would also be interested in pollution levels, whether the river has been adequately 
# maintained by the governing bodies of the countries which depend on it. i would also be interested
# in the groundwater supplies and their availability. Are they being depleted? If so, are they being replenished 
# adequately? 
