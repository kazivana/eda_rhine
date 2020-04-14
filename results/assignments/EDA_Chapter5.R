
library(data.table) 
library(ggplot2) 

colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")

runoff_summary <- readRDS('./data/runoff_summary.rds')
runoff_stats <- readRDS('./data/runoff_stats.rds')
runoff_day <- readRDS('./data/runoff_day.rds')
runoff_month <- readRDS('./data/runoff_month.rds')
runoff_summer <- readRDS('./data/runoff_summer.rds')
runoff_winter <- readRDS('./data/runoff_winter.rds')
runoff_year <- readRDS('./data/runoff_year.rds')

runoff_summary_key <- readRDS('data/runoff_summary_key.rds')
runoff_day_key <- readRDS('data/runoff_day_key.rds')
runoff_month_key <- readRDS('data/runoff_month_key.rds')
runoff_summer_key <- readRDS('data/runoff_summer_key.rds')
runoff_winter_key <- readRDS('data/runoff_winter_key.rds')
runoff_year_key <- readRDS('data/runoff_year_key.rds')

runoff_winter[, value_norm := scale(value), sname]
runoff_summer[, value_norm := scale(value), sname]

key_stations <- c('DOMA', 'BASR', 'KOEL')
n_stations <- nrow(runoff_summary)


# Navigator tasks --------
 
#1: Yearly and monthly boxplots

runoff_year_key <- runoff_year_key[sname %in% key_stations]
runoff_year_key[year <= 2000, period := factor('pre_2000')]
runoff_year_key[year > 2000, period := factor('aft_2000')]

ggplot(runoff_year_key, aes(period, value, fill = period)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  theme_bw()

runoff_month_key <- runoff_month[sname %in% key_stations]
runoff_month_key[year <= 2000, period := factor('pre_2000')]
runoff_month_key[year > 2000, period := factor('aft_2000')]


ggplot(runoff_month_key, aes(factor(month), value, fill = period)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  theme_bw()

#2: 

runoff_day_key <- runoff_day[sname %in% key_stations]
runoff_day_key[year <= 1986, period := factor('pre_1986')]
runoff_day_key[year > 1986, period := factor('aft_1986')]
runoff_day_key[, q_10 := quantile(value, 0.1), by = .(sname, month)]
runoff_day_key[, q_90 := quantile(value, 0.9), by = .(sname, month)]
runoff_day_key[, runoff_type := factor('normal')]
runoff_day_key[value <= q_10, runoff_type := factor('low')]
runoff_day_key[value >= q_90, runoff_type := factor('high')]
runoff_day_key[, n_days := .N, .(sname, runoff_type, season, year)]

to_plot <- unique(runoff_day_key[, .(sname, n_days, period, runoff_type, season, year)])

ggplot(to_plot[season == 'winter' | season == 'summer'], aes(season, n_days, fill = period)) +
  geom_boxplot() +
  facet_wrap(runoff_type~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Season") +
  ylab(label = "Number of days") 


#3:
#loess

ggplot(runoff_winter[year > 1950 & year < 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  ggtitle('Winter runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

ggplot(runoff_summer[year > 1950 & year < 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  ggtitle('Summer runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

# linear

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

precip_day <- readRDS('./data/raw/precip_day.rds')
precip_day[, year := year(date)]
precip_day[, month := month(date)]
precip_day <- precip_day[year < 2019]

precip_day[month == 12 | month == 1 | month == 2, season := 'winter']
precip_day[month == 3 | month == 4 | month == 5, season := 'spring']
precip_day[month == 6 | month == 7 | month == 8, season := 'summer']
precip_day[month == 9 | month == 10 | month == 11, season := 'autumn']
precip_day[, season := factor(season, levels = c('winter', 'spring', 'summer', 'autumn'))]

precip_winter <- precip_day[season == 'winter', .(value = sum(value)), by = year]
precip_summer <- precip_day[season == 'summer', .(value = sum(value)), by = year]

year_thres <- 1980
to_plot <- rbind(cbind(precip_winter, season = factor('winter')), 
                 cbind(precip_summer, season = factor('summer'))) 

to_plot[year < year_thres, period := factor('1950-1980')]
to_plot[year >= year_thres, period := factor('1981-2016')]
to_plot[year < year_thres, period := factor('1950-1980')]
to_plot[year >= year_thres, period := factor('1981-2016')]

to_plot <- to_plot[year >= 1950]

ggplot(to_plot, aes(season, value, fill = period)) +
  geom_boxplot() +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Season") +
  ylab(label = "Precitation (mm)") +
  theme_bw()

ggplot(to_plot, aes(year, value, col = season)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = colset_4[c(4, 1)]) +
  geom_smooth(se = F) +
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
