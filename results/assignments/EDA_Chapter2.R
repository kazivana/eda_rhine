library(data.table)

## Navigator task

temp <- c(3, 6, 10, 14)
weights <- c(1, 0.8, 1.2, 1)

correction <- function(x, y){
  x*y
}

temp_corrected <- correction(temp, weights)

## Explorer tasks

catchment_area <- 185000 #km2
catchment_area <- catchment_area * 1000 ^ 2 #m2

precip_hour <- 5 #mm
precip_day <- precip_hour * 24 / 1000 # m 
water_over_catchment <- precip_day * catchment_area

river_runoff <- 2900 #m3 / s
river_runoff_day <- river_runoff * 60 * 60 * 24 #m3/d

water_over_catchment / river_runoff_day

# 88.60153 times larger than daily discharge

river_length <- 1233000 #m
river_depth <- 30 #m
river_width <- 200 #m

water_velocity <- river_runoff / (river_depth * river_width) #m/s

time_to_sea <- river_length / water_velocity #s
time_to_sea <- time_to_sea / (60 * 60 * 24) 

# Question 4. 

# a. Which other hydroclimatic changes reported in the article and not discussed above?
# They also looked at snowmelt and ground water storage. They noted that due to the impacts of changing temperatures
# there was a higher frequency of floods in winter months, and lower water discharge in the summer. 


# b. Can you detect three assumptions made by Middelkoop et al. (2001)?
# i) That according to the Intergovernmental Panel on Climate Change (IPCC, 1996) the global average temperature is
#      likely to increase between 1 and 3.5 degrees Celcius over the century.
# ii) This change in average temperature will have an impact on the hydrological cycle, by affecting the precipitation
#       and evaporation rates. This will then have an impact on water availability and runoff.
# iii) That this will have a significant socioeconomic impact on those who rely on the Rhine river, which spans across 
#      large populations and highly industrialised areas in Europe. 


# c. Why Middelkoop and his colleagues made this study? Why is it important?
# This study was important because it provided insights into the ways climate change is impacting discharge from the 
# second biggest river in Europe. They looked for trends which showcase how the rate of discharge might change, which
# will surely have an impact on the countries which are involved. If the flow of the river shows vulnerability to changes
# in climate, that could have huge impacts on various aspects of society, not least of which are industry and agriculture,
# increased incidence of flooding in winter, freshwater availability in the summer, and more. 


# d. Are there other studies that have a similar analysis over Rhine, or a similar hypothesis in other regions?

# i) L. Phil Graham "Climate Change Effects on River Flow to the Baltic Sea," AMBIO: A Journal of the Human Environment 33(4), 235-241, (1 June 2004). https://doi.org/10.1579/0044-7447-33.4.235
# ii) W. Collischonn,, C.E.M. Tucci, R.T. Clarke "Further evidence of changes in the hydrological regime of the River Paraguay: part of a wider phenomenon of climate change?," Journal of Hydrology, 245, (1 May 2001).https://doi.org/10.1016/S0022-1694(01)00348-1
# iii) Nigel W. Arnell, Simon N. Gosling "The impacts of climate change on river flow regimes at the global scale," Journal of Hydrology, 486, (12 April 2013)


# e. Is there any evidence in the news about low or high flow events of Rhine since 2000?
# Winter, 2020, high flow leading to flooding: "Rhine level in Bonn is currently rising rapidly"
# Link: https://www.general-anzeiger-bonn.de/ga-english/news/rhine-level-in-bonn-is-currently-rising-rapidly_aid-48739343
# Summer, 2019, low flow leading to drought: "Europe's Most Important River Risks a Repeat of Historic Shutdown"
# Link: https://www.bloomberg.com/news/articles/2019-07-23/the-rhine-river-risks-a-repeat-of-last-year-s-historic-shutdown












