###Tracy Shane, Ph.D.
###Wildland Fire Acres in Nevada and Drought Interactions
###04-21-2026

library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)


#grab the fire polygons
fire_csv25 <- read.csv("C:/Users/tshane/Box/Extension/training/rangeland_ecology_app/fire_figure/fire_polys_nv2025.csv")


group_ac <- fire_csv25 %>%
  group_by(year = fire_year) %>%
  summarise(sum_ac = sum(acres))

group_ac06_25 <- subset(group_ac, year >= 1995)
group_ac95_25 <- subset(group_ac, year >= 1995)

#grab the dsci files
dcsi <- read.csv("C:/Users/tshane/Box/Extension/training/rangeland_ecology_app/fire_figure/dsci_export_06_25.csv")
dcsi95 <- read.csv("C:/Users/tshane/Box/Extension/training/rangeland_ecology_app/fire_figure/dsci_1995_2026.csv")
dcsi <- dcsi95


# Data with dates and variables. The column 'date' is of class "Date"
dcsi$MapDate <- ymd(dcsi$MapDate)

ggplot(dcsi, aes(x = MapDate, y = DSCI)) +
  geom_area(fill = "orange", alpha = 0.9) +
  geom_line()

dcsi$Date <- dcsi$MapDate

group_ac95_25d <- group_ac95_25 %>%
  mutate(year = as.Date(paste0(year, "-09-01")))  # midpoint of year

group_ac95_25d$Date <- as.Date(paste0(group_ac95_25d$year, "-09-01"))

ggplot(group_ac95_25d, aes(x = year, y = sum_ac)) +
  geom_line()


range_ac   <- range(group_ac95_25d$sum_ac, na.rm = TRUE)
range_dcsi <- range(dcsi$DSCI, na.rm = TRUE)

b <- diff(range_ac) / diff(range_dcsi)
a <- range_ac[1] - b * range_dcsi[1]

# ggplot() +
#   geom_area(data = dcsi, aes(x = Date, y = DSCI), fill = "orange", alpha = 0.9) + 
#   geom_line() +
# 
#   geom_line(data = group_ac06_25d,
#             aes(x = Date, y = a + b * sum_ac),
#             color = "blue", linewidth = 1) +
#   
#   scale_y_continuous(
#     "Acres",
#     sec.axis = sec_axis(~ (. - a)/b, name = "DSCI")
#   )


ggplot() +
  geom_area(data = dcsi,
            aes(x = Date, y = a + b * DSCI),
            fill = "orange", alpha = 0.9) + 
  
  geom_line(data = group_ac95_25d,
            aes(x = Date, y = sum_ac),
            color = "blue", linewidth = 1) +
  
  scale_y_continuous(
    name = "Acres",
    sec.axis = sec_axis(~ (. - a)/b, name = "DSCI")
  )


#########
#Now precip anomoly figure
#########

library(FedData)
library(terra)
library(ncdf4)

southernsage <- vect("C:/Users/tshane/Box/Extension/training/rangeland_ecology_app/southern_GB_clip.shp")

daymet_crs <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
sage_proj <- project(southernsage, daymet_crs)
sage_proj84 <- project(southernsage, "EPSG:4326")


eurekaco <- vect("C:/Users/tshane/Box/Extension/training/rangeland_ecology_app/eureka_co_intersect.shp")
eureka_proj <- project(eurekaco, daymet_crs)

print(FedData::sagebrush())
unlink(sagebrush, recursive = TRUE)


meve <- FedData::meve
# template = your study area polygon (sf or SpatRaster)
daymet <- get_daymet(
  template = meve,
  label = "meve",
  elements = c("prcp"),
  years = 2020
)

# Convert daily to annual totals
annual_prcp <- tapp(daymet, index = rep(1980:2023, each = 365), fun = sum)