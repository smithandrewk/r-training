install.packages("sf")   # For spatial data handling
install.packages("ggplot2")  # For data visualization
library(sf)
library(ggplot2)
#https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
#https://r-spatial.org/book/
# Replace "path_to_shapefile" with the actual path to your downloaded shapefile.
sc_counties <- st_read("/Users/andrew/Downloads/cb_2018_us_county_500k/cb_2018_us_county_500k.shp")
#https://www.bls.gov/respondents/mwr/electronic-data-interchange/appendix-d-usps-state-abbreviations-and-fips-codes.htm
# Plot the map
ggplot() +
  geom_sf(data = sc_counties, fill = "white", color = "black") +
  coord_sf(datum = NA) +
  theme_minimal()

# Generate some fake data for disease prevalence
set.seed(42) # Setting a seed for reproducibility
fake_data <- data.frame(
  NAME = sc_counties$NAME, # Assuming the shapefile has a column 'NAME' with county names
  Disease_Prevalence = runif(nrow(sc_counties), 1, 15) # Generating random prevalence values between 1 and 15%
)

# Merge the fake data with the county shapefile data
sc_counties_data <- merge(sc_counties, fake_data, by = "NAME")

# Plot the county-level map with the disease prevalence data
ggplot() +
  geom_sf(data = sc_counties_data, aes(fill = Disease_Prevalence), color = "white") +
  scale_fill_viridis_c(name = "Disease Prevalence (%)", label = scales::percent_format(scale = 1)) +
  coord_sf(datum = NA) +
  theme_minimal()

ggplot() +
  geom_sf(data = sc_counties_data, aes(fill = Disease_Prevalence), color = "white") +
  scale_fill_distiller(name = "Disease Prevalence (%)", label = scales::percent_format(scale = 1)) +
  coord_sf(datum = NA) +
  theme_minimal()

ggplot() +
  geom_sf(data = sc_counties_data, aes(fill = Disease_Prevalence), color = "white") +
  scale_fill_distiller(name = "Disease Prevalence (%)", label = scales::percent_format(scale = 1),palette='RdPu') +
  coord_sf(datum = NA) +
  theme_minimal()

install.packages("scico")
library(scico)
ggplot() +
  geom_sf(data = sc_counties_data, aes(fill = Disease_Prevalence), color = "white") +
  scale_fill_scico(name = "Disease Prevalence (%)", label = scales::percent_format(scale = 1),palette='bilbao') +
  coord_sf(datum = NA) +
  theme_minimal()

ggplot() +
  geom_sf(data = sc_counties_data, aes(fill = Disease_Prevalence), color = "white") +
  scale_fill_scico(name = "Disease Prevalence (%)", label = scales::percent_format(scale = 1),palette='vik') +
  coord_sf(datum = NA) +
  theme_minimal()

ggplot() +
  geom_sf(data = sc_counties_data, aes(fill = Disease_Prevalence), color = "white") +
  scale_fill_continuous(name = "Disease Prevalence (%)", label = scales::percent_format(scale = 1),low='white',high='black') +
  coord_sf(datum = NA) +
  theme_minimal()

ggplot() +
  geom_sf(data = sc_counties_data, aes(fill = Disease_Prevalence), color = "white") +
  scale_fill_continuous(name = "Disease Prevalence (%)", label = scales::percent_format(scale = 1),low='#000000',high='#FFFFFF') +
  coord_sf(datum = NA) +
  theme_minimal()

library(wesanderson)
library(ggsci)
pal <- wes_palette("GrandBudapest1", 100, type = "continuous")

# Plot the county-level map with the disease prevalence data
ggplot() +
  geom_sf(data = sc_counties_data, aes(fill = Disease_Prevalence), color = "black") +
  scale_fill_gradientn(name = "Disease Prevalence (%)", label = scales::percent_format(scale = 1),colours = pal) +
  coord_sf(datum = NA) +
  theme_minimal()

# Plot the county-level map with the disease prevalence data
ggplot() +
  geom_sf(data = sc_counties_data, aes(fill = Disease_Prevalence), color = "black") +
  scale_color_jco(name = "Disease Prevalence (%)", label = scales::percent_format(scale = 1)) +
  coord_sf(datum = NA) +
  theme_minimal()

library(readr)
data <- read_csv("R_training/data.csv")
View(data)

df <- data.frame(NAME=names(table(data$COUNTY)),VALUE=as.numeric(table(data$COUNTY)))
rownames(sc_counties) <- toupper(sc_counties$NAME)
sc_counties[df$NAME,"VALUE"] <- df$VALUE
data[data$COUNTY == "OCONEEE","COUNTY"] <- "OCONEE"
df <- data.frame(NAME=names(table(data$COUNTY)),VALUE=as.numeric(table(data$COUNTY)))
rownames(sc_counties) <- toupper(sc_counties$NAME)
sc_counties[df$NAME,"VALUE"] <- df$VALUE
