# Plotting values by zip code for U.S.
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-02-27

library(sf)
library(dplyr)
library(ggplot2)
library(maps)

# https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.2022.html#list-tab-EICGI7KCL6PKK63IWP
zip_shapes <- st_read("data/zcta/tl_2022_us_zcta520.shp")

# Lots of census data in there, just want zip codes
zips <- zip_shapes[, c("ZCTA5CE20", "geometry")]
# plot(zips)

# Will remove leading zeros if we do not explicitly say the column is character
territories <- read.csv(file = "data/territory-zip-codes.csv", 
                        colClasses = c("character", "character"))

# Includes all of U.S., including PR, Guam, HI, AK, et al.
# remove territories
zips <- zips[!(zips$ZCTA5CE20 %in% territories$zip), ]

# Need to make a three-digit zip in the shapefile
zips$ZIP_three <- substr(x = zips$ZCTA5CE20,
                         start = 1, 
                         stop = 3)

# Reading in the rural/urban classification file
rural <- read.csv(file = "data/rural_classification.csv")

# Need to get Zip codes back to three-digit (leading zeros are removed)
rural <- rural %>%
  mutate(zip_char = as.character(three_digit_zip)) %>%
  mutate(zip_three = case_when(nchar(zip_char) == 2 ~ paste0("0", zip_char),
                               nchar(zip_char) == 3 ~ zip_char,
                               TRUE ~ NA_character_)) %>%
  filter(zip_tag == "Positive") %>%
  select(zip_three, percentage, three_digit_zip)

# Join the percentage data with the shapefile data
combined <- zips %>% # If we try to join the other direction, we lose geometry
  left_join(rural, by = c("ZIP_three" = "zip_three"))

# TODO: given that we are looking at three-digit zip codes, it would be nice to
# combine all those polygons, but maybe something for another time

# Need to identify Alaska and Hawaii zip codes for separate maps
# AK: 99501 to 99950
# HI: 96701 to 96898
ak <- 501:950
hi <- 701:898
ak_five <- paste0("99", ak)
hi_five <- paste0("96", hi)
ak_hi <- c(ak_five, hi_five)

# Pull out AK and HI for separate maps
ak_zips <- combined[combined$ZCTA5CE20 %in% ak_five, ]
hi_zips <- combined[combined$ZCTA5CE20 %in% hi_five, ]
# Make just lower 48 object
cont_zips <- combined[!(combined$ZCTA5CE20 %in% ak_hi), ]

# Manually creating a palette to get a yellow-ish midpoint
rural_palette <- colorRampPalette(c("#01665e", "#ffffb2", "#8c510a"))
rural_colors <- rural_palette(50)

# Before plotting, we want to get state borders, too. Requires maps package
# Alaska and Hawaii aren't included in states map (!?!), so we use the world
# and crop it
usa_map <- st_as_sf(map("world", regions = "USA", plot = FALSE, fill = TRUE))
cont_map <- st_crop(x = usa_map,
                    y = c(xmin = -128, ymin = 20, xmax = -35, ymax = 50))

# For plotting political (state) boundaries, we start with the map layer for 
# fill, to indicate no/missing data;
# Next we plot the actual percentage rural
# Finally we draw the border (color) with the map again

# Now plot lower 48
cont_plot <- ggplot(data = cont_zips,
                    mapping = aes(fill = percentage)) +
  geom_sf(data = cont_map, fill = "#D4D4D4", color = NA) +
  geom_sf(color = NA) + 
  scale_fill_gradientn(colors = rural_colors,
                       limits = c(0, 100),
                       name = "% Rural") +
  # geom_sf(data = cont_map, fill = NA, color = "#000000", lwd = 0.25) +
  theme_bw()
cont_plot
ggsave(filename = "rural-us.png", plot = cont_plot)

# Plot Alaska
# Cludgey crop
ak_map <- st_crop(x = usa_map, 
                  y = c(xmin = -179, ymin = 50, xmax = -130, ymax = 75))

ak_plot <- ggplot(data = ak_zips,
                    mapping = aes(fill = percentage)) +
  geom_sf(data = ak_map, fill = "#D4D4D4", color = NA) +
  geom_sf(color = NA) +
  scale_fill_gradientn(colors = rural_colors,
                        limits = c(0, 100),
                       name = "% Rural") +
  # geom_sf(data = ak_map, fill = NA, color = "#000000", lwd = 0.25) +
  theme_bw()
ak_plot
ggsave(filename = "rural-ak.png", plot = ak_plot)

# Plot Hawaii
# Everything is a nail
hi_map <- st_crop(x = usa_map,
                  y = c(xmin = -160, ymin = 18.5, xmax = -154.5, ymax = 22.5))
hi_plot <- ggplot(data = hi_zips,
                  mapping = aes(fill = percentage)) +
  geom_sf(data = hi_map, fill = "#D4D4D4", color = NA) +
  geom_sf(color = NA) +
  scale_fill_gradientn(colors = rural_colors,
                       limits = c(0, 100),
                       name = "% Rural") +
  # geom_sf(data = hi_map, fill = NA, color = "#000000", lwd = 0.25) +
  theme_bw()
hi_plot
ggsave(filename = "rural-hi.png", plot = hi_plot)
