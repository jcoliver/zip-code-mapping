# Plotting values by zip code for U.S.
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-02-27

library(sf)
library(dplyr)
library(ggplot2)

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

# For now, skip AK & HI
# AK: 99501 to 99950
# HI: 96701 to 96898
ak <- 501:950
hi <- 701:898
ak_five <- paste0("99", ak)
hi_five <- paste0("96", hi)
ak_hi <- c(ak_five, hi_five)
zips <- zips[!(zips$ZCTA5CE20 %in% ak_hi), ]

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

# Need to make a three-digit zip in the shapefile
zips$ZIP_three <- substr(x = zips$ZCTA5CE20,
                         start = 1, 
                         stop = 3)

# Join the percentage data with the shapefile data
combined <- zips %>% # If we try to join the other direction, we lose geometry
  left_join(rural, by = c("ZIP_three" = "zip_three"))

# TODO: given that we are looking at three-digit zip codes, it would be nice to
# combine all those polygons, but maybe something for another time

# Now plot
rural_plot <- ggplot(data = combined,
                    mapping = aes(fill = percentage, color = percentage)) +
  geom_sf() +
  scale_fill_distiller(palette = "BrBG") +
  scale_color_distiller(palette = "BrBG") +
  theme_bw()
rural_plot
ggsave(filename = "rural-us.png", plot = rural_plot)
