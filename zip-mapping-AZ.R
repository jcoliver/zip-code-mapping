# Plotting values by zip code for Arizona
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-02-27

library(sf)
library(dplyr)
library(ggplot2)

# Read in Arizona zip code data
# zip_shapes <- st_read("data/AZ_ZipCodes/AZ_ZipCodes.shp")

# Read in national data, then subset to AZ
zip_shapes <- st_read("data/zcta/tl_2022_us_zcta520.shp")
az_zips <- paste0("8", 5001:6556)
zip_shapes <- zip_shapes[zip_shapes$ZCTA5CE20 %in% az_zips, ]
# Rename the column to align with other potential Arizona zip code source
zip_shapes <- zip_shapes %>%
  rename(ZIP = ZCTA5CE20)

# Lots of census data in there, just want zip codes
zips <- zip_shapes[, c("ZIP", "geometry")]
# plot(zips)

# Read in rurality data
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
zips$ZIP_three <- substr(x = zips$ZIP,
                         start = 1, 
                         stop = 3)

# Join the percentage data with the shapefile data
combined <- zips %>%
  left_join(rural, by = c("ZIP_three" = "zip_three"))

# Now plot
rural_plot <- ggplot(data = combined,
                    mapping = aes(fill = percentage, color = percentage)) +
  geom_sf() +
  scale_fill_distiller(palette = "BrBG") +
  scale_color_distiller(palette = "BrBG") +
  theme_bw()
rural_plot
ggsave(filename = "rural-az.png", plot = rural_plot)
