# Plotting values by zip code
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-02-27

library(sf)
library(dplyr)
library(ggplot2)

# Read in Arizona zip code data
zip_shapes <- st_read("data/AZ_ZipCodes/AZ_ZipCodes.shp")

# Lots of census data in there, just want zip codes
zips <- zip_shapes[, c("ZIP", "geometry")]
plot(zips)

# Reading in just one file
high <- read.csv(file = "data/high.csv")

# Need to get Zip codes back to five-digit
high <- high %>%
  mutate(zip_char = as.character(three_digit_zip)) %>%
  mutate(zip_five = case_when(nchar(zip_char) == 2 ~ paste0("850", zip_char),
                            nchar(zip_char) == 3 ~ paste0("85", zip_char),
                            TRUE ~ NA_character_)) %>%
  select(zip_five, percentage)

# Join the percentage data with the shapefile data
combined <- zips %>%
  left_join(high, by = c("ZIP" = "zip_five"))

# Now plot
high_plot <- ggplot(data = combined,
                    mapping = aes(fill = percentage, color = percentage)) +
  geom_sf() +
  scale_fill_distiller(palette = "BrBG") +
  scale_color_distiller(palette = "BrBG") +
  theme_bw()
ggsave(filename = "example-high.png", plot = high_plot)
