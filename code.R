library(tigris)
library(tidyverse)
library(ggplot2)
library(sf)
library(mapview)
library(conflicted)
library(rnaturalearth)

#### Inset map bounds ####
# states
states <- states(year = 2022) %>%
  filter(!GEOID %in% c("60", "66", "69", "72", "78", "15", "02")) %>%
  st_transform("EPSG:5070")
# river
mississippi <- ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", returnclass = "sf") %>%
  filter(name == "Mississippi") %>%
  st_transform("EPSG:5070")

#### MEMPHIS BOUNDS ####
# arkansas
ar <- tracts(state = "AR", county = "Crittenden") %>%
select(geometry) %>%
  st_make_valid()

# tennessee
tn <- tracts(state = "TN", county = "Shelby") %>%
  select(geometry) %>%
  st_make_valid()

# mississipi
ms <- tracts(state = "MS", county = "DeSoto") %>%
  select(geometry) %>%
  st_make_valid()

# city bounds 
memphis_bound <- places(state = "TN", year = 2020) %>%
  filter(NAME == "Memphis")

# city centroid buffer
memphis_buff <- st_centroid(memphis_bound) %>%
  st_buffer(20000) 

# background tracts
memphis_bg <- rbind(ar, tn, ms) %>%
  st_intersection(memphis_buff) %>%
  erase_water()
  
#### MINNEAPOLIS & ST PAUL BOUNDS ####
# minnesota
mn <- tracts(state = "MN") %>%
  select(geometry) %>%
  st_make_valid()

# wisconsin
wi <- tracts(state = "WI") %>%
  select(geometry) %>%
  st_make_valid()

# city bounds 
minne_bound <- places(state = "MN", year = 2020) %>%
  filter(NAME == "Minneapolis")

stpaul_bound <- places(state = "MN", year = 2020) %>%
  filter(NAME == "St. Paul")

minne_stpaul <- rbind(minne_bound, stpaul_bound) %>%
  st_union()

# city centroid buffer
minne_stpaul_buff <- st_centroid(minne_stpaul) %>%
  st_buffer(20000)

# background tracts
minne_stpaul_bg <- rbind(wi, mn) %>%
  st_intersection(minne_stpaul_buff) %>%
  erase_water()

#### NOLA BOUNDS ####
# louisiana
la <- tracts(state = "LA") %>%
  select(geometry) %>%
  st_make_valid()

# city bounds 
nola_bound <- places(state = "LA", year = 2020) %>%
  filter(NAME == "New Orleans")

# city centroid buffer
nola_buff <- st_centroid(nola_bound) %>%
  st_buffer(30000)

# background tracts
nola_bg <- la %>%
  st_intersection(nola_buff) %>%
  erase_water()

# city tracts
nola_tracts <- nola_bg %>%
  st_make_valid() %>%
  st_intersection(nola_bound)

#### MAPS ####
#nola 
ggplot() +
  geom_sf(data = nola_buff, fill = "lightblue1", color = NA) +
  geom_sf(data = nola_bg, color = "gray20", fill = "gray10") +
  theme_void()

#twin cities
ggplot() +
  geom_sf(data = minne_stpaul_buff, fill = "lightblue1", color = NA) +
  geom_sf(data = minne_stpaul_bg, color = "gray20", fill = "gray10") +
  theme_void()

#twin cities
ggplot() +
  geom_sf(data = memphis_buff, fill = "lightblue1", color = NA) +
  geom_sf(data = memphis_bg, color = "gray20", fill = "gray10") +
  theme_void()

# city bounds (for masking river)
buffer_all <- st_union(minne_stpaul_buff, nola_buff) %>%
  st_union(memphis_buff) %>%
  st_transform("EPSG:5070")

# river highlights
river_hilite <- mississippi %>%
  st_intersection(buffer_all)

# inset map
ggplot() +
  geom_sf(data = st_union(states), fill = "gray13", color = NA) +
  geom_sf(data = mississippi, color = "lightblue1", alpha = 0.2) +
  geom_sf(data = river_hilite, color = "lightblue1") +
  theme_void()
