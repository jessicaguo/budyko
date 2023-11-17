# To calculate PET/P 
# and draw a Budyko curve for places I have lived/done research

# 30 yr normals
# Need Ra, Rs, T ,and RH
# PRISM data form RS, T, and Dewpoint T
library(prism)
library(tidyverse)
library(weathermetrics)
library(SPEI)

prism_set_dl_dir('data/prism')

get_prism_normals(type = "tmean",
                  resolution = "800m",
                  mon = 1:12,
                  annual = FALSE,
                  keepZip = FALSE)

get_prism_normals(type = "tmin",
                  resolution = "800m",
                  mon = 1:12,
                  annual = FALSE,
                  keepZip = FALSE)

get_prism_normals(type = "tmax",
                  resolution = "800m",
                  mon = 1:12,
                  annual = FALSE,
                  keepZip = FALSE)

# get_prism_normals(type = "tdmean",
#                   resolution = "800m",
#                   annual = TRUE)

get_prism_normals(type = "ppt",
                  resolution = "800m",
                  mon = 1:12,
                  annual = FALSE,
                  keepZip = FALSE)

# Manually downloaded solar radiation (horizontal), version M3
# Other vars are M4

# Stack products
pd_ras <- pd_stack(prism_archive_ls())
raster::crs(pd_ras)

# Import lat/lons and extract
locs <- read_csv("data/locs.csv") %>%
  select(site, lat, lon) %>%
  relocate(site, lon, lat) %>% # need to give in x, y
  as_tibble() %>%
  tibble::column_to_rownames(var = "site")

# extract
env <- raster::extract(pd_ras, locs) %>%
  data.frame() %>%
  cbind(locs) %>%
  tibble::rownames_to_column("site") %>%
  pivot_longer(starts_with("PRISM"),
               names_to = "temp",
               values_to = "value") %>%
  mutate(var = str_extract(temp, "_([^_]+)_"),
         month = str_extract(temp, "(\\d{2})_bil"),
         var = str_extract(var, "(?<=_)[a-zA-Z]+(?=_)"),
         month = str_extract(month, "\\d{2}") %>%
           as.numeric()) 
  
# Calculate Thornthwaite PET by month
temp <- env %>%
  filter(var == "tmean") %>%
  group_by(site) %>%
  reframe(PET_thorn = thornthwaite(value, lat = unique(lat))) %>%
  mutate(month = rep(1:12, nrow(locs)))
  

ppt_only <- env %>%
  filter(var == "ppt")

AI_thorn <- ppt_only %>%
  left_join(temp) %>%
  mutate(AI = round(PET_thorn / value, 3))
  # group_by(site) %>%
  # summarize(AI_mean = mean(AI),
  #           AI_sd = sd(AI))

ggplot() +
  geom_point(data = ppt_only, aes(x = month, y = value,
                                  color = "PPT")) +
  geom_point(data = temp, aes(x = month, y = PET_thorn,
                              color = "PET")) +
  facet_wrap(~site)

ggplot(AI_thorn) +
  geom_point(aes(x = month, y = AI)) +
  facet_wrap(~site, scales = "free_y")

AI_thorn_ann <- AI_thorn %>%
  filter(!is.infinite(AI)) %>%
  group_by(site) %>%
  summarize(AI_mean = mean(AI),
            AI_max = max(AI),
            AI_med = median(AI),
            AI_sd = sd(AI),
            ppt_tot = sum(value),
            pet_thorn_tot = sum(PET_thorn)) %>%
  mutate(AI_thorn_annual = pet_thorn_tot/ppt_tot)
  

# Calculate Hargreaves PET by month
temp2 <- env %>%
  select(-temp) %>%
  filter(var %in% c("tmin", "tmax", "ppt")) %>%
  pivot_wider(names_from = "var",
              values_from = "value") %>%
  group_by(site) %>%
  reframe(PET_harg = hargreaves(tmin, tmax, 
                                lat = unique(lat),
                                Pre = ppt)) %>%
  mutate(month = rep(1:12, nrow(locs)))


ppt_only <- env %>%
  filter(var == "ppt")

AI_harg <- ppt_only %>%
  left_join(temp2) %>%
  mutate(AI = round(PET_harg / value, 3))

ggplot() +
  geom_point(data = ppt_only, aes(x = month, y = value,
                                  color = "PPT")) +
  geom_point(data = temp2, aes(x = month, y = PET_harg,
                              color = "PET")) +
  facet_wrap(~site)

ggplot(AI_harg) +
  geom_point(aes(x = month, y = AI)) +
  facet_wrap(~site, scales = "free_y")

AI_harg_ann <- AI_harg %>%
  filter(!is.infinite(AI)) %>%
  group_by(site) %>%
  summarize(AI_mean = mean(AI),
            AI_max = max(AI),
            AI_med = median(AI),
            AI_sd = sd(AI),
            ppt_tot = sum(value),
            pet_harg_tot = sum(PET_harg)) %>%
  mutate(AI_harg_annual = pet_harg_tot/ppt_tot)

# Combine and save out
pet_comb <- cbind.data.frame(AI_thorn_ann[,c(1,6:8)], AI_harg_ann[,7:8])
write_csv(pet_comb, file = "data/AI_ann.csv")
