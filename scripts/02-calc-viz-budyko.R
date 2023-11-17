# Read in calculated annual AI and calculate place on Budyko curve
# Make series of plots

library(tidyverse)

pet_in <- read_csv("data/AI_ann.csv")

# Function from Budyko 1974
budyko <- function(PET, P) {
  ae_p = (PET/P * tanh(P/PET) * (1- exp(-PET/P)))^0.5
  return(ae_p)
}

budyko2 <- function(AI) {
  ae_p = (AI * tanh(1/AI) * (1- exp(-AI)))^0.5
  return(ae_p)
}

# Calculate AET / P for y axis 
pet_budyko <- pet_in %>%
  mutate(AET_thorn = budyko(pet_thorn_tot, ppt_tot),
         AET_harg = budyko(pet_harg_tot, ppt_tot))

# dataframe for line
bud_line <- data.frame(x = seq(0, 21, by = 0.1),
                       y = budyko2(seq(0, 21, by = 0.1)))


pet_budyko %>%
  filter(site != "Death Valley") %>%
  ggplot() +
  geom_line(data = bud_line,
            aes(x = x,
                y = y),
            color = "gray") +
  geom_point(aes(x = AI_thorn_annual,
                 y = AET_thorn,
                 col = site),
             size = 3) +
  geom_hline(yintercept = 1,
             lty = 2) +
  geom_vline(xintercept = 1,
             lty = 2) +
  scale_x_continuous("PET/P", limits = c(0, 21)) +
  scale_y_continuous("AET/P", limits = c(0, 1.2)) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())


#### order dataset and make full set of graphs

places <- c("Phoenix", "NYC", "Flagstaff", "Salt Lake City", "Tucson")

pet_budyko_1 <- pet_budyko %>%
  filter(site %in% places) %>%
  mutate(site = factor(site, levels = places)) %>%
  arrange(site)

for(i in 1:length(places)) {
  fig <- pet_budyko_1[1:i,] %>%
  ggplot() +
    geom_hline(yintercept = 1,
               lty = 2) +
    geom_vline(xintercept = 1,
               lty = 2) +
    geom_line(data = bud_line,
              aes(x = x,
                  y = y),
              color = "gray") +
    geom_point(aes(x = AI_thorn_annual,
                   y = AET_thorn,
                   col = site),
               size = 3) +
    scale_x_continuous("PET/P", limits = c(0, 21)) +
    scale_y_continuous("AET/P", limits = c(0, 1.2)) +
    scale_color_brewer(type = "qual", palette = 3) +
    theme_bw(base_size = 12) +
    theme(panel.grid = element_blank(),
          legend.title = element_blank())
  
  ggsave(paste0("plots/initial/fig_", i, ".png"),
         plot = fig,
         width = 6,
         height = 3,
         units = "in")
}

### make 2nd set of figures, with first one grayed out
places <- c("Phoenix", "NYC", "Flagstaff", "Salt Lake City", "Tucson")
cities <- c("Los Angeles", "Phoenix", "San Diego", "Las Vegas", 
             "Salt Lake City", "Sacramento")

pet_budyko_1 <- pet_budyko %>%
  filter(site %in% places) %>%
  mutate(site = factor(site, levels = places)) %>%
  arrange(site)

pet_budyko_2 <- pet_budyko %>%
  filter(site %in% cities) %>%
  mutate(site = factor(site, levels = cities)) %>%
  arrange(site)

fig_cities <- ggplot() +
    geom_hline(yintercept = 1,
               lty = 2) +
    geom_vline(xintercept = 1,
               lty = 2) +
    geom_line(data = bud_line,
              aes(x = x,
                  y = y),
              color = "gray") +
    geom_point(data = pet_budyko_1,
               aes(x = AI_thorn_annual,
                   y = AET_thorn),
               size = 3,
               color = "gray",
               alpha = 0.25) +
  geom_point(data = pet_budyko_2,
             aes(x = AI_thorn_annual,
                 y = AET_thorn,
                 col = site),
             size = 3) +
    scale_x_continuous("PET/P", limits = c(0, 21)) +
    scale_y_continuous("AET/P", limits = c(0, 1.2)) +
    scale_color_brewer(type = "seq", palette = 3, direction = -1) +
    theme_bw(base_size = 12) +
    theme(panel.grid = element_blank(),
          legend.title = element_blank())
  
ggsave(paste0("plots/fig_cities.png"),
         plot = fig_cities,
         width = 6,
         height = 3,
         units = "in")

### make 3rd set of figures, with first two grayed out
places <- c("Phoenix", "NYC", "Flagstaff", "Salt Lake City", "Tucson")
cities <- c("Los Angeles", "Phoenix", "San Diego", "Las Vegas", 
            "Salt Lake City", "Sacramento")
ag <- c("San Diego", "Fresno", "Sacramento", "Tucson", "El Centro", "Yuma")

pet_budyko_12 <- pet_budyko %>%
  filter(site %in% c(places, cities))

pet_budyko_3 <- pet_budyko %>%
  filter(site %in% ag) %>%
  mutate(site = factor(site, levels = ag)) %>%
  arrange(site)

fig_ag <- ggplot() +
  geom_hline(yintercept = 1,
             lty = 2) +
  geom_vline(xintercept = 1,
             lty = 2) +
  geom_line(data = bud_line,
            aes(x = x,
                y = y),
            color = "gray") +
  geom_point(data = pet_budyko_12,
             aes(x = AI_thorn_annual,
                 y = AET_thorn),
             size = 3,
             color = "gray",
             alpha = 0.25) +
  geom_point(data = pet_budyko_3,
             aes(x = AI_thorn_annual,
                 y = AET_thorn,
                 col = site),
             size = 3) +
  scale_x_continuous("PET/P", limits = c(0, 21)) +
  scale_y_continuous("AET/P", limits = c(0, 1.2)) +
  scale_color_brewer(type = "qual", palette = 1) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank())

ggsave(paste0("plots/fig_ag.png"),
       plot = fig_ag,
       width = 6,
       height = 3,
       units = "in")

### make another figures, with first two grayed out
places <- c("Phoenix", "NYC", "Flagstaff", "Salt Lake City", "Tucson")
cities <- c("Los Angeles", "Phoenix", "San Diego", "Las Vegas", 
            "Salt Lake City", "Sacramento")
ag <- c("San Diego", "Fresno", "Sacramento", "Tucson", "El Centro", "Yuma")

pet_budyko_123 <- pet_budyko %>%
  filter(site %in% c(places, cities, ag))

pet_budyko_phx <- pet_budyko %>%
  filter(site %in% "Phoenix") %>% 
  arrange(site)

fig_dbg <- ggplot() +
  geom_hline(yintercept = 1,
             lty = 2) +
  geom_vline(xintercept = 1,
             lty = 2) +
  geom_line(data = bud_line,
            aes(x = x,
                y = y),
            color = "gray") +
  geom_point(data = pet_budyko_123,
             aes(x = AI_thorn_annual,
                 y = AET_thorn),
             size = 5,
             color = "gray",
             alpha = 0.25) +
  geom_point(data = pet_budyko_phx,
             aes(x = AI_thorn_annual,
                 y = AET_thorn,
                 col = site),
             size = 5) +
  scale_x_continuous("PET/P", limits = c(0, 21)) +
  scale_y_continuous("AET/P", limits = c(0, 1.2)) +
  scale_color_brewer(type = "qual", palette = 3) +
    theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.75, 0.25))

ggsave(paste0("plots/fig_dbg.png"),
       plot = fig_dbg,
       width = 4,
       height = 2.5,
       units = "in")


pet_budyko_phx_be <- pet_budyko %>%
  filter(site %in% c("Phoenix", "Bears Ears")) %>% 
  mutate(site = factor(site, levels = c("Phoenix", "Bears Ears")))
  arrange(site)

fig_dbg_be <- ggplot() +
  geom_hline(yintercept = 1,
             lty = 2) +
  geom_vline(xintercept = 1,
             lty = 2) +
  geom_line(data = bud_line,
            aes(x = x,
                y = y),
            color = "gray") +
  geom_point(data = pet_budyko_123,
             aes(x = AI_thorn_annual,
                 y = AET_thorn),
             size = 5,
             color = "gray",
             alpha = 0.25) +
  geom_point(data = pet_budyko_phx_be,
             aes(x = AI_thorn_annual,
                 y = AET_thorn,
                 col = site),
             size = 5) +
  scale_x_continuous("PET/P", limits = c(0, 21)) +
  scale_y_continuous("AET/P", limits = c(0, 1.2)) +
  scale_color_brewer(type = "qual", palette = 3) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.75, 0.25))

ggsave(paste0("plots/fig_dbg_be.png"),
       plot = fig_dbg_be,
       width = 4,
       height = 2.5,
       units = "in")

pet_budyko_phx_be_srer <- pet_budyko %>%
  filter(site %in% c("Phoenix", "Bears Ears", "NEON SRER")) %>% 
  mutate(site = factor(site, levels = c("Phoenix", "Bears Ears", "NEON SRER")))
  arrange(site)

fig_dbg_be_srer <- ggplot() +
  geom_hline(yintercept = 1,
             lty = 2) +
  geom_vline(xintercept = 1,
             lty = 2) +
  geom_line(data = bud_line,
            aes(x = x,
                y = y),
            color = "gray") +
  geom_point(data = pet_budyko_123,
             aes(x = AI_thorn_annual,
                 y = AET_thorn),
             size = 5,
             color = "gray",
             alpha = 0.25) +
  geom_point(data = pet_budyko_phx_be_srer,
             aes(x = AI_thorn_annual,
                 y = AET_thorn,
                 col = site),
             size = 5) +
  scale_x_continuous("PET/P", limits = c(0, 21)) +
  scale_y_continuous("AET/P", limits = c(0, 1.2)) +
  scale_color_brewer(type = "qual", palette = 3) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.75, 0.3))

ggsave(paste0("plots/fig_dbg_be_srer.png"),
       plot = fig_dbg_be_srer,
       width = 4,
       height = 2.5,
       units = "in")

#### Research at Mudd
pet_budyko_CA <- pet_budyko %>%
  filter(site %in% c("Claremont", "Death Valley", "Joshua Tree")) %>% 
  mutate(site = factor(site, levels = c("Claremont", "Death Valley", "Joshua Tree")))


fig_CA <- ggplot() +
  geom_hline(yintercept = 1,
             lty = 2) +
  geom_vline(xintercept = 1,
             lty = 2) +
  geom_line(data = bud_line,
            aes(x = x,
                y = y),
            color = "gray") +
  geom_point(data = pet_budyko_123,
             aes(x = AI_thorn_annual,
                 y = AET_thorn),
             size = 5,
             color = "gray",
             alpha = 0.25) +
  geom_point(data = pet_budyko_CA,
             aes(x = AI_thorn_annual,
                 y = AET_thorn,
                 col = site),
             size = 5) +
  scale_x_continuous("PET/P") +
  scale_y_continuous("AET/P", limits = c(0, 1.2)) +
  scale_color_brewer(type = "qual", palette = 3) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.75, 0.3))

ggsave(paste0("plots/fig_CA.png"),
       plot = fig_CA,
       width = 4,
       height = 2.5,
       units = "in")

