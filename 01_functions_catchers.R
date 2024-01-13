# Load Packages ----
library(tidyverse)
library(gt)
library(gtExtras)
library(sportyR)
library(shiny)
library(patchwork)
library(reactable)
library(jpeg)
library(grid)
library(ggridges)
library(sf)
library(sp)


# # Load Data ----

# Home Plate
x <- c(-8.5, -8, 0, 8, 8.5, -8.5)
z <- c(0, 2, 4, 2, 0, 0)
home_plate <- data.frame(x, z)

# Strikezone
x <- c(-10, -10, 10, 10, -10)
z <- c(18, 42, 42, 18, 18)
sz <- data.frame(x, z)

### Heart
x <- c(-20/3, -20/3, 20/3, 20/3, -20/3)
z <- c(22, 38, 38, 22, 22)

catcher_poly_01 <- sp::Polygon(cbind(x, z))

CT_Poly_01 <- sp::Polygons(list(catcher_poly_01), ID = "Catcher_01")

CT_SpatialPoly_01 <- sp::SpatialPolygons(list(CT_Poly_01))

### Top Row ----

#### Catcher Zone 11

x <- c(-40/3, -40/3, -20/3, -20/3, -40/3)
z <- c(38, 46, 46, 38, 38)

catcher_poly_11 <- sp::Polygon(cbind(x, z))

CT_Poly_11 <- sp::Polygons(list(catcher_poly_11), ID = "Catcher_11")

CT_SpatialPoly_11 <- sp::SpatialPolygons(list(CT_Poly_11))

#### Catcher Zone 12

x <- c(-20/3, -20/3, 20/3, 20/3, -20/3)
z <- c(38, 46, 46, 38, 38)

catcher_poly_12 <- sp::Polygon(cbind(x, z))

CT_Poly_12 <- sp::Polygons(list(catcher_poly_12), ID = "Catcher_12")

CT_SpatialPoly_12 <- sp::SpatialPolygons(list(CT_Poly_12))

#### Catcher Zone 13

x <- c(40/3, 40/3, 20/3, 20/3, 40/3)
z <- c(38, 46, 46, 38, 38)

catcher_poly_13 <- sp::Polygon(cbind(x, z))

CT_Poly_13 <- sp::Polygons(list(catcher_poly_13), ID = "Catcher_13")

CT_SpatialPoly_13 <- sp::SpatialPolygons(list(CT_Poly_13))


### Middle Row ----

#### Catcher Zone 14

x <- c(-40/3, -40/3, -20/3, -20/3, -40/3)
z <- c(22, 38, 38, 22, 22)

catcher_poly_14 <- sp::Polygon(cbind(x, z))

CT_Poly_14 <- sp::Polygons(list(catcher_poly_14), ID = "Catcher_14")

CT_SpatialPoly_14 <- sp::SpatialPolygons(list(CT_Poly_14))

#### Catcher Zone 16

x <- c(40/3, 40/3, 20/3, 20/3, 40/3)
z <- c(22, 38, 38, 22, 22)

catcher_poly_16 <- sp::Polygon(cbind(x, z))

CT_Poly_16 <- sp::Polygons(list(catcher_poly_16), ID = "Catcher_16")

CT_SpatialPoly_16 <- sp::SpatialPolygons(list(CT_Poly_16))


### Bottom Row ----

#### Catcher Zone 17

x <- c(-40/3, -40/3, -20/3, -20/3, -40/3)
z <- c(14, 22, 22, 14, 14)

catcher_poly_17 <- sp::Polygon(cbind(x, z))

CT_Poly_17 <- sp::Polygons(list(catcher_poly_17), ID = "Catcher_17")

CT_SpatialPoly_17 <- sp::SpatialPolygons(list(CT_Poly_17))

#### Catcher Zone 18

x <- c(-20/3, -20/3, 20/3, 20/3, -20/3)
z <- c(14, 22, 22, 14, 14)

catcher_poly_18 <- sp::Polygon(cbind(x, z))

CT_Poly_18 <- sp::Polygons(list(catcher_poly_18), ID = "Catcher_18")

CT_SpatialPoly_18 <- sp::SpatialPolygons(list(CT_Poly_18))

#### Catcher Zone 19

x <- c(40/3, 40/3, 20/3, 20/3, 40/3)
z <- c(14, 22, 22, 14, 14)

catcher_poly_19 <- sp::Polygon(cbind(x, z))

CT_Poly_19 <- sp::Polygons(list(catcher_poly_19), ID = "Catcher_19")

CT_SpatialPoly_19 <- sp::SpatialPolygons(list(CT_Poly_19))


#### StrikeZone Polys----
Catcher_Polys <- sp::SpatialPolygons(
  list(
    CT_Poly_01,
    CT_Poly_11,
    CT_Poly_12,
    CT_Poly_13,
    CT_Poly_14,
    CT_Poly_16,
    CT_Poly_17,
    CT_Poly_18,
    CT_Poly_19
  )
)

save(Catcher_Polys, file = "data/Catcher_Polys.rda")


#### ----
save(home_plate, sz,
     CT_SpatialPoly_01, CT_SpatialPoly_11, CT_SpatialPoly_12, CT_SpatialPoly_13,
     CT_SpatialPoly_14, CT_SpatialPoly_16, CT_SpatialPoly_17, CT_SpatialPoly_18,
     CT_SpatialPoly_19,
     
     file = "data/app_setup_catcher.rda")



load(file = here::here("data/info.rda"))
load(file = here::here("data/app_setup_catcher.rda"))

df <- input %>% 
  filter(!is.na(c(PlateLocHeight)))

Catcher_Points <- SpatialPoints(coords = cbind(df$PlateLocSide, df$PlateLocHeight))

df <- df %>% 
  mutate(
    catch_zone = case_when(
      over(Catcher_Points, CT_SpatialPoly_01, returnList = TRUE) == 1 ~ "Catcher_01",
      over(Catcher_Points, CT_SpatialPoly_11, returnList = TRUE) == 1 ~ "Catcher_11",
      over(Catcher_Points, CT_SpatialPoly_12, returnList = TRUE) == 1 ~ "Catcher_12",
      over(Catcher_Points, CT_SpatialPoly_13, returnList = TRUE) == 1 ~ "Catcher_13",
      over(Catcher_Points, CT_SpatialPoly_14, returnList = TRUE) == 1 ~ "Catcher_14",
      over(Catcher_Points, CT_SpatialPoly_16, returnList = TRUE) == 1 ~ "Catcher_16",
      over(Catcher_Points, CT_SpatialPoly_17, returnList = TRUE) == 1 ~ "Catcher_17",
      over(Catcher_Points, CT_SpatialPoly_18, returnList = TRUE) == 1 ~ "Catcher_18",
      over(Catcher_Points, CT_SpatialPoly_19, returnList = TRUE) == 1 ~ "Catcher_19"
    )
  )


##### ----

x <- c(-8.5, -8, 0, 8, 8.5, -8.5)
z <- c(0, 2, 4, 2, 0, 0)

home_plate <- data.frame(x, z)

x <- c(-10, -10, 10, 10, -10)
z <- c(18, 42, 42, 18, 18)

sz <- data.frame(x, z)


catchers <- df %>% 
  filter(
    Catcher == catcher,
    PitchCall %in% c('BallCalled', 'StrikeCalled'),
    !is.na(catch_zone)
  ) %>% 
  mutate(
    isBall = if_else(PitchCall == 'BallCalled', 1, 0),
    isStrike = if_else(PitchCall == 'StrikeCalled', 1, 0)
  ) %>% 
  select(Catcher, catch_zone, PlateLocSide, PlateLocHeight, PitchCall, isBall, isStrike)

catch_pitches <- catchers %>% 
  group_by(Catcher, catch_zone) %>% 
  summarise(
    N = n()
  )

catch_nKs <- catchers %>% 
  group_by(Catcher) %>% 
  summarise(
    tot_nKs = sum(isStrike)
  )

catch_strikes <- catchers %>% 
  group_by(Catcher, catch_zone) %>% 
  summarise(
    nStrikes = sum(isStrike)
  )

catcher_pitchKs <- left_join(catch_pitches, catch_strikes, by = c("Catcher", 'catch_zone'))

catcher_eval <- left_join(catcher_pitchKs, catch_nKs, by = c("Catcher"))

catcher_framing <- catcher_eval %>% 
  mutate(
    freq = nStrikes/N * 100
  )

df_catcher_polys = fortify(Catcher_Polys, region = "id")

colnames(df_catcher_polys)[6] <- "catch_zone"
colnames(df_catcher_polys)[1] <- "x"
colnames(df_catcher_polys)[2] <- "z"
df_catcher_polys

df_pitch_polys <- left_join(x = catcher_framing, y = df_catcher_polys, by = "catch_zone")

df_pitch_polys

### Labels
labels <- df_pitch_polys %>% 
  select(catch_zone, freq) %>% 
  distinct() %>% 
  arrange(catch_zone)

### Label Coordinates
label_info <- data.frame(
  catch_zone = c("Catcher_01", "Catcher_11", "Catcher_12",
                 "Catcher_13", "Catcher_14", "Catcher_16",
                 "Catcher_17", "Catcher_18", "Catcher_19"),
  x = c(0, -10, 0, 10, -10, 10, -10, 0, 10),
  z = c(30, 42, 42, 42, 30, 30, 18, 18, 18)
)

label_coords <- full_join(x = labels, y = label_info, by = "catch_zone")

ggplot() +
  geom_polygon(df_pitch_polys, mapping = aes(x, z, group = catch_zone, fill = freq),
               color = "black", show.legend = FALSE, alpha = 0.8) +
  geom_polygon(home_plate, mapping = aes(x, z), fill = "grey", color = "darkgrey") +
  geom_path(sz, mapping = aes(x, z), color = "green2", linetype = 9) +
  coord_equal() +
  scale_fill_distiller(palette = "RdBu", limits = c(0, 100)) +
  geom_text(label_coords, 
            mapping = aes(x, z, label = sprintf("%0.1f", round(freq, digits = 1))),
            size = 3.5, fontface = "bold", family="mono") +
  theme_minimal() +
  labs(
    title = "Called Strike Percentage"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = nu_purp),
    plot.subtitle = element_text(hjust = 0.5, color = nu_purp),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.position = "bottom"
  )



framing <- function(catcher, input) {
  
  load(file = "data/Catcher_Polys.rda")
  load(file = here::here("data/app_setup_catcher.rda"))
  
  df <- input %>% 
    filter(!is.na(c(PlateLocHeight)))
  
  Catcher_Points <- SpatialPoints(coords = cbind(df$PlateLocSide, df$PlateLocHeight))
  
  df <- df %>% 
    mutate(
      catch_zone = case_when(
        over(Catcher_Points, CT_SpatialPoly_01, returnList = TRUE) == 1 ~ "Catcher_01",
        over(Catcher_Points, CT_SpatialPoly_11, returnList = TRUE) == 1 ~ "Catcher_11",
        over(Catcher_Points, CT_SpatialPoly_12, returnList = TRUE) == 1 ~ "Catcher_12",
        over(Catcher_Points, CT_SpatialPoly_13, returnList = TRUE) == 1 ~ "Catcher_13",
        over(Catcher_Points, CT_SpatialPoly_14, returnList = TRUE) == 1 ~ "Catcher_14",
        over(Catcher_Points, CT_SpatialPoly_16, returnList = TRUE) == 1 ~ "Catcher_16",
        over(Catcher_Points, CT_SpatialPoly_17, returnList = TRUE) == 1 ~ "Catcher_17",
        over(Catcher_Points, CT_SpatialPoly_18, returnList = TRUE) == 1 ~ "Catcher_18",
        over(Catcher_Points, CT_SpatialPoly_19, returnList = TRUE) == 1 ~ "Catcher_19"
      )
    )
  
  
  ##### ----
  
  x <- c(-8.5, -8, 0, 8, 8.5, -8.5)
  z <- c(0, 2, 4, 2, 0, 0)
  
  home_plate <- data.frame(x, z)
  
  x <- c(-10, -10, 10, 10, -10)
  z <- c(18, 42, 42, 18, 18)
  
  sz <- data.frame(x, z)
  
  
  catchers <- df %>% 
    filter(
      Catcher == catcher,
      PitchCall %in% c('BallCalled', 'StrikeCalled'),
      !is.na(catch_zone)
    ) %>% 
    mutate(
      isBall = if_else(PitchCall == 'BallCalled', 1, 0),
      isStrike = if_else(PitchCall == 'StrikeCalled', 1, 0)
    ) %>% 
    select(Catcher, catch_zone, PlateLocSide, PlateLocHeight, PitchCall, isBall, isStrike)
  
  catch_pitches <- catchers %>% 
    group_by(Catcher, catch_zone) %>% 
    summarise(
      N = n()
    )
  
  catch_nKs <- catchers %>% 
    group_by(Catcher) %>% 
    summarise(
      tot_nKs = sum(isStrike)
    )
  
  catch_strikes <- catchers %>% 
    group_by(Catcher, catch_zone) %>% 
    summarise(
      nStrikes = sum(isStrike)
    )
  
  catcher_pitchKs <- left_join(catch_pitches, catch_strikes, by = c("Catcher", 'catch_zone'))
  
  catcher_eval <- left_join(catcher_pitchKs, catch_nKs, by = c("Catcher"))
  
  catcher_framing <- catcher_eval %>% 
    mutate(
      freq = nStrikes/N * 100
    )
  
  df_catcher_polys = fortify(Catcher_Polys, region = "id")
  
  colnames(df_catcher_polys)[6] <- "catch_zone"
  colnames(df_catcher_polys)[1] <- "x"
  colnames(df_catcher_polys)[2] <- "z"
  df_catcher_polys
  
  df_pitch_polys <- left_join(x = catcher_framing, y = df_catcher_polys, by = "catch_zone")
  
  df_pitch_polys
  
  ### Labels
  labels <- df_pitch_polys %>% 
    select(catch_zone, freq) %>% 
    distinct() %>% 
    arrange(catch_zone)
  
  ### Label Coordinates
  label_info <- data.frame(
    catch_zone = c("Catcher_01", "Catcher_11", "Catcher_12",
                   "Catcher_13", "Catcher_14", "Catcher_16",
                   "Catcher_17", "Catcher_18", "Catcher_19"),
    x = c(0, -10, 0, 10, -10, 10, -10, 0, 10),
    z = c(30, 42, 42, 42, 30, 30, 18, 18, 18)
  )
  
  label_coords <- full_join(x = labels, y = label_info, by = "catch_zone")
  
  ggplot() +
    geom_polygon(df_pitch_polys, mapping = aes(x, z, group = catch_zone, fill = freq),
                 color = "black", show.legend = FALSE, alpha = 0.8) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "grey", color = "darkgrey") +
    geom_path(sz, mapping = aes(x, z), color = "green2", linetype = 9) +
    coord_equal() +
    scale_fill_distiller(palette = "RdBu", limits = c(0, 100)) +
    geom_text(label_coords, 
              mapping = aes(x, z, label = sprintf("%0.1f", round(freq, digits = 1))),
              size = 3.5, fontface = "bold", family="mono") +
    theme_minimal() +
    labs(
      title = "Called Strike Percentage"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", color = nu_purp),
      plot.subtitle = element_text(hjust = 0.5, color = nu_purp),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      legend.position = "bottom"
    )
  
}
