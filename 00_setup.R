library(tidyverse)

#### Zone Charts ----
Zone_Charts <- c("Zone - Total Pitches", "Zone - Pitch Percentage",
                 "Zone - Exit Velocity", "Zone - Launch Angle",
                 "Zone - Hit Percentage", "Zone - Swing Percentage",
                 "Zone - Whiff Percentage")

#### Pitch Charts ----
Pitch_Charts <- c("Pitch Types", "Pitch Description",
                  "Pitch Result", "Batted Ball Type", "Contact Type",
                  "Release Point", "Release Extension", "Pitch Movement")

Heat_Maps <- c("Pitch Heatmap", "Swing Heatmap")

chart_types <- cbind(Zone_Charts, Pitch_Charts, Heat_Maps)

chart_types <- as_tibble(chart_types)

colnames(chart_types)[1] <- "Zone Charts"
colnames(chart_types)[2] <- "Pitch Charts"
colnames(chart_types)[3] <- "Heatmaps"

Pitcher_Throws <- c("All", "Right", "Left")

Batter_Stands <- c("All", "Right", "Left")

#### Home Plate ----
x <- c(-8.5, -8, 0, 8, 8.5, -8.5)
z <- c(0, 2, 4, 2, 0, 0)

home_plate <- data.frame(x, z)

# Strike Zone ----
x <- c(-10, 10, 10, -10, -10)
z <- c(18, 18, 42, 42, 18)
sz <- data.frame(x, z)

# New Strike Zone ----
x <- c(-10/3, -10/3, 10/3, 10/3, -10/3)
z <- c(18, 42, 42, 18, 18)
sz_2 <- data.frame(x, z)

# New Strike Zone ----
x <- c(-10, -10, 10, 10, -10)
z <- c(26, 34, 34, 26, 26)
sz_3 <- data.frame(x, z)

### Outer Zones ----
x <- c(-10, -14, -14, 0, 0, -10, -10)
z <- c(30, 30, 46, 46, 42, 42, 30)
kzone_11 <- data.frame(x, z)

x <- c(-10, -14, -14, 0, 0, -10, -10)
z <- c(30, 30, 14, 14, 18, 18, 30)
kzone_13 <- data.frame(x, z)

x <- c(10, 10, 14, 14, 0, 0, 10)
z <- c(42, 30, 30, 46, 46, 42, 42)
kzone_12 <- data.frame(x, z)

x <- c(10, 10, 14, 14, 0, 0, 10)
z <- c(18, 30, 30, 14, 14, 18, 18)
kzone_14 <- data.frame(x, z)

#### ----

### Top Row ----

## K Zone 01

x <- c(-10, -10, -10/3, -10/3, -10)
z <- c(34, 42, 42, 34, 34)

kzone_poly_01 <- sp::Polygon(cbind(x, z))

SZ_Poly_01 <- sp::Polygons(list(kzone_poly_01), ID = "KZONE_01")

SZ_SpatialPoly_01 <- sp::SpatialPolygons(list(SZ_Poly_01))

#### K Zone 02

x <- c(-10/3, -10/3, 10/3, 10/3, -10/3)
z <- c(34, 42, 42, 34, 34)

kzone_poly_02 <- sp::Polygon(cbind(x, z))

SZ_Poly_02 <- sp::Polygons(list(kzone_poly_02), ID = "KZONE_02")

SZ_SpatialPoly_02 <- sp::SpatialPolygons(list(SZ_Poly_02))

#### K Zone 03

x <- c(10/3, 10/3, 10, 10, 10/3)
z <- c(34, 42, 42, 34, 34)

kzone_poly_03 <- sp::Polygon(cbind(x, z))

SZ_Poly_03 <- sp::Polygons(list(kzone_poly_03), ID = "KZONE_03")

SZ_SpatialPoly_03 <- sp::SpatialPolygons(list(SZ_Poly_03))


### Middle Row ----

## K Zone 04

x <- c(-10, -10, -10/3, -10/3, -10)
z <- c(26, 34, 34, 26, 26)

kzone_poly_04 <- sp::Polygon(cbind(x, z))

SZ_Poly_04 <- sp::Polygons(list(kzone_poly_04), ID = "KZONE_04")

SZ_SpatialPoly_04 <- sp::SpatialPolygons(list(SZ_Poly_04))

#### K Zone 05

x <- c(-10/3, -10/3, 10/3, 10/3, -10/3)
z <- c(26, 34, 34, 26, 26)

kzone_poly_05 <- sp::Polygon(cbind(x, z))

SZ_Poly_05 <- sp::Polygons(list(kzone_poly_05), ID = "KZONE_05")

SZ_SpatialPoly_05 <- sp::SpatialPolygons(list(SZ_Poly_05))

#### K Zone 06

x <- c(10/3, 10/3, 10, 10, 10/3)
z <- c(26, 34, 34, 26, 26)

kzone_poly_06 <- sp::Polygon(cbind(x, z))

SZ_Poly_06 <- sp::Polygons(list(kzone_poly_06), ID = "KZONE_06")

SZ_SpatialPoly_06 <- sp::SpatialPolygons(list(SZ_Poly_06))


### Bottom Row ----

## K Zone 07

x <- c(-10, -10, -10/3, -10/3, -10)
z <- c(18, 26, 26, 18, 18)

kzone_poly_07 <- sp::Polygon(cbind(x, z))

SZ_Poly_07 <- sp::Polygons(list(kzone_poly_07), ID = "KZONE_07")

SZ_SpatialPoly_07 <- sp::SpatialPolygons(list(SZ_Poly_07))

#### K Zone 08

x <- c(-10/3, -10/3, 10/3, 10/3, -10/3)
z <- c(18, 26, 26, 18, 18)

kzone_poly_08 <- sp::Polygon(cbind(x, z))

SZ_Poly_08 <- sp::Polygons(list(kzone_poly_08), ID = "KZONE_08")

SZ_SpatialPoly_08 <- sp::SpatialPolygons(list(SZ_Poly_08))

#### K Zone 09

x <- c(10/3, 10/3, 10, 10, 10/3)
z <- c(18, 26, 26, 18, 18)

kzone_poly_09 <- sp::Polygon(cbind(x, z))

SZ_Poly_09 <- sp::Polygons(list(kzone_poly_09), ID = "KZONE_09")

SZ_SpatialPoly_09 <- sp::SpatialPolygons(list(SZ_Poly_09))





### Outer Zones ----

#### Zone 11 (Top Left)
x <- c(-10, -14, -14, 0, 0, -10, -10)
z <- c(30, 30, 46, 46, 42, 42, 30)

kzone_poly_11 <- sp::Polygon(cbind(x, z))

SZ_Poly_11 <- sp::Polygons(list(kzone_poly_11), ID = "KZONE_11")

SZ_SpatialPoly_11 <- sp::SpatialPolygons(list(SZ_Poly_11))

#### Zone 13 (Bottom Left)
x <- c(-10, -14, -14, 0, 0, -10, -10)
z <- c(30, 30, 14, 14, 18, 18, 30)

kzone_poly_13 <- sp::Polygon(cbind(x, z))

SZ_Poly_13 <- sp::Polygons(list(kzone_poly_13), ID = "KZONE_13")

SZ_SpatialPoly_13 <- sp::SpatialPolygons(list(SZ_Poly_13))


#### Zone 12 (Top Right)
x <- c(10, 10, 14, 14, 0, 0, 10)
z <- c(42, 30, 30, 46, 46, 42, 42)

kzone_poly_12 <- sp::Polygon(cbind(x, z))

SZ_Poly_12 <- sp::Polygons(list(kzone_poly_12), ID = "KZONE_12")

SZ_SpatialPoly_12 <- sp::SpatialPolygons(list(SZ_Poly_12))


#### Zone 14 (Bottom Right)
x <- c(10, 10, 14, 14, 0, 0, 10)
z <- c(18, 30, 30, 14, 14, 18, 18)
kzone_14 <- data.frame(x, z)

kzone_poly_14 <- sp::Polygon(cbind(x, z))

SZ_Poly_14 <- sp::Polygons(list(kzone_poly_14), ID = "KZONE_14")

SZ_SpatialPoly_14 <- sp::SpatialPolygons(list(SZ_Poly_14))

#### StrikeZone Polys----
Strikezone_Polys <- sp::SpatialPolygons(
  list(
    sp::Polygons(list(kzone_poly_01), ID = "KZONE_01"),
    sp::Polygons(list(kzone_poly_02), ID = "KZONE_02"),
    sp::Polygons(list(kzone_poly_03), ID = "KZONE_03"),
    sp::Polygons(list(kzone_poly_04), ID = "KZONE_04"),
    sp::Polygons(list(kzone_poly_05), ID = "KZONE_05"),
    sp::Polygons(list(kzone_poly_06), ID = "KZONE_06"),
    sp::Polygons(list(kzone_poly_07), ID = "KZONE_07"),
    sp::Polygons(list(kzone_poly_08), ID = "KZONE_08"),
    sp::Polygons(list(kzone_poly_09), ID = "KZONE_09"),
    sp::Polygons(list(kzone_poly_11), ID = "KZONE_11"),
    sp::Polygons(list(kzone_poly_12), ID = "KZONE_12"),
    sp::Polygons(list(kzone_poly_13), ID = "KZONE_13"),
    sp::Polygons(list(kzone_poly_14), ID = "KZONE_14")
  )
)


save(Strikezone_Polys, file = "data/Strikezone_Polys.rda")


#### ----
save(home_plate, sz, sz_2, sz_3, kzone_11, kzone_12, kzone_13, kzone_14,
     Pitcher_Throws, Batter_Stands, chart_types, 
     SZ_SpatialPoly_01, SZ_SpatialPoly_02, SZ_SpatialPoly_03, SZ_SpatialPoly_04,
     SZ_SpatialPoly_05, SZ_SpatialPoly_06, SZ_SpatialPoly_07, SZ_SpatialPoly_08,
     SZ_SpatialPoly_09, SZ_SpatialPoly_11, SZ_SpatialPoly_12, SZ_SpatialPoly_13,
     SZ_SpatialPoly_14,
     
     file = "data/app_setup.rda")


