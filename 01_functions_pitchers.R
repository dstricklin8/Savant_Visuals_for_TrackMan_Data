library(tidyverse)
library(sp)
library(sf)

mutate_data_pitcher <- function(input) {
  
  nu_df <- input
  
  nu_df <- nu_df %>% 
    filter(!is.na(RelSpeed)) %>% 
    mutate(
      PlateLocSide = 12*PlateLocSide,
      PlateLocHeight = 12*PlateLocHeight
    )
  
  nu_df <- nu_df %>% 
    separate(Pitcher, c("Pitcher_Last", "Pitcher_First"))
  
  nu_df <- nu_df %>% 
    separate(Batter, c("Batter_Last", "Batter_First"))
  
  nu_df <- nu_df %>% mutate(
    PlayResult = case_when(
      KorBB == "Strikeout" ~ "Strikeout",
      KorBB == "Walk" ~ "Walk", 
      PlayResult == "Undefined" ~ NA,
      TRUE ~ PlayResult),
  
    PlayResult = factor(PlayResult, levels = c("Single", "Double", "Triple", "HomeRun",
                                             "Walk", "Strikeout", "Out", "Error", "Sacrifice",
                                             "FieldersChoice")),
  
    AutoHitType = factor(AutoHitType, levels = c("GroundBall", "LineDrive", "FlyBall", "Popup")),
    
    AutoPitchType = case_when(
      AutoPitchType %in% c("Four-Seam", "Sinker", "Cutter") ~ "Fastball",
      AutoPitchType == "Splitter" ~ "Changeup",
      TRUE ~ AutoPitchType),
  
    AutoPitchType = factor(AutoPitchType, levels = c("Fastball", "Changeup", "Curveball", "Slider")),
  
    PitchCall = factor(PitchCall, levels = c("BallCalled", "BallinDirt", "BallIntentional", "InPlay",
                                           "StrikeCalled", "FoulBall", "StrikeSwinging", "HitByPitch")),
    
    ynhit = case_when(
      PlayResult %in% c("Single", "Double", "Triple", "HomeRun") ~ 1,
      PlayResult %in% c("Strikeout", "Out", "Error", "FieldersChoice") ~ 0,
      PlayResult == "Undefined" ~ NA
      ),
    launch_speed_angle = case_when(
      ExitSpeed * 1.5 - Angle >= 117 &
        ExitSpeed + Angle >= 124 &
        ExitSpeed >= 98 &
        Angle >= 4 & Angle <= 50 ~ "Barrel",
      
      ExitSpeed * 1.5 - Angle >= 111 &
        ExitSpeed + Angle >= 119 &
        ExitSpeed >= 95 &
        Angle >= 0 & Angle <= 52 ~ "Solid_Contact",
      
      ExitSpeed * 2 - Angle >= 87 &
        Angle <= 41 & 
        ExitSpeed * 2 + Angle <= 175 &
        ExitSpeed + Angle * 1.3 >= 89 &
        ExitSpeed >= 59 & Angle <= 72 ~ "Flare_or_Burner",
      
      ExitSpeed + Angle * 1.3 <= 112 &
        ExitSpeed + Angle * 1.55 >= 92 &
        ExitSpeed >= 72 & Angle <= 86 ~ "Flare_or_Burner",
      
      Angle <= 20 &
        ExitSpeed + Angle * 2.4 >= 98 &
        ExitSpeed >= 86 & ExitSpeed <= 95 ~ "Flare_or_Burner",
      
      ExitSpeed - Angle >= 76 &
        ExitSpeed + Angle * 2.4 >= 98 &
        ExitSpeed >= 95 &
        Angle <= 30 ~ "Flare_or_Burner",
      
      ExitSpeed + Angle * 2 >= 116 ~  "Poorly_Under",
      
      ExitSpeed + Angle * 2 <= 116 ~  "Poorly_Topped",
      
      ExitSpeed <= 59 ~ "Poorly_Weak"
    ),
    
    launch_speed_angle = factor(launch_speed_angle, levels = c(
      "Barrel", "Solid_Contact", "Flare_or_Burner", "Poorly_Under", "Poorly_Topped", "Poorly_Weak"
    ))
  )
  
  save(nu_df, file = "data/nu_df.rda")
}

#### Batted Ball Type
batted_ball_type_pitcher <- function(Pitcher, df) {
  load(file = "data/app_setup.rda")
  
  load(file = "data/Strikezone_Polys.rda")

  load(file = "data/custom_aes.rda")
  
    ggplot() +
      coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
      geom_point(df %>% filter(Pitcher_Last == Pitcher & !is.na(AutoHitType)),
                 mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = AutoHitType),
                 size = 5, shape = 21 ) +
      geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
      geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
      geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
      geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
      scale_fill_manual(values = HitTypeColors) +
      theme_minimal() +
      TH +
      theme(
        plot.margin = unit(c(1,1,1,1), "cm"),
        legend.position = "bottom",
        legend.background = element_rect(color = "#582c83")
      )
}

#### Pitch Result
pitch_result_pitcher <- function(Pitcher, df) {
  
  load(file = "data/app_setup.rda")
  
  load(file = "data/Strikezone_Polys.rda")
  
  load(file = "data/custom_aes.rda")
  
  ggplot() +
    coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
    geom_point(df %>% filter(Pitcher_Last == Pitcher & !is.na(PlayResult)),
               mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = PlayResult),
               size = 5, shape = 21 ) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
    scale_fill_manual(values = PlayResultColors) +
    theme_minimal() +
    TH +
    theme(
      plot.margin = unit(c(1,1,1,1), "cm"),
      legend.position = "bottom",
      legend.background = element_rect(color = "#582c83")
    )
}

#### Pitch Types
pitch_types_pitcher <- function(Pitcher, df) {
  
  load(file = "data/app_setup.rda")
  
  load(file = "data/Strikezone_Polys.rda")
  
  load(file = "data/custom_aes.rda")
  
  ggplot() +
    geom_point(df %>% filter(Pitcher_Last == Pitcher),
               mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = AutoPitchType),
               size = 5, shape = 21) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
    coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
    theme_minimal() +
    TH +
    theme(
      plot.margin = unit(c(1,1,1,1), "cm"),
      legend.position = "bottom",
      legend.background = element_rect(color = "#582c83")
    )
  
}

#### Pitch Desc.
pitch_description_pitcher <- function(Pitcher, df) {
  
  load(file = "data/app_setup.rda")
  
  load(file = "data/Strikezone_Polys.rda")
  
  load(file = "data/custom_aes.rda")
  
  ggplot() +
    geom_point(df %>% filter(Pitcher_Last == Pitcher),
               mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = PitchCall),
               size = 5, shape = 21) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") + 
    scale_fill_manual(values = PitchCallColors) +
    coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
    theme_minimal() +
    TH +
    theme(
      plot.margin = unit(c(1,1,1,1), "cm"),
      legend.position = "bottom",
      legend.background = element_rect(color = "#582c83")
    )
}

#### Pitch Heatmap
pitch_heatmap_pitcher <- function(Pitcher, df) {
  
  load(file = "data/app_setup.rda")
  
  load(file = "data/Strikezone_Polys.rda")
  
  load(file = "data/custom_aes.rda")
  
  ggplot(df %>% filter(Pitcher_Last == Pitcher),
         mapping = aes(PlateLocSide, PlateLocHeight)) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    stat_density_2d(geom = "polygon", aes(fill = after_stat(nlevel)), show.legend = FALSE) +
    geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
    scale_fill_distiller(palette = "RdBu") +
    coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
    theme_minimal() +
    TH +
    theme(
      plot.margin = unit(c(1,1,1,1), "cm"),
      legend.position = "bottom",
      legend.background = element_rect(color = "#582c83")
    )
}


## Polys ----

#### Zone - Pitch Percent
zone_pitch_perc_pitcher <- function(Pitcher, input) {
  
  load(file = "data/app_setup.rda")
  
  load(file = "data/Strikezone_Polys.rda")
  
  load(file = "data/custom_aes.rda")
  
  require(sp)
  require(sf)
  
  df <- input %>% 
    filter(!is.na(c(PlateLocHeight)))
  
  K_Zone_Points <- SpatialPoints(coords = cbind(df$PlateLocSide, df$PlateLocHeight))
  
  df <- df %>% 
    mutate(
      pitch_zone = case_when(
        over(K_Zone_Points, SZ_SpatialPoly_01, returnList = TRUE) == 1 ~ "KZONE_01",
        over(K_Zone_Points, SZ_SpatialPoly_02, returnList = TRUE) == 1 ~ "KZONE_02",
        over(K_Zone_Points, SZ_SpatialPoly_03, returnList = TRUE) == 1 ~ "KZONE_03",
        over(K_Zone_Points, SZ_SpatialPoly_04, returnList = TRUE) == 1 ~ "KZONE_04",
        over(K_Zone_Points, SZ_SpatialPoly_05, returnList = TRUE) == 1 ~ "KZONE_05",
        over(K_Zone_Points, SZ_SpatialPoly_06, returnList = TRUE) == 1 ~ "KZONE_06",
        over(K_Zone_Points, SZ_SpatialPoly_07, returnList = TRUE) == 1 ~ "KZONE_07",
        over(K_Zone_Points, SZ_SpatialPoly_08, returnList = TRUE) == 1 ~ "KZONE_08",
        over(K_Zone_Points, SZ_SpatialPoly_09, returnList = TRUE) == 1 ~ "KZONE_09",
        over(K_Zone_Points, SZ_SpatialPoly_11, returnList = TRUE) == 1 ~ "KZONE_11",
        over(K_Zone_Points, SZ_SpatialPoly_12, returnList = TRUE) == 1 ~ "KZONE_12",
        over(K_Zone_Points, SZ_SpatialPoly_13, returnList = TRUE) == 1 ~ "KZONE_13",
        over(K_Zone_Points, SZ_SpatialPoly_14, returnList = TRUE) == 1 ~ "KZONE_14",
        
        PlateLocSide < -10 & PlateLocHeight > 30 |
          PlateLocSide < 0 & PlateLocHeight > 42 ~ 'KZONE_11',
        
        PlateLocSide > 10 & PlateLocHeight > 30 |
          PlateLocSide > 0 & PlateLocHeight > 42 ~ 'KZONE_12',
        
        PlateLocSide < -10 & PlateLocHeight < 30 |
          PlateLocSide < 0 & PlateLocHeight < 18 ~ 'KZONE_13',
        
        PlateLocSide > 10 & PlateLocHeight < 30 |
          PlateLocSide > 0 & PlateLocHeight < 18 ~ 'KZONE_14'
      )
    )
  
  # ggplot(df, aes(PlateLocSide, PlateLocHeight, color = pitch_zone)) +
  #   geom_point()
  
  #### Data Frame ----
  df_pitches <- df %>%
    filter(Pitcher_Last == Pitcher) %>%
    select(Pitcher_Last, PlayResult, AutoPitchType, Angle, ExitSpeed, RelSpeed, ynhit, pitch_zone) %>%
    group_by(pitch_zone) %>%
    summarise(
      Pitches = n(),
      EV = mean(ExitSpeed, na.rm = TRUE),
      LA = mean(Angle, na.rm = TRUE),
      Hits = mean(ynhit, na.rm = TRUE)
    ) %>%
    mutate(
      Freq_PitchPercent = (Pitches/sum(Pitches)) * 100,
      Freq_NumPitches = Pitches,
      Freq_EV = EV,
      Freq_LA = LA,
      Freq_Hits = Hits * 100
    )
  
  # ggplot(df_pitches, aes(PlateLocSide, PlateLocHeight, color = pitch_zone)) +
  #   geom_point()
  
  load("data/Strikezone_Polys.rda")

  #### Fortify Polys ----
  df_sz_polys = fortify(Strikezone_Polys, region = "id")

  #### Fix Column Names ----
  colnames(df_sz_polys)[1] <- "x"
  colnames(df_sz_polys)[2] <- "z"
  colnames(df_sz_polys)[6] <- "pitch_zone"

  #### Join Data and Polys ----
  df_pitch_polys <- full_join(x = df_pitches, y = df_sz_polys, by = "pitch_zone")
  
  #### Labels ----
  labels <- df_pitch_polys %>%
    select(pitch_zone, Freq_PitchPercent, Freq_NumPitches, Freq_EV, Freq_LA, Freq_Hits) %>%
    distinct() %>%
    arrange(pitch_zone)
  
  #### Label Coordinates ----
  label_info <- data.frame(
    pitch_zone = c("KZONE_01", "KZONE_02", "KZONE_03",
                   "KZONE_04", "KZONE_05", "KZONE_06",
                   "KZONE_07", "KZONE_08", "KZONE_09",
                   "KZONE_11", "KZONE_12", "KZONE_13", "KZONE_14"),
    x = c(-20/3, 0, 20/3, -20/3, 0, 20/3, -20/3, 0, 20/3, -10, 10, -10, 10),
    z = c(38, 38, 38, 30, 30, 30, 22, 22, 22, 44, 44, 16, 16)
  )
  
  #### Join Label Coords ----
  label_coords <- full_join(x = labels, y = label_info, by = "pitch_zone")
  
  #### Fix NA ----
  df_pitch_polys <- df_pitch_polys %>%
    filter(!is.na(Freq_PitchPercent))
  
  label_coords <- label_coords %>% 
    filter(!is.na(Freq_PitchPercent))
  
  ggplot() +
    geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_path(sz_2, mapping = aes(x, z), color = "black") +
    geom_path(sz_3, mapping = aes(x, z), color = "black") +
    geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(df_pitch_polys, mapping = aes(x, z, group = pitch_zone, fill = Freq_PitchPercent),
                 color = "black", show.legend = FALSE) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
    scale_fill_distiller(palette = "RdYlBu", limits = c(0, max(df_pitch_polys$Freq_PitchPercent) + 5),
                         breaks = c(0, max(df_pitch_polys$Freq_PitchPercent) + 5, 4),
                         na.value = "lightgrey", guide = "legend") +
    geom_text(label_coords, 
              mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_PitchPercent, digits = 1))),
              size = 4, fontface = "bold", family = "mono") +
    theme_minimal() +
    TH

  # save(df_pitch_polys, label_coords, file = "data/sz_poly_results.rda")
}

#### Zone - Pitch Percent
zone_total_pitches_pitcher <- function(Pitcher, input) {
  
  load(file = "data/app_setup.rda")
  
  load(file = "data/Strikezone_Polys.rda")
  
  load(file = "data/custom_aes.rda")
  
  #### Setup ----
  require(sp)
  require(sf)
  
  df <- input %>% 
    filter(!is.na(c(PlateLocHeight)))
  
  K_Zone_Points <- SpatialPoints(coords = cbind(df$PlateLocSide, df$PlateLocHeight))
  
  df <- df %>% 
    mutate(
      pitch_zone = case_when(
        over(K_Zone_Points, SZ_SpatialPoly_01, returnList = TRUE) == 1 ~ "KZONE_01",
        over(K_Zone_Points, SZ_SpatialPoly_02, returnList = TRUE) == 1 ~ "KZONE_02",
        over(K_Zone_Points, SZ_SpatialPoly_03, returnList = TRUE) == 1 ~ "KZONE_03",
        over(K_Zone_Points, SZ_SpatialPoly_04, returnList = TRUE) == 1 ~ "KZONE_04",
        over(K_Zone_Points, SZ_SpatialPoly_05, returnList = TRUE) == 1 ~ "KZONE_05",
        over(K_Zone_Points, SZ_SpatialPoly_06, returnList = TRUE) == 1 ~ "KZONE_06",
        over(K_Zone_Points, SZ_SpatialPoly_07, returnList = TRUE) == 1 ~ "KZONE_07",
        over(K_Zone_Points, SZ_SpatialPoly_08, returnList = TRUE) == 1 ~ "KZONE_08",
        over(K_Zone_Points, SZ_SpatialPoly_09, returnList = TRUE) == 1 ~ "KZONE_09",
        over(K_Zone_Points, SZ_SpatialPoly_11, returnList = TRUE) == 1 ~ "KZONE_11",
        over(K_Zone_Points, SZ_SpatialPoly_12, returnList = TRUE) == 1 ~ "KZONE_12",
        over(K_Zone_Points, SZ_SpatialPoly_13, returnList = TRUE) == 1 ~ "KZONE_13",
        over(K_Zone_Points, SZ_SpatialPoly_14, returnList = TRUE) == 1 ~ "KZONE_14",
        
        PlateLocSide < -10 & PlateLocHeight > 30 |
          PlateLocSide < 0 & PlateLocHeight > 42 ~ 'KZONE_11',
        
        PlateLocSide > 10 & PlateLocHeight > 30 |
          PlateLocSide > 0 & PlateLocHeight > 42 ~ 'KZONE_12',
        
        PlateLocSide < -10 & PlateLocHeight < 30 |
          PlateLocSide < 0 & PlateLocHeight < 18 ~ 'KZONE_13',
        
        PlateLocSide > 10 & PlateLocHeight < 30 |
          PlateLocSide > 0 & PlateLocHeight < 18 ~ 'KZONE_14'
      )
    )
  
  # ggplot(df, aes(PlateLocSide, PlateLocHeight, color = pitch_zone)) +
  #   geom_point()
  
  #### Data Frame ----
  df_pitches <- df %>%
    filter(Pitcher_Last == Pitcher) %>%
    select(Pitcher_Last, PlayResult, AutoPitchType, Angle, ExitSpeed, RelSpeed, ynhit, pitch_zone) %>%
    group_by(pitch_zone) %>%
    summarise(
      Pitches = n(),
      EV = mean(ExitSpeed, na.rm = TRUE),
      LA = mean(Angle, na.rm = TRUE),
      Hits = mean(ynhit, na.rm = TRUE)
    ) %>%
    mutate(
      Freq_PitchPercent = (Pitches/sum(Pitches)) * 100,
      Freq_NumPitches = Pitches,
      Freq_EV = EV,
      Freq_LA = LA,
      Freq_Hits = Hits * 100
    )
  
  # ggplot(df_pitches, aes(PlateLocSide, PlateLocHeight, color = pitch_zone)) +
  #   geom_point()
  
  load("data/Strikezone_Polys.rda")
  
  #### Fortify Polys ----
  df_sz_polys = fortify(Strikezone_Polys, region = "id")
  
  #### Fix Column Names ----
  colnames(df_sz_polys)[1] <- "x"
  colnames(df_sz_polys)[2] <- "z"
  colnames(df_sz_polys)[6] <- "pitch_zone"
  
  #### Join Data and Polys ----
  df_pitch_polys <- full_join(x = df_pitches, y = df_sz_polys, by = "pitch_zone")
  
  #### Labels ----
  labels <- df_pitch_polys %>%
    select(pitch_zone, Freq_PitchPercent, Freq_NumPitches, Freq_EV, Freq_LA, Freq_Hits) %>%
    distinct() %>%
    arrange(pitch_zone)
  
  #### Label Coordinates ----
  label_info <- data.frame(
    pitch_zone = c("KZONE_01", "KZONE_02", "KZONE_03",
                   "KZONE_04", "KZONE_05", "KZONE_06",
                   "KZONE_07", "KZONE_08", "KZONE_09",
                   "KZONE_11", "KZONE_12", "KZONE_13", "KZONE_14"),
    x = c(-20/3, 0, 20/3, -20/3, 0, 20/3, -20/3, 0, 20/3, -10, 10, -10, 10),
    z = c(38, 38, 38, 30, 30, 30, 22, 22, 22, 44, 44, 16, 16)
  )
  
  #### Join Label Coords ----
  label_coords <- full_join(x = labels, y = label_info, by = "pitch_zone")
  
  #### Fix NA ----
  df_pitch_polys <- df_pitch_polys %>%
    filter(!is.na(Freq_NumPitches))
  
  label_coords <- label_coords %>% 
    filter(!is.na(Freq_NumPitches))
  
  #### GGPLOT ----
  ggplot() +
    geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_path(sz_2, mapping = aes(x, z), color = "black") +
    geom_path(sz_3, mapping = aes(x, z), color = "black") +
    geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(df_pitch_polys, mapping = aes(x, z, group = pitch_zone, fill = Freq_NumPitches),
                 color = "black", show.legend = FALSE) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
    scale_fill_distiller(palette = "RdBu", limits = c(0, max(df_pitch_polys$Freq_NumPitches)),
                         na.value = "lightgrey", guide = "legend") +
    geom_text(label_coords, 
              mapping = aes(x, z, label = Freq_NumPitches),
              size = 4, fontface = "bold", family="mono") +
    theme_minimal() +
    labs(
      caption = paste("Date Range: ", min(input$Date), "to", max(input$Date))
    ) +
    TH
}

#### Zone - Exit Velocity
zone_exit_velo_pitcher <- function(Pitcher, input) {
  
  load(file = "data/app_setup.rda")
  
  load(file = "data/Strikezone_Polys.rda")
  
  load(file = "data/custom_aes.rda")
  
  #### Setup ----
  require(sp)
  require(sf)
  
  df <- input %>% 
    filter(!is.na(c(PlateLocHeight)))
  
  K_Zone_Points <- SpatialPoints(coords = cbind(df$PlateLocSide, df$PlateLocHeight))
  
  df <- df %>% 
    mutate(
      pitch_zone = case_when(
        over(K_Zone_Points, SZ_SpatialPoly_01, returnList = TRUE) == 1 ~ "KZONE_01",
        over(K_Zone_Points, SZ_SpatialPoly_02, returnList = TRUE) == 1 ~ "KZONE_02",
        over(K_Zone_Points, SZ_SpatialPoly_03, returnList = TRUE) == 1 ~ "KZONE_03",
        over(K_Zone_Points, SZ_SpatialPoly_04, returnList = TRUE) == 1 ~ "KZONE_04",
        over(K_Zone_Points, SZ_SpatialPoly_05, returnList = TRUE) == 1 ~ "KZONE_05",
        over(K_Zone_Points, SZ_SpatialPoly_06, returnList = TRUE) == 1 ~ "KZONE_06",
        over(K_Zone_Points, SZ_SpatialPoly_07, returnList = TRUE) == 1 ~ "KZONE_07",
        over(K_Zone_Points, SZ_SpatialPoly_08, returnList = TRUE) == 1 ~ "KZONE_08",
        over(K_Zone_Points, SZ_SpatialPoly_09, returnList = TRUE) == 1 ~ "KZONE_09",
        over(K_Zone_Points, SZ_SpatialPoly_11, returnList = TRUE) == 1 ~ "KZONE_11",
        over(K_Zone_Points, SZ_SpatialPoly_12, returnList = TRUE) == 1 ~ "KZONE_12",
        over(K_Zone_Points, SZ_SpatialPoly_13, returnList = TRUE) == 1 ~ "KZONE_13",
        over(K_Zone_Points, SZ_SpatialPoly_14, returnList = TRUE) == 1 ~ "KZONE_14",
        
        PlateLocSide < -10 & PlateLocHeight > 30 |
          PlateLocSide < 0 & PlateLocHeight > 42 ~ 'KZONE_11',
        
        PlateLocSide > 10 & PlateLocHeight > 30 |
          PlateLocSide > 0 & PlateLocHeight > 42 ~ 'KZONE_12',
        
        PlateLocSide < -10 & PlateLocHeight < 30 |
          PlateLocSide < 0 & PlateLocHeight < 18 ~ 'KZONE_13',
        
        PlateLocSide > 10 & PlateLocHeight < 30 |
          PlateLocSide > 0 & PlateLocHeight < 18 ~ 'KZONE_14'
      )
    )
  
  # ggplot(df, aes(PlateLocSide, PlateLocHeight, color = pitch_zone)) +
  #   geom_point()
  
  #### Data Frame ----
  df_pitches <- df %>%
    filter(Pitcher_Last == Pitcher) %>%
    select(Pitcher_Last, PlayResult, AutoPitchType, Angle, ExitSpeed, RelSpeed, ynhit, pitch_zone) %>%
    group_by(pitch_zone) %>%
    summarise(
      Pitches = n(),
      EV = mean(ExitSpeed, na.rm = TRUE),
      LA = mean(Angle, na.rm = TRUE),
      Hits = mean(ynhit, na.rm = TRUE)
    ) %>%
    mutate(
      Freq_PitchPercent = (Pitches/sum(Pitches)) * 100,
      Freq_NumPitches = Pitches,
      Freq_EV = EV,
      Freq_LA = LA,
      Freq_Hits = Hits * 100
    )
  
  # ggplot(df_pitches, aes(PlateLocSide, PlateLocHeight, color = pitch_zone)) +
  #   geom_point()
  
  load("data/Strikezone_Polys.rda")
  
  #### Fortify Polys ----
  df_sz_polys = fortify(Strikezone_Polys, region = "id")
  
  #### Fix Column Names ----
  colnames(df_sz_polys)[1] <- "x"
  colnames(df_sz_polys)[2] <- "z"
  colnames(df_sz_polys)[6] <- "pitch_zone"
  
  #### Join Data and Polys ----
  df_pitch_polys <- full_join(x = df_pitches, y = df_sz_polys, by = "pitch_zone")
  
  #### Labels ----
  labels <- df_pitch_polys %>%
    select(pitch_zone, Freq_PitchPercent, Freq_NumPitches, Freq_EV, Freq_LA, Freq_Hits) %>%
    distinct() %>%
    arrange(pitch_zone)
  
  #### Label Coordinates ----
  label_info <- data.frame(
    pitch_zone = c("KZONE_01", "KZONE_02", "KZONE_03",
                   "KZONE_04", "KZONE_05", "KZONE_06",
                   "KZONE_07", "KZONE_08", "KZONE_09",
                   "KZONE_11", "KZONE_12", "KZONE_13", "KZONE_14"),
    x = c(-20/3, 0, 20/3, -20/3, 0, 20/3, -20/3, 0, 20/3, -10, 10, -10, 10),
    z = c(38, 38, 38, 30, 30, 30, 22, 22, 22, 44, 44, 16, 16)
  )
  
  #### Join Label Coords ----
  label_coords <- full_join(x = labels, y = label_info, by = "pitch_zone")
  
  #### Fix NA ----
  df_pitch_polys <- df_pitch_polys %>%
    filter(!is.na(Freq_EV))
  
  label_coords <- label_coords %>% 
    filter(!is.na(Freq_EV))
  
  ggplot() +
    geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_path(sz_2, mapping = aes(x, z), color = "black") +
    geom_path(sz_3, mapping = aes(x, z), color = "black") +
    geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(df_pitch_polys, mapping = aes(x, z, group = pitch_zone, fill = Freq_EV),
                 color = "black", show.legend = FALSE) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
    scale_fill_distiller(palette = "RdBu",
                         limits = c(25, max(df_pitch_polys$Freq_EV)),
                         na.value = "lightgrey", guide = "legend") +
    geom_text(label_coords, 
              mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_EV, digits = 1))),
              size = 4, fontface = "bold", family="mono") +
    theme_minimal() +
    labs(
      caption = paste("Date Range: ", min(input$Date), "to", max(input$Date))
    ) +
    TH
}

#### Zone - Launch Angle
zone_launch_angle_pitcher <- function(Pitcher, input) {
  
  load(file = "data/app_setup.rda")
  
  load(file = "data/Strikezone_Polys.rda")
  
  load(file = "data/custom_aes.rda")
  
  #### Setup ----
  require(sp)
  require(sf)
  
  df <- input %>% 
    filter(!is.na(c(PlateLocHeight)))
  
  K_Zone_Points <- SpatialPoints(coords = cbind(df$PlateLocSide, df$PlateLocHeight))
  
  df <- df %>% 
    mutate(
      pitch_zone = case_when(
        over(K_Zone_Points, SZ_SpatialPoly_01, returnList = TRUE) == 1 ~ "KZONE_01",
        over(K_Zone_Points, SZ_SpatialPoly_02, returnList = TRUE) == 1 ~ "KZONE_02",
        over(K_Zone_Points, SZ_SpatialPoly_03, returnList = TRUE) == 1 ~ "KZONE_03",
        over(K_Zone_Points, SZ_SpatialPoly_04, returnList = TRUE) == 1 ~ "KZONE_04",
        over(K_Zone_Points, SZ_SpatialPoly_05, returnList = TRUE) == 1 ~ "KZONE_05",
        over(K_Zone_Points, SZ_SpatialPoly_06, returnList = TRUE) == 1 ~ "KZONE_06",
        over(K_Zone_Points, SZ_SpatialPoly_07, returnList = TRUE) == 1 ~ "KZONE_07",
        over(K_Zone_Points, SZ_SpatialPoly_08, returnList = TRUE) == 1 ~ "KZONE_08",
        over(K_Zone_Points, SZ_SpatialPoly_09, returnList = TRUE) == 1 ~ "KZONE_09",
        over(K_Zone_Points, SZ_SpatialPoly_11, returnList = TRUE) == 1 ~ "KZONE_11",
        over(K_Zone_Points, SZ_SpatialPoly_12, returnList = TRUE) == 1 ~ "KZONE_12",
        over(K_Zone_Points, SZ_SpatialPoly_13, returnList = TRUE) == 1 ~ "KZONE_13",
        over(K_Zone_Points, SZ_SpatialPoly_14, returnList = TRUE) == 1 ~ "KZONE_14",
        
        PlateLocSide < -10 & PlateLocHeight > 30 |
          PlateLocSide < 0 & PlateLocHeight > 42 ~ 'KZONE_11',
        
        PlateLocSide > 10 & PlateLocHeight > 30 |
          PlateLocSide > 0 & PlateLocHeight > 42 ~ 'KZONE_12',
        
        PlateLocSide < -10 & PlateLocHeight < 30 |
          PlateLocSide < 0 & PlateLocHeight < 18 ~ 'KZONE_13',
        
        PlateLocSide > 10 & PlateLocHeight < 30 |
          PlateLocSide > 0 & PlateLocHeight < 18 ~ 'KZONE_14'
      )
    )
  
  # ggplot(df, aes(PlateLocSide, PlateLocHeight, color = pitch_zone)) +
  #   geom_point()
  
  #### Data Frame ----
  df_pitches <- df %>%
    filter(Pitcher_Last == Pitcher) %>%
    select(Pitcher_Last, PlayResult, AutoPitchType, Angle, ExitSpeed, RelSpeed, ynhit, pitch_zone) %>%
    group_by(pitch_zone) %>%
    summarise(
      Pitches = n(),
      EV = mean(ExitSpeed, na.rm = TRUE),
      LA = mean(Angle, na.rm = TRUE),
      Hits = mean(ynhit, na.rm = TRUE)
    ) %>%
    mutate(
      Freq_PitchPercent = (Pitches/sum(Pitches)) * 100,
      Freq_NumPitches = Pitches,
      Freq_EV = EV,
      Freq_LA = LA,
      Freq_Hits = Hits * 100
    )
  
  # ggplot(df_pitches, aes(PlateLocSide, PlateLocHeight, color = pitch_zone)) +
  #   geom_point()
  
  load("data/Strikezone_Polys.rda")
  
  #### Fortify Polys ----
  df_sz_polys = fortify(Strikezone_Polys, region = "id")
  
  #### Fix Column Names ----
  colnames(df_sz_polys)[1] <- "x"
  colnames(df_sz_polys)[2] <- "z"
  colnames(df_sz_polys)[6] <- "pitch_zone"
  
  #### Join Data and Polys ----
  df_pitch_polys <- full_join(x = df_pitches, y = df_sz_polys, by = "pitch_zone")
  
  #### Labels ----
  labels <- df_pitch_polys %>%
    select(pitch_zone, Freq_PitchPercent, Freq_NumPitches, Freq_EV, Freq_LA, Freq_Hits) %>%
    distinct() %>%
    arrange(pitch_zone)
  
  #### Label Coordinates ----
  label_info <- data.frame(
    pitch_zone = c("KZONE_01", "KZONE_02", "KZONE_03",
                   "KZONE_04", "KZONE_05", "KZONE_06",
                   "KZONE_07", "KZONE_08", "KZONE_09",
                   "KZONE_11", "KZONE_12", "KZONE_13", "KZONE_14"),
    x = c(-20/3, 0, 20/3, -20/3, 0, 20/3, -20/3, 0, 20/3, -10, 10, -10, 10),
    z = c(38, 38, 38, 30, 30, 30, 22, 22, 22, 44, 44, 16, 16)
  )
  
  #### Join Label Coords ----
  label_coords <- full_join(x = labels, y = label_info, by = "pitch_zone")
  
  #### Fix NA ----
  df_pitch_polys <- df_pitch_polys %>%
    filter(!is.na(Freq_LA))
  
  label_coords <- label_coords %>% 
    filter(!is.na(Freq_LA))
  
  ggplot() +
    geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_path(sz_2, mapping = aes(x, z), color = "black") +
    geom_path(sz_3, mapping = aes(x, z), color = "black") +
    geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(df_pitch_polys, mapping = aes(x, z, group = pitch_zone, fill = Freq_LA),
                 color = "black", show.legend = FALSE) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
    scale_fill_gradientn(colours = c("#2166ac",'white', '#b2182b', "#b2182b", '#b2182b', 'white', "#2166ac"),
                         limits = c(0, 45), na.value = "#2166ac") +
    geom_text(label_coords, 
              mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_LA, digits = 1))),
              size = 4, fontface = "bold", family="mono") +
    theme_minimal() +
    labs(
      caption = paste("Date Range: ", min(input$Date), "to", max(input$Date))
    ) +
    TH
}

#### Zone - Hit Percentage
zone_hit_perc_pitcher <- function(Pitcher, input) {
  
  load(file = "data/app_setup.rda")
  
  load(file = "data/Strikezone_Polys.rda")
  
  load(file = "data/custom_aes.rda")
  
  #### Setup ----
  require(sp)
  require(sf)
  
  df <- input %>% 
    filter(!is.na(c(PlateLocHeight)))
  
  K_Zone_Points <- SpatialPoints(coords = cbind(df$PlateLocSide, df$PlateLocHeight))
  
  df <- df %>% 
    mutate(
      pitch_zone = case_when(
        over(K_Zone_Points, SZ_SpatialPoly_01, returnList = TRUE) == 1 ~ "KZONE_01",
        over(K_Zone_Points, SZ_SpatialPoly_02, returnList = TRUE) == 1 ~ "KZONE_02",
        over(K_Zone_Points, SZ_SpatialPoly_03, returnList = TRUE) == 1 ~ "KZONE_03",
        over(K_Zone_Points, SZ_SpatialPoly_04, returnList = TRUE) == 1 ~ "KZONE_04",
        over(K_Zone_Points, SZ_SpatialPoly_05, returnList = TRUE) == 1 ~ "KZONE_05",
        over(K_Zone_Points, SZ_SpatialPoly_06, returnList = TRUE) == 1 ~ "KZONE_06",
        over(K_Zone_Points, SZ_SpatialPoly_07, returnList = TRUE) == 1 ~ "KZONE_07",
        over(K_Zone_Points, SZ_SpatialPoly_08, returnList = TRUE) == 1 ~ "KZONE_08",
        over(K_Zone_Points, SZ_SpatialPoly_09, returnList = TRUE) == 1 ~ "KZONE_09",
        over(K_Zone_Points, SZ_SpatialPoly_11, returnList = TRUE) == 1 ~ "KZONE_11",
        over(K_Zone_Points, SZ_SpatialPoly_12, returnList = TRUE) == 1 ~ "KZONE_12",
        over(K_Zone_Points, SZ_SpatialPoly_13, returnList = TRUE) == 1 ~ "KZONE_13",
        over(K_Zone_Points, SZ_SpatialPoly_14, returnList = TRUE) == 1 ~ "KZONE_14",
        
        PlateLocSide < -10 & PlateLocHeight > 30 |
          PlateLocSide < 0 & PlateLocHeight > 42 ~ 'KZONE_11',
        
        PlateLocSide > 10 & PlateLocHeight > 30 |
          PlateLocSide > 0 & PlateLocHeight > 42 ~ 'KZONE_12',
        
        PlateLocSide < -10 & PlateLocHeight < 30 |
          PlateLocSide < 0 & PlateLocHeight < 18 ~ 'KZONE_13',
        
        PlateLocSide > 10 & PlateLocHeight < 30 |
          PlateLocSide > 0 & PlateLocHeight < 18 ~ 'KZONE_14'
      )
    )
  
  # ggplot(df, aes(PlateLocSide, PlateLocHeight, color = pitch_zone)) +
  #   geom_point()
  
  #### Data Frame ----
  df_pitches <- df %>%
    filter(Pitcher_Last == Pitcher) %>%
    select(Pitcher_Last, PlayResult, AutoPitchType, Angle, ExitSpeed, RelSpeed, ynhit, pitch_zone) %>%
    group_by(pitch_zone) %>%
    summarise(
      Hits = mean(ynhit, na.rm = TRUE)
    ) %>%
    mutate(
      Freq_Hits = Hits * 100
    )

  
  load("data/Strikezone_Polys.rda")
  
  #### Fortify Polys ----
  df_sz_polys = fortify(Strikezone_Polys, region = "id")
  
  #### Fix Column Names ----
  colnames(df_sz_polys)[1] <- "x"
  colnames(df_sz_polys)[2] <- "z"
  colnames(df_sz_polys)[6] <- "pitch_zone"
  
  #### Join Data and Polys ----
  df_pitch_polys <- full_join(x = df_pitches, y = df_sz_polys, by = "pitch_zone")
  
  #### Labels ----
  labels <- df_pitch_polys %>%
    select(pitch_zone, Freq_Hits) %>%
    distinct() %>%
    arrange(pitch_zone)
  
  #### Label Coordinates ----
  label_info <- data.frame(
    pitch_zone = c("KZONE_01", "KZONE_02", "KZONE_03",
                   "KZONE_04", "KZONE_05", "KZONE_06",
                   "KZONE_07", "KZONE_08", "KZONE_09",
                   "KZONE_11", "KZONE_12", "KZONE_13", "KZONE_14"),
    x = c(-20/3, 0, 20/3, -20/3, 0, 20/3, -20/3, 0, 20/3, -10, 10, -10, 10),
    z = c(38, 38, 38, 30, 30, 30, 22, 22, 22, 44, 44, 16, 16)
  )
  
  #### Join Label Coords ----
  label_coords <- full_join(x = labels, y = label_info, by = "pitch_zone")
  
  #### Fix NA ----
  df_pitch_polys <- df_pitch_polys %>%
    filter(!is.na(Freq_Hits))
  
  label_coords <- label_coords %>% 
    filter(!is.na(Freq_Hits))
  
  ggplot() +
    geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_path(sz_2, mapping = aes(x, z), color = "black") +
    geom_path(sz_3, mapping = aes(x, z), color = "black") +
    geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(df_pitch_polys, mapping = aes(x, z, group = pitch_zone, fill = Freq_Hits),
                 color = "black", show.legend = FALSE) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
    scale_fill_distiller(palette = "RdYlBu", limits = c(0, max(df_pitch_polys$Freq_Hits)),
                         na.value = "lightgrey") +
    geom_text(label_coords, 
              mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_Hits, digits = 1))),
              size = 4, fontface = "bold", family="mono") +
    theme_minimal() +
    labs(
      caption = paste("Date Range: ", min(input$Date), "to", max(input$Date))
    ) +
    TH
}

#### Zone - Swing Percentage
zone_swing_perc_pitcher <- function(Pitcher, input) {
  
  load(file = "data/app_setup.rda")
  
  load(file = "data/Strikezone_Polys.rda")
  
  load(file = "data/custom_aes.rda")
  
  #### Setup ----
  require(sp)
  require(sf)
  
  df <- input %>% 
    filter(!is.na(c(PlateLocHeight)))
  
  K_Zone_Points <- SpatialPoints(coords = cbind(df$PlateLocSide, df$PlateLocHeight))
  
  df <- df %>% 
    mutate(
      pitch_zone = case_when(
        over(K_Zone_Points, SZ_SpatialPoly_01, returnList = TRUE) == 1 ~ "KZONE_01",
        over(K_Zone_Points, SZ_SpatialPoly_02, returnList = TRUE) == 1 ~ "KZONE_02",
        over(K_Zone_Points, SZ_SpatialPoly_03, returnList = TRUE) == 1 ~ "KZONE_03",
        over(K_Zone_Points, SZ_SpatialPoly_04, returnList = TRUE) == 1 ~ "KZONE_04",
        over(K_Zone_Points, SZ_SpatialPoly_05, returnList = TRUE) == 1 ~ "KZONE_05",
        over(K_Zone_Points, SZ_SpatialPoly_06, returnList = TRUE) == 1 ~ "KZONE_06",
        over(K_Zone_Points, SZ_SpatialPoly_07, returnList = TRUE) == 1 ~ "KZONE_07",
        over(K_Zone_Points, SZ_SpatialPoly_08, returnList = TRUE) == 1 ~ "KZONE_08",
        over(K_Zone_Points, SZ_SpatialPoly_09, returnList = TRUE) == 1 ~ "KZONE_09",
        over(K_Zone_Points, SZ_SpatialPoly_11, returnList = TRUE) == 1 ~ "KZONE_11",
        over(K_Zone_Points, SZ_SpatialPoly_12, returnList = TRUE) == 1 ~ "KZONE_12",
        over(K_Zone_Points, SZ_SpatialPoly_13, returnList = TRUE) == 1 ~ "KZONE_13",
        over(K_Zone_Points, SZ_SpatialPoly_14, returnList = TRUE) == 1 ~ "KZONE_14",
        
        PlateLocSide < -10 & PlateLocHeight > 30 |
          PlateLocSide < 0 & PlateLocHeight > 42 ~ 'KZONE_11',
        
        PlateLocSide > 10 & PlateLocHeight > 30 |
          PlateLocSide > 0 & PlateLocHeight > 42 ~ 'KZONE_12',
        
        PlateLocSide < -10 & PlateLocHeight < 30 |
          PlateLocSide < 0 & PlateLocHeight < 18 ~ 'KZONE_13',
        
        PlateLocSide > 10 & PlateLocHeight < 30 |
          PlateLocSide > 0 & PlateLocHeight < 18 ~ 'KZONE_14'
      )
    )
  
  df <- df %>% 
    mutate(
      swing = ifelse(
        PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay"), 1, 0)
    )
  
  #### Data Frame ----
  df_pitches <- df %>%
    filter(Pitcher_Last == Pitcher) %>%
    select(Pitcher_Last, PlayResult, AutoPitchType, Angle, ExitSpeed, RelSpeed, swing, pitch_zone) %>%
    group_by(pitch_zone) %>%
    summarise(
      Swings = mean(swing, na.rm = TRUE)
    ) %>%
    mutate(
      Freq_SwingPercent = Swings * 100
    )
  
  load("data/Strikezone_Polys.rda")
  
  #### Fortify Polys ----
  df_sz_polys = fortify(Strikezone_Polys, region = "id")
  
  #### Fix Column Names ----
  colnames(df_sz_polys)[1] <- "x"
  colnames(df_sz_polys)[2] <- "z"
  colnames(df_sz_polys)[6] <- "pitch_zone"
  
  #### Join Data and Polys ----
  df_pitch_polys <- full_join(x = df_pitches, y = df_sz_polys, by = "pitch_zone")
  
  #### Labels ----
  labels <- df_pitch_polys %>%
    select(pitch_zone, Freq_SwingPercent) %>%
    distinct() %>%
    arrange(pitch_zone)
  
  #### Label Coordinates ----
  label_info <- data.frame(
    pitch_zone = c("KZONE_01", "KZONE_02", "KZONE_03",
                   "KZONE_04", "KZONE_05", "KZONE_06",
                   "KZONE_07", "KZONE_08", "KZONE_09",
                   "KZONE_11", "KZONE_12", "KZONE_13", "KZONE_14"),
    x = c(-20/3, 0, 20/3, -20/3, 0, 20/3, -20/3, 0, 20/3, -10, 10, -10, 10),
    z = c(38, 38, 38, 30, 30, 30, 22, 22, 22, 44, 44, 16, 16)
  )
  
  #### Join Label Coords ----
  label_coords <- full_join(x = labels, y = label_info, by = "pitch_zone")
  
  #### Fix NA ----
  df_pitch_polys <- df_pitch_polys %>%
    filter(!is.na(Freq_SwingPercent))
  
  label_coords <- label_coords %>% 
    filter(!is.na(Freq_SwingPercent))
  
  ggplot() +
    geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_path(sz_2, mapping = aes(x, z), color = "black") +
    geom_path(sz_3, mapping = aes(x, z), color = "black") +
    geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(df_pitch_polys, mapping = aes(x, z, group = pitch_zone, fill = Freq_SwingPercent),
                 color = "black", show.legend = FALSE) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
    scale_fill_distiller(palette = "RdYlBu", limits = c(0, max(df_pitch_polys$Freq_SwingPercent)),
                         na.value = "lightgrey") +
    geom_text(label_coords, 
              mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_SwingPercent, digits = 1))),
              size = 4, fontface = "bold", family="mono") +
    theme_minimal() +
    labs(
      caption = paste("Date Range: ", min(input$Date), "to", max(input$Date))
    ) +
    TH
}


save(mutate_data_pitcher, batted_ball_type_pitcher, pitch_result_pitcher, pitch_types_pitcher,
     pitch_description_pitcher, pitch_heatmap_pitcher, zone_swing_perc_pitcher, zone_hit_perc_pitcher,
     zone_launch_angle_pitcher, zone_exit_velo_pitcher, zone_total_pitches_pitcher,
     zone_pitch_perc_pitcher, file = "data/functions_pitcher.rda")





# library(tidyverse)
# 
# load("data/functions.rda")
# load("data/custom_aes.rda")
# season <- read.csv("data/23_season.csv")
# 
# mutate_data(season)
# load("data/nu_df.rda")
# 
# pitch_types("Calarco", nu_df)






