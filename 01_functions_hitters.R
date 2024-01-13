library(tidyverse)
library(sp)
library(sf)

mutate_data <- function(input) {
  
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
  
  nu_df <- nu_df %>% 
    mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
    mutate(
      Date = format(Date, "%b-%d")
    )
  
  nu_df <- nu_df %>% 
    mutate(
      swing = ifelse(
        PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay"), 1, 0),
      whiff = ifelse(
        PitchCall %in% c("StrikeSwinging"), 1, 0)
    )
  
  save(nu_df, file = "data/nu_df.rda")
}

#### Pitch Types
pitch_types <- function(Batter, df) {
  
  load(file = "data/app_setup.rda")
  
  load(file = "data/Strikezone_Polys.rda")
  
  load(file = "data/custom_aes.rda")
  
  ggplot() +
    geom_point(df %>% filter(Batter_Last == Batter),
               mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = AutoPitchType),
               size = 5, shape = 21, position = "jitter") +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
    coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
    theme_minimal() +
    TH +
    theme(
      plot.margin = unit(c(1,1,1,1), "cm"),
      legend.position = "bottom",
      legend.background = element_rect(color = "#582c83")
    )
  
}

#### Pitch Result
pitch_result <- function(Batter, df) {
  
  load(file = "data/app_setup.rda")
  
  load(file = "data/Strikezone_Polys.rda")
  
  load(file = "data/custom_aes.rda")
  
  ggplot() +
    geom_point(df %>% filter(Batter_Last == Batter & !is.na(PlayResult)),
               mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = PlayResult),
               size = 5, shape = 21) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
    scale_fill_manual(values = PlayResultColors) +
    coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
    theme_minimal() +
    TH +
    theme(
      plot.margin = unit(c(1,1,1,1), "cm"),
      legend.position = "bottom",
      legend.background = element_rect(color = "#582c83")
    )
}

#### Pitch Desc.
pitch_description <- function(Batter, df) {
  
  load(file = "data/app_setup.rda")
  
  load(file = "data/Strikezone_Polys.rda")
  
  load(file = "data/custom_aes.rda")
  
  ggplot() +
    geom_point(df %>% filter(Batter_Last == Batter),
               mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = PitchCall),
               size = 5, shape = 21) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") + 
    scale_fill_manual(values = PitchCallColors) +
    coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
    theme_minimal() +
    TH +
    theme(
      plot.margin = unit(c(1,1,1,1), "cm"),
      legend.position = "bottom",
      legend.background = element_rect(color = "#582c83")
    )
}

#### Batted Ball Type
batted_ball_type <- function(Batter, df) {
  load(file = "data/app_setup.rda")
  
  load(file = "data/Strikezone_Polys.rda")
  
  load(file = "data/custom_aes.rda")
  
  ggplot() +
    geom_point(df %>% filter(Batter_Last == Batter & !is.na(AutoHitType)),
               mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = AutoHitType),
               size = 5, shape = 21) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
    scale_fill_manual(values = HitTypeColors) +
    coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
    theme_minimal() +
    TH +
    theme(
      plot.margin = unit(c(1,1,1,1), "cm"),
      legend.position = "bottom",
      legend.background = element_rect(color = "#582c83")
    )
}

#### Contact Type
contact_type <- function(Batter, df) {
  load(file = "data/app_setup.rda")
  
  load(file = "data/Strikezone_Polys.rda")
  
  load(file = "data/custom_aes.rda")
  
  ggplot() +
    geom_point(df %>% filter(Batter_Last == Batter & !is.na(launch_speed_angle)),
               mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = launch_speed_angle),
               size = 5, shape = 21) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
    coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
    theme_minimal() +
    TH +
    theme(
      plot.margin = unit(c(1,1,1,1), "cm"),
      legend.position = "bottom",
      legend.background = element_rect(color = "#582c83")
    )
}

# Heatmaps ----

#### Pitch Heatmap
pitch_heatmap <- function(Batter, df) {
  
  load(file = "data/app_setup.rda")
  
  load(file = "data/Strikezone_Polys.rda")
  
  load(file = "data/custom_aes.rda")
  
  ggplot(df %>% filter(Batter_Last == Batter),
         mapping = aes(PlateLocSide, PlateLocHeight)) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    stat_density_2d(geom = "polygon", aes(fill = after_stat(nlevel)), show.legend = FALSE) +
    geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
    scale_fill_distiller(palette = "RdBu") +
    coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
    theme_minimal() +
    TH +
    theme(
      plot.margin = unit(c(1,1,1,1), "cm"),
      legend.position = "bottom",
      legend.background = element_rect(color = "#582c83")
    )
}

#### Swing Heatmap
swing_heatmap <- function(Batter, df) {
  
  load(file = "data/app_setup.rda")
  
  load(file = "data/Strikezone_Polys.rda")
  
  load(file = "data/custom_aes.rda")
  
  df <- df %>% 
    filter(PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay"))
  
  ggplot(df %>% filter(Batter_Last == Batter),
         mapping = aes(PlateLocSide, PlateLocHeight)) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    stat_density_2d(geom = "polygon", aes(fill = after_stat(nlevel)), show.legend = FALSE) +
    geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
    scale_fill_distiller(palette = "RdBu") +
    coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
    theme_minimal() +
    TH +
    theme(
      plot.margin = unit(c(1,1,1,1), "cm"),
      legend.position = "bottom",
      legend.background = element_rect(color = "#582c83")
    )
}

# Polys ----

zone_function_setup <- function(Batter, input){
  
  require(sp)
  require(sf)
  require(tidyverse)
  
  load(file = "data/app_setup.rda")
  load(file = "data/Strikezone_Polys.rda")
  
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
  
  #### Data Frame ----
  df_pitches <- df %>%
    filter(Batter_Last == Batter) %>%
    group_by(pitch_zone) %>%
    summarise(
      Pitches = n(),
      EV = mean(ExitSpeed, na.rm = TRUE),
      LA = mean(Angle, na.rm = TRUE), 
      Hits = mean(ynhit, na.rm = TRUE),
      Swings = mean(swing, na.rm = TRUE),
      Whiffs = mean(whiff, na.rm = TRUE)
    ) %>%
    mutate(
      Freq_PitchPercent = (Pitches/sum(Pitches)) * 100,
      Freq_NumPitches = Pitches,
      Freq_EV = EV,
      Freq_LA = LA,
      Freq_Hits = Hits * 100,
      Freq_Swings = Swings * 100,
      Freq_Whiffs = Whiffs * 100
    )
  
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
    select(pitch_zone, Freq_PitchPercent, Freq_NumPitches, Freq_EV,
           Freq_LA, Freq_Hits, Freq_Swings, Freq_Whiffs) %>%
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
  
  #### Save data ----
  
  save(df_pitch_polys, label_coords, file = "data/zone_setup.rda")
  
}

#### Zone - Pitch Percent
zone_pitch_perc <- function(Batter, df_input, label_input) {
  
  #### Load App Setup
  load(file = "data/app_setup.rda")
  
  #### Fix NA ----
  df_input <- df_input %>%
    filter(!is.na(Freq_PitchPercent))
  
  label_input <- label_input %>% 
    filter(!is.na(Freq_PitchPercent))
  
  #### Create Plot ----
  ggplot() +
    geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_path(sz_2, mapping = aes(x, z), color = "black") +
    geom_path(sz_3, mapping = aes(x, z), color = "black") +
    geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(df_input, mapping = aes(x, z, group = pitch_zone, fill = Freq_PitchPercent),
                 color = "black", show.legend = FALSE) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
    scale_fill_distiller(palette = "RdBu", limits = c(0, max(df_input$Freq_PitchPercent) + 5),
                         na.value = "lightgrey", guide = "legend") +
    geom_text(label_input, 
              mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_PitchPercent, digits = 1))),
              size = 4, fontface = "bold", family = "mono") +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      legend.text = element_text(family = "sans", size = 12),
      legend.title = element_blank(),
      legend.position = "right"
    )
}

#### Zone - Pitch Percent
zone_total_pitches <- function(Batter, df_input, label_input) {
  
  load(file = "data/app_setup.rda")
  
  #### Fix NA ----
  df_input <- df_input %>%
    filter(!is.na(Freq_NumPitches))
  
  label_input <- label_input %>% 
    filter(!is.na(Freq_NumPitches))
  
  #### Create Plot ----
  ggplot() +
    geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_path(sz_2, mapping = aes(x, z), color = "black") +
    geom_path(sz_3, mapping = aes(x, z), color = "black") +
    geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(df_input, mapping = aes(x, z, group = pitch_zone, fill = Freq_NumPitches),
                 color = "black", show.legend = FALSE) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
    scale_fill_distiller(palette = "RdBu", limits = c(0, max(df_input$Freq_NumPitches)),
                         na.value = "lightgrey", guide = "legend") +
    geom_text(label_input, 
              mapping = aes(x, z, label = Freq_NumPitches),
              size = 4, fontface = "bold", family="mono") +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      legend.text = element_text(family = "sans", size = 12),
      legend.title = element_blank(),
      legend.position = "right"
    )
}

#### Zone - Exit Velocity
zone_exit_velo <- function(Batter, df_input, label_input) {
  
  load(file = "data/app_setup.rda")
  
  #### Fix NA ----
  df_input <- df_input %>%
    filter(!is.na(Freq_EV))
  
  label_input <- label_input %>% 
    filter(!is.na(Freq_EV))
  
  #### Create Plot ----
  ggplot() +
    geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_path(sz_2, mapping = aes(x, z), color = "black") +
    geom_path(sz_3, mapping = aes(x, z), color = "black") +
    geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(df_input, mapping = aes(x, z, group = pitch_zone, fill = Freq_EV),
                 color = "black", show.legend = FALSE) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
    scale_fill_distiller(palette = "RdBu",
                         limits = c(25, max(df_input$Freq_EV)),
                         na.value = "lightgrey", guide = "legend") +
    geom_text(label_input, 
              mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_EV, digits = 1))),
              size = 4, fontface = "bold", family="mono") +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      legend.text = element_text(family = "sans", size = 12),
      legend.title = element_blank(),
      legend.position = "right"
    )
}

#### Zone - Launch Angle
zone_launch_angle <- function(Batter, df_input, label_input) {
  
  load(file = "data/app_setup.rda")
  
  #### Fix NA ----
  df_input <- df_input %>%
    filter(!is.na(Freq_LA))
  
  label_input <- label_input %>% 
    filter(!is.na(Freq_LA))
  
  #### Create Plot ----
  ggplot() +
    geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_path(sz_2, mapping = aes(x, z), color = "black") +
    geom_path(sz_3, mapping = aes(x, z), color = "black") +
    geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(df_input, mapping = aes(x, z, group = pitch_zone, fill = Freq_LA),
                 color = "black", show.legend = FALSE) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
    scale_fill_gradientn(colours = c("#2166ac",'white', '#b2182b', "#b2182b", '#b2182b', 'white', "#2166ac"),
                         limits = c(0, 45), na.value = "#2166ac") +
    geom_text(label_input, 
              mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_LA, digits = 1))),
              size = 4, fontface = "bold", family="mono") +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      legend.text = element_text(family = "sans", size = 12),
      legend.title = element_blank(),
      legend.position = "right"
    )
}

#### Zone - Hit Percentage
zone_hit_perc <- function(Batter, df_input, label_input) {
  
  #### Load App Setup
  load(file = "data/app_setup.rda")
  
  #### Fix NA ----
  df_input <- df_input %>%
    filter(!is.na(Freq_Hits))
  
  label_input <- label_input %>% 
    filter(!is.na(Freq_Hits))
  
  #### Create Plot ----
  ggplot() +
    geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_path(sz_2, mapping = aes(x, z), color = "black") +
    geom_path(sz_3, mapping = aes(x, z), color = "black") +
    geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(df_input, mapping = aes(x, z, group = pitch_zone, fill = Freq_Hits),
                 color = "black", show.legend = FALSE) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
    scale_fill_distiller(palette = "RdYlBu", limits = c(0, max(df_input$Freq_Hits)),
                         na.value = "lightgrey") +
    geom_text(label_input, 
              mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_Hits, digits = 1))),
              size = 4, fontface = "bold", family="mono") +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      legend.text = element_text(family = "sans", size = 12),
      legend.title = element_blank(),
      legend.position = "right"
    )
}

#### Zone - Swing Percentage
zone_swing_perc <- function(Batter, df_input, label_input) {
  
  #### Load App Setup
  load(file = "data/app_setup.rda")
  
  #### Fix NA ----
  df_input <- df_input %>%
    filter(!is.na(Freq_Swings))
  
  label_input <- label_input %>% 
    filter(!is.na(Freq_Swings))
  
  #### Create Plot ----
  ggplot() +
    geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_path(sz_2, mapping = aes(x, z), color = "black") +
    geom_path(sz_3, mapping = aes(x, z), color = "black") +
    geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(df_input, mapping = aes(x, z, group = pitch_zone, fill = Freq_Swings),
                 color = "black", show.legend = FALSE) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
    scale_fill_distiller(palette = "RdYlBu", limits = c(0, max(df_input$Freq_Swings)),
                         na.value = "lightgrey") +
    geom_text(label_input, 
              mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_Swings, digits = 1))),
              size = 4, fontface = "bold", family="mono") +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      legend.text = element_text(family = "sans", size = 12),
      legend.title = element_blank(),
      legend.position = "right"
    )
}

#### Zone - Swing Percentage
zone_whiff_perc <- function(Batter, df_input, label_input) {
  
  #### Load App Setup
  load(file = "data/app_setup.rda")
  
  #### Fix NA ----
  df_input <- df_input %>%
    filter(!is.na(Freq_Whiffs))
  
  label_input <- label_input %>% 
    filter(!is.na(Freq_Whiffs))
  
  #### Create Plot ----
  ggplot() +
    geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_path(sz_2, mapping = aes(x, z), color = "black") +
    geom_path(sz_3, mapping = aes(x, z), color = "black") +
    geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(df_input, mapping = aes(x, z, group = pitch_zone, fill = Freq_Whiffs),
                 color = "black", show.legend = FALSE) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
    scale_fill_distiller(palette = "RdYlBu", limits = c(0, max(df_input$Freq_Whiffs)),
                         na.value = "lightgrey") +
    geom_text(label_input, 
              mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_Whiffs, digits = 1))),
              size = 4, fontface = "bold", family="mono") +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      legend.text = element_text(family = "sans", size = 12),
      legend.title = element_blank(),
      legend.position = "right"
    )
}


# Save Data ----
save(mutate_data, zone_function_setup,
     
     batted_ball_type, pitch_result, pitch_types, pitch_description,
     pitch_heatmap, contact_type, swing_heatmap,
     
     zone_swing_perc, zone_hit_perc, zone_launch_angle, zone_exit_velo,
     zone_total_pitches, zone_pitch_perc, zone_swing_perc, zone_whiff_perc,
     
     file = "data/functions.rda")



# ----

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



# test1 <- function(batter, pitcher, df) {
# 
#   if (is.null(pitcher)) {
#     df <- df %>%
#       filter(Batter == batter & !PlayResult %in% c("Undefined"))
#   }
# 
#   if (is.null(batter)) {
#     df <- df %>%
#       filter(Pitcher == pitcher & !PlayResult %in% c("Undefined"))
#   }
# 
#   ggplot() +
#     coord_equal(xlim = c(-2, 2), ylim = c(0, 4)) +
#     geom_point(df,
#                mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = PlayResult),
#                size = 5, shape = 21, position = "jitter") +
#     geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
#     geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
#     geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
#     geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
#     scale_fill_manual(values = PlayResultColors) +
#     theme_minimal() +
#     TH +
#     theme(
#       plot.margin = unit(c(1,1,1,1), "cm"),
#       legend.position = "bottom",
#       legend.background = element_rect(color = "#582c83")
#     )
# }
# 
# test1("Calarco, Alex", NULL, season)
# 
# test1(NULL,"Sund, Ethan",season)






