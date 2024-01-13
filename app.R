library(tidyverse)
library(shiny)
library(patchwork)
library(reactable)
library(grid)
library(ggridges)
library(sf)
library(sp)
library(rsconnect)
library(scales)
library(ggpubr)
library(bslib)
library(shinythemes)

load(file = "data/app_setup.rda")

ui <- navbarPage("DS8 Analytics", #### ----
                 fluid = TRUE,
                 theme = bs_theme(version = 5, bootswatch = "pulse"),
                 
                 tabPanel("Hitters", #### Hitters ----
                          sidebarLayout(
                            position = "left",
                            
                            sidebarPanel(
                              width = 3,
                              fileInput("upload_1", "Upload TrackMan .csv File", accept = c(".csv")),
                              
                              textInput("hitter",
                                        "Enter Hitter's Last Name",
                                        value = "",
                                        width = NULL,
                                        placeholder = NULL),
                              
                              selectInput("var_1", 
                                          label = "Select Chart",
                                          choices = chart_types,
                                          selected = NULL),
                              
                              selectInput("pitcher_throws", 
                                          label = "Pitcher Throws:",
                                          choices = Pitcher_Throws),
                              
                              actionButton("goButton_1", "Create Visual")
                            ),
                            
                            mainPanel(
                              width = 8,
                              plotOutput("hitterPlots")
                            )
                          )
                 ),
                 tabPanel("Pitchers", #### Pitchers ----
                          sidebarLayout(
                            position = "left",
                            
                            sidebarPanel(
                              width = 3,
                              fileInput("upload_2", "Upload TrackMan .csv File", accept = c(".csv")),
                              
                              textInput("pitcher",
                                        "Enter Pitcher's Last Name",
                                        value = "",
                                        width = NULL,
                                        placeholder = NULL),
                              
                              selectInput("var_2", 
                                          label = "Select Chart",
                                          choices = chart_types,
                                          selected = NULL),
                              
                              selectInput("batter_side", 
                                          label = "Batter Stands:",
                                          choices = Batter_Stands),
                              
                              actionButton("goButton_2", "Create Visual")
                            ),
                            
                            mainPanel(
                              width = 8,
                              plotOutput("pitcherPlots")
                            )
                          )
                 ),
                 tabPanel("Catchers", #### Catchers ----
                          sidebarLayout(
                            position = "left",
                            
                            sidebarPanel(
                              width = 3,
                              fileInput("upload_3", "Upload TrackMan .csv File", accept = c(".csv")),
                              
                              textInput("catcher",
                                        "Enter Catcher's Last Name",
                                        value = "",
                                        width = NULL,
                                        placeholder = NULL),
                              
                              selectInput("var_3", 
                                          label = "Select Chart",
                                          choices = c("Called Strike Percentage"),
                                          selected = NULL),
                              
                              actionButton("goButton_3", "Create Visual")
                            ),
                            
                            mainPanel(
                              width = 8,
                              plotOutput("catcherPlots")
                            )
                          )
                 )
)

#### Server ----
server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 10 * 1024^2)
  
  # Hitters ----
  df_1 <- eventReactive(input$upload_1, {
    read.csv(input$upload_1$datapath)
  })
  
  data_1 <- eventReactive(input$goButton_1, {
    # Load Files ----
    load(file = "data/Strikezone_Polys.rda")
    load(file = "data/custom_aes.rda")
    load(file = "data/functions.rda")
    
    #### Mutate Data ----
    mutate_data(df_1())
    
    #### Load New Data ----
    load(file = "data/nu_df.rda")
    
    zone_function_setup(input$hitter, nu_df)
    
    load(file = "data/zone_setup.rda")
    
    #### Pitcher Handness ----
    if (input$pitcher_throws == "Right") {
      nu_df <- nu_df %>% 
        filter(PitcherThrows == "Right")
    }
    else if (input$pitcher_throws == "Left") {
      nu_df <- nu_df %>% 
        filter(PitcherThrows == "Left")
    }
    
    #### Plots ----
    if (input$var_1 == "Zone - Total Pitches") {
      zone_total_pitches(input$hitter, df_pitch_polys, label_coords)
    }
    else if (input$var_1 == "Zone - Pitch Percentage") {
      zone_pitch_perc(input$hitter, df_pitch_polys, label_coords)
    }
    else if (input$var_1 == "Zone - Exit Velocity") {
      zone_exit_velo(input$hitter, df_pitch_polys, label_coords)
    }
    else if (input$var_1 == "Zone - Launch Angle") {
      zone_launch_angle(input$hitter, df_pitch_polys, label_coords)
    }
    else if (input$var_1 == "Zone - Hit Percentage") {
      zone_hit_perc(input$hitter, df_pitch_polys, label_coords)
    }
    else if (input$var_1 == "Zone - Swing Percentage") {
      zone_swing_perc(input$hitter, df_pitch_polys, label_coords)
    }
    else if (input$var_1 == "Zone - Whiff Percentage") {
      zone_whiff_perc(input$hitter, df_pitch_polys, label_coords)
    }
    
    else if (input$var_1 == "Pitch Types") {
      pitch_types(input$hitter, nu_df)
    }
    else if (input$var_1 == "Pitch Description") {
      pitch_description(input$hitter, nu_df)
    }
    else if (input$var_1 == "Pitch Result") {
      pitch_result(input$hitter, nu_df)
    }
    else if (input$var_1 == "Batted Ball Type") {
      batted_ball_type(input$hitter, nu_df)
    }
    else if (input$var_1 == "Contact Type") {
      contact_type(input$hitter, nu_df)
    }
    else if (input$var_1 == "Pitch Heatmap") {
      pitch_heatmap(input$hitter, nu_df)
    }
  })
  
  
  # Pitchers ----
  df_2 <- eventReactive(input$upload_2, {
    read.csv(input$upload_2$datapath)
  })
  
  data_2 <- eventReactive(input$goButton_2, {
    # Load Files ----
    load(file = "data/Strikezone_Polys.rda")
    
    load(file = "data/custom_aes.rda")
    
    load(file = "data/functions_pitcher.rda")
    
    #### Mutate Data ----
    mutate_data_pitcher(df_2())
    
    load(file = "data/nu_df.rda")
    
    nu_df <- nu_df %>% 
      mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
      mutate(
        Date = format(Date, "%b-%d")
      )
    
    if (input$batter_side == "Right") {
      nu_df <- nu_df %>% 
        filter(BatterSide == "Right")
    }
    else if (input$batter_side == "Left") {
      nu_df <- nu_df %>% 
        filter(BatterSide == "Left")
    }
    
    #### Plots ----
    if (input$var_2 == "Batted Ball Type") {
      batted_ball_type_pitcher(input$pitcher, nu_df)
    }
    
    else if (input$var_2 == "Pitch Result") {
      pitch_result_pitcher(input$pitcher, nu_df)
    }
    
    else if (input$var_2 == "Pitch Types") {
      pitch_types_pitcher(input$pitcher, nu_df)
    }
    
    else if (input$var_2 == "Pitch Description") {
      pitch_description_pitcher(input$pitcher, nu_df)
    }
    
    else if (input$var_2 == "Pitch Heatmap") {
      pitch_heatmap_pitcher(input$pitcher, nu_df)
    }
    
    else if (input$var_2 == "Zone - Pitch Percentage") {
      zone_pitch_perc_pitcher(input$pitcher, nu_df)
    }
    
    else if (input$var_2 == "Zone - Total Pitches") {
      zone_total_pitches_pitcher(input$pitcher, nu_df)
    }
    
    else if (input$var_2 == "Zone - Exit Velocity") {
      zone_exit_velo_pitcher(input$pitcher, nu_df)
    }
    
    else if (input$var_2 == "Zone - Launch Angle") {
      zone_launch_angle_pitcher(input$pitcher, nu_df)
    }
    
    else if (input$var_2 == "Zone - Hit Percentage") {
      zone_hit_perc_pitcher(input$pitcher, nu_df)
    }
    
    else if (input$var_2 == "Zone - Swing Percentage") {
      zone_swing_perc_pitcher(input$pitcher, nu_df)
    }
    
    else if (input$var_2 == "Release Point") {
      
      nu_purp <- "#4E2A84"
      
      ggplot(nu_df %>% filter(Pitcher_Last == input$pitcher)) +
        geom_point(aes(RelSide*12, RelHeight*12, fill = AutoPitchType), shape = 21, size = 3) +
        geom_polygon(home_plate, mapping = aes(x*12, z*12), fill = "grey", color = "grey") +
        coord_equal() +
        theme_linedraw() +
        geom_hline(aes(yintercept = 0), linewidth = 0.3, lty = 2) +
        geom_vline(aes(xintercept = 0), linewidth = 0.3, lty = 2) +
        scale_x_continuous(limits = c(-45, 45),
                           breaks = seq(-45, 45, by = 10)) +
        scale_y_continuous(limits = c(0, 90),
                           breaks = seq(0, 90, by = 10)) +
        labs(
          x = "Release Side (in.)",
          y = "Release Height (in.)",
          title = "Release Point",
          subtitle = paste(input$pitcher),
          fill = "Pitch Type"
        ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", color = nu_purp),
          plot.subtitle = element_text(hjust = 0.5, color = nu_purp),
          axis.title.x = element_text(size = 10, face="bold", color = "black"),
          axis.title.y = element_text(size = 10, face="bold", color = "black"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          legend.position = "bottom"
        )
    }
    
    else if (input$var_2 == "Release Extension") {
      
      x <- c(0, 0, 34/12, 9, 0)
      z <- c(-1, 0, 0, -1, -1)
      
      pitching_mound <- data.frame(x, z)
      
      pitches <- nu_df %>%
        filter(Pitcher_Last == input$pitcher & !is.na(Extension))
      
      mean <- pitches %>% 
        filter(Pitcher_Last == input$pitcher) %>% 
        group_by(AutoPitchType) %>%
        summarise(
          mean_val = mean(Extension, na.rm = TRUE)
        )
      
      
      ggplot(pitches) +
        geom_polygon(pitching_mound, mapping = aes(x, z), fill = "#ededed", lty = 1, color = "lightgrey") +
        geom_point(aes(Extension, RelHeight, fill = AutoPitchType), shape = 21, size = 3) +
        coord_cartesian(xlim = c(min(pitches$Extension), max(pitches$Extension))) +
        theme_linedraw() +
        geom_vline(data = mean, aes(xintercept = mean_val, col = AutoPitchType), lty = 2, show.legend = FALSE) +
        scale_y_continuous(limits = c(-1, 8),
                           breaks = seq(-1, 8, by = 1),
                           expand = c(0, 0)) +
        labs(
          x = "Extension (ft)",
          y = "Release Height (ft)",
          fill = "Pitch Type"
        ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          axis.title.x = element_text(size = 10, face="bold", color = "black"),
          axis.title.y = element_text(size = 10, face="bold", color = "black"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          legend.position = "bottom"
        )
    }
    
    else if (input$var_2 == "Pitch Movement") {
      
      pitches <- nu_df %>%
        filter(Pitcher_Last == input$pitcher & !is.na(Extension))
      
      pitches_dat <- pitches %>%
        select(AutoPitchType, HorzBreak, InducedVertBreak) %>% 
        group_by(AutoPitchType) %>%
        summarise(
          Avg_HB = mean(HorzBreak),
          Avg_IVB = mean(InducedVertBreak)
        )
      nu_purp <- "#4E2A84"
      
      ggplot() +
        geom_point(pitches, mapping = aes(HorzBreak, InducedVertBreak, fill = AutoPitchType),
                   shape = 21, size = 5, alpha = 0.7) +
        geom_point(pitches_dat, mapping = aes(Avg_HB, Avg_IVB, fill = AutoPitchType),
                   shape = 21, size = 8, stroke = 1.5) +
        coord_fixed(xlim = c(-40, 40), ylim = c(-40, 40)) +
        theme_linedraw() +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
        labs(
          x = "Horz. Break (in.)",
          y = "Ind. Vert. Break (in.)",
          fill = "Pitch Type"
        ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", color = nu_purp),
          plot.subtitle = element_text(hjust = 0.5, color = nu_purp),
          axis.title.x = element_text(size = 10, face="bold", color = "black"),
          axis.title.y = element_text(size = 10, face="bold", color = "black"),
          legend.position = "bottom"
        )
      
    }
  })
  
  # Catchers ----
  df_3 <- eventReactive(input$upload_3, {
    read.csv(input$upload_3$datapath)
  })
  
  data_3 <- eventReactive(input$goButton_3, {
    # Load Files ----
    
    load(file = "data/custom_aes.rda")
    
    load(file = here::here("data/app_setup_catcher.rda"))
    load(file = here::here("data/Catcher_Polys.rda"))
    
    #### Mutate Data ----
    
    mutate_data_pitcher(df_3())
    
    load(file = "data/nu_df.rda")
    
    nu_df <- nu_df %>% 
      mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
      mutate(
        Date = format(Date, "%b-%d")
      )
    
    #### Plots ----
    if (input$var_3 == "Called Strike Percentage") {
      
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
      
      swings <- nu_df
      
      Catcher_Points <- SpatialPoints(coords = cbind(swings$PlateLocSide, swings$PlateLocHeight))
      
      swings <- swings %>% 
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
      
      catchers <- swings %>% 
        filter(
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
      
      df_catcher <- catcher_framing %>% 
        filter(Catcher == input$catcher)
      
      df_catcher_polys = fortify(Catcher_Polys, region = "id")
      
      colnames(df_catcher_polys)[6] <- "catch_zone"
      colnames(df_catcher_polys)[1] <- "x"
      colnames(df_catcher_polys)[2] <- "z"
      
      df_pitch_polys <- left_join(x = df_catcher, y = df_catcher_polys, by = "catch_zone")
      
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
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          legend.position = "bottom"
        )
    }
  })
  
  output$hitterPlots <- renderPlot({
    data_1()
  }, height = 600
  )
  
  output$pitcherPlots <- renderPlot({
    data_2()
  }, height = 600
  )
  
  output$catcherPlots <- renderPlot({
    data_3()
  }, height = 600
  )
}

shinyApp(ui, server)



