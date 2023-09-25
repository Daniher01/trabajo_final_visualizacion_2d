# Instalacion de paquetes
library(devtools)
library(StatsBombR)
library(ggplot2)
library(ggforce)

# --------------------------------- CREACION DE LA CANCHA --------------------------------------------------------------

# datos de la medidas
dims <- list(
  length = 110,
  width = 73,
  penalty_box_length = 16.5,
  penalty_box_width = 40.32,
  six_yard_box_length = 5.5,
  six_yard_box_width = 18.21,
  penalty_spot_distance = 11,
  central_circle_radius = 9.15, 
  goal_width = 7.32,
  origin_x = 0,
  origin_y = 0
)

# funcion para dibujar la cancha
get_pitch <- function(gp, dims, pitch_fill = "white", pitch_col = "grey70", background_fill = pitch_fill, margin = 0){
  
  contorno_df <- data.frame(x = dims$origin_x, 
                            xend = dims$length,
                            y = dims$origin_y,
                            yend = dims$width)
  
  x_start_areas <- c(dims$origin_x, dims$origin_x, dims$length - dims$penalty_box_length, dims$length - dims$six_yard_box_length)
  
  x_end_areas <- c(dims$penalty_box_length, dims$six_yard_box_length, dims$length, dims$length)
  
  y_start_areas <- c((dims$width - dims$penalty_box_width)/2, (dims$width - dims$six_yard_box_width)/2, (dims$width - dims$penalty_box_width)/2, (dims$width - dims$six_yard_box_width)/2)
  
  y_end_areas <- c(dims$width - (dims$width - dims$penalty_box_width)/2, dims$width - (dims$width - dims$six_yard_box_width)/2, dims$width - (dims$width - dims$penalty_box_width)/2, dims$width - (dims$width - dims$six_yard_box_width)/2)
  
  areas_df<- data.frame(x = x_start_areas, 
                        xend = x_end_areas, 
                        y = y_start_areas,
                        yend = y_end_areas)
  
  gp +
    theme_void() +
    theme(panel.background = element_rect(fill = background_fill, colour = "transparent"),
          plot.margin = unit(c(margin, margin, margin, margin), "cm")) +
    
    # rectángulos
    #áreas
    geom_rect(data = contorno_df,
              aes(xmin = x, xmax = xend, ymin = y, ymax = yend), col = pitch_col, fill = pitch_fill) +
    geom_rect(data = areas_df,
              aes(xmin = x, xmax = xend, ymin = y, ymax = yend), col = pitch_col, fill = pitch_fill) +
    #porterías
    geom_rect(aes(xmin = dims$length, xmax = dims$length + 1.5, ymin = dims$width/2 - dims$goal_width/2, ymax = dims$width/2 + dims$goal_width/2), 
              fill = pitch_col, col = pitch_col) +
    geom_rect(aes(xmin = dims$origin_x, xmax = dims$origin_x - 1.5, ymin = dims$width/2 - dims$goal_width/2, ymax = dims$width/2 + dims$goal_width/2), 
              fill = pitch_col, col = pitch_col) +
    
    # puntos
    geom_point(aes(x = dims$length/2, y = dims$width/2), col = pitch_col) +
    geom_point(aes(x = dims$length - dims$penalty_spot_distance, y = dims$width/2), col = pitch_col) +
    geom_point(aes(x = dims$penalty_spot_distance, y = dims$width/2), col = pitch_col) +
    
    #círculo central
    geom_circle(aes(x0 = dims$length/2, y0 = dims$width/2, r = dims$central_circle_radius), color = pitch_col) +
    #línea central
    geom_segment(aes(x = dims$length/2, xend = dims$length/2, y = dims$width, yend = dims$origin_y), color = pitch_col) +
    
    # semi círculos áreas
    geom_arc(aes(x0 = dims$length - dims$penalty_spot_distance, y0 = dims$width/2, r = dims$central_circle_radius, 
                 start = -37*pi/180, end = -143*pi/180), col = pitch_col) +
    geom_arc(aes(x0 = dims$penalty_spot_distance, y0 = dims$width/2, r = dims$central_circle_radius, 
                 start = 37*pi/180, end = 143*pi/180), col = pitch_col) +
    
    # semi círculos corners
    geom_arc(aes(x0 = dims$length, y0 = dims$origin_y, r = 1, 
                 start = 270*pi/180, end = 360*pi/180), col = pitch_col) +
    geom_arc(aes(x0 = dims$length, y0 = dims$width, r = 1, 
                 start = 180*pi/180, end = 270*pi/180), col = pitch_col) +
    geom_arc(aes(x0 = dims$origin_x, y0 = dims$origin_y, r = 1, 
                 start = 0*pi/180, end = 90*pi/180), col = pitch_col) +
    geom_arc(aes(x0 = dims$origin_x, y0 = dims$width, r = 1, 
                 start = 90*pi/180, end = 180*pi/180), col = pitch_col)
}


# ------------------------------- OBTENCION Y TRANSFORMACION DE LOS DATOS ------------------------------------------------------------
max_x_source <- 120
max_y_source <- 80
max_x_final <- 110
max_y_final <- 73

# obtencion de los datos
competiciones <- FreeCompetitions()
qatar_2022 <- competiciones %>% filter(competition_id == 43 & season_name == '2022')

qatar_2022_juegos <- FreeMatches(qatar_2022)

qatar_2022_eventos_sin_limpiar <- free_allevents(MatchesDF = qatar_2022_juegos)
qatar_2022_eventos_limpios <- allclean(qatar_2022_eventos_sin_limpiar)
qatar_2022_eventos <- qatar_2022_eventos_limpios %>%
                            mutate(pos_x_meter = location.x/max_x_source*max_x_final,
                                   pos_y_meter = location.y/max_y_source*max_y_final,
                                   pos_y_meter = 73 - pos_y_meter,
                                   pass_end_pos_x_meter = pass.end_location.x/max_x_source*max_x_final,
                                   pass_end_pos_y_meter = pass.end_location.y/max_y_source*max_y_final,
                                   pass_end_pos_y_meter = 73 - pass_end_pos_y_meter)


pases_messi <- qatar_2022_eventos %>% filter(player.name == 'Lionel Andrés Messi Cuccittini' & type.name == 'Pass')
pases_messi_regular <- pases_messi %>%
                                  mutate(pass_forward = ifelse(pass_end_pos_x_meter > pos_x_meter, "Yes", "No")) %>% 
                                  mutate(pass_correct = ifelse(is.na(pass.outcome.name), "Completado", "Incompleto")) %>%
                                  filter(play_pattern.name == 'Regular Play') %>%
                                  left_join(qatar_2022_juegos, by = "match_id") %>%
                                  mutate(to_facet = paste0(match_date, ", vs ", 
                                                           ifelse(home_team.home_team_name == "Argentina", 
                                                                  away_team.away_team_name, home_team.home_team_name), 
                                                           " (", competition_stage.name, ")"))

# -------------------------------- VISUALIZACION DE LOS DATOS EN LA CANCHA ------------------------------------
g <- get_pitch(gp = ggplot(data = pases_messi_regular), dims = dims)
g1 <- g + geom_segment(aes(x = pos_x_meter, y = pos_y_meter, 
                      xend = pass_end_pos_x_meter, yend = pass_end_pos_y_meter, 
                  linetype = pass_forward, col = pass_correct),
                  size = 0.5,
                  arrow = arrow(length = unit(0.2, "cm"))) +
  scale_color_manual(values = c("blue", "red")) +
  scale_linetype_manual(values = c(2,1))

g2 <- g1 + facet_wrap(~to_facet, nrow = 3)

g2
