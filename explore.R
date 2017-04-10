library(stringr)
library(lubridate)
library(plotly)

df = read.csv("ATP.csv")

Sys.setenv("plotly_username"="shubh24")
Sys.setenv("plotly_api_key"="Jcgrh6kwxqOMZ3PerBKb")

font <- list(
  family = "Roboto",
  size = 18,
  color = "#7f7f7f"
)

df$date = as.Date(df$tourney_date, "%Y%m%d")
df$year = lubridate::year(df$date)

fedal = df[(df$winner_name == "Roger Federer" & df$loser_name == "Rafael Nadal") | (df$winner_name == "Rafael Nadal" & df$loser_name == "Roger Federer"),]

rafa = df[df$winner_name == "Rafael Nadal" | df$loser_name == "Rafael Nadal" ,]
rafa$win = as.numeric(rafa$winner_id == 104745)

roger = df[df$winner_name == "Roger Federer" | df$loser_name == "Roger Federer" ,]
roger$win = as.numeric(roger$winner_id == 103819)

yoy_win_ratio = function(athlete){
  athlete_yoy = aggregate(win ~ year, data = athlete, FUN = sum)
  athlete_yoy = merge(athlete_yoy, as.data.frame(table(athlete$year)), by.x = "year", by.y = "Var1")
  athlete_yoy$win_ratio = round(100*as.numeric(athlete_yoy$win/athlete_yoy$Freq), 2)
  
  return(athlete_yoy)
}
plot_yoy_win_ratio = function(athlete_win_ratio){
  xaxis <- list(
    title = "Year",
    titlefont = font
  )
  
  yaxis <- list(
    title = "Win Percentage",
    titlefont = font
  )
  
  p = plot_ly(athlete_win_ratio, x = ~year, y = ~win_ratio, type = "scatter", mode = "lines", color = ~athlete, colors = c("maroon", "green")) %>%
    layout(yaxis = yaxis, xaxis = xaxis, title = "Win percentage over the years")
  
  return (p)
  
}

roger_win_ratio = yoy_win_ratio(roger)
roger_win_ratio$athlete = "Roger Federer"
rafa_win_ratio = yoy_win_ratio(rafa)
rafa_win_ratio$athlete = "Rafael Nadal"

athlete_win_ratio = rbind(roger_win_ratio, rafa_win_ratio)
p = plot_yoy_win_ratio(athlete_win_ratio)
plotly_IMAGE(p, format = "png", out_file = "./viz/win_ratio_yoy")

plot_yoy_seed = function(athlete_seed){
  xaxis <- list(
    title = "Year",
    titlefont = font
  )
  
  yaxis <- list(
    title = "Seed",
    titlefont = font
  )
  
  p = plot_ly(athlete_seed, x = ~date, y = ~seed, type = "scatter", mode = "lines", color = ~athlete, colors = c("maroon", "green")) %>%
    layout(yaxis = yaxis, xaxis = xaxis, title = "Athletes' seeds over the years")
  
  return (p)
  
}

roger$seed = ifelse(roger$win == 1, roger$winner_seed, roger$loser_seed)
year_end_date = aggregate(date ~ year, data = roger, FUN = max)
roger_seed = unique(merge(year_end_date, roger[, c("date", "seed")], by = "date"))
roger_seed$athlete = "Roger Federer"

rafa$seed = ifelse(rafa$win == 1, rafa$winner_seed, rafa$loser_seed)
year_end_date = aggregate(date ~ year, data = rafa, FUN = max)
rafa_seed = unique(merge(year_end_date, rafa[, c("date", "seed")], by = "date"))
rafa_seed$athlete = "Rafael Nadal"

athlete_seed = rbind(roger_seed, rafa_seed)
p = plot_yoy_seed(athlete_seed)
plotly_IMAGE(p, format = "png", out_file = "./viz/seeds_yoy")

surface_sensitivity = function(athlete){
  
  athlete_surface_wins = aggregate(win ~ year + surface, data = athlete, FUN = sum)
  
  athlete$dummy = 1
  athlete_surface_total = aggregate(dummy ~ year + surface, data = athlete, FUN = sum)
  athlete_surface_yoy = merge(athlete_surface_wins, athlete_surface_total, by = c("year", "surface"))
  
  athlete_surface_yoy$sensitivity = round(100*as.numeric(athlete_surface_yoy$win/athlete_surface_yoy$dummy), 2)
  
  return(athlete_surface_yoy)
}
plot_surface_sensitivity = function(athlete_surface_sensitivity){
  xaxis <- list(
    title = "Year",
    titlefont = font
  )
  
  yaxis <- list(
    title = "% Wins on Surface",
    titlefont = font
  )
  
  p = plot_ly(athlete_surface_sensitivity, x = ~year, y = ~sensitivity, type = "scatter", mode = "lines", colors = c("brown", "green", "blue"), color = ~surface) %>%
    layout(yaxis = yaxis, xaxis = xaxis, title = "Win percentage over the years")
  
  return (p)
  
}

roger_surface_sensitivity = surface_sensitivity(roger)
roger_surface_sensitivity = roger_surface_sensitivity[!(as.character(roger_surface_sensitivity$surface) %in% c("", "Carpet")),]
roger_surface_sensitivity$surface = as.character(roger_surface_sensitivity$surface)
p = plot_surface_sensitivity(roger_surface_sensitivity)
plotly_IMAGE(p, format = "png", out_file = "./viz/roger_surface_sensitivity")

rafa_surface_sensitivity = surface_sensitivity(rafa)
rafa_surface_sensitivity = rafa_surface_sensitivity[!(as.character(rafa_surface_sensitivity$surface) %in% c("", "Carpet")),]
rafa_surface_sensitivity$surface = as.character(rafa_surface_sensitivity$surface)
p = plot_surface_sensitivity(rafa_surface_sensitivity)
plotly_IMAGE(p, format = "png", out_file = "./viz/rafa_surface_sensitivity")

tourney_fav = function(athlete){
  tourney_df = aggregate(win ~ tourney_name, data = athlete, FUN = sum)
  tourney_df = merge(tourney_df, as.data.frame(table(athlete$tourney_name)), by.x = "tourney_name", by.y = "Var1")
  tourney_df$win_ratio = round(100*as.numeric(tourney_df$win/tourney_df$Freq), 2)
  
  return(tourney_df[tourney_df$Freq > 10, c("tourney_name", "win_ratio")])
}
roger_tourney = tourney_fav(roger)
rafa_tourney = tourney_fav(rafa)

opponent_fav = function(athlete){
  
  athlete$opponent = ifelse(athlete$win == 1, as.character(athlete$loser_name), as.character(athlete$winner_name))
  
  athlete_df = aggregate(win ~ opponent, data = athlete, FUN = sum)
  athlete_df = merge(athlete_df, as.data.frame(table(athlete$opponent)), by.x = "opponent", by.y = "Var1")
  athlete_df$win_ratio = round(100*as.numeric(athlete_df$win/athlete_df$Freq), 2)
  
  return(athlete_df[athlete_df$Freq > 10, c("opponent", "win_ratio")])
}
roger_opponent = opponent_fav(roger)
rafa_opponent = opponent_fav(rafa)

#fedal
fedal$winner_name = droplevels(fedal$winner_name)
table(fedal$winner_name)

plot_fedal_surface = function(fedal_yoy_surface){
  xaxis <- list(
    title = "Year",
    titlefont = font
  )
  
  yaxis <- list(
    title = "Number of Fedals",
    titlefont = font
  )
  
  p = plot_ly(fedal_yoy_surface, x = ~year, y = ~clay, type = "bar", color = I("maroon"), name = "clay") %>%
    add_trace(y = ~grass, name = 'grass', color = I("light green") ) %>%
    add_trace(y = ~hard, name = 'hard', color = I("sky blue")) %>%
    layout(yaxis = yaxis, xaxis = xaxis, title = "Fedal clashes over the years",  barmode = 'stack')
  
  return (p)
  
}

fedal_yoy_surface = as.matrix(table(fedal$year, fedal$surface))
fedal_yoy_surface = data.frame(cbind(rownames(fedal_yoy_surface), fedal_yoy_surface[,3], fedal_yoy_surface[,4], fedal_yoy_surface[,5]))
colnames(fedal_yoy_surface) = c("year", "clay", "grass", "hard")
p = plot_fedal_surface(fedal_yoy_surface)
plotly_IMAGE(p, format = "png", out_file = "./viz/fedal_surface_yoy")

table(fedal$winner_name, fedal$surface)

#outlier wins
fedal[as.character(fedal$surface) == "Clay" & as.character(fedal$winner_name) == "Roger Federer", c("tourney_name", "year", "winner_name", "score")]
fedal[as.character(fedal$surface) == "Grass" & as.character(fedal$winner_name) == "Rafael Nadal", c("tourney_name", "year", "winner_name", "score")]

