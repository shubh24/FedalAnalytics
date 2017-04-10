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

fedal_fed_won = df[df$winner_name == "Roger Federer" & df$loser_name == "Rafael Nadal",]
fedal_rafa_won = df[df$winner_name == "Rafael Nadal" & df$loser_name == "Roger Federer",]

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
plotly_IMAGE(p, format = "png", out_file = "win_ratio_yoy")
