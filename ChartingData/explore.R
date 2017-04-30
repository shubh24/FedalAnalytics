library(plotly)

Sys.setenv("plotly_username"="shubh24")
Sys.setenv("plotly_api_key"="Jcgrh6kwxqOMZ3PerBKb")

font <- list(
  family = "Roboto",
  size = 18,
  color = "#7f7f7f"
)

brackets_to_vals = function(df){
  
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      df[i, j] = gsub(".*\\((.*)\\).*", "\\1", df[i,j])
      df[i, j] = substr(df[i, j], 1, nchar(df[i, j])-1)
    }
  }
  
  return (df)
}

remove_percentage = function(df){
  
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      df[i, j] = substr(df[i, j], 1, nchar(df[i, j])-1)
    }
  }
  
  return (df)
}

non_brackets_to_vals = function(df){
  
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      df[i, j] = gsub("(.*)\\((.*)\\).*", "\\1", df[i,j])
    }
  }
  
  return (df)
}

plot_rally_length = function(rally_outcomes){
  xaxis <- list(
    title = "Rally Length",
    titlefont = font
  )
  
  yaxis <- list(
    title = "Win Percentage",
    titlefont = font
  )
  
  p = plot_ly(rally_outcomes, x = ~rally_length, y = ~rn_fe, name = "Rafael Nadal Forced Errors", type = "scatter", mode = "lines") %>%
        add_trace(y = ~rf_fe,name="Roger Federer Forced Errors") %>%
    layout(yaxis = yaxis, xaxis = xaxis, title = "Win percentage against Rally Length")
  
  return (p)
  
}

#Rally Length
rally_outcomes = read.table("AusOpen/rally_outcomes.csv", stringsAsFactors = FALSE, sep = "\t")
colnames(rally_outcomes) = c("rally_length", "points", "rn_winning", "rn_winners", "rn_fe", "rn_ufe", "rf_winning", "rf_winners", "rf_fe", "rf_ufe")
rally_outcomes = rally_outcomes[2:nrow(rally_outcomes),]

rally_outcomes[, 3:ncol(rally_outcomes)] = brackets_to_vals(rally_outcomes[, 3:ncol(rally_outcomes)])
for (i in 2:ncol(rally_outcomes)){
  rally_outcomes[, i] = as.numeric(rally_outcomes[, i])
}
rally_outcomes = rally_outcomes[c(2,5,8,11),]
rally_outcomes$rally_length = as.vector(c("01-03 Shots","04-06 Shots","07-09 Shots","10+ Shots"))

plot_rally_length(rally_outcomes)

#Serve Analysis -- Both Players
serve_overview = read.table("AusOpen/serve_overview.csv", stringsAsFactors = FALSE, sep = "\t")
colnames(serve_overview) = c("serve_basics", "pts", "won", "aces", "unret", "fe", "<=3w", "wide", "body", "t")
serve_overview = serve_overview[2:nrow(serve_overview),]
serve_overview[, 3:ncol(serve_overview)] = brackets_to_vals(serve_overview[, 3:ncol(serve_overview)])
serve_matrix = data.frame("Player" = c("Rafael Nadal", "Roger Federer"), "FirstServe.WinPercentage" = c(serve_overview[2,3], serve_overview[5,3]), "FirstServe.WinPercentage" = c(serve_overview[3,3], serve_overview[6,3]))

plot_serve_direction = function(serve_overview_direction){
  xaxis <- list(
    title = "Serve Direction (in the natural order)",
    titlefont = font
  )
  
  yaxis <- list(
    title = "Percentage of Serves",
    titlefont = font
  )
  
  serve_overview_direction$player = as.vector(c("Rafael Nadal", "Roger Federer")) 
  serve_overview_direction = melt(serve_overview_direction, id.vars = "player")
  
  p = plot_ly(serve_overview_direction, x = ~variable, y = ~value, type = "scatter", mode = "lines+markers", color = ~player) %>%
    layout(yaxis = yaxis, xaxis = xaxis, title = "Where did they serve? -- Advantage Court")

  return (p)
  
}

serve_overview_direction = read.table("AusOpen/serve_overview_direction.csv", stringsAsFactors = FALSE, sep = "\t")
colnames(serve_overview_direction) = c("player", "deuce_wide", "deuce_body", "deuce_t", "ad_wide", "ad_body", "ad_t", "net", "wide", "deep", "w&d", "foot", "unk")
serve_overview_direction = serve_overview_direction[2:nrow(serve_overview_direction),]
serve_overview_direction[, 2:ncol(serve_overview_direction)] = brackets_to_vals(serve_overview_direction[, 2:ncol(serve_overview_direction)])
for (i in 2:7){
  serve_overview_direction[, i] = as.numeric(serve_overview_direction[, i])
}
plot_serve_direction(serve_overview_direction[c(1, 4), c("player", "deuce_wide", "deuce_body", "deuce_t")])
plot_serve_direction(serve_overview_direction[c(1, 4), c("player", "ad_t", "ad_body", "ad_wide")])

plot_serve_influence = function(serve_influence){
  xaxis <- list(
    title = "Shots in the rally",
    titlefont = font
  )
  
  yaxis <- list(
    title = "Percentage Points Won by Server",
    titlefont = font
  )
  
  serve_influence$pts = NULL
  serve_influence$player = as.vector(c("Rafael Nadal", "Roger Federer")) 
  serve_influence = melt(serve_influence, id.vars = "player")
  
  p = plot_ly(serve_influence, x = ~variable, y = ~value, type = "scatter", mode = "lines+markers", color = ~player) %>%
    layout(yaxis = yaxis, xaxis = xaxis, title = "How long a rally can they handle? -- Second Serve")
  
  return (p)
  
}

serve_influence = read.table("AusOpen/serve_influence.csv", stringsAsFactors = FALSE, sep = "\t")
colnames(serve_influence) = c("player", "pts", "1 point", "2 point", "3 point", "4 point", "5 point", "6 point", "7 point", "8 point", "9 point", "10 point")
serve_influence = serve_influence[2:nrow(serve_influence),]
serve_influence[, 2:ncol(serve_influence)] = remove_percentage(serve_influence[, 2:ncol(serve_influence)])
for (i in 2:ncol(serve_influence)){
  serve_overview_direction[, i] = as.numeric(serve_overview_direction[, i])
}
plot_serve_influence(serve_influence[c(1, 3), ])
plot_serve_influence(serve_influence[c(2, 4), ])

#Serve Breakdown -- %3W for both players
serve_rf = read.table("AusOpen/rogerfederer/serve_breakdown_2.csv", stringsAsFactors = F, sep = "\t")
serve_rf = serve_rf[2:nrow(serve_rf),]
serve_rf = serve_rf[, c(1, 7)]

serve_rn = read.table("AusOpen/rafaelnadal/serve_breakdown_2.csv", stringsAsFactors = F, sep = "\t")
serve_rn = serve_rn[2:nrow(serve_rn),]
serve_rn = serve_rn[, c(1, 7)]

plot_serve_3w = function(serve_players){
  xaxis <- list(
    title = "Direction of First Serve",
    titlefont = font
  )
  
  yaxis <- list(
    title = "Percentage Points Won(by Server) within 3 rallies",
    titlefont = font
  )
  
  p = plot_ly(serve_players, x = ~Direction, y = ~Federer, type = "scatter", mode = "lines+markers", name = "Roger Federer") %>%
    add_trace(y = ~Nadal,name="Rafael Nadal") %>%
    layout(yaxis = yaxis, xaxis = xaxis, title = "How strong is your first serve?")
  
  return (p)
  
}

serve_players = merge(serve_rf, serve_rn, by = "V1")
serve_players[, c(2,3)] = brackets_to_vals(as.data.frame(serve_players[, c(2,3)]))
colnames(serve_players) = c("Direction", "Federer", "Nadal")
for (i in 2:ncol(serve_players)){
  serve_players[, i] = as.numeric(serve_players[, i])
}
serve_players[is.na(serve_players)] = 0
serve_players = serve_players[c(1, 3, 4, 6, 8, 9),]
serve_players$Direction = as.vector(c("5.Ad-Body", "4.Ad-T", "6.Ad-Wide", "2.Deuce-Body", "3.Deuce-T", "1.Deuce-Wide"))
plot_serve_3w(serve_players[c(6,4,5,2,1,3),])

#Return Analysis
return_rf = read.table("AusOpen/rogerfederer/return_outcomes.csv", stringsAsFactors = F, sep = "\t")
return_rf = return_rf[2:nrow(return_rf),]
return_rf$player = "Roger Federer"

return_rn = read.table("AusOpen/rafaelnadal/return_outcomes.csv", stringsAsFactors = F, sep = "\t")
return_rn = return_rn[2:nrow(return_rn),]
return_rn$player = "Rafael Nadal"

return_avg_rally = as.data.frame(rbind(cbind(return_rf[2, 9], return_rf[3, 9]), cbind(return_rn[2, 9], return_rn[3, 9])))
return_avg_rally = cbind(return_avg_rally, c("Rafael Nadal", "Roger Federer"))
colnames(return_avg_rally) = c("1stServe Rally Length", "2ndServe Rally Length", "Server")
View(return_avg_rally[c(2, 1), c("Server", "1stServe Rally Length", "2ndServe Rally Length")])

return_shot = merge(return_rf[c(7,8,9,10), c(1,3)], return_rn[c(7,8,9,10), c(1,3)], by = "V1")
colnames(return_shot) = c("Return Shot", "Federer", "Nadal")
return_shot[, c(2,3)] = brackets_to_vals(as.data.frame(return_shot[, c(2,3)]))
for (i in 2:ncol(return_shot)){
  return_shot[, i] = as.numeric(return_shot[, i])
}
return_shot$`Return Shot` = as.vector(c("2.Backhand", "3.Flat/Topspin", "1.Forehand", "4.Slice/Chip"))
plot_return_win_percentage = function(return_shot){
  xaxis <- list(
    title = "Return Shot Type",
    titlefont = font
  )
  
  yaxis <- list(
    title = "Percentage Points Won by Receiver",
    titlefont = font
  )
  
  p = plot_ly(return_shot, x = ~`Return Shot`, y = ~Federer, type = "bar", name = "Roger Federer") %>%
    add_trace(y = ~Nadal,name="Rafael Nadal") %>%
    layout(yaxis = yaxis, xaxis = xaxis, title = "How well do they return?")
  
  return (p)
  
}
plot_return_win_percentage(return_shot)

#Return Position 
return_position = merge(return_rf[c(13,14,15), c(1,3)], return_rn[c(13,14,15), c(1,3)], by = "V1")
colnames(return_position) = c("Return Position", "Federer", "Nadal")
return_position[, c(2,3)] = brackets_to_vals(as.data.frame(return_position[, c(2,3)]))
for (i in 2:ncol(return_position)){
  return_position[, i] = as.numeric(return_position[, i])
}
return_position$`Return position` = as.vector(c("2.Body Serves", "3.T Serves", "1.Wide Serves"))
plot_return_win_percentage = function(return_position){
  xaxis <- list(
    title = "Return position Type",
    titlefont = font
  )
  
  yaxis <- list(
    title = "Percentage Points Won by Receiver",
    titlefont = font
  )
  
  p = plot_ly(return_position, x = ~`Return position`, y = ~Federer, type = "bar", name = "Roger Federer") %>%
    add_trace(y = ~Nadal,name="Rafael Nadal") %>%
    layout(yaxis = yaxis, xaxis = xaxis, title = "From where is the return good?", annotations = list(x = ~`Return position`, y = ~Nadal, text = as.vector(c("24/47   3/6", "17/45   19/64", "15/56   22/65")),  xanchor = 'center', yanchor = 'bottom', showarrow = FALSE))

  return (p)
  
}
plot_return_win_percentage(return_position)

#Depth
return_rf = read.table("AusOpen/rogerfederer/return_depth.csv", stringsAsFactors = F, sep = "\t")
return_rf = return_rf[2:nrow(return_rf),]
return_rf = return_rf[c(4,5,6,7), c(1,3,4)]
return_rf$player = "Federer"

return_rn = read.table("AusOpen/rafaelnadal/return_depth.csv", stringsAsFactors = F, sep = "\t")
return_rn = return_rn[2:nrow(return_rn),]
return_rn = return_rn[c(4,5,6,7), c(1,3,4)]
return_rn$player = "Nadal"

return_player = rbind(return_rf, return_rn)
return_player[, c(2:3)] = non_brackets_to_vals(return_player[,c(2:3)])
colnames(return_player) = c("Return Shot", "Short", "Deep",)
return_player$`Return Shot` = as.vector(c("2.Backhand", "3.Flat/Topspin", "1.Forehand", "4.Slice/Chip"))
