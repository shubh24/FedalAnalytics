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

plot_rally_length = function(rally_outcomes){
  xaxis <- list(
    title = "Rally Length",
    titlefont = font
  )
  
  yaxis <- list(
    title = "Win Percentage",
    titlefont = font
  )
  
  p = plot_ly(rally_outcomes, x = ~rally_length, y = ~rn_winning, name = "Rafael Nadal", type = "scatter", mode = "lines") %>%
    add_trace(y = ~rf_winning,name="Roger Federer") %>%
    layout(yaxis = yaxis, xaxis = xaxis, title = "Win percentage against Rally Length")
  
  return (p)
  
}

#Rally Length
rally_outcomes = read.table("AusOpen/rally_outcomes.csv", stringsAsFactors = FALSE, sep = "\t")
colnames(rally_outcomes) = c("rally_length", "points", "rn_winning", "rn_winners", "rn_fe", "rn_ufe", "rf_winning", "rf_winners", "rf_fe", "rf_ufe")
rally_outcomes = rally_outcomes[2:nrow(rally_outcomes),]

rally_outcomes[, 3:ncol(rally_outcomes)] = brackets_to_vals(rally_outcomes[, 3:ncol(rally_outcomes)])
rally_outcomes = rally_outcomes[c(2,5,8,11),]
rally_outcomes$rally_length = as.vector(c("01-03 Shots","04-06 Shots","07-09 Shots","10+ Shots"))

plot_rally_length(rally_outcomes)

