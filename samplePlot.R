library(tidyverse)
library(ggplot2)
library(pracma)
library(quantmod)
library(tidyquant)
library(plotly)
library(ggthemes)

# Load Cached Objects
load("adpData.RData")

# Fliter to player in question
df <- filter(adpObject, player_id == "11675")

# Calculate IQR and remove outliers
q25 = quantile(df$adp, 0.25)
q75 = quantile(df$adp, 0.75)
IQR = q75 - q25

df <- filter(df, df$adp >= (q25 - (1.5*IQR))) %>%
  filter(df$adp <= (q75 + (1.5*IQR)))

# Group by date
df$Date <- as.Date(df$timestamp)
df <- df %>%
  group_by(Date) %>%
  summarise(avg = mean(adp), 
            min = min(adp), 
            max = max(adp), 
            med = median(adp))

# Add missing dates
date_range <- tibble(
  Date = seq.Date(from = min(df$Date, na.rm = TRUE), to = max(df$Date, na.rm = TRUE), by = "days")
)

df <- df %>% 
  dplyr::full_join(date_range, by = "Date") %>%
  arrange(Date)

# Replace missing values
for (i in 2:dim(df)[1]) {
  if (is.na(df$avg[i])) {
    df$avg[i] <- df$avg[i-1]
    df$min[i] <- df$min[i-1]
    df$max[i] <- df$max[i-1]
    df$med[i] <- df$med[i-1]
  }
}

# Moving Averages
df$ma10 <- movavg(df$avg, n=10, type = 'e')
df$ma20 <- movavg(df$avg, n=20, type = 'e')
df$ma50 <- movavg(df$avg, n=50, type = 'e')

# raw with Plotly
candles <- list(line = list(color = '#B6B6B4'))

fig <- df %>% plot_ly(x = df$Date, type="candlestick",
                      open = df$avg, close = df$med,
                      high = df$max, low = df$min, name = "Daily ADP",
                      increasing = candles, decreasing = candles) %>%
              add_lines(x = df$Date, y= df$ma10, color = I("orange"), name = "10 day EMA") %>%
              add_lines(x = df$Date, y= df$ma20, color = I("blue"), name = "20 day EMA") %>%
              add_lines(x = df$Date, y= df$ma50, color = I("black"), name = "50 day EMA")


# Add Cross over Events
for (i in 1:dim(df)[1]) {
  # Red
  if (df$ma20[i] > df$ma50[i] && df$ma20[i-1] <= df$ma50[i-1]){
    fig <- fig %>%
      add_lines(x=df$Date[i], y=df$avg, color=I("red"))
  }
  
  # green
  if (df$ma20[i] < df$ma50[i] && df$ma20[i-1] >= df$ma50[i-1]){
    fig <- fig %>%
      add_lines(x=df$Date[i], y=df$avg, color=I("green"))
  }
}

fig <- fig %>% layout(title = "Algorithmic Stock Trading with Player ADPs",
                      showlegend = FALSE,
                      xaxis = list(rangeslider = list(visible = F),
                                   title="Date"),
                      yaxis = list(autorange = "reversed",
                                  title="Daily ADP"))
fig
