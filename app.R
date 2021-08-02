#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(pracma)
library(quantmod)
library(tidyquant)
library(plotly)
library(ggthemes)

# Load Cached Objects
load("adpData.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Algorithmic Stock Trading Player ADPs"),
   
   # Show a plot of the generated distribution
  plotOutput("distPlot"),
  
  hr(),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
     column(4,
            textInput(
              inputId = "playerID",
              label = h4("Select Player:"),
              placeholder = "11675", # Davante Adams
              value = 11675
            ),
            
            br(),
            
            checkboxGroupInput("checkGroup", 
              h4("Filters:"), 
              choices = list("Bestball" = 1, 
                            "IDP" = 2))
      ),
     
     column(3,
            radioButtons("radioQB", h4("QBs:"),
                         choices = list("1QB" = 1, "2QB/SF" = 2,
                                        "Any" = 3),selected = 3),
            
            radioButtons("radioTE", h4("TE Premium:"),
                         choices = list("Yes" = 1, "No" = 2,
                                        "Any" = 3),selected = 3)
      ),
     
      column(3,
             dateRangeInput("dateRange", h4("Date Range:"), 
                            min = "2021-04-15", max= Sys.Date(), start = "2021-04-15", end=Sys.Date())
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
   output$distPlot <- renderPlotly({
     
     # Fliter to player in question
     df <- filter(adpObject, player_id == get(input$playerID))
     
     # Calculate IQR and remove outliers
     q25 = quantile(df$adp, 0.25)
     q75 = quantile(df$adp, 0.75)
     IQR = q75 - q25
     
     df <- filter(df, df$adp >= (q25 - (1.5*IQR))) %>%
       filter(df$adp <= (q75 + (1.5*IQR)))
     View(df)
     
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
     
     candles <- list(line = list(color = '#B6B6B4'))
     
     output$distPlot <- df %>% plot_ly(x = df$Date, type="candlestick",
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
         output$distPlot <- output$distPlot %>%
           add_lines(x=df$Date[i], y=df$avg, color=I("red"))
       }
       
       # green
       if (df$ma20[i] < df$ma50[i] && df$ma20[i-1] >= df$ma50[i-1]){
         output$distPlot <- output$distPlot %>%
           add_lines(x=df$Date[i], y=df$avg, color=I("green"))
       }
     }
     
     output$distPlot <- output$distPlot %>% layout(title = "Algorithmic Stock Trading with Player ADPs",
                           showlegend = FALSE,
                           xaxis = list(rangeslider = list(visible = F),
                                        title="Date"),
                           yaxis = list(autorange = "reversed",
                                        title="Daily ADP"))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

