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
library(pracma)
library(plotly)
library(ggthemes)
library(DT)

options(warn = - 1)                # Disable warning messages globally

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Algorithmic Stock Trading Player ADPs"),
   
   # Show a plot of the generated distribution
  plotlyOutput("distPlot"),
  
  hr(),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
     column(4,
            selectizeInput(
              inputId = "playerID",
              label = h4("Select Player:"),
              selected = 11675,
              multiple = FALSE,
              choices = NULL
            ),
            radioButtons("radioBB", h4("Bestball:"),
                         choices = list("Yes" = 1, "No" = 2,
                                        "Any" = 3),selected = 3)
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
                            min = "2021-05-01", max= Sys.Date(), start = "2021-05-01", end=Sys.Date()),
             radioButtons("draftType", h4("Draft Type:"),
                          choices = list("Rookie" = 1, "Redraft" = 2,
                                         "Dynasty/Keeper Startup" = 3,
                                         "Any" = 4),selected = 4)
      )
   ),
  
  hr(),
  
  tabsetPanel(
    id = 'dataset',
    tabPanel("Buys", DT::dataTableOutput("buys")),
    tabPanel("Sells", DT::dataTableOutput("sells"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Load Cached Objects
  adpObject <- readRDS("data/appData.rds")
  
  playerList <- adpObject %>%
    distinct(player_id, .keep_all = TRUE) %>%
    select(player_id, pos, player_name)
  
  # Updated Player List
  updateSelectizeInput(session, 'playerID', choices = playerList$player_name, server = FALSE)
  
  # Build Buys/Sells Data Table
  date_range <- tibble(
    timestamp = seq.Date(from = min(adpObject$timestamp, na.rm = TRUE), to = max(adpObject$timestamp, na.rm = TRUE), by = "days")
  )
  
  buys <- adpObject %>%
    group_by(timestamp, player_id) %>%
    summarise(adp = mean(adp)) %>%
    arrange(timestamp) %>%
    group_by(player_id)
  
  # Next I want to group by player_id keeping the arrays of adps intact
  # Then dplyr::full_join(date_range, by = "timestamp") where each group is joined independtly createding equal group sizes
  # Then I want to add 2 columns
  # ma50 <- movavg(adp, n=50, type = 'e') and ma20 <- movavg(df$avg, n=10, type = 'e')
  # Where the groups are handled independently
    
    
   output$distPlot <- renderPlotly({
     # Fliter to player in question
     req(input$playerID)
     index <- which(playerList$player_name %in% input$playerID)
     df <- adpObject %>% filter(player_id == playerList$player_id[index])
     
     # Filter Bestball
     if (input$radioBB == 1){
       df <- filter(df, best_ball == TRUE)
     } else if (input$radioBB == 2) {
       df <- filter(df, best_ball == FALSE)
     }
     
     # Filter 2QB
     if (input$radioQB == 1){
       df <- filter(df, qb_type == "1QB")
     } else if (input$radioQB == 2) {
       df <- filter(df, qb_type == "2QB/SF")
     }
     
     # Filter TEP
     if (input$radioTE == 1){
       df <- filter(df, grepl("TEPrem",scoring_flags) == TRUE)
     } else if (input$radioTE == 2) {
       df <- filter(df, grepl("TEPrem",scoring_flags) == FALSE)
     }
       
     # Calculate IQR and remove outliers
     q25 = quantile(df$adp, 0.25)
     q75 = quantile(df$adp, 0.75)
     IQR = q75 - q25
     
     df <- df %>%
       filter(df$adp >= (q25 - (1.5*IQR))) %>%
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
     
     candles <- list(line = list(color = '#7C97C4'))
     
     distPlot <- df %>% plot_ly(x = df$Date, type="candlestick",
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
         distPlot <- distPlot %>%
           add_lines(x=df$Date[i], y=df$avg, color=I("red"))
       }
       
       # green
       if (df$ma20[i] < df$ma50[i] && df$ma20[i-1] >= df$ma50[i-1]){
         distPlot <- distPlot %>%
           add_lines(x=df$Date[i], y=df$avg, color=I("green"))
       }
     }
     
     distPlot <- distPlot %>% layout(showlegend = FALSE,
                                     font = list(color = "#000", family = "Open Sans, verdana, arial, sans-serif"),
                                     paper_bgcolor = "#fff",
                                     plot_bgcolor = "#E8E8F2",
                                     title = list(text = paste("<b>", input$playerID, "</b>")),
                                     xaxis = list(rangeslider = list(visible = F),
                                                  title="<b>Date</b>",
                                                  range = c(input$dateRange[1],input$dateRange[2]),
                                                  gridcolor = "#fff"),
                                     yaxis = list(autorange = "reversed",
                                                  title="<b>Daily ADP</b>",
                                                  gridcolor = "#fff")
                                     )
     
     return(distPlot)
   })
}
   

# Run the application 
shinyApp(ui = ui, server = server)
