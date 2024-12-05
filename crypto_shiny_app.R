# SDS 313 Shiny App Example - Crypto News Sentiment Analysis

library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)

crypto_data <- read.csv("cryptonews.csv")
cleaned_sentiment <- str_replace_all(crypto_data$sentiment, "[{}]", "")
split_sentiment <- str_split(cleaned_sentiment, ",")
crypto_data$sentiment_class <- str_replace(sapply(split_sentiment, "[", 1), ".*:", "")
crypto_data$subjectivity <- as.numeric(str_replace(sapply(split_sentiment, "[", 3), ".*:", ""))
crypto_data$date <- parse_date_time(crypto_data$date, orders = c("ymd HMS", "ymd HM"))

ui <- fluidPage(
  titlePanel("Crypto News Sentiment Analysis"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("dateSlider", "Select Date Range:",
                  min = as.Date(min(crypto_data$date, na.rm = TRUE)),
                  max = as.Date("2023-12-19"),
                  value = c(as.Date(min(crypto_data$date, na.rm = TRUE)),
                            as.Date("2023-12-19")),
                  timeFormat = "%Y-%m-%d"),
      
      selectInput("analysisType", label = h3("Choose Analysis Type"), 
                  choices = c("Univariate", "Bivariate"), 
                  selected = "Univariate"),
      
      selectInput("selectvar", label = h3("Choose Variable(s)"), 
                  choices = c("source", "subject", "sentiment_class", "subjectivity"),
                  selected = "source"),
      
      selectInput("selectvar2", label = h3("Choose Second Variable (for Bivariate Analysis)"), 
                  choices = c("source", "subject", "sentiment_class", "subjectivity"),
                  selected = "subjectivity"),
      
      selectInput("graphColor", label = h3("Choose Graph Color (for Univariate Analysis)"), 
                  choices = c("blue", "red", "green"),
                  selected = "blue")
    ),
    mainPanel(
      tags$img(
        src = "https://storage.googleapis.com/kaggle-datasets-images/1712408/3110692/cc96d0943632aa0e1bccebb7193ab374/dataset-cover.png?t=2022-01-29-14-22-28",
        alt = "Crypto News Dataset Cover",
        height = "300px"
      ),
      tags$p(
        "Oliviervha. (2023, December 19). ",
        tags$em("Crypto news +."), 
        " Kaggle. ",
        tags$a(href = "https://www.kaggle.com/datasets/oliviervha/crypto-news", "https://www.kaggle.com/datasets/oliviervha/crypto-news")
      ),
      hr(),
      plotOutput("graphOutput"),
      hr(),
      h4("Descriptive Statistics"),
      verbatimTextOutput("statsOutput")
    )
  )
)

server <- function(input, output) {
  
  output$graphOutput <- renderPlot({
    data <- crypto_data %>%
      filter(date >= as.POSIXct(input$dateSlider[1]) & date <= as.POSIXct(input$dateSlider[2]))
    
    if (input$analysisType == "Bivariate" && input$selectvar == input$selectvar2) {
      return(NULL)
    }
    
    var1 <- input$selectvar
    var2 <- input$selectvar2
    if (input$analysisType == "Bivariate" && var1 == "subjectivity") {
      var1 <- input$selectvar2
      var2 <- input$selectvar
    }
    
    if (input$analysisType == "Univariate") {
      if (var1 %in% c("source", "subject", "sentiment_class")) {
        ggplot(data, aes_string(x = var1)) +
          geom_bar(fill = input$graphColor) +
          labs(title = paste("Distribution of", var1), x = var1, y = "Count")
      } else if (var1 == "subjectivity") {
        ggplot(data, aes_string(x = var1)) +
          geom_histogram(bins = 30, fill = input$graphColor) +
          labs(title = paste("Distribution of", var1), x = var1, y = "Frequency")
      }
    } else if (input$analysisType == "Bivariate") {
      if (var1 %in% c("source", "subject", "sentiment_class") &&
          var2 %in% c("source", "subject", "sentiment_class")) {
        ggplot(data, aes_string(x = var1, fill = var2)) +
          geom_bar(position = "dodge") +
          labs(title = paste(var2, "by", var1), x = var1, y = "Count", fill = var2)
      } else if (var1 %in% c("source", "subject", "sentiment_class") &&
                 var2 == "subjectivity") {
        ggplot(data, aes_string(x = var1, y = var2, fill = var1)) +
          geom_boxplot() +
          labs(title = paste(var2, "by", var1), x = var1, y = var2)
      }
    }
  })
  
  output$statsOutput <- renderPrint({
    data <- crypto_data %>%
      filter(date >= as.POSIXct(input$dateSlider[1]) & date <= as.POSIXct(input$dateSlider[2]))
    
    if (input$analysisType == "Bivariate" && input$selectvar == input$selectvar2) {
      return("Please choose two different variables for bivariate analysis.")
    }
    
    var1 <- input$selectvar
    var2 <- input$selectvar2
    if (input$analysisType == "Bivariate" && var1 == "subjectivity") {
      var1 <- input$selectvar2
      var2 <- input$selectvar
    }
    
    if (input$analysisType == "Univariate") {
      if (var1 %in% c("source", "subject", "sentiment_class")) {
        counts <- as.data.frame(table(data[[var1]]))
        colnames(counts) <- c(var1, "Count")
        proportions <- as.data.frame(prop.table(table(data[[var1]])))
        colnames(proportions) <- c(var1, "Proportion")
        print(counts)
        print(proportions)
      } else if (var1 == "subjectivity") {
        mean_val <- mean(data[[var1]], na.rm = TRUE)
        sd_val <- sd(data[[var1]], na.rm = TRUE)
        print(mean_val)
        print(sd_val)
      }
    } else if (input$analysisType == "Bivariate") {
      if (var1 %in% c("source", "subject", "sentiment_class") &&
          var2 == "subjectivity") {
        means <- aggregate(data[[var2]] ~ data[[var1]], FUN = mean)
        sds <- aggregate(data[[var2]] ~ data[[var1]], FUN = sd)
        colnames(means) <- c(var1, "Mean")
        colnames(sds) <- c(var1, "Standard Deviation")
        print(means)
        print(sds)
      } else if (var1 %in% c("source", "subject", "sentiment_class") &&
                 var2 %in% c("source", "subject", "sentiment_class")) {
        counts <- as.data.frame(table(data[[var1]], data[[var2]]))
        colnames(counts) <- c(var1, var2, "Count")
        proportions <- as.data.frame(prop.table(table(data[[var1]], data[[var2]])))
        colnames(proportions) <- c(var1, var2, "Proportion")
        print(counts)
        print(proportions)
      }
    }
  })
}

shinyApp(ui = ui, server = server)