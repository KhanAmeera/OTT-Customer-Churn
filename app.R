# Load necessary libraries
library(shiny)
library(flexdashboard)
library(plotly)
library(MASS)

# Load the dataset
churn <- read.csv("E:/other downloads/OTT Customer Churn Dataset.csv")

#Data preprocessing to be consistent with the pre-trained model

# Map categorical values to 0 and 1
churn$multi_screen <- as.factor(mapvalues(churn$multi_screen,
                                          from = c("no", "yes"),
                                          to = c("0", "1")))
churn$mail_subscribed <- as.factor(mapvalues(churn$mail_subscribed,
                                             from = c("no", "yes"),
                                             to = c("0", "1")))
churn$gender <- as.factor(mapvalues(churn$gender,
                                    from = c("Male", "Female"),
                                    to = c("0", "1")))

# Convert categorical variables to numeric
churn$multi_screen <- as.numeric(churn$multi_screen, na.rm = TRUE)
churn$mail_subscribed <- as.numeric(churn$mail_subscribed, na.rm = TRUE)
churn$gender <- as.numeric(churn$gender, na.rm = TRUE)

# Perform exploratory data analysis

# Calculate correlation between numerical values
numeric.var <- sapply(churn, is.numeric)
corr.matrix <- cor(churn[, numeric.var])

# Visualize correlation plot
corrplot(corr.matrix, main = "\n\nCorrelation Plot for Numerical Variables",
         method = "circle", addCoef.col = "red", tl.cex = 0.7, tl.col = "black")

# Remove highly correlated variables (dimensionality reduction)
churn$minimum_daily_mins <- NULL
churn$maximum_daily_mins <- NULL

# (Replace this with your actual preprocessing steps)

# Load the logistic regression model
LogisticRegressionModel <- readRDS("C:/Users/Admin/OneDrive/Documents/LogisticRegressionModel.rds")

# Define UI
ui <- fluidPage(
  titlePanel("Customer Churn Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("customer_id", "Select Customer ID", choices = unique(churn$customer_id)),
      actionButton("predict_button", "Predict")
    ),
    
    mainPanel(
      plotlyOutput("gauge_chart", width = "100%", height = "80vh"),
      div(id = "churn_risk_value", style = "text-align: center; font-size: 24px; margin-top: 20px;")
      # Other outputs or plots if needed
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$predict_button, {
    # Get customer data based on selected customer ID
    customer_data <- churn[churn$customer_id == input$customer_id, ]
    if (nrow(customer_data) > 0) {
      # Predict churn risk using the loaded model
      prediction <- predict(LogisticRegressionModel, newdata = customer_data, type = "response")
      churn_risk <- round(prediction * 100, 2)
      
      output$churn_risk_value <- renderText({
        paste("Churn Risk Prediction for ID", input$customer_id, ":", churn_risk, "%")
      })
      
      # Render gauge chart for churn risk prediction
      output$gauge_chart <- renderPlotly({
        plot_ly(
          type = "indicator",
          mode = "gauge+number",
          value = churn_risk,
          title = "Churn Risk Prediction",
          gauge = list(
            axis = list(range = list(NULL, 100)),
            steps = list(
              list(range = c(0, 50), color = "lightgreen"),
              list(range = c(50, 70), color = "yellow"),
              list(range = c(70, 100), color = "red")
            ),
            threshold = list(
              line = list(color = "black", width = 4),
              thickness = 0.75,
              value = 85
            )
          )
        ) %>% layout(
          margin = list(l = 0, r = 0, t = 0, b = 0),
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)'
        )
      })
    } else {
      # Clear details and reset elements if no valid customer ID is selected
      output$churn_risk_value <- renderText({ "" })
      output$gauge_chart <- renderPlotly({ plot_ly() })
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)






