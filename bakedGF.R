library(shiny)
library(readr)
library(ggplot2)
library(dplyr)

# Load Data
ingredients <- read.csv("ingredients.csv")

# Calculate total batch cost

# Identify all columns related to total ingredient costs and compute batch cost
cost_columns <- grep("total", colnames(ingredients), value = TRUE)

# Replace NA values with 0 in total cost columns
ingredients[cost_columns] <- lapply(ingredients[cost_columns], function(x) ifelse(is.na(x), 0, x))

# Compute batch cost
ingredients <- ingredients %>%
  rowwise() %>%
  mutate(total_batch_cost = sum(c_across(all_of(cost_columns)), na.rm = TRUE)) %>%
  ungroup()
ingredients$total_batch_cost <- round(ingredients$total_batch_cost, digits = 2)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  titlePanel(div("bakedGF Sales Dashboard", class = "title-panel")),
  
  sidebarLayout(
    sidebarPanel(
      h3("Baked Goods"),
      dateInput("sale_date", "Sale Date:", value = Sys.Date()),
      selectInput("treat", "Select a Treat:", choices = ingredients$treat),
      numericInput("bake_cost", "Baking Cost ($):", value = 0, min = 0, step = 0.01),
      numericInput("treats_per_batch", "Number of Treats per Batch:", value = 0, min = 0),
      numericInput("treats_sold", "Number of Treats Sold:", value = 0, min = 0),
      numericInput("price_per_treat", "Selling Price per Treat ($):", value = 0, min = 0, step = 0.01),
      actionButton("add_sale", "Add Sale"),
      downloadButton("download_data", "Export Data")
    ),
    
    mainPanel(
      tags$img(src = "baked.png"),
      h3("Daily Profit Summary"),
      tableOutput("profit_summary"),
      h3("Total Daily Profit"),
      verbatimTextOutput("total_profit"),
      h3("Total Sales"),
      verbatimTextOutput("sales_summary"),
      h3("Profit by Treat"),
      plotOutput("profit_plot")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive values to store profit data
  profit_data <- reactiveValues(data = data.frame(
    Date = as.Date(character()),
    Treat = character(),
    Baking_Cost = numeric(),
    Treats_Per_Batch = numeric(),
    Treats_Sold = numeric(),
    Price_Per_Treat = numeric(),
    Baking_Cost_Per_Treat = numeric(),
    Profit = numeric(),
    Sales = numeric(),
    stringsAsFactors = FALSE
  ))
  
  # Update batch cost when a treat is selected
  observeEvent(input$treat, {
    selected_treat <- input$treat
    baking_cost <- ingredients$total_batch_cost[ingredients$treat == selected_treat]
    baking_cost <- ifelse(is.na(baking_cost), 0, baking_cost)
    updateNumericInput(session, "bake_cost", value = baking_cost)
  })
  
  observeEvent(input$add_sale, {
    # Check for valid inputs
    if (input$treat == "" || 
        input$treats_per_batch <= 0 || 
        input$treats_sold < 0 || 
        input$price_per_treat < 0 || 
        input$bake_cost < 0) {
      showNotification("Please ensure all input fields are valid.", type = "error")
      return()
    }
    
    print(paste("Treat:", input$treat))
    print(paste("Baking Cost:", input$bake_cost))
    print(paste("Treats per Batch:", input$treats_per_batch))
    print(paste("Treats Sold:", input$treats_sold))
    print(paste("Price per Treat:", input$price_per_treat))
    print(paste("Baking Cost per Treat: ", input$baking_cost_per_treat))
    
    # Calculate profit and expenses for the sale
    if (input$treats_per_batch > 0) {
      cost_per_treat <- input$bake_cost / input$treats_per_batch
      baking_cost_per_treat <- input$bake_cost / input$treats_per_batch
      profit <- (input$price_per_treat - cost_per_treat) * input$treats_sold
      sales <- (input$price_per_treat * input$treats_sold)
      
      # Add data to the profit summary
      new_data <- data.frame(
        Treat = input$treat,
        Baking_Cost = input$bake_cost,
        Treats_Per_Batch = input$treats_per_batch,
        Treats_Sold = input$treats_sold,
        Price_Per_Treat = input$price_per_treat,
        Baking_Cost_Per_Treat = baking_cost_per_treat,
        Profit = profit,
        Sales = sales,
        stringsAsFactors = FALSE
      )
  
      # If profit_data$data is empty, initialize with new_data
      if (nrow(profit_data$data) == 0) {
        profit_data$data <- new_data
      } else {
        profit_data$data <- rbind(profit_data$data, new_data)
      }
    }
  })
      
  output$profit_summary <- renderTable({
    profit_data$data
  })
  
  output$total_profit <- renderText({
    total_profit <- sum(profit_data$data$Profit)
    paste("$", round(total_profit, 2))
  })
  
  output$sales_summary <- renderText({
    sales_summary <- sum(profit_data$data$Sales)
    paste("$", round(sales_summary, 2))
  })
  
  # Render Profit Plot
  output$profit_plot <- renderPlot({
    if (nrow(profit_data$data) > 0){
      ggplot(profit_data$data, aes(x = Treat, y = Profit, fill = Treat)) +
      geom_bar(stat = "identity") +
      labs(title = "Profit by Treat Graph", x = "Treat", y = "Profit") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set3")
    }
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("bakedGF_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(profit_data$data, file)
    }
  )
  
}

# Run App
shinyApp(ui = ui, server = server)