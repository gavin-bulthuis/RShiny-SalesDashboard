library(shiny)
library(dplyr)
library(shinyWidgets)
library(shinyjs)
library(DT)

# Define treat prices with categories
treat_prices <- data.frame(
  treat = c(
    "Birthday Cake", "PB&J", "Chocolate Strawberry", "DF Chocolate Strawberry",
    "Salted Nut Roll", "Scotcheroo", "Rice Krispie", "Monster Bar", "Lemonies",
    "Peanut Butter", "Cookies & Cream", "Cinnamon Chocolate Chip",
    "Oatmeal Cream Pie", "Frosted Sugar", "Cookie Flight"
  ),
  price = c(6.00, 6.00, 6.00, 6.00, 5.00, 5.00, 5.00, 5.00, 5.00,
            3.00, 4.00, 3.00, 5.00, 5.00, 10.00),
  category = c(
    rep("Cupcakes", 4),
    rep("Bars", 5),
    rep("Cookies", 5),
    rep("Flights", 1)
  ),
  stringsAsFactors = FALSE
)

# CSV file to store sales
sales_file <- "sales_log.csv"

if (!file.exists(sales_file)) {
  write.csv(data.frame(
    time = character(),
    treat = character(),
    quantity = numeric(),
    subtotal = numeric(),
    stringsAsFactors = FALSE
  ), sales_file, row.names = FALSE)
}

ui <- fluidPage(
  useShinyjs(),
  titlePanel(div("baKed GF Pop Up Sales", class = "title-panel")),
  br(),
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="styles.css")
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar-panel", 
      h4("Select Treat(s):"),
      
      # Treat buttons grouped by category
      lapply(unique(treat_prices$category), function(cat) {
        div(
          h5(cat),
          lapply(treat_prices$treat[treat_prices$category == cat], function(t) {
            actionButton(
              inputId = paste0("btn_", gsub(" ", "_", t)), # sanitize ID
              label = t,
              class = "btn btn-outline-primary",
              style = "margin:2px; white-space: normal;"
            )
          }),
          style = "margin-bottom: 15px;"
        )
      }),
      
      br(),
      uiOutput("quantity_display"),
      actionButton("add_sale", "Add Sale", class = "btn"),
      br(), br(),
      
      h4("Transaction Total:"),
      div(textOutput("transaction_total"), class = "total-box"),
      br()
    ),
    
    mainPanel(
      class = "main-panel",
      br(),
      h4("Event Total Sales"),
      div(textOutput("all_time_total"), class = "total-box"),
      DTOutput("sales_table"),
      br(), br(), br(), br(),
      actionButton("clear_sales", "Clear Sales Log", class = "btn")
    )
  )
)

server <- function(input, output, session) {
  
  # ReactiveValues to track quantity
  treat_qty <- reactiveValues()
  for (t in treat_prices$treat) treat_qty[[t]] <- 0
  
  # Increment when clicked
  lapply(treat_prices$treat, function(t) {
    observeEvent(input[[paste0("btn_", gsub(" ", "_", t))]], {
      treat_qty[[t]] <- treat_qty[[t]] + 1
    })
  })
  
  # Show current quantity
  output$quantity_display <- renderUI({
    lapply(treat_prices$treat, function(t) {
      div(
        paste(t, ": ", treat_qty[[t]]),
        style = "margin-bottom: 5px;"
      )
    })
  })
  
  # Transaction data
  transaction_data <- reactive({
    df <- data.frame(
      treat = treat_prices$treat,
      price = treat_prices$price,
      quantity = sapply(treat_prices$treat, function(t) treat_qty[[t]]),
      stringsAsFactors = FALSE
    )
    df <- df[df$quantity > 0, ]
    df$subtotal <- df$price * df$quantity
    df
  })
  
  # Transaction total
  output$transaction_total <- renderText({
    paste0("$", round(sum(transaction_data()$subtotal), 2))
  })
  
  # Add transaction
  observeEvent(input$add_sale, {
    df <- transaction_data()
    if (nrow(df) > 0) {
      df$time <- Sys.time()
      df <- df %>% select(time, treat, quantity, subtotal)
      write.table(df, sales_file, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      for (t in treat_prices$treat) treat_qty[[t]] <- 0
    }
  })
  
  # Clear log
  observeEvent(input$clear_sales, {
    write.csv(data.frame(
      time = character(),
      treat = character(),
      quantity = numeric(),
      subtotal = numeric(),
      stringsAsFactors = FALSE
    ), sales_file, row.names = FALSE)
    for (t in treat_prices$treat) treat_qty[[t]] <- 0
  })
  
  # Sales log reactive
  sales_log <- reactiveVal(read.csv(sales_file))
  
  observe({
    invalidateLater(1000, session)
    sales_log(read.csv(sales_file))
  })
  
  # Table
  output$sales_table <- renderDT({
    datatable(sales_log(), editable = TRUE, options = list(dom = "t"))
  })
  
  # Capture edits
  observeEvent(input$sales_table_cell_edit, {
    info <- input$sales_table_cell_edit
    df <- sales_log()
    df[info$row, info$col] <- info$value
    if (names(df)[info$col] == "quantity") {
      treat_name <- df$treat[info$row]
      price <- treat_prices$price[match(treat_name, treat_prices$treat)]
      df$subtotal[info$row] <- as.numeric(df$quantity[info$row]) * price
    }
    sales_log(df)
    write.csv(df, sales_file, row.names = FALSE)
  })
  
  # All-time total
  output$all_time_total <- renderText({
    paste0("$", round(sum(sales_log()$subtotal), 2))
  })
}

shinyApp(ui = ui, server = server)
