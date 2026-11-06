
# ==========================================
# MUSIC LISTENING HABITS AND STUDY CONCENTRATION
# MODERN PURPLE AND BLUE DASHBOARD
# ==========================================

library(shiny)
library(ggplot2)

music_data <- read.csv("music_listening_concentration.csv")

# Calculate filtered data summaries inside the app

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #F3F0FF;
        font-family: 'Segoe UI', sans-serif;
        color: #29264A;
      }

      .main-header {
        background: linear-gradient(120deg, #5426A8, #3478D4);
        color: white;
        padding: 30px;
        border-radius: 15px;
        margin-bottom: 25px;
        text-align: center;
        box-shadow: 0 5px 15px rgba(65, 45, 130, 0.2);
      }

      .main-header h2 {
        font-weight: bold;
        margin-bottom: 8px;
      }

      .main-header p {
        font-size: 15px;
        opacity: 0.9;
      }

      .sidebar {
        background-color: white;
        padding: 20px;
        border-radius: 15px;
        box-shadow: 0 3px 12px rgba(50, 40, 100, 0.08);
      }

      .sidebar h4 {
        color: #5426A8;
        font-weight: bold;
      }

      .summary-card {
        background: white;
        border-radius: 15px;
        padding: 20px;
        margin-bottom: 20px;
        min-height: 125px;
        box-shadow: 0 3px 12px rgba(50, 40, 100, 0.08);
        border-left: 5px solid #7546D8;
      }

      .summary-card h4 {
        color: #77718D;
        font-size: 14px;
        font-weight: bold;
      }

      .summary-value {
        font-size: 27px;
        color: #5426A8;
        font-weight: bold;
      }

      .section-card {
        background: white;
        border-radius: 15px;
        padding: 20px;
        margin-bottom: 22px;
        box-shadow: 0 3px 12px rgba(50, 40, 100, 0.07);
      }

      .section-card h3 {
        color: #5426A8;
        font-size: 19px;
        font-weight: bold;
      }

      .btn {
        background-color: #6741C6;
        color: white;
        border: none;
        border-radius: 8px;
        font-weight: bold;
        width: 100%;
      }

      .btn:hover {
        background-color: #45228E;
        color: white;
      }

      .form-control {
        border-radius: 8px;
        border: 1px solid #D9D0F0;
      }

      table {
        background: white;
        width: 100%;
      }

      th {
        background-color: #6741C6;
        color: white;
      }

      .footer {
        text-align: center;
        color: #77718D;
        padding: 20px;
      }
    "))
  ),
  
  div(
    class = "main-header",
    h2("MUSIC & STUDY ANALYTICS"),
    p("Music Listening Habits and Study Concentration"),
    p("Student Research Dashboard | Quantitative Methods")
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      div(
        class = "sidebar",
        
        h4("DASHBOARD FILTERS"),
        p("Customize your student data view."),
        
        selectInput(
          "year",
          "Year Level",
          choices = c("All", sort(unique(music_data$Year_Level)))
        ),
        
        selectInput(
          "genre",
          "Preferred Music Genre",
          choices = c("All", sort(unique(music_data$Preferred_Music_Genre)))
        ),
        
        selectInput(
          "music",
          "Music During Study",
          choices = c("All", sort(unique(music_data$Music_During_Study)))
        ),
        
        br(),
        actionButton("reset", "RESET FILTERS")
      )
    ),
    
    mainPanel(
      width = 9,
      
      h3("STUDENT OVERVIEW"),
      
      fluidRow(
        column(
          4,
          div(
            class = "summary-card",
            h4("TOTAL STUDENTS"),
            div(class = "summary-value", textOutput("total_students"))
          )
        ),
        
        column(
          4,
          div(
            class = "summary-card",
            h4("AVERAGE MUSIC HOURS"),
            div(class = "summary-value", textOutput("average_music"))
          )
        ),
        
        column(
          4,
          div(
            class = "summary-card",
            h4("AVERAGE CONCENTRATION"),
            div(class = "summary-value", textOutput("average_concentration"))
          )
        )
      ),
      
      div(
        class = "section-card",
        h3("01 | Concentration by Music Genre"),
        plotOutput("graph1", height = "300px")
      ),
      
      div(
        class = "section-card",
        h3("02 | Music Listening Hours vs Concentration"),
        plotOutput("graph2", height = "300px")
      ),
      
      div(
        class = "section-card",
        h3("03 | Academic Performance by Music During Study"),
        plotOutput("graph3", height = "300px")
      ),
      
      div(
        class = "section-card",
        h3("STUDENT DATA RECORDS"),
        tableOutput("student_table")
      ),
      
      div(
        class = "footer",
        "Music Listening Habits and Study Concentration | Quantitative Methods Project"
      )
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    
    data <- music_data
    
    if (input$year != "All") {
      data <- data[data$Year_Level == input$year, ]
    }
    
    if (input$genre != "All") {
      data <- data[data$Preferred_Music_Genre == input$genre, ]
    }
    
    if (input$music != "All") {
      data <- data[data$Music_During_Study == input$music, ]
    }
    
    data
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "year", selected = "All")
    updateSelectInput(session, "genre", selected = "All")
    updateSelectInput(session, "music", selected = "All")
  })
  
  output$total_students <- renderText({
    nrow(filtered_data())
  })
  
  output$average_music <- renderText({
    
    data <- filtered_data()
    
    if (nrow(data) == 0) return("No data")
    
    paste0(round(mean(data$Daily_Music_Listening_Hours), 2), " hrs")
  })
  
  output$average_concentration <- renderText({
    
    data <- filtered_data()
    
    if (nrow(data) == 0) return("No data")
    
    paste0(round(mean(data$Study_Concentration_Score), 2), "/10")
  })
  
  output$graph1 <- renderPlot({
    
    data <- filtered_data()
    
    validate(need(nrow(data) > 0, "No matching records."))
    
    genre_average <- aggregate(
      Study_Concentration_Score ~ Preferred_Music_Genre,
      data = data,
      FUN = mean
    )
    
    ggplot(
      genre_average,
      aes(
        x = Preferred_Music_Genre,
        y = Study_Concentration_Score,
        fill = Preferred_Music_Genre
      )
    ) +
      geom_col(width = 0.7) +
      labs(x = "Music Genre", y = "Concentration Score") +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold")
      )
  })
  
  output$graph2 <- renderPlot({
    
    data <- filtered_data()
    
    validate(need(nrow(data) > 0, "No matching records."))
    
    ggplot(
      data,
      aes(
        x = Daily_Music_Listening_Hours,
        y = Study_Concentration_Score
      )
    ) +
      geom_point(color = "#6741C6", size = 3, alpha = 0.8) +
      geom_smooth(
        method = "lm",
        se = FALSE,
        color = "#E36B9A"
      ) +
      labs(x = "Daily Music Listening Hours", y = "Concentration Score") +
      theme_minimal(base_size = 13)
  })
  
  output$graph3 <- renderPlot({
    
    data <- filtered_data()
    
    validate(need(nrow(data) > 0, "No matching records."))
    
    ggplot(
      data,
      aes(
        x = Music_During_Study,
        y = Academic_Performance_Score,
        fill = Music_During_Study
      )
    ) +
      geom_boxplot(alpha = 0.85) +
      labs(x = "Music During Study", y = "Academic Performance Score") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  })
  
  output$student_table <- renderTable({
    filtered_data()
  })
  
}

shinyApp(ui = ui, server = server)
