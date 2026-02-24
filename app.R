
library(shiny)
library(tidyverse)
library(lubridate)
url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-02-10/schedule.csv"
schedule <- read_csv(url, show_col_types = FALSE) |>
  mutate(
    date         = as.Date(date),
    start_dt     = ymd_hms(start_datetime_local),
    end_dt       = ymd_hms(end_datetime_local),
    duration_min = as.numeric(difftime(end_dt, start_dt, units = "mins")),
    event_type   = case_when(
      is_training    ~ "Training",
      is_medal_event ~ "Medal",
      TRUE           ~ "Competition"
    )
  ) |>
  filter(duration_min > 0, duration_min < 600)

# Returns event counts by discipline, stacked by event type
count_by_discipline <- function(df) {
  df |>
    count(discipline_name, event_type) |>
    group_by(discipline_name) |>
    mutate(total = sum(n)) |>
    ungroup() |>
    mutate(discipline_name = fct_reorder(discipline_name, total))
}

# Returns median event duration per discipline
duration_by_discipline <- function(df) {
  df |>
    filter(!is.na(duration_min)) |>
    group_by(discipline_name) |>
    summarise(
      median_dur = median(duration_min),
      n          = n(),
      .groups    = "drop"
    )
}




# UI
ui <- fluidPage(
  titlePanel("2026 Winter Olympics — Milan-Cortina Event Schedule"),
  
  p("This app explores the schedule of 1,866 events at the 2026 Winter Olympics
    in Milan-Cortina, Italy. Events span medal competitions, non-medal competitions,
    and athlete training sessions across 16 sport disciplines.
    Use the controls on the left to filter the data."),
  
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      # Dynamic query 1: event type filter
      checkboxGroupInput(
        inputId  = "event_types",
        label    = "Filter by Event Type",
        choices  = c("Medal", "Competition", "Training"),
        selected = c("Medal", "Competition", "Training")
      ),
      
      hr(),
      
      # Dynamic query 2: date range filter
      sliderInput(
        inputId    = "date_range",
        label      = "Filter by Date",
        min        = min(schedule$date),
        max        = max(schedule$date),
        value      = c(min(schedule$date), max(schedule$date)),
        step       = 1,
        timeFormat = "%b %d"
      ),
      
      hr(),
      
      p(em("Data: TidyTuesday 2026 Week 6. Curated by Daniel Chen (Posit / UBC)."))
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel(
          "Events by Discipline",
          br(),
          p("How many events does each sport have? Bars are stacked by event type."),
          plotOutput("bar_plot", height = "500px")
        ),
        
        tabPanel(
          "Typical Duration",
          br(),
          p("How long are events in each sport? The dot marks the median duration
            in minutes for all events matching the current filter. Endurance and
            team sports tend to run much longer than sprint or judged disciplines."),
          plotOutput("duration_plot", height = "500px")
        )
        
      )
    )
  )
)
# Server
server <- function(input, output, session) {
  filtered <- reactive({
    schedule |>
      filter(
        event_type %in% input$event_types,
        date >= input$date_range[1],
        date <= input$date_range[2]
      )
  })
  
  
  
  # Tab 1: stacked bar chart of event counts by discipline
  output$bar_plot <- renderPlot({
    df <- count_by_discipline(filtered())
    validate(need(nrow(df) > 0, "No events match the selected filters."))
    
    ggplot(df, aes(x = n, y = discipline_name, fill = event_type)) +
      geom_col() +
      scale_fill_manual(
        values = c(Medal = "#C9962E", Competition = "#4A90D9", Training = "#7CB87C"),
        name   = "Event Type"
      ) +
      labs(
        title = "Number of Scheduled Events by Discipline",
        x     = "Number of Events",
        y     = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })
  
  # Tab 2: chart of median duration by discipline
  output$duration_plot <- renderPlot({
    df <- duration_by_discipline(filtered())
    validate(need(nrow(df) > 0, "No events match the selected filters."))
    
    ggplot(df, aes(x = median_dur, y = reorder(discipline_name, median_dur))) +
      geom_segment(
        aes(x = 0, xend = median_dur, yend = discipline_name),
        colour = "grey70"
      ) +
      geom_point(size = 3, colour = "#4A90D9") +
      labs(
        title = "Median Event Duration by Discipline",
        x     = "Median Duration (minutes)",
        y     = NULL
      ) +
      theme_minimal(base_size = 13)
  })
}


#options(shiny.reactlog = TRUE)
# Run
shinyApp(ui, server)




