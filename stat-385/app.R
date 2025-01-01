library(shiny)
library(tidyverse)

raptor = read.csv("data/raptor")


ui = navbarPage(
  title = "RAPTOR of Players by Season",
  tabPanel(title = "Input/Visualization",
           titlePanel("Historical Raptor from 1977 - 2022"),
           sidebarLayout(
             sidebarPanel(
               selectInput(
                 inputId = "season", 
                 label = "Season:", choices = sort(unique(raptor$season), decreasing = TRUE),
                 selected = "2022"
                 ),
               selectInput(
                 inputId = "season_type", 
                 label = "Season Type:", choices = sort(unique(raptor$season_type), decreasing = TRUE) ,
                 selected = "RS"
                 ),
               selectInput(
                 inputId = "team", 
                 label = "Team:", choices = c("ALL", sort(unique(raptor$team))),
                 selected = "ALL"
                 ),
               sliderInput(
                 inputId = "mp",
                 label = "Minutes Played:",
                 min = 0, 
                 max = max(raptor$mp),
                 step = 50,
                 value = 500,
                 ticks = FALSE
               )
             ), 
             mainPanel(plotOutput("Plot", height = "800px"))
           )
          ),
  
  
  
  tabPanel(title = "Table By Team", dataTableOutput("table")),
  tabPanel("About", includeMarkdown("about.Rmd"))
)

server = function(input, output) {
  
  raptor_season = reactive({
    raptor |>
      filter(season == input$season)
    })
  
  observeEvent(
    eventExpr = input$season,
    handlerExpr = {
      updateSelectInput(inputId = "season_type",
                        selected = "RS"
                        )
      updateSelectInput(inputId = "team",
                        choices = c("ALL", sort(unique(raptor_season()$team))),
                        selected = "ALL"
                        )
      updateSliderInput(
        inputId = "mp",
        value = 500,
        max = max(raptor_season()$mp))
    }
  )
  
  raptor_season_type = reactive({
    raptor_season() |>
      filter(season_type == input$season_type)
  })
  
  observeEvent(
    eventExpr = input$season_type,
    handlerExpr = {
      updateSelectInput(inputId = "team",
                        choices = c("ALL", sort(unique(raptor_season_type()$team))),
                        selected = "ALL"
      )
      updateSliderInput(
        inputId = "mp",
        value = 500,
        max = max(raptor_season_type()$mp))
    }
    
  )
  
  raptor_season_type_team = reactive({
    raptor_season_type() |>
      filter(team == input$team) |>
      filter(mp >= input$mp)
  })
  
  

  
  
  output$Plot <- renderPlot({
     raptor |>
      filter(season == input$season) |>
      filter(mp >= input$mp) |>
      filter(season_type == input$season_type) |>
      filter(if(input$team != "ALL") team == input$team else TRUE)|>
      ggplot() +
      annotate("rect", xmin = Inf, xmax = 0, ymin = Inf, ymax = 0, fill= "#03dbfc", alpha = 0.2) +
      annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0, fill= "#ff5c4a", alpha = 0.2) + 
      aes(x = raptor_offense, y = raptor_defense, label = player_name) +
      labs(x = "RAPTOR Offense", y = "RAPTOR Defense") +
      geom_point(size = 3, shape = 21, fill = "white") +
      geom_text(size = 3, nudge_y = 0.5) +
      xlim(-10, 10) + 
      ylim(-10, 10) +
      theme_bw()
    })
  
  output$table = renderDataTable({
    raptor_season_type_team()
  },
  options = 
    list(searching = FALSE,paging = FALSE,
         language = list(
           zeroRecords = "Please choose a team to view.") 
         ))
}

# Run the application 
shinyApp(ui = ui, server = server)
