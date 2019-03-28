

seed <- 0
num_maps <- 5
max_turbines <- 15

#' @import shiny
start_game <- function(n_turbines = 15, n_maps = 5){
  max_turbines <<- n_turbines
  num_maps <<- n_maps

  print(num_turbines)
  shinyApp(ui, server)

}

#' @import shiny
ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
                    pre, table.table {
                    font-size: smaller;
                    }
                    ")),
    shiny::tags$style(type = 'text/css', '#score
               {font-size: 18px; font-family: Helvetica; background-color: rgba(255,255,255,0.40);
               color: blue; border-style: none;}')
    ),

  shiny::fluidRow(
    shiny::column(width = 3,
           # In a plotOutput, passing values for click, dblclick, hover, or brush
           # will enable those interactions.
           shiny::plotOutput("plot1", height = "600px", width = "250%",
                      # Equivalent to: click = clickOpts(id = "plot_click")
                      click = shiny::clickOpts(id = "plot_click"),
                      dblclick = shiny::dblclickOpts(
                        id = "plot_dblclick"
                      ),
                      hover = shiny::hoverOpts(
                        id = "plot_hover",
                        delay=10
                      )
           )
    ),
    shiny::column(width = 4, offset = 4,
           DT::dataTableOutput('table')
           #DT::DTOutput('table')
    )
  ),

  shiny::fluidRow(
    shiny::column(width = 3,
           shiny::verbatimTextOutput("hover_info")
    ),
    shiny::column(width = 3,
           span(shiny::verbatimTextOutput("score"), style="color:blue")
    )
  )
)

#' @import shiny
server <- function(input, output) {

  game_over <- FALSE
  seed <<- seed %% num_maps + 1
  init_wind_map <- make_initial_wind_map(seed)

  cur_x <<- 1
  cur_y <<- 1

  turbine_array <<- data.frame(x = numeric(0), y=numeric(0), orig.ws = numeric(0), waked.ws=numeric(0))
  cur_wind_map <<- init_wind_map

  output$plot1 <- renderPlot({
     plot_wind_map(cur_wind_map, main=sprintf("Arrange %d Turbines for Wind Map %d. ", max_turbines, seed))
  })

  observeEvent(input$plot_click, {
    cur_x <<-  round(input$plot_hover$x * domain_dims[1])
    cur_y <<- round(input$plot_hover$y * domain_dims[2])

    cur_x <<- max(min(cur_x, domain_dims[1]), 1)
    cur_y <<- max(min(cur_y, domain_dims[2]), 1)

    if(!game_over){
      ws <- round(init_wind_map[cur_y, cur_x], 1)
      tw = turbine_conditional_wind_map(c(cur_y, cur_x), init_wind_map)
      cur_wind_map <<- pmin(tw, cur_wind_map)
      waked.ws <- round(cur_wind_map[cbind(c(turbine_array$y, cur_y),
                                           c(turbine_array$x, cur_x))], 1)
      turbine_array <<- data.frame(x = c(turbine_array$x, cur_x),
                                   y = c(turbine_array$y, cur_y),
                                   orig.ws = c(turbine_array$orig.ws, ws),
                                   waked.ws = waked.ws)

      output$table <- DT::renderDataTable(DT::datatable(turbine_array,
                                                    options = list(searching = FALSE, paging = FALSE)))
    }
    print (turbine_array)

    output$plot1 <- renderPlot({
      plot_wind_map(cur_wind_map, main=sprintf("Arrange %d Turbines for Wind Map %d. ", max_turbines, seed))
    })

    if(nrow(turbine_array) == max_turbines){
      if(!game_over) beepr::beep(1)
      game_over <<- TRUE
    }

    output$score <- renderText({
    score <- sum(turbine_array$waked.ws)
      if(!game_over)
        sprintf("\nCurrent Score: %5.1f", score)
      else
        sprintf("GAME OVER    \nFinal   Score: %5.1f", score)

    })


  })

  output$hover_info <- renderPrint({
    cur_x <-  round(input$plot_hover$x * domain_dims[1])
    cur_y <- round(input$plot_hover$y * domain_dims[2])

    cur_x <- max(min(cur_x, domain_dims[1]), 1)
    cur_y <- max(min(cur_y, domain_dims[2]), 1)
    cat("Location and wind speed\n")
    cat(paste0("x = ", cur_x, "\ny = ", cur_y,
               "\nWind speed = ", round(cur_wind_map[cur_y, cur_x], 1)))
  })

  output$dblclick_info <- renderPrint({
    cat("input$plot_dblclick:\n")
    str(input$plot_dblclick)
  })


}


