library(shiny)
library(DT)
source("wind_array.R")

init_wind_map <- make_initial_wind_map(5)

ui <- fluidPage(
  # Some custom CSS for a smaller font for preformatted text
  tags$head(
    tags$style(HTML("
                    pre, table.table {
                    font-size: smaller;
                    }
                    "))
    ),

  fluidRow(
    column(width = 3,
           # In a plotOutput, passing values for click, dblclick, hover, or brush
           # will enable those interactions.
           plotOutput("plot1", height = "600px", width = "250%",
                      # Equivalent to: click = clickOpts(id = "plot_click")
                      click = clickOpts(id = "plot_click"),
                      dblclick = dblclickOpts(
                        id = "plot_dblclick"
                      ),
                      hover = hoverOpts(
                        id = "plot_hover",
                        delay=50
                      )
           )
    ),
    column(width = 4, offset = 4,
           DT::dataTableOutput('table')
    )
  ),

  fluidRow(
    column(width = 3,
           verbatimTextOutput("hover_info")
    ),
    column(width = 3,
           verbatimTextOutput("click_info")
    )
  )
)

server <- function(input, output) {
  cur_x <<- 1
  cur_y <<- 1

  turbine_array <<- data.frame(x = numeric(0), y=numeric(0), wind.speed=numeric(0))
  cur_wind_map <<- init_wind_map

  output$plot1 <- renderPlot({
     plot_wind_map(cur_wind_map)
  })

  observeEvent(input$plot_click, {
    cur_x <<-  round(input$plot_hover$x * domain_dims[1])
    cur_y <<- round(input$plot_hover$y * domain_dims[2])

    cur_x <<- max(min(cur_x, domain_dims[1]), 1)
    cur_y <<- max(min(cur_y, domain_dims[2]), 1)


    turbine_array <<- rbind(turbine_array,
                            data.frame(x = cur_x, y=cur_y, ws=round(cur_wind_map[cur_y, cur_x], 1)))
    tw = turbine_conditional_wind_map(c(cur_y, cur_x), init_wind_map)
    cur_wind_map <<- pmin(tw, cur_wind_map)
    turbine_array$wind.speed <- round(cur_wind_map[cbind(turbine_array$y, turbine_array$x)], 1)
    output$table <- renderDataTable(DT::datatable(turbine_array,
                                                  options = list(searching = FALSE, paging = FALSE)))

    output$plot1 <- renderPlot({
      plot_wind_map(cur_wind_map)
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


shinyApp(ui, server)
