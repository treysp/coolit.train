library(shiny)
library(plotly)
library(shinyjs)

img_path <- system.file("external/rlogo.grd", package="raster")
#img_path <- "C:/Users/wfu3/Desktop/temp/tiles/orig_tiles/tiles_tower/980195_09_yes_tile23_yes.tif"

img_tempfile <- tempfile(fileext = ".tif")
img <- raster::brick(img_path)
raster::writeRaster(img, img_tempfile, datatype = "INT1U", overwrite = TRUE)

img_flip <- raster::flip(img, 2)
img_flip_tempfile <- tempfile(fileext = ".tif")
raster::writeRaster(img_flip, img_flip_tempfile, datatype = "INT1U", overwrite = TRUE)

txt <- lapply(c(img_tempfile, img_flip_tempfile), function(x) {
  RCurl::base64Encode(
    readBin(x, "raw", file.info(x)[1, "size"]), 
    "txt"
  )
  })

ui <- fluidPage(
  useShinyjs(),
  # code to reset plotlys event_data("plotly_click", source="A") to NULL
  #   - source name goes in location of braces with no quotes: '.clientValue-plotly_click-{A}'
  extendShinyjs(
    text = "shinyjs.resetSelect = function() { Shiny.onInputChange('.clientValue-plotly_selected-selected', 'null'); }"
    ),
  div(
    h3("PLOTLYPROXY", align = 'center'), 
    style = "border-bottom: solid; border-width: thin;"
  ),
  br(),
  fluidRow(
    column(3,
           h4("relayout", style = "text-decoration: underline;"),
           actionButton("flip_img_button", "Flip image")
    ),
    
    column(3,
           textOutput("sel_points"))
  ),
  br(),
  div(
    plotlyOutput("plot"),
    style = "border: solid black 1px"
  )
)

server <- function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
  })
  
  current_img <- 1
  
  output$plot <- renderPlotly({
    plot_ly(
      type = 'scatter',
      mode = 'markers',
      x = c(1, 2, 2.5, 3, 4), 
      y = c(2, 4,   3, 2, 4),
      source = "selected"
    ) %>%
      layout(
        title = 'Original Title',
        images = list(
          list(source =  paste('data:image/png;base64', txt[[1]], sep=','),
               xref = "x",
               yref = "y",
               x = 1.5,
               y = 4,
               sizex = 2,
               sizey = 2,
               sizing = "stretch",
               layer = "below"
          )
        )
      )
  })
  
  # plotly.relayout
  observeEvent(input$flip_img_button, {
    current_img <<- if (current_img == 1) 2 else 1
    
    # undo selection
    cat(file=stderr(), "Points selected prior to reset:", unlist(selected_points), "\n")
    js$resetSelect()
    cat(file=stderr(), "Points selected after reset:", unlist(selected_points), "\n")

    plotlyProxy("plot", session) %>%
      plotlyProxyInvoke("relayout", 
                        list(
                          images = list(
                            list(source =  paste('data:image/png;base64', 
                                                 txt[[current_img]], sep=','),
                                 xref = "x",
                                 yref = "y",
                                 x = 1.5,
                                 y = 4,
                                 sizex = 2,
                                 sizey = 2,
                                 sizing = "stretch",
                                 layer = "below"
                                 )
                            )
                          )
                        )
  })
  
  output$sel_points <- renderText({
    selected_points <<- event_data("plotly_selected", source = "selected")
 
    paste0("You selected: ", paste(unlist(selected_points), collapse = " "))
  })
  
}

runApp(list(ui = ui, server = server), launch.browser = TRUE)
