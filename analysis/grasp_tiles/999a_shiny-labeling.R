library(shinyjs)
library(ggplot2)
library(plotly)
library(rgdal)
library(RCurl)
library(shiny)

backup_options <- options()
options(viewer = NULL)
options(shiny.reactlog = TRUE)

base_path <- "C:/Users/wfu3/Desktop/temp/tiles/orig_tiles/tiles_tower"
img_path <- c(file.path(base_path, "980195_09_yes_tile23_yes.tif"),
              file.path(base_path, "980195_100_yes_tile10_yes.tif"))

# get image dimensions for plot size
img_info <- rgdal::GDALinfo(img_path[1])

img_height <- img_info["rows"]
names(img_height) <- NULL # plotly doesn't like named vectors

img_width <- img_info["columns"]
names(img_width) <- NULL # plotly doesn't like named vectors

# get list of image names to display as text
img_names <- strsplit(img_path, "/")
img_names <- sapply(img_names, function(x) x[length(x)])
img_names <- gsub(x = img_names, pattern = "\\.tif", replacement = "")

# encode images as base64 binary for plotly
img_encode <- lapply(img_path, function(x) RCurl::base64Encode(
  readBin(x, "raw", file.info(x)[1, "size"]), 
  "txt"
))

# setup invisible data points used for making boxes
fake_dat <- expand.grid(x = 1:img_width, y = 1:img_height)

# setting to make plot axes invisible
ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)

ui <- fluidPage(
  useShinyjs(),
  # code to reset plotlys event_data("plotly_click", source="A") to NULL
  #   - source name goes in location of braces with no quotes: '.clientValue-plotly_click-{A}'
  extendShinyjs(
    text = "shinyjs.resetSelect = function() { Shiny.onInputChange('.clientValue-plotly_selected-selected', 'null'); }"
    ),
  
  titlePanel("Label some towers!"),
  
  fluidRow(
    column(8,
      wellPanel(
        plotlyOutput("imageplot")
        )
      ),
    
    column(4, 
      wellPanel(
        actionButton("undo_box_button", "Undo"),
        
        actionButton("submit_box_button", "Submit"),
        
        HTML("<br><br>"),
        
        actionButton(
          "previous_button", 
          label = HTML(
            "<span class='small'><i class='glyphicon glyphicon-arrow-left'></i> Previous</span>"
          )
        ),
        
        actionButton(
          inputId = "next_button", 
          label = HTML(
            "<span class='small'>Next <i class='glyphicon glyphicon-arrow-right'></i></span>"
          )
        ),
        
        HTML("<br><br>"),
        
        textOutput("image_name")
      )
    )
  )
)

server <- function(input, output, session) {
  image_number <- reactiveVal(1)
  tower_coords <- list() # store submitted tower coordinates
  all_boxes <- list() # updated list of plotly lines annotating the image
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  output$imageplot <- renderPlotly({
    config(displaylogo = FALSE, collaborate = FALSE,
      p = plot_ly(data = fake_dat, x = ~x, y = ~y, source = "selected") %>%
            add_markers(opacity = 0, hoverinfo = "none") %>%
            layout(
              images = list(
                list(source =  paste('data:image/png;base64', img_encode[[1]], sep=','),
                     xref = "x",
                     yref = "y",
                     x = 1,
                     y = img_height,
                     sizex = img_width,
                     sizey = img_height,
                     sizing = "stretch",
                     layer = "below"
                )
              ),
              xaxis = ax, 
              yaxis = append(ax, list(scaleanchor = "x")),
              dragmode = "select"
            )
          )
    })

  # undo selection, remove selection boxes, and advance image
  #  if either 'next' or 'submit' buttons are clicked
  observeEvent(
    {(input$next_button | input$submit_box_button)},
    ignoreInit = TRUE,
    {
    if (image_number() == length(img_encode)) return()  
    
    # # undo selection
    # cat(file=stderr(), "Next button selected prior to reset:", unlist(selected_points), "\n")
    # js$resetSelect()
    # cat(file=stderr(), "Next button selected after reset:", unlist(selected_points), "\n")
    #   
    # remove boxes
    all_boxes <<- list()
    
    plotlyProxy("imageplot", session) %>%
      plotlyProxyInvoke("relayout", list(
        shapes = all_boxes
      )
      )
      
    # advance image  
    image_number(image_number() + 1)

    plotlyProxy("imageplot", session) %>%
    plotlyProxyInvoke("relayout", list(
      images = list(
        list(source =  paste('data:image/png;base64', img_encode[[image_number()]], sep=','),
             xref = "x",
             yref = "y",
             x = 1,
             y = img_height,
             sizex = img_width,
             sizey = img_height,
             sizing = "stretch",
             layer = "below"
        )
        )
      )
    )
  })
  
  observeEvent(input$previous_button, {
    if (image_number() %in% c(0, 1)) return()  
    
    # remove boxes
    all_boxes <<- list()
    
    plotlyProxy("imageplot", session) %>%
      plotlyProxyInvoke("relayout", list(
        shapes = all_boxes
      )
      )
    
    # change image
    image_number(image_number() - 1)
    
    plotlyProxy("imageplot", session) %>%
      plotlyProxyInvoke("relayout", list(
        images = list(
          list(source =  paste('data:image/png;base64', img_encode[[image_number()]], sep=','),
               xref = "x",
               yref = "y",
               x = 1,
               y = img_height,
               sizex = img_width,
               sizey = img_height,
               sizing = "stretch",
               layer = "below"
          )
        )
      )
      )
    })  
  
  output$image_name <- renderText({
    paste0("Image: ", img_names[image_number()])
    })
  
  # when points are selected in the plot this observer fires
  observe({
    selected_points <- event_data("plotly_selected", source = "selected")
    
    cat(file=stderr(), unlist(selected_points), "\n")
    
    if(is.null(selected_points)) return()
    
    # coordinates of current box selection
    x0 <- min(selected_points$x)
    x1 <- max(selected_points$x)
    y0 <- min(selected_points$y)
    y1 <- max(selected_points$y)
    
    current_box <- list(
      list(
        type = "line", xref = "x", yref = "y",
        x0 = x0,
        y0 = y0,
        x1 = x0,
        y1 = y1
      ),
      list(
        type = "line", xref = "x", yref = "y",
        x0 = x0,
        y0 = y1,
        x1 = x1,
        y1 = y1
      ),
      list(
        type = "line", xref = "x", yref = "y",
        x0 = x1,
        y0 = y1,
        x1 = x1,
        y1 = y0
      ),
      list(
        type = "line",
        xref = "x",
        yref = "y",
        x0 = x1,
        y0 = y0,
        x1 = x0,
        y1 = y0
      )
    )
    
    # add new lines to the list of all lines currently on the plot
    all_boxes <<- append(all_boxes, current_box)
    
    # add new lines to plot
    plotlyProxy("imageplot", session) %>%
      plotlyProxyInvoke("relayout", list(
        shapes = all_boxes
        )
      )
  })
  
  # this removes the most recent box when the Undo button is clicked
  observeEvent(input$undo_box_button, {
    # how many lines are currently on the plot?
    num_lines <- length(all_boxes)
    
    # remove the last 4
    all_boxes[(num_lines - 3):num_lines] <<- NULL
    
    # update lines
    plotlyProxy("imageplot", session) %>%
      plotlyProxyInvoke("relayout", list(
        shapes = all_boxes
        )
      )
  })
  
  observeEvent(input$submit_box_button, {
    if (length(all_boxes) == 0) return()
    
    names(all_boxes) <- rep(isolate(img_names[image_number()]), length(all_boxes))
  
    tower_coords <<- append(tower_coords, all_boxes)
    
    cat(file=stderr(), "Saved tower coords!", "\n")
  })
  
}

runApp(list(ui = ui, server = server), launch.browser = TRUE)

option <- backup_options
options(shiny.reactlog=FALSE)