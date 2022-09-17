#' Construct the detailed performance spectrum in an interactive Shiny gadget
#'
#' @param eventlog The Event Log object on which you want to mine the performance spectrum
#' @param plotly True if you desire a ggplotly object as visualisation instead of a ggplot
#' @return A ggplot2 or plotly object describing the detailed performance spectrum
#' @examples
#' \dontrun{
#' iperformance_detailed(sepsis, TRUE)
#' }
#' @export

iperformance_detailed <- function(eventlog, plotly = FALSE) {

  ui <- miniPage(
    gadgetTitleBar("Interactive Detailed Performance Miner"),
    miniContentPanel(
      column(width = 2,
             numericInput("n_segments", label = "Number of segments (if 0, uses coverage):",
                          min = 0,
                          value = 5),
             sliderInput("coverage", label = "Coverage:",
                         min = 0,
                         max = 1,
                         step = 0.05,
                         value = 0.2),
             selectizeInput("classificationattribute", "Classification attribute",
                            choices = c("quartile", colnames(eventlog)[6:(length(colnames(eventlog))-2)]),
                            multiple = FALSE)
      ),
      column(width = 10,
             uiOutput("plot")
      )
    )
  )

  server <- function(input, output, session){

    construct_seg <- reactive({
      construct_segments(eventlog, input$classificationattribute)
    })

    build_class <- reactive({
      build_classifier(construct_seg(), input$classificationattribute)
    })

    filter_seg <- reactive({
      filter_segments(build_class(), input$coverage, input$n_segments, mapping(eventlog))
    })

    log <- reactive({
      order_segments(filter_seg(), mapping(eventlog)) %>%
        gather(key, x, ta, tb) %>%
        arrange(segment, !!bupaR:::case_id_(mapping(eventlog))) %>%
        mutate(y = if_else(key == "ta", 1, 0)) %>%
        filter(key %in% c("ta", "tb"))
    })

    output$plot <- renderUI({
      if(plotly){
        plotlyOutput("plotly_detailed", height = 700)
      } else {
        plotOutput("plot_detailed", height = 700)
      }

    })

    output$plot_detailed <- renderPlot({
      plot_detailed(log(), input$classificationattribute, mapping(eventlog))
    })

    output$plotly_detailed <- renderPlotly({
      plot_detailed(log(), input$classificationattribute, mapping(eventlog)) %>%
        ggplotly(tooltip = c("case_id", input$classificationattribute))
    })

    observeEvent(input$done, {
      stopApp()
    })
  }

  runGadget(shinyApp(ui, server), viewer = dialogViewer("Interactive Detailed Performance Miner", height = 900, width = 1600))

}
