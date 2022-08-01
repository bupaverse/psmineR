#' Construct the aggregated performance spectrum in an interactive Shiny gadget
#'
#' @param eventlog The Event Log object on which you want to mine the performance spectrum
#' @param plotly True if you desire a ggplotly object as visualisation instead of a ggplot
#' @return A ggplot2 or plotly object describing the aggregated performance spectrum
#' @examples
#' iperformance_aggregated(sepsis, TRUE)
#' @export

iperformance_aggregated <- function(eventlog, plotly = FALSE) {

  ui <- miniPage(
    gadgetTitleBar("Interactive Aggregated Performance Miner"),
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
                            multiple = FALSE),
             selectizeInput("grouping", "Grouping:",
                            choices = c("start", "stop"),
                            multiple = FALSE,
                            selected = "start"),
             numericInput("bins", label = "Number of bins:",
                          min = 5,
                          value = 10)
      ),
      column(width = 10,
             uiOutput("plot")
      )
    )
  )

  server <- function(input, output, session){

    construct_seg <- reactive({
      construct_segments(eventlog)
    })

    build_class <- reactive({
      build_classifier(construct_seg(), input$classificationattribute)
    })

    filter_seg <- reactive({
      filter_segments(build_class(), input$coverage, input$n_segments)
    })

    log <- reactive({
      order_segments(filter_seg())
    })

    output$plot <- renderUI({
      if(plotly){
        plotlyOutput("plotly_aggregated", height = 700)
      } else {
        plotOutput("plot_aggregated", height = 700)
      }

    })

    output$plot_aggregated <- renderPlot({
      plot_aggregated(log(), input$classificationattribute, input$grouping, input$bins)
    })

    output$plotly_aggregated <- renderPlotly({
      plot_aggregated(log(), input$classificationattribute, input$grouping, input$bins) %>%
        ggplotly()
    })

    observeEvent(input$done, {
      stopApp()
    })
  }

  runGadget(shinyApp(ui, server), viewer = dialogViewer("Interactive Aggregated Performance Miner", height = 900, width = 1600))

}
