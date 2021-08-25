source("R/functions.R")

TRship_ui <- function(id,label = "Counter") {
  ns <- NS(id)
  tagList(
    div(class="ui three column stackable grid container",
        div(class="five wide column",
            p(strong("Select a Date:")),
            dropdown_input(input_id = ns("TR_date"), choices = NULL)
        ),
        div(class="six wide column",
            p(strong("Select a Port:")),
            dropdown_input(input_id = ns("TR_port"), choices = NULL)
        ),
        div(class="five wide column",
            p(strong("Select ship type:")),
            dropdown_input(input_id = ns("TRship_type"), choices = NULL)
        )
    ),br(),
    main_panel(
      segment(
        div(class="ui black ribbon label", "Map"),
        leafletOutput(ns("Traffic_map"))
      ),div(class="ui divider"),
      segment(
        h4("Summary of each ship"),
        semantic_DTOutput(ns("tab_resume"))
      )
    )
  )
}

TRship_server <- function(id){
  ns <- NS(id)
  moduleServer(id,function(input,output,session){
    
    update_dropdown_input(session, "TR_date",
                          choices = sort(as.character(unique(data$date))))
    update_dropdown_input(session, "TR_port",
                          choices = sort(unique(data$PORT)))
    
    observe({
      req(input$TR_port,input$TR_date)
      TR_data <- data %>%
        filter(date == input$TR_date,
               PORT == input$TR_port)
      update_dropdown_input(session,"TRship_type",
                            choices = sort(unique(TR_data$ship_type)))
    })
    
    TRselect_info <- reactive({
      req(input$TR_date,input$TR_port,input$TRship_type)
      data %>%
        filter(date==input$TR_date,
               PORT==input$TR_port,
               ship_type==input$TRship_type) %>%
        traffic_sail()
    })
    
    output$tab_resume <- renderDataTable({
      resume <- TRselect_info() %>%
        ltime_resume()
      semantic_DT(resume,
                  options = list(bInfo = F,
                                 dom = "rtp",
                                 pageLength = 4))
    })
    output$Traffic_map <- renderLeaflet({
      bx <- TRselect_info() %>% bbox()
      TRselect_info() %>%
        geojson_json(lat="lat_i",lon="lon_i") %>%
        leaflet() %>%
        addTiles() %>%
        fitBounds(bx$lon1,bx$lat1,
                  bx$lon2,bx$lat2) %>%
        addTimeline(
          sliderOpts = sliderOptions(position = "bottomright"),
          timelineOpts = timelineOptions(
            styleOptions = NULL,
            pointToLayer = htmlwidgets::JS(
              "
              function(data, latlng) {
               return L.circleMarker(latlng, {
               radius: 5,
               color: 'black',
               weight: 1,
               fillColor: data.properties.color,
               fillOpacity: 1
              }) .bindTooltip(
              data.properties.SHIPNAME,
              {permanent: false}
              ).openTooltip()
              }
              "
            )
          )
        )
    })
  })
}