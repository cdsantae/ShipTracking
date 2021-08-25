source("R/functions.R")

Tship_ui <- function(id,label = "Counter") {
  ns <- NS(id)
  tagList(
    sidebar_layout(
      sidebar_panel(
        p(strong("Select a Date:")),
        dropdown_input(input_id = ns("T_date"), choices = NULL),
        p(strong("Select a Port:")),
        dropdown_input(input_id = ns("T_port"), choices = NULL),
        p(strong("Select vessel type:")),
        dropdown_input(input_id = ns("Tship_type"), choices = NULL),
        p(strong("Select vessel name:")),
        dropdown_input(input_id = ns("Tship_name"), choices = NULL)
      ),
      main_panel(
        segment(
          div(class="ui black ribbon label", "Map"),
          leafletOutput(ns("Travel_map")))
        )
      )
    )
}

TSelect_info <- function(input,output, session, data){
  
  return(DF)
}

Tship_server <- function(id){
  ns <- NS(id)
  moduleServer(id,function(input,output,session){
    
    shipIcon <- iconList(
      start = makeIcon("cargo_ship_grey.svg", iconWidth = 60, iconHeight = 60),
      end = makeIcon("cargo_ship_col.svg", iconWidth = 60, iconHeight = 60)
    )
    
    update_dropdown_input(session, "T_date",
                          choices = sort(as.character(unique(data$date))))
    update_dropdown_input(session,"T_port",
                          choices = unique(data$PORT))
    
    observe({
      req(input$T_date, input$T_port)
      T_data <- data %>%
        filter(date == input$T_date,
               PORT == input$T_port)
      update_dropdown_input(session,"Tship_type",
                            choices = unique(T_data$ship_type))
    })
    
    observe({
      req(input$T_date, input$T_port, input$Tship_type)
      T_data <- data %>%
        filter(date == input$T_date,
               PORT == input$T_port,
               ship_type == input$Tship_type)
      update_dropdown_input(session,"Tship_name",
                            choices = unique(T_data$SHIPNAME))
    })
    
    Tselect_info <- reactive({
      req(input$T_date,input$T_port,
          input$Tship_type,input$Tship_name)
      data %>%
        filter(date==input$T_date,
               PORT==input$T_port,
               ship_type==input$Tship_type,
               SHIPNAME==input$Tship_name) %>%
        mutate(end = c(DATETIME[-1],last(DATETIME)+1),
               lat_f = c(LAT[-1],last(LAT)),
               lon_f = c(LON[-1],last(LAT))) %>%
        rename(lat_i=LAT,lon_i=LON,start=DATETIME) %>%
        rowwise() %>%
        mutate(dist=distVincentyEllipsoid(c(lon_i,lat_i),
                                          c(lon_f,lat_f))) %>%
        data.frame()
    })
   
    output$Travel_map <- renderLeaflet({
      Trav_res <- Tselect_info() %>%
        group_by(SHIPNAME,ship_type,is_parked) %>%
        summarise(lon=mean(range(lon_i)),lat=mean(range(lat_i)),
                  total=sum(dist),AvSp=mean(SPEED),
                  TopS=max(SPEED),length=mean(LENGTH,na.rm=T),
                  width=mean(WIDTH,na.rm=T),
                  DWT=mean(DWT,na.rm=T)) %>%
        mutate_at(vars(DWT),~replace(.,is.nan(.),0)) %>%
        ungroup %>% group_by(SHIPNAME,ship_type) %>%
        summarise(lon=mean(lon),lat=mean(lat),
                  total=sum(total),AvSp=sum(AvSp),
                  TopS=sum(TopS),length=mean(length,na.rm=T),
                  width=mean(width,na.rm=T),
                  DWT=sum(DWT,na.rm=T)) %>%
        mutate(title=paste(sep=" ",
                           "<b>Name:</b>",SHIPNAME,"<br>",
                           "<b>type:</b>",ship_type,"<br>",
                           "<b>Total Traveled:</b>",round(total,2),"<br>",
                           "<b>Average Speed:</b>",round(AvSp,2),"<br>",
                           "<b>Top Speed:</b>",TopS,"<br>")) %>% data.frame
      
      Tselect_info() %>%
        leaflet() %>%
        addTiles() %>%
        addMarkers(lng = ~first(lon_i),
                   lat=~first(lat_i),
                   icon = ~shipIcon$start,
                   popup = ~paste("<b>Start</b><br>",
                                  "<b>LAT</b>:",first(lat_i),
                                  "<br><b>LON</b>:",first(lon_i),
                                  "<br><b>Time measure:</b>",first(start))) %>%
        addMarkers(lng = ~last(lon_i),
                   lat=~last(lat_i),
                   icon = ~shipIcon$end,
                   popup = ~paste("<b>End</b><br>",
                                  "<b>LAT</b>:",last(lat_i),
                                  "<br><b>LON</b>:",last(lon_i),
                                  "<br><b>Time measure:</b>",last(end))) %>%
        addPolylines(lng = ~lon_i,lat = ~lat_i) %>%
        addPopups(data = Trav_res,
                  lng = ~lon,
                  lat = ~lat,
                  popup = ~title)
    })
  })
}