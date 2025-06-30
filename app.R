library(shiny)
library(shinydashboard)
library(shinybusy)
library(flexdashboard) 
library(lubridate)
library(leaflet)
library(DT)
#library(httr)
library(sf)
library(data.table)
#library(later)
library(base64enc)
library(jsonlite)
library(DBI)
library(RPostgres)
library(plotly)
library(ggplot2)


# library(sodium)
# passkey <- sha256(charToRaw("password123"))
# plaintext <- "example"
# plaintext.raw <- serialize(plaintext, NULL)
# ciphertext <- data_encrypt(plaintext.raw, key = passkey)
# unserialize(data_decrypt(ciphertext, key = sha256(charToRaw("password123"))))

if (file.exists(".Renviron")) {
    readRenviron(".Renviron")
}



ui <- shinydashboard::dashboardPage(
    dashboardHeader(
        title = "UB Air Monitoring"#,
        #titleWidth = 400
    ),
    shinydashboard::dashboardSidebar(disable = TRUE),
    dashboardBody(
        tags$head(
            tags$style(HTML("
        /* 밝은 파랑색 (#3c8dbc) */
        
        /* 1) Header/Logo/상단 바 */
        .main-header .logo, 
        .skin-blue .main-header .navbar {
          background-color: #3c8dbc !important;
          color: white !important;
        }
        
        /* 2) Footer 색상 */
        .footer {
          background-color: #3c8dbc;
          position: fixed;
          left: 0;
          bottom: 0;
          width: 100%;
          color: white;
          text-align: right;
          padding: 10px 20px;
          z-index: 1000;
        }
      "))
        ),
        fluidRow(
            # 왼쪽 컬럼: 지도
            column(
                width = 5,
                #style = "height: calc(100vh - 50px); overflow: auto;",
                box(
                    title = "Location Map", 
                    status = "primary", 
                    solidHeader = TRUE, 
                    width = NULL,
                    #height = "calc(100vh-0px)",
                    leafletOutput("map", height = "calc(100vh - 200px)")
                )
            ),
            # 오른쪽 컬럼: 게이지 및 하단 영역 (Controls + Data Table)
            column(
                width = 7,
                #style = "height: calc(100vh - 150px); overflow: auto;",
                
                # Air Quality Gauges
                box(
                    title = "Air Quality Gauges", 
                    status = "primary", 
                    solidHeader = TRUE,
                    width = NULL,
                    tabsetPanel(
                        tabPanel("PM2.5 Levels",
                                 fluidRow(
                                     column(6,
                                            tags$div("Selected Zipcode Avg.", style = "text-align: center; font-weight: bold;"),
                                            gaugeOutput("pm25_gauge", height = "120px")
                                     ),
                                     column(6,
                                            tags$div("City-wide Avg.", style = "text-align: center; font-weight: bold;"),
                                            gaugeOutput("pm25_avg_gauge", height = "120px")
                                     )
                                 ),
                                 # PM2.5 컬러 램프
                                 plotOutput("pm25_color_ramp", height = "50px"),
                                 # 컬러 램프 안내 문구
                                 tags$p("Color Ramp: indicates air quality levels from 'Good' (Green) to 'Hazardous' (Red).",
                                        style = "text-align:center; font-size:14px; margin-top:5px; font-weight:bold;")
                        ),
                        tabPanel("VOC Levels",
                                 fluidRow(
                                     column(6,
                                            tags$div("Selected Zipcode Avg.", style = "text-align: center; font-weight: bold;"),
                                            gaugeOutput("voc_gauge", height = "120px")
                                     ),
                                     column(6,
                                            tags$div("City-wide Avg.", style = "text-align: center; font-weight: bold;"),
                                            gaugeOutput("voc_avg_gauge", height = "120px")
                                     )
                                 ),
                                 # VOC 컬러 램프
                                 plotOutput("voc_color_ramp", height = "50px"),
                                 # 컬러 램프 안내 문구
                                 tags$p("Color Ramp: indicates air quality levels from 'Good' (Green) to 'Hazardous' (Red).",
                                        style = "text-align:center; font-size:14px; margin-top:5px; font-weight:bold;")
                        )
                    )
                ),
                
                # Controls와 Data Table을 나란히 배치
                fluidRow(
                    column(
                        width = 4,
                        uiOutput("controls_box")
                        # box(
                        #     title = "Controls", 
                        #     status = "primary", 
                        #     solidHeader = TRUE,
                        #     width = NULL,
                        #     height = "calc(100vh - 485px)",
                        #     {
                        #         #Start time should be considered
                        #         init_time <- now(tzone = "America/New_York") |> floor_date("day")
                        #         sliderInput(
                        #             inputId = "time_range",
                        #             label = "Select Time Range:",
                        #             step = 3600*24,
                        #             min = init_time - 604800, #as_datetime(1725116400,tz="utc")
                        #             max = init_time,
                        #             value = c(init_time - 86400, init_time),
                        #             timeFormat = "%b-%d-%y", #"%b-%d-%y<br>%H:%M:%S",
                        #             timezone = "-0500"
                        #         )
                        #         
                        #     },
                        # )
                    ),
                    column(
                        width = 8,
                        box(
                            title = "Each day’s average — last 7 days", 
                            status = "primary", 
                            solidHeader = TRUE,
                            width = NULL,
                            #height = "calc(100vh - 600px)",
                            plotlyOutput("bar_plot",height = "calc(100vh - 547px)")
                        )
                    )
                )
            )
        ),
        div(class = "footer",
            img(src = "UB_logo-removebg-preview.png", alt = "UB Logo", height = "40px",
                style = "vertical-align: middle; margin-right: 10px; margin-left: 10px;"),
            span("UB Clean Air")
        )
    )
)



addLegendCustom <- function(map, theme, position = "bottomright") {
    if (theme == "pm2.5_atm") {
        colors <- c("#73C557", "#FA9857", "#FA4662")
        labels <- c("0–12 µg/m³", "12–55.4 µg/m³", ">55.4 µg/m³")
        title <- "PM2.5 Range"
    } else if (theme == "voc") {
        colors <- c("#73C557", "#FA9857", "#FA4662")
        labels <- c("0–300 ppb", "300–500 ppb", ">500 ppb")
        title <- "VOC Range"
    } else {
        colors <- "gray"
        labels <- "No range"
        title <- "N/A"
    }
    
    leaflet::addLegend(
        map,
        colors = colors,
        labels = labels,
        title = title,
        opacity = 1,
        position = position
    )
}



server <- function(input, output, session) {
    
    
    calibrate_pm25 <- function(pm25atm,rh){
        
        
        y = (0.444*pm25atm) + (-0.059*rh) + 4.768
        
        return(y)
        
    }
    
    show_modal_spinner(
        spin = "cube-grid",
        color = "firebrick",
        text = "Initializing... Please wait..."
    )
    
    
    # 1) 데이터 불러오기
    target <- st_read("data/zip/target.gpkg", quiet = TRUE)
    
    participants <- Sys.getenv("PARTICIPANTS") |>
        base64enc::base64decode() |>
        rawToChar() |>
        jsonlite::fromJSON() |>
        as.data.table()
    
    # # Make the connection
    con <- DBI::dbConnect(RPostgres::Postgres(), dbname = Sys.getenv("DB_NAME"),

                          host = Sys.getenv("DB_HOST"), port = Sys.getenv("DB_PORT"), user = Sys.getenv("DB_USER"),

                          password = Sys.getenv("DB_PASS"))
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # query <- "
    #             SELECT 
    #               time_stamp,
    #               voc,
    #               \"pm2.5_atm\",
    #               sensor_index
    #             FROM \"Purple_Air\"
    #             WHERE time_stamp >= (EXTRACT(EPOCH FROM NOW()) - 3600)
    #             "
    # 
    # 
    # 
    # database <-  dbGetQuery(con, query) |> as.data.table()
    # 
    #DBI::dbDisconnect(con)


    database <- fread("./data/base_PA.csv")
    
    end_time <- database[,max(time_stamp),by = sensor_index]
    latest_conditions <- paste0(
        "(sensor_index = '", end_time$sensor_index,
        "' AND time_stamp > ", end_time$V1, ")"
    )
    where_clause <- paste(latest_conditions, collapse = " OR ")
    
    # hourly reduced fetch query
    query <- paste("
                      SELECT 
                        date_trunc('hour', to_timestamp(time_stamp)) AS time_stamp,
                        sensor_index,
                        AVG(voc) AS voc,
                        AVG(humidity) AS humidity,
                        AVG(\"pm2.5_atm\") AS \"pm2.5_atm\"
                      FROM \"Purple_Air\"
                      WHERE",where_clause,"
                      GROUP BY time_stamp, sensor_index
                    ")
    
    # reduce to daily and calibrate 2.5's
    database <- dbGetQuery(con, query) |> as.data.table()  |> rbind(database,use.names = T) |>
        _[,  `Date_Time(ET)`  :=  floor_date(with_tz(as_datetime(time_stamp,tz="UTC"), tzone = "America/New_York"),unit = "hour")][
        #_[, `Date_Time(ET)` := with_tz(as_datetime(time_stamp,tz="UTC"), tzone = "America/New_York")][
                 # 센서별, 시간별 평균 계산
                , .(
                    humidity = mean(humidity, na.rm = TRUE),
                    voc = mean(voc, na.rm = TRUE),
                    `pm2.5_atm` = mean(`pm2.5_atm`, na.rm = TRUE) 
                    )
                , by = .(sensor_index, `Date_Time(ET)`)][
            ,c("sensor_index","pm2.5_atm"):=list(as.numeric(sensor_index),calibrate_pm25(`pm2.5_atm`,humidity))]|>
        _[participants, on = c(sensor_index = "sensor index")]
    
    

    target$ZCTA5CE10 <- as.character(target$ZCTA5CE10)
    
    
    
    # 2) 시간 범위 필터
    
    # init values
    #end_time <- now(tzone = "UTC")#dbGetQuery(con, "SELECT MAX(time_stamp) FROM \"Purple_Air\"")[[1]]#as.numeric(as_datetime(max(as.numeric(database$time_stamp))),tz = "America/New_York")

    

    
    filtered_data <- reactive({
        req(input$end_time_inp)
        
        end_time_m   <- (as_date(input$end_time_inp, tz = "America/New_York")+days(1))
        start_time_m <- (as_date(input$end_time_inp, tz = "America/New_York"))
        
        database[`Date_Time(ET)` >= start_time_m & `Date_Time(ET)` <end_time_m]
    })
    
    
    filtered_data_barplot<- reactive({
        req(input$end_time_inp)
        
        end_time_b   <- (as_date(input$end_time_inp, tz = "America/New_York")+days(1))
        start_time_b <- (as_date(input$end_time_inp, tz = "America/New_York")-days(6))
        
        database[`Date_Time(ET)` >= start_time_b & `Date_Time(ET)` < end_time_b]
    })
    
    
    
    # 3) ZIP별 평균 계산
    zipgroup <- reactive({
        fd <- filtered_data()
        if (nrow(fd) == 0) {
            return(data.table(zipcode = character(), pm2.5_atm = numeric(), voc = numeric()))
        }
        fd[, .(
            pm2.5_atm = mean(pm2.5_atm, na.rm = TRUE),
            voc       = mean(voc, na.rm = TRUE)
        ), by = zipcode]

    })
    
    # 4) 지도용 sf와 ZIP별 평균 병합
    target_attr <- reactive({
        tg <- copy(target)
        zg <- zipgroup()
        if ("zipcode" %in% names(zg)) {
            zg[, zipcode := as.character(zipcode)]
        }
        merge(tg, zg, by.x = "ZCTA5CE10", by.y = "zipcode", all.x = TRUE)
    })
    remove_modal_spinner()
    
    
    # 5) 지도 출력
    output$map <- renderLeaflet({
        ta <- target_attr()
        val_vec <- ta[[input$color_theme]]
        
        # 1. 커스텀 색상 함수 정의
        get_custom_pal <- function(theme) {
            function(x) {
                if (theme == "pm2.5_atm") {
                    ifelse(x <= 12, "#73C557",       # green
                           ifelse(x <= 55.4, "#FA9857", "#FA4662"))  # yellow / red
                } else if (theme == "voc") {
                    ifelse(x <= 300, "#73C557",
                           ifelse(x <= 500, "#FA9857", "#FA4662"))
                } else {
                    # fallback
                    rep("gray", length(x))
                }
            }
        }
        
        my_pal <- get_custom_pal(input$color_theme)
        
        # 2. leaflet 시각화
        leaflet(ta) %>%
            addTiles() %>%
            addPolygons(
                layerId = ~ZCTA5CE10,
                fillColor = ~my_pal(ta[[input$color_theme]]),
                color = "black",
                weight = 1,
                fillOpacity = 0.7,
                popup = ~paste(
                    "Zip Code:", ZCTA5CE10, "<br>",
                    "PM2.5:", ifelse(is.na(pm2.5_atm), "No data", round(pm2.5_atm, 1)), " µg/m³<br>",
                    "VOC:", ifelse(is.na(voc), "No data", round(voc, 1)), " ppb"
                )
            ) %>%
            # 3. 수동 범례 추가
            addLegendCustom(
                theme = input$color_theme,
                position = "bottomright"
            )
    })
    
    
    # 6) 지도 클릭 -> 선택된 ZIP 저장
    selected_region <- reactiveVal(NULL)
    observeEvent(input$map_shape_click, {
        click <- input$map_shape_click
        if (!is.null(click$id)) {
            selected_region(click$id)
        }
    })
    
    # 7) PM2.5 게이지 (0-12: green, 12-55.4: yellow, >55.4: red)
    output$pm25_gauge <- renderGauge({
        region_id <- selected_region()
        sel <- zipgroup()[zipcode %in% region_id]
        val <- if (nrow(sel) > 0) sel$pm2.5_atm else "NaN"
        gauge(val, min = 0, max = 100, symbol = " µg/m³",
              gaugeSectors(success = c(0, 12), warning = c(12, 55.4), danger = c(55.4, 100)))
    })
    
    output$pm25_avg_gauge <- renderGauge({
        fd <- filtered_data()
        val <- if(nrow(fd) > 0) mean(fd$pm2.5_atm, na.rm = TRUE) else "NaN"
        gauge(val, min = 0, max = 100, symbol = " µg/m³",
              gaugeSectors(success = c(0, 12), warning = c(12, 55.4), danger = c(55.4, 100)))
    })
    
    # 8) VOC 게이지 (0-300: green, 300-500: yellow, >500: red)
    output$voc_gauge <- renderGauge({
        region_id <- selected_region()
        sel <- zipgroup()[zipcode %in% region_id]
        val <- if (nrow(sel) > 0) sel$voc else "NaN"
        gauge(val, min = 0, max = 1000, symbol = " ppb",
              gaugeSectors(success = c(0, 300), warning = c(300, 500), danger = c(500, 1000)))
    })
    
    output$voc_avg_gauge <- renderGauge({
        fd <- filtered_data()
        val <- if(nrow(fd) > 0) mean(fd$voc, na.rm = TRUE) else "NaN"
        gauge(val, min = 0, max = 1000, symbol = " ppb",
              gaugeSectors(success = c(0, 300), warning = c(300, 500), danger = c(500, 1000)))
    })
    
    # 9) PM2.5 Color Ramp
    output$pm25_color_ramp <- renderPlot({
        par(mar = c(2, 1, 1, 1))
        plot.new()
        plot.window(xlim = c(0, 100), ylim = c(0, 1))
        rect(0,     0, 12,   1, col = "#73C557", border = NA)  # green
        rect(12,    0, 55.4, 1, col = "#FA9857", border = NA)  # yellow
        rect(55.4,  0, 100,  1, col = "#FA4662", border = NA)  # red
        axis(1, at = c(0, 12, 55.4, 100), labels = c("0", "12", "55.4", "100"), cex.axis = 0.8)
    })
    
    # 10) VOC Color Ramp
    output$voc_color_ramp <- renderPlot({
        par(mar = c(2, 1, 1, 1))
        plot.new()
        plot.window(xlim = c(0, 1000), ylim = c(0, 1))
        rect(0,   0, 300, 1, col = "#73C557", border = NA)  # green
        rect(300, 0, 500, 1, col = "#FA9857", border = NA)  # yellow
        rect(500, 0, 1000,1, col = "#FA4662", border = NA)   # red
        axis(1, at = c(0, 300, 500, 1000), labels = c("0", "300", "500", "1000"), cex.axis = 0.8)
    })
    

    # 11) 컨트롤 박스 연결
    
    end_time <- max(database$`Date_Time(ET)`,na.rm = T) |>
        as_datetime(tz = "America/New_York") |> floor_date("day")
    
    output$controls_box <- renderUI({
        box(
            title = "Controls", 
            status = "primary", 
            solidHeader = TRUE,
            width = NULL,
            height = "calc(100vh - 485px)",
            
            dateInput("end_time_inp", 
                      label = "Average from the day before:",
                      format = "MM-dd-yyyy",
                      max = end_time,
                      value = end_time
                      ),
            
            radioButtons(
                inputId = "color_theme",
                label = "Map Theme:",
                choices = c("PM2.5" = "pm2.5_atm", "VOC" = "voc"),
                selected = "pm2.5_atm",
                inline = T
            )
        )
    })
    # 12) 데이터 테이블 (ET Time, Zipcode, PM2.5, VOC)
    output$bar_plot <- renderPlotly({
        req(input$color_theme)
        region_id <- selected_region()
        fd <- filtered_data_barplot()
        
        # 날짜 추출
        fd[, date_only := as_date(`Date_Time(ET)`, tz = "America/New_York"    )]
        
        print(table(fd$date_only))
        
        col_selected <- input$color_theme
        label <- if (col_selected == "pm2.5_atm") "PM2.5" else "VOC"
        
        
        # 전체 데이터의 날짜별 평균
        total_avg <- fd[, .(
            value = mean(get(col_selected), na.rm = TRUE)
        ), by = date_only]
        total_avg[, group := "City-wide"]
        
        
        
        # 선택한 ZIP의 날짜별 평균
        zip_avg <- fd[zipcode %in% region_id, .(
            value = mean(get(col_selected), na.rm = TRUE)
        ), by = date_only]
        
        
        
        if (!is.null(region_id) && length(region_id) > 0 && dim(zip_avg)[1]>0) {
            
            zip_avg[, group := paste("ZIP", region_id)]
            
            # 합치기
            plot_dt <- rbind(zip_avg, total_avg)
            
            # 그래프
            plot_ly(
                data = plot_dt,
                x = ~date_only,
                y = ~value,
                color = ~group,
                colors = c("skyblue","tomato"),
                type = "bar"
            ) |>
                layout(
                    title = paste("Selected ZIP", 
                                  #region_id,
                                  "vs City-wide"),
                    xaxis = list(title = "Date"),
                    yaxis = list(title = paste(label, "Average")),
                    barmode = "group"
                )
            
            
        } else {
            #zip_avg <- data.table(date_only = as.Date(character()), value = numeric(), group = character())
            plot_dt <- total_avg
            
            
            # 그래프
            plot_ly(
                data = plot_dt,
                x = ~date_only,
                y = ~value,
                #color = ~group,
                type = "bar"
            ) |>
                layout(
                    title = "City-wide",
                    xaxis = list(title = "Date"),
                    yaxis = list(title = paste(label, "Average")),
                    barmode = "group"
                )
            
        }

        

    })
    
    
    
    
    session$onSessionEnded(function() {
        DBI::dbDisconnect(con)
    })
    
    
}

shinyApp(ui, server)
#rsconnect::deployApp(appDir = getwd(),appName ="23_dashboardubcleanair")