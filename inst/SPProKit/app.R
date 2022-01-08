## app.R ##
library(shiny)
library(flexdashboard)
library(bs4Dash)
# library(shinydashboard)
# library(shinydashboardPlus)
library(shinyWidgets)
library(tidyverse)
library(glue)
library(here)
library(duckdb)
library(DBI)
library(clock)

src_tz <- "UTC"
tz <- Sys.timezone()

duckdb_path <- "~/SProKit-data/Selectronics_data.duckdb"

hfdata_levels <- c("load_w", "log_load_w", "battery_w",
                   "solarinverter_w", "log_solarinverter_w", "battery_soc", "grid_w")
hfdata_labels <- c("Load (W)", "Log Load (logW)","Battery usage (W)",
                   "Solar inverter (W)", "Log Solar inverter (logW)", "State of charge (%)",
                   "Gen/grid input (W)")

getdata_levels <- c("load_w", "log_load_w", "battery_w",
                   "load_wh_today", "log_load_wh_today", "battery_soc", "grid_w")
getdata_labels <- c("Load (W)", "Log Load (logW)","Battery usage (W)",
                   "Daily total load (kWh)", "Log daily total load (logkWh)", "State of charge (%)",
                   "Gen/grid input (W)")

last_update <- clock::date_now(zone=tz)

ui <- dashboardPage(
  header = dashboardHeader(title="SPProKit"),
## Sidebar content
  sidebar = dashboardSidebar(
    fixed = TRUE,
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Recent", tabName = "recent", icon = icon("stats", lib="glyphicon")),
      menuItem("Historical", tabName = "historical", icon = icon("stats", lib="glyphicon")),
      menuItem("Messages", tabName = "messages", icon = icon("bullhorn", lib="glyphicon"))
    ),
      hr(),
      h4("Current state", align = "center"),
      gaugeOutput("soc_gauge", height="auto", width="auto"),
      gaugeOutput("load_gauge", height="auto", width="auto"),
      gaugeOutput("solar_gauge", height="auto", width="auto"),
      gaugeOutput("battery_charge_gauge", height="auto", width="auto"),
      gaugeOutput("battery_discharge_gauge", height="auto", width="auto"),
            # A static valueBox
      customArea = tagList(
          htmlOutput("LastUpdate")
      )
  ),

## Right sidebar
     controlbar = dashboardControlbar(
            title = "Skin selector and stop button",
            skinSelector(),
            actionBttn("stop_button",
                       label="Stop app",
                       color="warning",
                       icon = icon("stop"))

     ),
## Body content
 body = dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "overview",
        fluidRow(
          box(plotOutput("ShortTermPlot"),
              title = "Recent data",
              width=12,
              sidebar = boxSidebar(
                            id = "short_term_box_sidebar",
                            width = 25,
                            sliderInput("shortTermRange",
                                     width="90%",
                                     label = "Time range (hours ago)",
                                     min = -24, max = 1,
                                     value = c(-24, 0)),

                            checkboxInput("logarithmic", "Show log of load and solar production"),
                            checkboxInput("show_grid", "Show grid/gen flow")
                        )
              )
        ),
        fluidRow(
          box(plotOutput("RecentSummaryPlot"),
              title = "Historical data",
              width=12,
              sidebar = boxSidebar(
                                    id = "historical_box_sidebar",
                                    width = 25,
                                    sliderInput("recentSummaryRange",
                                             width="90%",
                                             label = "Date range (days ago)",
                                             min = -31, max = 1,
                                             value = c(-28, 0)),
                                    checkboxInput("logarithmic_summary",
                                                  "Show log of load and daily total load")
                                    )
        )
        )
      ),

      tabItem(tabName = "recent",
        h2("Widgets tab content")
      ),

      # Second tab content
      tabItem(tabName = "history",
        h2("Widgets tab content")
      ),

      # Third tab content
      tabItem(tabName = "messages",
        h2("Messages and announcements")
      )
    )
 )
)

server <- function(input, output, session) {
    observe({

          output$soc_gauge = renderGauge({
            invalidateLater(15000, session)
            con_duck <- dbConnect(duckdb::duckdb(), duckdb_path, read_only=TRUE)
            hfdata <- tbl(con_duck, 'hfdata')
            batt_soc <- hfdata %>%
                filter(timestamp == max(timestamp)) %>%
                select(battery_soc) %>%
                collect() %>%
                pull()
            rm(hfdata)
            duckdb::dbDisconnect(con_duck, shutdown=TRUE)
            gauge(round(batt_soc),
                  min = 0,
                  max = 100,
                  symbol = "%",
                  label = "Battery SoC",
                  sectors = gaugeSectors(success = c(40, 100),
                                         warning = c(20, 40),
                                         danger = c(0, 20)))
              })

          output$load_gauge = renderGauge({
            invalidateLater(15000, session)
            con_duck <- dbConnect(duckdb::duckdb(), duckdb_path, read_only=TRUE)
            hfdata <- tbl(con_duck, 'hfdata')
            current_load <- hfdata %>%
                filter(timestamp == max(timestamp)) %>%
                select(load_w) %>%
                collect() %>%
                pull()
            rm(hfdata)
            duckdb::dbDisconnect(con_duck, shutdown=TRUE)
            gauge(round(current_load),
                  min = 0,
                  max = 10000,
                  symbol = "W",
                  label = "Load",
                  sectors = gaugeSectors(success = c(0, 5000),
                                         warning = c(5000, 7500),
                                         danger = c(7500, 10000)))
              })

          output$solar_gauge = renderGauge({
            invalidateLater(15000, session)
            con_duck <- dbConnect(duckdb::duckdb(), duckdb_path, read_only=TRUE)
            hfdata <- tbl(con_duck, 'hfdata')
            current_solar <- hfdata %>%
                filter(timestamp == max(timestamp)) %>%
                select(solarinverter_w) %>%
                collect() %>%
                pull()
            rm(hfdata)
            duckdb::dbDisconnect(con_duck, shutdown=TRUE)
            gauge(round(current_solar),
                  min = 0,
                  max = 10000,
                  symbol = "W",
                  label = "Solar production",
                  sectors = gaugeSectors(success = c(40, 100),
                                         warning = c(8200, 9000),
                                         danger = c(9000, 10000)))
              })

          output$battery_charge_gauge = renderGauge({
            invalidateLater(15000, session)
            con_duck <- dbConnect(duckdb::duckdb(), duckdb_path, read_only=TRUE)
            hfdata <- tbl(con_duck, 'hfdata')
            battery_charge_rate_w <- hfdata %>%
                filter(timestamp == max(timestamp)) %>%
                mutate(battery_charge_rate_w = if_else(battery_w <= 0,
                                                     -battery_w,
                                                     0)) %>%
                select(battery_charge_rate_w) %>%
                collect() %>%
                pull()
            rm(hfdata)
            duckdb::dbDisconnect(con_duck, shutdown=TRUE)
            gauge(round(battery_charge_rate_w),
                  min = 0,
                  max = 10000,
                  symbol = "W",
                  label = "Battery charge rate",
                  sectors = gaugeSectors(success = c(0,10000)))
              })

          output$battery_discharge_gauge = renderGauge({
            invalidateLater(15000, session)
            con_duck <- dbConnect(duckdb::duckdb(), duckdb_path, read_only=TRUE)
            hfdata <- tbl(con_duck, 'hfdata')
            battery_discharge_rate_w <- hfdata %>%
                filter(timestamp == max(timestamp)) %>%
                mutate(battery_discharge_rate_w = if_else(battery_w >= 0,
                                                     battery_w,
                                                     0)) %>%
                select(battery_discharge_rate_w) %>%
                collect() %>%
                pull()
            rm(hfdata)
            duckdb::dbDisconnect(con_duck, shutdown=TRUE)
            gauge(round(battery_discharge_rate_w),
                  min = 0,
                  max = 10000,
                  symbol = "W",
                  label = "Battery discharge rate",
                  sectors = gaugeSectors(danger = c(7500, 10000),
                                         warning = c(0,7500)))
              })

        output$LastUpdate <- renderUI({
            invalidateLater(1000, session)
            h6(glue("Data refreshed ",
                 as.numeric(difftime(clock::date_now(zone=tz),last_update,units="secs")),
                 " seconds ago."), align="center")
        })

        output$ShortTermPlot <- renderPlot({
            invalidateLater(15000, session)
            right_now <- clock::date_now(tz)
            earliest <- clock::add_hours(right_now, input$shortTermRange[1])
            latest <- clock::add_hours(right_now, input$shortTermRange[2])
            con_duck <- dbConnect(duckdb::duckdb(), duckdb_path, read_only=TRUE)
            hfdata <- tbl(con_duck, 'hfdata')
            hf <- hfdata %>%
                select(timestamp, load_w, battery_w, solarinverter_w, battery_soc, grid_w) %>%
                collect()
            rm(hfdata)
            duckdb::dbDisconnect(con_duck, shutdown=TRUE)
            display_hf <- hf %>%
                mutate(timestamp = clock::date_set_zone(timestamp, zone = tz)) %>%
                mutate(log_load_w = log10(load_w),
                       log_solarinverter_w = log10(solarinverter_w)) %>%
                pivot_longer(!timestamp,
                                names_to = "Parameter",
                                values_to = "Value") %>%
                mutate(Parameter = factor(Parameter,
                                             levels = hfdata_levels,
                                             labels = hfdata_labels)) %>%
                filter(timestamp <= latest & timestamp >= earliest)
            last_update <<- display_hf %>%
                filter(timestamp == max(timestamp)) %>%
                select(timestamp) %>%
                slice(1) %>%
                pull()

            if (!input$logarithmic) {
            display_hf <- display_hf %>%
                filter(Parameter != "Log Load (logW)",
                       Parameter != "Log Solar inverter (logW)")
            }

            if (!input$show_grid) {
            display_hf <- display_hf %>%
                filter(Parameter != "Gen/grid input (W)")
            }

            display_hf %>%
                ggplot(aes(x=timestamp, y=Value)) +
                geom_point(aes(colour=Parameter)) +
                geom_line() +
                facet_grid(Parameter~., scales = "free_y",
                           labeller = labeller(Parameter = label_wrap_gen(10))) +
                theme(legend.position = "none",
                      text = element_text(size = 15)) +
                labs(x="Time")

        })

        output$RecentSummaryPlot <- renderPlot({
            invalidateLater(600000, session)
            right_now <- clock::date_now(tz)
            earliest <- clock::add_days(right_now, input$recentSummaryRange[1])
            latest <- clock::add_days(right_now, input$recentSummaryRange[2])
            con_duck <- dbConnect(duckdb::duckdb(), duckdb_path, read_only=TRUE)
            getdata <- tbl(con_duck, 'getdata')
            gd <- getdata %>%
                select(ts_epoch, load_w, load_wh_today, battery_w, battery_soc, grid_w) %>%
                collect()
            rm(getdata)
            duckdb::dbDisconnect(con_duck, shutdown=TRUE)
            display_gd <- gd %>%
                mutate(ts_epoch = clock::date_set_zone(ts_epoch, zone = tz)) %>%
                mutate(log_load_w = log10(load_w),
                       log_load_wh_today = log10(load_wh_today)) %>%
                pivot_longer(!ts_epoch,
                                names_to = "Parameter",
                                values_to = "Value") %>%
                mutate(Parameter = factor(Parameter,
                                             levels = getdata_levels,
                                             labels = getdata_labels)) %>%
                filter(ts_epoch <= latest & ts_epoch >= earliest)
            if (!input$logarithmic_summary) {
            display_gd <- display_gd %>%
                filter(Parameter != "Log Load (logW)",
                       Parameter != "Log daily total load (logkWh)")
            }
            display_gd %>%
                ggplot(aes(x=ts_epoch, y=Value)) +
                geom_point(aes(colour=Parameter)) +
                geom_line() +
                facet_grid(Parameter~., scales = "free_y",
                           labeller = labeller(Parameter = label_wrap_gen(10))) +
                theme(legend.position = "none",
                      text = element_text(size = 15)) +
                labs(x="10 minute epoch data/time")

        })

    })

    observeEvent(input$stop_button, {
        stopApp("App stopped!")
    })
}

shinyApp(ui, server)
