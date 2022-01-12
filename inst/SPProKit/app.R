## app.R ##
library(shiny)
library(flexdashboard)
library(bs4Dash)
library(shinyWidgets)
library(dygraphs)
library(tidyverse)
library(tidygraph)
library(ggridges)
library(visNetwork)
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
hfdata_labels <- c("Load (W)", "Log Load (logW)","Battery charge/ discharge rate (W)",
                   "Solar inverter output (W)", "Log Solar inverter output (logW)", "Battery State of charge (%)",
                   "Gen/grid input (W)")

getdata_levels <- c("load_w", "log_load_w", "battery_w",
                   "load_wh_today", "log_load_wh_today", "battery_soc", "grid_w")
getdata_labels <- c("Load (W)", "Log Load (logW)","Battery charge/ discharge rate (W)",
                   "Daily total load (kWh)", "Log daily total load (logkWh)", "Battery State of charge (%)",
                   "Gen/grid input (W)")

last_update <- clock::date_now(zone=tz)

ui <- dashboardPage(

  dark = FALSE,

  header = dashboardHeader(title=tags$img(src='SPPro21_icon_small.png',
                                          style =
                                "display: block; margin-left: auto; margin-right: auto;padding-top:10px;")),

  sidebar = dashboardSidebar(
    fixed = TRUE,
    skin = "light",
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard", lib="glyphicon")),
      menuItem("Historical", tabName = "historical", icon = icon("chart-line")),
      menuItem("Analysis", tabName = "analysis", icon = icon("calculator")),
      menuItem("Messages", tabName = "messages", icon = icon("bullhorn")),
      menuItem("Log", tabName = "log", icon = icon("book"))
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
          # htmlOutput("LastUpdate")
      )
  ),

  controlbar = dashboardControlbar(
            title = "Skin selector and stop button",
            collapsed = TRUE,
            pinned = TRUE,
            div(class = "p-3", skinSelector()),
            actionBttn("stop_button",
                       label="Stop app",
                       color="warning",
                       icon = icon("stop"))

     ),

 body = dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "overview",
        fluidRow(
          box(plotOutput("ShortTermPlot"),
              title = htmlOutput("LastUpdate"),
              width=12,
              sidebar = boxSidebar(
                            id = "short_term_box_sidebar",
                            width = 25,
                            sliderInput("shortTermRange",
                                     width="90%",
                                     label = "Time range (hours ago)",
                                     min = -24, max = 0,
                                     value = c(-24, 0)),

                            checkboxInput("logarithmic", "Show log of load and solar production"),
                            checkboxInput("show_grid", "Show grid/gen input/output")
                        )
              )
        ),
        fluidRow(
          box(plotOutput("RecentSummaryPlot"),
              title = "Historical 10 minute averages",
              width=9,
              sidebar = boxSidebar(
                                    id = "historical_box_sidebar",
                                    width = 25,
                                    sliderInput("recentSummaryRange",
                                             width="90%",
                                             label = "Date range (days ago)",
                                             min = -31, max = 0,
                                             value = c(-28, 0)),
                                    checkboxInput("logarithmic_summary",
                                                  "Show log of load and daily total load")
                                    )
            ),

          box(
              visNetworkOutput("visNetworkPlot"),
              title = "Power flows",
              width=3)
        )
      ),

      tabItem(tabName = "analysis",
              fluidRow(
                  box(plotOutput("RidgePlot"),
                        title = "Time-of-day plot",
                        width=12,
                        height=1000,
                        sidebar = boxSidebar(
                            id = "ridge_plot_sidebar",
                            width = 25,
                            sliderInput("ridgePlotRange",
                                     width="90%",
                                     label = "Time range (days ago)",
                                     min = -30, max = 0,
                                     value = c(-20, 0))
                        )

                      )
              )
      ),

      tabItem(tabName = "historical",
        h2("Yet to be implemented")
      ),

      tabItem(tabName = "messages",
        h2("Messages and announcements")
      ),

      tabItem(tabName = "log",
        h2("Logs")
      )
    )
 )
)

server <- function(input, output, session) {

        gaugeData <- reactive({
            on.exit(invalidateLater(15000, session))
            con_duck <- dbConnect(duckdb::duckdb(), duckdb_path, read_only=TRUE)
            hfdata <- tbl(con_duck, 'hfdata')
            gauge_data <- hfdata %>%
                filter(timestamp == max(timestamp)) %>%
                mutate(battery_charge_rate_w = if_else(battery_w <= 0,
                                                     -battery_w,
                                                     0),
                       battery_discharge_rate_w = if_else(battery_w >= 0,
                                                     battery_w,
                                                     0)) %>%
                select(battery_soc, load_w, solarinverter_w,
                       battery_charge_rate_w, battery_discharge_rate_w, grid_w) %>%
                collect() %>%
                slice(1)
            rm(hfdata)
            duckdb::dbDisconnect(con_duck, shutdown=TRUE)
            gauge_data
                            })

          output$soc_gauge = renderGauge({
            gauge(round(gaugeData() %>% pull(battery_soc)),
                  min = 0,
                  max = 100,
                  symbol = "%",
                  label = "Battery SoC",
                  sectors = gaugeSectors(success = c(40, 100),
                                         warning = c(20, 40),
                                         danger = c(0, 20)))
              })

          output$load_gauge = renderGauge({
            gauge(round(gaugeData() %>% pull(load_w)),
                  min = 0,
                  max = 10000,
                  symbol = "W",
                  label = "Load",
                  sectors = gaugeSectors(success = c(0, 5000),
                                         warning = c(5000, 7500),
                                         danger = c(7500, 10000)))
              })

          output$solar_gauge = renderGauge({
            gauge(round(gaugeData() %>% pull(solarinverter_w)),
                  min = 0,
                  max = 10000,
                  symbol = "W",
                  label = "Solar production",
                  sectors = gaugeSectors(success = c(40, 100),
                                         warning = c(8200, 9000),
                                         danger = c(9000, 10000)))
              })

          output$battery_charge_gauge = renderGauge({
            gauge(round(gaugeData() %>% pull(battery_charge_rate_w)),
                  min = 0,
                  max = 10000,
                  symbol = "W",
                  label = "Battery charge rate",
                  sectors = gaugeSectors(success = c(0,10000)))
              })

          output$battery_discharge_gauge = renderGauge({
            gauge(round(gaugeData() %>% pull(battery_discharge_rate_w)),
                  min = 0,
                  max = 10000,
                  symbol = "W",
                  label = "Battery discharge rate",
                  sectors = gaugeSectors(danger = c(7500, 10000),
                                         warning = c(0,7500)))
              })

        output$LastUpdate <- renderUI({
            invalidateLater(1000, session)
            glue("Recent data -  refreshed approximately ",
                 as.numeric(difftime(clock::date_now(zone=tz),last_update,units="secs")),
                 " seconds ago.")
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
                mutate(battery_w = -battery_w) %>%
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
                       Parameter != "Log Solar inverter output (logW)")
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
                           labeller = labeller(Parameter = label_wrap_gen(8))) +
                theme_minimal() +
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
                mutate(battery_w = -battery_w) %>%
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
                           labeller = labeller(Parameter = label_wrap_gen(8))) +
                theme_minimal() +
                theme(legend.position = "none",
                      text = element_text(size = 15)) +
                labs(x="10 minute epoch date/time")

        })


        output$visNetworkPlot <- renderVisNetwork({

            bat_soc <- gaugeData() %>%
                        pull(battery_soc)
            load_w <- gaugeData() %>%
                        pull(load_w)
            solar_w <- gaugeData() %>%
                        pull(solarinverter_w)
            battery_charge_rate_w <- gaugeData() %>%
                        pull(battery_charge_rate_w)
            battery_discharge_rate_w <- gaugeData() %>%
                        pull(battery_discharge_rate_w)
            gen_w <- gaugeData() %>%
                        pull(grid_w)

            battery_label <- glue("Battery ", bat_soc, "%")

            nodes_tbl <- tibble::tribble(~id, ~label, ~group, ~level,
                                         1, "Generator",  "A", 2,
                                         2, "Selectronic", "B", 1,
                                         3, "Solar", "C", 2,
                                         4, battery_label, "D", 2,
                                         5, "Load", "E", 2,
                                         ) %>%
                mutate(shadow = TRUE)

            edges_tbl <- tibble::tribble(~from, ~to, ~width, ~length, ~label,
                    1, 2, log(gen_w,4), 150, glue(round(gen_w), "W"),
                    3, 2, log(solar_w,4), 150, glue(round(solar_w), "W"),
                    2, 4, log(battery_charge_rate_w,4), 150, glue(round(battery_charge_rate_w), "W"),
                    4, 2, log(battery_discharge_rate_w,4), 150, glue(round(battery_discharge_rate_w), "W"),
                    2, 5, log(load_w,4), 150, glue(round(load_w), "W")) %>%
                mutate(arrows = "to",
                       arrowStrikethrough = FALSE,
                       widthConstraint = 20,
                       shadow = TRUE)

            visNetwork(nodes_tbl, edges_tbl) %>% #, width = "100%", height="800px")
            visInteraction(dragNodes = FALSE, dragView = FALSE, zoomView = FALSE) %>%
              visGroups(groupname = "A", shape = "icon",
                        icon = list(face = "'Font Awesome 5 Free'", code = "f5e7", size=45)) %>%
              visGroups(groupname = "B", shape = "box") %>%
              visGroups(groupname = "C", shape = "icon",
                        icon = list(face = "'Font Awesome 5 Free'", code = "f185", size=45)) %>%
              visGroups(groupname = "D", shape = "icon",
                        icon = list(face = "'Font Awesome 5 Free'", code = "f5df", size=45)) %>%
              visGroups(groupname = "E", shape = "icon",
                        icon = list(face = "'Font Awesome 5 Free'", code = "f015", size=45)) %>%
            addFontAwesome(name = "font-awesome-visNetwork") %>%
            visLayout(randomSeed = 128, hierarchical = FALSE)

})

        output$RidgePlot <- renderPlot(width = 1000, height = 800, {
            right_now <- clock::date_now(tz)
            earliest <- clock::add_days(right_now, input$ridgePlotRange[1])
            latest <- clock::add_days(right_now, input$ridgePlotRange[2])
            con_duck <- dbConnect(duckdb::duckdb(), duckdb_path, read_only=TRUE)
            hfdata <- tbl(con_duck, 'hfdata')
            hf <- hfdata %>%
                select(timestamp, battery_soc) %>%
                collect()
            rm(hfdata)
            duckdb::dbDisconnect(con_duck, shutdown=TRUE)
            ridge_data <- hf %>%
                mutate(timestamp = clock::date_set_zone(timestamp, zone = tz)) %>%
                filter(timestamp >= earliest, timestamp <= latest) %>%
                mutate(datestamp = clock::as_date(timestamp)) %>%
                mutate(time_of_day = get_hour(timestamp) + get_minute(timestamp)/60) %>%
                # group_by(datestamp) %>%
                mutate(scaled_battery_soc = scales::rescale(battery_soc)) # %>%
                # ungroup()

            ridge_data %>%
                ggplot(aes(x=time_of_day, y = datestamp, group = datestamp,
                           height = scaled_battery_soc, fill=identity(scaled_battery_soc))) +
                geom_density_ridges_gradient(stat = "identity", scale = 0.9) +
                scale_fill_viridis_c(name = "Battery SoC", option = "C") +
                theme(text = element_text(size = 15)) +
                labs(y="Date", x="Time of day")
        })

    observeEvent(input$stop_button, {
        stopApp("App stopped!")
    })

}

shinyApp(ui, server)
