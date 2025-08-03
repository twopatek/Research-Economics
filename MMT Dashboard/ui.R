# ---- UI ----
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Economic Health Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about_tab",   icon = icon("info")),
      menuItem("Budget Watch", tabName = "budget_tab", icon = icon("dollar-sign")),
      menuItem("Scaling Plot", tabName = "plot_tab", icon = icon("chart-line")),
      menuItem("Analysis", tabName = "analysis_tab", icon = icon("calculator"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "about_tab",
        fluidRow(
          column(
            width = 12,
            box(
              title = "Welcome to the Economic Health Dashboard",
              status = "info", 
              solidHeader = TRUE, 
              width = 12,
              p("This Shiny app pulls a selection of key U.S. economic time series from FRED and lets you:"),
              htmltools::tags$ul(
                tags$li("Compare levels or normalized changes across any combination of series."),
                tags$li("Zoom in on arbitrary date ranges or use one-click quick-select buttons (1 mo, 3 mo, etc.)."),
                tags$li("Explore lead–lag relationships between any two series (e.g. does X lead Y?).")
              ),
              p("**Why this app?**
                I built it to give a lightweight, interactive way to track and compare
                important fiscal, monetary, and labor indicators without installing R or sifting through FRED’s website."),
              p("**Data source disclaimer:**
                This app is not affiliated with the Federal Reserve or FRED®.
                It simply uses FRED’s public API to fetch time series data for analysis.")
            )
          )
        )
      ),
      tabItem(
        tabName = "budget_tab",
        fluidRow(
          column(
            width = 3,
            box(
              width = 12,
              title = "Budget Year",
              solidHeader = TRUE,
              status = "info",
              pickerInput(
                inputId = "budget_year",
                label = "Budget Year:",
                choices = budget_year_choices,
                selected = first(budget_year_choices),
                multiple = FALSE
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            solidHeader = TRUE,
            status = "info",
            tabsetPanel(
              id = "cbo_budget",
              tabPanel(
                title = "Sankey Flow Chart",
                fluidRow(
                  column(
                    width = 12,
                    box(
                      width = 12,
                      title = "Receipts",
                      solidHeader = TRUE,
                      status = "info",
                      sankeyNetworkOutput("cbo_budget_receipts_sankey_plot")
                    )
                  ),
                  column(
                    width = 12,
                    box(
                      width = 12,
                      title = "Outlays",
                      solidHeader = TRUE,
                      status = "info",
                      sankeyNetworkOutput("cbo_budget_outlays_sankey_plot")
                    )
                  )
                )
              ),
              tabPanel(
                title = "Budget Data Guide",
                fluidRow(
                  column(
                    width = 12,
                    box(
                      width = 12,
                      title = "Guide",
                      status = "info",
                      solidHeader = TRUE,
                      DTOutput("budget_guide_table", height = "600px")
                    )
                  )
                )
            )
            )
          )
        )
      ),
      tabItem(
        tabName = "plot_tab",
        fluidRow(
          column(
            width = 12,
            box(
              width = 12,
              tabsetPanel(
                id = "plot_subtabs",
                tabPanel(
                  title = "Plot",
                  fluidRow(
                    column(
                      width = 3,
                      box(
                        title = "Series & Period Selection",
                        solidHeader = TRUE,
                        status = "info",
                        width = 12,
                        # existing pickerInput
                        pickerInput(
                          inputId = "series_name",
                          label = "Select series:",
                          choices = NULL,
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `live-search` = TRUE,
                            `selected-text-format` = "count > 3"
                          )
                        ),
                        # new date range input
                        dateRangeInput(
                          inputId = "date_range",
                          label = "Select date range:",
                          start  = NULL,  # will set in server
                          end    = NULL,
                          format = "yyyy-mm-dd"
                        ),
                        # quick-select buttons
                        fluidRow(
                          column(6, actionButton("btn_6m",    "6 mo")),
                          column(6, actionButton("btn_1y",    "1 yr")),
                          column(6, actionButton("btn_5y",    "5 yr")),
                          column(6, actionButton("btn_10y",   "10 yr")),
                          column(6, actionButton("btn_20y",   "20 yr")),
                          column(6, actionButton("btn_30y",   "30 yr")),
                          column(6, actionButton("btn_reset", "Reset Date Range"))
                        ),
                        p("Hover over the lines for analytics."),
                        p("*Shaded areas = GDP recessions"),
                        p("Data source: FRED")
                      )
                    ),
                    column(
                      width = 9,
                      box(
                        title = "Output",
                        solidHeader = TRUE,
                        status = "info",
                        width = 12,
                        plotlyOutput("plot", height = "600px")
                      )
                    )
                  )
                ),
                tabPanel(
                  title = "Guide",
                  fluidRow(
                    column(
                      width = 12,
                      box(
                        title = "Series Guide",
                        solidHeader = TRUE,
                        status = "info",
                        width = 12,
                        DTOutput("series_guide_table")
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "analysis_tab",
        # three side-by-side panels for each analysis
        tabsetPanel(
          ### 1) Lead–Lag Explorer ###
          tabPanel("Lead–Lag Explorer",
                   fluidRow(
                     box(
                       width = 4, 
                       status = "info", 
                       solidHeader = TRUE,
                       selectInput("lag_series1", "Series 1", choices = NULL),
                       selectInput("lag_series2", "Series 2", choices = NULL),
                       sliderInput("lag_max", "Max lead-lag (months)", min = 1, max = 24, value = 12),
                       p(
                         strong("How to use:"), br(),
                         "Select the series you think leads in Series 1 and the follower in Series 2 (order matters).", br(),
                         "Then move the “Max lag” slider to test how many months forward/back you want to explore.", br(),
                         "The chart shows correlation at each lead/lag, and the table lists the top 3 strongest relationships."
                       )
                     ),
                     box(
                       width = 8, 
                       status = "info", 
                       solidHeader = TRUE,
                       plotlyOutput("ccf_plot", height = "300px"),
                       tableOutput("lag_table")
                     )
                   )
          ),

          ### 2) Recession Impact (placeholder) ###
          tabPanel("Recession Impact",
                   h4("Coming soon…")
          ),

          ### 3) Volatility & Alerts (placeholder) ###
          tabPanel("Volatility & Alerts",
                   h4("Coming soon…")
          )
        )
      )
    ),
    htmltools::tags$footer(
      HTML("Data sourced from <a href='https://fred.stlouisfed.org/' target='_blank'>FRED</a>.
           This application is independent and not endorsed by the Federal Reserve."),
      style = "
        position:fixed; bottom:0; width:100%; padding:10px;
        background: #f8f9fa; text-align:center; font-size:80%;"
    )
  )
)