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
              title = "About This Research",
              status = "info", 
              solidHeader = TRUE, 
              width = 12,
              p("This application is an ongoing research project built in R using the Shiny framework to publish interactive software.  
                It brings together macroeconomics and statistics to explore the implications of Modern Monetary Theory (MMT) in our financial system."),
              
              p(strong("Key Data Series:")),
              htmltools::tags$ul(
                tags$li("Federal Receipts & Outlays (Sankey diagrams illustrating cash flows)"),
                tags$li("M2 Money Stock & Real M2 (measuring private liquidity)"),
                tags$li("Treasury Yield Curve (10-year minus 2-year spreads)"),
                tags$li("Inflation Metrics (CPI, Core CPI, PCE, Core PCE)"),
                tags$li("Labor Market Indicators (Nonfarm Payrolls, Unemployment Rate)")
              ),
              
              p(strong("App Features:")),
              htmltools::tags$ul(
                tags$li("Interactive time-series charts with normalization and percent-change overlays"),
                tags$li("Dynamic date-range selection with quick-select buttons (6 mo, 1 yr, 5 yr, etc.)"),
                tags$li("Lead–lag correlation explorer to test directional relationships between any two series"),
                tags$li("Searchable series guide with definitions and metadata")
              ),
              
              p("Use this tool to deepen your understanding of how government finance operations interact with monetary aggregates and real-world economic indicators under an MMT framework.")
            )
          )
        )
      ),
      tabItem(
        tabName = "budget_tab",
        fluidRow(
          column(
            width = 12,
            solidHeader = TRUE,
            status = "info",
            tabsetPanel(
              id = "cbo_budget",
              
              # --- Historical Budget Subtab ---
              tabPanel(
                title = "Historical",
                fluidRow(
                  column(
                    width = 3,
                    box(
                      width = 12,
                      title = "Select Year",
                      solidHeader = TRUE,
                      status = "info",
                      pickerInput(
                        inputId = "historical_budget_year",
                        label = "Budget Year:",
                        choices = historical_budget_year_choices,
                        selected = first(historical_budget_year_choices),
                        multiple = FALSE
                      )
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    box(
                      width = 12,
                      title = "Receipts",
                      solidHeader = TRUE,
                      status = "info",
                      sankeyNetworkOutput("historical_receipts_sankey_plot")
                    )
                  ),
                  column(
                    width = 12,
                    box(
                      width = 12,
                      title = "Outlays",
                      solidHeader = TRUE,
                      status = "info",
                      sankeyNetworkOutput("historical_outlays_sankey_plot")
                    )
                  )
                )
              ),
              
              # --- Projections Budget Subtab ---
              tabPanel(
                title = "Projections",
                fluidRow(
                  column(
                    width = 3,
                    box(
                      width = 12,
                      title = "Select Year",
                      solidHeader = TRUE,
                      status = "info",
                      pickerInput(
                        inputId = "projection_budget_year",
                        label = "Projection Year:",
                        choices = projection_budget_year_choices,
                        selected = first(projection_budget_year_choices),
                        multiple = FALSE
                      )
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    box(
                      width = 12,
                      title = "Receipts",
                      solidHeader = TRUE,
                      status = "info",
                      sankeyNetworkOutput("projection_receipts_sankey_plot")
                    )
                  ),
                  column(
                    width = 12,
                    box(
                      width = 12,
                      title = "Outlays",
                      solidHeader = TRUE,
                      status = "info",
                      sankeyNetworkOutput("projection_outlays_sankey_plot")
                    )
                  )
                )
              ),
              
              # --- Budget Guide Subtab ---
              tabPanel(
                title = "Budget Data Guide",
                fluidRow(
                  column(
                    width = 12,
                    box(
                      width = 12,
                      title = "Guide",
                      solidHeader = TRUE,
                      status = "info",
                      DTOutput("budget_guide_table")
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
      HTML("Data sourced from <a href='https://fred.stlouisfed.org/' target='_blank'>FRED</a> 
      and <a href='https://www.cbo.gov/data/budget-economic-data/' target='_blank'>CBO</a>.
           This application is independent and not endorsed by the Federal Reserve or Congressional Budget Office."),
      style = "
        position:fixed; bottom:0; width:100%; padding:10px;
        background: #f8f9fa; text-align:center; font-size:80%;"
    )
  )
)