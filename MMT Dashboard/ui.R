# Define User Interface
ui <- dashboardPage(
  dashboardHeader(title = "Economic Health Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Plot", tabName = "plot_tab"),
      menuItem("Analysis", tabName = "analysis_tab")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "plot_tab",
        sidebarPanel(
          selectInput(
            "series_name", 
            "Select series:", 
            choices = NULL, 
            multiple = TRUE
          ),
          p("Hover over the lines for analytics. "),
          p("*Shaded areas = GDP recessions"),
          p("Data source: FRED")
        ),
        mainPanel(
          plotlyOutput("plot", height = "600px")
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
                       width = 4, status = "primary", solidHeader = TRUE,
                       selectInput("lag_series1", "Series 1", choices = NULL),
                       selectInput("lag_series2", "Series 2", choices = NULL),
                       sliderInput("lag_max", "Max lag (months)", min = 1, max = 24, value = 12)
                     ),
                     box(
                       width = 8, status = "info", solidHeader = TRUE,
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
    )
  )
)