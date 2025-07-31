#load libraries 
library(tidyverse)
library(plotly)
library(eFRED)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(RColorBrewer)
library(readxl)
library(scales)

# set key to pull data from FRED
api_key <- "21489194ba838be7e47627eb82142f3a"
set_fred_key(api_key)

# define a list of series to load
series_list <- list(
  yield_curve            = list(value = "T10Y2YM"),
  interest_payments      = list(value = "A091RC1Q027SBEA"),
  
  # Fiscal aggregates
  total_debt             = list(value = "GFDEBTN"),    # Total public debt
  federal_deficit        = list(value = "FYFSD"),      # Federal surplus/deficit
  
  # Output
  gdp                    = list(value = "GDP"),        # Nominal GDP
  
  # Inflation measures
  cpi                    = list(value = "CPIAUCSL"),   # CPI (headline)
  core_cpi               = list(value = "CPILFESL"),   # CPI less food & energy
  pce_price              = list(value = "PCEPI"),      # PCE price index
  pce_ex_food_energy     = list(value = "PCEPILFE"),   # Core PCE
  
  # Money aggregates
  m2                     = list(value = "M2SL"),       # M2 money stock
  real_m2                = list(value = "M2REAL"),     # Real M2
  
  # Fed policy and balance sheet
  interest_rate          = list(value = "DFF"),
  fed_assets             = list(value = "WALCL")       # Fed total assets (QE)
)


# map function to generate series titles
series <- ""

get_series_info <- function(series) {
  df <- fred(series, all = FALSE)
  print(str(df))
  notes <- attr(df, "info")
}

series_info <- map_dfr(series_list, ~get_series_info(.))

# series_titles <- titles %>% 
#   select(id, title)

series_meta <- tibble(
  series_id   = names(series_list),
  series_code = map_chr(series_list, "value"),
  series_name = c(
    "10Y–2Y Yield Curve",
    "Federal Interest Payments",
    "Total Public Debt",
    "Federal Surplus/Deficit",
    "Nominal GDP",
    "Headline CPI",
    "Core CPI",
    "PCE Price Index",
    "Core PCE",
    "M2 Money Stock",
    "Real M2",
    "Effective Fed Funds Rate",
    "Fed Total Assets"
  )
)

df <- series_meta %>% 
  mutate(
    raw = map(
      series_code,
      ~ fred(series = .x, all = FALSE) %>%
        as_tibble()             
    )
  ) %>% 
  unnest(raw) %>%   
  group_by(series_id, series_name) %>%       
  arrange(date) %>% 
  mutate(
    value_norm = series / max(series, na.rm = TRUE),
    pct_change  = (series / lag(series) - 1) * 100
  ) %>% 
  ungroup() %>% 
  arrange(series_id)

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

# server
server <- function(input, output, session) {
  # populate the series selector once df is ready
  observe({
    updateSelectInput(
      session, 
      "series_name", 
      choices = sort(unique(df$series_name)),
      selected = unique(df$series_name)[1]
    )
  })
  
  # filter by series AND date‐range
  filtered_data <- reactive({
    req(input$series_name)
    df %>% 
      filter(
        series_name %in% input$series_name
        # date >= input$date_range[1],
        # date <= input$date_range[2]
      )
  })
  
  n_series <- length(unique(df$series_name))
  palette_hue <- hue_pal()(n_series)
  
  colors <- setNames(palette_hue, unique(df$series_name))
  
  output$plot <- renderPlotly({
    
    d <- filtered_data()
    
    plot_ly(d, x = ~date) %>%
      add_lines(
        y = ~value_norm, 
        name = ~series_name,
        text = ~paste0(
          "Series: ", series_name,
          "<br>Date: ", date,
          "<br>Value: ", series,
          "<br>%chg: ", pct_change, "%"
        ),
        hoverinfo = "text",
        color = ~series_name
      ) %>%
      layout(
        title = "Historical Data of Consumer Financial Health",
        xaxis = list(rangeslider = list(type = "period")),
        yaxis = list(title = "Normalized Value"),
        showlegend = FALSE,
        legend = list(x = 0, y = -0.2, font = list(size = 10)),
        plot_bgcolor = "#f7f7f7",
        title = list(x = 0.5),
        colorway = colors,
        shapes = list(
          list(
            type = "rect",
            x0 = as.Date("1979-10-01"),
            x1 = as.Date("1979-10-31"),
            y0 = -1,
            y1 = 1,
            fillcolor = "blue",
            opacity = 0.1
          ),
          list(
            type = "rect",
            x0 = as.Date("1981-04-01"),
            x1 = as.Date("1982-04-01"),
            y0 = -1,
            y1 = 1,
            fillcolor = "blue",
            opacity = 0.1
          ),
          list(
            type = "rect",
            x0 = as.Date("1989-10-01"),
            x1 = as.Date("1991-01-01"),
            y0 = -1,
            y1 = 1,
            fillcolor = "blue",
            opacity = 0.1
          ),
          list(
            type = "rect",
            x0 = as.Date("2001-01-01"),
            x1 = as.Date("2001-07-01"),
            y0 = -1,
            y1 = 1,
            fillcolor = "blue",
            opacity = 0.1
          ),
          list(
            type = "rect",
            x0 = as.Date("2007-10-01"),
            x1 = as.Date("2009-04-01"),
            y0 = -1,
            y1 = 1,
            fillcolor = "blue",
            opacity = 0.1
          ),
          list(
            type = "rect",
            x0 = as.Date("2020-01-01"),
            x1 = as.Date("2020-04-01"),
            y0 = -1,
            y1 = 1,
            fillcolor = "blue",
            opacity = 0.1
          )
        ))
  
  })
  
  # populate the two series selectors
  observe({
    series_names <- sort(unique(df$series_name))
    updateSelectInput(session, "lag_series1", choices = series_names, selected = series_names[1])
    updateSelectInput(session, "lag_series2", choices = series_names, selected = series_names[2])
  })
  
  # compute CCF for the two selected series
  ccf_data <- reactive({
    req(input$lag_series1, input$lag_series2)
    y1 <- df %>% 
      filter(series_name == input$lag_series1) %>% 
      arrange(date) %>% 
      pull(value_norm)
    y2 <- df %>% 
      filter(series_name == input$lag_series2) %>% 
      arrange(date) %>% 
      pull(value_norm)
    
    # cross-correlation
    cc <- stats::ccf(y1, y2, plot = FALSE, lag.max = input$lag_max)
    tibble(
      lag = as.integer(cc$lag),
      acf = as.numeric(cc$acf)
    )
  })
  
  # CCF plot
  output$ccf_plot <- renderPlotly({
    d <- ccf_data()
    plot_ly(d, x = ~lag, y = ~acf, type = "scatter", mode = "lines+markers") %>%
      layout(
        title = paste("CCF:", input$lag_series1, "vs", input$lag_series2),
        xaxis = list(title = "Lag (periods)"),
        yaxis = list(title = "Correlation")
      )
  })
  
  # Top 3 absolute correlations (excluding lag 0)
  output$lag_table <- renderTable({
    d <- ccf_data() %>% 
      filter(lag != 0) %>% 
      arrange(desc(abs(acf))) %>% 
      slice_head(n = 3) %>% 
      mutate(acf = round(acf, 3))
    colnames(d) <- c("Lag", "Correlation")
    d
  }, digits = 3)
  
}

shinyApp(ui = ui, server = server)
