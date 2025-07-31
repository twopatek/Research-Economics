# server
server <- function(input, output, session) {
  # populate the series selector once df is ready
  observe({
    updatePickerInput(
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
          "<br>Value: ", comma(series),
          "<br>%chg: ", round(pct_change, 2), "%"
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
  
  output$series_guide_table <- renderDT({
    tibble(
      `Series Name` = c(
        "Yield Curve (T10Y2YM)",
        "Interest Payments (A091RC1Q027SBEA)",
        "Total Public Debt (GFDEBTN)",
        "Federal Surplus/Deficit (FYFSD)",
        "Nominal GDP (GDP)",
        "Headline CPI (CPIAUCSL)",
        "Core CPI (CPILFESL)",
        "PCE Price Index (PCEPI)",
        "Core PCE (PCEPILFE)",
        "M2 Money Stock (M2SL)",
        "Real M2 (M2REAL)",
        "Federal Funds Rate (DFF)",
        "Fed Total Assets (WALCL)"
      ),
      Description = c(
        "The difference between the 10-year and 2-year U.S. Treasury interest rates. A negative value (called an 'inverted yield curve') often signals a potential economic recession, while a positive value typically reflects confidence in future growth.",
        
        "The total amount of money the federal government pays each quarter in interest on its debt. Higher interest payments can indicate rising borrowing costs or growing debt burdens, affecting the federal budget and financial stability.",
        
        "The total amount of money the U.S. government owes to creditors. This includes all federal debt held by the public and by government accounts. It reflects how much the government has borrowed over time to finance deficits.",
        
        "The difference between what the federal government earns (revenue) and what it spends. A deficit means spending is higher than revenue, while a surplus means the government brought in more than it spent. This is a key measure of fiscal health.",
        
        "The total market value of all goods and services produced in the U.S. without adjusting for inflation. It gives a snapshot of the size and growth of the economy but doesn't reflect changes in the purchasing power of money.",
        
        "The standard Consumer Price Index, which measures how prices for a fixed basket of goods and services change over time for urban consumers. It is one of the main indicators of inflation that affects cost of living.",
        
        "A version of the CPI that removes food and energy prices, which tend to be volatile. Core CPI is used to analyze underlying or 'core' inflation trends that better reflect long-term price changes.",
        
        "The Personal Consumption Expenditures Price Index tracks changes in the prices of goods and services purchased by consumers. It is favored by the Federal Reserve because it captures a broader range of spending habits than CPI.",
        
        "This measure removes food and energy prices from the PCE index to focus on more stable price trends. The Federal Reserve closely watches Core PCE to guide interest rate decisions and monetary policy.",
        
        "A broad measure of the money supply that includes physical cash, checking accounts, savings accounts, and other near-liquid assets. Changes in M2 can influence inflation, interest rates, and economic growth.",
        
        "The M2 money supply adjusted for inflation. It gives a better picture of the real purchasing power of the money supply, helping to understand whether people’s cash holdings are growing in value or losing buying power.",
        
        "The interest rate banks charge each other for overnight loans. It serves as the baseline rate for many forms of lending and is the primary tool the Federal Reserve uses to influence short-term interest rates and control inflation.",
        
        "The total value of financial assets held by the Federal Reserve, such as Treasury securities and mortgage-backed securities. This reflects the scale of the Fed’s involvement in the economy, especially during monetary stimulus efforts like quantitative easing."
      )
    ) %>% 
      arrange(`Series Name`) %>% 
      datatable(
        options = list(
          paging = FALSE,
          searching = TRUE,
          info = FALSE,
          ordering = TRUE,
          scrollY = "600px",  
          dom = 't'
        ),
        rownames = FALSE
      )

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