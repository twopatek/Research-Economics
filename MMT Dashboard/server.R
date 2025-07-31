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