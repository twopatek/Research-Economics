# ---- SERVER ----
server <- function(input, output, session) {
  
  # populate the series selector once df is ready
  observe({
    updatePickerInput(
      session, 
      "series_name", 
      choices  = sort(unique(df$series_name)),
      selected = preselected_series_names
    )
  })
  
  # reactive that returns min/max dates for the currently‐selected series
  sel_span <- reactive({
    req(input$series_name)
    sel <- df %>% filter(series_name %in% input$series_name)
    list(
      min = min(sel$date, na.rm = TRUE),
      max = max(sel$date, na.rm = TRUE)
    )
  })
  
  # whenever series change, reset the date_range to full span of those series
  observeEvent(sel_span(), {
    updateDateRangeInput(
      session,
      "date_range",
      start = sel_span()$min,
      end   = sel_span()$max
    )
  })
  
  # quick‐select observers using sel_span()
  observeEvent(input$btn_6m, {
    end   <- sel_span()$max
    start <- end %m-% months(6)
    updateDateRangeInput(session, "date_range", start = start, end = end)
  })
  observeEvent(input$btn_1y, {
    end   <- sel_span()$max
    start <- end %m-% years(1)
    updateDateRangeInput(session, "date_range", start = start, end = end)
  })
  observeEvent(input$btn_5y, {
    end   <- sel_span()$max
    start <- end %m-% years(5)
    updateDateRangeInput(session, "date_range", start = start, end = end)
  })
  observeEvent(input$btn_10y, {
    end   <- sel_span()$max
    start <- end %m-% years(10)
    updateDateRangeInput(session, "date_range", start = start, end = end)
  })
  observeEvent(input$btn_20y, {
    end   <- sel_span()$max
    start <- end %m-% years(20)
    updateDateRangeInput(session, "date_range", start = start, end = end)
  })
  observeEvent(input$btn_30y, {
    end   <- sel_span()$max
    start <- end %m-% years(30)
    updateDateRangeInput(session, "date_range", start = start, end = end)
  })
  
  # reset button also uses sel_span()
  observeEvent(input$btn_reset, {
    updateDateRangeInput(
      session,
      "date_range",
      start = sel_span()$min,
      end   = sel_span()$max
    )
  })
  
  # filter by series AND date-range
  filtered_data <- reactive({
    req(input$series_name, input$date_range)
    df %>% 
      filter(
        series_name %in% input$series_name,
        date >= input$date_range[1],
        date <= input$date_range[2]
      )
  })
  
  n_series <- length(unique(df$series_name))
  palette_hue <- hue_pal()(n_series)
  
  colors <- setNames(palette_hue, unique(df$series_name))
  
  output$plot <- renderPlotly({
    
    plot_df <- filtered_data() %>%
      group_by(series_name) %>%
      mutate(
        scaled = (series - min(series, na.rm=TRUE)) /
          ( max(series, na.rm=TRUE) - min(series, na.rm=TRUE) )
      ) %>%
      ungroup()
    
    # compute dynamic y‐limits
    y_min <- min(plot_df$scaled, na.rm = TRUE)
    y_max <- max(plot_df$scaled, na.rm = TRUE)
    
    plot_ly(plot_df, x = ~date) %>%
      add_lines(
        y = ~scaled, 
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
        xaxis = list(
          range = list(
            as.character(input$date_range[1]),
            as.character(input$date_range[2])
          ),
          title = "Date"
          ),
        yaxis = list(
          range = c(y_min, y_max),
          title = "Normalized Value"
        ),
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
        "Nonfarm Payroll (PAYEMS)",
        "Unemployment Rate (UNRATE)",
        "Total Public Debt (GFDEBTN)",
        "Federal Surplus/Deficit (FYFSD)",
        "Interest Payments (A091RC1Q027SBEA)",
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
        
        "Nonfarm Payroll Employment (PAYEMS) measures the total number of paid U.S. workers excluding farm workers, private household employees, and nonprofit organization employees. It’s a key indicator of labor market strength and overall economic activity.",
        
        "Civilian Unemployment Rate (UNRATE) represents the percentage of the civilian labor force that is unemployed and actively seeking work. It’s one of the primary gauges of labor market slack and economic health.",
        
        "The total amount of money the U.S. government owes to creditors. This includes all federal debt held by the public and by government accounts. It reflects how much the government has borrowed over time to finance deficits.",
        
        "The difference between what the federal government earns (revenue) and what it spends. A deficit means spending is higher than revenue, while a surplus means the government brought in more than it spent. This is a key measure of fiscal health.",
        
        "The total amount of money the federal government pays each quarter in interest on its debt. Higher interest payments can indicate rising borrowing costs or growing debt burdens, affecting the federal budget and financial stability.",
        
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
  

  
  
  
  
  
  
    # reactive for the selected year
  historical_selected_year <- reactive({
    req(input$historical_budget_year)
    input$historical_budget_year
    })

  # reactive subset for receipts
  receipts_subset <- reactive({
    df_cbo_budget_receipts %>%
      filter(fiscal_year == historical_selected_year())
  })

  # reactive subset for outlays
  outlays_subset <- reactive({
    df_cbo_budget_outlays %>%
      filter(fiscal_year == historical_selected_year())
  })

  output$historical_receipts_sankey_plot <- renderSankeyNetwork({
    # nodes & links for revenue breakdown
    rev_nodes <- data.frame(name = c(receipts_subset()$category, "Total Revenue"), stringsAsFactors = FALSE)
    rev_idx   <- function(x) match(x, rev_nodes$name) - 1
    rev_links <- receipts_subset() %>% transmute(
      source = rev_idx("Total Revenue"),
      target = rev_idx(category),
      value  = amount
    )

    # plot revenue
    rev_sankey <- sankeyNetwork(
      Links    = rev_links, Nodes = rev_nodes,
      Source   = "source", Target = "target",
      Value    = "value",  NodeID = "name",
      fontSize = 12, nodeWidth = 30, nodePadding = 10
    )

    rev_sankey
  })

  output$historical_outlays_sankey_plot <- renderSankeyNetwork({
    # — EXPENSE Sankey —
    tier1 <- c(
      "Discretionary",
      "Programmatic Outlays",
      "Offsetting Receipts",
      "Net Interest"
    )
    tier2a <- c(
      "Defense",
      "Nondefense"
    )
    tier2b <- c(
      "Social Security",
      "Medicare",
      "Medicaid",
      "Income Security",
      "Federal Civilian And Military Retirement",
      "Veterans Programs",
      "Other Programs"
    )

    exp_nodes <- data.frame(
      name = c("Total Expense", tier1, tier2a, tier2b),
      stringsAsFactors = FALSE
    )
    exp_idx <- function(x) match(x, exp_nodes$name) - 1

    # Tier 1 links
    exp_t1 <- outlays_subset() %>% filter(category %in% tier1) %>%
      transmute(
        source = exp_idx("Total Expense"),
        target = exp_idx(category),
        value  = amount
      )

    # Tier 2 links
    exp_t2a <- outlays_subset() %>% filter(category %in% tier2a) %>%
      transmute(
        source = exp_idx("Discretionary"),
        target = exp_idx(category),
        value  = amount
      )
    exp_t2b <- outlays_subset() %>% filter(category %in% tier2b) %>%
      transmute(
        source = exp_idx("Programmatic Outlays"),
        target = exp_idx(category),
        value  = amount
      )

    exp_links <- bind_rows(exp_t1, exp_t2a, exp_t2b)

    # plot expense
    exp_sankey <- sankeyNetwork(
      Links    = exp_links, Nodes = exp_nodes,
      Source   = "source", Target = "target",
      Value    = "value",  NodeID = "name",
      fontSize = 12, nodeWidth = 30, nodePadding = 10
    )

    exp_sankey
  })
  
  output$budget_guide_table <- renderDT({
    datatable(
      budget_guide,
      rownames = FALSE,
      options = list(
        paging         = FALSE,
        scrollX        = TRUE,
        autoWidth      = TRUE,
        dom            = 't'  # keep minimal layout
      ),
      class = "stripe hover nowrap"
    )
  })
  
  
  # reactive for the selected year
  projected_selected_year <- reactive({
    req(input$projection_budget_year)
    input$projection_budget_year
  })
  
  output$projection_receipts_sankey_plot <- renderSankeyNetwork({
  
    receipts_sub <- df_cbo_projections_receipts %>%
      filter(Year == projected_selected_year())
    
    # Step 2: Create nodes
    receipts_nodes <- data.frame(
      name = c("Total Revenue", unique(receipts_sub$Category)),
      stringsAsFactors = FALSE
    )
    
    # Step 3: Function to get node index
    receipts_idx <- function(x) match(x, receipts_nodes$name) - 1
    
    # Step 4: Create links (from Total Revenue to each category)
    receipts_links <- receipts_sub %>%
      transmute(
        source = receipts_idx("Total Revenue"),
        target = receipts_idx(Category),
        value  = Amount
      )
    
    # Step 5: Create Sankey
    rev_sankey <- sankeyNetwork(
      Links    = receipts_links,
      Nodes    = receipts_nodes,
      Source   = "source",
      Target   = "target",
      Value    = "value",
      NodeID   = "name",
      fontSize = 12,
      nodeWidth = 30,
      nodePadding = 10
    )
    
    rev_sankey

  })
  
  output$projection_outlays_sankey_plot <- renderSankeyNetwork({
    
    exp_sub <- df_cbo_projections_joined_outlays %>%
      filter(Year == projected_selected_year())
    
    # Define tiers
    tier1 <- c("Discretionary", "Programmatic Outlays", "Offsetting Receipts", "Net interest")
    tier2b <- c(
      "Social Security", "Medicare", "Medicaid", "Income Security",
      "Federal Civilian and Military Retirement", "Veterans Programs", "Other Programs"
    )
    
    # Create node list
    exp_nodes <- data.frame(
      name = c("Total Expense", tier1, tier2b),
      stringsAsFactors = FALSE
    )
    
    exp_idx <- function(x) match(x, exp_nodes$name) - 1
    
    # Tier 1 links: direct from Total Expense
    exp_t1 <- exp_sub %>%
      filter(Category %in% c("Discretionary", "Offsetting Receipts", "Net interest")) %>%
      transmute(
        source = exp_idx("Total Expense"),
        target = exp_idx(Category),
        value  = Amount
      )
    
    # Add Programmatic Outlays total
    programmatic_total <- exp_sub %>%
      filter(Category %in% tier2b) %>%
      summarise(value = sum(Amount, na.rm = TRUE)) %>%
      mutate(
        source = exp_idx("Total Expense"),
        target = exp_idx("Programmatic Outlays")
      )
    
    # Tier 2b links: detailed categories from Programmatic Outlays
    exp_t2b <- exp_sub %>%
      filter(Category %in% tier2b) %>%
      transmute(
        source = exp_idx("Programmatic Outlays"),
        target = exp_idx(Category),
        value  = Amount
      )
    
    # Combine all links
    exp_links <- bind_rows(exp_t1, programmatic_total, exp_t2b)
    
    # Create Sankey
    exp_sankey <- sankeyNetwork(
      Links    = exp_links,
      Nodes    = exp_nodes,
      Source   = "source",
      Target   = "target",
      Value    = "value",
      NodeID   = "name",
      fontSize = 12,
      nodeWidth = 30,
      nodePadding = 10
    )
    
    # View it
    exp_sankey
  
  })
  
}