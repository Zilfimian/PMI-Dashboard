pacman::p_load(
  DBI, RMySQL, shiny, shinydashboard, dashboardthemes, plotly,
  DT, stringr, ggplot2, readxl, tidyr, lubridate, RColorBrewer,
  reactable, glmnet, leaps, MASS, jtools, moments, ggpubr, naniar,
  imputeTS, formattable, reactablefmtr, shinycssloaders,
  dplyr
)

con <- dbConnect(MySQL(),
  dbname = "aw_schema",
  user = "root",
  password = "IronMan2024",
  host = "127.0.0.1"
)

source("Help.R")

server <- function(input, output, session) {
  # Set this to "force" instead of TRUE for testing locally (without Shiny Server)
  session$allowReconnect(TRUE)

  session$onSessionEnded(function() {
    dbDisconnect(con)
  })

  #---------------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------- Tab 1 -----------------------------------
  #---------------------------------------------------------------------------------------------------------


  output$UI1 <- renderUI({
    fluidRow(
      class = "text-center & center",
      box(
        width = 12, solidHeader = TRUE,
        valueBox(value = textOutput("totSales"), subtitle = "Total Sales", color = "green", width = 3),
        valueBox(value = textOutput("orderNum"), subtitle = "Number of Orders", color = "olive", width = 3),
        valueBox(value = textOutput("customNum"), subtitle = "Numer of Customers", color = "teal", width = 3),
        valueBox(value = textOutput("producNum"), subtitle = "Number of Products", color = "green", width = 3),
        helpText(textOutput("tab1HelpText")),

        # Tab 1.1 (Sales Amount)

        box(
          width = 5,
          shinycssloaders::withSpinner(plotlyOutput("Sales_Plot")),
          shinycssloaders::withSpinner(dataTableOutput("Sales_Summary_Table"))
        ),



        # Tab 1.2 (Product category and Product price)

        box(
          width = 7,
          box(shinycssloaders::withSpinner(plotlyOutput("Product_Price_Plot")), width = 8, solidHeader = T),
          box(shinycssloaders::withSpinner(reactableOutput("Product_Price_Table")), width = 4, solidHeader = T),
          shinycssloaders::withSpinner(dataTableOutput("Product_Summary_Table"))
        ),

        #  Tab 1.3 (Customer demographics)
        valueBox(subtitle = "Customer Demographics Analysis", value = "", color = "aqua", width = 12),
        box(uiOutput("firstSelectionDemo"),
          shinycssloaders::withSpinner(plotlyOutput("Demo1_Plot")),
          width = 3, solidHeader = T
        ),
        box(uiOutput("secondSelectionDemo"),
          shinycssloaders::withSpinner(plotlyOutput("Demo2_Plot")),
          width = 3, solidHeader = T
        ),
        box(shinycssloaders::withSpinner(plotlyOutput("Demo3_Bivariate")), width = 6, solidHeader = T)
      )
    )
  }) # UI1





  # Example of rendering dynamic table based on selections
  dynamic_table <- function(data, vars, condition = NULL) {
    vars <- paste0(vars, collapse = ", ")
    data_ <- dbGetQuery(con, trimws(paste("SELECT", vars, "FROM", data, condition)))
    return(data_)
  }

  # Tab 1.1 (Sales Amount)

  sales_amount_row <- reactive({
    dynamic_table(data = "sales_salesorderheader", vars = c("OrderDate", "Status", "SubTotal", "SalesOrderID", "CustomerID"), condition = "WHERE Status = 5")
  })


  sales_amount <- reactive({
    sales_amount_row() %>%
      mutate(OrderDate = as.Date(OrderDate)) %>%
      group_by(OrderDate) %>%
      summarise(`Sales amount` = sum(SubTotal)) %>%
      mutate(`Sales amount (log)` = log(`Sales amount`))
  })


  output$Sales_Plot <- renderPlotly({
    sales_amount <- sales_amount()
    ggplotly(
      ggplot(data = sales_amount, mapping = aes(
        x = `Sales amount (log)`,
        text = paste(
          "Mean: ", round(mean(sales_amount$`Sales amount (log)`, na.rm = T), 2), "\n",
          "Median:", round(median(sales_amount$`Sales amount (log)`, na.rm = T), 2), "\n",
          "Str.Dev:", round(sd(sales_amount$`Sales amount (log)`, na.rm = T), 2), "\n"
        )
      ), alpha = 0.7) +
        geom_histogram(alpha = 0.3, position = "identity", col = "darkgreen", fill = "lightgreen") +
        xlab("") +
        ylab("Frequency") +
        ggtitle(paste("Distribution of Daily Sales Amount (log)")) +
        labs(fill = "") +
        geom_vline(xintercept = mean(sales_amount$`Sales amount (log)`, na.rm = T), col = "darkred", alpha = 0.9, size = 1, lty = 2) +
        geom_vline(xintercept = median(sales_amount$`Sales amount (log)`, na.rm = T), col = "darkblue", alpha = 0.9, size = 1, lty = 3) +
        ggthemes::theme_pander(),
      tooltip = c("x", "text")
    ) %>% plotly::layout(margin = list(t = 75))
  })



  output$Sales_Summary_Table <- renderDataTable({
    sales_amount() %>%
      summarise(across(where(is.numeric),
        .fns =
          list(
            Median = median,
            Mean = mean,
            SD = sd,
            CV = ~ sd(.) / mean(.) * 100,
            SK = skewness
          )
      )) %>%
      pivot_longer(everything(), names_sep = "_", names_to = c("variable", ".value")) %>%
      DT::datatable() %>%
      formatCurrency(2:6, currency = "", interval = 3, mark = ",")
  })

  output$tab1HelpText <- renderText({
    paste0("Displaying information for the period from ", min(sales_amount()$OrderDate), " to ", max(sales_amount()$OrderDate))
  })

  output$totSales <- renderText({
    paste0("$", round(sum(sales_amount()$`Sales amount` / 1000000, na.rm = T)), "M")
  })

  output$orderNum <- renderText({
    ordNum <- sales_amount_row()$SalesOrderID %>%
      unique() %>%
      length()
    format(ordNum, big.mark = ",")
  })

  output$customNum <- renderText({
    cust <- sales_amount_row()$CustomerID %>%
      unique() %>%
      length()
    format(cust, big.mark = ",")
  })


  # Tab 1.3 (Product category and Product price)

  Product_Price <- reactive({
    dbGetQuery(con, product_tab1) %>%
      mutate(CategoryName = factor(CategoryName))
  })




  output$producNum <- renderText({
    Product_Price()$CategoryName %>%
      unique() %>%
      length()
  })


  output$Product_Price_Plot <- renderPlotly({
    ggplotly(ggbarplot(
      distinct(dplyr::select(Product_Price(), c(CategoryName, ListPrice))),
      x = "CategoryName", y = "ListPrice",
      add = c("mean_se", "jitter"),
      fill = "CategoryName", palette = "Greens",
      position = position_dodge(0.8)
    ) + labs(y = "List Price", x = "") + ggtitle("Product Price by Category") + coord_flip(), tooltip = c("y", "fill")) %>% hide_legend()
  })

  output$Product_Price_Table <- renderReactable({
    Product_Price <- Product_Price()
    reactable(
      arrange(Product_Price[, 2:1], desc(Product_Price$ListPrice)),
      columns = list(
        ListPrice = colDef(style = color_scales(Product_Price, colors = c("white", "darkgreen")))
      ), defaultPageSize = 5
    )
  })

  output$Product_Summary_Table <- renderDataTable({
    distinct(dplyr::select(Product_Price(), c(CategoryName, ListPrice))) %>%
      group_by(CategoryName) %>%
      summarise(
        `Median of Price` = median(ListPrice),
        `Mean of Price` = mean(ListPrice),
        `SD of Price` = sd(ListPrice),
        `CV of Price` = sd(ListPrice) / mean(ListPrice) * 100,
        `Min of Price` = min(ListPrice),
        `Max of Price` = max(ListPrice)
      ) %>%
      DT::datatable(options = list(dom = "t")) %>%
      formatCurrency(2:6, currency = "", interval = 3, mark = ",")
  })


  # Tab 1.2 (Customer demographics)

  Demo <- reactive({
    Date_max <- sales_amount()$OrderDate %>% max(na.rm = T)
    # Date_max = today()

    df <- dbGetQuery(con, query_demo_tab1)
    df[df == ""] <- NA

    df <- df %>% mutate(
      MaritalStatus = factor(MaritalStatus, levels = c("M", "S"), labels = c("Married", "Single")),
      Gender = factor(Gender, levels = c("F", "M"), labels = c("Female", "Male")),
      Education = factor(Education, levels = c("Partial College", "Partial High School", "High School", "Bachelors ", "Graduate Degree")),
      HomeOwnerFlag = factor(HomeOwnerFlag, levels = c("0", "1"), labels = c("No Home", "Homw Owner")),
      TotalChildren = factor(TotalChildren),
      NumberChildrenAtHome = factor(NumberChildrenAtHome),
      NumberCarsOwned = factor(NumberCarsOwned),
      TotalPurchaseYTD = as.numeric(TotalPurchaseYTD),
      OldUser = as.numeric((Date_max - as.Date(DateFirstPurchase)) / 365.25),
      Age = as.numeric(round((Date_max - as.Date(BirthDate)) / 365.25)),
      YearlyIncome = factor(YearlyIncome, levels = sort(unique(YearlyIncome))),
      Occupation = factor(Occupation),
      CommuteDistance = factor(CommuteDistance, levels = sort(unique(CommuteDistance)))
    )
    df
  })

  # For demographic Select Inputs
  output$firstSelectionDemo <- renderUI({
    choice_1 <- colnames(Demo())[c(18, 4, 7:17)]
    selectInput("Demo1", "Select the variable:", choices = choice_1)
  })

  output$secondSelectionDemo <- renderUI({
    shiny::validate(
      need(length(colnames(Demo())) > 0, "A message!")
    )

    choice_1 <- colnames(Demo())[c(7:18, 4)]
    choice_2 <- choice_1[!str_detect(choice_1, input$Demo1)]
    selectInput("Demo2", "Select the variable:", choices = choice_2)
  })

  numeric_vs_categorical <- function(xx) {
    Demo <- Demo()
    Demo <- Demo[, c("PersonID", xx)] %>% na.omit()


    if (class(Demo[, xx]) == "factor") {
      # categorical
      tablecat1 <- function(m) {
        m <- enquo(m)
        Demo %>%
          count(.dots = m) %>%
          mutate(Percentage = round(prop.table(n) * 100, 1)) %>%
          na.omit()
      }


      ggplotly(
        ggplot(Demo, aes(x = !!rlang::sym(xx), fill = !!rlang::sym(xx))) +
          geom_bar(position = "dodge", alpha = 0.7) +
          geom_text(
            data = tablecat1(!!rlang::sym(xx)),
            aes(y = n, label = paste0(Percentage, "%")), position = position_dodge(width = 0.5), size = 3
          ) +
          xlab(" ") +
          ylab("Frequency (N)") +
          labs(fill = " ") +
          ggtitle(paste("Distribution of", xx)) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 90, hjust = 0.9),
            legend.position = "None",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
          ) +
          scale_fill_brewer(palette = "Paired"),
        tooltip = c("x", "y") # to handle problem with hovertext
      ) %>% plotly::layout(showlegend = FALSE, margin = list(t = 75)) # %>% config(  modeBarButtonsToRemove = "hoverCompareCartesian")
    } else {
      # numeric

      g1 <- ggplot(Demo, aes(y = !!rlang::sym(xx), x = "1")) +
        geom_boxplot(alpha = 0.3, col = "darkblue", fill = "blue") +
        xlab("") +
        ylab("") +
        stat_summary(fun.y = mean, geom = "point", shape = 20, size = 4, color = "white") +
        ggthemes::theme_pander() +
        ggtitle(paste("Distribution of", xx)) +
        theme(
          legend.position = "None",
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        ) +
        coord_flip()
      ggplotly(g1, tooltip = c("text", "y")) %>% plotly::layout(
        margin = list(t = 75),
        annotations = list(x = 0, y = 1.05, text = "Dot represents the mean", showarrow = F, xref = "paper", yref = "paper")
      )
    }
  }

  output$Demo1_Plot <- renderPlotly({
    numeric_vs_categorical(input$Demo1)
  })

  output$Demo2_Plot <- renderPlotly({
    shiny::validate(
      need(length(colnames(Demo())) > 0, "A message!")
      # need(nchar(input$Demo2)>0, "A message!")
    )

    numeric_vs_categorical(input$Demo2)
  })

  output$Demo3_Bivariate <- renderPlotly({
    shiny::validate(
      need(length(colnames(Demo())) > 0, "A message!")
      # need(nchar(input$Demo2)>0, "A message!")
    )

    Demo <- Demo()
    xx <- input$Demo1
    yy <- input$Demo2


    class_xx <- class(Demo[, xx])
    class_yy <- class(Demo[, yy])

    Demo <- Demo[, c(xx, yy)] %>% na.omit()

    if (class_xx == "factor" & class_yy == "factor") {
      # Categorical - categorical
      tablecatG <- function(m, cat, round = 1) {
        m <- enquo(m)
        Demo %>%
          group_by(!!cat) %>%
          count(.dots = m) %>%
          mutate(Percentage = round(prop.table(n) * 100, round)) %>%
          na.omit()
      }


      ggplotly(Demo %>%
        ggplot(aes(x = !!rlang::sym(xx), fill = !!rlang::sym(yy))) +
        geom_bar(position = "dodge", alpha = 0.7) +
        geom_text(
          data = tablecatG(!!rlang::sym(xx), quo(!!rlang::sym(yy)), round = 0), aes(
            y = n,
            label = paste0(Percentage, "% ")
          ),
          position = position_dodge(width = 0.8),
          size = 2.5
        ) +
        xlab(" ") +
        ylab("Frequency (N)") +
        labs(fill = " ") +
        ggtitle(paste("Distribution of", xx, "by", yy)) +
        theme_minimal() +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 0.9), legend.position = "bottom"
        ) +
        scale_fill_brewer(palette = "Set1"), tooltip = c("text", "y")) %>%
        plotly::layout(margin = list(t = 75))
    } else if (class_xx != "factor" & class_yy != "factor") {
      # Numeric - Numeric


      g1 <- ggplot(Demo, aes(y = !!rlang::sym(xx), x = !!rlang::sym(yy))) +
        geom_point(alpha = 0.3, col = "darkblue", fill = "blue") +
        xlab("") +
        ylab("") +
        ggthemes::theme_pander() +
        ggtitle(paste("Relationship between", xx, "and", yy)) +
        theme(legend.position = "None")
      ggplotly(g1, tooltip = c("x", "y")) %>% hide_legend()
    } else {
      if (class_xx == "factor") {
        k <- yy
        yy <- xx
        xx <- k
      }

      class_xx <- class(Demo[, xx])
      class_yy <- class(Demo[, yy])

      Demo %>% ggplot(aes(
        x = !!rlang::sym(yy),
        y = !!rlang::sym(xx),
        fill = !!rlang::sym(yy)
      )) +
        geom_boxplot() +
        stat_summary(fun.y = mean, geom = "point", shape = 20, size = 4, color = "white") +
        theme(axis.text.x = element_text(angle = 90, size = 7), legend.position = "None") +
        ylab("Demand (Count)") +
        xlab("") +
        ggtitle(paste0("Distribution of cash demand by ", tolower(input$MonWeek), "s and banknotes"))

      g1 <- ggplot(Demo, aes(y = !!rlang::sym(xx), x = !!rlang::sym(yy), fill = !!rlang::sym(yy))) +
        geom_boxplot(alpha = 0.3) +
        xlab("") +
        ylab("") +
        stat_summary(fun.y = mean, geom = "point", shape = 20, size = 4, color = "white") +
        ggthemes::theme_pander() +
        ggtitle(paste("Distribution of", xx, "per", yy)) +
        theme(legend.position = "None") +
        coord_flip()
      ggplotly(g1, tooltip = c("text", "y")) %>%
        plotly::layout(
          margin = list(t = 75),
          annotations = list(x = 0, y = 1.05, text = "Dot represents the mean", showarrow = F, xref = "paper", yref = "paper")
        ) %>%
        hide_legend()
    }
  })


  #---------------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------- Tab 2 -----------------------------------
  #---------------------------------------------------------------------------------------------------------



  output$UI2 <- renderUI({
    fluidRow(
      class = "text-center & center",
      box(
        solidHeader = T, width = 12,
        valueBox(subtitle = "Sales Trends Analysis", value = "", color = "light-blue", width = 12),
        box(width = 6, solidHeader = T, dateRangeInput("dateRangeInput", "Select Date Range for Plots", start = "2011-06-30", end = "2014-05-29")),
        valueBox(value = textOutput("MinDate"), subtitle = "Start Date", color = "aqua", width = 3),
        valueBox(value = textOutput("MaxDate"), subtitle = "End Date", color = "teal", width = 3),
        helpText(textOutput("tab2HelpText")),

        # Sales Trends By Territory
        box(
          width = 9,
          shinycssloaders::withSpinner(plotlyOutput("Sales_Trend_Ter")),
          box(
            solidHeader = T, width = 12,
            checkboxInput("cb1", label = "Add to Report", value = F)
          )
        )
        # Tab 2.1 Sales by Product Category

        , box(
          width = 3,
          shinycssloaders::withSpinner(plotlyOutput("Sales_Category")),
          box(
            solidHeader = T, width = 12,
            checkboxInput("cb2", label = "Add to Report", value = F)
          )
        ),

        ## Sales Proportion by Territory and Product Category

        box(
          width = 4, height = 600,
          box(shinycssloaders::withSpinner(plotlyOutput("Sales_Category_Territory")), solidHeader = T, width = 12, height = 500),
          box(
            solidHeader = T, width = 12,
            checkboxInput("cb3", label = "Add to Report", value = F)
          )
        ),


        ## More detailed analysis

        box(
          width = 8,
          selectInput("territoryInput", "Select Territory", choices = c("Australia", "Canada", "Central", "France", "Germany", "Northeast", "Northwest", "Southeast", "Southwest", "United Kingdom"), selected = "Southwest"),
          box(shinycssloaders::withSpinner(plotlyOutput("Detailed_Trends")), width = 8, box(
            solidHeader = T, width = 12,
            checkboxInput("cb4", label = "Add to Report", value = F)
          )),
          box(shinycssloaders::withSpinner(reactableOutput("Sales_Top_Table")), width = 4, box(
            solidHeader = T, width = 12,
            downloadButton("Sales_Product", "Download the data")
          )),
        ),
        box(
          solidHeader = T, width = 12,
          radioButtons("report_format", "Choose the Report Format", c("PDF", "HTML", "Word"), inline = TRUE),
          downloadButton("report", "Create and Download the Report")
        )
      ) # box full
    )
  }) # UI2



  #*****************************************************************
  ### For Report.rmd (ifelse - include or not)
  #*****************************************************************


  checkgraps <- reactiveValues(
    cb1 = FALSE, cb2 = FALSE, cb3 = F, cb4 = FALSE, cb5 = FALSE
  )

  observeEvent(input$cb1, {
    checkgraps$cb1 <- input$cb1
  })
  observeEvent(input$cb2, {
    checkgraps$cb2 <- input$cb2
  })

  observeEvent(input$cb3, {
    checkgraps$cb3 <- input$cb3
  })

  observeEvent(input$cb4, {
    checkgraps$cb4 <- input$cb4
  })
  observeEvent(input$cb5, {
    checkgraps$cb5 <- input$cb5
  })

  # To save graphs
  vals <- reactiveValues(g1 = NULL, g2 = NULL, g3 = NULL, g4 = NULL, g5 = NULL)
  # To show graphs


  start_end_data <- reactive({
    dynamicQuery <- paste0("SELECT OrderDate FROM (", query_sales_trends_tab2, ") AS subquery WHERE 1=1")

    dbGetQuery(con, dynamicQuery) %>% mutate(OrderDate = as.Date(OrderDate))
  })
  output$MinDate <- renderText({
    as.character(min(start_end_data()$OrderDate))
  })
  output$MaxDate <- renderText({
    as.character(max(start_end_data()$OrderDate))
  })

  sales_trends <- reactive({
    # Dynamic Query for Sales Trends

    Start_date <- input$dateRangeInput[1]
    End_date <- input$dateRangeInput[2]

    # Starting point
    dynamicQuery <- paste0("SELECT * FROM (", query_sales_trends_tab2, ") AS subquery WHERE 1=1")

    # Add Query User Input Date
    if (!is.null(input$dateRangeInput)) {
      dynamicQuery <- paste0(dynamicQuery, " AND  CAST(OrderDate as date) BETWEEN '", str_remove_all(Start_date, "-"), "' AND '", str_remove_all(End_date, "-"), "'")
    }

    df <- dbGetQuery(con, dynamicQuery) %>% mutate(
      OrderDate = as.Date(OrderDate),
      OrderMonth = factor(format(OrderDate, "%b-%Y"), levels = unique(format(OrderDate, "%b-%Y"))),
      Year = format(OrderDate, "%Y"),
      ProductLine = factor(ProductLine, levels = c("M", "S", "R", "T"), labels = c("Mountain", "Standard", "Road", "Touring"))
    )

    df
  })

  output$tab2HelpText <- renderText({
    paste0("Displaying information for the period from ", min(sales_trends()$OrderDate), " to ", max(sales_trends()$OrderDate))
  })

  # Sales Trends By Group

  output$Sales_Trend_Ter <- renderPlotly({
    sales <- sales_trends()
    vals$g1 <- sales %>%
      dplyr::select(OrderMonth, LineTotal, OrderQty, Group) %>%
      group_by(OrderMonth, Group) %>%
      summarise(
        SalesAmount = sum(LineTotal) / 1000000,
        OrderQty = sum(OrderQty) / 1000
      ) %>%
      mutate(Prop = SalesAmount / sum(SalesAmount)) %>%
      ggplot(aes(x = OrderMonth, fill = Group, group = Group, text = paste0(round(Prop * 100), "% (", paste0(round(SalesAmount * 1000000), "$)")))) +
      geom_col(aes(y = `SalesAmount`)) +
      theme_minimal() +
      geom_text(position = position_stack(vjust = 0.5), size = 3, aes(y = `SalesAmount`, label = ifelse(Prop > 0.2, paste0(round(Prop * 100), "%"), ""))) +
      theme(axis.text.x = element_text(size = 9, angle = 90)) +
      scale_fill_brewer(palette = "Blues") +
      labs(fill = "", y = "Sales Amount", x = "") +
      geom_line(aes(y = OrderQty / 2.5, color = Group), size = 0.7, alpha = 1) +
      geom_point(aes(y = OrderQty / 2.5, color = Group), size = 1, alpha = 1) +
      scale_y_continuous(sec.axis = sec_axis(trans = ~ . * 2.5, name = "")) +
      scale_color_brewer(palette = "Greens") +
      labs(col = "") +
      # guides(fill = "none")+
      ggtitle("Sales Amount and Order Quantity Trends by Territory Group") +
      theme(legend.position = "bottom")

    ggplotly(vals$g1, tooltip = c("text", "fill")) %>% layout(legend = list(orientation = "h", xanchor = "center", y = -0.25, x = 0.5))
  })

  # Tab 2.1 Sales by Product Category

  output$Sales_Category <- renderPlotly({
    sales <- sales_trends()
    Pie_Data <- sales %>%
      group_by(CategoryName) %>%
      summarise(Amount = round(sum(LineTotal)), Count = round(sum(OrderQty))) %>%
      mutate(Prop = Amount / sum(Amount)) %>%
      arrange(desc(CategoryName)) %>%
      mutate(lab.ypos = cumsum(Prop) - 0.5 * Prop)

    cols <- RColorBrewer::brewer.pal(nrow(Pie_Data), "Blues")


    vals$g2 <- ggplot(Pie_Data, aes(x = 2, y = Prop, fill = CategoryName)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0) +
      geom_text(aes(y = lab.ypos, label = paste0(round(Prop * 100), "% (", round(Amount / 1000000), "M)")), color = "black") +
      scale_fill_brewer(palette = "Blues") +
      theme_void() +
      xlim(0.5, 2.5) +
      ggtitle("Sales Amount by Product Category")

    plotly::plot_ly(Pie_Data) %>%
      add_pie(Pie_Data,
        labels = ~ factor(CategoryName), values = ~Amount,
        marker = list(colors = cols),
        insidetextorientation = "radial",
        textinfo = "label+percent", type = "pie", hole = 0.6
      ) %>%
      layout(title = "Sales Amount by Product Category", margin = list(l = 0, r = 0, t = 40, b = 0)) %>%
      hide_legend()
  })

  ## Sales Proportion by Territory and Product Category

  output$Sales_Category_Territory <- renderPlotly({
    sales <- sales_trends()
    vals$g3 <- sales %>%
      group_by(CategoryName, NameTerritory) %>%
      summarise(Amount = round(sum(LineTotal)), Count = round(sum(OrderQty))) %>%
      ungroup() %>%
      group_by(NameTerritory) %>%
      mutate(
        Prop = Amount / sum(Amount),
        CategoryName = factor(CategoryName, levels = c("Accessories", "Bikes", "Clothing", "Components"))
      ) %>%
      ggplot(aes(x = NameTerritory, y = Amount, fill = CategoryName, text = paste0(round(Prop * 100), "% (", paste0(round(Amount), "$)")))) +
      geom_bar(position = "fill", stat = "identity") +
      theme_minimal() +
      geom_text(position = position_fill(vjust = 0.5), size = 3, aes(y = Amount, label = ifelse(Prop > 0.1, paste0(round(Prop * 100), "% ($", paste0(round(Amount / 1000000), "M)")), ""))) +
      theme(
        axis.text.y = element_text(size = 9),
        axis.text.x = element_blank(), axis.ticks.x = element_blank()
      ) +
      scale_fill_brewer(palette = "Greens") +
      labs(fill = "") +
      coord_flip() +
      labs(x = " ", y = "") +
      ggtitle("Distribution of Sales Across Product \n Categories in Different Territories")

    ggplotly(vals$g3, tooltip = c("text", "fill")) %>% layout(legend = list(orientation = "h", xanchor = "center", y = -0.25, x = 0.5), height = 700)
  })

  ## More detailed analysis

  sales_trends_filter <- reactive({
    # Dynamic Query for Sales Trends

    Start_date <- input$dateRangeInput[1]
    End_date <- input$dateRangeInput[2]

    # Starting point
    dynamicQuery <- paste0("SELECT * FROM (", query_sales_trends_tab2, ") AS subquery WHERE 1=1")

    # Add Query User Input Date
    if (!is.null(input$dateRangeInput)) {
      dynamicQuery <- paste0(dynamicQuery, " AND  CAST(OrderDate as date) BETWEEN '", str_remove_all(Start_date, "-"), "' AND '", str_remove_all(End_date, "-"), "'")
    }

    # Add filter conditions for Territory name
    if (!is.null(input$territoryInput)) {
      TN <- input$territoryInput
      dynamicQuery <- paste0(dynamicQuery, " AND NameTerritory = '", TN, "'")
    }

    df <- dbGetQuery(con, dynamicQuery) %>% mutate(
      OrderDate = as.Date(OrderDate),
      OrderMonth = factor(format(OrderDate, "%b-%Y"), levels = unique(format(OrderDate, "%b-%Y"))),
      Year = format(OrderDate, "%Y"),
      ProductLine = factor(ProductLine, levels = c("M", "S", "R", "T"), labels = c("Mountain", "Standard", "Road", "Touring"))
    )

    df
  })

  output$Detailed_Trends <- renderPlotly({
    sales <- sales_trends_filter()
    vals$g4 <- sales %>%
      dplyr::select(OrderMonth, LineTotal, OrderQty, CategoryName) %>%
      group_by(OrderMonth, CategoryName) %>%
      summarise(
        SalesAmount = sum(LineTotal) / 1000000,
        OrderQty = sum(OrderQty) / 1000,
        low = n() * quantile(LineTotal, probs = 0.25) / 1000000,
        high = n() * quantile(LineTotal, probs = 0.75) / 1000000
      ) %>%
      mutate(`Sales Amount (MA)` = na_ma(SalesAmount, k = 10, weighting = "exponential")) %>%
      ggplot(aes(x = OrderMonth, col = CategoryName, group = CategoryName, text = paste0(
        "Sales Amount: $", paste0(round(SalesAmount * 1000000), " \n"),
        "Order Quantity: ", paste0(round(OrderQty * 1000), "")
      ))) +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 9, angle = 90)) +
      geom_line(aes(y = SalesAmount, color = CategoryName), size = 1, alpha = 0.7) +
      geom_line(aes(y = `Sales Amount (MA)`, color = CategoryName), size = 0.5, alpha = 0.7) +
      geom_point(aes(y = SalesAmount, color = CategoryName), size = 1, alpha = 0.7) +
      scale_color_brewer(palette = "Greens") +
      labs(col = "") +
      geom_ribbon(aes(ymin = low, ymax = high, fill = CategoryName), alpha = 0.1) +
      guides(fill = "none") +
      labs(x = "", y = "Sales Amount") +
      ggtitle(paste0(" Sales Patterns Across Product Categories in the Territory: ", unique(sales$NameTerritory)))

    ggplotly(vals$g4, tooltip = c("text", "fill")) %>% layout(legend = list(orientation = "h", xanchor = "center", y = -0.24, x = 0.5))
  })

  table_top <- reactive({
    sales <- sales_trends_filter()

    table_top <- sales %>%
      group_by(ProductName) %>%
      summarise(
        SalesAmount = sum(LineTotal),
        OrderQty = sum(OrderQty)
      ) %>%
      arrange(desc(OrderQty))

    colnames(table_top) <- c("Product Name", "Amount", "Quantity")
    table_top$`Quantity` <- round(table_top$`Quantity`)
    table_top$`Amount` <- round(table_top$`Amount`)
    table_top
  })

  output$Sales_Top_Table <- renderReactable({
    table_top <- table_top()
    reactable(
      table_top,
      columns = list(
        `Quantity` = colDef(style = color_scales(table_top, colors = c("white", "darkgreen"))),
        `Amount` = colDef(style = color_scales(table_top, colors = c("white", "darkblue")))
      ), defaultPageSize = 5,
      style = list(fontSize = "12px")
    )
  })


  output$Sales_Product <- downloadHandler(
    filename = function() {
      paste0("Product-Sales-Data", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(table_top(), path = file)
    }
  )


  output$report <- downloadHandler(
    filename = function() {
      paste(paste0("Sales-Trends-Report-", Sys.Date()), sep = ".", switch(input$report_format,
        PDF = "pdf",
        HTML = "html",
        Word = "docx"
      ))
    },
    content = function(file) {
      src <- normalizePath(
        # switch(input$report_format, PDF =
        "report.Rmd"
        # , HTML = 'report1.Rmd', Word = 'report1.Rmd')
      )
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src,
        # switch(input$report_format, PDF =
        "report.Rmd"
        #    , HTML = 'report1.Rmd', Word = 'report1.Rmd')
        ,
        overwrite = TRUE
      )

      library(rmarkdown)
      if (input$report_format == "PDF") {
        out <- render("report.Rmd", "all") # ,pdf_file = tempfile(fileext = ".pdf")
      } else {
        out <- render("report.Rmd", switch(input$report_format,
          HTML = html_document(),
          Word = word_document()
        ))
      }


      file.rename(out, file)
    }
  )


  #---------------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------- Tab 3 -----------------------------------
  #---------------------------------------------------------------------------------------------------------





  #---------------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------- Tab 4 -----------------------------------
  #---------------------------------------------------------------------------------------------------------




  #---------------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------- Tab 5 -----------------------------------
  #---------------------------------------------------------------------------------------------------------
} # server

# lapply(dbListConnections(MySQL()), dbDisconnect)
