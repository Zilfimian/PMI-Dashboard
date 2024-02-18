pacman::p_load(
  DBI, RMySQL, shiny, shinydashboard, dashboardthemes, plotly,
  DT, stringr, ggplot2, readxl, tidyr, lubridate, RColorBrewer,
  reactable, glmnet, leaps, MASS, jtools, moments, ggpubr, naniar,
  dplyr
)

con <- dbConnect(MySQL(),
                 dbname = 'aw_schema',
                 user = 'root',
                 password = 'IronMan2024',
                 host = '127.0.0.1')

source("Help.R")

server <- function(input, output) {
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
          plotlyOutput("Sales_Plot"),
          dataTableOutput("Sales_Summary_Table")
        ),



        # Tab 1.2 (Product category and Product price)

        box(
          width = 7,
          box(plotlyOutput("Product_Price_Plot"), width = 8, solidHeader = T),
          box(reactableOutput("Product_Price_Table"), width = 4, solidHeader = T),
          dataTableOutput("Product_Summary_Table")
        ),

        #  Tab 1.3 (Customer demographics)
        valueBox(subtitle = "Customer Demographics Analysis", value = "", color = "olive", width = 12),
        box(uiOutput("firstSelectionDemo"),
          plotlyOutput("Demo1_Plot"),
          width = 3, solidHeader = T
        ),
        box(uiOutput("secondSelectionDemo"),
          plotlyOutput("Demo2_Plot"),
          width = 3, solidHeader = T
        ),
        box(plotlyOutput("Demo3_Bivariate"), width = 6, solidHeader = T)
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
    levels_set <- function(x) {
      unique(TotalChildren)[ncahr(unique(TotalChildren) > 1)]
    }
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
      OldUser = as.numeric(round((Date_max - as.Date(DateFirstPurchase)) / 365.25)),
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
    choice_1 <- colnames(Demo())[c(7:18, 4)]
    choice_2 <- choice_1[!str_detect(choice_1, input$Demo1)]
    selectInput("Demo2", "Select the variable:", choices = choice_2)
  })

  numeric_vs_categorical <- function(xx) {
    Demo <- Demo()

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
          ggtitle(paste("Disribution of", xx)) +
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
        ggtitle(paste("Disribution of", xx)) +
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
    numeric_vs_categorical(input$Demo2)
  })

  output$Demo3_Bivariate <- renderPlotly({
    Demo <- Demo()
    xx <- input$Demo1
    yy <- input$Demo2


    class_xx <- class(Demo[, xx])
    class_yy <- class(Demo[, yy])

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
          axis.text.x = element_text(angle = 90, hjust = 0.9), legend.position = "None"
        ) +
        scale_fill_brewer(palette = "Set1"), tooltip = c("text", "y")) %>%
        plotly::layout(margin = list(t = 75)) %>%
        hide_legend()
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
        ggtitle(paste("Disribution of", xx, "per", yy)) +
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
