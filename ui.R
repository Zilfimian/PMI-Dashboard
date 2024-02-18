ui <- dashboardPage( 
  
  title =  "Adventure Works ", 
  
  
  dashboardHeader(   titleWidth = 240,
                     title = shinyDashboardLogo(
                       theme = "blue_gradient",
                       boldText = "Adventure Works",
                       mainText = "",
                       badgeText = ""
                     ),
                     
                     tags$li(
                       class = "dropdown", 
                       style = 'height: 50px;
        font-size: 20px;
        line-height: 30px;
        text-align: center;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 0px;
        overflow: hidden;
        color: white;',
                       tags$a(href='https://medium.com/@lusine.zilfimian',
                              tags$p(style="color:#ADD8E6;font-style: oblique;", "Developed by LZ"),target="_blank"))
                     
                     
  ),# dashboardHeader
  dashboardSidebar( 
    
    tags$style(HTML(".main-sidebar{width: 255px;}")),
    sidebarMenu(id = "MenuItemID",
                menuItem("Exploratory data analysis", tabName = "tab1", icon=icon("home")),
                menuItem("Sales Trends by Product and Region", tabName = "tab2", icon=icon("diagnoses")),
                menuItem("Inventory Management Efficiency", tabName = "tab3", icon=icon("info-circle")),
                menuItem("Supplier and Sales Performance", tabName = "tab4", icon=icon("info-circle")),
                menuItem("Sales prediction", tabName = "tab5", icon=icon("info-circle"))
    )),
  
  dashboardBody(
    
    tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:200,450');")),
    shinyDashboardThemes(theme = "blue_gradient"),# purple_gradient, grey_light
    
    tabItems(
      tabItem(tabName = "tab1",  uiOutput("UI1"))#,
      #tabItem(tabName = "tab2",  uiOutput("UI2")),
      #tabItem(tabName = "tab3",  uiOutput("UI3")),
      #tabItem(tabName = "tab4",  uiOutput("UI4")),
      #tabItem(tabName = "tab5",  uiOutput("UI5"))
      
    )
  )
  
  
)# dashboardPage