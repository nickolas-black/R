shinyUI(dashboardPage(
  # заголовок
  dashboardHeader(title = "Продажи зерна"),
  
  #Меню
  dashboardSidebar(
    sidebarMenu(
      menuItem("Показатели", tabName = "structure_tab", icon = icon("dashboard")),
      menuItem("Позиция", tabName = "positions_tab", icon = icon("calendar"))
    )
  ),
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "structure_tab",
              fluidRow(
                useShinyalert(),
                box(title = "Структура затрат", width = 6, collapsible = TRUE,
                    selectInput("prod_select", label = "Продукт",
                                choices = c("ПШЕНИЦА", "ЛЕН", "ЯЧМЕНЬ", "КУНЖУТ","КУКУРУЗА","ГОРОХ","РОЖЬ", "НУТ")),
                    plotOutput("costs_plot")
                ),
                box(title = "Цены на зерно", width = 6, collapsible = TRUE,
                    plotlyOutput("prices_plot")),
                box(title = "Редактирование затрат", width = 12,
                    actionButton("table_save_button", label = "Сохранить"),
                    br(),
                    rHandsontableOutput("costs_edit_table", height = 400, width = 600))
              )),
      tabItem(tabName = "positions_tab",
              fluidRow(
                valueBoxOutput("purchase_box"),
                valueBoxOutput("sales_box"),
                infoBoxOutput("margin_box"),
                box(title = "Позиции", width = 12, collapsible = TRUE,
                    collapsed = TRUE,
                    dataTableOutput("positions_table"))
                
              )
              )
    )
  )
))