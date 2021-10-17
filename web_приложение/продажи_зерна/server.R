shinyServer(function(input, output) {
  
  vars <- reactiveValues(costs = costs)
  
  #Вкладка структур показателей
  output$costs_plot <- renderPlot({
    
    costs_plot <- ggplot(filter(vars$costs, Product == input$prod_select)) + geom_col(mapping = aes(x = Warehouse, y = CostForMT, fill = DCType), 
                                           position = position_stack()) + labs(x = "Элеватор", y = "Сумма затрат, руб", fill = "Статья затрат")
    costs_plot
  })
  
  output$prices_plot <- renderPlotly({
    prices_plot <- ggplot(prices, mapping = aes(x = Commodity, y = Price_for_MT)) + geom_col(fill = "darkgreen") + labs(x = "Продукт", y = "Ценна за тонну")
    
    ggplotly(prices_plot)
    
    
  })
  
  output$costs_edit_table <- renderRHandsontable({
    rhandsontable(vars$costs)
    
  })
  
  observeEvent(input$table_save_button, {
    edited_costs <- hot_to_r(input$costs_edit_table)
    vars$costs <- edited_costs
    
    infoDialog("Успех!", "Данные изменены и сохранены.")
    
  })
  
  #Вкладка заказы
  
  output$positions_table <- renderDataTable({
    p <- positions %>% select(Commodity, QuantityMT, PositionType, Price_for_MT, Position_cost, Silo)
    colnames(p) <- c("Товар", "Кол-во т", "Тип", "Цена за т", "Себестоимость", "Хранение")
    datatable(p)
  })
  
  #Закупка
  
  output$purchase_box <- renderValueBox({
    purchases <- filter(positions, PositionType == "Закупка") %>% summarise(purch = sum(Position_cost))
    v <- paste(round(purchases$purch[1] / 1000000, 1), "M", sep = "")
    valueBox(v, "Закупки", color = "purple", icon = icon("truck"))
  })
  
  #Продажа
  
  output$sales_box <- renderValueBox({
    sales <- filter(positions, PositionType == "Продажа") %>% summarise(sale = sum(QuantityMT * Price_for_MT))
    v <- paste(round(sales$sale[1] / 1000000, 1), "M", sep = "")
    valueBox(v, "Продажи", color = "orange", icon = icon("ship"))
  })
  
  #Маржа
  
  output$margin_box <- renderInfoBox({
    sales <- filter(positions, PositionType == "Продажа") %>% summarise(sale = sum(QuantityMT * Price_for_MT))
    sales_cost <- filter(positions, PositionType == "Продажа") %>% summarise(cost = sum(Position_cost))
    v <- paste(round(100 - (sales_cost$cost[1] / sales$sale[1] * 100), 1), "%", sep = "")
    infoBox("Маржа", v, icon = icon("percentage"), color = "red")
    
  })
  
})