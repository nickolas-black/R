library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(plotly)
library(DT)
library(rhandsontable)
library(shinyalert)

positions <- read_excel("D:/данные/разработка/R/приложения/web_приложение/les8/Grain_data.xlsx", sheet = "Positions")
costs <- read_excel("D:/данные/разработка/R/приложения/web_приложение/les8/Grain_data.xlsx", sheet = "SiloCosts")
prices <- positions %>% group_by(Commodity) %>% summarise(Price_for_MT = mean(Price_for_MT))

# Функция для более простого отображения всплывающего диалога с информацией
infoDialog <- function (title = "Информация", text = "Текст информации", callback = function(x) { message(x) }) {
  shinyalert(
    title = title,
    text = text,
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = FALSE,
    type = "info",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
}
