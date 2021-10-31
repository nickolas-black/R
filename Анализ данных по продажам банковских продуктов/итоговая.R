# загрузка необходимых библиотек

library(readr)
library(tidyr)
library(stringr)
library(outliers)
library(dplyr)
library(ggplot2)
library(plotly)

# Загрузка данных
bank_full <- read.csv("D:/данные/разработка/R/итоговая/Diploma datasets/bank-full.csv", header=TRUE,sep=";")
usloviy <- read.csv("D:/данные/разработка/R/итоговая/Diploma datasets/deposits-tariff.csv",  header=TRUE,sep=";")
test_bank <- read.csv("D:/данные/разработка/R/итоговая/Diploma datasets/bank-test.csv", header=TRUE,sep=";")
View(test_bank)
View(usloviy)
View(bank_full)
## Посмотрим названия колонок

colnames(test_bank)
colnames(usloviy)

#Очистка данных
## для удобаства очистки переместим в clear_bank данные, чтоб неизменить изначальные данные в проуессе проверки
clear_bank <- bank_full

## проверим какие классы содержатьсья в данных
sapply(clear_bank, class)
sapply(test_bank, class)

## проверим на пропуски данные
sum(is.na(clear_bank))
sum(is.null(clear_bank))
## как видно есть 5 пропусков, проверим в каком именно столбце они есть

sum(is.na(clear_bank["age"]))
## видно что 5 пропусков находяться в age, но давайте посмотрим и на другие столбцы
sum(is.na(clear_bank["job"]))
sum(is.na(clear_bank["marital"]))
sum(is.na(clear_bank["education"]))
sum(is.na(clear_bank["default"]))
sum(is.na(clear_bank["balance"]))
sum(is.na(clear_bank["housing"]))
sum(is.na(clear_bank["loan"]))
sum(is.na(clear_bank["salary"]))
sum(is.na(clear_bank["contact"]))
sum(is.na(clear_bank["day"]))
sum(is.na(clear_bank["month"]))
sum(is.na(clear_bank["duration"]))
sum(is.na(clear_bank["campaign"]))
sum(is.na(clear_bank["pdays"]))
sum(is.na(clear_bank["previous"]))
sum(is.na(clear_bank["poutcome"]))
sum(is.na(clear_bank["y"]))
sum(is.na(clear_bank["deposittype"]))

sum(is.null(clear_bank))

## как видно Na только в age и составляет 5 из 45 211. 5 шт. это меньше 1 %, поэтому можно их просто убарть.

clear_bank <- drop_na(clear_bank)

sum(is.na(clear_bank))

## Посмотрим на получившиеся данные и посмотрим на описательную статистику используя функцию summary.
summary(clear_bank)

outlier(clear_bank$'age')

## видно что есть аномальное значение в age (-100), что мало вероятно, поэтому удалим его так как 100 лет, 
## тоже мало вероятно, что это верно. Скорее всего это выброс и посмотрим какие еще есть "нетепичные" возраста

View(clear_bank)
## визуально увидели еще два не типичных возраста -9 и -1, которые тоже удалим

clear_bank <- clear_bank[-c(2059, 14639, 22284),]

## проверим еще раз описательную статистику

summary(clear_bank)

## максимальный возраст 95 лет, конечно сомнительно, но возможно. допустим, что возможно.

## получившиеся данные очищены. Теперь приступим к трансформированию и объединению данных с данными по маркетингу
## объелинил через merge
c <- merge(clear_bank, usloviy, by="deposittype")
## второй вариант через left_join
itog <- left_join(clear_bank, usloviy, by = "deposittype")

sum(is.na(itog))

itog_ob <- itog

itog <- drop_na(itog)

sum(is.na(c))

## Посмотрим что получилось, какие колонки

colnames(itog)

## Итого получилось, что взяли депозит 5288

## Провести визуальный анализ данных: построить диаграммы, показывающие результаты маркетинговой компании. 

##Посмотрим сколько у нас больше выдано клиенту в итоге макретинга
## разделил на 3 группы, чтоб если потребуется узнать отдельную информацию по определенной группе не потерять
## но это не обязательно было делать

max_vzjli <- filter(itog, deposittype == "max")
optimal_vzjli <- filter(itog, deposittype == "optimum")
saving_vzjli <- filter(itog, deposittype == "saving")


obshee_max <- itog %>% group_by(deposittype) %>% summarise(sred_balanc = mean(balance), sred_vozrast = mean(age), kol = n())

## Посмотрим как у нас проявляется ставка от возраста

ggplot(data = itog, mapping = aes(x = age, y = drate, color = deposittype)) + geom_point()

## Корреляция между длительностью общения и возрастом

ggplot(data = itog, mapping = aes(x = age, y = duration.x, color = deposittype)) + geom_point() +geom_smooth() + labs(x = "возраст", y = "длительность последней коммуникации в сек.", color =' продукт') + ggtitle("Корреляция между общением и возрастом, с указанием как был выбран продукт")


## Зависимость продукта от баланса

ggplot(itog) + geom_point(mapping = aes(x = deposittype, y = balance, color = deposittype)) + ggtitle("Зависимость выбранного продукта от баланса")

ggplot(itog_ob) + geom_point(mapping = aes(x = deposittype, y = balance, color = deposittype)) + ggtitle("Общая зависимость выбранного продукта от баланса по продукту")

## Зависимости количество контакта от выбранного продукта

ggplot(itog) + geom_line(mapping = aes(y = deposittype, x = campaign, color = deposittype)) + ggtitle("Количество контакта от выбранного продукта")

## Зависимости количество выбранных продуктов от клиентов с которыми ранее не было общения

ggplot(itog) + geom_line(mapping = aes(x = deposittype, y = previous, color = deposittype)) + ggtitle("Количество выбранных продуктов от клиентов с которыми ранее не было общения")

## Построим гистограмму распределения клиентов по возрасту 

ggplot(itog_ob, mapping = aes(x = age)) + geom_histogram(color = "white") + ggtitle("Все данные по возрасту")

ggplot(itog, mapping = aes(x = age)) + geom_histogram(color = "white") + ggtitle("Данные по возрасту, которые взяли")

## Построим гистограмму распределения длительности последней коммуникации в секундах

ggplot(itog, mapping = aes(x = duration.x)) + geom_histogram(color = "white") + ggtitle("Взятые продукты и число длительности последней коммуникаци")

ggplot(itog_ob, mapping = aes(x = duration.x)) + geom_histogram(color = "white") + ggtitle("Общее число длительности последней коммуникаци")

## Построим гистограмму распределения когда в месяце была последняя коммуникация 
ggplot(itog, mapping = aes(x = day)) + geom_histogram(color = "white") + ggtitle("Только взятые продукты")

ggplot(itog_ob, mapping = aes(x = day)) + geom_histogram(color = "white") + ggtitle("Общие данные")


##Рассчитать и вывести показатели эффективности маркетинговой кампании: сколько процентов коммуникаций приводит к успеху,
##после какой коммуникации в среднем клиент берет новый продукт, какая средняя длительность успешной и неуспешной коммуникации) 

## Посмотрим на количество контактов с клиентом в рамках данной рекламной кампании 
ggplot(itog_ob, mapping = aes(x = campaign)) + geom_histogram(color = "white") + ggtitle("Общие данные")

ggplot(itog, mapping = aes(x = campaign)) + geom_histogram(color = "white") + ggtitle("Только взятые продукты")

ggplot(filter(itog, marital %in% c("married")), mapping = aes(x = campaign)) + geom_histogram(color = "white") + ggtitle("Только взятые продукты с учетом семейного положения(женаты)")

ggplot(filter(itog, marital %in% c("divorced")), mapping = aes(x = campaign)) + geom_histogram(color = "white") + ggtitle("Только взятые продукты с учетом семейного положения(в разводе)")

ggplot(filter(itog, marital %in% c("single")), mapping = aes(x = campaign)) + geom_histogram(color = "white") + ggtitle("Только взятые продукты с учетом семейного положения(холосты)")

##Количество контактов в среднем
ggplot(itog_ob %>% group_by(y) %>% summarise(sred_kontakt = mean(campaign))) + geom_col(mapping = aes(x = y, y = sred_kontakt, fill = y)) + ggtitle("Количество контактов с клиентом в рамках данной рекламной кампании в среднем")
ggplot(itog_ob %>% group_by(deposittype) %>% summarise(sred_kontakt = mean(campaign))) + geom_col(mapping = aes(x = deposittype, y = sred_kontakt, fill = deposittype)) + ggtitle("Количество контактов с клиентом в рамках данной рекламной кампании в среднем по продуктам")
kontakt <- itog_ob %>% group_by(deposittype) %>% summarise(sred_kontakt = mean(campaign))
kontakt
##Количество успешных контактов
ggplot(itog_ob) + geom_col(mapping = aes(x = y, y = campaign/sum(campaign) * 100)) + ggtitle("Проценты приведших к успеху")
uspeh <- itog_ob %>% group_by(y) %>% summarise(uspeh_proc = sum(campaign))
uspeh

ggplot(itog_ob %>% group_by(y) %>% summarise(sred_dlit = mean(duration.x))) + geom_col(mapping = aes(x = y, y = sred_dlit, fill = y)) + ggtitle("Средняя длительность успешной и неуспешной коммуникации")


## число дней с прошлой коммуникации 

ggplot(itog, mapping = aes(x = pdays)) + geom_histogram(color = "white")
ggplot(itog, mapping = aes(x = previous)) + geom_histogram(color = "white")
## большое количесто -1, что означает, что ранее с клиентом не связывались  и число 

## Портрет клиента

ggplot(itog) + geom_bar(mapping = aes(x = job, fill=y), position = 'dodge') + ggtitle("Количество видов занятости, взявщих продукт")

ggplot(itog) + geom_bar(mapping = aes(x = job, fill=deposittype), position = 'dodge') + ggtitle("Количество видов занятости, взявщих по видам продукт")

ggplot(itog) + geom_bar(mapping = aes(x = marital, fill=y), position = 'dodge') + ggtitle("Количество семейного состояния, взявщих продукт")  

ggplot(itog) + geom_bar(mapping = aes(x = marital, fill=deposittype), position = 'dodge') + ggtitle("Количество семейного состояния, взявщих по видам продукт")  

ggplot(itog) + geom_bar(mapping = aes(x = education, fill=deposittype), position = 'dodge') + ggtitle("Количество образования, взявщих во видам продукт")

  
  
  