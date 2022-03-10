#install.packages("xlsx")
#install.packages("foreign")
#install.packages("haven")
#install.packages("Synth")
#install.packages("xtable")
#install.packages('tidysynth')
#install.packages("devtools")
#devtools::install_github("edunford/tidysynth")
#install.packages("GOplot")
#install.packages(c("cluster.datasets"), dependencies = TRUE)
#install.packages("ggplot2")

library("xlsx")
library(foreign)
library(Synth)
library(xtable)
library(tidysynth)
library(GOplot)
library(cluster.datasets)
library(haven)
library(ggplot2)
library(dplyr)

getwd()        
setwd("C:/Users/User/Desktop/Эконометрика")
data <- read_dta("data_cholera.dta")

#############задание 1

#Оценить эффект локального шока на стоимость аренды жилья через 10 лет после
#эпидемии как простое среднее между тритмент- и контрольной группой. Приведите
#результаты. Чему равен эффект?

#удаляю данные в которых есть пустые ячейки
data[is.na(data)] = -10

data = subset(data, data$rentals_64 != - 10)
data = subset(data, data$rentals_53 != - 10)
data = subset(data, data$broad != - 10)


help_subset = data
broad1 = subset(help_subset, broad == 1)
broad0 = subset(help_subset, broad == 0)
mean = mean(broad1$rentals_64) - mean(broad0$rentals_64)

#Среднее тритмент группы = 44.73441
#Среднее контрольной группы = 52.54823
#Эффект = -7.813823
#Арендная стоимость недвижимости в тритмент группе в среднем меньше на 7.813823 долларов, чем в контрольной группе.


#########задание 2

# Корректна ли эта оценка в данной ситуации? Почему?

model1 <- lm(help_subset$rentals_64 ~ help_subset$broad)
summary (model1)

t.test(help_subset$rentals_53[help_subset$broad == 1], help_subset$rentals_53[help_subset$broad == 0])
#p-value < 0,05 => отвергаем Н0 => по арендной цене недвижимости в 1853 году группы неодинаковые

t.test(help_subset$dist_cent[help_subset$broad == 1], help_subset$dist_cent[help_subset$broad == 0])
#p-value < 0,05 => отвергаем Н0 => по расстоянию до центра группы неодинаковые

#Нет, не корректна
#Проведя 2 t-теста Стьюдента (аренда недвижимости в 1853 и расстояние от центра), сделала вывод, что p-value < 0,05 => H0 отвергаю => группы неодинаковые по данным показателям
#Например, средняя арендная плата (broad = 1) в 1853 = 44.00308, а в (broad = 0) = 50.97970. Разница составляет -6.97662, что также примерно равно разницу в 1864 году. 

#############задание 3

#На основе расстояния до границы создайте «бегущую переменную». 
#Постройте диаграмму разброса стоимости аренды жилья через 10 лет после эпидемии от «бегущей переменной»

help_subset = subset(help_subset, help_subset$dist_netw != - 10)

#делаю расстояние внутри зоны отрицательными
i = 1
while  (i <= 1324)
{
  if (help_subset[i,29] == 1)
  {
    help_subset[i,31] = -help_subset[i,31]
  }
  i = i + 1
}

#install.packages("rddtools")
library(rddtools)

data_3 <- rdd_data(help_subset$rentals_64, help_subset$dist_netw, cutpoint = 0)

plot(data_3,
     col = "red",
     cex = 1, 
     xlab = "Расстояние от дома до холерной границы, м.", 
     ylab = "Арендная стоимость недвижимости через 10 лет после эпидемии, долл.")


####################задание 4

#Оцените МНК-регрессию для стоимость аренды жилья через 10 лет после эпидемии,
#которая учитывала бы разрыв по границе. 
#Есть ли эффект и чему он равен? 
                                                                        

rentals_64 <- help_subset$rentals_64
dist_netw <- help_subset$dist_netw
rentals_53 <- help_subset$rentals_53

data4 <- data.frame(rentals_64, dist_netw, rentals_53)

rdd_mod_4 <- rdrobust(data4$rentals_64, data4$dist_netw, c = 0)
summary(rdd_mod_4)

#Эффект есть, он равен 13.048

#################задание 5

#Корректна ли эта оценка в данной ситуации? Почему?

#делаю плацебо-тест

data_5_placebo <- rdrobust(data4$rentals_53, data4$dist_netw, c = 0)
summary(data_5_placebo)

#P-value < 0.01, коэффициент значимый на уровне 99%.
#Если провести плацебо-тест (зависимая переменная – rentals_53), то получим p-value < 0.268 / p-value < 0.257 => коэффициенты незначимые=>оценка корректна


################задание 6

#Оцените МНК-регрессию для стоимость аренды жилья через 10 лет после эпидемии,
#которая учитывала бы разрыв по границе, оставив только дома в пределах 100 м от
#границы. Есть ли эффект и чему он равен?

data6<- subset(help_subset, help_subset$dist_netw <= 100)
data6<- subset(help_subset, help_subset$dist_netw >= -100)
mod_6 <- rdrobust(data6$rentals_64, data6$dist_netw, c = 0)
summary(mod_6)

#Эффект есть, равен 13.251. P-value < 0.05, коэффициент значимый на уровне 95%

##############задание 7

#Оцените разрывную регрессию с треугольным ядром, ядром Епанечникова, прямоугольным ядром. 
#Есть ли эффект и чему он равен?

#install.packages("rdd")
library(rdd)

rentals_64 <- help_subset$rentals_64
dist_netw <- help_subset$dist_netw
data7 <- data.frame(rentals_64, dist_netw)

#прямоугольное
uniform <- rdrobust(data7$rentals_64, data7$dist_netw, c = 0, kernel = "uniform") 
summary(uniform)

#треугольный
triangular <- rdrobust(data7$rentals_64, data7$dist_netw, c = 0, kernel = "triangular") 
summary(triangular)

#Епанечиков
epanechnikov <- rdrobust(data7$rentals_64, data7$dist_netw, c = 0, kernel = "epanechnikov") 
summary(epanechnikov)

#Значения изменились незначительно, причина – в распределении весов выбросов. 
#При треугольном ядре - выбросы имеют меньший вес линейно, при ядре Епанечникова – выбросы имеют меньший вес “по кривой”, 
#при прямоугольном  - выбросы имеют тот же вес, что и “невыбросы”.

#Ядро	          Эффект	  Уровень значимости
#Задание 4  	  13.048	  99%
#Задание 6	    13.251	  95%
#Прямоугольное	11.451	  95%
#Треугольное	  13.048	  99%
#Епанечников	  12.802	  95%


##########задание 8

#Зачем авторы статьи приводят таблицу B3? 
#Сделайте аналогичную процедуру только для стоимости аренды жилья в 1864 г., 
#адаптировав под имеющиеся у Вас данные
#и интерпретируйте результат.

#Таблица B3 показывает, что если использовать другие границы, 
#которые не связаны никак с территорией зараженного колодца, 
#из-за которого началась эпидемия, то «эффект границы» исчезает, 
#то есть ложные границы не создают разрыв в цене за аренду.
#Исследователи исключили те дома, где broad = 1 (я сделала то же самое) => 
#все значения расстояния от границы теперь положительные

rentals_64 <- help_subset$rentals_64
dist_netw <- help_subset$dist_netw
rentals_53 <- help_subset$rentals_53
broad <- help_subset$broad
data10 <- data.frame(rentals_64, rentals_53, dist_netw, broad)
data10 <- subset(data10, data$broad == 0)

#cutoff = 20
mod_20 <- rdrobust(data10$rentals_64, data10$dist_netw, c = 20)
summary(mod_20)

#	Если разрыв = 20, коэффициент незначимый согласно p-value

#cutoff = 40
mod_40 <- rdrobust(data10$rentals_64, data10$dist_netw, c = 40)
summary(mod_40)

#	Если разрыв = 40, коэффициент незначимый согласно p-value

#cutoff = 100
mod_100 <- rdrobust(data10$rentals_64, data10$dist_netw, c = 100)
summary(mod_100)

#	Если разрыв = 100, коэффициент незначимый согласно p-value

#Действительно, различные false границы не создают разрывов в 
#цене за аренду => разрыв арендной платы по true границы существует 
#и не случаен. 
