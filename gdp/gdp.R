#install.packages("xlsx")
#install.packages("foreign")
#install.packages("Synth")
#install.packages("xtable")
#install.packages('tidysynth')
#install.packages("devtools")
#devtools::install_github("edunford/tidysynth")
#install.packages("GOplot")
#install.packages(c("cluster.datasets"), dependencies = TRUE)

library("xlsx")
library(foreign)
library(Synth)
library(xtable)
library(tidysynth)
library(GOplot)
library(cluster.datasets)

getwd()        
setwd("C:/Users/User/Downloads")
data1 <- read.csv("C:/Users/User/Downloads/data.csv", sep = ",", dec = ".", header = TRUE)
attach(data1)

#задание 1
#Повторить рисунок 2 авторов для любых двух (на ваш выбор) 
#из 7 рассматриваемых стран для темпов роста ВВП.


#убираю страны, по которым не буду делать выборку и которые убрали исследователи
data_sub <- subset(data1, data1$Country != "Cabo Verde")
data_sub <- subset(data_sub, data_sub$Country != "Mozambique")
data_sub <- subset(data_sub, data_sub$Country != "Rwanda")
data_sub <- subset(data_sub, data_sub$Country != "Senegal")
data_sub <- subset(data_sub, data_sub$Country != "Tanzania")
data_sub <- subset(data_sub, data_sub$Country != "Uganda")
data_sub <- subset(data_sub, data_sub$Country != "Barbados")
data_sub <- subset(data_sub, data_sub$Country != "Equatorial Guinea")
data_sub <- subset(data_sub, data_sub$Country != "Kiribati")
data_sub <- subset(data_sub, data_sub$Country != "Kuwait")
data_sub <- subset(data_sub, data_sub$Country != "Marshall Islands")
data_sub <- subset(data_sub, data_sub$Country != "Micronesia")
data_sub <- subset(data_sub, data_sub$Country != "Montenegro")
data_sub <- subset(data_sub, data_sub$Country != "Myanmar")
data_sub <- subset(data_sub, data_sub$Country != "Palau")
data_sub <- subset(data_sub, data_sub$Country != "Qatar")
data_sub <- subset(data_sub, data_sub$Country != "Samoa")
data_sub <- subset(data_sub, data_sub$Country != "Syria")
data_sub <- subset(data_sub, data_sub$Country != "Timor-Leste")
data_sub <- subset(data_sub, data_sub$Country != "Turkmenistan")
data_sub <- subset(data_sub, data_sub$Country != "Tuvalu")
data_sub <- subset(data_sub, data_sub$Country != "United Arab Emirates")
data_Nigeria <- data_sub

#сортирую страны для tidysynth
#Нигерия
data_Nigeria<- data_Nigeria[order(data_Nigeria$Year),]
data_Nigeria<- data_Nigeria[order(data_Nigeria$Country),]
require(tidysynth)

data_Nigeria %>% dplyr::glimpse()

data_out_Nigeria<- data_Nigeria %>%
  synthetic_control(outcome = Real.GDP,
                    time = Year,
                    unit = Country,
                    i_unit = "Nigeria",
                    i_time = 2006,
                    generate_placebos=T) %>%
  
  generate_predictor(time_window = 1992:2006,
                     investment.rate = mean(investment.rate, na.rm = T),
                     openness = mean(openness, na.rm = T),
                     population.density = mean(population.density, na.rm = T),
                     share.of.agriculture = mean(share.of.agriculture, na.rm = T),
                     share.of.industry = mean(share.of.industry, na.rm = T),
                     sec.school.enr.rate = mean(sec.school.enr.rate, na.rm = T),
                     tert.school.enr.rate = mean(tert.school.enr.rate, na.rm = T),
                     latitude=mean(latitude, na.rm = T)) %>%
  
  generate_predictor(time_window = 1995,
                     GDP_1995 = Real.GDP) %>%
  generate_predictor(time_window = 2000,
                     GDP_2000 = Real.GDP) %>%
  generate_predictor(time_window = 2005,
                     GDP_2005 = Real.GDP) %>%
  
  generate_weights(optimization_window = 1992:2006,
                   margin_ipop = 0.02, sign_ipop = 7, bound_ipop = 6) %>%
  
  generate_control()


data_out_Nigeria %>% plot_trends() #gdp1


#заново загружаю данные чтобы получить нерассортированную таблицу
getwd()        
setwd("C:/Users/User/Downloads")
data1 <- read.csv("C:/Users/User/Downloads/data.csv", sep = ",", dec = ".", header = TRUE)
attach(data1)
data_sub <- subset(data1, data1$Country != "Cabo Verde")
data_sub <- subset(data_sub, data_sub$Country != "Mozambique")
data_sub <- subset(data_sub, data_sub$Country != "Rwanda")
data_sub <- subset(data_sub, data_sub$Country != "Senegal")
data_sub <- subset(data_sub, data_sub$Country != "Nigeria")
data_sub <- subset(data_sub, data_sub$Country != "Uganda")
data_sub <- subset(data_sub, data_sub$Country != "Barbados")
data_sub <- subset(data_sub, data_sub$Country != "Equatorial Guinea")
data_sub <- subset(data_sub, data_sub$Country != "Kiribati")
data_sub <- subset(data_sub, data_sub$Country != "Kuwait")
data_sub <- subset(data_sub, data_sub$Country != "Marshall Islands")
data_sub <- subset(data_sub, data_sub$Country != "Micronesia")
data_sub <- subset(data_sub, data_sub$Country != "Montenegro")
data_sub <- subset(data_sub, data_sub$Country != "Myanmar")
data_sub <- subset(data_sub, data_sub$Country != "Palau")
data_sub <- subset(data_sub, data_sub$Country != "Qatar")
data_sub <- subset(data_sub, data_sub$Country != "Samoa")
data_sub <- subset(data_sub, data_sub$Country != "Syria")
data_sub <- subset(data_sub, data_sub$Country != "Timor-Leste")
data_sub <- subset(data_sub, data_sub$Country != "Turkmenistan")
data_sub <- subset(data_sub, data_sub$Country != "Tuvalu")
data_sub <- subset(data_sub, data_sub$Country != "United Arab Emirates")
data_Tanzania <- data_sub
data_Tanzania<- data_Tanzania[order(data_Tanzania$Year),]
data_Tanzania<- data_Tanzania[order(data_Tanzania$Country),]
require(tidysynth)

#Танзания
data_Tanzania %>% dplyr::glimpse()

data_out_Tanzania<- data_Tanzania %>%
  synthetic_control(outcome = Real.GDP,
                    time = Year,
                    unit = Country,
                    i_unit = "Tanzania",
                    i_time = 2007,
                    generate_placebos=T) %>%
  
  generate_predictor(time_window = 1992:2007,
                     investment.rate = mean(investment.rate, na.rm = T),
                     openness = mean(openness, na.rm = T),
                     population.density = mean(population.density, na.rm = T),
                     share.of.agriculture = mean(share.of.agriculture, na.rm = T),
                     share.of.industry = mean(share.of.industry, na.rm = T),
                     sec.school.enr.rate = mean(sec.school.enr.rate, na.rm = T),
                     tert.school.enr.rate = mean(tert.school.enr.rate, na.rm = T),
                     latitude=mean(latitude, na.rm = T)) %>%
  
  generate_predictor(time_window = 1994,
                     GDP_1994 = Real.GDP) %>%
  generate_predictor(time_window = 2000,
                     GDP_2000 = Real.GDP) %>%
  generate_predictor(time_window = 2006,
                     GDP_2006 = Real.GDP) %>%
  
  generate_weights(optimization_window = 1992:2007,
                   margin_ipop = 0.02, sign_ipop = 7, bound_ipop = 6) %>%
  
  generate_control()


plot_Tanzania<-data_out_Tanzania %>% plot_trends() #gdp2

#задание 2
#Сделать аналог таблицы A2 (кроме последней строчки) из статьи 
#для ваших расчётов.
library(dplyr)
Tanz<-data_out_Tanzania %>% grab_unit_weights()
Nig<-data_out_Nigeria %>% grab_unit_weights()

Tanz[c(2)]<-round(Tanz[c(2)],3)
Nig[c(2)]<-round(Nig[c(2)],3)


#задание 3
#Рассчитайть «Fit Index» для Вашего случая.

Tan<-c("Tanzania","Nigeria")

#Танзания  
T<-data_out_Tanzania %>% grab_synthetic_control()
Tanzania_RMSPE<-sqrt(sum((T[2]-T[3])^2)/(2007-1992))
Tanzania_fit<-round(Tanzania_RMSPE/sqrt(sum((T[2])^2)/(2007-1992)),3)

#Нигерия

N<-data_out_Nigeria %>% grab_synthetic_control()
Nigeria_RMSPE<-sqrt(sum((N[2]-N[3])^2)/(2006-1992))
Nigeria_fit<-round(Nigeria_RMSPE/sqrt(sum((N[2])^2)/(2006-1992)),3)

fit<-c(Tanzania_fit,Nigeria_fit)
names(fit)<-Tan

#Танзания: 0,063
#Нигерия: 0,114

#задача4

#Создайте график, который показывает, 
#насколько субъекты похожи на объект по синтетическому контролю.

library(ggplot2)
data_out_Tanzania %>%plot_mspe_ratio()+ geom_hline(yintercept=1) #gdp3
data_out_Nigeria%>%plot_mspe_ratio()+ geom_hline(yintercept=1) #gdp4

#В Танзании вмешательство МВФ не привело ни к какому эффекту, 
#так как post/pre близкое к 
#единице, а в Нигерии post/pre значительно выше 1, 
#поэтому PSI имело положительный эффект на ВВП Нигерии.

#задача 5

#Сделайть аналог части таблицы А6 для ваших расчётов, 
#добавить также средние значения показателей для набора "донорских"стран.

Tanz5<-grab_balance_table(data_out_Tanzania)[1:3]
Nig5<-grab_balance_table(data_out_Nigeria)[1:3]

Tanz5[2:3]<-round(Tanz5[2:3],3)
Nig5[2:3]<-round(Nig5[2:3],3)


#задача 6 

#Повторить действия авторов для рисунка 3

data_out_Tanzania %>%plot_placebos(prune = FALSE) #gdp5
data_out_Nigeria%>%plot_placebos(prune = FALSE) #gdp6

#В случае Танзании, то после получения трита ВВП не отличается от стран-плацебо, а то время как в 
#Нигерии показатель ВВП после получения трита значительно улучшается.

#задание 7 (код ниже не показывает warnings если в environment и plots нажать на clean objects from workspace (метелка))

#Проверить отсутствие структурных сдвигов до момента начала
#программы PSI. Привести график 

data1 <- read.csv("C:/Users/User/Downloads/data.csv", sep = ",", dec = ".", header = TRUE)

data_sub <- subset(data1, data1$Country != "Cabo Verde")
data_sub <- subset(data_sub, data_sub$Country != "Mozambique")
data_sub <- subset(data_sub, data_sub$Country != "Rwanda")
data_sub <- subset(data_sub, data_sub$Country != "Senegal")
data_sub <- subset(data_sub, data_sub$Country != "Nigeria")
data_sub <- subset(data_sub, data_sub$Country != "Uganda")
data_sub <- subset(data_sub, data_sub$Country != "Barbados")
data_sub <- subset(data_sub, data_sub$Country != "Equatorial Guinea")
data_sub <- subset(data_sub, data_sub$Country != "Kiribati")
data_sub <- subset(data_sub, data_sub$Country != "Kuwait")
data_sub <- subset(data_sub, data_sub$Country != "Marshall Islands")
data_sub <- subset(data_sub, data_sub$Country != "Micronesia")
data_sub <- subset(data_sub, data_sub$Country != "Montenegro")
data_sub <- subset(data_sub, data_sub$Country != "Myanmar")
data_sub <- subset(data_sub, data_sub$Country != "Palau")
data_sub <- subset(data_sub, data_sub$Country != "Qatar")
data_sub <- subset(data_sub, data_sub$Country != "Samoa")
data_sub <- subset(data_sub, data_sub$Country != "Syria")
data_sub <- subset(data_sub, data_sub$Country != "Timor-Leste")
data_sub <- subset(data_sub, data_sub$Country != "Turkmenistan")
data_sub <- subset(data_sub, data_sub$Country != "Tuvalu")
data_sub <- subset(data_sub, data_sub$Country != "United Arab Emirates")
data_Tanzania <- data_sub
data_Tanzania<- data_Tanzania[order(data_Tanzania$Year),]
data_Tanzania<- data_Tanzania[order(data_Tanzania$Country),]
require(tidysynth)

#Танзания

data_Tanzania %>% dplyr::glimpse()

Tanzania_2006<- data_Tanzania %>%
  synthetic_control(outcome = Real.GDP,
                    time = Year,
                    unit = Country,
                    i_unit = "Tanzania",
                    i_time = 2006,
                    generate_placebos=T) %>%
  
  generate_predictor(time_window = 1992:2006,
                     investment.rate = mean(investment.rate, na.rm = T),
                     openness = mean(openness, na.rm = T),
                     population.density = mean(population.density, na.rm = T),
                     share.of.agriculture = mean(share.of.agriculture, na.rm = T),
                     share.of.industry = mean(share.of.industry, na.rm = T),
                     sec.school.enr.rate = mean(sec.school.enr.rate, na.rm = T),
                     tert.school.enr.rate = mean(tert.school.enr.rate, na.rm = T),
                     latitude=mean(latitude, na.rm = T)) %>%
  
  generate_predictor(time_window = 1995,
                     GDP_1995 = Real.GDP) %>%
  generate_predictor(time_window = 2000,
                     GDP_2000 = Real.GDP) %>%
  generate_predictor(time_window = 2005,
                     GDP_2005 = Real.GDP) %>%
  generate_weights(optimization_window = 1992:2006,
                   margin_ipop = 0.02, sign_ipop = 7, bound_ipop = 6) %>%
  
  generate_control()
Tanzania_2006 %>% plot_trends() #gdp7

#Нигерия
data1 <- read.csv("C:/Users/User/Downloads/data.csv", sep = ",", dec = ".", header = TRUE)
#убираю страны, по которым не буду делать выборку и которые убрали исследователи
data_sub <- subset(data1, data1$Country != "Cabo Verde")
data_sub <- subset(data_sub, data_sub$Country != "Mozambique")
data_sub <- subset(data_sub, data_sub$Country != "Rwanda")
data_sub <- subset(data_sub, data_sub$Country != "Senegal")
data_sub <- subset(data_sub, data_sub$Country != "Tanzania")
data_sub <- subset(data_sub, data_sub$Country != "Uganda")
data_sub <- subset(data_sub, data_sub$Country != "Barbados")
data_sub <- subset(data_sub, data_sub$Country != "Equatorial Guinea")
data_sub <- subset(data_sub, data_sub$Country != "Kiribati")
data_sub <- subset(data_sub, data_sub$Country != "Kuwait")
data_sub <- subset(data_sub, data_sub$Country != "Marshall Islands")
data_sub <- subset(data_sub, data_sub$Country != "Micronesia")
data_sub <- subset(data_sub, data_sub$Country != "Montenegro")
data_sub <- subset(data_sub, data_sub$Country != "Myanmar")
data_sub <- subset(data_sub, data_sub$Country != "Palau")
data_sub <- subset(data_sub, data_sub$Country != "Qatar")
data_sub <- subset(data_sub, data_sub$Country != "Samoa")
data_sub <- subset(data_sub, data_sub$Country != "Syria")
data_sub <- subset(data_sub, data_sub$Country != "Timor-Leste")
data_sub <- subset(data_sub, data_sub$Country != "Turkmenistan")
data_sub <- subset(data_sub, data_sub$Country != "Tuvalu")
data_sub <- subset(data_sub, data_sub$Country != "United Arab Emirates")
data_Nigeria <- data_sub

data_Nigeria<- data_Nigeria[order(data_Nigeria$Year),]
data_Nigeria<- data_Nigeria[order(data_Nigeria$Country),]
require(tidysynth)

data_Nigeria %>% dplyr::glimpse()


Nigeria_2005<- data_Nigeria %>%
  synthetic_control(outcome = Real.GDP,
                    time = Year,
                    unit = Country,
                    i_unit = "Nigeria",
                    i_time = 2005,
                    generate_placebos=T) %>%
  
  generate_predictor(time_window = 1992:2005,
                     investment.rate = mean(investment.rate, na.rm = T),
                     openness = mean(openness, na.rm = T),
                     population.density = mean(population.density, na.rm = T),
                     share.of.agriculture = mean(share.of.agriculture, na.rm = T),
                     share.of.industry = mean(share.of.industry, na.rm = T),
                     sec.school.enr.rate = mean(sec.school.enr.rate, na.rm = T),
                     tert.school.enr.rate = mean(tert.school.enr.rate, na.rm = T),
                     latitude=mean(latitude, na.rm = T)) %>%
  
  generate_predictor(time_window = 1995,
                     GDP_1995 = Real.GDP) %>%
  generate_predictor(time_window = 2000,
                     GDP_2000 = Real.GDP) %>%
  generate_predictor(time_window = 2004,
                     GDP_2004 = Real.GDP) %>%
  generate_weights(optimization_window = 1992:2005,
                   margin_ipop = 0.02, sign_ipop = 7, bound_ipop = 6) %>%
  
  generate_control()
Nigeria_2005 %>% plot_trends() #gdp8

#При сравнении этих графиков и графиков из пункта 1 можно сделать вывод, что структурные 
#сдвиги отсутствуют, так как графики совпадают
