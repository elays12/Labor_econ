library(ggplot2)
library(data.table)
library(stargazer)
library(data.table)
library(lfe)
library(haven)
library(fixest)
library(tictoc)
library(tidyverse)

# load the workers database (only relevant vars)
workers = read_dta("C:\\Users\\User\\Videos\\Captures\\HW2_DATA\\anagr.dta", col_select = c("cod_pgr", "sesso", "anno_n"))
workers <- data.table(workers)
colnames(workers)<-c("worker", "gender1", "birth_year")

# make gender a numeric var (and drop if missing)
workers[gender1 == "M",gender := 0]
workers[gender1 == "F",gender := 1]
workers[,gender1 := NULL]
workers = workers[!is.na(gender)]

# load the wages data (only relevant vars)
wages = read_dta("C:/Econ Seminar/targil2/contr.dta",col_select = c("cod_pgr", "anno", "matr_az","retrib03"))
wages <- data.table(wages)
colnames(wages)<-c("worker", "year", "firm", "wage")

# load the firm data (only relevant vars)
firms = read_dta("C:/Econ Seminar/targil2/azien.dta",col_select = c("matr_az","ateco81","data_cost","data_cess"))
firms <- data.table(firms)
colnames(firms)<-c("firm","start_year","end_year","industry")

# strip the 4 first digits of the date
firms[,start_year := as.numeric(substr(start_year,1,4))]
firms[,end_year := as.numeric(substr(end_year,1,4))]

# calulate the difference in end year to start year, if end year is missing, we will use 2001
firms[end_year == 0,end_year := 2001]
firms[,operation := end_year - start_year]


# merge the datasets (wages and workers)
df <- merge(wages , workers,by="worker")
#rm(workers,wages)

# merge the datasets(firms with the combined one)
df <- merge(df , firms,by="firm")
#rm(firms)

# sampling random 400000 different workers
sampled_person_ids <- sample(df[, unique(worker)], size = 400000)
sampled_data <- df[worker %in% sampled_person_ids]

# gen vars
sampled_data[,firm_size := .N , by = .(firm,year)]
sampled_data[,age := year-birth_year]
sampled_data = sampled_data[wage > 0]
sampled_data[,lwage := log(wage)]

# keep workers aged 25-64
sampled_data = sampled_data[age >= 25 & age <= 64]

# Aggregate the number of men and women per company per year
sampled_data[, `:=`(
  men_count = sum(gender == 0),
  women_count = sum(gender == 1),
  mean_wage = mean(lwage, na.rm = TRUE)  # Calculate mean salary
  
), by = .(firm, year)]

sampled_data[, women_percentage := women_count / (men_count + women_count) * 100]


# add a column of how much money each firm paid in wages in every year
sampled_data[, `:=`(
  total_wages = sum(wage)
), by = .(firm, year)]


firm_Data <- sampled_data %>% select(firm, year, industry,women_percentage,firm_size,operation,total_wages) %>% unique()

# add a column to firm_data of the mean of total wages
firm_Data[, mean_total_wages := total_wages/firm_size]



filtered_firms <- firm_Data[firm_size >10]

# sort filtered firms by firms and year
filtered_firms <- filtered_firms[order(firm, year)]

cor(filtered_firms$women_percentage, filtered_firms$mean_total_wages)
cor(filtered_firms$women_percentage, filtered_firms$operation)