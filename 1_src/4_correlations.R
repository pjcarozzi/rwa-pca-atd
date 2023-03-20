######################################
# 4.- Correlations and summary
# rsalaslewin@gmail.com
# Rev. March 2023
######################################

###---ENVIRONMENT.

rm(list=ls())
pacman::p_load(readxl, data.table, lubridate, stringr, tidyverse, dplyr, 
               labelled, ggplot2, lavaan, semPlot, psych, haven, car, rio,
               writexl)

setwd("/Users/rsalaslewin/Dropbox/LAB/Democracia y Participación/Repositorio/")

data1 <- import("0_data/data_sel_v1.csv") %>% 
  select(-ID)

data2 <- import("0_data/data_sel_v2.csv") %>% 
        select(-ID)

data3 <- import("0_data/data_sel_v3.csv") %>% 
  select(-ID)

###---CORRELATION MATRIX.

cor1 <- corr.test(data1, use = "pairwise",method="pearson")
pearson1 <- as.data.frame(cor1$r)
p1 <- as.data.frame(cor1$p)

cor2 <- corr.test(data2, use = "pairwise",method="pearson")
pearson2 <- as.data.frame(cor2$r)
p2 <- as.data.frame(cor2$p)

cor3 <- corr.test(data3, use = "pairwise",method="pearson")
pearson3 <- as.data.frame(cor3$r)
p3 <- as.data.frame(cor3$p)

sheets1 <- list("Cor" = pearson1, "P" = p1)
export(sheets1, "2_output/1_alpha/correlations_v1.xlsx", rowNames = T)

sheets2 <- list("Cor" = pearson2, "P" = p2)
export(sheets2, "2_output/1_alpha/correlations_v2.xlsx", rowNames = T)

sheets3 <- list("Cor" = pearson3, "P" = p3)
export(sheets3, "2_output/1_alpha/correlations_v3.xlsx", rowNames = T)


###---SUMMARY STATISTICS.

summary1 <- data1 %>%
            summarise(across(everything(), 
                             list(min = min, max = max, mean = mean, sd = sd), na.rm = T)) %>%
            pivot_longer(cols = everything(),
                         names_sep = "_",
                         names_to  = c("variable", ".value"))

summary2 <- data2 %>%
  summarise(across(everything(), 
                   list(min = min, max = max, mean = mean, sd = sd), na.rm = T)) %>%
  pivot_longer(cols = everything(),
               names_sep = "_",
               names_to  = c("variable", ".value"))

summary3 <- data3 %>%
  summarise(across(everything(), 
                   list(min = min, max = max, mean = mean, sd = sd), na.rm = T)) %>%
  pivot_longer(cols = everything(),
               names_sep = "_",
               names_to  = c("variable", ".value"))

export(summary1, "2_output/1_alpha/summary_v1.xlsx", rowNames = F)

export(summary2, "2_output/1_alpha/summary_v2.xlsx", rowNames = F)

export(summary3, "2_output/1_alpha/summary_v3.xlsx", rowNames = F)










