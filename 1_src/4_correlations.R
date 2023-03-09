######################################
# 4.- Correlations
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

###---CORRELATION MATRIX.

cor1 <- corr.test(data1, use = "pairwise",method="pearson")
pearson1 <- as.data.frame(cor1$r)
p1 <- as.data.frame(cor1$p)

cor2 <- corr.test(data2, use = "pairwise",method="pearson")
pearson2 <- as.data.frame(cor2$r)
p2 <- as.data.frame(cor2$p)

sheets1 <- list("Cor" = pearson1, "P" = p1)
export(sheets1, "2_output/1_alpha/correlations_v1.xlsx", rowNames = T)

sheets2 <- list("Cor" = pearson2, "P" = p2)
export(sheets2, "2_output/1_alpha/correlations_v2.xlsx", rowNames = T)










