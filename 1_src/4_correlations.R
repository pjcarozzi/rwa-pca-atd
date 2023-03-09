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

cor1 <- as.data.frame(cor(data1,
                         use = "pairwise.complete.obs",
                         method = "pearson"))

cor2 <- as.data.frame(cor(data2,
                          use = "pairwise.complete.obs",
                          method = "pearson"))

export(cor1, "2_output/1_alpha/correlations_v1.xlsx", rowNames = T)

export(cor2, "2_output/1_alpha/correlations_v2.xlsx", rowNames = T)








