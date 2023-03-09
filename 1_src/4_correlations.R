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

data <- import("0_data/data_sel_v2.csv") %>% 
        select(-ID)

###---CORRELATION MATRIX.

cor <- as.data.frame(cor(data,
                         use = "pairwise.complete.obs",
                         method = "pearson"))

export(cor, "2_output/1_alpha/correlations.xlsx", rowNames = T)








