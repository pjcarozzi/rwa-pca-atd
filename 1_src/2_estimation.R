# ### ### ### ### ### ### ### ### ###
# 2.- Model Estimation
# pjcarozzi@uc.cl
# Rev. March 2023
# ### ### ### ### ### ### ### ### ###

# PREV ####
## Clean workspace ####
rm(list = ls(all.names = TRUE))

## Where am I? ####
here::i_am("1_src/2_estimation.R")

## Libraries ####
library(data.table) 
library(dplyr)
library(lavaan)

## Load Environment
load("2_output/environ.RData")

## Load Data
data <- list(fread("0_data/data_sel_v1.csv"),fread("0_data/data_sel_v2.csv"))

## There are empty cases!
# table(rowSums(is.na(data[[1]][,-1]))) # 92 cases with data in just one variable
# table(rowSums(is.na(data[[2]][,-1]))) # 92 cases with data in just one variable
for (i in 1:length(data)){
  data[[i]] <- data[[i]][rowSums(is.na(data[[i]][,-1])) < ncol(data[[i]][,-1])-1, ]
}

# ESTIMATION ####
## WHAT ARE WE ESTIMATING? ####
hypothesis <- c("med.pca","med.atd","med.rwa")
models <- c("m1","m2","m3")
effects <- c("a1","a2","b1","b2","c1","c2","d1","d2")

fit <- list()
for (d in 1:length(data)){
  dt <- data[[d]]
  
  fit[[d]] <- list()
  for (h in hypothesis){
    fit[[d]][[h]] <- list() 
    # Between and within part
    code_bw <- HYPOTH[["bw"]][[h]]
    # Varcov part (common)
    code_vc <- HYPOTH[["vc"]]
    for (m in models){
      fit[[d]][[h]][[m]] <- list()
      for (e in effects){
        # Effects
        code_ef <- MODELS[[m]][[e]]
        fit[[d]][[h]][[m]][[e]] <- lavaan(model = c(code_bw,code_ef,code_vc),
                           data = dt,
                           estimator = "MLR",
                           missing = "FIML",
                           meanstructure = T,
                           int.ov.free = T)
        }
    }
  }
  }  

save(fit, file = "2_output/all_fits.RData")


est <- list()
for (i in 1:length(fit)){
  d <- paste0("data",i,sep="")
  est[[d]] <- list()
  for (h in names(fit[[i]])){ # over hypo
    est[[d]][[h]] <- list()
    for (m in names(fit[[i]][[h]])){ # over mod
      est[[d]][[h]][[m]] <- list()
      for (e in names(fit[[i]][[h]][[m]])){ # over eff
        est[[d]][[h]][[m]][[e]] <- data.table(model=e,parameterEstimates(fit[[i]][[h]][[m]][[e]]))
      }
      est[[d]][[h]][[m]] <- data.table(set=m,dplyr::bind_rows(est[[d]][[h]][[m]]))
    }
    est[[d]][[h]] <- data.table(hypo=h,dplyr::bind_rows(est[[d]][[h]]))
  }
}

# SAVE ####
for (i in names(est)){
  file <- paste0("2_output/2_estimates/unst_",i,".xlsx")
  for (k in 1:length(est[[i]])) {
  xlsx::write.xlsx(est[[i]][[k]], file, 
                   sheetName=names(est[[i]])[k], 
                   row.names = FALSE,
                   append=TRUE)
  }
}



save.image(file = "2_output/environ.RData")
