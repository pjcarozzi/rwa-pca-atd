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
dtv1 <- fread("0_data/data_sel_v1.csv")
dtv2 <- fread("0_data/data_sel_v2.csv")

## There are empty cases!
# table(rowSums(is.na(dt[,-1]))) # 92 cases with data in just one variable
dtv1 <- dtv1[rowSums(is.na(dtv1[,-1])) < ncol(dtv1[,-1])-1, ]
dtv2 <- dtv2[rowSums(is.na(dtv2[,-1])) < ncol(dtv2[,-1])-1, ]

# ESTIMATION ####
effects <- c("a1","a2","b1","b2","c1","c2","d1","d2")
fit_v1 <- list()
fit_v2 <- list()
for (k in names(M)){
  bw <- M[[k]][["bwcomp"]]
  vc <- M[[k]][["varcov"]]
  for (i in effects){
    ef <- M[[k]][[i]]
    fit_v1[[k]][[i]] <- lavaan(model = c(bw,ef,vc),
                            data = dtv1, 
                            estimator = "MLR", 
                            missing = "FIML",
                            meanstructure = T, 
                            int.ov.free = T)
    fit_v2[[k]][[i]] <- lavaan(model = c(bw,ef,vc),
                            data = dtv2, 
                            estimator = "MLR", 
                            missing = "FIML",
                            meanstructure = T, 
                            int.ov.free = T)
  }
}

est_v1 <- list()
for (k in names(fit_v1)){
  for (i in 1:length(fit_v1[[k]])){
    est_v1[[k]][[names(fit_v1[[k]])[i]]] <- data.table(model=names(fit_v1[[k]])[i],parameterEstimates(fit_v1[[k]][[i]]))
  }
  est_v1[[k]] <- data.table(set=k,dplyr::bind_rows(est_v1[[k]]))
}
est_v2 <- list()
for (k in names(fit_v2)){
  for (i in 1:length(fit_v2[[k]])){
    est_v2[[k]][[names(fit_v2[[k]])[i]]] <- data.table(model=names(fit_v2[[k]])[i],parameterEstimates(fit_v2[[k]][[i]]))
  }
  est_v2[[k]] <- data.table(set=k,dplyr::bind_rows(est_v2[[k]]))
}


# SAVE ####
for (i in names(est_v1)) {
  xlsx::write.xlsx(est_v1[[i]], "2_output/2_estimates/unstandardized_v1.xlsx", 
                   sheetName=i, 
                   row.names = FALSE,
                   append=TRUE)
}
for (i in names(est_v2)) {
  xlsx::write.xlsx(est_v2[[i]], "2_output/2_estimates/unstandardized_v2.xlsx", 
                   sheetName=i, 
                   row.names = FALSE,
                   append=TRUE)
}


save.image(file = "2_output/environ.RData")
