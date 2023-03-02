# ### ### ### ### ### ### ### ### ###
# 3.- GOF
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

# GOF MEASURES ####

## Extract ####
gofdt_v1 <- list()
for (k in names(fit_v1)){
  for (i in 1:length(fit_v1[[k]])){
    x <- fitMeasures(fit_v1[[k]][[i]])[c("chisq.scaled","df.scaled","pvalue.scaled","cfi.scaled","tli.scaled","rmsea.scaled","srmr_mplus","aic","bic","bic2","logl","npar","scaling.factor.h0")]
    
    gofdt_v1[[k]][[i]] <- setNames(as.numeric(x),c("X2","df","pvalue","CFI","TLI","RMSEA","SRMR","AIC","BIC","aBIC","LL","par","LLcorrectf"))
  }
  gofdt_v1[[k]] <- data.table(m=names(fit_v1[[k]]),dplyr::bind_rows(gofdt_v1[[k]]))
}

gofdt_v2 <- list()
for (k in names(fit_v2)){
  for (i in 1:length(fit_v2[[k]])){
    x <- fitMeasures(fit_v2[[k]][[i]])[c("chisq.scaled","df.scaled","pvalue.scaled","cfi.scaled","tli.scaled","rmsea.scaled","srmr_mplus","aic","bic","bic2","logl","npar","scaling.factor.h0")]
    
    gofdt_v2[[k]][[i]] <- setNames(as.numeric(x),c("X2","df","pvalue","CFI","TLI","RMSEA","SRMR","AIC","BIC","aBIC","LL","par","LLcorrectf"))
  }
  gofdt_v2[[k]] <- data.table(m=names(fit_v2[[k]]),dplyr::bind_rows(gofdt_v2[[k]]))
}

## Compare ####
### Function
gof.comp  = function(data, pairs,
                     measures = c("CFI","TLI","RMSEA","SRMR",
                                  "AIC","BIC","aBIC","par","LL")){
  comp <- list()
  for (i in 1:length(pairs)){
    gof <- data
    nest <- pairs[[i]][1]
    full <- pairs[[i]][2]
    delta <- NULL
    for (k in measures){
      delta[paste0(k,"_D")] <- gof[m==nest, get(k)] - gof[m==full, get(k)]
    }
    par_LLcorf_nest <- gof[m==nest,par]*gof[m==nest,LLcorrectf]
    par_LLcorf_full <- gof[m==full,par]*gof[m==full,LLcorrectf]
    delta["CD"] <- (par_LLcorf_nest-par_LLcorf_full)/delta["par_D"]
    delta["TRd"] <- (-2*delta["LL_D"])/delta["CD"]
    delta["TRd_df"] <- gof[m==full, "par"] - gof[m==nest, "par"]
    delta["TRd_pvalue"] <- pchisq(as.numeric(delta["TRd"]),
                                  as.numeric(delta["TRd_df"]), lower.tail = F)
    comp[[paste0(nest," vs. ",full,sep="")]] <- delta
  }
  comp <- data.table(comp=names(comp),dplyr::bind_rows(comp))
  return(comp)
}

### Are the effects equal across time? (Model 1 vs Model 2)
comp1_v1 <- list()
for (k in names(gofdt_v1)){
  comp1_v1[[k]] <- gof.comp(data = gofdt_v1[[k]], pairs = list(c("a2","a1"),c("b2","b1"),c("c2","c1"),c("d2","d1")))
}

comp1_v2 <- list()
for (k in names(gofdt_v2)){
  comp1_v2[[k]] <- gof.comp(data = gofdt_v2[[k]], pairs = list(c("a2","a1"),c("b2","b1"),c("c2","c1"),c("d2","d1")))
}

### What is the direction of dependence? (Model A vs Model B...)
comp2_v1 <- list()
for (k in names(gofdt_v1)){
  comp2_v1[[k]] <- gof.comp(data = gofdt_v1[[k]], pairs =list(c("a2","b2"),c("a2","c2"),c("a2","d2"),c("b2","d2"),c("c2","d2")))
}

comp2_v2 <- list()
for (k in names(gofdt_v2)){
  comp2_v2[[k]] <- gof.comp(data = gofdt_v2[[k]], pairs =list(c("a2","b2"),c("a2","c2"),c("a2","d2"),c("b2","d2"),c("c2","d2")))
}


# SAVE ####
for (i in names(gofdt_v1)) {
  xlsx::write.xlsx(gofdt_v1[[i]], "2_output/3_gof/gof_v1.xlsx", 
                   sheetName=i, 
                   row.names = FALSE,
                   append=TRUE)
}

for (i in names(gofdt_v2)) {
  xlsx::write.xlsx(gofdt_v2[[i]], "2_output/3_gof/gof_v2.xlsx", 
                   sheetName=i, 
                   row.names = FALSE,
                   append=TRUE)
}

for (i in names(comp1_v1)) {
  xlsx::write.xlsx(dplyr::bind_rows(list(comp1_v1[[i]],comp2_v1[[i]])), "2_output/3_gof/compgof_v1.xlsx", 
                   sheetName=i, 
                   row.names = FALSE,
                   append=TRUE)
}

for (i in names(comp1_v2)) {
  xlsx::write.xlsx(dplyr::bind_rows(list(comp1_v2[[i]],comp2_v2[[i]])), "2_output/3_gof/compgof_v2.xlsx", 
                   sheetName=i, 
                   row.names = FALSE,
                   append=TRUE)
}

save.image(file = "2_output/environ.RData")
