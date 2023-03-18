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
load("2_output/all_fits.RData")

# GOF MEASURES ####

## Extract ####
gofdt <- list()
for (i in 1:length(fit)){
  d <- paste0("data",i,sep="")
  gofdt[[d]] <- list()
  for (h in names(fit[[i]])){ # over hypo
    gofdt[[d]][[h]] <- list()
    for (m in names(fit[[i]][[h]])){ # over mod
      gofdt[[d]][[h]][[m]] <- list()
      for (e in names(fit[[i]][[h]][[m]])){ # over eff
        x <- fitMeasures(fit[[i]][[h]][[m]][[e]])[c("chisq.scaled","df.scaled","pvalue.scaled","cfi.scaled","tli.scaled","rmsea.scaled","srmr_mplus","aic","bic","bic2","logl","npar","scaling.factor.h0")]
        gofdt[[d]][[h]][[m]][[e]] <- setNames(as.numeric(x),c("X2","df","pvalue","CFI","TLI","RMSEA","SRMR","AIC","BIC","aBIC","LL","par","LLcorrectf"))
      }
      gofdt[[d]][[h]][[m]] <- data.table(s=m,m=names(fit[[i]][[h]][[m]]),dplyr::bind_rows(gofdt[[d]][[h]][[m]]))
    }
  }
}
save(gofdt, file = "2_output/gofdt.RData")


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
comp1 <- list()
for (i in 1:length(fit)){
  d <- paste0("data",i,sep="")
  comp1[[d]] <- list()
  for (h in names(fit[[i]])){ # over hypo
    comp1[[d]][[h]] <- list()
    for (m in names(fit[[i]][[h]])){ # over mod
      comp1[[d]][[h]][[m]] <- data.table(s=m,
                                         gof.comp(data = gofdt[[d]][[h]][[m]], 
                                                  pairs = list(c("a2","a1"),c("b2","b1"),c("c2","c1"),c("d2","d1"))))
      }
    comp1[[d]][[h]] <- dplyr::bind_rows(comp1[[d]][[h]])
  }
}

### What is the direction of dependence? (Model A vs Model B...)
comp2 <- list()
for (i in 1:length(fit)){
  d <- paste0("data",i,sep="")
  comp2[[d]] <- list()
  for (h in names(fit[[i]])){ # over hypo
    comp2[[d]][[h]] <- list()
    for (m in names(fit[[i]][[h]])){ # over mod
      comp2[[d]][[h]][[m]] <- data.table(s=m,
                                         gof.comp(data = gofdt[[d]][[h]][[m]], 
                                                  pairs = list(c("a2","b2"),c("a2","c2"),c("a2","d2"),c("b2","d2"),c("c2","d2"))))
    }
    comp2[[d]][[h]] <- dplyr::bind_rows(comp2[[d]][[h]])
  }
}


# SAVE ####

# SAVE ####
for (i in names(gofdt)){
  file <- paste0("2_output/3_gof/gof_",i,".xlsx")
  for (k in 1:length(gofdt[[i]])) {
    xlsx::write.xlsx(dplyr::bind_rows(gofdt[[i]][[k]]), file, 
                     sheetName=names(gofdt[[i]])[k], 
                     row.names = FALSE,
                     append=TRUE)
  }
}

for (i in names(comp1)){
  file <- paste0("2_output/3_gof/gofcomp_",i,".xlsx")
  for (k in 1:length(comp1[[i]])) {
    xlsx::write.xlsx(dplyr::bind_rows(list(comp1[[i]][[k]],comp2[[i]][[k]])), file, 
                     sheetName=names(comp1[[i]])[k], 
                     row.names = FALSE,
                     append=TRUE)
  }
}
save.image(file = "2_output/environ.RData")
