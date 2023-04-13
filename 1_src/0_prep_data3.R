# ### ### ### ### ### ### ### ### ###
# 0.- Prepare Data
# pjcarozzi@uc.cl
# Rev. March 2023
# ### ### ### ### ### ### ### ### ###

# PREV ####
## Clean workspace ####
rm(list = ls(all.names = TRUE))

## Where am I? ####
here::i_am("1_src/0_prep_data3.R")

## Libraries ####
library(data.table) 
library(rio)

## Data: Fondecyt Regular Nº1161371 ####
raw <- fread("0_data/e5_long_sin_escalas.csv")

# PREPARING DATA ####
## Variable and wave selection ####
vars <- grep("^pca[1-8]$|^sdo[1-8]$|^aut[1-4]$|^adem1$|^wave$|^ID$", names(raw), value = T)
raw[,wave:=gsub("e5","",wave)]
raw <- raw[wave %in% paste0("w",1:5),..vars]

## Reverse code ####
vars <- c("sdo3","sdo4","sdo7","sdo8")
raw[ , (vars) := lapply(.SD, function(x) 6-x), .SDcols = vars]

## Variable names ####
### PCA PCA[1-4] are conventional and PCA[5-8] are radical
setnames(raw,grep("^pca[5-8]$",names(raw), value=T),gsub("pca","pra",grep("^pca[5-8]$",names(raw), value=T)))
### ATD
setnames(raw,grep("^adem\\d{1}$",names(raw), value=T),gsub("adem","atd",grep("^adem\\d{1}$",names(raw), value=T)))
### RWA
setnames(raw,grep("^aut\\d{1}$",names(raw), value=T),gsub("aut","rwa",grep("^aut\\d{1}$",names(raw), value=T)))
names(raw)

## Long to wide ####
dt <- data.table::dcast(raw, ID ~ wave, value.var = grep("^pca|^pra|^rwa|^sdo|^atd",names(raw),value = T))

## Scales and Reliability ####
alpha_total <- data.frame()
alpha_drop <- list()

scales <- c("pca","pra","rwa","sdo","atd")
for (i in 1:5){
  for (k in 1:length(scales)){
    # Scale by wave
    pattern <- paste0(scales[k],"\\d{1}_w",i)
    items <- grep(pattern,names(dt),value = T)
    if (length(items)>1){
    x <- na.omit(dt[, ..items])
    # Cronbach's alpha
    if (nrow(x)!=0){
      a <- psych::alpha(x)
      alpha_total <- rbind(alpha_total,cbind(Scale=scales[k],Wave=i,a[["total"]]))
      alpha_drop[[paste0(scales[k],"_w",i, sep="")]] <- a[["alpha.drop"]]
    }
    # Compute average score
    dt[, paste0("w",i,scales[k]) := rowMeans(.SD, na.rm = F), by = ID, .SDcols = items]
    } else {
    dt[, paste0("w",i,scales[k]) := get(items)]
    }
  }
  }


## Select scales ####
vars <- grep("ID|^w\\d{1}atd$|^w\\d{1}pca$|^w\\d{1}rwa$",names(dt), value = T)
dt <- dt[,..vars]
# summarytools::descr(dt[,-1])

## Filter all na cases ####
dt <- dt[rowSums(is.na(dt[,-1])) != ncol(dt[,-1]), ]

# SAVE ####
## Alpha ####
export(alpha_total, "2_output/1_alpha/alpha_v3.xlsx", 
                 sheetName="total", 
                 rowNames = FALSE,
                 append=F)

for (i in names(alpha_drop)) {
  export(alpha_drop[[i]], "2_output/1_alpha/alpha_v3.xlsx", 
                   sheetName=paste0(i,"_drop",sep=""), 
                   rowNames = FALSE,
                   append=TRUE)
}

## Data ####
write.csv(dt,file=here::here("0_data","data_sel_v3.csv"),row.names = F)
