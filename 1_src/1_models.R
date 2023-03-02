# ### ### ### ### ### ### ### ### ###
# 1.- Declare models
# pjcarozzi@uc.cl
# Rev. March 2023
# ### ### ### ### ### ### ### ### ###

# PREV ####
## Clean workspace ####
rm(list = ls(all.names = TRUE))

## Where am I? ####
here::i_am("1_src/1_models.R")

# MODELS ####
M <- list()

## Without mediation ####
### Common part ####
#### Between and within
M[["wo"]][["bwcomp"]] <- '
          # Random Intercepts
              RI_x =~ 1*w1atd + 1*w2atd + 1*w3atd + 1*w4atd + 1*w5atd
              RI_y =~ 1*w1rwa + 1*w2rwa + 1*w3rwa + 1*w4rwa + 1*w5rwa
              
          # Within‐person centered variables
              cw1x =~ 1*w1atd
              cw2x =~ 1*w2atd
              cw3x =~ 1*w3atd
              cw4x =~ 1*w4atd
              cw5x =~ 1*w5atd
              
              cw1y =~ 1*w1rwa
              cw2y =~ 1*w2rwa
              cw3y =~ 1*w3rwa
              cw4y =~ 1*w4rwa
              cw5y =~ 1*w5rwa
          
          # Constrain the measurement error variances to zero
              w1atd ~~ 0*w1atd
              w2atd ~~ 0*w2atd
              w3atd ~~ 0*w3atd
              w4atd ~~ 0*w4atd
              w5atd ~~ 0*w5atd
              
              w1rwa ~~ 0*w1rwa
              w2rwa ~~ 0*w2rwa
              w3rwa ~~ 0*w3rwa
              w4rwa ~~ 0*w4rwa
              w5rwa ~~ 0*w5rwa
'
#### Varcov
M[["wo"]][["varcov"]] <- '
          # Covariance at the first wave
              cw1x ~~ cw1y
              
          # Covariances between the residuals
              cw2x ~~ cw2y
              cw3x ~~ cw3y
              cw4x ~~ cw4y
              cw5x ~~ cw5y
          
          # (residual) variance of the within-person centered variables.
              cw1x ~~ cw1x # Variances
              cw1y ~~ cw1y 
              cw2x ~~ cw2x # Residual variances
              cw3x ~~ cw3x
              cw4x ~~ cw4x
              cw5x ~~ cw5x
              cw2y ~~ cw2y 
              cw3y ~~ cw3y 
              cw4y ~~ cw4y 
              cw5y ~~ cw5y   
              
          # Variance and covariances between the RI
              RI_x ~~ RI_x 
              RI_y ~~ RI_y
              RI_x ~~ RI_y
              
          # Correlation between the individual factors and RIs
              RI_x ~~ 0*cw1x
              RI_x ~~ 0*cw1y
              RI_y ~~ 0*cw1x
              RI_y ~~ 0*cw1y
'
### Effects: A ####
M[["wo"]][["a1"]] <- '
          # Lagged effects 
              cw2x ~ cw1x
              cw3x ~ cw2x
              cw4x ~ cw3x
              cw5x ~ cw4x
              cw2y ~ cw1y
              cw3y ~ cw2y
              cw4y ~ cw3y
              cw5y ~ cw4y
'
M[["wo"]][["a2"]] <- '
          # Lagged effects 
              cw2x ~ a*cw1x
              cw3x ~ a*cw2x
              cw4x ~ a*cw3x
              cw5x ~ a*cw4x
              cw2y ~ d*cw1y
              cw3y ~ d*cw2y
              cw4y ~ d*cw3y
              cw5y ~ d*cw4y
'

### Effects: B ####
M[["wo"]][["b1"]] <- '
          # Lagged effects 
              cw2x ~ cw1x
              cw3x ~ cw2x 
              cw4x ~ cw3x
              cw5x ~ cw4x
              cw2y ~ cw1x + cw1y
              cw3y ~ cw2x + cw2y
              cw4y ~ cw3x + cw3y
              cw5y ~ cw4x + cw4y
'
M[["wo"]][["b2"]] <- '
          # Lagged effects 
              cw2x ~ a*cw1x
              cw3x ~ a*cw2x
              cw4x ~ a*cw3x
              cw5x ~ a*cw4x
              cw2y ~ c*cw1x + d*cw1y
              cw3y ~ c*cw2x + d*cw2y
              cw4y ~ c*cw3x + d*cw3y
              cw5y ~ c*cw4x + d*cw4y
'

### Effects: C ####
M[["wo"]][["c1"]] <- '
          # Lagged effects 
              cw2x ~ cw1x + cw1y
              cw3x ~ cw2x + cw2y 
              cw4x ~ cw3x + cw3y
              cw5x ~ cw4x + cw4y
              cw2y ~ cw1y
              cw3y ~ cw2y
              cw4y ~ cw3y
              cw5y ~ cw4y
'
M[["wo"]][["c2"]] <- '
          # Lagged effects 
              cw2x ~ a*cw1x + b*cw1y
              cw3x ~ a*cw2x + b*cw2y 
              cw4x ~ a*cw3x + b*cw3y
              cw5x ~ a*cw4x + b*cw4y
              cw2y ~ d*cw1y
              cw3y ~ d*cw2y
              cw4y ~ d*cw3y
              cw5y ~ d*cw4y
'
### Effects: D ####
M[["wo"]][["d1"]] <- '
          # Lagged effects 
              cw2x ~ cw1x + cw1y
              cw3x ~ cw2x + cw2y 
              cw4x ~ cw3x + cw3y
              cw5x ~ cw4x + cw4y
              cw2y ~ cw1x + cw1y
              cw3y ~ cw2x + cw2y
              cw4y ~ cw3x + cw3y
              cw5y ~ cw4x + cw4y
'
M[["wo"]][["d2"]] <- '
          # Lagged effects 
              cw2x ~ a*cw1x + b*cw1y
              cw3x ~ a*cw2x + b*cw2y 
              cw4x ~ a*cw3x + b*cw3y
              cw5x ~ a*cw4x + b*cw4y
              cw2y ~ c*cw1x + d*cw1y
              cw3y ~ c*cw2x + d*cw2y
              cw4y ~ c*cw3x + d*cw3y
              cw5y ~ c*cw4x + d*cw4y
'

## PCA Mediation ####
### Common part ####
#### Between and within
M[["m1"]][["bwcomp"]] <- '
          # Random Intercepts
              RI_x =~ 1*w1atd + 1*w2atd + 1*w3atd + 1*w4atd + 1*w5atd
              RI_y =~ 1*w1rwa + 1*w2rwa + 1*w3rwa + 1*w4rwa + 1*w5rwa
              RI_z =~ 1*w1pca + 1*w2pca + 1*w3pca + 1*w4pca + 1*w5pca
              
          # Within‐person centered variables
              cw1x =~ 1*w1atd
              cw2x =~ 1*w2atd
              cw3x =~ 1*w3atd
              cw4x =~ 1*w4atd
              cw5x =~ 1*w5atd
              
              cw1y =~ 1*w1rwa
              cw2y =~ 1*w2rwa
              cw3y =~ 1*w3rwa
              cw4y =~ 1*w4rwa
              cw5y =~ 1*w5rwa

              cw1z =~ 1*w1pca
              cw2z =~ 1*w2pca
              cw3z =~ 1*w3pca
              cw4z =~ 1*w4pca
              cw5z =~ 1*w5pca
          
          # Constrain the measurement error variances to zero
              w1atd ~~ 0*w1atd
              w2atd ~~ 0*w2atd
              w3atd ~~ 0*w3atd
              w4atd ~~ 0*w4atd
              w5atd ~~ 0*w5atd
              
              w1rwa ~~ 0*w1rwa
              w2rwa ~~ 0*w2rwa
              w3rwa ~~ 0*w3rwa
              w4rwa ~~ 0*w4rwa
              w5rwa ~~ 0*w5rwa

              w1pca ~~ 0*w1pca
              w2pca ~~ 0*w2pca
              w3pca ~~ 0*w3pca
              w4pca ~~ 0*w4pca
              w5pca ~~ 0*w5pca
'

#### Varcov
M[["m1"]][["varcov"]] <- '
          # Covariance at the first wave
              cw1x ~~ cw1z
              cw1x ~~ cw1y
              cw1z ~~ cw1y
              
          # Covariances between the residuals
              cw2x ~~ cw2z
              cw2x ~~ cw2y
              cw2z ~~ cw2y

              cw3x ~~ cw3z
              cw3x ~~ cw3y
              cw3z ~~ cw3y
              
              cw4x ~~ cw4z
              cw4x ~~ cw4y
              cw4z ~~ cw4y      
              
              cw5x ~~ cw5z
              cw5x ~~ cw5y
              cw5z ~~ cw5y
              
          # (residual) variance of the within-person centered variables.
              cw1x ~~ cw1x # Variances
              cw1z ~~ cw1z
              cw1y ~~ cw1y 
              cw2x ~~ cw2x # Residual variances
              cw3x ~~ cw3x
              cw4x ~~ cw4x
              cw5x ~~ cw5x
              cw2z ~~ cw2z
              cw3z ~~ cw3z
              cw4z ~~ cw4z
              cw5z ~~ cw5z
              cw2y ~~ cw2y 
              cw3y ~~ cw3y 
              cw4y ~~ cw4y 
              cw5y ~~ cw5y   
              
          # Variance and covariances between the RI
              RI_x ~~ RI_x
              RI_x ~~ RI_z
              RI_x ~~ RI_y
              RI_z ~~ RI_z
              RI_z ~~ RI_y
              RI_y ~~ RI_y
              
          # Correlation between the individual factors and RIs
              RI_x ~~ 0*cw1x
              RI_x ~~ 0*cw1z
              RI_x ~~ 0*cw1y
              
              RI_z ~~ 0*cw1x
              RI_z ~~ 0*cw1z
              RI_z ~~ 0*cw1y
              
              RI_y ~~ 0*cw1x
              RI_y ~~ 0*cw1z
              RI_y ~~ 0*cw1y
'

### Effects: A ####
M[["m1"]][["a1"]] <- '
          # Lagged effects 
              cw2x ~ cw1x 
              cw3x ~ cw2x 
              cw4x ~ cw3x 
              cw5x ~ cw4x 
              cw2z ~ cw1z
              cw3z ~ cw2z
              cw4z ~ cw3z
              cw5z ~ cw4z
              cw2y ~ cw1y
              cw3y ~ cw2y
              cw4y ~ cw3y
              cw5y ~ cw4y
'
M[["m1"]][["a2"]] <- '
          # Lagged effects 
              cw2x ~ a*cw1x 
              cw3x ~ a*cw2x 
              cw4x ~ a*cw3x 
              cw5x ~ a*cw4x 
              cw2z ~ d*cw1x
              cw3z ~ d*cw2x
              cw4z ~ d*cw3x
              cw5z ~ d*cw4x
              cw2y ~ g*cw1x
              cw3y ~ g*cw2x
              cw4y ~ g*cw3x
              cw5y ~ g*cw4x
'

### Effects: B ####
M[["m1"]][["b1"]] <- '
          # Lagged effects 
              cw2x ~ cw1x 
              cw3x ~ cw2x 
              cw4x ~ cw3x 
              cw5x ~ cw4x 
              cw2z ~ cw1x + cw1z 
              cw3z ~ cw2x + cw2z 
              cw4z ~ cw3x + cw3z 
              cw5z ~ cw4x + cw4z 
              cw2y ~ cw1x + cw1z + cw1y 
              cw3y ~ cw2x + cw2z + cw2y 
              cw4y ~ cw3x + cw3z + cw3y 
              cw5y ~ cw4x + cw4z + cw4y
'
M[["m1"]][["b2"]] <- '
          # Lagged effects 
              cw2x ~ a*cw1x 
              cw3x ~ a*cw2x 
              cw4x ~ a*cw3x 
              cw5x ~ a*cw4x 
              cw2z ~ d*cw1x + e*cw1z 
              cw3z ~ d*cw2x + e*cw2z 
              cw4z ~ d*cw3x + e*cw3z 
              cw5z ~ d*cw4x + e*cw4z 
              cw2y ~ g*cw1x + h*cw1z + i*cw1y 
              cw3y ~ g*cw2x + h*cw2z + i*cw2y 
              cw4y ~ g*cw3x + h*cw3z + i*cw3y 
              cw5y ~ g*cw4x + h*cw4z + i*cw4y
'

### Effects: C ####
M[["m1"]][["c1"]] <- '
          # Lagged effects 
              cw2x ~ cw1x + cw1z + cw1y
              cw3x ~ cw2x + cw2z + cw2y
              cw4x ~ cw3x + cw3z + cw3y
              cw5x ~ cw4x + cw4z + cw4y
              cw2z ~ cw1x + cw1z
              cw3z ~ cw2x + cw2z
              cw4z ~ cw3x + cw3z
              cw5z ~ cw4x + cw4z
              cw2y ~ cw1y
              cw3y ~ cw2y
              cw4y ~ cw3y
              cw5y ~ cw4y
'
M[["m1"]][["c2"]] <- '
          # Lagged effects 
              cw2x ~ a*cw1x + b*cw1z + c*cw1y
              cw3x ~ a*cw2x + b*cw2z + c*cw2y
              cw4x ~ a*cw3x + b*cw3z + c*cw3y
              cw5x ~ a*cw4x + b*cw4z + c*cw4y
              cw2z ~ d*cw1x + e*cw1z
              cw3z ~ d*cw2x + e*cw2z
              cw4z ~ d*cw3x + e*cw3z
              cw5z ~ d*cw4x + e*cw4z
              cw2y ~ g*cw1x
              cw3y ~ g*cw2x
              cw4y ~ g*cw3x
              cw5y ~ g*cw4x
'
### Effects: D ####
M[["m1"]][["d1"]] <- '
          # Lagged effects 
              cw2x ~ cw1x + cw1z + cw1y
              cw3x ~ cw2x + cw2z + cw2y
              cw4x ~ cw3x + cw3z + cw3y
              cw5x ~ cw4x + cw4z + cw4y
              cw2z ~ cw1x + cw1z + cw1y
              cw3z ~ cw2x + cw2z + cw2y
              cw4z ~ cw3x + cw3z + cw3y
              cw5z ~ cw4x + cw4z + cw4y
              cw2y ~ cw1x + cw1z + cw1y
              cw3y ~ cw2x + cw2z + cw2y
              cw4y ~ cw3x + cw3z + cw3y
              cw5y ~ cw4x + cw4z + cw4y
'

M[["m1"]][["d2"]] <- '
          # Lagged effects 
              cw2x ~ a*cw1x + b*cw1z + c*cw1y
              cw3x ~ a*cw2x + b*cw2z + c*cw2y
              cw4x ~ a*cw3x + b*cw3z + c*cw3y
              cw5x ~ a*cw4x + b*cw4z + c*cw4y
              cw2z ~ d*cw1x + e*cw1z + f*cw1y
              cw3z ~ d*cw2x + e*cw2z + f*cw2y
              cw4z ~ d*cw3x + e*cw3z + f*cw3y
              cw5z ~ d*cw4x + e*cw4z + f*cw4y
              cw2y ~ g*cw1x + h*cw1z + i*cw1y
              cw3y ~ g*cw2x + h*cw2z + i*cw2y
              cw4y ~ g*cw3x + h*cw3z + i*cw3y
              cw5y ~ g*cw4x + h*cw4z + i*cw4y
              
          # Defined Parameters:
              xzy.i := d*h
              xzy.d := g
'

save.image(file = "2_output/environ.RData")