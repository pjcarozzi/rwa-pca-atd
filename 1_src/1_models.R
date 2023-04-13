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
HYPOTH <- list()
MODELS <- list()

## Mediational Hypothesis ####
### PCA Mediation: Between and within part ####
HYPOTH[["bw"]][["med.pca"]] <- '
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
### ATD Mediation: Between and within part ####
HYPOTH[["bw"]][["med.atd"]] <- '
          # Random Intercepts
              RI_x =~ 1*w1pca + 1*w2pca + 1*w3pca + 1*w4pca + 1*w5pca
              RI_y =~ 1*w1rwa + 1*w2rwa + 1*w3rwa + 1*w4rwa + 1*w5rwa
              RI_z =~ 1*w1atd + 1*w2atd + 1*w3atd + 1*w4atd + 1*w5atd
              
          # Within‐person centered variables
              cw1x =~ 1*w1pca 
              cw2x =~ 1*w2pca 
              cw3x =~ 1*w3pca 
              cw4x =~ 1*w4pca 
              cw5x =~ 1*w5pca 
              
              cw1y =~ 1*w1rwa
              cw2y =~ 1*w2rwa
              cw3y =~ 1*w3rwa
              cw4y =~ 1*w4rwa
              cw5y =~ 1*w5rwa

              cw1z =~ 1*w1atd
              cw2z =~ 1*w2atd
              cw3z =~ 1*w3atd
              cw4z =~ 1*w4atd
              cw5z =~ 1*w5atd

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
### RWA Mediation: Between and within part ####
HYPOTH[["bw"]][["med.rwa"]] <- '
          # Random Intercepts
              RI_x =~ 1*w1pca + 1*w2pca + 1*w3pca + 1*w4pca + 1*w5pca
              RI_y =~ 1*w1atd + 1*w2atd + 1*w3atd + 1*w4atd + 1*w5atd
              RI_z =~ 1*w1rwa + 1*w2rwa + 1*w3rwa + 1*w4rwa + 1*w5rwa
              
          # Within‐person centered variables
              cw1x =~ 1*w1pca 
              cw2x =~ 1*w2pca 
              cw3x =~ 1*w3pca 
              cw4x =~ 1*w4pca 
              cw5x =~ 1*w5pca 
              
              cw1y =~ 1*w1atd
              cw2y =~ 1*w2atd
              cw3y =~ 1*w3atd
              cw4y =~ 1*w4atd
              cw5y =~ 1*w5atd
              
              cw1z =~ 1*w1rwa
              cw2z =~ 1*w2rwa
              cw3z =~ 1*w3rwa
              cw4z =~ 1*w4rwa
              cw5z =~ 1*w5rwa
              
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
### Common: Varcov part ####
HYPOTH[["vc"]] <- '
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

## Models ####
### M1 ####
#### Effects: A ####
MODELS[["m1"]][["a1"]] <- '
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
MODELS[["m1"]][["a2"]] <- '
          # Lagged effects 
              cw2x ~ a*cw1x
              cw3x ~ a*cw2x
              cw4x ~ a*cw3x
              cw5x ~ a*cw4x
              cw2z ~ e*cw1z
              cw3z ~ e*cw2z
              cw4z ~ e*cw3z
              cw5z ~ e*cw4z
              cw2y ~ i*cw1y
              cw3y ~ i*cw2y
              cw4y ~ i*cw3y
              cw5y ~ i*cw4y
'

#### Effects: B ####
MODELS[["m1"]][["b1"]] <- '
          # Lagged effects 
              cw2x ~ a*cw1x 
              cw3x ~ a*cw2x 
              cw4x ~ a*cw3x 
              cw5x ~ a*cw4x 
              cw2z ~ cw1x + e*cw1z 
              cw3z ~ cw2x + e*cw2z 
              cw4z ~ cw3x + e*cw3z 
              cw5z ~ cw4x + e*cw4z 
              cw2y ~ cw1x + cw1z + i*cw1y 
              cw3y ~ cw2x + cw2z + i*cw2y 
              cw4y ~ cw3x + cw3z + i*cw3y 
              cw5y ~ cw4x + cw4z + i*cw4y
'
MODELS[["m1"]][["b2"]] <- '
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
              
          # Defined Parameters:
              xzy.ind := d*h
              xzy.dir := g
'

#### Effects: C ####
MODELS[["m1"]][["c1"]] <- '
          # Lagged effects 
              cw2x ~ a*cw1x + cw1z + cw1y
              cw3x ~ a*cw2x + cw2z + cw2y
              cw4x ~ a*cw3x + cw3z + cw3y
              cw5x ~ a*cw4x + cw4z + cw4y
              cw2z ~ e*cw1z + cw1y
              cw3z ~ e*cw2z + cw2y
              cw4z ~ e*cw3z + cw3y
              cw5z ~ e*cw4z + cw4y
              cw2y ~ i*cw1y
              cw3y ~ i*cw2y
              cw4y ~ i*cw3y
              cw5y ~ i*cw4y
'
MODELS[["m1"]][["c2"]] <- '
          # Lagged effects 
              cw2x ~ a*cw1x + b*cw1z + c*cw1y
              cw3x ~ a*cw2x + b*cw2z + c*cw2y
              cw4x ~ a*cw3x + b*cw3z + c*cw3y
              cw5x ~ a*cw4x + b*cw4z + c*cw4y
              cw2z ~ e*cw1z + f*cw1y
              cw3z ~ e*cw2z + f*cw2y
              cw4z ~ e*cw3z + f*cw3y
              cw5z ~ e*cw4z + f*cw4y
              cw2y ~ i*cw1y
              cw3y ~ i*cw2y
              cw4y ~ i*cw3y
              cw5y ~ i*cw4y

          # Defined Parameters:
              yzx.ind := f*b
              yzx.dir := c
'

#### Effects: D ####
MODELS[["m1"]][["d1"]] <- '
          # Lagged effects 
              cw2x ~ a*cw1x + cw1z + cw1y
              cw3x ~ a*cw2x + cw2z + cw2y
              cw4x ~ a*cw3x + cw3z + cw3y
              cw5x ~ a*cw4x + cw4z + cw4y
              cw2z ~ cw1x + e*cw1z + cw1y
              cw3z ~ cw2x + e*cw2z + cw2y
              cw4z ~ cw3x + e*cw3z + cw3y
              cw5z ~ cw4x + e*cw4z + cw4y
              cw2y ~ cw1x + cw1z + i*cw1y
              cw3y ~ cw2x + cw2z + i*cw2y
              cw4y ~ cw3x + cw3z + i*cw3y
              cw5y ~ cw4x + cw4z + i*cw4y
'

MODELS[["m1"]][["d2"]] <- '
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
              xzy.ind := d*h
              xzy.dir := g

              yzx.ind := f*b
              yzx.dir := c

              zxy.ind := b*g
              zxy.dir := h

              yxz.ind := c*d
              yxz.dir := f

              xyz.ind := g*f
              xyz.dir := d

              zyx.ind := h*c
              zyx.dir := b
'

# ### M2 ####
# #### Effects: A ####
# MODELS[["m2"]][["a1"]] <- '
#           # Lagged effects 
#               cw2x ~ cw1x 
#               cw3x ~ cw2x 
#               cw4x ~ cw3x 
#               cw5x ~ cw4x 
#               cw2z ~ cw1z
#               cw3z ~ cw2z
#               cw4z ~ cw3z
#               cw5z ~ cw4z
#               cw2y ~ cw1y
#               cw3y ~ cw2y
#               cw4y ~ cw3y
#               cw5y ~ cw4y
# '
# MODELS[["m2"]][["a2"]] <- '
#           # Lagged effects 
#               cw2x ~ a*cw1x
#               cw3x ~ a*cw2x
#               cw4x ~ a*cw3x
#               cw5x ~ a*cw4x
#               cw2z ~ g*cw1z
#               cw3z ~ g*cw2z
#               cw4z ~ g*cw3z
#               cw5z ~ g*cw4z
#               cw2y ~ m*cw1y
#               cw3y ~ m*cw2y
#               cw4y ~ m*cw3y
#               cw5y ~ m*cw4y
# '
# 
# #### Effects: B ####
# MODELS[["m2"]][["b1"]] <- '
#           # Lagged effects 
#               cw2x ~ cw1x
#               cw3x ~ cw2x
#               cw4x ~ cw3x
#               cw5x ~ cw4x
#               cw2z ~ cw1x + cw1z
#               cw3z ~ cw2x + cw2z + cw1x
#               cw4z ~ cw3x + cw3z + cw2x
#               cw5z ~ cw4x + cw4z + cw3x
#               cw2y ~ cw1x + cw1z + cw1y
#               cw3y ~ cw2x + cw2z + cw2y + cw1x + cw1z
#               cw4y ~ cw3x + cw3z + cw3y + cw2x + cw2z
#               cw5y ~ cw4x + cw4z + cw4y + cw3x + cw3z
# '
# MODELS[["m2"]][["b2"]] <- '
#           # Lagged effects 
#               cw2x ~ a*cw1x
#               cw3x ~ a*cw2x
#               cw4x ~ a*cw3x
#               cw5x ~ a*cw4x
#               cw2z ~ f*cw1x + g*cw1z
#               cw3z ~ f*cw2x + g*cw2z + i*cw1x
#               cw4z ~ f*cw3x + g*cw3z + i*cw2x
#               cw5z ~ f*cw4x + g*cw4z + i*cw3x
#               cw2y ~ k*cw1x + l*cw1z + m*cw1y
#               cw3y ~ k*cw2x + l*cw2z + m*cw2y + n*cw1x + o*cw1z
#               cw4y ~ k*cw3x + l*cw3z + m*cw3y + n*cw2x + o*cw2z
#               cw5y ~ k*cw4x + l*cw4z + m*cw4y + n*cw3x + o*cw3z
#               
#           # Defined Parameters:
#               xzy.ind := f*l
#               xzy.dir := n
#               xzy.tot := xzy.ind + n
# '
# 
# #### Effects: C ####
# MODELS[["m2"]][["c1"]] <- '
#           # Lagged effects 
#               cw2x ~ cw1x + cw1z + cw1y
#               cw3x ~ cw2x + cw2z + cw2y + cw1y + cw1z
#               cw4x ~ cw3x + cw3z + cw3y + cw2y + cw2z
#               cw5x ~ cw4x + cw4z + cw4y + cw3y + cw3z
#               cw2z ~ cw1z + cw1y
#               cw3z ~ cw2z + cw2y + cw1y
#               cw4z ~ cw3z + cw3y + cw2y
#               cw5z ~ cw4z + cw4y + cw3y
#               cw2y ~ cw1y
#               cw3y ~ cw2y
#               cw4y ~ cw3y
#               cw5y ~ cw4y
# '
# MODELS[["m2"]][["c2"]] <- '
#           # Lagged effects 
#               cw2x ~ a*cw1x + b*cw1z + c*cw1y
#               cw3x ~ a*cw2x + b*cw2z + c*cw2y + d*cw1y + e*cw1z
#               cw4x ~ a*cw3x + b*cw3z + c*cw3y + d*cw2y + e*cw2z
#               cw5x ~ a*cw4x + b*cw4z + c*cw4y + d*cw3y + e*cw3z
#               cw2z ~ g*cw1z + h*cw1y
#               cw3z ~ g*cw2z + h*cw2y + j*cw1y
#               cw4z ~ g*cw3z + h*cw3y + j*cw2y
#               cw5z ~ g*cw4z + h*cw4y + j*cw3y
#               cw2y ~ m*cw1y
#               cw3y ~ m*cw2y
#               cw4y ~ m*cw3y
#               cw5y ~ m*cw4y
#               
#           # Defined Parameters:
#               yzx.ind := h*b
#               yzx.dir := d
#               yzx.tot := yzx.ind + d
# '
# 
# 
# #### Effects: D ####
# MODELS[["m2"]][["d1"]] <- '
#           # Lagged effects 
#               cw2x ~ cw1x + cw1z + cw1y
#               cw3x ~ cw2x + cw2z + cw2y + cw1y + cw1z
#               cw4x ~ cw3x + cw3z + cw3y + cw2y + cw2z
#               cw5x ~ cw4x + cw4z + cw4y + cw3y + cw3z
#               cw2z ~ cw1x + cw1z + cw1y
#               cw3z ~ cw2x + cw2z + cw2y + cw1x + cw1y
#               cw4z ~ cw3x + cw3z + cw3y + cw2x + cw2y
#               cw5z ~ cw4x + cw4z + cw4y + cw3x + cw3y
#               cw2y ~ cw1x + cw1z + cw1y
#               cw3y ~ cw2x + cw2z + cw2y + cw1x + cw1z
#               cw4y ~ cw3x + cw3z + cw3y + cw2x + cw2z
#               cw5y ~ cw4x + cw4z + cw4y + cw3x + cw3z
# '
# 
# MODELS[["m2"]][["d2"]] <- '
#           # Lagged effects 
#               cw2x ~ a*cw1x + b*cw1z + c*cw1y
#               cw3x ~ a*cw2x + b*cw2z + c*cw2y + d*cw1y + e*cw1z
#               cw4x ~ a*cw3x + b*cw3z + c*cw3y + d*cw2y + e*cw2z
#               cw5x ~ a*cw4x + b*cw4z + c*cw4y + d*cw3y + e*cw3z
#               cw2z ~ f*cw1x + g*cw1z + h*cw1y
#               cw3z ~ f*cw2x + g*cw2z + h*cw2y + i*cw1x + j*cw1y
#               cw4z ~ f*cw3x + g*cw3z + h*cw3y + i*cw2x + j*cw2y
#               cw5z ~ f*cw4x + g*cw4z + h*cw4y + i*cw3x + j*cw3y
#               cw2y ~ k*cw1x + l*cw1z + m*cw1y
#               cw3y ~ k*cw2x + l*cw2z + m*cw2y + n*cw1x + o*cw1z
#               cw4y ~ k*cw3x + l*cw3z + m*cw3y + n*cw2x + o*cw2z
#               cw5y ~ k*cw4x + l*cw4z + m*cw4y + n*cw3x + o*cw3z
#               
#           # Defined Parameters:
#               xzy.ind := f*l
#               xzy.dir := n
#               xzy.tot := xzy.ind + n
#               
#               yzx.ind := h*b
#               yzx.dir := d
#               yzx.tot := yzx.ind + d
#               
#               zxy.ind := b*k
#               zxy.dir := o
#               zxy.tot := zxy.ind + o
#               
#               yxz.ind := c*f
#               yxz.dir := j
#               yxz.tot := yxz.ind + j
#               
#               xyz.ind := k*h
#               xyz.dir := i
#               xyz.tot := xyz.ind + i
#               
#               zyx.ind := l*c
#               zyx.dir := e
#               zyx.tot := zyx.ind + e              
# '
# 
# ### M3 ####
# #### Effects: A ####
# MODELS[["m3"]][["a1"]] <- '
#           # Lagged effects 
#               cw2x ~ cw1x 
#               cw3x ~ cw2x + cw1x
#               cw4x ~ cw3x + cw2x
#               cw5x ~ cw4x + cw3x
#               cw2z ~ cw1z
#               cw3z ~ cw2z + cw1z
#               cw4z ~ cw3z + cw2z
#               cw5z ~ cw4z + cw3z
#               cw2y ~ cw1y
#               cw3y ~ cw2y + cw1y
#               cw4y ~ cw3y + cw2y
#               cw5y ~ cw4y + cw3y
# '
# MODELS[["m3"]][["a2"]] <- '
#           # Lagged effects 
#               cw2x ~ a*cw1x
#               cw3x ~ a*cw2x + p*cw1x
#               cw4x ~ a*cw3x + p*cw2x
#               cw5x ~ a*cw4x + p*cw3x
#               cw2z ~ g*cw1z
#               cw3z ~ g*cw2z + q*cw1z
#               cw4z ~ g*cw3z + q*cw2z
#               cw5z ~ g*cw4z + q*cw3z
#               cw2y ~ m*cw1y
#               cw3y ~ m*cw2y + r*cw1x
#               cw4y ~ m*cw3y + r*cw2x
#               cw5y ~ m*cw4y + r*cw3x
# '
# 
# #### Effects: B ####
# MODELS[["m3"]][["b1"]] <- '
#           # Lagged effects 
#               cw2x ~ cw1x
#               cw3x ~ cw2x + cw1x
#               cw4x ~ cw3x + cw2x
#               cw5x ~ cw4x + cw3x
#               cw2z ~ cw1x + cw1z
#               cw3z ~ cw2x + cw2z + cw1x + cw1z
#               cw4z ~ cw3x + cw3z + cw2x + cw2z
#               cw5z ~ cw4x + cw4z + cw3x + cw3z
#               cw2y ~ cw1x + cw1z + cw1y
#               cw3y ~ cw2x + cw2z + cw2y + cw1x + cw1z + cw1y
#               cw4y ~ cw3x + cw3z + cw3y + cw2x + cw2z + cw2y
#               cw5y ~ cw4x + cw4z + cw4y + cw3x + cw3z + cw3y
# '
# MODELS[["m3"]][["b2"]] <- '
#           # Lagged effects 
#               cw2x ~ a*cw1x
#               cw3x ~ a*cw2x + p*cw1x
#               cw4x ~ a*cw3x + p*cw2x
#               cw5x ~ a*cw4x + p*cw3x
#               cw2z ~ f*cw1x + g*cw1z
#               cw3z ~ f*cw2x + g*cw2z + i*cw1x + q*cw1z
#               cw4z ~ f*cw3x + g*cw3z + i*cw2x + q*cw2z
#               cw5z ~ f*cw4x + g*cw4z + i*cw3x + q*cw3z
#               cw2y ~ k*cw1x + l*cw1z + m*cw1y
#               cw3y ~ k*cw2x + l*cw2z + m*cw2y + n*cw1x + o*cw1z + r*cw1y
#               cw4y ~ k*cw3x + l*cw3z + m*cw3y + n*cw2x + o*cw2z + r*cw2y
#               cw5y ~ k*cw4x + l*cw4z + m*cw4y + n*cw3x + o*cw3z + r*cw3y
#               
#           # Defined Parameters:
#               xzy.ind := f*l
#               xzy.dir := n
#               xzy.tot := xzy.ind + n
# '
# 
# #### Effects: C ####
# MODELS[["m3"]][["c1"]] <- '
#           # Lagged effects 
#               cw2x ~ cw1x + cw1z + cw1y
#               cw3x ~ cw2x + cw2z + cw2y + cw1y + cw1z + cw1x
#               cw4x ~ cw3x + cw3z + cw3y + cw2y + cw2z + cw2x
#               cw5x ~ cw4x + cw4z + cw4y + cw3y + cw3z + cw3x
#               cw2z ~ cw1z + cw1y
#               cw3z ~ cw2z + cw2y + cw1y + cw1z
#               cw4z ~ cw3z + cw3y + cw2y + cw2z
#               cw5z ~ cw4z + cw4y + cw3y + cw3z
#               cw2y ~ cw1y
#               cw3y ~ cw2y + cw1y
#               cw4y ~ cw3y + cw2y
#               cw5y ~ cw4y + cw3y
# '
# MODELS[["m3"]][["c2"]] <- '
#           # Lagged effects 
#               cw2x ~ a*cw1x + b*cw1z + c*cw1y
#               cw3x ~ a*cw2x + b*cw2z + c*cw2y + d*cw1y + e*cw1z + p*cw1x
#               cw4x ~ a*cw3x + b*cw3z + c*cw3y + d*cw2y + e*cw2z + p*cw2x
#               cw5x ~ a*cw4x + b*cw4z + c*cw4y + d*cw3y + e*cw3z + p*cw3x
#               cw2z ~ g*cw1z + h*cw1y
#               cw3z ~ g*cw2z + h*cw2y + j*cw1y + q*cw1z
#               cw4z ~ g*cw3z + h*cw3y + j*cw2y + q*cw2z
#               cw5z ~ g*cw4z + h*cw4y + j*cw3y + q*cw3z
#               cw2y ~ m*cw1y
#               cw3y ~ m*cw2y + r*cw1x
#               cw4y ~ m*cw3y + r*cw2x
#               cw5y ~ m*cw4y + r*cw3x
#               
#           # Defined Parameters:
#               yzx.ind := h*b
#               yzx.dir := d
#               yzx.tot := yzx.ind + d
# '
# 
# #### Effects: D ####
# MODELS[["m3"]][["d1"]] <- '
#           # Lagged effects 
#               cw2x ~ cw1x + cw1z + cw1y
#               cw3x ~ cw2x + cw2z + cw2y + cw1y + cw1z + cw1x
#               cw4x ~ cw3x + cw3z + cw3y + cw2y + cw2z + cw2x
#               cw5x ~ cw4x + cw4z + cw4y + cw3y + cw3z + cw3x
#               cw2z ~ cw1x + cw1z + cw1y 
#               cw3z ~ cw2x + cw2z + cw2y + cw1x + cw1y + cw1z
#               cw4z ~ cw3x + cw3z + cw3y + cw2x + cw2y + cw2z
#               cw5z ~ cw4x + cw4z + cw4y + cw3x + cw3y + cw3z
#               cw2y ~ cw1x + cw1z + cw1y 
#               cw3y ~ cw2x + cw2z + cw2y + cw1x + cw1z + cw1y
#               cw4y ~ cw3x + cw3z + cw3y + cw2x + cw2z + cw2y
#               cw5y ~ cw4x + cw4z + cw4y + cw3x + cw3z + cw3y
# '
# 
# MODELS[["m3"]][["d2"]] <- '
#           # Lagged effects 
#               cw2x ~ a*cw1x + b*cw1z + c*cw1y
#               cw3x ~ a*cw2x + b*cw2z + c*cw2y + d*cw1y + e*cw1z + p*cw1x
#               cw4x ~ a*cw3x + b*cw3z + c*cw3y + d*cw2y + e*cw2z + p*cw2x
#               cw5x ~ a*cw4x + b*cw4z + c*cw4y + d*cw3y + e*cw3z + p*cw3x
#               cw2z ~ f*cw1x + g*cw1z + h*cw1y                   
#               cw3z ~ f*cw2x + g*cw2z + h*cw2y + i*cw1x + j*cw1y + q*cw1z
#               cw4z ~ f*cw3x + g*cw3z + h*cw3y + i*cw2x + j*cw2y + q*cw2z
#               cw5z ~ f*cw4x + g*cw4z + h*cw4y + i*cw3x + j*cw3y + q*cw3z
#               cw2y ~ k*cw1x + l*cw1z + m*cw1y                   
#               cw3y ~ k*cw2x + l*cw2z + m*cw2y + n*cw1x + o*cw1z + r*cw1y
#               cw4y ~ k*cw3x + l*cw3z + m*cw3y + n*cw2x + o*cw2z + r*cw2y
#               cw5y ~ k*cw4x + l*cw4z + m*cw4y + n*cw3x + o*cw3z + r*cw3y
#               
#           # Defined Parameters:
#               xzy.ind := f*l
#               xzy.dir := n
#               xzy.tot := xzy.ind + n
#               
#               yzx.ind := h*b
#               yzx.dir := d
#               yzx.tot := yzx.ind + d
#               
#               zxy.ind := b*k
#               zxy.dir := o
#               zxy.tot := zxy.ind + o
#               
#               yxz.ind := c*f
#               yxz.dir := j
#               yxz.tot := yxz.ind + j
#               
#               xyz.ind := k*h
#               xyz.dir := i
#               xyz.tot := xyz.ind + i
#               
#               zyx.ind := l*c
#               zyx.dir := e
#               zyx.tot := zyx.ind + e              
# '

save.image(file = "2_output/environ.RData")