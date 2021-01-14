## Simulate Data

library(lavaan)

loadvec1 <- rep(.400,6)
loadvec2 <- rep(.400,6)

modVec <- glue::glue("LV1 =~ {loadvec1[1]}*LV11 + 
                             {loadvec1[2]}*LV12 +
                             {loadvec1[3]}*LV13 +
                             {loadvec1[4]}*LV14 +
                             {loadvec1[5]}*LV15 +
                             {loadvec1[6]}*LV16 
                      LV2 =~ {loadvec2[1]}*LV21 + 
                             {loadvec2[2]}*LV22 +
                             {loadvec2[3]}*LV23 +
                             {loadvec2[4]}*LV24 +
                             {loadvec2[5]}*LV25 +
                             {loadvec2[6]}*LV26 
                             
                             
                     ## factor correlation

                     LV1 ~~ 0.6*LV2
                     
                     ### Residual correlations
                     
                     LV13 ~~ -.2*LV14
                     LV11 ~~ .2*LV12
                     LV15 ~~ -.2*LV16
                     LV23 ~~ .2*LV24
                     LV21 ~~ -.2*LV22
                     LV25 ~~ .2*LV26
                     ")


dat  <- simulateData(modVec,
                     std.lv=T,
                     meanstructure=T, 
                     model.type="cfa",
                     sample.nobs=500,
                     seed=1234567)

for (i in 1:length(names(dat))) {
  dat[,i] <- dat[,i]+abs(min(dat[,i]))
  dat[,i] <- dat[,i]/max(dat[,i])
  dat[,i] <- (dat[,i]*4)+1
  dat[,i] <- round(dat[,i])
}

sapply(dat,table)

dat$ID <- sprintf("%04.0f", 0001:nrow(dat))

### Fit population Model

modPop <- '

## LV1
LV1 =~ LV11 +
       LV12 +
       LV13 +
       LV14 +
       LV15 +
       LV16 


## LV2
LV2 =~ LV21 +
       LV22 +
       LV23 +
       LV24 +
       LV25 +
       LV26 +
       LV11 + 
       LV12
       
       LV13 ~~ LV14
       LV11 ~~ LV12
       LV15 ~~ LV16
       LV23 ~~ LV24
       LV21 ~~ LV22
       LV25 ~~ LV26
       
'
fitP <- cfa(model = modPop,
            data = dat,
            std.lv=T,
            meanstructure=T)
summary(fitP,
        fit.measures=TRUE,
        standardized=TRUE,
        #modindices=TRUE,
        rsquare=T)

### Fit CFA With observed data
mod1 <- '

## LV1
LV1 =~ LV11 +
       LV12 +
       LV13 +
       LV14 +
       LV15 +
       LV16 


## LV2
LV2 =~ LV21 +
       LV22 +
       LV23 +
       LV24 +
       LV25 +
       LV26
'
fit1 <- cfa(model = mod1,
            data = dat,
            std.lv=T,
            meanstructure=T)
summary(fit1,
        fit.measures=TRUE,
        standardized=TRUE,
        modindices=TRUE,
        rsquare=T)

### Parceling
dat$P11 <- rowMeans(dat[,c("LV11","LV12")])
dat$P12 <- rowMeans(dat[,c("LV13","LV14")])
dat$P13 <- rowMeans(dat[,c("LV15","LV16")])

dat$P21 <- rowMeans(dat[,c("LV21","LV22")])
dat$P22 <- rowMeans(dat[,c("LV23","LV24")])
dat$P23 <- rowMeans(dat[,c("LV25","LV26")])

### Fit parceled model

mod2 <- '

## LV1
LV1 =~ P11 +
       P12 +
       P13 

## LV2
LV2 =~ P21 +
       P22 +
       P23 
'
fit2 <- cfa(model = mod2,
            data = dat,
            std.lv=T,
            meanstructure=T)
summary(fit2,
        fit.measures=TRUE,
        standardized=TRUE,
        modindices=TRUE,
        rsquare=T)

fitTable <- rbind(fitMeasures(fit2,  c("chisq",
                                  "df",
                                  "pvalue",
                                  "tli",
                                  "cfi",
                                  "rmsea",
                                  "srmr")))


## Install Packages
install.packages("lavaan")
install.packages("glue")
install.packages("tidyverse")

## Library Packages
library(lavaan)
library(glue)
library(tidyverse)

## Generate Vectors of factor loadings
loadvec1 <- c(.458,.542,.601,.399,.589,.411)
loadvec2 <- c(.602,.398,.405,.595,.456,.544)

## Define population Model
modVec <- glue::glue("LV1 =~ {loadvec1[1]}*LV11 + 
                             {loadvec1[2]}*LV12 +
                             {loadvec1[3]}*LV13 +
                             {loadvec1[4]}*LV14 +
                             {loadvec1[5]}*LV15 +
                             {loadvec1[6]}*LV16 
                      LV2 =~ {loadvec2[1]}*LV21 + 
                             {loadvec2[2]}*LV22 +
                             {loadvec2[3]}*LV23 +
                             {loadvec2[4]}*LV24 +
                             {loadvec2[5]}*LV25 +
                             {loadvec2[6]}*LV26 +
                             (.4)*LV15 +(-.4)*LV16
                             
                             
                     ## factor correlation

                     LV1 ~~ 0.6*LV2
                     
                     ### Residual correlations
                     
                     LV13 ~~ .3*LV14
                     LV11 ~~ .2*LV12
                     LV15 ~~ .5*LV16
                     LV23 ~~ .4*LV24
                     LV21 ~~ .3*LV22
                     LV25 ~~ .2*LV26
                     ")

## Generate Data
dat  <- simulateData(modVec,
                     std.lv=T,
                     meanstructure=T, 
                     model.type="cfa",
                     sample.nobs=500,
                     seed=1234567)

## Convert continuous data into descrete observations
for (i in 1:length(names(dat))) {
  dat[,i] <- dat[,i]+abs(min(dat[,i]))
  dat[,i] <- dat[,i]/max(dat[,i])
  dat[,i] <- (dat[,i]*4)+1
  dat[,i] <- round(dat[,i])
}

## Check data distributions
sapply(dat,table)

## Add an ID variable to the dataset
dat$ID <- sprintf("%04.0f", 0001:nrow(dat))

### Fit population Model
modPop <- '

## LV1
LV1 =~ LV11 +
       LV12 +
       LV13 +
       LV14 +
       LV15 +
       LV16 


## LV2
LV2 =~ LV21 +
       LV22 +
       LV23 +
       LV24 +
       LV25 +
       LV26 +
       LV15 + 
       LV16
       
       LV13 ~~ LV14
       LV11 ~~ LV12
       LV15 ~~ LV16
       LV23 ~~ LV24
       LV21 ~~ LV22
       LV25 ~~ LV26
       
'
fitP <- cfa(model = modPop,
            data = dat,
            std.lv=T,
            meanstructure=T)
summary(fitP,
        fit.measures=TRUE,
        standardized=TRUE,
        #modindices=TRUE,
        rsquare=T)

### Fit CFA With observed data
mod1 <- '

## LV1
LV1 =~ LV11 +
       LV12 +
       LV13 +
       LV14 +
       LV15 +
       LV16 


## LV2
LV2 =~ LV21 +
       LV22 +
       LV23 +
       LV24 +
       LV25 +
       LV26
'
fit1 <- cfa(model = mod1,
            data = dat,
            std.lv=T,
            meanstructure=T)
summary(fit1,
        fit.measures=TRUE,
        standardized=TRUE,
        modindices=F,
        rsquare=T)

## Capture the modification indices
mods1 <- modificationIndices(fit1)

## Arrange the modindices by modification index
mods1 <- mods1[,1:5] %>%
  arrange(desc(mi))

## Write out modindices table 1
write.csv(mods1,"c:/Users/zacks/OneDrive/Documents/Grad School/Spring 2020/EPSY 6349 Longitudinal SEM/ModInd.csv",row.names = F)

### Parceling
dat$P11 <- rowMeans(dat[,c("LV11","LV12")])
dat$P12 <- rowMeans(dat[,c("LV13","LV14")])
dat$P13 <- rowMeans(dat[,c("LV15","LV16")])

dat$P21 <- rowMeans(dat[,c("LV21","LV22")])
dat$P22 <- rowMeans(dat[,c("LV23","LV24")])
dat$P23 <- rowMeans(dat[,c("LV25","LV26")])

### Fit parceled models

## Model with 1 parcel
mod2a <- '

## LV1
LV1 =~ LV11 +
       LV12 +
       LV13 +
       LV14 +
       P13 

## LV2
LV2 =~ LV21 +
       LV22 +
       LV23 +
       LV24 +
       LV25 +
       LV26
'
fit2a <- cfa(model = mod2a,
            data = dat,
            std.lv=T,
            meanstructure=T)
summary(fit2a,
        fit.measures=TRUE,
        standardized=TRUE,
        modindices=F,
        rsquare=T)

mods2a <- modificationIndices(fit2a)

mods2a <- mods2a[,1:5] %>%
  arrange(desc(mi))

## Write out modindices table 2
write.csv(mods2a,"c:/Users/zacks/OneDrive/Documents/Grad School/Spring 2020/EPSY 6349 Longitudinal SEM/ModInd2.csv",row.names = F)

## Model with all parcels
mod2 <- '

## LV1
LV1 =~ P11 +
       P12 +
       P13 

## LV2
LV2 =~ P21 +
       P22 +
       P23 
'
fit2 <- cfa(model = mod2,
            data = dat,
            std.lv=T,
            meanstructure=T)
summary(fit2,
        fit.measures=TRUE,
        standardized=TRUE,
        modindices=TRUE,
        rsquare=T)

fitTable2 <- rbind(fitMeasures(fit2,  c("chisq",
                                       "df",
                                       "pvalue",
                                       "tli",
                                       "cfi",
                                       "rmsea",
                                       "srmr")))

## Set up Random Parceling Procedure
fitTable2 <- NA                             ## Ensures the FitTable object is empty
combos <- combinat::permn(seq(1:6))         ## Creates the list of all combinations of 1-6
combos <- combos[seq(1,by=3, len=720/3)]

lv1Vars <- grep("LV1",names(dat),value = T) ## Makes a vector of all LV1 variables
lv2Vars <- grep("LV2",names(dat),value = T) ## Makes a vector of all LV2 variables

## Loop over the combos list and conduct a CFA for every combination of parcel
for (i in 1:length(combos)) {
  dat$P11 <- rowMeans(dat[,c(lv1Vars[combos[[i]][1]],lv1Vars[combos[[i]][2]])])
  dat$P12 <- rowMeans(dat[,c(lv1Vars[combos[[i]][3]],lv1Vars[combos[[i]][4]])])
  dat$P13 <- rowMeans(dat[,c(lv1Vars[combos[[i]][5]],lv1Vars[combos[[i]][6]])])
  
  dat$P21 <- rowMeans(dat[,c(lv2Vars[combos[[i]][1]],lv2Vars[combos[[i]][2]])])
  dat$P22 <- rowMeans(dat[,c(lv2Vars[combos[[i]][3]],lv2Vars[combos[[i]][4]])])
  dat$P23 <- rowMeans(dat[,c(lv2Vars[combos[[i]][5]],lv2Vars[combos[[i]][6]])])
  
  mod2 <- '

## LV1
LV1 =~ P11 +
       P12 +
       P13 

## LV2
LV2 =~ P21 +
       P22 +
       P23 
'
  fit2 <- cfa(model = mod2,
              data = dat,
              std.lv=T,
              meanstructure=T)
  fitTable2 <- rbind(fitTable2,fitMeasures(fit2,  c("chisq",
                                                  "df",
                                                  "pvalue",
                                                  "tli",
                                                  "cfi",
                                                  "rmsea",
                                                  "srmr")))
  
}

## Make the fitTable2 a dataframe
fitTable2 <- as.data.frame(fitTable2)

## Remove top NA row
fitTable2 <- fitTable2[-1,]

## Generate density plots
d_cfi <- density(fitTable2$cfi)
plot(d_cfi,main = "CFI")
abline(v=.9,col="red")

d_tli <- density(fitTable2$tli)
plot(d_tli,main = "TLI")
abline(v=.9,col="red")

d_rmsea <- density(fitTable2$rmsea)
plot(d_rmsea,main = "RMSEA")
abline(v=.08,col="red")

d_srmr <- density(fitTable2$srmr)
plot(d_srmr,main = "SRMR")
abline(v=.08,col="red")

d_chisq <- density(fitTable2$chisq)
plot(d_chisq,main = "CHI SQ")
abline(v=8,col="red")

d2 <- density(fitTable2$pvalue)
plot(d2,main = "p Value")

## Examine output for best parcel combos
fitTable2_CFI <- fitTable2 %>%
  arrange(desc(cfi))

fitTable2_TLI <- fitTable2 %>%
  arrange(desc(tli))

fitTable2_chi <- fitTable2 %>%
  arrange(chisq)

head(fitTable2_CFI)
head(fitTable2_TLI)
head(fitTable2_chi)


#### Balanced ####


## Library Packages
library(lavaan)
library(glue)
library(tidyverse)

## Generate Vectors of factor loadings
loadvec1 <- c(.458,.542,.601,.399,.589,.411)
loadvec2 <- c(.602,.398,.405,.595,.456,.544)
## Define population Model
modVec <- glue::glue("LV1 =~ {loadvec1[1]}*LV11 + 
                             {loadvec1[2]}*LV12 +
                             {loadvec1[3]}*LV13 +
                             {loadvec1[4]}*LV14 +
                             {loadvec1[5]}*LV15 +
                             {loadvec1[6]}*LV16 
                      LV2 =~ {loadvec2[1]}*LV21 + 
                             {loadvec2[2]}*LV22 +
                             {loadvec2[3]}*LV23 +
                             {loadvec2[4]}*LV24 +
                             {loadvec2[5]}*LV25 +
                             {loadvec2[6]}*LV26 
                             
                             
                             
                     ## factor correlation

                     LV1 ~~ 0.6*LV2
                     
                     ### Residual correlations")

## Generate Data
dat  <- simulateData(modVec,
                     std.lv=T,
                     meanstructure=T, 
                     model.type="cfa",
                     sample.nobs=500,
                     seed=1234567)

## Convert continuous data into descrete observations
for (i in 1:length(names(dat))) {
  dat[,i] <- dat[,i]+abs(min(dat[,i]))
  dat[,i] <- dat[,i]/max(dat[,i])
  dat[,i] <- (dat[,i]*4)+1
  dat[,i] <- round(dat[,i])
}

## Check data distributions
sapply(dat,table)

## Add an ID variable to the dataset
dat$ID <- sprintf("%04.0f", 0001:nrow(dat))

### Fit CFA With observed data
mod1 <- '

## LV1
LV1 =~ LV11 +
       LV12 +
       LV13 +
       LV14 +
       LV15 +
       LV16 


## LV2
LV2 =~ LV21 +
       LV22 +
       LV23 +
       LV24 +
       LV25 +
       LV26
'
fit1 <- cfa(model = mod1,
            data = dat,
            std.lv=T,
            meanstructure=T)
summary(fit1,
        fit.measures=TRUE,
        standardized=TRUE,
        modindices=F,
        rsquare=T)

## Capture the modification indices
mods1 <- modificationIndices(fit1)

## Arrange the modindices by modification index
mods1 <- mods1[,1:5] %>%
  arrange(desc(mi))

## Write out modindices table 1
write.csv(mods1,"c:/Users/zacks/OneDrive/Documents/Grad School/Spring 2020/EPSY 6349 Longitudinal SEM/ModInd.csv",row.names = F)

### Parceling
dat$P11 <- rowMeans(dat[,c("LV11","LV12")])
dat$P12 <- rowMeans(dat[,c("LV13","LV14")])
dat$P13 <- rowMeans(dat[,c("LV15","LV16")])

dat$P21 <- rowMeans(dat[,c("LV21","LV22")])
dat$P22 <- rowMeans(dat[,c("LV23","LV24")])
dat$P23 <- rowMeans(dat[,c("LV25","LV26")])

### Fit parceled models

## Model with 1 parcel
mod2a <- '

## LV1
LV1 =~ LV11 +
       LV12 +
       LV13 +
       LV14 +
       P13 

## LV2
LV2 =~ LV21 +
       LV22 +
       LV23 +
       LV24 +
       LV25 +
       LV26
'
fit2a <- cfa(model = mod2a,
             data = dat,
             std.lv=T,
             meanstructure=T)
summary(fit2a,
        fit.measures=TRUE,
        standardized=TRUE,
        modindices=F,
        rsquare=T)

mods2a <- modificationIndices(fit2a)

mods2a <- mods2a[,1:5] %>%
  arrange(desc(mi))

## Write out modindices table 2
write.csv(mods2a,"c:/Users/zacks/OneDrive/Documents/Grad School/Spring 2020/EPSY 6349 Longitudinal SEM/ModInd2.csv",row.names = F)

## Model with all parcels
mod2 <- '

## LV1
LV1 =~ P11 +
       P12 +
       P13 

## LV2
LV2 =~ P21 +
       P22 +
       P23 
'
fit2 <- cfa(model = mod2,
            data = dat,
            std.lv=T,
            meanstructure=T)
summary(fit2,
        fit.measures=TRUE,
        standardized=TRUE,
        modindices=TRUE,
        rsquare=T)

fitTable2 <- rbind(fitMeasures(fit2,  c("chisq",
                                        "df",
                                        "pvalue",
                                        "tli",
                                        "cfi",
                                        "rmsea",
                                        "srmr")))