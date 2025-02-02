### CITATION: This code is modified from scripts shared in Clay et al. 2020, J. Anim. Ecol.

### AIM: Run HMM combinations based on wind and personality predictors; use AIC to identify the best model


# Preamble ----------------------------------------------------------------

#Clear environment and set working directory
rm(list=ls())
setwd("C:/ALAN_StreakedShearwater")

#load packages
library(momentuHMM); library(ggplot2); library(dplyr)

### Create outputs folder if not exist already
out.path <- "./Data_outputs_allOil_allsex/"
if(dir.exists(out.path) == FALSE){
  dir.create(out.path)
}

### LOAD IN TRACKS
load(file="RawData/sw_NB_22to24_envall.Rdata")
data<-as.data.frame(sw_NB_22to24_envall)
data<-data%>%
  select(ID,
         sex,
         avg_rad_5,
         rad_log_5,
         period,
         fraction,
         Oil,
         UTMx,
         UTMy,
         long,
         lat,
         timestamps)
data$sex<-ifelse(data$sex=="F",0,1)
data$Oil<-ifelse(data$Oil=="No",0,1)
dat <- prepData(data,type='UTM',coordNames = c("UTMx","UTMy"))
# Remove errorneous step lengths & only select night
dat <- subset(dat, step < 90000)
dat <- subset(dat,period=="night")
#hist(dat$step)

# INITIALISE HMM DATA ---------------------------------------------------------
#Note: initial value guessed based on the paper Behavioural mapping of a pelagic seabird: combining multiple sensors and a hidden Markov model reveals the distribution of at-sea behaviour
### initial step distribution natural scale parameters
#stepPar0 <- c(0.78,2.41,10.68,0.78,2.41,10.68) This is in km for long/lat prepdata
stepPar0 <- c(780,2410,10680,780,2410,10680)
### initial angle distribution natural scale parameters
anglePar0 <- c(0,pi,0,1,0.5,1)

# RUN THE NULL MODEL ------------------------------------------------------

#  first run null models with no covariates on transition probabilities
set.seed(0805)
stateNames <- c("Raft","Forage", "Travel")

m1 <- fitHMM(data=dat, nbStates=3,
             dist=list(step="gamma",angle="vm"),
             Par0=list(step=stepPar0, angle=anglePar0),
             estAngleMean = list(angle=TRUE),
             stateNames = stateNames)

#save the null model
save(m1, file = "Data_outputs_allOil_allsex/Mod_1.rda")

# SET UP FORMULA FOR ALL MODEL COMBINATIONS -------------------------------

## Run next couple of models for males and females separately - make sure to change model names as appropriate

#Load null model
load("Data_outputs_allOil_allsex/Mod_1.rda")
## Set up personality formulae 

formula <- list()	

formula[[2]] <- ~ rad_log_5+sex+Oil
formula[[3]] <- ~ rad_log_5+fraction+sex+Oil
formula[[4]] <- ~ rad_log_5*fraction+sex+Oil


# this function gets starting values for each model from existing null model fit for a specified covariate formula
Par <- list()
for (i in 2:length(formula)){
  Par[[i]] <- getPar0(model=m1, nbStates=3, formula = formula[[i]])
}



# RUN ALL THE MODELS ------------------------------------------------------

## the following code iterates through and runs all 40 models, pasting each out in turn
stateNames <- c("Raft","Forage", "Travel")
# Specify output lists of length formula
m.list <- vector(mode = "list", length =length(formula))

for (i in 2:length(formula)) {
  print(i)
  m.list[[i]] <- fitHMM(data=dat, nbStates=3,
                        dist=list(step="gamma",angle="vm"),
                        Par0=list(step=Par[[i]]$Par$step, angle=Par[[i]]$Par$angle,delta0 = Par[[i]]$delta),
                        estAngleMean = list(angle=TRUE), beta0 = Par[[i]]$beta,
                        stateNames = stateNames, 
                        formula = formula[[i]])
  model <- m.list[[i]]
  file.out <- paste0("./Data_outputs_allOil_allsex/", paste0("Mod_", i, ".rda"))
  save(model, file = file.out)
}


# FIND THE BEST MODEL -----------------------------------------------------

# Iterate through each model set, load in models, output autocorrelation plots for step lengths and turning angles, pseudo-residual 
# and qq plots, extract model coefficients for each transition and plot, and paste out AIC table

#################################################For Female
# Specifying output lists of length formula
m.list <- vector(mode = "list", length =length(formula))
out.df <- vector(mode = "list", length =length(formula))

for (i in 1:length(formula)) {
  print(i)
  
  file.in <- paste0("./Data_outputs_allOil_allsex/", paste0("Mod_", i, ".rda"))
  load(file = file.in)
  if (i == 1) { m.list[[i]] <- m1} else { m.list[[i]] <- model}
  
  ## Extract AIC of model
  if (i == 1) { form_out <- 1} else { form_out <- as.character(formula[[i]])[2]}
  out.df[[i]] <- data.frame(Model = paste0("m", i),
                            Formula = form_out, AIC = AIC(m.list[[i]]))
}

# Warning messages appear ("removed containing missing values") due to upper and lower CIs which are sometimes "NA"

all.out <- do.call(rbind, out.df)
all.out <- all.out[order(all.out$AIC),]

# Print out AIC table
out.path <- "./Data_outputs_allOil_allsex/AIC_table_sex.csv"
write.csv(all.out, out.path, row.names=T)
