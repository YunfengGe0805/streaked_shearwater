#Clear environment and set working directory
rm(list=ls())
setwd("C:/ALAN_StreakedShearwater")
library(momentuHMM); 
library(ggplot2)
library(dplyr);library(tidyverse)

## LOAD MODELS
# best model
file.in <- paste0("./Data_outputs_allOil_allsex/", paste0("Mod_", 4, ".rda"))
load(file = file.in)
best.mod <- model

# Get all environmental covariate combinations for model prediction
cov.alan <- seq(from = min(best.mod$data$rad_log_5), to = max(best.mod$data$rad_log_5), by = 0.2)
cov.moon <- c(0.1,0.9)
cov.sex<-c(0,1)
cov.oil<-c(0,1)
cov.comb <- expand.grid(cov.alan, cov.moon,cov.sex,cov.oil)
cov <- data.frame(rad_log_5=cov.comb$Var1, fraction = cov.comb$Var2,
                  sex = cov.comb$Var3,Oil = cov.comb$Var4)
##################################################################################################
##################################################################################################
#stationary probability
sp<-stationary(best.mod,covs=cov)
spm<-as.data.frame(sp)

ci.stationary <- function(model, cov, alpha) {
  gridLength <- nrow(cov)
  model2 <- momentuHMM:::delta_bc(model) # extended format allowing for backwards compatibility
  nbStates <- length(model2$stateNames)
  Sigma <- model2$mod$Sigma
  
  lci <- matrix(NA,gridLength,nbStates)
  uci <- matrix(NA,gridLength,nbStates)
  
  formula <- model2$conditions$formula # for covariates
  newForm <- momentuHMM:::newFormulas(formula, nbStates) # a formula for each state transition
  newformula <- newForm$newformula
  
  nbCovs <- ncol(model.matrix(newformula, model2$data)) - 1 # model.matrix gives values of covariate terms of the formula for each observation
  # in my opinion, nbCovs gives exactly the same number of terms as formula (-1 is minus intercept)
  
  gamInd <- (length(model2$mod$estimate) - (nbCovs + 1) * nbStates * (nbStates - 1) 
             + 1):(length(model2$mod$estimate)) - ncol(model2$covsDelta) * (nbStates - 1) * (!model2$conditions$stationary) # if the mode is stationary there wouldn't be any covariates
  # here we just enumerate parameters leaving out the first ones which are step and angle related, and the last ones which are delta related
  
  rawCovs <- model2$rawCovs
  # changing order of variables tomatch with raw data
  rawCovs <- rawCovs[,order(names(rawCovs))]
  tempCovs <- cov
  tempCovs <- tempCovs[,sort(names(rawCovs))]
  
  # fixing format problems
  for (i in which(unlist(lapply(rawCovs, is.factor)))) {
    tempCovs[[i]] <- factor(tempCovs[[i]], levels = levels(rawCovs[, 
                                                                   i])) 
  }
  tmpSplineInputs <- momentuHMM:::getSplineFormula(newformula, model2$data, 
                                                   tempCovs) # just a format for spline use later
  desMat <- model.matrix(tmpSplineInputs$formula, data = tmpSplineInputs$covs) # tmpSplineInputs$covs is tempCovs
  # model.matrix gives the design matrix for the formula in the argument
  
  probs <- as.data.frame(stationary(model2, covs=desMat)) # the stationary probability is computed based on desMat, which has only a range of values for one of the covariates
  
  for(state in 1:nbStates) {
    dN <- t(apply(desMat, 1, function(x)
      numDeriv::grad(momentuHMM:::get_stat,model2$mod$estimate[gamInd][unique(c(model2$conditions$betaCons))],covs=matrix(x,nrow=1),nbStates=nbStates,i=state,betaRef=model2$conditions$betaRef,betaCons=model2$conditions$betaCons,workBounds=model2$conditions$workBounds$beta)))
    
    se <- t(apply(dN, 1, function(x)
      suppressWarnings(sqrt(x%*%Sigma[gamInd[unique(c(model2$conditions$betaCons))],gamInd[unique(c(model2$conditions$betaCons))]]%*%x))))
    
    lci[,state] <- 1/(1 + exp(-(log(probs[,state]/(1-probs[,state])) -
                                  qnorm(1-(1-alpha)/2) * (1/(probs[,state]-probs[,state]^2)) * se)))
    uci[,state] <- 1/(1 + exp(-(log(probs[,state]/(1-probs[,state])) +
                                  qnorm(1-(1-alpha)/2) * (1/(probs[,state]-probs[,state]^2)) * se)))
  }
  lci <- as.data.frame(lci)
  names(lci) <- model$stateNames
  uci <- as.data.frame(uci)
  names(uci) <- model$stateNames
  ci.list <- list(lci, uci)
  names(ci.list) <- c("lower", "upper")
  return(ci.list)
}


# Create confidence intervals around stationary probabilities
ci <- ci.stationary(model = best.mod, cov = cov, alpha = 0.95) 
u<-ci$upper
l<-ci$lower

New<-"black"
Full<-"orange"

Forage<-cbind(spm$Forage,u$Forage,l$Forage,cov$rad_log_5,cov$sex,cov$fraction,cov$Oil)
Forage<-as.data.frame(Forage)
colnames(Forage)[1]<-"sp"
colnames(Forage)[2]<-"u"
colnames(Forage)[3]<-"l"
colnames(Forage)[4]<-"alan"
colnames(Forage)[5]<-"sex"
colnames(Forage)[6]<-"moon_fraction"
colnames(Forage)[7]<-"Oil"
Forage$sex<-as.factor(ifelse(Forage$sex==0,"Female","Male"))
Forage$moon_fraction<-as.factor(ifelse(Forage$moon_fraction==0.1,"New","Full"))
Forage$Oil<-as.factor(ifelse(Forage$Oil==0,"No","Yes"))
Fsp<-ggplot(Forage, aes(x = alan, y=sp))+facet_wrap(~Oil)+
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=l, ymax=u,col=moon_fraction,fill=sex), alpha=0.15) + 
  geom_line(aes(colour = moon_fraction,linetype=sex), size = 1.3)+
  scale_color_manual(name = "moon phase", labels = c("Full", "New"), values = c(Full, New)) +
  ggtitle("Forage Stationary Probability") +
  ylab("Probability") + 
  xlab("log(ALAN)") + theme_bw()
Fsp
ggsave("Data_outputs_allOil_allsex/Fsp.png",width = 15, height = 7, units = "in", dpi = 350)

Raft<-cbind(spm$Raft,u$Raft,l$Raft,cov$rad_log_5,cov$sex,cov$fraction,cov$Oil)
Raft<-as.data.frame(Raft)
colnames(Raft)[1]<-"sp"
colnames(Raft)[2]<-"u"
colnames(Raft)[3]<-"l"
colnames(Raft)[4]<-"alan"
colnames(Raft)[5]<-"sex"
colnames(Raft)[6]<-"moon_fraction"
colnames(Raft)[7]<-"Oil"
Raft$sex<-as.factor(ifelse(Raft$sex==0,"Female","Male"))
Raft$moon_fraction<-as.factor(ifelse(Raft$moon_fraction==0.1,"New","Full"))
Raft$Oil<-as.factor(ifelse(Raft$Oil==0,"No","Yes"))
Rsp<-ggplot(Raft, aes(x = alan, y=sp))+facet_wrap(~Oil)+
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=l, ymax=u,col=moon_fraction,fill=sex), alpha=0.15) + 
  geom_line(aes(colour = moon_fraction,linetype=sex), size = 1.3)+
  scale_color_manual(name = "moon phase", labels = c("Full", "New"), values = c(Full, New)) +
  ggtitle("Raft Stationary Probability") +
  ylab("Probability") + 
  xlab("log(ALAN)") + theme_bw()
Rsp
ggsave("Data_outputs_allOil_allsex/Rsp.png",width = 15, height = 7, units = "in", dpi = 350)

Travel<-cbind(spm$Travel,u$Travel,l$Travel,cov$rad_log_5,cov$sex,cov$fraction,cov$Oil)
Travel<-as.data.frame(Travel)
colnames(Travel)[1]<-"sp"
colnames(Travel)[2]<-"u"
colnames(Travel)[3]<-"l"
colnames(Travel)[4]<-"alan"
colnames(Travel)[5]<-"sex"
colnames(Travel)[6]<-"moon_fraction"
colnames(Travel)[7]<-"Oil"
Travel$sex<-as.factor(ifelse(Travel$sex==0,"Female","Male"))
Travel$moon_fraction<-as.factor(ifelse(Travel$moon_fraction==0.1,"New","Full"))
Travel$Oil<-as.factor(ifelse(Travel$Oil==0,"No","Yes"))
Tsp<-ggplot(Travel, aes(x = alan, y=sp))+facet_wrap(~Oil)+
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=l, ymax=u,col=moon_fraction,fill=sex), alpha=0.15) + 
  geom_line(aes(colour = moon_fraction,linetype=sex), size = 1.3)+
  scale_color_manual(name = "moon phase", labels = c("Full", "New"), values = c(Full, New)) +
  ggtitle("Travel Stationary Probability") +
  ylab("Probability") +
  xlab("log(ALAN)") + theme_bw()
Tsp
ggsave("Data_outputs_allOil_allsex/Tsp.png",width = 15, height = 7, units = "in", dpi = 350)

#############################################################################################
###############################################################################################
#transition probability
#tp<-getTrProbs(cov,nbStates = 3,beta=best.mod$mle$beta,formula=best.mod$conditions$formula,getCI=TRUE)
#tpm<-as.data.frame(t(as.data.frame(tp)))
#colnames(tpm)[1]<-"Raft"
#colnames(tpm)[2]<-"Forage"
#colnames(tpm)[3]<-"Travel"


tock <- Sys.time()
tplist <- lapply(1:nrow(cov), function(x) {
  print(x)
  cov.sub.df <- cov[x,]
  return(CIreal(best.mod,covs=cov.sub.df)$gamma)
})
tick <- Sys.time()
tick-tock

# Extract means, upper and lower bounds and put into separate lists
tp<- lapply(tplist, '[[', 1) 
tp<-as.data.frame(t(as.data.frame(tp)))
tp$class<-rep(c("Raft","Forage","Travel"),times=3,length=nrow(tp))
l <- lapply(tplist,'[[',3)
l<-as.data.frame(t(as.data.frame(l)))
l$class<-rep(c("Raft","Forage","Travel"),times=3,length=nrow(l))
u <- lapply(tplist,'[[',4)
u<-as.data.frame(t(as.data.frame(u)))
u$class<-rep(c("Raft","Forage","Travel"),times=3,length=nrow(u))


#AlltoF
AlltoF<-subset(tp,class=="Forage")%>%
  mutate(alan=cov$rad_log_5,
         moon_fraction=cov$fraction,
         u=subset(u,class=="Forage"),
         l=subset(l,class=="Forage"),
         sex=cov$sex,
         Oil=cov$Oil)
AlltoF$sex<-as.factor(ifelse(AlltoF$sex==0,"Female","Male"))
AlltoF$moon_fraction<-as.factor(ifelse(AlltoF$moon_fraction==0.1,"New","Full"))
AlltoF$Oil<-as.factor(ifelse(AlltoF$Oil==0,"No","Yes"))
write.csv(AlltoF,file="Data_outputs_allOil_allsex/AlltoF.csv")
save(AlltoF,file="Data_outputs_allOil_allsex/AlltoF.Rdata")

#AlltoT
AlltoT<-subset(tp,class=="Travel")%>%
  mutate(alan=cov$rad_log_5,
         moon_fraction=cov$fraction,
         u=subset(u,class=="Travel"),
         l=subset(l,class=="Travel"),
         sex=cov$sex,
         Oil=cov$Oil)
AlltoT$sex<-as.factor(ifelse(AlltoT$sex==0,"Female","Male"))
AlltoT$moon_fraction<-as.factor(ifelse(AlltoT$moon_fraction==0.1,"New","Full"))
AlltoT$Oil<-as.factor(ifelse(AlltoT$Oil==0,"No","Yes"))
write.csv(AlltoT,file="Data_outputs_allOil_allsex/AlltoT.csv")
save(AlltoT,file="Data_outputs_allOil_allsex/AlltoT.Rdata")

#AlltoR
AlltoR<-subset(tp,class=="Raft")%>%
  mutate(alan=cov$rad_log_5,
         moon_fraction=cov$fraction,
         u=subset(u,class=="Raft"),
         l=subset(l,class=="Raft"),
         sex=cov$sex,
         Oil=cov$Oil)
AlltoR$sex<-as.factor(ifelse(AlltoR$sex==0,"Female","Male"))
AlltoR$moon_fraction<-as.factor(ifelse(AlltoR$moon_fraction==0.1,"New","Full"))
AlltoR$Oil<-as.factor(ifelse(AlltoR$Oil==0,"No","Yes"))
write.csv(AlltoR,file="Data_outputs_allOil_allsex/AlltoR.csv")
save(AlltoR,file="Data_outputs_allOil_allsex/AlltoR.Rdata")

### BUILD PLOT 

New<-"black"
Full<-"orange"


#################################################To forage
load(file="Data_outputs_allOil_allsex/AlltoF.Rdata")
#Raft to forage
RtoF<-ggplot(AlltoF, aes(x = alan, y=Raft))+facet_wrap(~Oil)+
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=l$Raft, ymax=u$Raft,col=moon_fraction,fill=sex), alpha=0.15) + 
  geom_line(aes(colour = moon_fraction,linetype = sex), size = 1.3)+
  scale_color_manual(name = "moon phase", labels = c("Full", "New"), values = c(Full, New)) +
  ggtitle("Transition Probability from Raft to Forage") +
  ylab("Probability")+
  xlab("log(ALAN)")+theme_bw()
RtoF
ggsave("Data_outputs_allOil_allsex/RtoF.png",width = 15, height = 7, units = "in", dpi = 350)

#Travel to forage
TtoF<-ggplot(AlltoF, aes(x = alan, y=Travel))+facet_wrap(~Oil)+
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=l$Travel, ymax=u$Travel,col=moon_fraction,fill=sex), alpha=0.15) + 
  geom_line(aes(colour = moon_fraction,linetype = sex), size = 1.3)+
  scale_color_manual(name = "moon phase", labels = c("Full", "New"), values = c(Full, New)) +
  ggtitle("Transition Probability from Travel to Forage") +
  ylab("Probability") + 
  xlab("log(ALAN)") + theme_bw()
TtoF
ggsave("Data_outputs_allOil_allsex/TtoF.png",width = 15, height = 7, units = "in", dpi = 350)

#forage to forage
FtoF<-ggplot(AlltoF, aes(x = alan, y=Forage))+facet_wrap(~Oil)+
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=l$Forage, ymax=u$Forage,col=moon_fraction,fill=sex), alpha=0.15) + 
  geom_line(aes(colour = moon_fraction,linetype = sex), size = 1.3)+
  scale_color_manual(name = "moon phase", labels = c("Full", "New"), values = c(Full, New)) +
  ggtitle("Transition Probability from Forage to Forage") +
  ylab("Probability") + 
  xlab("log(ALAN)") + theme_bw()
FtoF
ggsave("Data_outputs_allOil_allsex/FtoF.png",width = 15, height = 7, units = "in", dpi = 350)


##################################To raft
load(file="Data_outputs_allOil_allsex/AlltoR.Rdata")
#raft to raft
RtoR<-ggplot(AlltoR, aes(x = alan, y=Raft))+facet_wrap(~Oil)+
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=l$Raft, ymax=u$Raft,col=moon_fraction,fill=sex), alpha=0.15) + 
  geom_line(aes(colour = moon_fraction,linetype = sex), size = 1.3)+
  scale_color_manual(name = "moon phase", labels = c("Full", "New"), values = c(Full, New)) +
  ggtitle("Transition Probability from Raft to Raft") +
  ylab("Probability") + 
  xlab("log(ALAN)") + theme_bw()
RtoR
ggsave("Data_outputs_allOil_allsex/RtoR.png",width = 15, height = 7, units = "in", dpi = 350)

#Travel to raft
TtoR<-ggplot(AlltoR, aes(x = alan, y=Travel))+facet_wrap(~Oil)+
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=l$Travel, ymax=u$Travel,col=moon_fraction,fill=sex), alpha=0.15) + 
  geom_line(aes(colour = moon_fraction,linetype = sex), size = 1.3)+
  scale_color_manual(name = "moon phase", labels = c("Full", "New"), values = c(Full, New)) +
  ggtitle("Transition Probability from Travel to Raft") +
  ylab("Probability") + 
  xlab("log(ALAN)") + theme_bw()
TtoR
ggsave("Data_outputs_allOil_allsex/TtoR.png",width = 15, height = 7, units = "in", dpi = 350)

#forage to raft
FtoR<-ggplot(AlltoR, aes(x = alan, y=Forage))+facet_wrap(~Oil)+
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=l$Forage, ymax=u$Forage,col=moon_fraction,fill=sex), alpha=0.15) + 
  geom_line(aes(colour = moon_fraction,linetype = sex), size = 1.3)+
  scale_color_manual(name = "moon phase", labels = c("Full", "New"), values = c(Full, New)) +
  ggtitle("Transition Probability from Forage to Raft") +
  ylab("Probability") + 
  xlab("log(ALAN)") + theme_bw()
FtoR
ggsave("Data_outputs_allOil_allsex/FtoR.png",width = 15, height = 7, units = "in", dpi = 350)

##################################To travel
load(file="Data_outputs_allOil_allsex/AlltoT.Rdata")
#raft to travel
RtoT<-ggplot(AlltoT, aes(x = alan, y=Raft))+facet_wrap(~Oil)+
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=l$Raft, ymax=u$Raft,col=moon_fraction,fill=sex), alpha=0.15) + 
  geom_line(aes(colour = moon_fraction,linetype = sex), size = 1.3)+
  scale_color_manual(name = "moon phase", labels = c("Full", "New"), values = c(Full, New)) +
  ggtitle("Transition Probability from Raft to Travel") +
  ylab("Probability") +
  xlab("log(ALAN)") + theme_bw()
RtoT
ggsave("Data_outputs_allOil_allsex/RtoT.png",width = 15, height = 7, units = "in", dpi = 350)

#Travel to travel
TtoT<-ggplot(AlltoT, aes(x = alan, y=Travel))+facet_wrap(~Oil)+
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=l$Travel, ymax=u$Travel,col=moon_fraction,fill=sex), alpha=0.15) + 
  geom_line(aes(colour = moon_fraction,linetype = sex), size = 1.3)+
  scale_color_manual(name = "moon phase", labels = c("Full", "New"), values = c(Full, New)) +
  ggtitle("Transition Probability from Travel to Travel") +
  ylab("Probability") + 
  xlab("log(ALAN)") + theme_bw()
TtoT
ggsave("Data_outputs_allOil_allsex/TtoT.png",width = 15, height = 7, units = "in", dpi = 350)

#forage to travel
FtoT<-ggplot(AlltoT, aes(x = alan, y=Forage))+facet_wrap(~Oil)+
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=l$Forage, ymax=u$Forage,col=moon_fraction,fill=sex), alpha=0.15) + 
  geom_line(aes(colour = moon_fraction,linetype = sex), size = 1.3)+
  scale_color_manual(name = "moon phase", labels = c("Full", "New"), values = c(Full, New)) +
  ggtitle("Transition Probability from Forage to Travel") +
  ylab("Probability") + 
  xlab("log(ALAN)") + theme_bw()
FtoT
ggsave("Data_outputs_allOil_allsex/FtoT.png",width = 15, height = 7, units = "in", dpi = 350)


