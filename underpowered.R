library("fmsb")
library("ddpcr")  # only needed for the "quiet()" function
set.seed(12345)
N_sim <- 10000  # Is this sim study adequately powered?


# This result is compatible with observing 5 cases of thrombosis 
# out of 3 million vaccinated individuals and 1 case amongst 3 million 
# unvaccinated individuals.
riskratio(5, 1, 3000000, 3000000)

# Assuming a true baseline risk amongst the unvaccinated of 1 in 3 million 
# and a true risk ratio of 5, a study with 3 million vaccinated individuals 
# and 3 million unvaccinated individuals would have statistical power of about 37\%.
simdata <- data.frame(matrix(0,N_sim,1))
simdata$study_pval
pb <- txtProgressBar(min = 0, max = N_sim, style = 3)
for(i in 1:(N_sim)){
  simdata$n.exp[i] <- 3000000
  simdata$n.cont[i] <- 3000000
  baseline_risk <- 1/3000000
  risk_ratio <- 5
  simdata$ev.exp[i] <- sum(rbinom(simdata$n.exp[i], 1, risk_ratio*baseline_risk))
  simdata$ev.cont[i] <- sum(rbinom(simdata$n.cont[i] , 1, baseline_risk))
  quiet(simdata$study_pval[i] <- riskratio(simdata$ev.exp[i], simdata$ev.cont[i], 
                                           simdata$n.exp[i],simdata$n.cont[i])$p.value)
  setTxtProgressBar(pb, i)
}

mean(simdata$study_pval<0.05, na.rm=TRUE)
### XXX1
### 0.3710503


# if one assumes a true risk ratio of 10 (a large but perhaps not implausible number 
# to consider given the context of the question), the study would have 80\% power.
simdata <- data.frame(matrix(0,N_sim,1))
simdata$study_pval
pb <- txtProgressBar(min = 0, max = N_sim, style = 3)
for(i in 1:(N_sim)){
  simdata$n.exp[i] <- 3000000
  simdata$n.cont[i] <- 3000000
  baseline_risk <- 1/3000000
  risk_ratio <- 10
  simdata$ev.exp[i] <- sum(rbinom(simdata$n.exp[i], 1, risk_ratio*baseline_risk))
  simdata$ev.cont[i] <- sum(rbinom(simdata$n.cont[i] , 1, baseline_risk))
  quiet(simdata$study_pval[i] <- riskratio(simdata$ev.exp[i], simdata$ev.cont[i], 
                                           simdata$n.exp[i],simdata$n.cont[i])$p.value)
  setTxtProgressBar(pb, i)
}
mean(simdata$study_pval<0.05, na.rm=TRUE)
### XXX2
### 0.835



# Suppose that the hypothetical study had only 1 million vaccinated and 1 million unvaccinated individuals.
# Then, assuming once again a true baseline risk amongst the unvaccinated of 1 in 3 million 
# and a true risk ratio of 5, the hypothetical study would have power of about 8\%.  
simdata <- data.frame(matrix(0,N_sim,1))
simdata$study_pval
pb <- txtProgressBar(min = 0, max = N_sim, style = 3)
for(i in 1:(N_sim)){
  simdata$n.exp[i] <- 1000000
  simdata$n.cont[i] <- 1000000
  baseline_risk <- 1/3000000
  risk_ratio <- 5
  simdata$ev.exp[i] <- sum(rbinom(simdata$n.exp[i], 1, risk_ratio*baseline_risk))
  simdata$ev.cont[i] <- sum(rbinom(simdata$n.cont[i] , 1, baseline_risk))
  quiet(simdata$study_pval[i] <- riskratio(simdata$ev.exp[i], simdata$ev.cont[i], 
                                           simdata$n.exp[i],simdata$n.cont[i])$p.value)
  setTxtProgressBar(pb, i)
}
mean(simdata$study_pval<0.05, na.rm=TRUE)
### XXX3
### 0.07748993



# With a true risk ratio of 10, the study has power of about 32\%.
simdata <- data.frame(matrix(0,N_sim,1))
simdata$study_pval
pb <- txtProgressBar(min = 0, max = N_sim, style = 3)
for(i in 1:(N_sim)){
  simdata$n.exp[i] <- 1000000
  simdata$n.cont[i] <- 1000000
  baseline_risk <- 1/3000000
  risk_ratio <- 10
  simdata$ev.exp[i] <- sum(rbinom(simdata$n.exp[i], 1, risk_ratio*baseline_risk))
  simdata$ev.cont[i] <- sum(rbinom(simdata$n.cont[i] , 1, baseline_risk))
  quiet(simdata$study_pval[i] <- riskratio(simdata$ev.exp[i], simdata$ev.cont[i], 
                                           simdata$n.exp[i],simdata$n.cont[i])$p.value)
  setTxtProgressBar(pb, i)
}
mean(simdata$study_pval<0.05, na.rm=TRUE)
### XXX4
### 0.3210256

# In order for the study to have a power of 80\%, one must assume a true risk ratio of more than 20.
simdata <- data.frame(matrix(0,N_sim,1))
simdata$study_pval
pb <- txtProgressBar(min = 0, max = N_sim, style = 3)
for(i in 1:(N_sim)){
  simdata$n.exp[i] <- 1000000
  simdata$n.cont[i] <- 1000000
  baseline_risk <- 1/3000000
  risk_ratio <- 20
  simdata$ev.exp[i] <- sum(rbinom(simdata$n.exp[i], 1, risk_ratio*baseline_risk))
  simdata$ev.cont[i] <- sum(rbinom(simdata$n.cont[i] , 1, baseline_risk))
  quiet(simdata$study_pval[i] <- riskratio(simdata$ev.exp[i], simdata$ev.cont[i], 
                                           simdata$n.exp[i],simdata$n.cont[i])$p.value)
  setTxtProgressBar(pb, i)  
}
mean(simdata$study_pval<0.05, na.rm=TRUE)
### XXX5
### 0.7745196


N_sim <- 1000

library(meta)
data(Olkin1995)
# Assuming that the true risk ratio in each study population is 5, we find 
# that about 11 studies of equal sample size (each study having 2 million 
# participants) would be required for a standard fixed-effect meta-analysis 
# to have the often sought-after 80\% power.
pval_vec_fixed <- rep(NA, N_sim)
pb <- txtProgressBar(min = 0, max = N_sim, style = 3)
for(j in 1:N_sim){
  N_studies <- 11
  simdata<-Olkin1995[1:N_studies,]
  
  for(i in 1:N_studies){
    simdata$n.exp[i] <- 1000000
    simdata$n.cont[i] <- 1000000
    baseline_risk <- 1/3000000
    risk_ratio <- 5
    simdata$ev.exp[i] <- sum(rbinom(simdata$n.exp[i], 1, risk_ratio*baseline_risk))
    simdata$ev.cont[i] <- sum(rbinom(simdata$n.cont[i] , 1, baseline_risk))
  }
  
  m1 <- metabin(ev.exp, n.exp, ev.cont, n.cont,
                data = simdata,
                studlab = paste(author, year),
                sm = "RR", method = "MH")
  
  pval_vec_fixed[j]<- m1$pval.fixed
  setTxtProgressBar(pb, i)
}
mean(pval_vec_fixed<0.05)
### XXX6
### 0.839

# Instead, if we assume that across all study populations, the true risk ratios range 
# from 4 to 6 (evenly-spaced), we find that about 14 studies of equal sample size would be 
# required for a standard random-effects meta-analysis to have 80\% power.
pval_vec <- rep(NA, N_sim)
pb <- txtProgressBar(min = 0, max = N_sim, style = 3)
for(j in 1:N_sim){
  N_studies <- 14
  simdata<-Olkin1995[1:N_studies,]
  
  for(i in 1:N_studies){
    simdata$n.exp[i] <- 1000000
    simdata$n.cont[i] <- 1000000
    baseline_risk <- 1/3000000
    risk_ratio <- runif(1, 4, 6)
    simdata$ev.exp[i] <- sum(rbinom(simdata$n.exp[i], 1, risk_ratio*baseline_risk))
    simdata$ev.cont[i] <- sum(rbinom(simdata$n.cont[i] , 1, baseline_risk))
  }
  
  m1 <- metabin(ev.exp, n.exp, ev.cont, n.cont,
                data = simdata,
                studlab = paste(author, year),
                sm = "RR", method = "MH")
  
  pval_vec[j]<- m1$pval.random
  setTxtProgressBar(pb, i)
}

mean(pval_vec<0.05)
### XXX7
### 0.827

# The trim and fill robust random-effect meta-analysis (the most popular approach to 
# correct for potential publication bias) will have less 
# power: our hypothetical analysis with 14 studies will have only about 65\% power. 
pval_vec <- rep(NA, N_sim)
pb <- txtProgressBar(min = 0, max = N_sim, style = 3)
for(j in 1:N_sim){
  N_studies <- 14
  simdata<-Olkin1995[1:N_studies,]
  
  for(i in 1:N_studies){
    simdata$n.exp[i] <- 1000000
    simdata$n.cont[i] <- 1000000
    baseline_risk <- 1/3000000
    risk_ratio <- runif(1, 4, 6)
    simdata$ev.exp[i] <- sum(rbinom(simdata$n.exp[i], 1, risk_ratio*baseline_risk))
    simdata$ev.cont[i] <- sum(rbinom(simdata$n.cont[i] , 1, baseline_risk))
  }
  
  m1 <- metabin(ev.exp, n.exp, ev.cont, n.cont,
                data = simdata,
                studlab = paste(author, year),
                sm = "RR", method = "MH")
  
  quiet(pval_vec[j] <- trimfill(m1)$pval.random)
  setTxtProgressBar(pb, i)
}

mean(pval_vec<0.05)
### XXX8
### 0.645

# Starting with the 14 studies as above, say that those yielding p-val>0.05
# have a 50% chance of being published, while the remainder have a 100% chance of being published. 
# A trim-and-fill random-effects meta-analysis of the published studies will have about 50% power.
pval_vec <- rep(NA, N_sim)
pb <- txtProgressBar(min = 0, max = N_sim, style = 3)
for(j in 1:N_sim){
  N_studies <- 14
  simdata<-Olkin1995[1:N_studies,]
  
  for(i in 1:N_studies){
    simdata$n.exp[i] <- 1000000
    simdata$n.cont[i] <- 1000000
    baseline_risk <- 1/3000000
    risk_ratio <- runif(1, 4, 6)
    simdata$ev.exp[i] <- sum(rbinom(simdata$n.exp[i], 1, risk_ratio*baseline_risk))
    simdata$ev.cont[i] <- sum(rbinom(simdata$n.cont[i] , 1, baseline_risk))
    
    quiet(study_pval <- riskratio(simdata$ev.exp[i], simdata$ev.cont[i], 
                                  simdata$n.exp[i],simdata$n.cont[i])$p.value)
    if(is.na(study_pval)){study_pval <- 1}
    if(study_pval>0.05  & (runif(1)<0.5)){ simdata[i,] <- NA }
  }
  
  m1 <- metabin(ev.exp, n.exp, ev.cont, n.cont,
                data = na.omit(simdata),
                studlab = paste(author, year),
                sm = "RR", method = "MH")
  
  quiet(pval_vec[j] <- m1$pval.random)
  try({
    quiet(pval_vec[j] <- trimfill(m1)$pval.random)}, silent=TRUE)
  setTxtProgressBar(pb, i)
  }
mean(pval_vec<0.05)
### XXX9
### 0.479


# Indeed, under these conditions one would need about 30 studies 
# (a total of 60 million participants) for the  meta-analysis to have 80% power. 
pval_vec <- rep(NA, N_sim)
pb <- txtProgressBar(min = 0, max = N_sim, style = 3)
for(j in 1:N_sim){
  N_studies <- 30
  simdata<-Olkin1995[1:N_studies,]
  
  for(i in 1:N_studies){
    simdata$n.exp[i] <- 1000000
    simdata$n.cont[i] <- 1000000
    baseline_risk <- 1/3000000
    risk_ratio <- runif(1, 4, 6)
    simdata$ev.exp[i] <- sum(rbinom(simdata$n.exp[i], 1, risk_ratio*baseline_risk))
    simdata$ev.cont[i] <- sum(rbinom(simdata$n.cont[i] , 1, baseline_risk))
    
    quiet(study_pval <- riskratio(simdata$ev.exp[i], simdata$ev.cont[i], 
                                  simdata$n.exp[i],simdata$n.cont[i])$p.value)
    if(is.na(study_pval)){study_pval <- 1}
    if(study_pval>0.05  & (runif(1)<0.5)){ simdata[i,] <- NA }
  }
  
  m1 <- metabin(ev.exp, n.exp, ev.cont, n.cont,
                data = na.omit(simdata),
                studlab = paste(author, year),
                sm = "RR", method = "MH")
  
  quiet(pval_vec[j] <- m1$pval.random)
  try({
    quiet(pval_vec[j] <- trimfill(m1)$pval.random)}, silent=TRUE)
  setTxtProgressBar(pb, i)
}
mean(pval_vec<0.05)
### XXX10
### 0.793

