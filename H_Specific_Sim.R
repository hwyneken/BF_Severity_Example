set.seed(1234)
source("BF_Comp_BruteForce.R")

NSim <- 200
NPerGroup <- 20

Obs_BF_HS <- BF_Comp_Brute_Force(20,5,15,c(0.2,0.3),c(0.7,0.8))
Obs_BF_Error <- BF_Comp_Brute_Force(20,5,15,c(0.25,0.6),c(0.4,0.75)) 
Obs_BF_Test <- Obs_BF_HS / Obs_BF_Error # BF of abot 6.3 - good comparison to vague hypothesis

BF_HSpecific_Sampling <- rep(NA,NSim)
BF_HConstruct_Sampling <- rep(NA,NSim)
BF_HError_Sampling <- rep(NA,NSim)

trueThetaA_Range <- c(0.25,0.6)
trueThetaB_Range <- c(0.4,0.75)

for (i in 1:NSim) {
  print(i)
  
  tempThetaA <- runif(n=1,min=trueThetaA_Range[1],
                      max=trueThetaA_Range[2])
  tempThetaB <- runif(n=1,min=trueThetaB_Range[1],
                      max=trueThetaB_Range[2])
  tempY_A <- rbinom(n=1,size=NPerGroup,prob=tempThetaA)
  tempY_B <- rbinom(n=1,size=NPerGroup,prob=tempThetaB)
  
  propTestA <- prop.test(tempY_A,NPerGroup,conf.level=0.2)
  propTestB <- prop.test(tempY_B,NPerGroup,conf.level=0.2)
  tempCI_A <- propTestA$conf.int[c(1,2)]
  tempCI_B <- propTestB$conf.int[c(1,2)]
  
  BF_HSpecific_Sampling[i] <- BF_Comp_Brute_Force(20,tempY_A,tempY_B,c(0.2,0.3),c(0.7,0.8))
  BF_HConstruct_Sampling[i] <- BF_Comp_Brute_Force(20,tempY_A,tempY_B,
                                                   tempCI_A,tempCI_B)
  BF_HError_Sampling[i] <- BF_Comp_Brute_Force(20,tempY_A,tempY_B,trueThetaA_Range,
                                               trueThetaB_Range)
}


HSpecificSim <- list(Obs_BF_HS = Obs_BF_HS,
                     Obs_BF_Error = Obs_BF_Error,
                     Obs_BF_Test = Obs_BF_Test,
                     BF_HSpecific_Sampling = BF_HSpecific_Sampling,
                     BF_HConstruct_Sampling = BF_HConstruct_Sampling,
                     BF_HError_Sampling = BF_HError_Sampling,
                     Error_Hypothesis = list(A = c(0.25,0.6),B=c(0.4,0.75)))
saveRDS(HSpecificSim,file = "simulation_output/HSpecificSim.RDS")