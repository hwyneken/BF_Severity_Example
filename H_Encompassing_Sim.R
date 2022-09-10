set.seed(1234)
source("BF_Comp_BruteForce.R")

NSim <- 200
NPerGroup <- 20

Obs_BF_HV <- BF_Comp_Brute_Force(20,5,15,c(0.1,0.49),c(0.51,0.9))
Obs_BF_HS <- BF_Comp_Brute_Force(20,5,15,c(0.2,0.3),c(0.7,0.8))

BF_HVague_Sampling <- rep(NA,NSim)
BF_HSpecific_Sampling <- rep(NA,NSim)

BF_HConstruct95_Sampling <- rep(NA,NSim)
BF_HConstruct20_Sampling <- rep(NA,NSim)

trueThetaA_Range <- c(0,1)
trueThetaB_Range <- c(0,1)

for (i in 1:NSim) {
  print(i)
  
  tempThetaA <- runif(n=1,min=trueThetaA_Range[1],
                      max=trueThetaA_Range[2])
  tempThetaB <- runif(n=1,min=trueThetaB_Range[1],
                      max=trueThetaB_Range[2])
  tempY_A <- rbinom(n=1,size=NPerGroup,prob=tempThetaA)
  tempY_B <- rbinom(n=1,size=NPerGroup,prob=tempThetaB)
  
  propTestA_20 <- prop.test(tempY_A,NPerGroup,conf.level=0.2)
  propTestB_20 <- prop.test(tempY_B,NPerGroup,conf.level=0.2)
  tempCI_A20 <- propTestA_20$conf.int[c(1,2)]
  tempCI_B20 <- propTestB_20$conf.int[c(1,2)]
  
  propTestA_95 <- prop.test(tempY_A,NPerGroup,conf.level=0.95)
  propTestB_95 <- prop.test(tempY_B,NPerGroup,conf.level=0.95)
  tempCI_A95 <- propTestA_95$conf.int[c(1,2)]
  tempCI_B95 <- propTestB_95$conf.int[c(1,2)]
  
  BF_HVague_Sampling[i] <- BF_Comp_Brute_Force(20,tempY_A,tempY_B,c(0.1,0.5),c(0.5,0.9))
  BF_HSpecific_Sampling[i] <- BF_Comp_Brute_Force(20,tempY_A,tempY_B,c(0.2,0.3),c(0.7,0.8))
  
  
  BF_HConstruct95_Sampling[i] <- BF_Comp_Brute_Force(20,tempY_A,tempY_B,
                                                     tempCI_A95,tempCI_B95)
  BF_HConstruct20_Sampling[i] <- BF_Comp_Brute_Force(20,tempY_A,tempY_B,
                                                   tempCI_A20,tempCI_B20)

}

HEncompSim <- list(Obs_BF_HV = Obs_BF_HV,
                   Obs_BF_HS = Obs_BF_HS,
                   BF_HVague_Sampling = BF_HVague_Sampling,
                   BF_HSpecific_Sampling = BF_HSpecific_Sampling,
                   BF_HConstruct_Sampling95 = BF_HConstruct95_Sampling,
                   BF_HConstruct20_Sampling = BF_HConstruct20_Sampling,
                   Error_Hypothesis = list(A = c(0,1),B=c(0,1)))
saveRDS(HEncompSim,file = "simulation_output/HEncompSim.RDS")