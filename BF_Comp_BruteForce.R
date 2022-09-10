BF_Comp_Brute_Force <- function(NPerGroup,y_A,y_B,range_thetaA,range_thetaB) {
  NumGridPoints <- 300
  
  ## encompassing model, no restrictions
  #   on theta_A or theta_B in [0,1]
  
  thetaAVals_H0 <- seq(from=0,to=1,length=NumGridPoints)
  thetaBVals_H0 <- seq(from=0,to=1,length=NumGridPoints)
  
  thetaGrid_H0 <- expand.grid(thetaA = thetaAVals_H0,
                              thetaB = thetaBVals_H0)
  GridSize_H0 <- dim(thetaGrid_H0)[1]
  totalWeight_H0 <- 0
  
  for (i in 1:GridSize_H0) {
    tempThetaA <- thetaGrid_H0$thetaA[i]
    tempThetaB <- thetaGrid_H0$thetaB[i]
    
    tempLik_A <- dbinom(x=y_A,size=NPerGroup,prob=tempThetaA)
    tempLik_B <- dbinom(x=y_B,size=NPerGroup,prob=tempThetaB)
    totalWeight_H0 <- totalWeight_H0 + tempLik_A*tempLik_B
  }
  averagedLik_H0 <- totalWeight_H0 / GridSize_H0
  
  ##### alternative hypothesis
  thetaAVals_HA <- seq(from=range_thetaA[1],
                       to=range_thetaA[2],
                       length=NumGridPoints)
  thetaBVals_HA <- seq(from=range_thetaB[1],
                       to=range_thetaB[2],
                       length=NumGridPoints)
  thetaGrid_HA <- expand.grid(thetaA = thetaAVals_HA,
                              thetaB = thetaBVals_HA)
  GridSize_HA <- dim(thetaGrid_HA)[1]
  totalWeight_HA <- 0
  
  for (i in 1:GridSize_HA) {
    tempThetaA <- thetaGrid_HA$thetaA[i]
    tempThetaB <- thetaGrid_HA$thetaB[i]
    
    tempLik_A <- dbinom(x=y_A,size=NPerGroup,prob=tempThetaA)
    tempLik_B <- dbinom(x=y_B,size=NPerGroup,prob=tempThetaB)
    totalWeight_HA <- totalWeight_HA + tempLik_A*tempLik_B
  }
  averagedLik_HA <- totalWeight_HA / GridSize_HA
  
  res <- averagedLik_HA / averagedLik_H0
  return(res)
  
}