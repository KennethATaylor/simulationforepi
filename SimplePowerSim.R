library(tmle)

power <- function(intercept, beta, sd, nTotalSubjects, nTreated, 
                    nIterations, alpha=0.05){
  pVec <- rep(NA,nIterations)
  tx <- c(rep(1,nTreated),rep(0,nTotalSubjects - nTreated))
  for(i in 1:nIterations){
    resid <- rep(rnorm(nTotalSubjects,0,sd))
    y <- intercept + beta*tx + resid
    m <- lm(y~tx)
    pVec[i] <- summary(m)$coefficients["tx","Pr(>|t|)"]
  }
  power <- length(pVec[pVec<alpha])/length(pVec)
  return(list(power=power,p=pVec))
}

# Power curve for sample size
sizes <- seq(100, 3000, by=50)
powerVals <- vector(length=length(sizes))
for (i in 1:length(sizes)) {
  powerVals[i] <- fnPower(intercept=1,beta=0.15,sd=1,
                          nTotalSubjects=sizes[i],nTreated=sizes[i]/2,
                          nIterations=100)$power
}
powerCalc <- data.frame(sizes, powerVals)
ggplot(powerCalc) + aes(x=sizes, y=powerVals) + geom_smooth(se=F) + geom_point() + 
  scale_x_continuous("Sample Size") +
  scale_y_continuous("Power")






fnPowerML <- function(intercept, beta, sd, nTotalSubjects, nTreated, 
                    cov1sd, cov1Effect, pCov2, cov2Effect, 
                    cov1AssociationWithTx, cov2AssociationWithTx,
                    useTMLE = F,
                    nIterations, alpha=0.05){
  pVec <- rep(NA,nIterations)
  tx <- c(rep(1,nTreated),rep(0,nTotalSubjects - nTreated))
  if (!is.na(cov1AssociationWithTx)) {
    cov1 <- rnorm(nTreated, 1, cov1sd)
    cov1 <- c(cov1, rnorm(nTotalSubjects - nTreated, 0, cov1sd))
  } else {
    cov1 <- rnorm(nTotalSubjects, 0, cov1sd)
  }
  if (pCov2 < 0.2 | pCov2 > 0.8) { stop ("pCov2 must be between 0.2 and 0.8")}
  if (!is.na(cov2AssociationWithTx)) {
    cov2 <- rbinom(nTreated, 1, pCov2+0.2)
    cov2 <- c(cov2, rbinom(nTotalSubjects - nTreated, 1, pCov2-0.2))
  } else {
    cov2 <- rbinom(nTotalSubjects, 1, pCov2)
  }
  for(i in 1:nIterations){
    resid <- rep(rnorm(nTotalSubjects,0,sd))
    y <- intercept + beta*tx + cov1*cov1Effect + cov2*cov2Effect + resid
    if (useTMLE) {
      m <- tmle(y, tx, cbind(cov1, cov2))
      pVec[i] <- m$estimates$ATE$pvalue
    } else {
      m <- lm(y~tx+cov1+cov2)
      pVec[i] <- summary(m)$coefficients["tx","Pr(>|t|)"]
    }
  }
  power <- length(pVec[pVec<alpha])/length(pVec)
  return(list(power=power,p=pVec))
}

sizes <- seq(100, 3000, by=50)
powerVals <- vector(length=length(sizes))
for (i in 1:length(sizes)) {
  powerVals[i] <- fnPowerML(intercept=1,beta=0.15,sd=1,
                            cov1sd = 2, cov1Effect = 1, pCov2 = 0.5, cov2Effect = 1,
                            cov1AssociationWithTx = T, cov2AssociationWithTx = T,
                            useTMLE = T,
                          nTotalSubjects=sizes[i],nTreated=sizes[i]/2,
                          nIterations=100)$power
}
powerCalcTMLE <- data.frame(sizes, powerVals)
head(powerCalcTMLE)
ggplot(powerCalcTMLE) + aes(x=sizes, y=powerVals) + geom_smooth(se=F) + geom_point() + 
  scale_x_continuous("Sample Size") +
  scale_y_continuous("Power")
head(powerCalcGLM)

ggplot(powerCalcTMLE) + aes(x=sizes, y=powerVals) + geom_smooth(se=F) + geom_point() + 
  scale_x_continuous("Sample Size") +
  scale_y_continuous("Power")

head(powerCalc)


powerCalcGLM$confounding = T
powerCalc$confounding = F
allPower <- rbind(powerCalc, powerCalcGLM)

ggplot(allPower) + aes(x=sizes, y=powerVals, group=confounding, color=confounding) + geom_smooth(se=F) + geom_point() + 
  scale_x_continuous("Sample Size") +
  scale_y_continuous("Power")

powerCalcTMLE$confounding = T
powerCalcTMLE$TMLE = T
powerCalcGLM$TMLE = F

tmleGLM <- rbind(powerCalcTMLE, powerCalcGLM)

ggplot(tmleGLM) + aes(x=sizes, y=powerVals, group=TMLE, color=TMLE) + geom_smooth(se=F) + geom_point() + 
  scale_x_continuous("Sample Size") +
  scale_color_brewer("Machine Learning", palette="Dark2") +
  scale_y_continuous("Power")


test <- tmle(y, tx, cbind(cov1, cov2))
test$estimates$ATE$pvalue
