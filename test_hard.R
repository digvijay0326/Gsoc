library(dirichletprocess)
library(ggplot2)
##### geometric beta model
Likelihood.geometric <- function(mdobj, x, theta){
  return(as.numeric(dgeom(x, theta[[1]])))
}

PriorDraw.geometric <- function(mdobj, n){
  draws <- rbeta(n, mdobj$priorParameters[1], mdobj$priorParameters[2])
  theta <- list(array(draws, dim=c(1,1,n)))
  return(theta)
}

PosteriorDraw.geometric <- function(mdobj, x, n=1){
  priorParameters <- mdobj$priorParameters
  lambda <- rbeta(n, priorParameters[1] + sum(x), priorParameters[2] + nrow(x))
  return(list(array(lambda, dim=c(1,1,n))))
}

Predictive.geometric <- function(mdobj, x){
  priorParameters <- mdobj$priorParameters
  pred <- numeric(length(x))
  for(i in seq_along(x)){
    alphaPost <- priorParameters[1] + 1
    betaPost <- priorParameters[2] + x[i] - 1
    pred[i] <- gamma(priorParameters[1] + priorParameters[2]) / (gamma(priorParameters[2])*gamma(priorParameters[1]))
    pred[i] <- pred[i] *(gamma(betaPost)*gamma(alphaPost)) / gamma(alphaPost+betaPost)
  }
  return(pred)
}

geometricMd <- MixingDistribution(distribution="geometric",
                              priorParameters = c(2, 2),
                              conjugate="conjugate")

y <- c(rgeom(150, 0.4), rgeom(150, 0.5)) 
plot(hist(y))
dp <- DirichletProcessCreate(y, geometricMd)
dp <- Initialise(dp)
dp <- Fit(dp, 1000)
pf <- PosteriorFrame(dp, 1:50, 1000)
trueFrame <- data.frame(x= 1:50,
                            y= 0.5*dgeom(1:50, 0.4) + 0.5*dgeom(1:50, 0.5))
ggplot() +
  geom_ribbon(data=pf,
                aes(x=x, ymin=X5., ymax=X95.),
                colour=NA,
                fill="red",
                alpha=0.2) + 
  geom_line(data=pf, aes(x=x, y=Mean), colour="red") + 
  geom_line(data=trueFrame, aes(x=x, y=y)) 
