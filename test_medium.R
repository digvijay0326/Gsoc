library(ggplot2)
library(dirichletprocess)
n <- 1000
x <- c(rlnorm(n, meanlog = log(1), sdlog = 1), rlnorm(n, meanlog = log(1), sdlog = 1))
dat <- data.frame(samples = (x))

p1 <- ggplot(dat, aes(x = samples, fill = samples), binwidth = 0.2) + geom_histogram(colour = "purple") +
  labs(title = "Histogram of generated samples", x = "samples", y = "Frequency") + theme_minimal()
p1
plot(density(x))
drmodel_lnorm <- DirichletProcessWeibull(x,g0Priors = c(2,2,4), alphaPriors = c(2, 4),
                                         mhStepSize = c(1, 1),
                                         hyperPriorParameters = c(6, 2, 1, 0.5),
                                         verbose=FALSE, mhDraws=250)
drmodel_lnorm <- Fit(drmodel_lnorm, 200)
plot(drmodel_lnorm)

### credible interval
pd <- PosteriorFrame(drmodel_lnorm, seq(0.05, 40, by = 0.15), 1000, ci_size =  0.1)
trueFrame <- data.frame(x=seq(0.05, 40, by = 0.30),
                         y=0.5*dlnorm(seq(0.05, 40, by = 0.30), log(1), 1)+
                           0.5*dlnorm(seq(0.05, 40, by = 0.30), log(1), 1))


ggplot(data = pd, aes(x=x))+
  geom_ribbon(aes(ymin=X5., ymax=X95.), alpha=0.25, fill="red") + 
  geom_line(aes(y=Mean), colour="red", alpha = 0.75) +
  geom_line(data=trueFrame, aes(x=x, y=y), alpha = 0.75)+
  labs(title = "Original vs fitted model")
  
