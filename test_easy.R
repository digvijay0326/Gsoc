library(dirichletprocess)
library(ggplot2)
data("faithful")
df <- faithful

### histogram of waiting
hist_density <- ggplot(df, aes(x = waiting)) + geom_histogram() + labs(title = "Frequency of waiting")
hist_density

### normalising the data 
transformed_waiting <- (df$waiting - mean(df$waiting))/sd(df$waiting)
plot(hist(transformed_waiting))
### fitting mixture model of gaussian
md <- DirichletProcessGaussian(transformed_waiting)
md <- Fit(md, 1000)
plot(md)
plot(md, data_method = "hist")
### predicting Posterior density by generating samples 
sample_x <- seq(-3,3, by = 0.01)
posterior_density <- data.frame(replicate(100, PosteriorFunction(dp)(sample_x)))
posterior_density <- data.frame(x = sample_x, y = rowMeans(posterior_density))
### predictive plot + original histogram
ggplot(data.frame(transformed_waiting), aes(x = transformed_waiting, y = ..density..), binwidth =  0.25) +
  geom_histogram() + geom_line(data = posterior_density, aes(x = x, y = y), colour = 'red')

### iris dataset

data("iris")
df <- data.frame(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width)
iris_transformed <- scale(df)
multi_normal <- DirichletProcessMvnormal(iris_transformed)
multi_normal <- Fit(multi_normal, 500)
pairs(df, col = multi_normal$clusterLabels)
