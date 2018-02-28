install.packages("tidyverse")


library(tidyverse)
genreg <- function(n){
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  eps <- rnorm(n)
  y <- 5-x1+2*x2+eps
  tibble(x1=x1, x2=x2, y=y)
}
dat <- genreg(100000)

dat <- mutate(dat,
        yhat=5,
        yhat1=5-x1,
        yhat2=5+2*x2,
        yhat12 = 5-x1+2*x2)

mse <- mean((dat$yhat-dat$y)^2) 
mse1 <- mean((dat$yhat1-dat$y)^2) 
mse2 <- mean((dat$yhat2-dat$y)^2) 
mse12 <- mean((dat$yhat12-dat$y)^2) 


mse
mse1
mse2
mse12

#Oracle classfication
gencla <- function(n) {
  x <- rnorm(n) 
  pB <- 0.8/(1+exp(-x))
  y <- map_chr(pB, function(x) 
    sample(LETTERS[1:3], size=1, replace=TRUE,
           prob=c(0.2, x, 1-x-0.2)))
  tibble(x=x, y=y)
}

p1 <- 0.8/(1+exp(-1))


#x=1

pa=0.2
pb <- 0.8/(1+exp(-1))
pc <- 1-pa-pb

pb
pc

#x=-2
pa2=0.2
pb2 <- 0.8/(1+exp(2))
pc2 <- 1-pa2-pb2

pb2
pc2

datc <- gencla(1000)
datc

datc <- mutate(datc,
               yhat = sapply(x, function(x_)
                 if (x_<0)"C" else "B"))

error<- mean(datc$yhat == datc$y)

