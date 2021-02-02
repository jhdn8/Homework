#homework
#q1
pvs <- numeric(1000)
set.seed(1234)
for(i in 1:1000){
  y1 <- rnorm(n = 50, mean = 100, sd = 15)
  y2 <- rnorm(n = 50, mean = 100, sd = 25)
  treat <- rep(c("control", "treatement"), each = 50)
  math <- c(y1, y2)
  dat <- data.frame(treat, math)
  res <- t.test(math ~ treat, data = dat)
  pvs[i] <- res$p.value
}
mean(pvs <= .05)

#q2
##a
library(gendata)
cors <- numeric(1000)
set.seed(1234)
for(i in 1:1000){
  d1 <- genmvnorm(cor = .3, k = 2, n = 50)
  res <- cor.test(d1$X1, d1$X2)
  cors[i] <- res$estimate 
}
mean(cors <= .05)

##b
d2 <- c(d1$X1, d1$X2)
hist(cors, xlab = 'Correlation coefficient', breaks = 20)

##c
mean(cors[1000])

##d
install.packages("pwr")
library(pwr)
pwr.r.test(n = 100, r = .15)

##e
pwr.r.test(r = .15, power = .80)
