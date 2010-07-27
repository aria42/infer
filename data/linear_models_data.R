library(glmnet)
library(plyr)

lambda <- 0.0315

set.seed(1000)

x <- scale(matrix(rnorm(100*20), 100, 20))
y <- rnorm(100)
y <- y - mean(y)

f <- glmnet(x,y, lambda=c(lambda))

coef(f, lambda)
wr<- write.table(x, "lars_x.csv", col.names=F, row.names=F, sep=",")
write.table(y, "lars_y.csv", col.names=F, row.names=F, sep=",")
write.table(as.matrix(coef(f, lambda)), "lars_betas.csv", col.names=F, row.names=F, sep=",")
write.table(lambda, "lars_lambda.csv", col.names=F, row.names=F, sep=",")