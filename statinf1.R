##question 1

##The exponential distribution can be simulated in R with rexp(n, lambda) 
##where lambda is the rate parameter. The mean of exponential distribution 
##is 1/lambda and the standard deviation is also also 1/lambda. 
##Set lambda = 0.2 for all of the simulations. 

##In this simulation, you will investigate the distribution of averages 
##of 40 exponential(0.2)s.

##Note that you will need to do a thousand or so simulated averages
##of 40 exponentials.

##Illustrate via simulation and associated explanatory text the 
##properties of the distribution of the mean of 40 exponential(0.2)s. 
##You should 
## 1. Show where the distribution is centered at and compare it to 
##the theoretical center of the distribution.
##2. Show how variable it is and compare it to the theoretical variance of the distribution.
##3. Show that the distribution is approximately normal. 
##4. Evaluate the coverage of the confidence interval for 1/lambda: X????1.96Sn???. 
##(This only needs to be done for the specific value of lambda).



set.seed(3)
lambda<-0.2
nsims<-1000
n<-40
## see the number of simulation in a matrix form

sim<-matrix(rexp(nsims*n, rate=lambda), nsims, n)

row_means<-rowMeans(sim)

##draw a histogram

hist(row_means, breaks=n, prob=TRUE, main="Distribution of averages of samples, drawn from exponential distribution with lambda=0.2", xlab="")

## density of the averages sample

lines(density(row_means))
##theoretical center of distribution

abline(v=1/lambda, col='red')

##theoretical density of averages of samples

xfit<-seq(min(row_means), max(row_means), length=100)

yfit<-dnorm(xfit, mean=1/lambda, sd=((1/lambda)/sqrt(n)))

hist(row_means, breaks=n, prob=TRUE, col="orange", xlab="Means", main="Density of Means", ylab="Density")

lines(xfit, yfit, pch=22, col="red", lty=2)


##add legend
legend('topright', legend=c("simulation","theoretical"), lty=1:2, col("black", "red"))

##question 2
qqnorm(row_means)

qqline(row_means)

##-----------------------------




lambda_vals<-seq(4, 6, by=0.01)
coverage<-sapply(lambda_vals, function(l){
        mu_hats<-rowMeans(matrix(rexp(n*nsims, rate=0.2), nsims, n))
        l1<-mu_hats - qnorm(0.975) *sqrt(1/lambda ** 2/n)
        u1<-mu_hats + qnorm(0.975) *sqrt(1/lambda ** 2/n)
        mean(l1<l & u1>l)
})

library(ggplot2)
qplot(lambda_vals, coverage) + geom_hline(yintercept=0.95)

