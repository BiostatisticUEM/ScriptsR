###### Geração de variáveis aleatórias - MONTE CARLO ######

# exemplo1 - gerar duas amostras binomiais(n=100,p=0.45)
rm(list=ls())
n <- 100
p <- 0.45
size <- 1
valor <- NULL

set.seed(123456789)
for (i in 1:n){
    x1 <- rbinom(10,size,p)
    x2 <- rbinom(10,size,p)
    valor <- c(valor,chisq.test(x1,x2,sim=200)$p.value)
}
valor
class(valor)
plot(valor)
quantile(valor,probs=seq(0,1,.025))

#pvs <- NULL
#pvs <- sapply( 1:n
#            , function( i ){
#                x1 <- rbinom(n,size,p)
#                x2 <- rbinom(n,size,p)
#                valor <- c(valor,chisq.test(x1,x2)$p.value)
#            } )

######### BOOTSTRAP #########
rm(list=ls())
#y = c(4.313, 4.513, 5.489, 4.265, 3.641, 5.106, 8.006, 5.087)
#yres <- sample(mean(y),2500,replace=T)
#head(yres)
#summary(yres)
#length(yres)
#hist(yres)
set.seed(1999)
y <- rnorm(1000)
ystar <- NULL
B <- NULL
nBoot <- 20000
#número de amostras bootstrap 
B <- array(0,dim=c(nBoot, 1)) # array p/ guardar o bootstrap
for(i in 1:nBoot){
#laço p/ bootstrap
#   ystar<- c(ystar,sample(y,size=10,replace=T))
    ystar <- sample(y,size=5,replace=F)
    B[i] <- mean(ystar)
    }

#inspecionando os resultados
head(ystar)
head(y)
head(B)
length(ystar)
length(B)
hist(B,breaks='Scott',prob=T)
lines(density(B),col='darkred')
lines(sort(B),dnorm(sort(B), mean(B),sd(B)),type='l',lty=3)

######### Solução do Omar #########
set.seed(1)
hist(rexp(1000,0.2))
#
msample = NULL
for (i in 1 : 1000) msample = c(msample, mean(rexp(40,0.2)))
hist(msample,breaks=15,freq=F,ylim=c(0,.65),main="",xlab='')
curve(dnorm(x,5,25/40),col=2,lty=1,lwd=2,type='l',add=TRUE)
lines(density(msample), col=1,lty=1,lwd=2,type='l')
legend(6,0.6,c("Normal","Actual"),cex=1.5,col=c(2,1),lty=1,bty='n',lwd=2)

mean(msample)
var.amostral<-var(msample);var.amostral
sd(msample)

vars = NULL
for (i in 1 : 1000) vars = c(vars, var(rexp(40,0.2)))
mean(vars)
mean(vars)/40
