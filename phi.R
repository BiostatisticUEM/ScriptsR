rm(list=ls())
#### Instalando pacote para usar a função phi
#install.packages('psych',repos='http://cran-r.c3sl.ufpr.br/')

### carregando o pacote
library(psych)

### Dados hipotéticos da professora Isolde
tabela1 <- matrix(c(15,3,7,5),nrow=2,byrow=T,dimnames=list(c("Lesão leve","Lesão grave"),c("Usa capac.","Não usa capac.")))
tabela1

### pacote auxiliar
library(vcd)
mosaic(tabela1, shade=T)
### teste qui-quadrado
Q1<-chisq.test(tabela1,sim=200)
Q1
Q1$observed ### frequência observada
Q1$expected ### frequência esperada
Q1$stat     ### estatística de teste

##fisher.test(tabela)
### Medida de associação phi

phi(tabela1)
                                        #phi = sqrt(X^2 / nobs)


### função para calcular medida de associação Cramér's V

                                        #CV = sqrt(X^2 / [n * (min(ncols, nrows) – 1)])

install.packages('lsr',repos='http:/cran-r/c3sl.ufpr.br/')
require(lsr)
cramersV(tabela1,sim=200)

### outra forma de calcular o coef. de Cramer V
dados <- read.csv("http://www.math.smith.edu/r/data/help.csv")
names(dados)
### Teste qui-quadrado
chisq.test(dados$female, dados$homeless)
cv.test <- function(x,y) {
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
    (length(x) * (min(length(unique(x)),length(unique(y))) - 1)))
  print.noquote("Cramér V / Phi:")
  return(as.numeric(CV))
}

cv.test(dados$female, dados$homeless)
### Coeficiente de contingência
                        ### CC = sqrt(X^2/X^2 +n)

n1 <- sum(tabela1)
CC1 <- sqrt(Q1$stat/(Q1$stat+n1))
CC1
# install.packages('DescTools')
require(DescTools)

ContCoef(tabela1)
Phi(tabela1)
CramerV(tabela1)

########################################################### EXEMPLO COM DADOS REAIS #######################################################

### Duas espécies de parasitas:
#Giardia negativa
#Giardia positiva

#Cryptosporidium negativo
#Cryptosporidium positivo


# As presença das espécies podem estar associadas? 

### Dados reais
tabela2 <- matrix(c(250,15,34,14),nrow=2,byrow=T,dimnames=list(c("CN","CP"),c("GN","GP")))

 ## Uma alternativa então é calcular o p-valor através de simulação ou o teste exato de Fisher. Note que quando simulamos o p-valor não é necessário usar a correção de continuidade de Yates.
Q2<-chisq.test(tabela2,sim=200)
Q2
Q2$observed ### frequência observada
Q2$expected ### frequência esperada
Q2$stat     ### estatística de teste
##Há evidências de se rejeitar H0

# Comandos para obtenção da diferença entre proporções e seu IC(95%)
## Calculando as proporções entre Cryptosporidium negativo e Cryptosporidium positivo
p11<-(tabela2[1,1]/(sum(tabela2[1,])))
p11
p21<-(tabela2[2,1]/(sum(tabela2[2,])))
d<-p11-p21 # diferença entre as proporções
d
vd<-((p11*(1-p11))/(sum(tabela2[1,])-1)) + ((p21*(1-p21))/(sum(tabela2[2,])-1)) ## Estimativa para a variância
dvd<-sqrt(vd) ## raíz quadrada da variância
z<-qnorm(0.975) #percentil da Normal padrão
li<- d - (z*dvd) # Limite inferior
ls<- d + (z*dvd) # Limite superior
cbind(d,li,ls) # Intervalo de Confiança de 95%. Como o valor zero não está contido no IC a diferença é significativa ao nível de 95% de confiança.

##Razão de Chances ou Odds Ratio (OR) e IC95%(OR)
OR<-(tabela2[1,1]*tabela2[2,2])/(tabela2[1,2]*tabela2[2,1]) ## Calculando a odds ratio (n11*n22/n12*n21)
## Quando OR=1 indica chances iguais. Se for OR>1, o grupo 1 apresenta maior chance que o grupo 2.
## Para o cálculo do IC para a OR, usamos o logaritmo da OR na base e.
vf<-(1/tabela2[1,1])+(1/tabela2[1,2])+(1/tabela2[2,1]+(1/tabela2[2,2])) ##Estimativa para variância
dpf<-sqrt(vf) ## raíz quadrada da variância
z<-qnorm(0.975) #Percentil da Normal padrão
liOR<-exp(log(OR)-z*dpf) #Limite inferior
lsOR<-exp(log(OR)+z*dpf) # Limite Superior
cbind(OR,liOR,lsOR)
## A chance de não haver Cryptosporidium e Giardia é 6,8 vezes maior que a presença podendo variar entre 3 e 15,4 vezes ao nível de confiança de 95%.



### Mosaico
mosaic(tabela2, shade=T)
### Teste phi
phi(tabela2)
### Teste de Cramer
cramersV(tabela2,sim=200)



https://en.wikipedia.org/wiki/Contingency_table#Measures_of_association
https://en.wikipedia.org/wiki/Phi_coefficient.



### Coeficiente de contingência
                        ### CC = sqrt(X^2/X^2 +n)

n2 <- sum(tabela2)
CC2 <- sqrt(Q2$stat/(Q2$stat+n2))
CC2
# install.packages('DescTools')
require(DescTools)
Phi(tabela2)
ContCoef(tabela2)
CramerV(tabela2)
