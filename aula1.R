==========================================================================================================================================
=                       TIPOS DE OJETOS                                                                                                  =
==========================================================================================================================================

========= VETORES =====================
vetor_numerico <- c(1,2,3,4,5,6,7,8,9,10)
vetor_numerico[c(3,5)]

vetor_caracter<-c('A','a','B','b','C','c','D','d')
vetor_caracter

vetor_misto<-c('S',2,'D',5,'w',11)
vetor_misto
mode(vetor_misto)
class(vetor_misto)

is.character(vetor_misto)
is.character(vetor_caracter)
is.character(vetor_numerico)
is.numeric(vetor_numerico)

vetor_misto_num<-as.numeric(vetor_misto)
vetor_misto_num

vetor_numerico[3]
vetor_numerico[1:3]
vetor_numerico[c(2,5)]
vetor_numerico<6
vetor_numerico[vetor_numerico<6]

vetor_caracter=='A'
vetor_caracter[vetor_caracter==c('a','b','c','d')]

length(vetor_numerico)

========= Gerando sequências
seq(1,10)
seq(from=1,to=10,by=4)

========= Gerando Repetições
rep(1,10)
rep('R',5)
rep(1:10,5)
rep(1:10,each=5)
rep(c(1,2,3,4,5),3)
rep(c(1,2,3,4,5),c(3))
rep(seq(1,5),seq(1,5))
rep(seq(1,10,2),2)

========= MATRIZES =====================
A<-matrix(c(1,2,3,4,1,4),nrow=2)
A
B<-array(c(1,2,3,4),dim=c(4,2))
B
is.matrix(A)
is.matrix(B)
is.data.frame(A)
m1<-matrix(1:12, ncol=3);m1
m2<-matrix(1:12, ncol=3, byrow=T);m2



## Outra forma de criar matrizes
m <- cbind(1, 1:7) # O '1' primeira coluna
m

m <- cbind(m, 8:14)[, c(1, 3, 2)] # insere coluna
m
cbind(0, rbind(1, 1:3))
cbind(I = 0, X = rbind(a = 1, b = 1:3,c=4:6))  
rbind(1,2,3,4)
rbind(2:5)
rbind(2,1,2,1:4)

### Substituindo valores faltantes
dados <- data.frame(parcela=rep(1:3, each=3), y=rnorm(9))
dados$substituir <- c(1,2,3,4,5,NA,7, NA,9)
mean(dados$substituir)
mean(dados$substituir,na.rm=T)
dados[is.na(dados)] <- mean(dados$substituir,na.rm=T)

dados[is.na(dados)] <- 0
dados

### Selecionando e Substituindo valores em uma matriz
m
m[1,2]
m[1,2]<-1000
m[1,2]
m

dados[,2]>1
dados[dados[,2]>1,2]
dados[dados[,2]>1,2] <- 1
dados




dim(m)
dimnames(m) <- list(c('L1', 'L2', 'L3', 'L4','L5','L6','L7'), c('C1', 'C2', 'C3'))
m

margin.table(m, margin = 1)
apply(m,1,sum)
rowSums(m)
colSums(m)

## Operação com Matrizes
m2 <- matrix(1:21, ncol=3,byrow=T)
m2
soma <- m+m2
soma
multp <- m*m2
multp
multp.v <- m%*%m2
## Transposta de uma matriz simétrica
simetrica <- matrix(c(3,1,2,1,4,3,2,3,5),ncol=3,byrow=T)
simetrica
t(simetrica)
## Exemplos para calcular Matriz Inversa
MA <- matrix(c(1,0,0,2,1,0,3,4,1),ncol=3)
MA
I <- solve(MA) ### comando solve inverte matriz
I
### Propriedade me Matriz - A inversa da inversa é ela mesma
solve(I)

c <- rep(1, 1000)

### x ~ N(1,2) - distribuição Normal com média=1 e variância=4
set.seed(1)
x <-rnorm(1000, 1,2) 

### w ~ N(1,2) - distribuição Normal com média=1 e variância=4
set.seed(2)
w <- rnorm(1000, 1, 2)

X = cbind(c,x,w)
head(X)#### mostra primeiras seis linhas do objeto - 6 é default
tail(X)#### mostra últimas seis linhas do objeto - 6 é default
# Invertendo a Matriz X'X
a <-solve(t(X)%*%X)
a


### Brincando com os dados da Fernanda
## Carregando os Dados
rm(list=ls())
age <- rep(c('5anos','8anos','15anos','30anos','50anos'),each=3);age
qtidade <- c(487,41,733,541,7750,217,226,355,151,194,487,1417,286,621,480)
AREA <- rep(c('area1','area2','area3'),5)
AREA
d <- data.frame(age,qtidade,AREA)
is.data.frame(d)
names(d)
summary(age)
summary(d$age)
factor(d$age)
boxplot(d$qtidade ~ d$age)
d$age <- factor(d$age, levels=c('5anos','8anos','15anos','30anos','50anos'))
levels(d$age) <- c('5 anos','8 anos','15 anos','30 anos','50 anos')

boxplot(d$qtidade ~ d$age)
levels(d$age)
levels(d$age) <- c('Pastagem', 'Pastagem', 'Ambos','Desmat.','Desmat.')

boxplot(d$qtidade ~ d$age)


set.seed(12345)
xx <- rnorm(100)
soma=0
media <- function(x)
{

    for(i in 1:length(x))
        soma=soma+x[i]

    media = soma/length(x)
    return(media)
}
media(xx)
mean(xx)
