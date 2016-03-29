

################################################# Exercício 6 ################################################################################

### dados
dados <- cbind(0.1, 0.25, 0.5, 4, 12, 12, 24, 24, 31, 36, 42, 55, 96)
colnames(dados) <- rep('',length(dados))
rownames(dados) <- 'dados'
dados
### calculando a média
media <- mean(dados)
### calculando a mediana
mediana <- median(dados)
table(d)
### função para identificar a moda
paste("moda")
moda <- function(x)
{
    for(i in 1:length(x))
        if(x[i]==12 | x[i]==24)
        {
            t <-c(x[i]) 
            print(t)
        }
}
### guardando o resultado da moda no objeto 'moda'
moda <- moda(dados)
### Calculando a amplitue
amplitude <- max(dados)-min(dados)
### guardando os resultados no objeto 'medidas'
medidas <- rbind(media, mediana, amplitude)
colnames(medidas) <- " "
### arredondadndo as medidas para duas casas decimais 
round(medidas,2)

### laço para calcular somatório de x_i - média
soma <- 0
for(i in 1:length(dados))
    soma <- soma + sum(dados[i]-mean(dados))
    round(soma,1)

################################################# Exercício 7 ################################################################################

### dados
calcio <- c(2.92, 3.84, 2.37, 2.99, 2.67, 3.17, 3.74, 3.44)
albumina <- c(43, 42, 42, 40, 42, 38, 34, 42)
dados <- data.frame(calcio,albumina)
dados

### calculando medidas para variável cálcio
media1 <- mean(dados$calcio)
mediana1 <- median(dados$calcio)
desvio.padrão1 <- round(sd(dados$calcio),2)
amplitude1 <- max(dados$calcio)- min(dados$calcio)
resumo1 <- cbind(media1, mediana1, desvio.padrão1, amplitude1)
resumo1

### calculando medidas para variável albumina
media2 <- mean(dados$albumina)
mediana2 <- median(dados$albumina)
desvio.padrão2 <- round(sd(dados$albumina),2)
amplitude2 <- max(dados$albumina)- min(dados$albumina)
resumo2 <- cbind(media2, mediana2, desvio.padrão2, amplitude2)
resumo2

################################################# Exercício 8 ################################################################################
Bulimica <- c(15.9, 16, 16.5, 17, 17.6, 18.1, 18.4, 18.9, 18.9, 18.9, 19.6, 21.5, 21.6, 22.9, 23.6, 24.1, 24.5, 25.1, 25.2, 25.6, 28,
              28.7, 29.2, 30.9, 30.6)
Saudavel <- c(20.7, 22.4, 23.1, 23.8, 24.5, 25.3, 25.7, 30.6, 33.2, 33.7, 36.6, 37.1,37.4, 40.8)
Bulimica
Saudavel

### calculando as medidas para as duas variáveis
mediana.bulimica <- median(Bulimica)
mediana.saudavel <- median(Saudavel)
cbind(mediana.bulimica, mediana.saudavel)
sumario1 <- summary(Bulimica)
sumario2 <- summary(Saudavel)
Intervalo.Bulimica <- sumario1[5]-sumario1[2]
Intervalo.Saudavel <- sumario2[5]-sumario2[2]
res <- data.frame(Intervalo.Bulimica,Intervalo.Saudavel) 
rownames(res) <- NULL
res


################################################# Exercício 10 ###############################################################################
dados <- data.frame(Nivel.de.cotinina=c('0-13','14-49','50-99','100-149','150-199','200-249','250-299','300+'),
                            Fumantes=c(78,133,142,206,197,220,151,412),
                            Nao.fumantes=c(3300,72,23,15,7,8,9,11))
dados

### calculando ponto médio das classes
ponto.medio <- c(13/2,14+(49-14)/2,50+(99-50)/2,100+(149-100)/2,150+(199-150)/2,200+(249-200)/2,
                 250+(299-250)/2,340)
### calculando as outras medidas
media.f <-sum(dados$Fumantes*ponto.medio)/sum(dados$Fumantes)
media.nf <-sum(dados$Nao.fumantes*ponto.medio)/sum(dados$Nao.fumantes)
desvio.f  <- sum((ponto.medio-media.f)^2*dados$Fumantes)/(sum(dados$Fumantes)-1)
desvio.nf <- sum((ponto.medio-media.nf)^2*dados$Nao.fumantes)/(sum(dados$Nao.fumantes)-1)
res <- cbind(media.f,media.nf,sqrt(desvio.f),sqrt(desvio.nf))
colnames(res) <- c('Media Fumantes','Média Não fumantes','Desvio Padrão Fumantes','Desvio Padrão Não Fumantes')
res

median(dados$Fumantes)
## A classe mediana dos fumantes é: '150-199'
median(dados$Nao.fumantes)
## A classe mediana dos não fumantes é: '0-13'

################################################# Exercício 11 ################################################################################
zinco <- c(50,51,53,55,56,58,rep(60,4),rep(61,4),rep(62,4),rep(63,3),rep(64,5),rep(65,3),rep(66,3),rep(67,5),rep(68,4),69,
          rep(70,9),rep(71,10),rep(72,13),rep(73,9),rep(74,12),rep(75,15),rep(76,13),rep(77,10),rep(78,12),rep(79,6),rep(80,11),
          rep(81,10),rep(82,13),rep(83,11),rep(84,14),rep(85,11),rep(86,13),rep(87,10),rep(88,14),rep(89,9),rep(90,8),
          rep(91,10),rep(92,8),rep(93,8),rep(94,11),rep(95,11),rep(96,10),rep(97,9),rep(98,9),rep(99,6),rep(100,5),rep(101,7),
          rep(102,8),rep(103,6),rep(104,7),rep(105,8),rep(106,6),rep(107,7),rep(108,7),rep(109,2),rep(110,2),rep(111,4),rep(112,3),
          rep(113,3),rep(114,4),rep(115,3),rep(116,4),rep(117,3),rep(118,3),rep(119,2),121,123,124,125,128,131,135,142,147,151,153)
zinco
M <- summary(zinco)
medidas <- data.frame(Media=M[4],Mediana=M[3],Desvio.padrao=round(sd(zinco),2),
                    Amplitude=M[6]-M[1],Intervalo.interquartil=M[5]-M[2])
medidas

### Desigualdade Chebychev
I1 <- c(medidas$Media-2*medidas$Desvio.padrao, medidas$Media+2*medidas$Desvio.padrao)
I2 <- c(medidas$Media-3*medidas$Desvio.padrao, medidas$Media+3*medidas$Desvio.padrao)
### Este intervalo contém pelo menos 75% dos dados
I1
## Este intervalo contém pelo menos 88,9% dos dados
I2



## Pelo histograma vemos que os dados são simétricos então poderíamos usar a regra empírica.
hist(zinco,prob=T,axes=F,main='Histograma dos níveis séricos de zinco',cex.main=.85,
     ylab='',,xlab='')
axis(1,at=seq(M[1],160),cex.axis=.65)
rug(zinco)
lines(density(zinco),col='darkgreen')


################################################# Exercício 12 ################################################################################

### dados
lowbwt <- c(20,7,9,19,7,8,6,6,50,6,12,8,11,6,21,13,6,7,9,7,10,16,9,6,14,8,6,10,11,7,11,4,16,7,15,4,5,17,6,14,21,20,15,9,8,
   9,19,33,14,9,15,4,7,5,11,6,7,7,18,11,10,10,10,20,17,9,11,9,12,16,20,10,12,4,15,15,16,4,6,10,10,23,25,8,11,5,16,7,11
   ,15,16,7,17,11,17,16,25,15,5,5,13,14,20,10,8,8,8,9,17,15,13,14)
med <- data.frame(Media=mean(lowbwt),Mediana=median(lowbwt),Media.aparada=mean(lowbwt,trim=0.05))
med


### Histograma
hist(lowbwt,prob=T,axes=F,main='Histograma do baixo peso dos bebês ao nascer',cex.main=.85,
     ylab='',,xlab='')
axis(1,at=seq(min(lowbwt),max(lowbwt)),cex.axis=.65)
rug(lowbwt)
lines(density(lowbwt),col='darkgreen')

### A média aparada é uma melhor medida, 
### pois vemos no histograma que há valores extremos que influenciam a média.


################################################# Exercício 13 ################################################################################

### dados
nic <- c(0.3,1.2,1.3,1,1.4,1.2,0.1,0.9,1.3,1.1,1.2,1,0.9,1.3,1.2,1.3,0.89,1.2,1.3,0.4,1.1,0.4,.2,1.1,1.3,1,1.3,1.3
         ,1.4,1.1,1.2,1.2,1,.5,0.09)
Medidas <- data.frame(Media=round(mean(nic),2),Mediana=median(nic))
Medidas

### Notamos uma assimetria a esquerda com um grupo de baixos valores discrepantes
### e um intervalo de valores sem nenhuma observação.
hist(nic,prob=T,axes=F,main='Histograma da concentração de nicotina',cex.main=.85,
     ylab='',,xlab='',breaks='Sturges')
axis(1,at=seq(min(nic),max(nic),0.1),cex.axis=.65)
rug(nic)
lines(density(nic),col='darkgreen')
### A mediana serria uma melhor medida devido a assimetria dos dados
### e por ser uma medida menos sensível a valores discrepantes,
### ou seja, é mais robusta que a média.

 

################################################# Exercício 14 ################################################################################

### dados
press <- c('115-124','125-134','135-144','145-154','155-164','165-174','175-184')
paci <- c(4,5,5,7,5,4,5)
dados <- data.frame(press,paci)
colnames(dados) <- c('PressãoSanguínea','NúmeroPacientes' )
dados

### Ponto médio das classes
ponto.medio <- c(115+(124-115)/2,125+(134-125)/2,135+(144-135)/2,145+(154-145)/2,155+(164-155)/2,165+(174-165)/2,
                 175+(184-175)/2)

### calculando as outras medidas
media.pac <-sum(dados$NúmeroPacientes*ponto.medio)/sum(dados$NúmeroPacientes)
desvio.pac  <- sum((ponto.medio-media.pac)^2*dados$NúmeroPacientes)/(sum(dados$NúmeroPacientes)-1)

res <- cbind(media.pac,sqrt(desvio.pac))
colnames(res) <- c('Media Agrupada','Desvio Padrão Agrupado')
res

### dados não-agrupados
sbp <- c(170,128,150,148,160,154,175,140,140,120,172,178,122,158,132,130,175,140,150,180,160,122,170,150,168,134,160,
         154,125,140,154,120,162,140,178)
res2 <- cbind('Media Não-agrupada'=mean(sbp),'Desvio padrão Não-agrupado'=sd(sbp))
res2

### As medidas agrupadas e não agrupadas diferem
### Isso ocorre pois quando criamos classes de valores, perdemos informação




################################################# Exercício 15 ################################################################################

### dados
sbp <- c(43, 51, 42,39,48,31,31,40,57,64,46,47,63,56,49,87,46,66,42,52,51,47,54,64,37,36,45,39,29,61,53,64,35,34,
         62,59,36,47,45,62,75,44,39,48,43,19,63,42,44,25,26,27,35,40,44,66,59,24,40,49,53,45,50,64,48,48,58,
         67,40,48,36,44,53,45,54,44,42,50,48,29,30,36,44,46,51,51,43,48,52,43,42,48,49,62,45,51,52,47,40,50)

sex <- c(1,1,0,0,0,1,1,rep(0,7),1,1,rep(0,4),1,0,1,1,0,1,0,1,0,0,1,0,0,1,0,0,1,0,1,0,rep(1,3),rep(0,3),
         1,1,rep(0,3),rep(1,3),0,1,rep(0,4),1,0,1,1,rep(0,6),1,1,0,0,rep(1,3),rep(0,7),rep(1,8),0,1,1,
         0,1,1,0,0)
### transformado a variável sexo em fator
sex <- as.factor(sex)

### Boxplot
boxplot(sbp~sex,names=c('Feminino','Masculino'),cex.axis=0.75,cex.main=.85,
        main='Boxplot da pressão sanguínea sistólica \n bebês nascidos com baixo peso')

### calculando medidas para ambos os sexos
media <- tapply(sbp,sex,mean)
desv.pd <- tapply(sbp,sex,sd)

### inserindo os valores no gráfico
text(1,49,'Média=46,46',cex=.75,col='darkblue')
text(2,48,'Média=47,86',cex=.75,col='darkblue')
text(1,43,'S=11,14',cex=.75,col='darkblue')
text(2,43,'S=11,81',cex=.75,col='darkblue')
text(1.5,22,'A variabilidade entre meninos e meninas, não difere muito.',cex=.86,col='red')

### Calculando coef. de variação para ambos os sexos
cvf <- desv.pd[1]/media[1]
cvm <- desv.pd[2]/media[2]
cv.s <- cbind(cvf,cvm)
colnames(cv.s) <- c('Coef. Variação Feminino','Coef. Variação Masculino')
rownames(cv.s) <- ''
cv.s
