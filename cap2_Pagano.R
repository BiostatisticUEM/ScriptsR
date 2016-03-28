  
################################ Exercício 8 ####################################################################
#################################################################################################################
## Limpando área de trabalho do R
rm(list=ls())

## Entrando com os dados
Nation <- c('Australia',' Austria','Belgium','Britain','Canada','Denmark','Finland',
            'France','Germany','Greece','Iceland','Ireland','Italy','Japan','Luxembourg',
            'Netherlands','New Zeland','Norway','Portugal','Spain','Sweden','Swizerland'
            ,'United States')
Exp <- c(1032,1093,980,836,1683,912,1067,1274,1232,371,1353,658,1050,1035,1193,1135,820,
         1234,464,644,1361,1376,2354)
## Criando um data frame com os dados
d <- data.frame(Nation,Exp)


## Ordenando de acordo com o gasto
rank <- d[order(d$Exp,decreasing=TRUE),]

## histograma com o gasto per capita com saúde.

hist(d$Exp,prob=T,axes=F,xlab='Gastos com saúde per capita',ylab='',breaks='FD',main='',cex.lab=.65)
axis(1, at=seq(0,2500,100), cex.axis=.7)
title('Histograma dos gastos com saúde per capita \n das nações da OECD',cex.main=.85, font.main=4)
## Indica onde os dados estão
rug(d$Exp)
## Estima a curva de densidade dos dados (distribuição empírica)
lines(density(d$Exp),col='darkred')    

 

############################# Exercício 10 #############################################################################
########################################################################################################################
casos <- c(122,250,455,848,1412,2811,3098)
barplot(casos,names.arg=c('1983','1984','1985','1986','1987','1988','1989'),las=2,col = heat.colors(11))

## Outra forma é usar o comando 'barchart' do pacote lattice barchart::lattice
require(lattice)
ano <- c('1983','1984','1985','1986','1987','1988','1989')
barchart(casos~ano,box.ratio=8,alpha=2,col='grey60',ylab='Número de casos',border='white',
         main='Casos pediátricos de AIDS nos EUA \n entre 1983-1989', 
         scales = list(x = list(rot = 45)))



############################# Exercício 11 #############################################################################
########################################################################################################################

## Carregando os dados
year <- c('1976','1977','1978','1979','1980','1981','1982','1983','1984','1985','1986','1987','1988','1989',
          '1990','1991','1992','1993','1994')
exec <- c(0,1,0,2,0,1,2,5,21,18,18,25,11,16,23,14,31,38,28)
barchart(exec~year,box.ratio=8,alpha=2,col='grey60',ylab='Número de casos',border='white',
         main='Execuções nos EUA \n entre 1976-1994', 
         scales = list(x = list(rot = 45)))



############################# Exercício 12 #############################################################################
########################################################################################################################
Grupo.de.Idade <- c('65-74','75-84','85-94','95')
Homens.Brancos<- c(36473,62513,40975,4088)
Mulheres.Brancas <- c(103105,233047,189459,18247)
Homens.Negros <- c(2295,2902,1659,208)
Mulheres.Negras <- c(3425,6819,5968,934)
## Montando data frame
M <- data.frame(Grupo.de.Idade,Homens.Brancos,Mulheres.Brancas,Homens.Negros,Mulheres.Negras)
## Nomeando as colunas
colnames(M) <- c('Grupo de Idade','Homens Brancos','Mulheres Brancas','Homens Negros','Mulheres Negras')
M


## Criando data frame auxiliar
dados1 <- data.frame(
  Raça = c(rep("Homens.Brancos",4),rep('Mulheres.Brancas',4) ,rep('Homens.Negros',4),rep('Mulheres.Negras',4)),
  Idade = rep(c('65-74','75-84','85-94','95'),4),
  Altas = c(36473,62513,40975,40881,103105,233047,189459,5968,2295,2902,1659,208,3425,6819,5968,934),
  posição <- c(1:16)
)
## Carregando pacote de recusrsos gráficos 'ggplot2', se não estiver instalado faça - >install.packages='ggplot2', repos='http://cran-r.c3sl.ufpr.br/')

require(ggplot2)
ggplot(dados1,aes(x = Idade,y = Altas/100, fill = Raça)) + 
geom_bar(stat = "identity",colour='white') +   coord_flip() 

############################# Exercício 13 #############################################################################
########################################################################################################################
Nivel.cotinina <- c('0-13','14-49','50-99','100-149','150-199','200-249','250-299','300+')
Fumantes <- c(78,133,142,206,197,220,151,412)
Não.Fumantes <- c(3300,72,23,15,7,8,9,11)
## Soma dos fumantes
Nf <- sum(Fumantes)
## Soma dos não fumantes
Nnf <- sum(Não.Fumantes)
## Calculando a frequência relativa e arredondando para duas casas deciamis
fr.f <- round(100*Fumantes/Nf,2)
fr.nf <- round(100*Não.Fumantes/Nnf,2)
## Criando data frame
rel <- data.frame(Nivel.cotinina,fr.f,fr.nf)
## Nomeando as colunas
colnames(rel) <- c('Nível de cotinina','Fumantes(%)','Não-Fumantes(%)');rel


## Gerando dados auxiliares
fr.f2 <- c(0,fr.f,0)
fr.nf2 <- c(0,fr.nf,0)
x <- c(0,6.5,31.5,75,125,175,225,275,325,375)
## Plotando o poligono em linhas
plot(x,fr.f2,type='p',lwd=2,col='darkblue',ylim=c(0,100),ylab='Freq. Rel.',xlab='Nível de continina(ng/ml)',pch=19,axes=F)
axis(1, at=seq(0,400,10), cex.axis=0.75)
axis(2, at=seq(0,100,10), cex.axis=0.75)
lines(x,fr.f2,lwd=3,col='darkblue')
lines(x,fr.nf2,lwd=3,col='red')
points(x,fr.nf2,col='red',pch=19)
legend(270,90, legend = c('Fumantes','Não-Fumantes'), col = c('darkblue','red'), pch = 19,
              lty = 1,bty='n',text.col=c('darkblue','red'))
     
 
############################# Exercício 14 #############################################################################
########################################################################################################################
## Os dados
chumbo <- c('<20','20-29','30-39','40-49','50-59','60-69','70-79','>80')
G1979 <- c(11.5,12.1,13.9,15.4,16.5,12.8,8.4,9.4)
G1987 <- c(37.8,14.7,13.1,15.3,10.5,6.8,1.4,0.4)
## Distribuição acumulada
daG1979 <- cumsum(G1979)
daG1987 <- cumsum(G1987)
## Data frame
acum <- data.frame(chumbo,daG1979,daG1987)
colnames(acum) <- c('Chumbo no sangue','Dist. acumulada-1979(%) ','Dist. acumulada-1987(%)')
acum
## Gráfico ramos em folhas (optei por esse)
stem(G1979,scale=1)
stem(G1987,scale=1)

############################# Exercício 15 #############################################################################
########################################################################################################################
## criando vetor com nomes dos meses
month <- format(ISOdatetime(2000,1:12,1,0,0,0),"%b")
## outra forma de criara vetor dos meses
m <- rep(month.abb,2)
## vetor dos anos
year <- rep(c('1991','1992'),c(12,12))

number <- c(325,312,346,340,355,342,358,346,365,355,324,342,334,304,360,330,361,333,352,350,357,345,332,325)
## transformando os dados em uma série temporal
number <- ts(number)
## Plotando a séris
plot(number,xlab='',main='Número de nascidos vivos \n EUA 1991-1992',ylab='Quantidade Mensal',type='l',
     col='darkorange',fg='darkblue',axes=F,cex.main=1)
points(number,pch=19,cex=0.5,col='darkblue')
axis(1,at=seq(1,24,1),c(rep(month.abb,2)),col='darkblue',col.axis = "darkblue",cex.axis=.75,font.axis=3,hadj=0.51)
axis(2,at=seq(0,370,5),col='darkblue',col.axis = "darkblue",cex.axis=.75,font.axis=3,hadj=0.51)
box()


############################# Exercício 16 #############################################################################
########################################################################################################################
## Os dados
zinc <- c(50,51,53,55,56,58,rep(60,4),rep(61,4),rep(62,4),rep(63,3),rep(64,5),rep(65,3),rep(66,3),rep(67,5),rep(68,4),69,
          rep(70,9),rep(71,10),rep(72,13),rep(73,9),rep(74,12),rep(75,15),rep(76,13),rep(77,10),rep(78,12),rep(79,6),rep(80,11),
          rep(81,10),rep(82,13),rep(83,11),rep(84,14),rep(85,11),rep(86,13),rep(87,10),rep(88,14),rep(89,9),rep(90,8),
          rep(91,10),rep(92,8),rep(93,8),rep(94,11),rep(95,11),rep(96,10),rep(97,9),rep(98,9),rep(99,6),rep(100,5),rep(101,7),
          rep(102,8),rep(103,6),rep(104,7),rep(105,8),rep(106,6),rep(107,7),rep(108,7),rep(109,2),rep(110,2),rep(111,4),rep(112,3),
          rep(113,3),rep(114,4),rep(115,3),rep(116,4),rep(117,3),rep(118,3),rep(119,2),121,123,124,125,128,131,135,142,147,151,153)


## Histograma
hist(zinc, breaks=c(50,60,70,80,90,100,110,120,130,140,150,160),axes=F, main='Níveis séricos de zinco \n de 462 homens entre 15 e 17 anos',cex.main=.95,xlab='Nível sérico de zinco(mg/dl)',prob=T,ylab='')
axis(1,at=seq(0,160,10),cex.axis=.75,font.axis=3,hadj=0.51)
rug(zinc)
lines(density(zinc),col='darkred')
## calculando as frequências relativas a partir do histograma
h <- hist(zinc)
h$counts <- h$counts/length(zinc)
round(h$counts,3)

# adicionando no gráfico as frequências relativas
text(55,0.0027 ,c('0.02') ,col='darkred',cex=.85)
text(65,0.0099 ,c('0.09') ,col='darkred',cex=.85)
text(75,0.025  ,c('0.24') ,col='darkred',cex=.85)
text(85,0.025  ,c('0.25') ,col='darkred',cex=.85)
text(95,0.0195 ,c('0.19') ,col='darkred',cex=.85)
text(105,0.0135,c('0.13') ,col='darkred',cex=.85)
text(115,0.0069,c('0.06') ,col='darkred',cex=.85)
text(125,0.002 ,c('0.01') ,col='darkred',cex=.85)
text(135,0.001 ,c('0.004'),col='darkred',cex=.85)
text(145,0.001 ,c('0.004'),col='darkred',cex=.85)
text(155,0.001 ,c('0.004'),col='darkred',cex=.85)
 
############################# Exercício 17 #############################################################################
########################################################################################################################
lowbwt <- c(20,7,9,19,7,8,6,6,50,6,12,8,11,6,21,13,6,7,9,7,10,16,9,6,14,8,6,10,11,7,11,4,16,7,15,4,5,17,6,14,21,20,15,9,8,
            9,19,33,14,9,15,4,7,5,11,6,7,7,18,11,10,10,10,20,17,9,11,9,12,16,20,10,12,4,15,15,16,4,6,10,10,23,25,8,11,5,16,7,11
            ,15,16,7,17,11,17,16,25,15,5,5,13,14,20,10,8,8,8,9,17,15,13,14)

## Dividindo a janela gráfica em uma linha e duas colunas e definindo as margens
par(mfrow=c(1,2), mar=c(4,4,4,3))
## Boxplot
boxplot(lowbwt, ylab="", main="Boxplot da porcentagem \n
 de bêbes com baixo peso ao nascer",cex.main=.85)
hist(lowbwt, prob=T, axes=F, main="Histograma da porcentagem \n
 de bêbes com baixo peso ao nascer",cex.main=.85,xlab='',ylab='')
axis(1,at=seq(0,50,5),cex.axis=.75,font.axis=3,hadj=0.51)
rug(lowbwt)
lines(density(lowbwt),col='darkred')


############################# Exercício 18 #############################################################################
########################################################################################################################
state <- c('Alabama','Alasca','Arizona','Arkansas','Califórnia','Carolina do Norte','Carolina do Sul','Colorado','Connecticut',
           'Dakota do Norte','Dakota do Sul','Delaware','Columbia','Flórida','Geórgia','Havaí','Idaho','Illinois','Indiana','Iowa',
           'Kansas','Kentucky','Luisiana','Maine','Maryland','Massachusetts','Michigan','Minnesota','Mississippi','Missouri',
           'Montana','Nebraska','Nevada','Nova Hampshire','Nova Iorque','Nova Jérsei','Novo México','Ohio','Oklahoma',
           'Oregon','Pensilvânia','Rhode Island','Tennessee','Texas','Utah','Vermont','Virgínia','Virgínia Ocidental',
           'Washington','Wisconsin','Wyoming')
resident <- c(35.7,22.5,21.7,50.5,26.7,30.4,28.8,40,54.6,59.5,74.9,43.2,34.4,23.1,45.5,13.6,31.3,52.1,59.2,70.5,65.4,47.8,59.7,51.4,
              44.6,54,36.1,67.6,40.6,57.6,44.2,67.4,18.6,52.7,33.4,30.2,37,49.3,59.1,30.2,40.6,60.3,45.8,47.7,29.1,46.2,33.8,37.8,
              32.3,62.1,37.8)
dados <- data.frame(state,resident)

menor <- state[resident==min(resident)]
maior <- state[resident==max(resident)]
rsul <- data.frame(menor,maior)
colnames(rsul) <- c('Estado com menor número de residentes','Estado com maior número de residentes')
rsul
par(mfrow=c(1,2), mar=c(4,4,4,3))
boxplot(resident, ylab="", main="Boxplot o número de residentes assistidos \n
 nos estados americanos",cex.main=.85)
hist(resident, prob=T, axes=F, main="Histograma do número de residentes assistidos \n
 nos estados americanos",cex.main=.85,xlab='',ylab='')
axis(1,at=seq(10,80,5),cex.axis=.75,font.axis=3,hadj=0.51)
rug(resident)
lines(density(resident),col='darkred')

############################# Exercício 19 #############################################################################
########################################################################################################################
tar <- c(3,14,16,10,18,13,1,10,16,13,15,12,8,12,13,15,12.9,16,17,4,13,3,1,13,16,11,16,16,19,10,16,16,9,4,0.7)
nic <- c(0.3,1.2,1.3,1,1.4,1.2,0.1,0.9,1.3,1.1,1.2,1,0.9,1.3,1.2,1.3,0.89,1.2,1.3,0.4,1.1,0.4,.2,1.1,1.3,1,1.3,1.3
         ,1.4,1.1,1.2,1.2,1,.5,0.09)
# Gráfico de dispersão unidimensional
stripchart(tar, pch=1,col='red',axes=F)
axis(1,at=seq(0,20,1),cex.axis=.7)
title(main="Diagrama de dispersão unidimensional \n para alcatrão", xlab="Concentração de alcatrão",cex.main=.85,cex.lab=.75)
box()
par(mfrow=c(1,2))
hist(tar, prob=T, axes=F, main="Histograma da concentração de alcatrãos",cex.main=.85,xlab='',ylab='')
axis(1,at=seq(0,20,1),cex.axis=.75,font.axis=3,hadj=0.51)
rug(tar)
lines(density(tar),col='darkred')
hist(nic, prob=T, axes=F, main="Histograma da concentração de nicotina",cex.main=.85,xlab='',ylab='')
axis(1,at=seq(0,2,.1),cex.axis=.75,font.axis=3,hadj=0.51)
rug(nic)
lines(density(nic),col='darkred')

par(mfrow=c(1,1))
plot(nic,tar,axes=F,main='Diagrama de dispersão entre \n alcatrão e nicotina',
     xlab='Nicotina',ylab='Alcatrão',cex.lab=.75,pch=19,col='darkgreen',cex.main=.85)
axis(1,at=seq(0,2,.1),cex.axis=.75,font.axis=3,hadj=0.51)
axis(2,at=seq(0,20,1),cex.axis=.75,font.axis=3,hadj=0.51)
abline(lm(tar~nic),lwd=1)
box()
# Coeficiente de correlação linear - Este coef. é não-paramétrico
cor.test(tar,nic, method='spearman')
text(0.9,4,'Coef. Correlação de Spearman \n 0.91',col='red',cex=.8)

############################# Exercício 20 #############################################################################
########################################################################################################################
# dados
b <- c(7.1,7.8,8,8.3,9,10.1,10.9,12.1,12.5,13.3,14.1,15.1,15.8,16.9,18.7,19.3,20.4,21,21.2,21.9,21.6,22.7,21.9,22.5,23,
       23.4,23.3,23.7,24.3,24.8,26.4,25.5,24.8,24.3,23.9,24.5,24.3,25.6,25.7,27.2,28.4,29.5,30,30.3,31,32.8,34.2,36,38.5,
       41.6,43.8,45.2,45.2)
y <- 1940:1992

plot(b,xlab='',main='Taxas de nascimento por mulheres não-casadas \n  EUA 1940-1992',ylab='Taxa anual',type='l',
     col='darkorange',fg='darkblue',axes=F,cex.main=1)
points(b,pch=19,cex=0.5,col='darkblue')
axis(1,at=seq(1,53,2),seq(1940,1992,2),col='darkblue',col.axis = "darkblue",cex.axis=.6,font.axis=3,hadj=0.51,las=2)
axis(2,at=seq(0,50,1),col='darkblue',col.axis = "darkblue",cex.axis=.75,font.axis=3,hadj=0.51)
box()

