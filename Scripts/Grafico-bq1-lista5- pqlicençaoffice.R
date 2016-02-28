
ggplot()
[S] mM	1/[S]	1/V	1/Va	1/Vb
0,02	5,00	0,10	0,25	0,22
0,40	2,50	0,06	0,15	0,12
1,00	1,00	0,04	0,09	0,06
2,00	0,50	0,03	0,07	0,04
10,00	0,10	0,02	0,05	0,02

#read

bq <- read.csv("C:/Users/CarlosVictor/Downloads/Livro.csv")
bq
ggplot(data = bq , aes(x = X1..S., y=X1.V), lm(bq))+
  geom_point () + geom_line ()
       
##Plot and Group Plots       
library(ggplot2)
ggplot (lm (bq))
coef(lm(bq))
plot(lm(bq))
plot(bq)
bq

v <- ggplot(data = bq , aes(x = X1..S., y=X1.V)) +
  geom_point () + geom_line ( colour="green") +
  geom_smooth(method=lm, colour="green") + 
  geom_text(aes(x=min(0), y=max(0.12), label=texto.v), hjust=0, vjust=1)


plot(data = bq, aes(x = X1..S., y=X1.V ~x1.vb))

install.packages("gridExtra")
library(gridExtra)

va <- ggplot(data = bq , aes(x = X1..S., y=X1.Va)) +
  geom_point () + geom_line ( colour="blue") +
  geom_smooth(method=lm, colour="blue") + 
  geom_text(aes(x=min(0), y=max(0.12), label=texto.va), hjust=0, vjust=1)


vb <- ggplot(data = bq , aes(x = X1..S., y=X1.Vb)) +
  geom_point () + geom_line ( colour="red") +
  geom_smooth(method=lm, colour="red") + 
  geom_text(aes(x=min(0), y=max(0.12), label=texto.vb), hjust=0, vjust=1)



modelo.v <- lm(xv)
coeficientes.v <- modelo.v$coefficients
texto.v <- sprintf('y = %.2f + %.2fx, r² = %.2f', coeficientes.v[1], coeficientes.v[2], summary(modelo.v)$r.squared)

modelo.va <- lm(xva)
coeficientes.va <- modelo.va$coefficients
texto.va <- sprintf('y = %.2f + %.2fx, r² = %.2f', coeficientes.va[1], coeficientes.va[2], summary(modelo.va)$r.squared)

modelo.vb <- lm(xvb)
coeficientes.vb <- modelo.vb$coefficients
texto.vb <- sprintf('y = %.2f + %.2fx, r² = %.2f', coeficientes.vb[1], coeficientes.vb[2], summary(modelo.vb)$r.squared)

table(bq)
xv <- select(bq,X1.V, X1..S.)
View(xv)

xva<- select(bq, X1.Va, X1..S.)

xvb <- select(bq, X1.Vb, X1..S.)

grid.arrange(v, va, vb, ncol=2, nrow=2)

### calcular a equação da reta
coef(lm(xvb))
lm(X1.Vb ~ X1..S., data=bq)
summary(lm(xvb))
