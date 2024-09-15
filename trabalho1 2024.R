# Instalando o pacote readxl 
install.packages("readxl")
install.packages("lmtest")
install.packages("sandwich")
install.packages('ARDL')
# Carregando o pacote
library(readxl)
library(lmtest)
library(sandwich)
library(ARDL)

setwd(paste("/home/kalil/Documents/Graduacao/UFRJ/",
            "MicroEconometria", sep = "/"))

# Importar a planilha pro R
dados <- read_excel("./DDMv2.xls", sheet = "Dados para regressao")

# Pegando cada variavel separadamente
M <- dados$M
Y <- dados$Y
Er <- dados$Er
Trimestre<-dados$Trimestre
Trim2 <- dados$Trim2*1
Trim3 <- dados$Trim3*1
Trim4 <- dados$Trim4*1

# Transformando os dados em log
m <- log(M)
y <- log(Y)
er <- log(Er)

Ldados<-cbind(m,y,er, Trim2, Trim3, Trim4)


# Fazer um grafico 
plot(m, type = "l", xlab = "tempo", ylab = "m", main = "Log Importações")
plot(y, type = "l", xlab = "tempo", ylab = "Dipp", main = "Log Dif ipp")
#install.packages('fastDummies')
#library('fastDummies')
#monthD <- dummy_cols(MONTH)

# REGRESSAO
Q1 <- lm(m~y)
summary(Q1)

# REGRESSAO
Q2 <- lm(m~y+er+Trim2+Trim3+Trim4)
summary(Q2)

Q3<-lm(m~y+Trim2+Trim3+Trim4)
summary(Q3)

# Confirmando:
Q3c<-lm(er~y+Trim2+Trim3+Trim4)
summary(Q3c)


### Duas formas para Q6
# Considere Q2 sem dummies
Q2sd <- lm(m~y+er)
summary(Q2sd)

A1<-lm(y~er)

summary(lm(m~A1$residuals))

# Considere Q2 com dummies
A2<-lm(y~er+Trim2+Trim3+Trim4)

summary(lm(m~A2$residuals))
summary(Q2)

#Obs - para replicar os desvios padrões
A3<-lm(m~er+Trim2+Trim3+Trim4)
summary(lm(A3$residuals~A2$residuals+er+Trim2+Trim3+Trim4))
# note que abaixo a razão entre os desvios padrões, 0.1128/0.1103, é igual a (90/86)^0.5.
summary(lm(A3$residuals~A2$residuals))
# aqui não funciona (vide notas de aula)
summary(lm(m~A2$residuals))
