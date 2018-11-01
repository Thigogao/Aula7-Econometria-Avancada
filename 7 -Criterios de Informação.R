                       #Aula 7 - Critérios de Informação

library(readxl)                                                        #Carrega os pacotes
variacao_PIB <- read.table("c:/Econometria/Variacao.xls", header = T)  #Carrega os arquivos
var_PIB <- ts(variacao_PIB$variacao_PIB, start =1951, frequency = 1 )  #Cria a serie temporal var_PIB
View(var_PIB)
plot(var_PIB, main="Variacao do PIB Brasileiro", col="Blue", ylab="%PIB", xlab="Ano")

acf(var_PIB)         #Cria a FAC -Função de Autocorrelação (ACF)
pacf(var_PIB)        #cria a FACP - Funçao de Autocorrelação Parcial (PACF)

AR1 <- arima(var_PIB, order = c(1,0,0))   #Estima um modelo autoreressivo de ordem p=1 ,AR(1)
MA1 <- arima(var_PIB, order = c(0,0,1))   #Estima um modelo de médias móveis ordem q=1 , MA(1)
ARMA11 <- arima(var_PIB, order = c(1,0,1))#Estima um modelo autoregressivo de médias móveis ordem p=1 e q=1 ARMA(1,1)

AIC(AR1) #Extrai a estatística AIC do modelo AR1
BIC(AR1) #Extrai a estatística BIC Ddo modelo AR1


AR1 <- arima(var_PIB, order = c(1,0,0))
AR2 <- arima(var_PIB, order = c(2,0,0))
AR3 <- arima(var_PIB, order = c(3,0,0))
AR4 <- arima(var_PIB, order = c(4,0,0))
AR5 <- arima(var_PIB, order = c(5,0,0))
AR6 <- arima(var_PIB, order = c(6,0,0))
AR7 <- arima(var_PIB, order = c(7,0,0))
AR8 <- arima(var_PIB, order = c(8,0,0))
AR9 <- arima(var_PIB, order = c(9,0,0))

MA1 <- arima(var_PIB, order = c(0,0,1))
MA2 <- arima(var_PIB, order = c(0,0,2))

ARMA1 <- arima(var_PIB, order = c(1,0,1))
ARMA2 <- arima(var_PIB, order = c(2,0,1))
ARMA3 <- arima(var_PIB, order = c(3,0,1))
ARMA4 <- arima(var_PIB, order = c(4,0,1))
ARMA5 <- arima(var_PIB, order = c(5,0,1))
ARMA6 <- arima(var_PIB, order = c(6,0,1))
ARMA7 <- arima(var_PIB, order = c(7,0,1))
ARMA8 <- arima(var_PIB, order = c(8,0,1))
ARMA9 <- arima(var_PIB, order = c(9,0,1))
ARMA10 <- arima(var_PIB, order = c(1,0,2))
ARMA11 <- arima(var_PIB, order = c(2,0,2))
ARMA12 <- arima(var_PIB, order = c(3,0,2))
ARMA13 <- arima(var_PIB, order = c(4,0,2))
ARMA14 <- arima(var_PIB, order = c(5,0,2))
ARMA15 <- arima(var_PIB, order = c(6,0,2))
ARMA16 <- arima(var_PIB, order = c(7,0,2))
ARMA17 <- arima(var_PIB, order = c(8,0,2))
ARMA18 <- arima(var_PIB, order = c(9,0,2))

#Exemplo aplicação múltipla - Extra (Deve-se completar as estimações antes de executar esse código)

estimacoes <- list(ARMA1,ARMA2, ARMA3, ARMA4,ARMA5, ARMA6,ARMA7,ARMA8,ARMA9,
                   ARMA10,ARMA11,ARMA12,ARMA13,ARMA14,ARMA15,ARMA16,ARMA17,ARMA18)      #Cria uma lista com os estimadores
sapply(estimacoes, AIC)                 #Aplica o comando AIC na lista
sapply(estimacoes, BIC)                 #Aplica o comando BIC na lista

#Exemplo de criação de tabela com resultados - Extra
AIC <- sapply(estimacoes, AIC) 
BIC <- sapply(estimacoes, BIC)
Modelo <- c("ARMA1", "ARMA2", "ARMA3", "ARMA4", "ARMA5", "ARMA6", "ARMA7", "ARMA8", "ARMA9", "ARMA10", "ARMA11", "ARMA12","ARMA13", "ARMA14", "ARMA15","ARMA16", "ARMA17","ARMA18")

Resultados <- data.frame(Modelo, AIC, BIC)
View(Resultados)

ARMA18
