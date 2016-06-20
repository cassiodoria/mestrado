dadosCestaBasica = read.table("CestaBasicaMensal-2012-2015.csv", header=TRUE, sep=";", dec=",")
# 1 Arroz
# 2 Feijão
# 3 Açucar
# 4 Café
# 5 Farinha de Trigo
# 6 Farinha de Mandioca
# 7 Batata
# 8 Cebola
# 9 Alho
# 10 Ovos Brancos
# 11 Margarina
# 12 Extrato de Tomate
# 13 Óleo de Soja
# 14 Leite Integral
# 15 Macarrão com Ovos
# 16 Biscoito Maisena
# ...
# 31 Absorvente
produto = 8 # escolha o produto
produtoCestaBasica = unlist(dadosCestaBasica[produto, -1])

produtoTreino = ts(produtoCestaBasica[1:37], start = c(2012,1), end = c(2014, 12), frequency = 12)
produtoTeste = ts(produtoCestaBasica[38:48], start = c(2015,1), end = c(2015, 12), frequency = 12)

pdf(paste(dadosCestaBasica[produto, 1], ".pdf", sep=" "), family = "Times", height = 7, width = 10.5)
par(mfrow=c(2,1))
# -------------------- Série ------------------------------ #
maiorValorSerie = max(produtoCestaBasica)
plot(ts(produtoCestaBasica, start = c(2012,1), end = c(2015, 12), frequency = 12), type='l', lwd=2, main=dadosCestaBasica[produto, 1], xlab="Ano", ylab="Preço", ylim=c(0,maiorValorSerie*1.1))
grid(col="darkgrey", lwd=1)

# -------------------- Predição ARIMA --------------------- #
require(forecast)
ARIMAfit <- auto.arima(produtoTreino, approximation=FALSE,trace=FALSE, d=0, D=1, allowdrift = FALSE, allowmean = FALSE)
summary(ARIMAfit)
forecastARIMA = forecast(ARIMAfit, h = 12);
maiorValor = max(max(forecastARIMA$upper[,2]), produtoTeste);
plot(forecast(ARIMAfit, h = 12), main=dadosCestaBasica[produto, 1], xlab="Ano", ylab="Preço", lwd=2, ylim=c(0,maiorValor*1.1))
lines(produtoTeste, col="green", lwd = 2)
grid(col="darkgrey", lwd=1)
dev.off()

# Calcular o ME     RMSE       MAE        MPE     MAPE      MASE        ACF1
dif = produtoTeste - unlist(forecast(ARIMAfit, h = 12)$mean)
