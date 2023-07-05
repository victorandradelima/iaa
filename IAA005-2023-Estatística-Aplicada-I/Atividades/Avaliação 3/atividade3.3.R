# Atividade 3 de Estatística Aplicada 1

# Carregar conjunto de dados
load("imoveiscwbav.RData")
imoveis <- imoveiscwbav

# Estimando um modelo preliminar
# Gerando um Regressão Linear tomando como referência o preço dos imóveis
resultados <- lm (price~., data=imoveis)
summary (resultados)

# Criando variável tranformada para melhorar o modelo:
# 1 - Substituindo o price por uma variável que representa o log natural
#     do preço do metro quadrado do imóvel
imoveis$lnPricePerMeter <- with(imoveis, log(price/tarea))
imoveis$lnPricePerMeterParea <- with(imoveis, log(price/parea))
imoveis$lnAge <- with(imoveis, log(age))
# 2 - Substituindo a variável idade por uma que representa o quadrado dela
# imoveis$age2 <- with(imoveis, age^2)

# Vamos reestimar o modelo agora com a variavel "lnPricePerMeter" ao inves de "price"
# e a variável "age2" ao invés de age
resultados <- lm (lnPricePerMeter~.-price-tarea-parea-age, data=imoveis)
summary (resultados)

# Verificando os outliers pelo teste de Bonferroni
library (carData)
library(car)
outliers <- outlierTest(resultados)
outliers
# Saída:
#     rstudent unadjusted p-value       Bonferroni p
# 393 4.315041            1.9099e-05    0.010333
# 31  4.218737            2.8977e-05    0.015677

# Deletando as linhas encontradas como outliers
row.names.imoveis <- c(as.numeric(names(outliers$rstudent))) # filtrando a linha do outlier
for (name in row.names.imoveis) {
  imoveis <- imoveis[!(row.names(imoveis) %in% name),] # autlizando os dados sem o outlier
}

# Verificando se ainda ha outliers
resultados <- lm (lnPricePerMeter~.-price-age-parea, data=imoveis)
summary (resultados)
outliers2 <- outlierTest(resultados)
outliers2
# Saída:
# No Studentized residuals with Bonferroni p < 0.05
# Largest |rstudent|:
#       rstudent unadjusted   p-value     Bonferroni p
# 361   -3.844855             0.0001356   0.073089
#
# Não foram encontrados mais outliers

# Realizando uma regressao stepwise, para verificar quais variaveis devem efetivamente entrar no modelo
library(RcmdrMisc)
stepwise <- stepwise(resultados, direction= 'backward/forward', criterion ='AIC')
lmStepwise <- lnPricePerMeter ~ tarea + bath + ensuit + garag + plaz + trans + school + bike + categ + lnPricePerMeterParea
# Verificando o valor "call" do stepwise, vemos quais variáveis devemos chamar:
# lm(formula = lnPricePerMeter ~ ensuit + garag + park + balc + elev + fitg + party + categ + age2, data = imoveis)
resultados <- lm(formula = lmStepwise, data=imoveis)
summary (resultados)

# Verificando a existencia de multicolineariedade
car::vif(resultados)
# Saída:
# ensuit    garag     park     balc     elev     fitg    party    categ     age2 
# 1.591122 1.629823 1.071353 1.522906 1.370204 1.675201 2.006241 1.122213 1.424282 
# Resultado: Vamos usar como base que o fator de Inflacao da variância deve ser maior
# do que 5 para ter uma correlação siginificativa.
# desta forma, nenhum dado deve ser desconsiderado por multicolineariedade.

# Fazendo o teste de especificacao do modelo
library (zoo)
library (lmtest)
resettest(lmStepwise, power=2:3, type="regressor", data=imoveis)
# Saída:
# RESET test
# data:  lnPricePerMeter ~ ensuit + garag + park + balc + elev + fitg + party + categ + age2
# RESET = 6.9628, df1 = 18, df2 = 511, p-value = 6.133e-16
# Valor tabelado para comparação

qf(.95,df1=20,df2=509)
# 1.624075

# Resultado: Desta forma pode avaliar que como o valor de F do modelo (RESET = 6.9628) é maior do que
# o valor tabelado (1.624075) nosso modelo está incorretamente especificado.





# Verificando a heterocedasticidade
# Teste utilizado: Teste de Breusch-Pagan

bptest(lmStepwise, studentize=FALSE, data=imoveis)
# Saída:
# Breusch-Pagan test
# data:  lnPricePerMeter ~ ensuit + garag + park + balc + elev + fitg + party + categ + age2
# BP = 51.693, df = 9, p-value = 5.166e-08

# F
qchisq(0.95, df=10)
# Saída:
# 16.91898

# Resultado
# Como o resultado do teste BP (51.693) eh maior que o tabelado (16.91898) rejeita-se a hipotese de homocedasticidade

# Fazendo correção da variância não constante por regressão robusta
library (sandwich)
resultados <- lm(lmStepwise, data=imoveis)
summary(resultados)
coeftest(resultados, vcov=vcovHC(resultados, type="HC1"))


lmStepwise2 <- lnPricePerMeter ~ tarea + bath + ensuit + garag + trans + school + bike + categ + lnPricePerMeterParea
bptest(lmStepwise2, studentize=FALSE, data=imoveis)
qchisq(0.95, df=9)

resultados <- lm(lmStepwise2, data=imoveis)
summary (resultados)

# Saída:
# t test of coefficients:
# Estimate  Std. Error  t value  Pr(>|t|)    
# (Intercept)  8.3137e+00  7.0283e-02 118.2890 < 2.2e-16 ***
# ensuit       7.7513e-02  1.2010e-02   6.4538 2.468e-10 ***
# garag        1.5768e-01  1.6263e-02   9.6957 < 2.2e-16 ***
# park        -7.1199e-02  1.5722e-02  -4.5286 7.344e-06 ***
# balc         6.0547e-02  1.8582e-02   3.2584  0.001192 ** 
# elev        -6.4070e-02  1.9669e-02  -3.2574  0.001197 ** 
# fitg         9.3154e-02  2.0617e-02   4.5183 7.696e-06 ***
# party        5.1679e-02  2.2907e-02   2.2561  0.024474 *  
# categ        3.1956e-01  4.8900e-02   6.5350 1.498e-10 ***
# age2        -2.6557e-04  2.1985e-05 -12.0796 < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Resultado: Pelo teste de coeficientes vemos que nenhnuma das variáveis utilizadas
# é insiginificante a mais de 95% do modelo, logo não vamos descastar nenhuma.

# # Vamos corrigir a heterocedasticidade por regressao linear robusta (sandwich)
# resultados1 <- coeftest(resultados, vcov=vcovHC(resultados, type="HC1"))
# # Saída:
# # t test of coefficients:
# # Estimate       Std. Error  t value      Pr(>|t|)    
# # (Intercept)    8.3137e+00  7.0283e-02   118.2890 < 2.2e-16 ***
# #   ensuit       7.7513e-02  1.2010e-02   6.4538 2.468e-10 ***
# #   garag        1.5768e-01  1.6263e-02   9.6957 < 2.2e-16 ***
# #   park        -7.1199e-02  1.5722e-02  -4.5286 7.344e-06 ***
# #   balc         6.0547e-02  1.8582e-02   3.2584  0.001192 ** 
# #   elev        -6.4070e-02  1.9669e-02  -3.2574  0.001197 ** 
# #   fitg         9.3154e-02  2.0617e-02   4.5183 7.696e-06 ***
# #   party        5.1679e-02  2.2907e-02   2.2561  0.024474 *  
# #   categ        3.1956e-01  4.8900e-02   6.5350 1.498e-10 ***
# #   age2        -2.6557e-04  2.1985e-05 -12.0796 < 2.2e-16 ***
# #   ---
# #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



# Calculando os intervalos de confiança para regressão linear robusta
confint <- confint (resultados, level = 0.95)
confint
# Saída:
#              2.5 %         97.5 %
# (Intercept)  8.1756165190  8.4517517710
# ensuit       0.0539192476  0.1011074549
# garag        0.1257324106  0.1896276328
# park        -0.1020847309 -0.0403134401
# balc         0.0240439414  0.0970509006
# elev        -0.1027089138 -0.0254309050
# fitg         0.0526522754  0.1336548581
# party        0.0066799569  0.0966786386
# categ        0.2235015598  0.4156255688
# age2        -0.0003087537 -0.0002223778

# Analisando indicadores de performace do modelo final
library(performance)
model_performance(resultados)



# Resíduos
resultados$residuals
realestate<- within(imoveis, {residuos <- residuals(resultados) })

# Teste de normalidade
# Amostra é grande, não precisa testar normalidade dos residuos

# Valores preditos
resultados$fitted.values
realestate1<- within(realestate, {preditos <- fitted.values(resultados)})

# Fazendo predicoes no modelos OLS

resultados

#Para:
# age2 = 14 # anos
# parea = 120 # (metros quadrados de area privativa)
# tarea = 180 # (metros quadrados de area total)
# bath = 2 # (banheiros)
# ensuit = 1 # (tem uma suite)
# garag = 1  # (tem uma garagem)
# park = 2 # (distancia em km de um parque)
# balc = 1 # (vari?vel binaria, significa que tem sacada)
# elev = 1 # (vari?vel binaria, significa que tem elevador)
# fitg = 1 # (significa que tem area fitness no condominio)
# party = 1 # (significa que tem area de festas no condominio)
# categ = 1 # (significa que eh um apartamento; zero significa casa)
# plaz = 1 # Distancia de pra;a mais proxima

tarea = 150
age = 14
ensuit = 1
garag = 1
plaz = 1
park = 2
bike = 1
balc = 1
elev = 1
fitg = 1
categ = 1

teste <- data.frame(age=age, ensuit=ensuit, garag=garag, plaz=plaz,
                    park=park, bike=bike, balc=balc, elev=elev,
                    fitg=fitg, categ=categ)

predito <- predict(object = resultados, teste)

# Como o preço do imóvel foi subintituído pelo ln do preço do metro quadrado
# precisamos fazer o seguinte calculo para chegar no valor de preço do imóvel
predmedfinal <- (exp(predito))*tarea
predmedfinal

# Estimando o preço inferior do intervalo de confiança
Lestimate=confint[1,1] + confint[2,1]*age + confint[3,1]*ensuit + confint[4,1]*garag +
  confint[5,1]*plaz + confint[6,1]*park + confint[7,1]*bike + confint[8,1]*balc +
  confint[9,1]*elev + confint[10,1]*fitg + confint[11,1]*categ
Lestimate
(exp(Lestimate))*tarea
# Saída
# 480337.2

# Estimando o preco superior do intervalo de confianca
Uestimate=confint[1,2] + confint[2,2]*age + confint[3,2]*ensuit + confint[4,2]*garag +
  confint[5,2]*plaz + confint[6,2]*park + confint[7,2]*bike + confint[8,2]*balc +
  confint[9,2]*elev + confint[10,2]*fitg + confint[11,2]*categ
Uestimate
(exp(Uestimate))*tarea
# Saída
# 1360475
# Resultado: O valor do intervalo de confiança ainda é muito amplo

# Estimando o intervalo de confiança para a media
n <- nrow(imoveis)
m <- predmedfinal
s <- sd(imoveis$price)
dam <- s/sqrt(n)
CIlwr <- m + (qnorm(0.025))*dam
CIupr <- m - (qnorm(0.025))*dam 

CIlwr
# 766511.9
CIupr
# 850258.8




