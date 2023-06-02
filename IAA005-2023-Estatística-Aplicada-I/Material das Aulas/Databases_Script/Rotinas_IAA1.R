###############################################################################
#                                    AULA 1                                   #
###############################################################################

####################### Comandos gerais do R ##################################
# Limpa a tela

CTRL + L

# Importar conjunto de dados

salarios <- read.table("C:/iaa/cps91.txt", header = TRUE, sep = "", 
                       na.strings = "NA", dec = ".", strip.white = TRUE)

# Ver e alterar conjunto de Dados

View (salarios)

# Ver variaveis do conjunto de dados ativo

names (salarios)

# Salvar conjunto de dados no formato R

save ("salarios", file = "C:/iaa/salarios.RData")

# Voce pode tambem definir o diretorio, neste caso elimina o caminho acima 

setwd("C:/iaa")

# Carregar conjunto de dados salvo no formato do R

load("C:/iaa/salarios.RData")

# Construcao de nova variavel - multiplicacao valores da variavel "age" por 1

salarios$age_new <- with (salarios, age*1)

# Apagar variavel

salarios <- within(salarios, {age_new<-NULL})

######################## Distribuicao de frequencia ###########################

# install.packages("fdth")

library (fdth)

table <- fdt(salarios$husearns)

print (table)

############################## Histograma #####################################   

# visualizando estatisticas e informacoes das variaveis
summary(salarios)

# Histograma por sturges

hist(salarios$husage)

# Histograma para 5 quebras

hist(salarios$husage, breaks = 5, xlab = "Husband Age",ylab = "Frequency")


################################# Graficos ####################################

################################## Boxplot ####################################

ceo <- read.table("C:/iaa/ceo.txt", header = TRUE, sep = "", na.strings = "NA", 
                  dec = ".", strip.white = TRUE)

View (ceo)

names (ceo)

# install.packages("car")

library (car)

Boxplot( ~ comten, data=ceo, id=list(method="y"))

################## Dispersao entre X e Y (scaterplot) #########################

# install.packages("lattice")

library (lattice)

xyplot(comten ~ age, type="p", pch=16, auto.key=list(border=TRUE), 
       par.settings=simpleTheme(pch=16), scales=list(x=list(relation='same'), 
       y=list(relation='same')), data=ceo)

# Outra forma de scaterplot

attach(ceo)
plot(comten, age, main="Tempo de Empresa X Idade do Funcion?rio",
     xlab="Tempo de Empresa ", ylab="Idade ", pch=19)
detach(ceo)

############################ Grafico de linhas ################################

# Importanto arquivo de dados temporais
setwd("C:/iaa/Estat I 2023")
economics <- read_excel("economics.xlsx", 
                        col_types = c("date", "numeric", "numeric", 
                        "numeric", "numeric", "numeric"))
View (economics)

# install.packages("ggplot2")
# install.packages("tidyverse")

library(ggplot2)
library(tidyverse)

ggplot(economics, aes(x = date, y = unemploy)) +
  geom_line() +
  geom_smooth(se = FALSE)

# Outro exemplo

# Importanto arquivo de dados temporais

# install.packages("readxl")
# install.packages(plotly)
# install.packages(tidyverse)

library(readxl)
library(plotly)
library(tidyverse)

basepetr4 <- read_excel("C:/iaa/estat_I/basepetr4.xlsx")

ggplotly(
  basepetr4 %>%
    mutate(Data = as.Date(Data)) %>%
    ggplot() +
    geom_line(aes(x = Data, y = Fechamento, color = "série")) +
    scale_color_viridis_d() +
    scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid = element_line(color = "grey90"),
          panel.border = element_rect(color = "black", fill = NA),
          legend.position = "none")
)

############### Distribuicoes Discretas de Probabilidade ######################

########################### Distribuicao Binomial #############################

# Exemplo: x = 0; n = 10; p = 0.3; Resultado = 2.82%

local({
 .Table <- data.frame(Probability = dbinom(0:5, size=10, prob = 0.3))
 rownames(.Table) <- 0:5
 print (.Table)   
})


############################ Distribuicao Poisson #############################

# Exemplo: lambda = 5; x = 3; resultado 14.04%

local({
  .Table <- data.frame(Probability = dpois(3, lambda=5))
  rownames(.Table) <- 3
  print (.Table)   
})


########################## Distribuicao Geometrica ############################

#Numero de fracassos ate primeiro sucesso

# Exemplo: Y=k=2; p = 0.4; Resultado = 14,40%

local({
  .Table <- data.frame(Probability = dgeom(0:5, prob=0.4))
  rownames(.Table) <- 0:5
  print (.Table)   
})

# Numero de tentativas necessarias para se obter o primeiro sucesso

# Para evitar a notacao cientifica 
options(scipen=999)

# Exemplo: p = 0.10; Y = k = 34 ==> K-1 = 33 ; Resultado 0.3%

local({
  .Table <- data.frame(Probability = dgeom(30:40, prob=0.1))
  rownames(.Table) <- 30:40
  print (.Table)   
})


###################### Distribuicao Hipergeometrica ###########################

# Exemplo: numero de sucessos na amostra = 2; 
# numero de sucessos na populacao = m = 3; 
# numero de fracassos na populacao = n = 3; 
# tamanho da amostra = k = 4; 
# Resultado = 60%

local({
  .Table <- data.frame(Probability = dhyper(1:3, m = 3, n = 3, k = 4))
  rownames(.Table) <- 1:3
  print (.Table)   
})


################ Distribuicao Binomial Negativa (inversa) #####################

# Exemplo: x = 10; k=numero de sucessos= size = 3; p = 0.02; Resposta = 0.025% 

local({
  .Table <- data.frame(Probability = dnbinom(6:10, size = 3, prob=0.02))
  rownames(.Table) <- 6:10
  print (.Table)   
})

# no resultado ler a observacao "7", pois 10-3=7 , dado que a distribuicao eh
# negativa(inversa)

###############################################################################
#                                 f                                      #
###############################################################################

################# Distribuicao Normal padronizada (Z) #########################

# para 95% de confianca o valor de "Z" para lado da distribuicao eh 1.96 

#install.packages("RcmdrMisc")
#install.packages("tigerstats")
library(RcmdrMisc)
library(tigerstats)

# Como no R os valores da tabela Z sao para uma cauda, para se obter o 
# nivel de significancia de 95% deve-se colocar na funcao o valor de
# 2,5% 

qnorm(0.025)

local({
  .x <- seq(-1.96, 1.96, length.out=1000)  
  plotDistr(.x, dnorm(.x, mean=0, sd=1), cdf=FALSE, xlab="x", ylab="Density", 
          main=paste("Normal Distribution (Z):  Mean=0, Standard deviation=1"))
})

pnormGC(c(-1.96,1.96),region="between",mean=0,
        sd=1,graph=TRUE)


######################## Distribuicao t de Student ############################

# Para 95% de confianca e 120 graus de liberdade t= 1.98
# Para a tabela t tambem deve-se colocar 2,5% porque os valores sao para uma 
# cauda

qt(0.025, 120)

local({
  .x <- seq(-1.98, 1.98, length.out=1000)  
  plotDistr(.x, dt(.x, df=120), cdf=FALSE, xlab="x", ylab="Density", 
            main=paste("t Distribution: Degrees of freedom=100"))
})

ptGC(c(-1.98,1.98),region="between",df=120, graph = TRUE)


######################## Distribuicao ChiSquare ###############################

# graus de liberdade 5 e 5% (1.145) na cauda inferior e 95% (11.070) na 
# cauda superior

qchisq(0.05,5)
qchisq(0.95,5)

local({
  .x <- seq(1.145, 11.070, length.out=1000)  
  plotDistr(.x, dchisq(.x, df=5), cdf=FALSE, xlab="x", ylab="Density", 
            main=paste("ChiSquared Distribution: Degrees of freedom=5"))
})

pchisqGC(c(11.070),region="below",df=5, graph = TRUE)


################## Distribuicao F de Fischer e Snedecor #######################

# com 20 (numerador) e 19 graus de liberdade (denominador)

qf(0.95, 20, 19)

local({
  .x <- seq(0, 2.155, length.out=1000)  
  plotDistr(.x, df(.x, df1=20, df2=19), cdf=FALSE, xlab="x", ylab="Density", 
            main=paste("F Distribution: Numerator df=20, denominator df=19"))
})

# install.packages("sjPlot")
library(sjPlot)

dist_f(f = 0, deg.f1 = 20, deg.f2 = 19)

######################### Distribuicao logistica ##############################

qlogis(0.95)

local({
  .x <- seq(-2.95, 2.95, length.out=1000)  
  plotDistr(.x, plogis(.x, location=0, scale=1), cdf=TRUE, xlab="x", 
            ylab="Cumulative Probability", 
            main=paste("Logistic Distribution:  Location=0, Scale=1"))
})


######################## Estatisticas descritivas #############################

################################## Media ######################################

load("C:/iaa/salarios.RData")

mean(salarios$husearns)
mean(salarios$earns)

# A media do rendimento dos maridos na amostra eh US$453.54
# A media do rendimento das esposas na amostra eh US$232,833

453.5406/232.833

# Portanto, o rendimento medio dos maridos eh quase o dobro do 
# rendimento medio das esposas


################################# Mediana #####################################

median (salarios$husearns)
median (salarios$earns)

# A mediana do rendimento dos maridos eh de US$418,5
# A mediana do rendimento das esposas eh de US$185,0 

418.5/185

# Em termos de mediana o rendimento dos maridos eh mais que o
# dobro do rendimento das esposas

################################## Moda #######################################

# Exemplo: moda da idade da esposa

v <- salarios$age
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
result <- getmode(v)
print(result)

# Outra forma de extrair a moda

# Para a idade dos maridos 
table(salarios$husage)
subset(table(salarios$husage), 
       table(salarios$husage) == max(table(salarios$husage)))

# Para a idade das esposas
table(salarios$age)
subset(table(salarios$age), 
       table(salarios$age) == max(table(salarios$age)))

# A moda da idade dos maridos eh de 44 anos, com 201 pessoas 
# A moda da idade das esposa eh de 37 anos, com 217 pessoas


############################### Variancia #####################################

var(salarios$husearns)

var(salarios$earns)

# A variancia do rendimento dos maridos eh de US$ 165638,1
# A variancia do rendimento das esposas eh de US$ 69340,83

165639.1/69340.83

# Portanto a variancia do rendimento dos maridos 
# eh mais que 2X (o dobro) a variancia dos rendimentos das esposas

############################# Desvio Padrao ###################################

sd(salarios$husearns)
sd(salarios$earns)

# O desvio padrao do rendimento dos maridos eh de US$406,98 
# O desvio padrao do rendimento das esposas eh de US$263,32

406.9878/263.3265

# O desvio padrao dos rendimentos dos maridos eh mais que 50%
# superior ao das esposas


######################## Coeficiente de Variacao ##############################

meanM <- mean(salarios$husearns)
meanE <- mean(salarios$earns)
sdM <- sd(salarios$husearns)
sdE <- sd(salarios$earns)

cvM <- (sdM/meanM)*100
cvM

cvE <- (sdE/meanE)*100
cvE

# O coeficiente de variacao do rendimento dos maridos eh de 89,73%
# O coeficiente de variacao do rendimento das esposas eh de 113,09%

# Isso quer dizer que o rendimento dos maridos e esposas variam muito
# na amostra. Ainda, pode-se concluir que os rendimentos das esposas
# variam mais do que dos maridos.

# menor ou igual a 15% eh baixa dispersao: dados homogeneos
# entre 15 e 30% eh media dispersao
# maior que 30% eh alta dispersao: dados heterogeneos


######################## Valores Minimo e Maximo ##############################

summary(salarios$husearns)
summary(salarios$earns)

# O rendimento maximo das esposas eh maior do que dos maridos
# mas percebe-se que a mediana, a media e o 3.Quartil sao menores.


################################ Quantis ######################################

Q1 <- quantile(salarios$earns, probs = 0.25)
Q1

Q2 <- quantile(salarios$husearns, probs = 0.50)
Q2

Q3 <- quantile(salarios$husearns, probs = 0.75)
Q3

##################### Distancia interquantilica (IQR) #########################

husearns = salarios$husearns
earns = salarios$earns

IQR(husearns)
IQR(earns)

# A comparacao entre as distancias interquartilicas entre os rendimentos
# das esposas e maridos confirma que a variacao entre os rendimentos das
# esposas eh menor. Q3 - Q1

summary(salarios$husearns)
summary(salarios$earns)


################################ Percentis ####################################

quantile(husearns, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) 
quantile(earns, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) 


############################################################################### 
#                                  AULA 3                                     #
###############################################################################

# Distribuicao amostral da media (dam) - populacao infinita

# Exemplo do slide: Media = m = 150; Desvio padrao da amostra = s = 36; n = 36

m <- 150
s <- 36
n <- 36

dam <- s/n^0.5
dam

dam1 <- m + dam
dam2 <- m - dam

dam1
dam2

# Isso quer dizer que os valores vao variar entre 144 e 156 com uma
# media de 150

##### Exemplo para a variavel "husearns" da base de dados salarios

n <- nrow(salarios)

m <- mean(salarios$husearns)
m
s <- sd(salarios$husearns)

dam <- s/sqrt(n)
dam
dam1 <- m + dam
dam2 <- m - dam 

dam1
dam2

# Isso quer dizer que o rendimento dos maridos vai variar de US$448.11 a
# US$458.96 com uma media de US$453.54

# Exemplo para a variavel earns da base de dados salarios


n <- nrow(salarios)
m <- mean(salarios$earns)
m

s <- sd(salarios$earns)

dam <- s/n^0.5

dam1 <- m + dam
dam2 <- m - dam 

dam1
dam2

# Isso quer dizer que os rendimentos das esposas vai variar de US$229.32
# a US$236.34, com uma media de US$232.83


########### Distribuicao amostral da media (damf) - populacao finita ##########

# Exemplo: n = 16; N = 100; s = 57

n <- 16
N <- 100
s <- 57

damf <- (s/(n^0.5))*((N-n)/(N-1))^0.5
damf

# "Facamos de conta" que a amostra da base de dados "salarios" permite dizer 
# que a populacao eh finita, com uma populacao de 100.000

# Para o rendimento das esposas (earns)
n <- nrow(salarios)

N <- 100000

s <- sd(salarios$earns)

m <- mean(salarios$earns)
m
damf <- (s/sqrt(n))*sqrt((N-n)/(N-1))
damf

dam1 <- m + damf
dam2 <- m - damf

dam1
dam2

mean(salarios$earns)

# Isso quer dizer que a media do rendimentos das esposas vai variar
# entre US$229.42 e US$236.24 com uma media de US$232.83


################# Intervalo de confianca para a media (Z) #####################

# install.packages("carData")
# install.packages("datasets")
# install.packages("BSDA")

library(carData)
library(datasets)
library(BSDA)

sd <- sd(salarios$husearns)

x <- salarios$husearns

options(scipen = 999)
z.te

st(x, y = NULL, alternative = "two.sided", mu = 0, sigma.x = sd,
       sigma.y = NULL, conf.level = 0.95)

# Isso quer dizer que a media do rendimento dos maridos vai variar 
# entre US$442.91 e US$464.16, com uma media de US$453.54, com 95% de 
# confianca ou 5% de significancia

# Estatistica z = 83,646

# Confrontamos esse valor com o valor tabelado da estatistica Z para 
# 95% de confianca ou 5% de significancia (usamos isso para obter o
# valor tabelado, conforme comando abaixo)

qnorm(0.025)
qnorm(0.975)

# Como a estistica z calculada (83.646) eh maior que a estatistica tabelada
# (1.96) rejeitamos a hipotese (H0) de que o valor verdadeiro da media
# eh estatisticamente igual a zero.Ressalta-se que se o valor da estatistica z
# calculada fosse menor que -1.96 o resultado seria o mesmo ao interpretar 
# o teste. Mas, se a estatistica t calculada se situasse entre -1.96 e 1.96
# (valor tabelado obtido pela funcao acima) aceitariamos H0, portanto 
# poderiamos considerar que a media da variavel seria estatisticamente igual
# a zero, com 95% de confianca ou 5% de significancia.


################ Intervalo de confianca para a media (t) ######################

# install.packages("stats")

library(stats)

# Exemplo de IC para a variavel husearns da base de dados "salarios"

x <- salarios$husearns

t.test(x, y = NULL,
       alternative = c("two.sided"),
       mu = 450, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95, data=salarios)

# Isso quer dizer que a media das rendas dos maridos vai variar entre US$442.91
# e US$464.17, com uma media de US$453.54, com 95% de confianca ou
# 5% de significancia.

# Estatistica t = 83.646, com 5633 graus de liberdade

# Confrontamos esse valor com o valor tabelado da estatistica t para 
# 95% de confianca ou 5% de significancia (usamos isso para obter o
# valor tabelado, conforme comando abaixo)

qt(0.975, df=5633)
qt(0.025, df=5633)

# Como a estistica t calculada (83.646) eh maior que a estatistica tabelada
# (1.96) rejeitamos a hipotese (H0) de que o valor verdadeiro da media
# eh estatisticamente igual a zero.Ressalta-se que se o valor da estatistica t
# calculada fossa menor que -1.96 o resultado seria o mesmo ao interpretar 
# o teste. Mas, se a estatistica t calculada se situasse entre -1.96 e 1.96
# (valor tabelado obtido pela funcao acima) aceitariamos H0, portanto 
# poderiamos considerar que a media da variavel seria estatisticamente igual
# a zero, com 95% de confianca ou 5% de significancia. 


################# Intervalo de confianca para a variancia ##################### 
################ pressupondo que a "distribuicao eh normal" ###################

var <- var(salarios$husearns)

n <- nrow(salarios)

# obtendo os valores de chi quadrado

chiinf <- qchisq(0.025, df=5633)
chisup <- qchisq(0.975, df=5633)

superior <- ((n-1)*var)/chiinf
inferior <- ((n-1)*var)/chisup

inferior
superior

# Isso quer dizer que a variancia do rendimento dos maridos vai variar
# entre 159688.1 e 171930.7 com 95% de confianca ou 5% de significancia

# Confrontamos esse valor com a variancia da variavel

var(salarios$husearns)

# Consideramos a estimativa do intervalo verdadeira pois o valor verdadeiro
# da variancia 165639.1 se encontra no intervalo obtido (159688.1 e 171930.7)


################ Teste para a diferenca entre duas medias (z) #################

# Vamos usar os rendimentos dos maridos e esposas
# calculando os desvios padrao de x e y

sdx <- sd(salarios$husearns)

sdy <- sd(salarios$earns)

# Criando os objetos com as variaveis em analise

x <- salarios$husearns

y <- salarios$earns

z.test(x, y, alternative = "two.sided", mu = 0, sigma.x = sdx,
       sigma.y = sdy, conf.level = 0.95)

# Quer dizer que a diferenca entre as medias estara entre US$ 208,04 e
# US$ 233,36, para as medias de US$453.54 e US$232.83 (dif. = 220.70),
# com 95% de confianca ou 5% de significancia.

# Estatistica z = 34,175

# Confrontamos esse valor com o valor tabelado da estatistica z para 
# 95% de confianca ou 5% de significancia (usamos isso para obter o
# valor tabelado, conforme comando abaixo)

qnorm(0.975)

# Como a estistica z calculada (34.175) eh maior que a estatistica tabelada
# (1.96) rejeitamos a hipotese (H0) de que as medias sao iguais
# ou seja, de que a diferenca verdadeira entre as medias eh igual a zero.
# Ressalta-se que se o valor da estatistica t calculada fosse menor que 
# 1.96 o resultado seria o mesmo ao interpretar o teste. Mas, se a
# estatistica z calculada se situasse entre -1.96 e 1.96 (valor tabelado
# obtido pela funcao acima) aceitariamos H0, portanto poderiamos considerar
# que as medias seriam estatisticamente iguais, com 95% de confianca ou
# 5% de significancia


############## Teste para a diferenca entre duas medias (t) ###################
##################### para pequenas amostras ##################################

# Facamos de conta que a amostra da base "salarios" eh pequena (<100)

x <- salarios$husearns

y <- salarios$earns

t.test(x, y,
       alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)

# Quer dizer que a diferenca entre as medias estara entre US$ 208,04 e
# US$ 233,36, para as medias de US$453.54 e US$232.83 (dif. = 220.70),
# com 95% de confianca ou 5% de significancia

# Estatistica t = 34,175, com 9646 graus de liberdade

# Confrontamos esse valor com o valor tabelado da estatistica t para 
# 95% de confianca ou 5% de significancia (usamos isso para obter o
# valor tabelado, conforme comando abaixo)

qt(0.975, df=9646)

# Como a estistica t calculada (34.175) eh maior que a estatistica tabelada
# (1.96) rejeitamos a hipotese (H0) de que as medias sao iguais
# ou seja, de que a diferenca verdadeira entre as medias eh igual a zero.
# Ressalta-se que se o valor da estatistica t calculada fosse menor que 
# -1.96 o resultado seria o mesmo ao interpretar o teste. Mas, se a
# estatistica t calculada se situasse entre -1.96 e 1.96 (valor tabelado
# obtido pela funcao acima) aceitariamos H0, portanto poderiamos considerar
# que as medias seriam estatisticamente iguais, com 95% de confianca ou
# 5% de significancia


############# Teste de diferenca entre duas variancias (F) ####################

x <- salarios$husearns

y <- salarios$earns

var.test(x, y, alternative = "two.sided", conf.level = 0.95)

# A razao da diferenca eh de 2.388767 e a estatistica F = 2.3888 com 5633 graus 
# de liberdade no numerador e denominador. Devemos confrontar esse valor da
# estatistica F com o valor da tabela F, conforme funcao abaixo:

qf(0.95, 5633, 5633)

# como a estatistica F (2.3888) eh superior ao valor tabelado (1.04481) 
# consideramos que as variancias nao sao estatisticamente iguais. 
# Ressalta-se que se o valor calculado da estatistica F fosse inferior a
# (1/1.04481 = 0.95711) o resultado seria o mesmo. Mas se a estatistica 
# F calculada estivesse entre (0.95711 e 1.04481) poderiamos dizer que as
# variancias seriam estatisticamente iguais com 95% de confianca ou 
# 5% de significancia.


##################### Teste de independencia/equivalencia  #################### 
##################### ou igualdade entre duas amostras (t) ####################

# Pressuposto: as variancias devem ser estatisticamente iguais

# install.packages("ggpubr")

library(ggpubr)

X <- salarios$husearns

y <- salarios$earns

t.test(x, y, alternative = "two.sided", var.equal = TRUE)

# A media de husearns eh 453.5406 e de earns eh 232.833, e para o teste o 
# resultado apresentou que a media conjunta deve ficar entre 208.0486 e
# 233.3667.

# Estatistica t = 34.175, com 11266 graus de liberdade 

# Para fazer o teste devemos comparar o valor da estatistica t calculada 
# com a estatistica t tabelada, obtida com a funcao abaixo:

qt(0.975, df=11266)

# Como a estistica t calculada (34.175) eh maior que a estatistica tabelada
# (1.96) rejeitamos a hipotese (H0) de que as amostras sao similares
# ou seja, de que a diferenca verdadeira entre elas eh igual a zero.
# Ressalta-se que se o valor da estatistica t calculada fosse menor que 
# -1.96 o resultado seria o mesmo ao interpretar o teste. Mas, se a
# estatistica t calculada se situasse entre -1.96 e 1.96 (valor tabelado
# obtido pela funcao acima) aceitariamos H0, portanto poderiamos considerar
# que as amostras seriam estatisticamente similares/dependentes, com 95% de
# confianca ou 5% de significancia

################## Teste de Wilcoxon-Mann-Whitney ############################# 
##################  para amostras independentes   #############################

X <- salarios$husearns

y <- salarios$earns

options(scipen = 999)

wilcox.test(x, y, alternative = "two.sided") 

# Se no Resultado p-value <= 0.05; entao as amostras sao independentes
# como o resultado do p-value eh menor que 0.05, as amostras sao
# independentes (rejeitamos H0 de que as amostras sao identicas)


############### Teste de normalidade de Kolmogorov-Smirnov ####################

# install.packages("RcmdrMisc")

library(RcmdrMisc)

normalityTest(~earns, test="lillie.test", data=salarios)

normalityTest(~husearns, test="lillie.test", data=salarios)

# Valor critico de Kolmogorov-smirnov para nosso exemplo = 0.018133333
# Os valores calculados sao D=0.18829 para "earns" e D=0.13256 para husearns
# 
# H0: A amostra provem de uma populacao normalmente distribuida
# Ha: A amostra nao provem de uma populacao normalmente distribuida
# 
# como os valores "D" calculados sao maiores que o valor critico (tabelado) 
# estes se encontram na regiao de rejeicao de H0, ou seja rejeita-se a
# "normalidade"
# REGRA DE BOLSO: se P-value > 0.05, existe normalidade da amostra. 

################## Teste de normalidade de Shapiro-Wilk #######################

shapiro.test(salarios$earns[1:5000])

# Nao "deu", amostra maior que 5000 observacoes, temos de usar outro tipo de 
# teste

# Vamos usar a base de dados "ceo", soh para testar a rotina 

shapiro.test(ceo$sales)
hist(ceo$sales)

# REGRA DE BOLSO: se P-value >= 0.05, existe normalidade da amostra.

# Como o p-value do teste aplicado eh menor que 0.05 entao a amostra nao eh
# considerada como de distribuicao "normal"


################# Teste de normalidade de Anderson-Darling ####################

library(nortest)

ad.test(salarios$earns)

# Como p-value eh menor que 0.05 rejeita-se a hipotese de normalidade da 
# variavel


################# Teste de normalidade de Cramer-von Mises ####################

cvm.test(salarios$earns)

# p-value < 0.05 rejeita-se normalidade

hist(salarios$earns)
hist(salarios$age)
cvm.test(salarios$age)

################# Transformacao Box-Cox para uma variavel #####################

# install.packages('AID')
library(AID)
data(textile)
# X11(width = 10, height = 12)
out <- boxcoxnc(textile[,1], method = "sw")
out$lambda.hat #Estimate of Box-Cox param. based on Shapiro-Wilk test statistic
out$p.value # p.value of Shapiro-Wilk test for transformed data (p-value>0.05)
out$tf.data # transformed data set
confInt(out) # mean and confidence interval for back transformed data
#X11(width = 10, height = 12)
out2 <- boxcoxnc(textile[,1], method = "sf")
out2$lambda.hat #Estimate of Box-Cox param. based on Shapiro-Francia test stat.
out2$p.value # p.value of Shapiro-Francia test for transformed data
out2$tf.data
confInt(out2)


################ Transformacao Box-Cox para modelos lineares ##################

library(AID)
trees=as.matrix(trees)
#X11(width = 10, height = 12)
outlm <- boxcoxlm(x = trees[,1:2], y = trees[,3])
outlm$lambda.hat #Estimate of Box-Cox param. based on Shapiro-Wilk test stat.
outlm$p.value # p.value of Shapiro-Wilk test for transformed data (p-value>0.05)
outlm$tf.residuals # transformed data residuals
outlm$tf.data # transformed data for y variable

###################### Correlacao (Matriz de correlacao) ######################

cor(salarios[,c("earns","husearns","age")], use="complete")

# veremos mais adiante que o melhor eh o FIV - Fator de inflacao da variancia
# quando estamos tratando de modelos lineares por OLS


############################################################################### 
#                                   AULA 3                                    #
###############################################################################

############# REGRESSAO POR MQO (MINIMOS QUADRADOS ORDINARIOS) ################

# Carregar conjunto de dados

load("C:/iaa/imoveiscwbav.RData")

imoveis <- imoveiscwbav

# Estimando um modelo preliminar

resultados <- lm (price~., data=imoveis)
summary (resultados)

# Verificando a especificacao do modelo

# Em estudos de pre?os de imoveis eh comum utilizarmos uma funcao log-linear

# Vamos criar uma variavel transformada para "price" em logaritmo neperiano

imoveis$lnprice <- with (imoveis, log(price))  

# Vamos reestimar o modelo agora com a variavel "lnprice" ao inves de "price"

resultados <- lm (lnprice~.-price, data=imoveis)
summary (resultados)

################ Agora vamos verificar a presenca de outliers #################
################        pelo teste de Bonferroni              #################

library (carData)
library(car)

outlierTest(resultados)

#Resultado do teste de outliers
#rstudent unadjusted p-value Bonferroni p
#393  5.02293      0.00000070067   0.00037906

# Existe um outlier na linhas 393
# neste caso vc deve deletar a linha indicada na tabela de dados 
# Vamos editar o conjunto de dados (eliminacao da linha 393)

row.names.imoveis<-c(393)
imoveis <- imoveis[!(row.names(imoveis) %in% row.names.imoveis),]

# Depois deve-se refazer a estimativa dos resultados e depois refazer o 
# teste de outliers, se necessario deve-se extrair novas observacoes e 
# recalcular o modelo e o teste de outlier

resultados <- lm (lnprice~.-price, data=imoveis)
summary (resultados)

outlierTest(resultados)

#rstudent unadjusted p-value Bonferroni p
#364 -4.005752        0.000070857     0.038263

# Ainda temos um outlier nas linhas 364, vamos retira-los

row.names.imoveis<-c(364)
imoveis <- imoveis[!(row.names(imoveis) %in% row.names.imoveis),]

# Vamos reestimar novamente apos deletar essa linha 

resultados <- lm (lnprice~.-price, data=imoveis)
summary (resultados)

outlierTest(resultados)

#No Studentized residuals with Bonferroni p < 0.05
#Largest |rstudent|:
#  rstudent unadjusted p-value Bonferroni p
#362 -3.632233         0.00030895      0.16652

# Nao temos mais outliers


############## Agora vamos estimar uma regressao stepwise #####################
##### para verificar quais variaveis devem efetivamente entrar no modelo ######
library(RcmdrMisc)

stepwise(resultados, direction= 'backward/forward', criterion ='AIC')

# A regressao setpwise indicou que devem permanecer no modelo as seguintes
# variaveis dependentes: age, parea, tarea, bath, ensuit, garag, plaz, park,
# trans, balc, elev, fitg, party, categ

# Vamos reestimar o modelo somente com essas variaveis

resultados <- lm(lnprice~age+parea+tarea+bath+ensuit+garag+plaz+park+trans+
                    balc+elev+fitg+party+categ, data=imoveis)
summary (resultados)

# Agora temos todas as variaveis estatisticamente significativas, excesao das
# variaveis "trans" e "plaz", que s?o significativas apenas a 80% de confianca,
# mas ainda devemos fazer o ajuste de heterocedasticidade e essas variaveis
# podem se tornar significativas

########################## Verificando a multicolinearidade ###################

# Vamos verificar a existencia de multicolineariedade, que ? verificar alta
# correlacao entre as variaveis, vamos fazer isso com o Fator de Inflacao
# da variancia, que eh uma medida da correlacao multipla das variaveis e
# sua influencia no R2

# install.packages("car")
library(car)

car::vif(resultados)

#    age    parea    tarea     bath   ensuit    garag     plaz     park    
#1.679947 4.074078 3.681996 3.046737 2.866718 2.152874 1.126224 1.608973 
#    trans  balc     elev     fitg    party    categ
# 1.479620 1.540686 1.396057 1.720227 2.040543 1.263249

# Aqui a regra nao eh unica, muitos pesquisadores usam valores de corte 
# diferentes, alguns usam valores acima de 4, outros acima de 5, outros acima
# de 10; eu uso valores acima de 5, que eh o que a maioria usa

# Portanto, nao existe nenhuma variavel com VIF acima de 5, entao n?o existe
# nenhuma variavel que retirar do modelo


########################## Teste de especificacao do modelo ##################

# Agora vamos fazer o teste RESET de especificacao do modelo

# install.packages("zoo")
# install.packages("lmtest")

library (zoo)
library (lmtest)

resettest(lnprice~age+parea+tarea+bath+ensuit+garag+plaz+park+trans+
          balc+elev+fitg+party+categ, power=2:3, type="regressor", data=imoveis)

# H0 = o modelo esta corretamente especificado; 
# HA = o modelo esta incorretamente especificado;

# Resultado do teste
# RESET test

# data:  lnprice ~ age + parea + tarea + bath + ensuit + garag + plaz + park + 
#                 trans + balc + elev + fitg + party + categ
#RESET = 3.3374, df1 = 28, df2 = 496, p-value = 0.00000004334

# qual o valor tabelado para compararmos com a estatistica RESET?

qf(.95,df1=28,df2=496)

# F tabelado = 1,499117

# Como o F calculado (3,3374) eh maior que o F tabelado (1,50) 
# existe algum erro de especificacao do modelo, que pode ter origem na
# heterocedasticidade, uma vez que nao existe motivo para que nao seja
# linear a relacao das variaveis explicativas com "lnprice"

# Em termos praticos 3.33 n?o esta muito distante de 1.50, haja vista que com
# erros severos de especificacao, a estatistica RESET pode superar o valor de
# 100. Infelizmente nao temos como fazer o teste RESET apos o ajuste de 
# heterocedasticidade


################## Verificando a autocorrelacao dos residuos ##################

# Verificando a autocorrelacao dos residuos - empregavel somente para dados em
# series temporais, vamos fazer aqui soh para exemplificar para que voce possa
# fazer quando a sua amostra for de dados em series temporais

library(lmtest)

dwtest(lnprice~age+parea+tarea+bath+ensuit+garag+plaz+park+trans+
         balc+elev+fitg+party+categ, alternative="greater", data=imoveis)

# Durbin-Watson test

#data:  lnprice ~ age + parea + tarea + bath + ensuit + garag + plaz +
#                 park + trans + balc + elev + fitg + party + categ
#DW = 1.9661, p-value = 0.3191
# alternative hypothesis: true autocorrelation is greater than 0

# TABELA DE ANALISE
# HIPOTESE NULA              DECISAO             SE
# nao existe autocorr +      rejeitar           0 < d < dl
# nao existe autocorr +      sem decisao        dl <= d <= du
# nao existe autocorr -      rejeitar           4 - dl < d < 4
# nao existe autocorr -      sem decisao        4 - du <= d <= 4 - dl
# nenhuma autocorr + ou -    nao rejeitar       du < d < 4-du ***

# k = 14,  n = 539 
# dl = 1,528, du = 1,824 (tabela no livro do Gujarati)

# 4 - dl = 2,472
# 4 - du = 2,176

# 1.9661 esta entre 1,824 e 2,176, ultima linha da tabela de analise, portanto
# nao existe autocorrelacao nos residuos

# caso exista autocorrelacao positiva ou negativa utilizar o metodo da primeira
# diferenca exposto na secao 12.9 "metodo da primeira diferenca" do livro do
# Gujarati e Porter
# Alternativamente, pode-se utilizar o estimador sandwich "HAC"

######################## Verificando a heterocedasticidade ####################

# Verificando a homocedasticidade = variancia constante (amostra homogenea?)

# para grandes amostras - mais de 100 obs - utilizar o teste de Breusch-Pagan, 
# caso contrario utilizar o teste de Goldfeld-Quandt (gqtest) descrito na 
# secao 11.5 do livro de Gujarati e Porter

bptest(lnprice~age+parea+tarea+bath+ensuit+garag+plaz+park+trans+
         balc+elev+fitg+party+categ, studentize=FALSE, data=imoveis)

# H0: homocedastico (variancias constantes); 
# HA: Heterocedatico (variancias nao constantes)

# Breusch-Pagan test

#  data:  lnprice~age+parea+tarea+bath+ensuit+garag+plaz+park+trans+balc+elev+
#                 fitg+party+categ
# BP = 59.988, df = 14, p-value = 0.0000001179

qchisq(0.95, df=14)

# O resultado deve ser confrontado em um teste qui-quadrado com k-1 graus de
# liberdade. Como o resultado do teste BP (59.988) eh maior que o tabelado 
# (23,68478) rejeita-se a hipotese de homocedasticidade 


################### Correcao da variancia nao constante ####################### 
###################      por regressao robusta          #######################

library (lmtest)
library (sandwich)

resultados <- lm(lnprice~age+parea+tarea+bath+ensuit+garag+plaz+park+trans+
                   balc+elev+fitg+party+categ, data=imoveis)
summary(resultados)

coeftest(resultados, vcov=vcovHC(resultados, type="HC1"))

# Analisando os resultados vemos que as variaveis "plaz" e "trans", nao sao
# significativas a 95% de confianca, portanto vamos retira-las do modelo, e 
# reestimar o modelo e o teste de homocedasticidade

resultados <- lm(lnprice~age+parea+tarea+bath+ensuit+garag+park+
                   balc+elev+fitg+party+categ, data=imoveis)
summary(resultados)

bptest(lnprice~age+parea+tarea+bath+ensuit+garag+park+
         balc+elev+fitg+party+categ, studentize=FALSE, data=imoveis)

# Breusch-Pagan test
# data:  lnprice ~ age + parea + tarea + bath + ensuit + garag + park + 
#                  balc + elev + fitg + party + categ
# BP = 55.195, df = 12, p-value = 0.000000167

# Melhorou a heterocedasticidade, o valor BP esta mais proximo de 23,68

# Vamos novamente corrigir a heterocedasticidade por regressao linear robusta
# (sandwich)

resultados1 <- coeftest(resultados, vcov=vcovHC(resultados, type="HC1"))
resultados1

# Agora todas as variaveis sao siginifcativas a 95% de confianca


################ Calculando os intervalos de confianca ########################
################    Para regressao linear robusta      ########################

confint <- confint (resultados1, level = 0.95)
confint

########### Analisando indicadores de performace do modelo final ##############

#install.packages("performace")
library(performance)

# Menores valores de AIC, BIC, Sigma e RMSE sao desejaveis
# Maiores valores de R2 e R2(adj) sao melhores
# Sigma eh o Erro Padrao dos Residuos
# RMSE eh a raiz quadrada media dos erros (menor eh melhor)

model_performance(resultados1)
model_performance(resultados)

# Mas onde estao os residuos??
resultados$residuals
realestate<- within(imoveis, {residuos <- residuals(resultados) })

# Amostra eh grande nao precisa testar normalidade dos residuos

# Mas onde estao os valores preditos (Y chapeu)

resultados$fitted.values
realestate1<- within(realestate, {preditos <- fitted.values(resultados)})


################### Fazendo predicoes no modelos OLS ########################## 

resultados

#Para:
age = 14 # anos
parea = 120 # (metros quadrados de area privativa)
tarea = 180 # (metros quadrados de area total)
bath = 2 # (banheiros)
ensuit = 1 # (tem uma suite)
garag = 1  # (tem uma garagem)
park = 2 # (distancia em km de um parque)
balc = 1 # (vari?vel binaria, significa que tem sacada)
elev = 1 # (vari?vel binaria, significa que tem elevador)
fitg = 1 # (significa que tem area fitness no condominio)
party = 1 # (significa que tem area de festas no condominio)
categ = 1 # (significa que eh um apartamento; zero significa casa)

predito <- predict(object = resultados,
        data.frame(age=age, parea=parea, tarea=tarea, bath=bath,
                   ensuit=ensuit, garag=garag, park=park, balc=balc,
                   elev=elev, fitg=fitg, party=party, categ=categ))

#o preco do im?vel estimado eh:
predmedfinal <- exp(predito)
predmedfinal
# R$712.817,00

# Estimando o preco inferior do intervalo de confianca

Lestimate=confint[1,1]+age*confint[2,1]+parea*confint[3,1]+tarea*confint[4,1]+
          bath*confint[5,1]+ensuit*confint[6,1]+garag*confint[7,1]+
          park*confint[8,1]+balc*confint[9,1]+elev*confint[10,1]+
          fitg*confint[11,1]+party*confint[12,1]+categ*confint[13,1]  
Lestimate
exp(Lestimate)
# R$332.242,80

# Estimando o preco superior do intervalo de confianca

Uestimate=confint[1,2]+age*confint[2,2]+parea*confint[3,2]+tarea*confint[4,2]+
  bath*confint[5,2]+ensuit*confint[6,2]+garag*confint[7,2]+
  park*confint[8,2]+balc*confint[9,2]+elev*confint[10,2]+
  fitg*confint[11,2]+party*confint[12,2]+categ*confint[13,2]  
Uestimate
exp(Uestimate)
# R$1.529.317,00

# Entao o preco estimado eh R$712.817,00, e pode variar entre R$332.242,80 e
# R$1.529.317,00

# O intervalo eh muito amplo; se pensarmos que o valor estimado R$712.817,00 eh
# o valor medio, ja que o metodo OLS eh a estimativa pela media; entao podemos
# estimar o intervalo de confianca para a media, como vimos anteriormente

n <- nrow(imoveis)
m <- predmedfinal
s <- sd(imoveis$price)
dam <- s/sqrt(n)
CIlwr <- m + (qnorm(0.025))*dam
CIupr <- m - (qnorm(0.025))*dam 

CIlwr
# R$670.822,50
CIupr
# R$754.806,80

# Entao agora temos que a media eh R$712.817,00 e pode variar entre R$670.822,50
# e R$754.806,80 

############################ Estimando A ANOVA ################################

anova(resultados)

############################## Fim da aula 3  ################################# 