# Avaliação 1 - Estatística Aplicada 1

# Com a base de dados “imoveiscwbav” obter os seguintes resultados com o auxílio do “R”
# 
# a)	Elaborar o histograma, scaterplot e o boxplot das variáveis “price e parea”.
# b)	Elaborar a tabela de distribuição de frequências da variável “price” (preço dos imóveis);
# c)	Para a variável “price” calcular os seguintes indicadores: média; mediana; moda
# 
# Apresentar esses resultados em um documento pdf. Não é para postar a rotina, mas sim a saída (resultados), fazer a interpretação dos resultados.
# Fazer upload do documento pdf no “ufprvirtual”, na tarefa aberta no Tópico 1.

load("imoveiscwbav.RData")

# a)	Elaborar o histograma, scaterplot e o boxplot das variáveis “price e parea”.
library (lattice)
xyplot(price ~ parea, pch=16, data=imoveiscwbav)

library (car)
Boxplot( ~ price, data=imoveiscwbav, id=list(method="y"))
hist(imoveiscwbav$price)
Boxplot( ~ parea, data=imoveiscwbav, id=list(method="y"))
hist(imoveiscwbav$parea)

# b)	Elaborar a tabela de distribuição de frequências da variável “price” (preço dos imóveis);
library (fdth)
table <- fdt(imoveiscwbav$price)
print (table)
dataframe_table <- data.frame(table$table)
write.csv2(dataframe_table, "tabela_df_price.csv")

# c)	Para a variável “price” calcular os seguintes indicadores: média; mediana; moda
cat("\n")

maxPrice <- max(imoveiscwbav$price)
cat("Max preço dos imóveis:", maxPrice, "\n")

minPrice <- min(imoveiscwbav$price)
cat("Min preço dos imóveis:", minPrice, "\n")

mediaPrice <- mean(imoveiscwbav$price)
cat("Média preço dos imóveis:", mediaPrice, "\n")

medianaPrice <- median(imoveiscwbav$price)
cat("Mediana preço dos imóveis:", medianaPrice, "\n")

precoImoveis <- imoveiscwbav$price
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
modaImoveis <- getmode(precoImoveis)
cat("Moda preço dos imóveis:", medianaPrice, "\n")