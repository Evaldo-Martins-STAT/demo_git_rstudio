# Instalar pacotes necessários, caso ainda não estejam instalados
if (!require("ggplot2")) {
  install.packages("ggplot2")
}

# Carregar pacotes necessários
library(ggplot2)

# Dados de exemplo: comprimento e peso dos peixes
comprimento <- c(10, 12, 14, 16, 18, 20, 22, 24, 26, 28)
peso <- c(30, 50, 70, 100, 140, 190, 250, 320, 400, 500)

# Transformar os dados para realizar a regressão linear
log_comprimento <- log10(comprimento)
log_peso <- log10(peso)

# Realizar a regressão linear
modelo_linear <- lm(log_peso ~ log_comprimento)

# Extrair coeficientes do modelo
coeficientes <- coef(modelo_linear)
a <- coeficientes[1]
b <- coeficientes[2]

# Função para prever o peso do peixe com base no comprimento
prever_peso <- function(x) {
  10^(a + b * log10(x))
}

# Testar a função com um valor de exemplo (ex: comprimento = 15 cm)
comprimento_teste <- 15
peso_previsto <- prever_peso(comprimento_teste)
cat("Peso previsto para um peixe com comprimento", comprimento_teste, "cm:", peso_previsto, "g\n")

# Visualizar os dados e o modelo de potência ajustado
dados <- data.frame(comprimento, peso)
ggplot(dados, aes(x = comprimento, y = peso)) +
  geom_point() +
  stat_function(fun = prever_peso, geom = "line", color = "blue", size = 1) +
  labs(title = "Regressão de Modelo de Potência para Crescimento de Peixes",
       x = "Comprimento (cm)",
       y = "Peso (g)") +
  theme_minimal()
