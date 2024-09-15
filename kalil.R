library(readxl)

setwd(paste("/home/kalil/Documents/Graduacao/UFRJ/",
            "MicroEconometria", sep = "/"))

dados <- read_excel("./DDMv2.xls")

group_counts <- table(dados$GRUPO)
group_counts
print(group_counts)

dados$idade <- as.numeric(dados$idade)
dados$renda_pc <- as.numeric(dados$renda_pc)
dados$escolaridade <- as.numeric(dados$escolaridade)

dados_3 <- subset(dados, GRUPO == 3)
dados_3
print(summary(dados_3))

# No grupo 3, escolhido, 2004 mulheres participaram da pesquisa
# A renda familiar per capita média foi de 443.1

tempo0 <- subset(dados_3, dados_3$tempo == 0)
tempo1 <- subset(dados_3, dados_3$tempo == 1)

escolaridade <- lm(escolaridade ~ tratado, data = tempo0)
print('Escolaridade no tempo 0')
print(summary(escolaridade))

idade <- lm(idade ~ tratado, data = tempo0)
print('Idade no tempo 0')
print(summary(idade))

luz <- lm(luz ~ tratado, data = tempo0)
print('Luz no tempo 0')
print(summary(luz))

renda_pc <- lm(renda_pc ~ tratado, data = tempo0)
print('Renda per capita no tempo 0')
print(summary(renda_pc))

# O parâmetro que diferencia o grupo tratado do grupo de controle é pequeno em relação ao intercepto
# Isso indica que há pouca diferença entre os grupos, o que é um bom sinal

tratado0 <- subset(dados_3, dados_3$tratado == 0)
tratado1 <- subset(dados_3, dados_3$tratado == 1)

impacto <- lm(renda_pc ~ tempo, data = tratado1)
print('Impacto do tratamento')
print(summary(impacto))

# O parâmetro de tempo é significativo, indicando que o tratamento teve impacto na renda per capita

impacto <- lm(renda_pc ~ tratado, data = tempo1)
print('Impacto do tratamento')
print(summary(impacto))

# O parâmetro de tratado é significativo, indicando que o tratamento teve impacto na renda per capita

impacto <- lm(renda_pc ~ tratado + idade + luz + escolaridade, data = tempo1)
print('Impacto do tratamento')
print(summary(impacto))