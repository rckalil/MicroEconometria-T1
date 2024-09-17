library(readxl)

# setwd(paste("/home/kalil/Documents/Graduacao/UFRJ/",
#             "MicroEconometria", sep = "/"))

dados <- read_excel("./DDMv2.xls")

group_counts <- table(dados$GRUPO)
group_counts
print(group_counts)

dados$idade <- as.numeric(dados$idade)
dados$renda_pc <- as.numeric(dados$renda_pc)
dados$escolaridade <- as.numeric(dados$escolaridade)
dados$luz <- as.numeric(dados$luz)

#Grupo de interesse
dados_3 <- subset(dados, GRUPO == 3)

#A)Estatísticas Descritivas, quantidade de Mulheres no Programa (no caso, t0) e renda média per capita
# #Length é o n da amostra, ou seja, n° de mulheres que participaram do programa
# #(Length: 2004)
summary(dados_3)
# #Desvio padrão
sapply(dados[, c("idade", "escolaridade", "renda_pc", "luz", "conjuge")], sd, na.rm = TRUE)
# #Média de renda per capita
print(mean(dados$renda_pc, na.rm = TRUE))

#B)Testes de Hipótese para Balanceamento de Características
# #Testes de hipóteses para verificar diferenças entre tratados e não tratados em t0. Significância 0.05
tempo0 <- subset(dados_3, dados_3$tempo == 0)
tempo1 <- subset(dados_3, dados_3$tempo == 1)
# #Teste t de tais diferenças possui como H0: diferenças entre as médias = 0.
# ## Escolaridade
t.test(escolaridade ~ tratado, data = tempo0) #P-valor 0,1018 não rejeita H0

# ## Idade
t.test(idade ~ tratado, data = tempo0)#P-valor 0,1018 rejeita H0

# ## Luz elétrica (teste qui-quadrado)
tabela_luz <- table(dados_3$luz, dados_3$tratado)
teste_qui2 <- chisq.test(tabela_luz)
print(teste_qui2) #P-valor bastante alto, não podendo rejeitar a H0 de que
# proporções de pessoas com luz elétrica são iguais entre os grupos tratados e controle.

# ## Cônjuge
tabela_conjuge <- table(dados_3$conjuge, dados_3$tratado)
teste_qui2 <- chisq.test(tabela_conjuge)
print(teste_qui2)

# ## Renda familiar per capita
t.test(renda_pc ~ tratado, data = tempo0) #P-valor abaixo de 0.01052, rejeita H0

#Em alguns aspectos, grupos parecem balanceados. Entretanto, ressalta-se as evidências
# de desbalanceamento se tratando das variaveis de idade e renda familiar.

#C)Impacto entre os tratados antes e depois (Feito um modelo DiD para simplificar)
impacto <- lm(renda_pc ~ tempo, data = tratado1)
print("Impacto do tratamento")
print(summary(impacto)) #O impacto estimado é 171.46, sendo bastante significativo

#D)Impacto na população usando dados do t1
impacto <- lm(renda_pc ~ tratado, data = tempo1)
print("Impacto do tratamento")
print(summary(impacto))

#E)Impacto na população usando dados do t1 (com variáveis de controle)
impacto <- lm(renda_pc ~ tratado + idade + luz + escolaridade, data = tempo1)
print("Impacto do tratamento")
print(summary(impacto))

#F)Tabela de médias de renda per capita e cálculo do efeito causal via Diferença em Diferenças
renda_media_antes <- aggregate(renda_pc ~ tratado + tempo, data = subset(dados, tempo == 0), FUN = mean)
renda_media_depois <- aggregate(renda_pc ~ tratado + tempo, data = subset(dados, tempo == 1), FUN = mean)

# Criando a tabela de médias
tabela_medias <- rbind(renda_media_antes, renda_media_depois)
print(tabela_medias)

#Efeito causal
diff <- mean(tratado1_tempo1$renda_pc) - mean(tratado1_tempo0$renda_pc) - mean(tratado0_tempo1$renda_pc) + mean(tratado0_tempo0$renda_pc)
print(diff)

#G)Modelo DiD (sintetiza DiD em uma regressão, sendo o termo de interação em parentesis a expressão do efeito causal ):
diff_diff <- lm(renda_pc ~ tratado + tempo + (tratado * tempo), data = dados_3)
print("Diferença das diferenças")
print(summary(diff_diff))

#H)Estime um modelo DiD com efeitos fixos para testar a significância do ATT estimado
install.packages("plm")
library(plm)
painel <- pdata.frame(dados, index = c("ID", "tempo"))

# Estimando o modelo DiD com efeitos fixos
modelo_did_efeitos_fixos <- plm(renda_pc ~ tratado * tempo + idade + escolaridade + luz + conjuge, 
                                data = painel, model = "within")
summary(modelo_did_efeitos_fixos)

# Modelo de efeitos fixos tem valor de 126.6027 na variável de interação. Resultado bastante
# significativo de impacto causal.


# ==============
# # Renda familiar per capita média
# renda_media <- mean(dados$renda_pc, na.rm = TRUE)
# cat("Renda familiar per capita média:", renda_media, "\n")

# escolaridade <- lm(escolaridade ~ tratado, data = tempo0)
# print("Escolaridade no tempo 0")
# print(summary(escolaridade))

# idade <- lm(idade ~ tratado, data = tempo0)
# print("Idade no tempo 0")
# print(summary(idade))

# luz <- lm(luz ~ tratado, data = tempo0)
# print("Luz no tempo 0")
# print(summary(luz))

# renda_pc <- lm(renda_pc ~ tratado, data = tempo0)
# print("Renda per capita no tempo 0")
# print(summary(renda_pc))

# # O parâmetro que diferencia o grupo tratado do grupo de controle
# # é pequeno em relação ao intercepto
# # Isso indica que há pouca diferença entre os grupos, o que é um bom sinal

# tratado0 <- subset(dados_3, dados_3$tratado == 0)
# tratado1 <- subset(dados_3, dados_3$tratado == 1)



# # O parâmetro de tempo é significativo, indicando que o tratamento
# #teve impacto na renda per capita


# # O parâmetro de tratado é significativo, indicando que o tratamento teve
# #impacto na renda per capita


# # O parâmetro de tratado é significativo, indicando que o tratamento teve
# # impacto na renda per capita
# # Assim, a diferença nele não é explicada por diferenças nas outras variáveis

# # Diferença das diferenças


