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

# sws