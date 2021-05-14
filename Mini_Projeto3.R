#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#         Mini Projeto 3
#
#
#       Prevendo a inadimplencia de clientes com Machine Learneing e Power BI
#
#
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Definindo o problema

#Instalando pacotes
install.packages("Amelia")
install.packages("caret")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("reshape")
install.packages("randomForest")
install.packages("e1071")
install.packages("devtools")

#Carregando bibliotecas
library(Amelia)
library(ggplot2)
library(caret)
library(reshape)
library(randomForest)
library(dplyr)
library(e1071)

#Carregando o dataset
dados_clientes <- read.csv("D:/Documentos/Analista de Dados/PowerBI/CAP15/dados/dataset.csv")

# Visualizando os dados
View(dados_clientes)
dim(dados_clientes)
str(dados_clientes)
summary(dados_clientes)


######### Analise Explorativa, Limpeza e Transformação ##########################

# Removendo primeir coluna: ID
dados_clientes$ID <- NULL
dim(dados_clientes)
View(dados_clientes)

# Renomenado coluna de clase
colnames(dados_clientes)
colnames(dados_clientes)[24] <- "Inadimplentes"
View(dados_clientes)

# Verificando valores ausentes
sapply(dados_clientes, function(x) sum(is.na(x)))
missmap(dados_clientes, main = "Valores Missing Observados")
dados_clientes <- na.omit(dados_clientes)# Apagar dados nulos

# Convertendo os atributos genero, escolaridade, estado civil e idade
#para fatores(categorias)

#renomenado colunas categoricas
colnames(dados_clientes)
colnames(dados_clientes)[2] <- "Genero"
colnames(dados_clientes)[3] <- "Escolaridade"
colnames(dados_clientes)[4] <- "Estado_Civil"
colnames(dados_clientes)[5] <- "Idade"
colnames(dados_clientes)
View(dados_clientes)

# Genero
View(dados_clientes$Genero)
str(dados_clientes$Genero)
summary(dados_clientes$Genero)
?cut
dados_clientes$Genero <- cut(dados_clientes$Genero,
                             c(0,1,2),
                             labels = c("Masculino",
                                        "Feminino"))
View(dados_clientes$Genero)
str(dados_clientes$Genero)
summary(dados_clientes$Genero)

# Escolaridade
View(dados_clientes$Escolaridade)
str(dados_clientes$Escolaridade)
summary(dados_clientes$Escolaridade)
?cut
dados_clientes$Escolaridade <- cut(dados_clientes$Escolaridade,
                             c(0,1,2,3,4),
                             labels = c("Pós-Graduado",
                                        "Graduado",
                                        "Médio",
                                        "Outros"))
View(dados_clientes$Escolaridade)
str(dados_clientes$Escolaridade)
summary(dados_clientes$Escolaridade)

# Estado Civil
View(dados_clientes$Estado_Civil)
str(dados_clientes$Estado_Civil)
summary(dados_clientes$Estado_Civil)
dados_clientes$Estado_Civil <- cut(dados_clientes$Estado_Civil,
                                   c(-1,0,1,2,3),
                                   labels = c("Desconhecido",
                                              "Casado",
                                              "Solteiro",
                                              "Outros"))
View(dados_clientes$Estado_Civil)
str(dados_clientes$Estado_Civil)
summary(dados_clientes$Estado_Civil)

# Convertendo a variavel para o tipo de faixa etaria
str(dados_clientes$Idade)
summary(dados_clientes$Idade)
hist(dados_clientes$Idade)

dados_clientes$Idade <- cut(dados_clientes$Idade,
                                   c(0,30,50,100),
                                   labels = c("Jovem",
                                              "Adulto",
                                              "Idoso"))
View(dados_clientes$Idade)
str(dados_clientes$Idade)
summary(dados_clientes$Idade)
View(dados_clientes)

# Converter a variavel que indica pagamentos para o tipo fator
dados_clientes$PAY_0 <- as.factor(dados_clientes$PAY_0)
dados_clientes$PAY_1 <- as.factor(dados_clientes$PAY_1)
dados_clientes$PAY_2 <- as.factor(dados_clientes$PAY_2)
dados_clientes$PAY_3 <- as.factor(dados_clientes$PAY_3)
dados_clientes$PAY_4 <- as.factor(dados_clientes$PAY_4)
dados_clientes$PAY_5 <- as.factor(dados_clientes$PAY_5)
dados_clientes$PAY_6 <- as.factor(dados_clientes$PAY_6)

# Dataset apos conversão
str(dados_clientes)
sapply(dados_clientes, function(x) sum(is.na(x)))
missmap(dados_clientes,main = "Valores Missing Observate")
dados_clientes<- na.omit(dados_clientes)
missmap(dados_clientes,main = "Valores Missing Observate")
dim(dados_clientes)
View(dados_clientes)

# Alterando a variavel dependente para o tipo de fator
str(dados_clientes$Inadimplentes)
colnames(dados_clientes)
dados_clientes$Inadimplentes<- as.factor(dados_clientes$Inadimplentes)
str(dados_clientes$Inadimplentes)
View(dados_clientes)

# Total de inadimplentes versus não -inadimplentes
?table
table(dados_clientes$Inadimplentes)

# Ver as porcentagens entre as classes
prop.table(table(dados_clientes$Inadimplentes))*100

# Plot da distribuição usando ggplot2
qplot(Inadimplentes,data = dados_clientes, geom = "bar")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1) )

# Set seed
set.seed(12345)

# Amostragem estratificada
# Seleciona as linhas de acordo com a variavel inadimplentes como strata
?createDataPartition
indice <- createDataPartition(dados_clientes$Inadimplentes, p= 0.75, list = FALSE)
dim(indice)

#Definido os dados de treinamento como subconjunto de dados original
#com numeros de indice de linha(Indentificado acima) e todas colunas
dados_treinoclientes <- dados_clientes[indice,]
dim(dados_treinoclientes)
table(dados_treinoclientes$Inadimplentes)

#Vendo as porcentagens das classes
prop.table(table(dados_treinoclientes$Inadimplentes))

#Comparar as porcentagens entre as classes de treinamento e dados originais
compara_dado <- cbind(prop.table(table(dados_treinoclientes$Inadimplentes)),
                      prop.table(table(dados_clientes$Inadimplentes)))
colnames(compara_dado) <- c("Treinamento", "Original")
compara_dado

# Melt Data - Converte colunas em linhas
melt_compara_dado <- melt(compara_dado)
melt_compara_dado

#Plot para ver a distribuição do treinamento vs original
ggplot(melt_compara_dado, aes(x= X1, y= value)) + 
  geom_bar( aes(fill = X2), stat = "identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Filtrando os dados de teste, tudo que não esta no dataset de treinamento 
#Observando o sinal de (-)
dados_teste <- dados_clientes[-indice,]
dim(dados_teste)
dim(dados_treinoclientes)

#############MODELO MACHINE LEARNING####################

#Construindo a primeira versão de modelo
?randomForest
#                         variavel alvo,função,variaveis preditoras
modelo_v1 <- randomForest(Inadimplentes ~ ., data = dados_treinoclientes)
modelo_v1

#Avaliar o modelo
plot(modelo_v1)

#Previsões com dados testes
previsoes_v1 <- predict(modelo_v1, dados_teste)

#Confusion Matrix
?caret :: confusionMatrix
cm_v1 <- caret::confusionMatrix(previsoes_v1,dados_teste$Inadimplentes, positive ="1")
cm_v1

#Balanceamento de classe

library("DMwR")


library(DMwR)
?SMOTE

#Aolicando o SMOTE - SMOTE: Synthetic Minority Over-sampling Technique
#https://arxiv.org/pdf/1106.1813.pdf
table(dados_treinoclientes$Inadimplentes)
prop.table(table(dados_treinoclientes$Inadimplentes))
set.seed(9560)
dados_treino_bal <- SMOTE(Inadimplentes ~., data = dados_treinoclientes)
table(dados_treino_bal$Inadimplentes)
prop.table(table(dados_treino_bal$Inadimplentes))

#Construindo a segunda versão do modelo
modelo_v2 <- randomForest(Inadimplentes ~., data = dados_treino_bal)
modelo_v2



# Avaliando o modelo
plot(modelo_v2)

# Previsões com dados de testes
previsoes_v2 <- predict(modelo_v2, dados_teste)

# Confusion matrix
cm_v2 <- caret::confusionMatrix(previsoes_v2, dados_teste$Inadimplentes, positive = "1")
cm_v2

# Calculando Precision, Recall e F1-Score, métricas de avaliação do modelo preditivo
y <- dados_teste$Inadimplentes
y_pred_v2 <- previsoes_v2

precision <- posPredValue(y_pred_v2, y)
precision

recall <- sensitivity(y_pred_v2, y)
recall

F1 <- (2 * precision * recall) / (precision + recall)
F1







































