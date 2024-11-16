# Carregadar as bibliotecas necessárias no R

library(caret)
library(car)
library(dplyr)
library(ggplot2)
library(pROC)
library(psych)

#------------------------------------------------------------------------------#

# Carregar a base de dados (no caso do TCC em formato CSV)

dados <- read.csv('C:/Users/Francielle/Desktop/GitHub/base_contas_ethereum_original.csv')
describe(dados)

# Verificar quais colunas têm todos os valores iguais a 0
colunas_com_zero <- sapply(dados, function(x) all(x == 0))

# Exibir as colunas que têm todos os valores iguais a 0
colunas_com_zero[colunas_com_zero == TRUE]

# Contar o número de NA em cada coluna
soma_na <- sapply(dados, function(x) sum(is.na(x)))
print(soma_na)

# Resumo com variáveis com NA em relação a variável FLAG

resumo <- dados %>%
  group_by(FLAG) %>%
  summarise(
    total = n(),
    NAs = sum(is.na(ERC20_uniq_rec_addr)),
    percent_NAs = mean(is.na(ERC20_uniq_rec_addr)) * 100
  )
print(resumo)

# Excluir variáveis com elevada quantidade de valores ausentes 

dados_prep <- subset(dados, select = -c(Index,
                                        Address,
                                        min_value_sent_to_contract,
                                        max_val_sent_to_contract,
                                        avg_value_sent_to_contract,
                                        total_ether_sent_contracts,
                                        ERC20_most_sent_token_type, 
                                        ERC20_most_rec_token_type,
                                        Total_ERC20_tnxs,
                                        ERC20_total_Ether_received,                                                      ERC20_total_ether_sent,
                                        ERC20_total_Ether_sent_contract,
                                        ERC20_uniq_sent_addr,
                                        ERC20_uniq_rec_addr, 
                                        ERC20_uniq_sent_addr.1, 
                                        ERC20_uniq_rec_contract_addr,                                                    ERC20_avg_time_between_sent_tnx, 
                                        ERC20_avg_time_between_rec_tnx,                                                  ERC20_avg_time_between_rec_2_tnx,
                                        ERC20_avg_time_between_contract_tnx,                                             ERC20_min_val_rec, 
                                        ERC20_min_val_sent,
                                        ERC20_max_val_sent, 
                                        ERC20_avg_val_sent, 
                                        ERC20_min_val_sent_contract,                                                     ERC20_max_val_sent_contract,
                                        ERC20_avg_val_sent_contract,                                                     ERC20_uniq_sent_token_name, 
                                        ERC20_uniq_rec_token_name,
                                        ERC20_max_val_rec, 
                                        ERC20_avg_val_rec))

#------------------------------------------------------------------------------#

# Avaliação da Correlação de Variáveis

correlacao <- cor(dados_prep)

# Filtrar correlações maiores que 0,9 ou menores que -0,9

correlacao_filtrada <- correlacao
correlacao_filtrada[abs(correlacao) <= 0.9] <- NA  

# Remover a diagonal (correlação de uma variável consigo mesma)

diag(correlacao_filtrada) <- NA
print(correlacao_filtrada)

#------------------------------------------------------------------------------#

# Converter a variável dependente binária (FLAG) para fator

dados_prep$FLAG <- as.factor(dados_prep$FLAG)

# Dividir os dados em conjunto de treino (70%) e teste (30%)

set.seed(123)  
treino_index <- createDataPartition(dados_prep$FLAG, p = 0.7, list = FALSE)
treino_dados <- dados_prep[treino_index, ]
teste_dados <- dados_prep[-treino_index, ]

#------------------------------------------------------------------------------#

# Construir o Modelo de Regressão Logística Binária

modelo_logistico <- glm(FLAG ~ ., family = binomial, data = treino_dados)

AIC(modelo_logistico)

summary(modelo_logistico)

#------------------------------------------------------------------------------#

modelo_step <- step(modelo_logistico, direction = "both")

AIC(modelo_step)

summary(modelo_step)

vif(modelo_step)

modelo_logistico <- modelo_step

#------------------------------------------------------------------------------#

modelo_logistico_ref <- glm(FLAG ~ Avg_min_between_sent_tnx + 
                            Time_Diff_between_first_and_last_.Mins. + 
                            Sent_tnx + 
                            Received_Tnx + 
                            Number_of_Created_Contracts +  
                            avg_val_received, 
                        family = binomial, data = treino_dados)

summary(modelo_logistico_ref)

modelo_logistico <- modelo_logistico_ref

#------------------------------------------------------------------------------#

# Predições da Base de Treino

treino_prob <- predict(modelo_logistico, type = "response")
treino_pred <- ifelse(treino_prob > 0.5, 1, 0)

#Avaliar o Modelo de Regressão Logística Binária na Base de Treino

treino_matriz_confusao <- confusionMatrix(as.factor(treino_pred), treino_dados$FLAG)
print("Avaliação do Modelo de Regressão Logística Binária na Base de Treino:")
print(treino_matriz_confusao)

# Construir a ROC curve e AUC

roc_obj <- roc(treino_dados$FLAG, treino_pred)
auc_valor <- auc(roc_obj)
print(paste("AUC:", auc_valor))

# Acurácia

acuracia <- treino_matriz_confusao$overall["Accuracy"]
print(paste("Acurácia:", acuracia))

# Precisão 

precisao <- treino_matriz_confusao$byClass["Pos Pred Value"]
print(paste("Precisão do modelo:", precisao))

# Sensibilidade (Recall)

sensibilidade <- treino_matriz_confusao$byClass["Sensitivity"]
print(paste("Sensibilidade:", sensibilidade))

# Especificidade

especificidade <- treino_matriz_confusao$byClass["Specificity"]
print(paste("Especificidade:", especificidade))

# F1 Score

f1_score <- 2 * ((precisao * sensibilidade) / (precisao + sensibilidade))
print(paste("F1 Score:", f1_score))

#------------------------------------------------------------------------------#

# Preparar as predições para Base de Teste

teste_prob <- predict(modelo_logistico, newdata = teste_dados, type = "response")
teste_pred <- ifelse(teste_prob > 0.5, 1, 0)

# Avaliação do modelo no conjunto de testes

teste_matriz_confusao <- confusionMatrix(as.factor(teste_pred), teste_dados$FLAG)
print("Avaliação do Modelo de Regressão Logística Binária na Base de Teste:")
print(teste_matriz_confusao)

# Construir a ROC curve e AUC

roc_obj <- roc(teste_dados$FLAG, teste_prob)
auc_valor <- auc(roc_obj)
print(paste("AUC:", auc_valor))

# Acurácia
acuracia <- teste_matriz_confusao$overall["Accuracy"]
print(paste("Acurácia:", acuracia))

# Precisão 

precisao <- teste_matriz_confusao$byClass["Pos Pred Value"]
print(paste("Precisão do modelo:", precisao))

# Sensibilidade (Recall)
sensibilidade <- teste_matriz_confusao$byClass["Sensitivity"]
print(paste("Sensibilidade:", sensibilidade))

# Especificidade
especificidade <- teste_matriz_confusao$byClass["Specificity"]
print(paste("Especificidade:", especificidade))

# F1 Score

f1_score <- 2 * ((precisao * sensibilidade) / (precisao + sensibilidade))
print(paste("F1 Score:", f1_score))

# Gráfico da Curva ROC 

plot(roc_obj, main = "Curva ROC", col = "black")
abline(a = 0, b = 1, lty = 2, col = "red")

# Salvar o modelo como arquivo RDS 

saveRDS(modelo_logistico, "modelo_regressao_logistica.rds")

