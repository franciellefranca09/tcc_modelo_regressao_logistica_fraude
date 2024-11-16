# Carregar bibliotecas necessárias

library(shiny)
library(caret)
library(dplyr)
library(pROC)
library(car)
library(psych)
library(shinythemes)
library(DT)

# Front do Shiny APP (UI)

ui <- fluidPage(
  theme = shinytheme("cosmo"),  
  titlePanel("Modelo de Regressão Logística Binária - Fraudes em Contas da Plataforma Ethereum"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Carregar o Banco de Dados (.csv):", accept = ".csv"),
      actionButton("train", "Treinar o Modelo"),
      hr(),
      h4("Configurações"),
      numericInput("threshold", "Threshold:", 
                   value = 0.5, min = 0.1, max = 0.9, step = 0.1)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Banco de Dados", DT::DTOutput("fulldata")),  
        tabPanel("Resumo dos Dados", DT::DTOutput("data_summary")),  
        tabPanel("Matriz Confusão e Estatísticas",
                 verbatimTextOutput("model_stats")),
        tabPanel("Curva ROC", plotOutput("roc_plot"))
      )
    )
  )
)

# Server

server <- function(input, output, session) {
  
# Carregar a base de dados (no caso do TCC em formato CSV)
  
dados <- reactive({
  req(input$file)
  data <- read.csv(input$file$datapath)
    
# Excluindo as variáveis independentes de acordo com o tratamento de dados realizado anteriormente
    
data <- subset(data, select = -c(Index,
                                 Address,
                                 min_value_sent_to_contract,
                                 max_val_sent_to_contract,
                                 avg_value_sent_to_contract,
                                 total_ether_sent_contracts,
                                 ERC20_most_sent_token_type, 
                                 ERC20_most_rec_token_type,
                                 Total_ERC20_tnxs,
                                 ERC20_total_Ether_received,
                                 ERC20_total_ether_sent,
                                 ERC20_total_Ether_sent_contract,
                                 ERC20_uniq_sent_addr,
                                 ERC20_uniq_rec_addr, 
                                 ERC20_uniq_sent_addr.1, 
                                 ERC20_uniq_rec_contract_addr,
                                 ERC20_avg_time_between_sent_tnx, 
                                 ERC20_avg_time_between_rec_tnx,
                                 ERC20_avg_time_between_rec_2_tnx,
                                 ERC20_avg_time_between_contract_tnx, 
                                 ERC20_min_val_rec, 
                                 ERC20_min_val_sent,
                                 ERC20_max_val_sent, 
                                 ERC20_avg_val_sent, 
                                 ERC20_min_val_sent_contract,
                                 ERC20_max_val_sent_contract,
                                 ERC20_avg_val_sent_contract, 
                                 ERC20_uniq_sent_token_name, 
                                 ERC20_uniq_rec_token_name,
                                 ERC20_max_val_rec, 
                                 ERC20_avg_val_rec))
    
# Converter a variável dependente binária (FLAG) para fator
    
data$FLAG <- as.factor(data$FLAG)
  return(data)
})
  
# Construindo a aba Banco de Dados
  
output$fulldata <- DT::renderDT({
  req(dados())
  datatable(dados(), 
            options = list(
              pageLength = 5,   
              autoWidth = TRUE,  
              dom = 'Bfrtip',    
              scrollX = TRUE    
            ),
            class = 'cell-border stripe'  
  )
})
  
# Construindo a Resumo de Dados
  
output$data_summary <- DT::renderDT({
  req(dados())
    
    
  summary_data <- as.data.frame(describe(dados()))
    
    
  colnames(summary_data) <- c("Index", 
                                "Total Observations", 
                                "Mean", 
                                "Standard Deviation", 
                                "Median", 
                                "Trimmed", 
                                "Mad", 
                                "Minimum", 
                                "Maximum", 
                                "Range",
                                "Skew",
                                "Kurtosis",
                                "Standard Error")
    
  datatable(summary_data, 
            options = list(
              pageLength = 5, 
              autoWidth = TRUE, 
              dom = 'Bfrtip', 
              scrollX = TRUE
            ),
            class = 'cell-border stripe'  # Adiciona bordas nas células e faixas alternadas
    )
  })
  
# Dividir os dados em conjunto de treino (70%) e teste (30%)
  
modelo <- eventReactive(input$train, {
  req(dados())
  data <- dados()
    
  set.seed(123)
  treino_index <- createDataPartition(data$FLAG, p = 0.7, list = FALSE)
  treino_dados <- data[treino_index, ]
    
# Modelo de Regressão Logística Binária Final
    
glm(FLAG ~ Avg_min_between_sent_tnx + 
      Time_Diff_between_first_and_last_.Mins. + 
      Sent_tnx + 
      Received_Tnx + 
      Number_of_Created_Contracts +  
      avg_val_received, 
    family = binomial, data = treino_dados)
})
    
output$model_stats <- renderPrint({
  req(modelo(), dados())
  data <- dados()
  set.seed(123)
  treino_index <- createDataPartition(data$FLAG, p = 0.7, list = FALSE)
  teste_dados <- data[-treino_index, ]
    
# Previsões
    
  teste_prob <- predict(modelo(), newdata = teste_dados, type = "response")
  teste_pred <- ifelse(teste_prob > input$threshold, 1, 0)
    
# Matriz Confusão
  matriz_confusao <- confusionMatrix(as.factor(teste_pred), teste_dados$FLAG)
  auc_valor <- auc(roc(teste_dados$FLAG, teste_prob))
  f1_score <- 2 * (matriz_confusao$byClass["Pos Pred Value"] * 
                       matriz_confusao$byClass["Sensitivity"]) / 
      (matriz_confusao$byClass["Pos Pred Value"] + 
         matriz_confusao$byClass["Sensitivity"])
    
    cat("Matriz de Confusão:\n")
    print(matriz_confusao)
    cat("\nEstatísticas do Modelo:\n")
    cat("AUC:", auc_valor, "\n")
    cat("F1 Score:", f1_score, "\n")
})
  
# Curva ROC

output$roc_plot <- renderPlot({
  req(modelo(), dados())
  data <- dados()
  set.seed(123)
  treino_index <- createDataPartition(data$FLAG, p = 0.7, list = FALSE)
  teste_dados <- data[-treino_index, ]
    
# Previsões
  
  teste_prob <- predict(modelo(), newdata = teste_dados, type = "response")
  roc_obj <- roc(teste_dados$FLAG, teste_prob)
    
# Gráfico Curva ROC 
  
  plot(roc_obj, main = "Curva ROC", col = "black", lwd = 2, 
       lty = 1, cex.main = 1.5)
  abline(a = 0, b = 1, lty = 2, col = "red")  
  })
}

# Rodar o aplicativo Shiny do Modelo de Regressão Logística Binária

shinyApp(ui, server)
