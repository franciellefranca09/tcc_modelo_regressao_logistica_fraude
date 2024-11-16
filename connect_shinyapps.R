# install.packages('rsconnect') (Instalar caso ainda não tenha o pacote no RStudio).
# Fazer o cadastro de usuário no Shinyapps.io, ao final o serviço irá prover as informações abaixo.

rsconnect::setAccountInfo(name='****',
                          token='***',
                          secret='**')

# Copiar o caminho do Diretório onde está o Shiny app, assim como o seu nome.

library(rsconnect)

rsconnect::deployApp(
  appDir = "***",
  appFiles = "**"
)

