install.packages('rsconnect')

rsconnect::setAccountInfo(name='****',
                          token='***',
                          secret='**')

library(rsconnect)

rsconnect::deployApp(
  appDir = "***",
  appFiles = "**"
)

