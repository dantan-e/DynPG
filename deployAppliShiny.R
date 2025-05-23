library(rsconnect)

rsconnect::setAccountInfo(name='dantan',
                          token='B2D7BAC8FD83615C622F264D9B6B8F59',
                          secret='u62b0WB5lup44SnZ08+T75iirKWZk+S65IUQhIxD')
deployApp(appName = "DynPG")

