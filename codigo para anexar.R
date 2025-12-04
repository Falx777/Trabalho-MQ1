# CÓDIGO TRABALHO
# Análise exploratória das tarifas comerciais dos Estados Unidos sobre as exportações brasileira: uma análise de série interrompida e ARIMA

#Mude o diretório no seu arquivo para a pasta onde estão as planilhas!

meu.diretorio <- "L:/Arquivo/planilhas"
setwd("L:/Arquivo/planilhas")

#para 2024, troque por:
#df <- read.csv2("EXP_2024_MUN.csv")

#para 2025:
df <- read.csv2("EXP_2025_MUN.csv")

#View(df)

library(dplyr)

# Os dados estão divididos a nível de estado.
# Para tratar a exportação do Brasil como um todo, é preciso agregar os valores
dados_por_pais <- df %>%
  group_by(CO_PAIS, SH4, CO_MES, CO_ANO) %>%
  summarise(
    Total_KG_Liquido = sum(KG_LIQUIDO, na.rm = TRUE),
    Total_VL_FOB = sum(VL_FOB, na.rm = TRUE)
  ) %>%
  ungroup()

#selecionando estatísticas descritivas
estatisticas.descritivas.total <- (psych::describe(dados_por_pais))
colnames(estatisticas.descritivas.total) <- c("vars","Quantidade de Amostras","Média","Desvio Padrão Amostral","Mediana","t","s","Minimo","Máximo","Alcance","sk","ku","se")
estatisticas.filtradas <- estatisticas.descritivas.total[,c(2,3,5,4,8,9,10)]
for (col in seq(1:length(rownames(estatisticas.filtradas)))){
  estatisticas.filtradas[col,8] <- length(unique(dados_por_pais[,col])[[1]])
}
colnames(estatisticas.filtradas) <- c(colnames(estatisticas.filtradas)[1:7], "Valores Unicos")

View(estatisticas.filtradas)


#Função para selecionar os 10 principais elementos do banco de dados (por país ou por produto)
#_____________________________________________
#++++++++++++++++ FUNçÃO +++++++++++++++++++++
#---------------------------------------------
top.10 <- function(tipo, df){
  dados <- df
  if(tipo == "pais"){
    dados <- df %>%
      group_by(CO_PAIS) %>%
      summarise(
        Total_KG_Liquido = sum(KG_LIQUIDO, na.rm = TRUE),
        Total_VL_FOB = sum(VL_FOB, na.rm = TRUE)
      ) %>%
      ungroup()
  }else{
    dados <- df %>%
      group_by(SH4) %>%
      summarise(
        Total_KG_Liquido = sum(KG_LIQUIDO, na.rm = TRUE),
        Total_VL_FOB = sum(VL_FOB, na.rm = TRUE)
      ) %>%
      ungroup()
  } 
  
  dados_ordenados <- dados %>%
    arrange(dados$Total_VL_FOB)
  
  #TOP 10 que mais exportaram no ano, em QT_EST
  tail(dados_ordenados, 10)
  top.10.itens <- tail(dados_ordenados, 10)
  top.10.itens <- top.10.itens[rev(rownames(top.10.itens)),]
  top.10.itens$composicao_relativa <- round((top.10.itens$Total_VL_FOB/sum(dados_ordenados$Total_VL_FOB))*100,2)
  demais <- " "
  if (tipo == "pais"){
    codigo.paises <- read.csv2("PAIS (1).csv", fileEncoding = "Latin1", encoding="Latin-1")
    indices <- match(top.10.itens$CO_PAIS, codigo.paises$CO_PAIS)
    top.10.itens$Nome_pais <- codigo.paises$NO_PAIS[indices]
    top.10.itens$Nome_pais <- substr(top.10.itens$Nome_pais, start = 1, stop = 14)
    demais <- "Países"
  }else{
    codigo.produtos <- read.csv2("NCM_SH.csv",fileEncoding = "Latin1", encoding="Latin-1")
    indices <- match(top.10.itens$SH4, codigo.produtos$CO_SH4)
    top.10.itens$Nome_prod <- codigo.produtos$NO_SH4_POR[indices]
    top.10.itens$Nome_prod <- substr(top.10.itens$Nome_prod, start = 1, stop = 66)
    demais <- "produtos"
  }
  
  top.10.itens[11,] <- data.frame(-1,
                                   sum(dados_ordenados$Total_KG_Liquido)-sum(top.10.itens$Total_KG_Liquido),
                                   sum(dados_ordenados$Total_VL_FOB)-sum(top.10.itens$Total_VL_FOB),
                                   100-sum(top.10.itens$composicao_relativa[1:10]), 
                                   paste("Demais",demais))
  
  return(top.10.itens)
}

#Função para fazer o gráfico de pizza dos top 10 itens
#_____________________________________________
#++++++++++++++++ FUNçÃO +++++++++++++++++++++
#---------------------------------------------
pizza <- function(tipo, top.10.itens){
  nomes <- c()
  codigos <- c()
  layout.pizza <- c()
  legenda <- c()
  if(tipo == "pais"){
    nomes <- top.10.itens$Nome_pais
    layout.pizza <- c(0.65, 0.35)
    legenda <- paste(top.10.itens$Nome_pais,"-",top.10.itens$composicao_relativa,"%")
  }else{
    nomes <- c(top.10.itens$SH4[1:10],"Demais Produtos")
    layout.pizza <- c(0.6, 0.4)
    leg <- c("Soja", "Óleos brutos de Petróleo","Minério de Ferro", "Café","Açúcares", "Carne bovina", "Óleos de Petróleo", "Pastas químicas de madeira","Carnes e miudezas","Tortas e derivados do óleo de soja", "Demais produtos")
    legenda <- paste(leg,"-",top.10.itens$composicao_relativa,"%")
  }
  layout(matrix(1:2, ncol = 2), widths = layout.pizza)
  par(mar=c(0,0,0,0))
  pie(top.10.itens$composicao_relativa,
      radius=1.3,
      labels =nomes,
      col = rainbow(length(top.10.itens$composicao_relativa)),
      cex = 2
  )
  par(mar=c(0,0,0,0))
  plot.new()
  legend("left",
         legend = legenda,
         fill = rainbow(length(top.10.itens$composicao_relativa)),
         cex = 2,
         y.intersp = 0.5)
  
}

#selecionando os 10 principais paises do ano
top.10.paises <- top.10("pais",df)
#View(top.10.paises)
pizza("pais", top.10.paises)
#selecionando os 10 principais produtos do ano
top.10.produtos <- top.10("produtos",df)
#View(top.10.produtos)
pizza("produtos", top.10.produtos)

#Função para pegar os dados dos demais anos nos diferentes arquivos e montando a série para os países ou produto
#---------------------------------------------------------------------------
#_____________________________________________
#++++++++++++++++ FUNçÃO +++++++++++++++++++++
#---------------------------------------------
montar.serie <- function(tipo, top.10.itens){
  #pacote para tratar das datas 
  library(lubridate)
  lista.series.anuais <- list()
  setwd(meu.diretorio)
  anos <- as.character(2015:2025)

  for (ano in anos){
    nome.arquivo <- paste("EXP_",ano,"_MUN.csv", sep="")
    df <- read.csv2(nome.arquivo)
    if (tipo == "pais"){      
      dados_por_mes.total <- df%>%
        group_by(CO_PAIS, SH4, CO_MES, CO_ANO) %>%
        summarise(
          Total_KG_Liquido = sum(KG_LIQUIDO, na.rm = TRUE),
          Total_VL_FOB = sum(VL_FOB, na.rm = TRUE)
        ) %>%
        ungroup()
      
      # Filtrar o banco dados_por_pais_e_ncm para manter apenas os países do top 10
      dados_top.10 <- dados_por_mes.total %>%
        filter(CO_PAIS %in% top.10.itens$CO_PAIS)
      
      # Agrupar por país e mês para análise temporal
      dados_por_mes <- dados_top.10 %>%
        group_by(CO_PAIS, CO_MES) %>%
        summarise(
          Total_KG_Liquido = sum(Total_KG_Liquido, na.rm = TRUE),
          Total_VL_FOB = sum(Total_VL_FOB, na.rm = TRUE),
          .groups = 'drop'
        )

    }else{
      dados_por_mes.total <- df%>%
        group_by(CO_PAIS, SH4, CO_MES, CO_ANO) %>%
        summarise(
          Total_KG_Liquido = sum(KG_LIQUIDO, na.rm = TRUE),
          Total_VL_FOB = sum(VL_FOB, na.rm = TRUE)
        ) %>%
        ungroup()
      
      # Filtrar o banco dados_por_pais_e_ncm para manter apenas os países do top 10
      dados_top.10 <- dados_por_mes.total %>%
        filter(SH4 %in% top.10.itens$SH4)
      
      # Agrupar por país e mês para análise temporal
      dados_por_mes <- dados_top.10 %>%
        group_by(SH4, CO_MES) %>%
        summarise(
          Total_KG_Liquido = sum(Total_KG_Liquido, na.rm = TRUE),
          Total_VL_FOB = sum(Total_VL_FOB, na.rm = TRUE),
          .groups = 'drop'
        )

    }
    
    dados_por_mes$data <- ym(paste(ano,dados_por_mes$CO_MES, sep = "-"))
    lista.series.anuais[[length(lista.series.anuais) + 1]] <- dados_por_mes
  }
  lista.series <- bind_rows(lista.series.anuais)
  return(lista.series)
}

series.top.10.paises <- montar.serie("pais",top.10.paises)
#View(series.top.10.paises)


setwd(paste(meu.diretorio,"/paises", sep=""))
write.csv(series.top.10.paises,"dados_exportados_pais.csv", row.names = TRUE)

series.top.10.produtos <- montar.serie("produtos",top.10.produtos)
#View(series.top.10.produtos)
setwd(paste(meu.diretorio,"/produtos", sep=""))
write.csv(series.top.10.produtos,"dados_exportados.csv", row.names = TRUE)

#Função para plotar os gráficos dos países e produtos
#_____________________________________________
#++++++++++++++++ FUNçÃO +++++++++++++++++++++
#---------------------------------------------
series.e.graficos <- function(tipo, df, top.10.itens){
  itens_unicos <- c()
  caminho <- ""
  if (tipo == "pais"){
    itens_unicos <- unique(df$CO_PAIS)
    caminho <- paste(meu.diretorio,"/paises", sep="")
  }else{
    itens_unicos <-  unique(df$SH4)
    caminho <- paste(meu.diretorio,"/produtos", sep="")
  }
  modelos <- list()
  series.cada.item <- list()
  tempo.item <- c()
  lista.nomes <- c()
  nome.serie <- ""
  par(mfrow=c(1,1), mar = c(4, 4, 3, 2) + 0.1) 
  for(item in itens_unicos) {
    # Filtra dados do país atual
    serie.item <- df
    if( tipo == "pais"){
      serie.item <- df %>% 
        filter(CO_PAIS == item) %>% 
        pull(Total_VL_FOB)      
    
      if(item == itens_unicos[1]){
        tempo.item <- df %>% 
          filter(CO_PAIS == item) %>% 
          pull(data)
      }
      lista.nomes <- c(lista.nomes, top.10.itens$Nome_pais[top.10.itens$CO_PAIS ==item])
      nome.serie <- top.10.itens$Nome_pais[top.10.itens$CO_PAIS ==item]
    }else{
      serie.item <- df %>% 
        filter(SH4 == item) %>% 
        pull(Total_VL_FOB)
      
      if(item == itens_unicos[1]){
        tempo.item <- df %>% 
          filter(SH4 == item) %>% 
          pull(data)
      }
      
      lista.nomes <- c(lista.nomes,top.10.itens$SH4[top.10.itens$SH4 ==item])
      nome.serie <- top.10.itens$SH4[top.10.itens$SH4 ==item]
      }
    series.cada.item[[length(series.cada.item)+1]] <- serie.item
    setwd(caminho)
    png(paste(nome.serie,".png"), width = 800, height = 600, res = 120)
    plot(x = tempo.item, y = serie.item, type = "l", main = nome.serie,
         xlab = "dias", ylab = "Total_VL_FOB", col =item,
         lwd="3")
    abline(v = ymd("2025-07-01"), col = "red", lty = 3, lwd = 2)
    dev.off()
  }
  cat(paste("Gráficos salvos com sucesso! \n Loca:",caminho))
  names(series.cada.item) <- lista.nomes
  return(series.cada.item)
}

#exportação dos 10 paises no tempo
series.cada.pais <- series.e.graficos("pais", series.top.10.paises, top.10.paises)

#exportação dos 10 produtos no tempo
series.cada.produto <- series.e.graficos("produto", series.top.10.produtos, top.10.produtos)

# função para gerar gráfico dos modelos arima
#_____________________________________________
#++++++++++++++++ FUNçÃO +++++++++++++++++++++
#---------------------------------------------
gerar.modelos.arima <-function(series.cada.item){
  library(stats)
  library(forecast)
  library(tseries)
  modelos <- list()
  ponto.pre.quebra <-length(series.cada.item[[1]])-4
  for (i in seq(1:length(series.cada.item))){
    valor.ex.ante <- (ts(series.cada.item[[i]][1:ponto.pre.quebra], frequency=12))
    valor.ex.post <- (ts(series.cada.item[[i]], frequency=12))
    
    modelo.estimado <- auto.arima(valor.ex.ante,
                                  seasonal = TRUE,
                                  stepwise = FALSE,        
                                  approximation = FALSE,   
                                  lambda = "auto",         
                                  biasadj = TRUE,          
                                  allowdrift = TRUE,
                                  allowmean = TRUE,
                                  max.p = 5, max.q = 5,    
                                  max.P = 3, max.Q = 3,
                                  trace = TRUE) 
    
    modelos[[length(modelos)+1]] <- modelo.estimado    
  }
  return(modelos)
}

############################################################
#
#gerar os modelos ARIMA para os 10 países
#
############################################################
#retire os comentários dos dois códigos abaixo para estimar os modelos.
#modelos.paises <- gerar.modelos.arima(series.cada.pais)
#names(modelos.paises) <- names(series.cada.pais)

#Salvando os modelos (para não demorar na estimação)
setwd(paste(meu.diretorio,"/paises", sep=""))
#saveRDS(modelos.paises, file = "modelos_arima_paises.rds")

#lendo arquivo com os modelos salvos
modelos.paises <- readRDS("modelos_arima_paises.rds")

############################################################
#
#gerar os modelos ARIMA para os 10 produtos
#
############################################################
#retire os comentários dos dois códigos abaixo para estimar os modelos.
#modelos.produtos <- gerar.modelos.arima(series.cada.produto)
#names(modelos.produtos) <- names(series.cada.produto)

#Salvando os modelos (para não demorar na estimação)
setwd(paste(meu.diretorio,"/produtos", sep=""))
#saveRDS(modelos.produtos, file = "modelos_arima_produtos.rds")

#lendo arquivo com os modelos salvos
modelos.produtos <- readRDS("modelos_arima_produtos.rds")


#Função para gerar gráficos do modelo ARIMA
#_____________________________________________
#++++++++++++++++ FUNçÃO +++++++++++++++++++++
#---------------------------------------------
gerar.graficos.arima <- function(tipo, modelos, series.cada.item){
  nome.grafico <- ""
  caminho <- ""
  if (tipo == "pais"){
    caminho <- paste(meu.diretorio,"/paises", sep="")
  }else{
    caminho <- paste(meu.diretorio,"/produtos", sep="")
  }
  ponto.pre.quebra <-length(series.cada.item[[1]])-4
  for (i in seq(1:length(modelos))){
    if (tipo == "pais"){
      nome.grafico <- names(series.cada.item)[i]
    }else{
      nome.grafico <- names(series.cada.item)[i]
    }
    valor.ex.ante <- (ts(series.cada.item[[i]][1:ponto.pre.quebra], frequency=12))
    valor.ex.post <- (ts(series.cada.item[[i]]))
    
    cut.off <- length(valor.ex.ante)
    if( modelos[[i]]$arma[5] == 12){
      valor.ex.post <- (ts(series.cada.item[[i]],  frequency=12))
      cut.off <- tsp(valor.ex.ante)[2]
    }

    fp <- forecast::forecast(modelos[[i]],level=c(95),h=4)
    estimado <- fp$mean
    efetivo <- ( series.cada.item[[i]][(ponto.pre.quebra+1): length(series.cada.item[[i]])])
    print(nome.grafico)
    print(efetivo/estimado)
    setwd(caminho)
    png(paste("previsão",i,nome.grafico,".png"), width=1400, height=800)
    par(cex.lab = 1.5, cex.axis = 2)
    plot(fp, xlab=names(modelos)[i], ylab="Valor", ylim = c(0, max(c(max(fp$upper),max(valor.ex.post)))))
    lines(valor.ex.post, col="darkgreen", lwd="4")
    abline(v = cut.off, col = "red", lty = 2, cex=2.2)  # marca o ponto de intervenção
    dev.off()
  }  
  cat(paste("Gráficos gerados com sucesso!\n Local:", caminho))
}

#gráfico do modelo ARIMA para os 10 paises
gerar.graficos.arima("pais",modelos.paises, series.cada.pais)

#-----------
#-----------
library(forecast)
ponto.pre.quebra <-length(series.cada.produto[[1]])-4
modelos.produtos[[1]] <- auto.arima(ts(series.cada.produto[[1]][1:ponto.pre.quebra], frequency=12),
                           seasonal = TRUE,
                           approximation = FALSE,   
                           allowdrift = TRUE,
                           allowmean = TRUE,
                           max.p = 3, max.q = 3,    
                           max.P = 3, max.Q = 3,
                           trace = TRUE) 

#gráfico do modelo ARIMA para os 10 produtos
gerar.graficos.arima("produto", modelos.produtos, series.cada.produto)

library(modelsummary)
#tabela com os resultados dos modelos dos 10 países
modelsummary(modelos.paises, stars = TRUE)

#tabela com os resultados dos modelos dos 9 produtos
modelsummary( modelos.produtos[2:10], stars = TRUE)

#Função para gerar o gráfico dos resíduos do modelo
#_____________________________________________
#++++++++++++++++ FUNçÃO +++++++++++++++++++++
#---------------------------------------------
graficos.residuos <- function(tipo, modelos){
  caminho <- ""
  if (tipo == "pais"){
    caminho <- paste(meu.diretorio,"/paises/residuos", sep="")
  }else{
    caminho <-  paste(meu.diretorio,"/produtos/residuos", sep="")
  }
  setwd(caminho)
  for(i in seq(1:length(modelos))){
    png(paste(names(modelos)[i],".png"), width = 1200, height = 600)
    checkresiduals(modelos[[i]], lag=24, test="LB")
    dev.off()
  }
  cat(paste("Gráficos gerados com sucesso!\n Local:", caminho))
}

#análise dos resíduos dos 10 países
graficos.residuos("pais",modelos.paises)

#análise dos resíduos dos 10 produtos
graficos.residuos("produtos",modelos.produtos)

#função para gerar a tabela de testes dos modelos
#_____________________________________________
#++++++++++++++++ FUNçÃO +++++++++++++++++++++
#---------------------------------------------
#install.packages("rugarch")
#install.packages("FinTS")
tabela.testes <- function(modelos, series.cada.item){
  library(FinTS)
  library(rugarch)
  testes <- data.frame(matrix(ncol=3,nrow=length(series.cada.item)))
  colnames(testes) <- c("Item", "P-valor teste Ljung-Box", "P-valor teste ARCH")
  
  for (i in seq(1:length(modelos))){
    teste_arch <- FinTS::ArchTest(residuals(modelos[[i]]), lags = 24)
    testes[i,1] <- names(modelos)[i]
    testes[i,2] <- round((Box.test(residuals(modelos[[i]]), 
                                   lag = 24, 
                                   type = "Ljung-Box", 
                                   fitdf = forecast::modeldf(modelos[[i]])))$p.value,5)
    testes[i,3] <- round(teste_arch$p.value,5)
  }
  
  return(testes)
}

#gerar tabela de teste dos 10 países
View(tabela.testes(modelos.paises,series.cada.pais))

#gerar tabela de teste dos 10 produtos
View(tabela.testes(modelos.produtos,series.cada.produto))

#######################################3
#baixando os dados da série do câmbio de venda
#install.packages("ipeadatar")
library(ipeadatar)

cambio <- ipeadata("BM12_ERV12")
cambio <- cambio |> 
  dplyr::select(date, value) |> 
  dplyr::rename(cambio_export = value)
cambio

#adicionando o cambio às séries dos paises
series.top.10.paises$data <- as.Date(series.top.10.paises$data)
series.top.10.paises <- series.top.10.paises |> 
  dplyr::left_join(cambio, by = c("data" = "date"))

#adicionando o cambio às séries dos produtos
series.top.10.produtos$data <- as.Date(series.top.10.produtos$data)
series.top.10.produtos <- series.top.10.produtos |> 
  dplyr::left_join(cambio, by = c("data" = "date"))

#Função para gerar os gráficos da regressão OLS
#_____________________________________________
#++++++++++++++++ FUNçÃO +++++++++++++++++++++
#---------------------------------------------
ols.graficos <- function(tipo, df, top.10.itens){
  itens_unicos <- c()
  caminho <- ""
  if (tipo == "pais"){
    itens_unicos <- unique(df$CO_PAIS)
    caminho <-  paste(meu.diretorio,"/paises/its paises", sep="")
  }else{
    itens_unicos <-  unique(df$SH4)
    caminho <- paste(meu.diretorio,"/produtos/its produtos", sep="")
  }
  modelos <- list()
  tempo.item <- c()
  cambio.mensal <- c()
  lista.nomes <- c()
  setwd(caminho)
  par(mfrow=c(1,1), mar = c(4, 4, 3, 2) + 0.1) 
  for(item in itens_unicos) {
    # Filtra dados do produto atual
    serie.item <- df
    nome.item <- ""
    if(tipo == "pais"){
      nome.item <- top.10.itens$Nome_pais[top.10.itens$CO_PAIS == item]
      serie.item <- df %>% 
        filter(CO_PAIS == item) %>% 
        pull(Total_VL_FOB)
      if(item == itens_unicos[1]){
        tempo.item <- df %>% 
          filter(CO_PAIS == item) %>% 
          pull(data) %>% 
          as.Date()
        cambio.mensal <- df %>% 
          filter(CO_PAIS == item) %>% 
          pull(cambio_export)
      }  
      
    }else{
      nome.item <- item
      serie.item <- df %>% 
        filter(SH4 == item) %>% 
        pull(Total_VL_FOB)
      
      if(item == itens_unicos[1]){
        tempo.item <- df %>% 
          filter(SH4 == item) %>% 
          pull(data) %>% 
          as.Date()
        
        cambio.mensal <- df %>% 
          filter(SH4 == item) %>% 
          pull(cambio_export)
      }  
      
    }
    
    corte <- which(tempo.item == "2025-07-01")
    tempo <- seq(1,length(tempo.item))
    mes <- factor(format(tempo.item, "%m"))
    dummy.tarifas <- c((seq(2,corte)^0) -1,seq(corte,length(tempo.item))^0)
    meses.pos.tarifas <- c((seq(1,corte)^0) -1,seq(1,length(tempo.item)-corte))
    
    its <- lm(serie.item ~ tempo + dummy.tarifas + meses.pos.tarifas +cambio.mensal+ mes)
    
    dummy.tarifas.falsa <- -1 +dummy.tarifas^0
    meses.pos.tarifas.falso <- -1 + meses.pos.tarifas^0
    
    dados.contrafact <- data.frame(tempo=tempo,
                                   dummy.tarifas = dummy.tarifas.falsa,
                                   meses.pos.tarifas=meses.pos.tarifas.falso,
                                   mes = mes,
                                   cambio.mensal = cambio.mensal)
    
    previsao.simulada <- predict(its, newdata = dados.contrafact)
    modelos[[length(modelos)+1]] <- its
    
    
    lista.nomes <- c(lista.nomes, nome.item)
    setwd(caminho)
    png(paste("Item",nome.item,".png"), width = 2000, height = 900)
    plot(x = tempo, y = serie.item, type = "l", main = nome.item,
         xlab = "dados mensais", ylab = "Total_VL_FOB", col =as.integer(as.factor(item)),
         lwd="8", xlim=c(0,length(tempo)), cex.lab = 2, cex.main = 3)
    lines(fitted(its), 
          col = "red",  
          lwd = 8)
    lines(previsao.simulada, 
          col = "blue",
          lty =3,
          lwd = 8)
    abline(v = corte, col = "green", lty = 3, lwd = 5)
    legend("topleft", legend=c("Anúncio das Tarifas","Modelo","Contrafactual","Observado"),
           fill = c("green","red","blue", as.integer(as.factor(item))),
           cex = 1.8)
    dev.off()
  }
  cat(paste("Gráficos gerados com sucesso!\n Local:", caminho))
  return(modelos)
}

#gerando gráficos para os 10 paises
modelos.ols.paises <- ols.graficos("pais", series.top.10.paises, top.10.paises)
names(modelos.ols.paises) <- names(series.cada.pais)

#gerando gráficos para os 10 produtos
modelos.ols.produtos <- ols.graficos("produtos", series.top.10.produtos, top.10.produtos)
names(modelos.ols.produtos) <- names(series.cada.produto)

#gerando tabela da regressão dos 10 paises
modelsummary(modelos.ols.paises, stars = TRUE, output = "tinytable")

#gerando tabela da regressão dos 10 produtos
modelsummary(modelos.ols.produtos, stars = TRUE, output = "tinytable")

##########################################


#Função para gerar as estatistias descritivas das séries
#_____________________________________________
#++++++++++++++++ FUNçÃO +++++++++++++++++++++
#---------------------------------------------
gerar.estatisticas.descritivas <- function(lista){
  estatisticas <- data.frame(matrix(ncol = 13, nrow=10))
  rownames(estatisticas) <- names(lista)
  colnames(estatisticas) <- c("vars","Quantidade de Amostras","Média","Desvio Padrão Amostral","Mediana","t","s","Minimo","Máximo","Alcance","sk","ku","se")
  for (i in seq(1:10)){
    estatisticas[i,] <-  psych::describe(lista[[i]])
  }
  return(estatisticas[,c(2,3,5,4,8,9,10)])
}

#Função para gerar os gráficos das estatísticas descritivas
#_____________________________________________
#++++++++++++++++ FUNçÃO +++++++++++++++++++++
#---------------------------------------------
graficos.estatisticas <- function( tipo, estatisticas, serie){
  if(tipo == "pais"){
    setwd(paste(meu.diretorio,"/paises/estatisticas", sep=""))
    
  }else{
    setwd(paste(meu.diretorio,"/produtos/estatisticas", sep=""))
  }
  for (i in seq(1:10)){
    cores <- colorRampPalette(c(i*2, i*11))(10)
    results <- round(est.paises[i,],4)
    results <- as.matrix(results)
    
    #colocando a frequência das estatísticas descritivas na variável estatistica.descritiva
    estatistica.descritiva <- barplot(results, plot=FALSE)
    
    png(paste("estatistica ",names(serie[i]),".png"), width = 1000, height = 400)
    #plotando barra
    barplot(results,
            ylim = c(min(results),max(results)*2),
            col = cores,
            main = names(serie[i]),
            cex.names = 0.8)
    text(x = estatistica.descritiva, 
         y = results,
         labels = results, 
         pos = 3,
         cex=0.8
    ) 
    dev.off()
    par(mfrow=c(1,1))
    boxplot(serie[i],
            main = paste("Boxplot ", names(serie[i])), 
            ylab = "Valor",
            col = cores)
    
    
  }
  
}

#gerando estatisticas para os paises
est.paises <- gerar.estatisticas.descritivas(series.cada.pais)
View(est.paises)
#gerando gráficos das estatisticas dos paises
graficos.estatisticas("pais", est.paises, series.cada.pais)
#gerando estatisticas para os produtos
est.produtos <- gerar.estatisticas.descritivas(series.cada.produto)
View(est.produtos)
#gerando gráficos das estatisticas dos produtos
graficos.estatisticas("produto", est.produtos, series.cada.produto)

#############################
#MATRIZ DE CORRELAÇÃO produtos
setwd(paste(meu.diretorio,"/produtos", sep=""))
dados_por_mes_anos <- read.csv2("dados_exportados.csv", encoding="UTF-8", sep=",")

codigos_sh4_unicos <- unique(dados_por_mes_anos$SH4)
matriz.cor <- data.frame(matrix(nrow=length(codigos_sh4_unicos), ncol=length(codigos_sh4_unicos)))
colnames(matriz.cor) <- codigos_sh4_unicos
rownames(matriz.cor) <- codigos_sh4_unicos
library(tidyr)
library(tibble)

dados_pivotados <- dados_por_mes_anos %>%
  select(data, SH4, Total_VL_FOB) %>%
  pivot_wider(
    names_from = SH4, 
    values_from = Total_VL_FOB
  ) %>%
  column_to_rownames(var = "data")

for (ele in codigos_sh4_unicos){
  for (ele2 in codigos_sh4_unicos){
    serie1 <- dados_pivotados[[as.character(ele)]]
    serie2 <- dados_pivotados[[as.character(ele2)]]
    correlacao <- cor(serie1, serie2, use = "complete.obs")
    matriz.cor[as.character(ele), as.character(ele2)] <- round(correlacao, 5)
  }  
}

library(pheatmap)
# criando um heatmap da matriz de correlação
pheatmap(
  matriz.cor,
  display_numbers = TRUE,
  cluster_rows = FALSE,      
  cluster_cols = FALSE,      
  show_rownames = TRUE,      
  show_colnames = TRUE,      
  main = "Matriz de corelação" 
)

##################################################3
#Matriz de correlação países
setwd(paste(meu.diretorio,"/paises", sep=""))
dados_por_mes_anos <- read.csv2("dados_exportados_pais.csv", encoding="UTF-8", sep=",")

nomes.paises <- top.10.paises %>% 
  select(CO_PAIS, Nome_pais)

dados_por_mes_anos <- dados_por_mes_anos %>%
  left_join(nomes.paises, by = "CO_PAIS")

codigos_pais_unicos <- unique(dados_por_mes_anos$Nome_pais)
matriz.cor <- data.frame(matrix(nrow=length(codigos_pais_unicos), ncol=length(codigos_pais_unicos)))
colnames(matriz.cor) <- codigos_pais_unicos
rownames(matriz.cor) <- codigos_pais_unicos

dados_pivotados <- dados_por_mes_anos %>%
  select(data, Nome_pais, Total_VL_FOB) %>%
  pivot_wider(
    names_from = Nome_pais, 
    values_from = Total_VL_FOB
  ) %>%
  column_to_rownames(var = "data")

for (ele in codigos_pais_unicos){
  for (ele2 in codigos_pais_unicos){
    serie1 <- dados_pivotados[[as.character(ele)]]
    serie2 <- dados_pivotados[[as.character(ele2)]]
    correlacao <- cor(serie1, serie2, use = "complete.obs")
    matriz.cor[as.character(ele), as.character(ele2)] <- round(correlacao, 5)
  }  
}

pheatmap(
  matriz.cor,
  display_numbers = TRUE,
  cluster_rows = FALSE,      
  cluster_cols = FALSE,      
  show_rownames = TRUE,      
  show_colnames = TRUE,      
  main = "Matriz de corelação" 
)

