install.packages(c("dplyr", "ggplot2", "tidyr", "readr", "summarytools", "skimr"))
library(dplyr)       # Manipulação de dados
library(ggplot2)     # Gráficos
library(tidyr)       # Limpeza de dados
library(readr)       # Leitura de arquivos CSV
library(summarytools) # Resumo estatístico n
library(skimr)       # Resumo detalhado dos dados n
library(leaflet)      # Criação de mapas interativos
library(leaflet.extras)
library(plotly)
library(corrplot)

#Carregando arquivos e corrijindo identificação do separador

df_1 <- read.csv("C:/Users/victo/Documents/cat_acidentes_parte1.csv", sep = ',')
df_2 <- read.csv("C:/Users/victo/Documents/cat_acidentes_parte2.csv", sep = ',')

# Juntando os dois DataFrames
df <- rbind(df_1, df_2)
head(df)

# Remover as colunas especificadas que não serão utilizadas ou que são redundantes
df <- df %>%
  select(-data_extracao, -predial1, -queda_arr, -idacidente, -log1, -log2, -consorcio)


# Visualizar os tipos de colunas
sapply(df, class)

# Contagem de valores ausentes por coluna
colSums(is.na(df))  

# Remover linhas com NA
df <- drop_na(df)

# Contagem de valores ausentes por coluna novamente
colSums(is.na(df))  

#install.packages(c("leaflet", "cluster", "factoextra"))
#library(leaflet)      # Criação de mapas interativos
#library(cluster)      # Algoritmos de clustering
#library(factoextra)   # Visualização de clusters

# Selecionar latitude e longitude
geo_data <- df %>% 
  select(latitude, longitude)

#Grafico de calor
leaflet(geo_data) %>%
  addTiles() %>%
  setView(lng = -51.2177, lat = -30.0346, zoom = 12) %>%  # Centraliza em Porto Alegre com zoom adequado
  addHeatmap(lng = ~longitude, lat = ~latitude, blur = 20, max = 0.05, radius = 15)

#Grafico de cluster
leaflet(geo_data) %>%
  addTiles() %>%
  setView(lng = -51.2177, lat = -30.0346, zoom = 12) %>%
  addMarkers(lng = ~longitude, lat = ~latitude, clusterOptions = markerClusterOptions())

#CENTRO  LESTE  NORTE    SUL 
#8878  19717  18971  15177 

#install.packages("leaflet.extras")

#library(leaflet.extras)























################ Proporcao por tipo de acidente


# Contando as ocorrências de cada tipo de acidente
contagem <- table(df$tipo_acid)
print(contagem)
# Número de categorias
qtd <- length(contagem)
print(qtd)

# Criando rótulos com apenas as porcentagens
lbls <- paste(round(contagem / sum(contagem) * 100, 1), "%")

# Ajustar a área de plotagem para dar espaço para a legenda
par(mar = c(5, 4, 4, 0))  # Ajuste das margens

# Criar o gráfico de pizza com rótulos apenas com porcentagens
pie(x = contagem, 
    labels = lbls, 
    main = "Proporção de Tipos de Acidentes",  # Ajuste do título
    col = rainbow(qtd),
    cex = 0.8,
    label.pos = 0.1)

# Adicionar a legenda à esquerda, ajustando o tamanho e nome
legend("left", 
       inset = c(0, 0),  # Posicionar a legenda
       col = rainbow(qtd), 
       pch = 15, 
       legend = names(contagem),  # Usar os tipos de acidente como legenda
       title = "Tipos de Acidente",  # Ajustar o título da legenda
       cex = 0.6)  # Ajustar o tamanho da legenda


############################
#Distribuição de acidentes


# Calcular as somas
soma_valores1 <- sum(df$auto)
soma_valores2 <- sum(df$taxi)
soma_valores3 <- sum(df$lotacao)
soma_valores4 <- sum(df$onibus_urb)
soma_valores5 <- sum(df$onibus_met)
soma_valores6 <- sum(df$onibus_int)
soma_valores7 <- sum(df$caminhao)
soma_valores8 <- sum(df$moto)
soma_valores9 <- sum(df$carroca)
soma_valores10 <- sum(df$bicicleta)
soma_valores11 <- sum(df$outro)
soma_valores12 <- sum(df$patinete)



#############

# Carregar a biblioteca plotly
library(plotly)

# Dados para o gráfico
contagens <- c(soma_valores1, soma_valores2, soma_valores3,
               soma_valores4, soma_valores5, soma_valores6,
               soma_valores7, soma_valores8, soma_valores9,
               soma_valores10, soma_valores11, soma_valores12)

# Nomes das categorias
nomes_categorias <- c("Auto", "Taxi", "Lotacao", "Onibus Urb", "Onibus Met", 
                      "Onibus Int", "Caminhao", "Moto", "Carroca", 
                      "Bicicleta", "Outro", "Patinete")

# Criar um DataFrame para plotly
df_contagens <- data.frame(
  Categoria = nomes_categorias,
  Contagem = contagens
)

# Ordenar o DataFrame por Contagem em ordem decrescente
df_contagens <- df_contagens[order(df_contagens$Contagem, decreasing = TRUE), ]

# Reordenar o fator Categoria para refletir a ordem desejada
df_contagens$Categoria <- factor(df_contagens$Categoria, levels = df_contagens$Categoria)

# Criar o gráfico de barras com plotly
fig <- plot_ly(df_contagens, x = ~Categoria, y = ~Contagem, type = 'bar')

# Atualizar layout para adicionar título e ajustar aparência
fig <- fig %>% layout(title = 'Distribuição dos Acidentes',
                      xaxis = list(title = 'Categoria'),
                      yaxis = list(title = 'Contagem'),
                      showlegend = FALSE)

# Exibir o gráfico
fig


##############################################
# Convertendo coluna para tipo Date
df$data <- as.Date(df$data)
# Supondo que df seja seu DataFrame e a coluna se chame 'data'
class(df$data)

# Transformando em posix
df$data <- as.POSIXct(df$data, format="%Y-%m-%d %H:%M:%S")

# Extraindo ano e mês
df <- df %>%
  mutate(ano_mes = format(data, "%Y-%m"))

# Contando o número de acidentes por ano e mês
df_contagem <- df %>%
  group_by(ano_mes) %>%
  summarise(contagem = n())%>%
  arrange(desc(contagem)) %>%
  top_n(20)

ggplot(df_contagem, aes(x = reorder(ano_mes, -contagem), y = contagem)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 10 Meses com Mais Acidentes", x = "Ano-Mês", y = "Número de Acidentes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#######################################
#Analise por mes

# Filtrando os dados para excluir o ano de 2024
df_filtrado <- df %>%
  filter(format(data, "%Y") != "2024")

# Extraindo o mês
df_mensal <- df_filtrado %>%
  mutate(mes = format(data, "%m")) %>%  # Extraindo o mês como número
  group_by(mes) %>%
  summarise(contagem = n(), .groups = 'drop') %>%
  arrange(as.numeric(mes))  # Ordena os meses numericamente

# Converter a coluna mes para fator com os níveis de 01 a 12
df_mensal <- df_mensal %>%
  mutate(mes = factor(mes, levels = sprintf("%02d", 1:12)))

# Criando o gráfico de barras
ggplot(df_mensal, aes(x = reorder(mes, -contagem), y = contagem)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Número de Acidentes por Mês (Excluindo 2024)", x = "Mês", y = "Número de Acidentes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#################### Por ano agr

# Extraindo o ano
df_anual <- df_filtrado %>%
  mutate(ano = format(data, "%Y")) %>%
  group_by(ano) %>%
  summarise(contagem = n(), .groups = 'drop') %>%
  arrange(ano)  # Ordena os anos

# Percebe-se erro de preenchimento com uma célula na coluna ano com valor 2202
# vamos exluir

# Carregar o pacote dplyr
library(dplyr)

# Filtrar o DataFrame para remover linhas onde a coluna 'ano' é igual a 2202
df_anual <- df_anual %>%
  filter(ano != 2202)

# Verificar o resultado
head(df_anual)

# Criando o gráfico de linhas
ggplot(df_anual, aes(x = ano, y = contagem, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Evolução do Número de Acidentes por Ano", x = "Ano", y = "Número de Acidentes") +
  theme_minimal()


# eliminar linha 2202!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



# Suponha que df seja o seu DataFrame e a coluna se chame 'hora'
df$hora <- hms(df$hora)

# Verificar a estrutura da coluna
str(df$hora)
unique(df$hora)
class(df$hora)



# Suponha que df seja o seu DataFrame e a coluna se chame 'hora'
df$hora <- as.POSIXct(df$hora, format = "%H:%M:%S")

# Verificar a estrutura da coluna
str(df$hora)



##########################################################
# Gravidade dos acidentes 

# Calcular a contagem de cada valor na coluna 'ups'
contagem_ups <- df %>%
  count(ups) %>%
  arrange(desc(n))  # Ordenar por contagem em ordem decrescente

# Garantir que os valores de ups sejam tratados como fatores com a ordem desejada
contagem_ups$ups <- factor(contagem_ups$ups, levels = c(1, 5, 13))

# Criar o gráfico de barras com plotly
fig <- plot_ly(contagem_ups, x = ~ups, y = ~n, type = 'bar', 
               text = ~n, textposition = 'auto',
               marker = list(color = 'skyblue')) %>%
  layout(title = 'Contagem de Valores na Coluna "ups"',
         xaxis = list(title = 'Valores', categoryorder = 'array', categoryarray = c(1, 5, 13)),
         yaxis = list(title = 'Contagem'))

# Exibir o gráfico
fig

#Acidente somente Danos Materiais - Fator de ponderação 1
#Acidente com Feridos - Fator de ponderação 5
#Acidente com Vítimas Fatais - Fator de ponderação 13


######################################################
#Analisando correlaçoes

# Selecionar as colunas de interesse
df_corr <- df %>%
  select(fatais,cont_vit, auto, taxi, moto,lotacao, onibus_met,onibus_urb,onibus_int,caminhao,carroca,bicicleta,patinete)

# Calcular a matriz de correlação
correlation_matrix <- cor(df_corr, method = "pearson", use = "complete.obs")

# Exibir a matriz de correlação
print(correlation_matrix)

# Instalar o pacote, se necessário
#install.packages("corrplot")
#library(corrplot)


# Matriz de correlaçao viual
corrplot(correlation_matrix,
         method = "color",  # Usar cores ao invés de variar o tamanho
         type = "upper",    # Apenas mostrar a parte superior da matriz
         tl.col = "black",  # Cor dos textos das labels (usar branco para contraste)
         tl.srt = 45,       # Ângulo das labels no eixo x
         col = colorRampPalette(c("blue", "white", "red"))(200),  # Gradiente de cor
         is.corr = TRUE,    # Para deixar a escala de cor entre -1 e 1
         cl.lim = c(-1, 1), # Limitar os valores da correlação para -1 e 1
         pch.cex = 2        # Controla o tamanho dos pontos (mantém o mesmo tamanho)
)

##########################################

# Verificar a estrutura da coluna
class(df$hora)


# Converte hora pra posix
df$hora <- as.POSIXct(df$hora, format = "%H:%M:%S")








# Cria uma coluna com a hora formatada

df$hora_formatada <- format(df$hora, "%H:%M:%S")
head(df$hora_formatada)

#Isola apenas a hora
df$horas <- format(df$hora, "%H")
head(df$horas)
#Coluna ta como character
class(df$horas)
# Transforma pra numerico
df$horas <- as.numeric(df$horas)


# Agrupar os dados por hora e contar os acidentes fatais
df_fatal <- df %>%
  group_by(horas) %>%
  summarise(fatalidades = sum(fatais > 0, na.rm = TRUE))

# Criar gráfico de linha para fatalidades por hora
ggplot(df_fatal, aes(x = horas, y = fatalidades)) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "red", size = 2) +  # Adiciona pontos nos dados
  labs(title = "Fatalidades por Hora do Dia", x = "Hora", y = "Número de Fatalidades") +
  scale_x_continuous(breaks = seq(0, 23, 1))  # Mostrar todas as horas do dia







# Cria df para ver dia da semana que têm acidentes mais fatais
df_fatal_dia <- df %>%
  group_by(dia_sem) %>%
  summarise(fatalidades = sum(fatais > 0, na.rm = TRUE))

# Criar gráfico de linha para fatalidades por dia da semana
ggplot(df_fatal_dia, aes(x = dia_sem, y = fatalidades, group = 1)) +  # group = 1 para garantir uma única linha
  geom_line(color = "red", size = 1) +
  geom_point(color = "red", size = 3) +  # Adiciona pontos nos dados
  labs(title = "Fatalidades por Dia da Semana", x = "Dia da Semana", y = "Número de Fatalidades") +
  theme_minimal()  # Estilo minimalista



contagem_ajuda <- table(df$regiao)
print(contagem_ajuda)




# Calcular a contagem de cada valor na coluna 'ups'
contagem_ups <- df %>%
  count(ups) %>%
  arrange(desc(n))  # Ordenar por contagem em ordem decrescente

# Garantir que os valores de ups sejam tratados como fatores com a ordem desejada
contagem_ups$ups <- factor(contagem_ups$ups, levels = c(1, 5, 13))

# Criar o gráfico de barras com plotly
fig <- plot_ly(contagem_ups, x = ~ups, y = ~n, type = 'bar', 
               text = ~n, textposition = 'auto',
               marker = list(color = 'skyblue')) %>%
  layout(title = 'Contagem de Valores na Coluna "ups"',
         xaxis = list(title = 'Valores', categoryorder = 'array', categoryarray = c(1, 5, 13)),
         yaxis = list(title = 'Contagem'))

# Exibir o gráfico
fig



unique(df$ups)




# Criar uma tabela de contigência entre 'regiao' e 'ups'
tabela_contigencia <- df %>%
  count(regiao, ups)

# Exibir a tabela
print(tabela_contigencia)


# Carregar o pacote plotly
library(plotly)

# Criar gráfico de barras empilhadas
fig <- plot_ly(tabela_contigencia, 
               x = ~regiao, 
               y = ~n, 
               color = ~ups, 
               type = 'bar') %>%
  layout(title = 'Relação entre Região e UPS',
         xaxis = list(title = 'Região'),
         yaxis = list(title = 'Contagem'))

# Exibir o gráfico
fig



install.packages("rmdformats")


