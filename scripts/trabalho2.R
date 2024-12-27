# Carregando os pacotes utilizados

library(dplyr)
library(tidyr)
library(readr)
library(tidymodels)
library(ggplot2)


## Carregar a base de dados:

trabalho2_dados_7 <- read_csv("dados/trabalho2_dados_7.csv")

### Consistência dos dados e dados faltantes:

## Visualização de dados faltantes:

visdat::vis_miss(trabalho2_dados_7)+
  labs(title = "Análise da consistência dos dados")+
  theme(plot.title = element_text(hjust = 0.5))


# Somente 3.3% dos dados estão faltantes.
# Nenhuma variável tem mais de 4% de dados faltantes
# A princípio a remoção dos NAs não seria prejudicial

## Determinar o número de observações concretas para escolher como tratar os NA's

trabalho2_dados_7|>
  drop_na()|>
  count()

# Existem somente 658 observações completas na nossa base de dados.
# Sendo assim remover os NA's resultaria em uma perda de 40% da nossa base de dados.

## Encontrar quantidade de NA's em cada observação.

trabalho2_dados_7|>
  rowwise()|>
  mutate(total_NA = rowSums(is.na(across(everything()))))|>
  select(total_NA)|>
  filter(total_NA>0)|>
  arrange(-total_NA)

# Existem 432 observações com mais de uma com mais de um NA's registrado.
# Em 7 observações existem 3 valores faltantes.
# Esse número, a princípio não sugere a remoção de nenhuma observação.


# Uma vez conhecida integralidade dos dados, 
# podemos prosseguir para conhecer as estruras e relações dos atributos.

### Visualizar as estrturas dos dados numéricos:

## Determinar distribuição dos dados (Histogramas, Densidade, Violin_plot)

trabalho2_dados_7 %>%
  select(where(is.numeric)) %>%
  purrr::imap(~ hist(.x, main = .y, xlab = .y, ylab = "Frequência", breaks = 30, freq = F))


# Histogramas
# Seleção de colunas numéricas e cálculo de dimensões
aux <- trabalho2_dados_7 |> select(where(is.numeric))
num_vars <- ncol(aux)

# Configuração da área de plotagem e margens
par(mfrow = c(ceiling(num_vars / 2), 2), mar = c(4, 4, 2, 1))

# Criação dos histogramas com linha da média
lapply(seq_along(aux), function(i) {
  hist(
    aux[[i]],
    main = colnames(aux)[i],    # Nome da variável
    xlab = "Valores",           # Rótulo do eixo x
    ylab = "Frequência",        # Rótulo do eixo y
    col = "skyblue",            # Cor do histograma
    border = "white"            # Cor das bordas
  )
  abline(v = mean(aux[[i]], na.rm = TRUE), col = "red", lwd = 2) # Linha da média
})

# Reseta a área de plotagem ao padrão
par(mfrow = c(1, 1))




# Idade: Por conta da assimetria normalizar (observações de 50 e 60 anos altera a média)
# altura: Nada a fazer
# peso : Nada a fazer
# consome_vegetais: Normalizar
# n_refeicoes: Discretizar (menos de 3, 3, mais de 3)
# consumo_diario_agua: Nonrmalizar(?)
# frequencia_atividade_fisica: Normalizar
# tempo_usando_eletronico: Normalizar



## Fazer o violinplot (Discutir discretização)
## Fazer o boxplot (Discutir outliers)

## Fazer gráfico de paineis para os histogramas (Linha para média) (Descutir distribuição)

## Box plot

trabalho2_dados_7|>
  select(where(is.numeric))|>
  reshape2::melt()|>
  ggplot(aes(x = variable,y = value, fill = variable))+
  geom_boxplot()+
  facet_wrap(~variable, scales = 'free_y', ncol = 2)+
  theme_bw()+
  guides(fill = 'none')+
  theme(axis.text.x = element_blank(),
        axis.title = element_blank())


## Violin plot

trabalho2_dados_7|>
  select(where(is.numeric))|>
  reshape2::melt()|>
  ggplot(aes(x = variable,y = value, fill = variable))+
  geom_violin()+
  guides(fill = 'none')+
  facet_wrap(~variable, scales = 'free_y', ncol = 2)+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.title = element_blank(),
        panel.border = element_blank() 
  )

## Ver Corrleações entre atributos

correlacoes<-trabalho2_dados_7|>
  drop_na()|>
  select(where(is.numeric))|>
  cor()

par(oma = c(0, 0, 0, 0))

corrplot::corrplot.mixed(correlacoes, order = "hclust", tl.pos = "lt", 
                         upper = "ellipse")



### Visualizar as estruturas dos dados categóricos:

## Ver proporções (Fazer gráfico)
## Verficar relevância da estratificação

trabalho2_dados_7|>
  select(!where(is.numeric))|>
  apply(MARGIN = 2, FUN = ftable)

trabalho2_dados_7|>
  select(!where(is.numeric))|>
  rownames_to_column()|>
  reshape2::melt(id = 'rowname',value.name = 'value')|>
  ggplot(aes(x = variable, fill = value))+
  geom_bar(position = 'fill')

### Seleção de variáveis:

## Remover variáveis altamente correlacionadas
## Remover dados inconsistentes
## Remover variáveis de baixa variação
## Remover estratificações desnecessárias


### Encontrar dados inconsistentes:

### Transformação dos dados:

## Normalizar dados numéricos (?)
## Escalonar dados numéricos (Centrar na média?, centrar na mediana?, normalizar antes de centralizar?)
## Colocar valores em percentis? Re-escalar pelo máximo e mínimo.
## Agrupar variáveis categóricas pouco frequentes (Lumping)
## Numerizar variáveis categóricas (Binarizar - One-Hot - Dummy Encode)


### NA Imput

## Imputar média
## Imputar mediana
## Imputar com knn
## Imputar com bagging


### Redução de dimensionalidade com PCA

