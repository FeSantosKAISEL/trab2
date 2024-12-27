# Carregando os pacotes utilizados

library(dplyr)
library(tidyr)
library(readr)
library(tidymodels)

## Carregar a base de dados:

trabalho2_dados_7 <- read_csv("dados/trabalho2_dados_7.csv")

### Consistência dos dados e dados faltantes:

## Visualização de dados faltantes:

visdat::vis_miss(trabalho2_dados_7) 

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
  purrr::imap(~ hist(.x, main = .y, xlab = .y, ylab = "Frequência"))

## Ver Corrleações entre atributos
## 


### Visualizar as estruturas dos dados categóricos:

## Ver proporções (Fazer gráfico)
## Verficar relevância da estratificação

trabalho2_dados_7|>
  select(!where(is.numeric))|>
  apply(MARGIN = 2, FUN = ftable)


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

