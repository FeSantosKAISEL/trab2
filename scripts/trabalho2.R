# Carregando os pacotes utilizados

library(dplyr)
library(tidyr)
library(readr)
library(tidymodels)
library(ggplot2)
library(RColorBrewer)

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


## Verficar relevância da estratificação

trabalho2_dados_7|>
  select(!where(is.numeric))|>
  apply(MARGIN = 2, FUN = ftable)

## Ver proporções (Fazer gráfico)

cores<- c("#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", 
          "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", 
          "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D", 
          "#8A7C64", "#599861")

trabalho2_dados_7|>
  select(!where(is.numeric))|>
  drop_na()|>
  rownames_to_column()|>
  reshape2::melt(id = 'rowname',value.name = 'value')|>
  group_by(value)|>
  mutate(n = n())|>
    ggplot(aes(x = variable, fill = interaction(variable,value)))+
  geom_bar(position = 'fill')+
  guides(fill = 'none')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 35,
                                   vjust = 1,
                                   hjust = 1,
                                   size = 14),
        axis.title = element_blank())+
  scale_fill_manual(values = cores)+
  geom_text(aes(label = ..count..), stat = "count", position = position_fill(vjust = 0.5), size = 3)


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

