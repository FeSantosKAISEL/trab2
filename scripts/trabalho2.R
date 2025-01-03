# Carregando os pacotes utilizados

library(dplyr)
library(tidyr)
library(readr)
library(tidymodels)
library(ggplot2)
library(RColorBrewer)
library(ggiraph)
library(ggrepel)
library(treemap)
library(forcats)
library(pheatmap)

# Funções internas:

plota_hist<-function(base){
   
  # Seleção de colunas numéricas e cálculo de dimensões
  aux <- base |> select(where(is.numeric))|> drop_na()
  num_vars <- ncol(aux)
  
  aux |> 
    pivot_longer(everything(), names_to = "variavel", values_to = "valor")|>
    group_by(variavel)|>
    mutate(media = mean(valor), mediana = median(valor))|>
    ggplot(aes(x = valor)) +
    geom_histogram(aes(y = ..density..), fill = "skyblue", color = "white", bins = 30) +
    geom_density(color = "blue", size = 1) +
    facet_wrap(~variavel, scales = "free", ncol = 2) +
    geom_vline(aes(xintercept = media), color = "red", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = mediana), color = "green", linetype = "dotted", size = 1) +
    theme_bw() +
    labs(title = "Distribuições das Variáveis Numéricas", x = "Valores", y = "Densidade") +
    theme(
          axis.title = element_blank(),
          panel.grid = element_blank(),
          strip.text = element_text(face = 'bold'),
          axis.ticks.y = element_line(),
          strip.background = element_rect(fill = 'white')
    )
  
  
}

## Carregar a base de dados:

trabalho2_dados_7 <- read_csv("dados/trabalho2_dados_7.csv")

dados<-trabalho2_dados_7

### Consistência dos dados e dados faltantes:

## Visualização de dados faltantes:

visdat::vis_miss(dados)+
  labs(title = "Análise da consistência dos dados")+
  theme(plot.title = element_text(hjust = 0.5))


# Somente 3.3% dos dados estão faltantes.
# Nenhuma variável tem mais de 4% de dados faltantes
# A princípio a remoção dos NAs não seria prejudicial

## Determinar o número de observações concretas para escolher como tratar os NA's

dados|>
  drop_na()|>
  count()

# Existem somente 658 observações completas na nossa base de dados.
# Sendo assim remover os NA's resultaria em uma perda de 40% da nossa base de dados.

## Encontrar quantidade de NA's em cada observação.

dados|>
  rowwise()|>
  mutate(total_NA = rowSums(is.na(across(everything()))))|>
  select(total_NA)|>
  filter(total_NA>1)|>
  arrange(-total_NA)

# Existem 92 observações com mais de uma com mais de um NA's registrado.
# Em 7 observações existem 3 valores faltantes.
# Esse número, a princípio não sugere a remoção de nenhuma observação.

# Uma vez conhecida integralidade dos dados, 
# podemos prosseguir para conhecer as estruras e relações dos atributos.

### NA Imput

## Imputar média (Pouco interessante)
## Imputar mediana (Pouco interessante)
## Imputar com knn: (hyperparametro 'neighbors' no 'chute')

knn_rec <- recipe(~., data = dados) |>
  step_impute_knn(all_predictors(), neighbors = 5)|>
  prep()

dados<-knn_rec|>
  bake(new_data = NULL)

visdat::vis_miss(dados)+
  labs(title = "Análise da consistência dos dados")+
  theme(plot.title = element_text(hjust = 0.5))

## Imputar com bagging (Testar no próximo trabalho)


### Visualizar as estrturas dos dados numéricos:

## Determinar distribuição dos dados (Histogramas, Densidade, Violin_plot)

#dados %>%
#  select(where(is.numeric)) %>%
#  purrr::imap(~ hist(.x, main = .y, xlab = .y, ylab = "Frequência", breaks = 30, freq = F))


# Histogramas
plota_hist(dados)

# Idade: Por conta da assimetria normalizar (observações de 50 e 60 anos altera a média)
# altura: Normalizar
# peso : Normalizar
# consome_vegetais: Normalizar
# n_refeicoes: Discretizar (menos de 3, 3, mais de 3)
# consumo_diario_agua: Nonrmalizar(?)
# frequencia_atividade_fisica: Normalizar
# tempo_usando_eletronico: Normalizar


## Fazer o boxplot (Discutir outliers)

## Fazer gráfico de paineis para os histogramas (Linha para média) (Descutir distribuição)

## Box plot

dados|>
  select(where(is.numeric))|>
  reshape2::melt()|>
  ggplot(aes(x = variable,y = value, fill = variable))+
  geom_boxplot(width = 0.3)+
  facet_wrap(~variable, scales = 'free', ncol = 2)+
  theme_bw()+
  guides(fill = 'none')+
  theme(axis.text.x = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(face = 'bold'),
        axis.ticks.y = element_line(),
        strip.background = element_rect(fill = 'white')
  )


## Fazer o violinplot (Discutir discretização)
## Violin plot

dados|>
  select(where(is.numeric))|>
  reshape2::melt()|>
  ggplot(aes(x = variable,y = value, fill = variable))+
  geom_violin(width = 0.3)+
  guides(fill = 'none')+
  facet_wrap(~variable, scales = 'free', ncol = 2)+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(face = 'bold'),
        axis.ticks.y = element_line(),
        strip.background = element_rect(fill = 'white')
  )

## Ver Corrleações entre variáveis numéricas:

correlacoes<-dados|>
  drop_na()|>
  select(where(is.numeric))|>
  cor(method = 'spearman')

ggcorrplot::ggcorrplot(correlacoes,
                       type = 'lower',
                       hc.order = T,colors = c('red','white','blue'),
                       lab = T,
                       title = 'Matriz de correlação para variáveis numéricas')

## Mostrar o resumo de relações entre as variáveis numéricas:

dados|>
  select(where(is.numeric))|>
  drop_na()|>
  GGally::ggpairs(title = 'Resumo das relações entre variáveis numéricas')+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(face = 'bold'),
        axis.ticks.y = element_line(),
        strip.background = element_rect(fill = 'white')
  )



### Visualizar as estruturas dos dados categóricos:


## Verficar relevância da estratificação

# Contagem por variável categórica:

dados|>
  select(!where(is.numeric))|>
  apply(MARGIN = 2, FUN = ftable)

# Frequência por variável categórica:
dados|>
  select(!where(is.numeric))|>
  apply(MARGIN = 2, FUN = function(x)  ftable(x) / sum(ftable(x)))

## Ver proporções (Fazer gráfico)

cores<- c("#1f77b4", "#08306b", "#ff7f0e", "#7f3f00", "#2ca02c", "#0f4000", 
          "#d62728", "#800000", "#9467bd", "#472d6b", "#8c564b", "#4d2c20", 
          "#e377c2", "#7f0044", "#7f7f7f", "#4c4c4c", "#bcbd22", "#6b6b00", 
          "#17becf", "#005f6b")




# Armazena as proporções para serem usadas no gráfico

proporcoes<-dados |>
  select(!where(is.numeric)) |>
  drop_na() |>
  rownames_to_column() |>
  reshape2::melt(id = 'rowname', value.name = 'value') |>
  group_by(variable, value) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(variable) |>
  mutate(prop = round(n / sum(n),2))


# Ordem das cores nas barras:

ordem<-with(proporcoes,forcats::fct_reorder(interaction(variable, value),
                            prop))

proporcoes|>
  ggplot(aes(x = variable, y = prop, fill = forcats::fct_reorder(interaction(variable, value),
                                                                 prop))) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(data = subset(proporcoes, prop > 0.09 & value != 'frequentemente' ),aes(label = paste(scales::percent(prop, accuracy = 1),"\n",value)),
            position = position_fill(vjust = 0.5),
            size = 3,
            col = "white",
            fontface = 'bold') +
  annotate(geom = 'text', label = paste("10% \n frequentemente"),
           col = "white", size = 3, fontface = 'bold',
           x = 4, y = 0.89)+
  scale_fill_manual(values = cores,
                    breaks = ordem) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(angle = 35, vjust = 1, hjust = 1, size = 10),
    axis.title = element_blank()
  ) +
  guides(
    fill = "none")+
  coord_flip()


## Gráfico interativo de proporções:

p<-proporcoes|>
  ggplot(aes(x = variable, y = prop, fill = forcats::fct_reorder(interaction(variable, value),
                                                                 prop))) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(data = subset(proporcoes, prop > 0.09 & value != 'frequentemente' ),aes(label = paste(scales::percent(prop, accuracy = 1),"\n",value)),
            position = position_fill(vjust = 0.5),
            size = 3,
            col = "white",
            fontface = 'bold') +
  annotate(geom = 'text', label = paste("10% \n frequentemente"),
           col = "white", size = 3, fontface = 'bold',
           x = 4, y = 0.89)+
  scale_fill_manual(values = cores,
                    breaks = ordem) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 35, vjust = 1, hjust = 1, size = 10),
    axis.title = element_blank()
  ) +
  guides(
    fill = "none")

plotly::ggplotly(p)

## Decisão variáveis categóricas:

# Estratificação por sexo

## Resumo numérico estratificado por sexo:

dados|>
  select(where(is.numeric),sexo)|>
  drop_na()|>
  GGally::ggpairs(aes(colour = sexo),
                  title = 'Influência da variável sexo na distribuição dos dados')

# Lumping se come entre as refeições (lumping lógico ou lumping numerico)
# Remover variável 'fuma'
# Lumping da variável meio de transporte (lumping lógico ou lumping numerico)

### Encontrar dados inconsistentes:

##

### Seleção de variáveis:

## Remover variáveis altamente correlacionadas: (Nenhuma variável removida pela correlação.)

## Remover variáveis numéricas de baixa variação: (# Nenhuma variável numérica foi removida.)

## Remover estratificações desnecessárias:

nzv_rec <- recipe(~., data = dados) |>
  step_zv(all_predictors()) |>
  step_nzv(all_predictors())|>
  prep()

# Mostra quais variáveis foram removidas por etapa do pré-process
nzv_rec|>
  tidy(2)

# Variável 'fuma' é removida uma vez que carrega pouca informação.

dados<-nzv_rec|>
  bake(new_data = NULL)

## Remover observações inconsistentes:

##

### Transformação dos dados:

## Discretizar dados numericos:

dados<-dados|>
  mutate(n_refeicoes = case_when(n_refeicoes < 3 ~ 'Menos_3',
                                 n_refeicoes > 3 ~ 'Mais_3',
                                 n_refeicoes == 3 ~ '3_ref',
                                 n_refeicoes == NA ~ NA))

# 608 3 ref (57%)
# 107 Mais_3 (10%)
# 354 Menos_3 (33%)

## Normalizar dados numéricos (step_YeoJohnson)
## Escalonar dados numéricos (step_center + step_scale)

## Reescalar dados numericos:

## range -1, 1
numeric_range_rec<-recipe(~., data = dados) |>
  step_YeoJohnson(all_numeric_predictors()) |>
  step_center(all_numeric_predictors())|>
  step_scale(all_numeric_predictors())|>
  step_range(all_numeric_predictors(), min = -1, max = 1)|>
  prep()


dados_range<-numeric_range_rec|>
  bake(new_data=NULL)

# Histogramas após transformações:

plota_hist(dados_range)

## Percentil:

numeric_percent_rec<-recipe(~., data = dados) |>
  step_YeoJohnson(all_numeric_predictors()) |>
  step_center(all_numeric_predictors())|>
  step_scale(all_numeric_predictors())|>
  step_percentile(all_numeric_predictors())|>
  prep()

dados_percent<-numeric_percent_rec|>
  bake(new_data = NULL)

# Histogramas após transformações:

plota_hist(dados_percent)

## Agrupar variáveis categóricas pouco frequentes (Lumping - numérico) 
#Agrupou 'moto, bicicleta, andando' - meio_transporte
#Agrupou 'não e sempre' - come_entre_refeições
## Numerizar variáveis categóricas (Binarizar - One-Hot - Dummy Encode)

# One hot 25 variáveis + colinearidade
# Dummy 18 variaveis
# binarizar 14 variáveis

cat_rec<-recipe(~. , data = dados_range)|>
  step_other(all_nominal_predictors())|>
  step_dummy(all_nominal_predictors())|>
  prep()

dados_dummy<-cat_rec|>
  bake(new_data = NULL)

## Binarizar:

cat_rec_bin<-recipe(~. , data = dados_range)|>
  step_other(all_nominal_predictors())|>
  extrasteps::step_encoding_binary(all_nominal_predictors())|>
  prep()

dados_binarized<-cat_rec_bin|>
  bake(new_data = NULL)

### Redução de dimensionalidade com PCA (Ordem de fazer a redução)

pca_rec<-recipe(~. , data = dados_range)|>
  step_pca(all_numeric_predictors())|>
  step_other(all_nominal_predictors())|>
  step_dummy(all_nominal_predictors())|>
  prep()

dados_pca<-pca_rec|>
  bake(new_data = NULL)


# 16 variáveis com PCA x 18 variáveis sem PCA pouca diferença por ter poucas variáveis categóricas:





#identificando variaveis dependentes pelo teste qui quadrado

vars <- dados |>
  select(!where(is.numeric)) |> colnames()

for (i in 1:(length(vars) - 1)) {
  for (j in (i + 1):length(vars)) {
    tabela <- table(dados[[vars[i]]], dados[[vars[j]]])
    teste <- chisq.test(tabela)
    if (teste$p.value < 0.0001 ) {
      print(c(vars[i], vars[j]))
    }
  }
}


# come_entre_refeicoes X sexo

test1 <- dados |> select(sexo, come_entre_refeicoes) |> table() |> chisq.test()


pheatmap(test1$residuals,
               display_numbers = TRUE,
               cluster_rows = FALSE,
               cluster_cols = FALSE,
               main = "come_entre_refeicoes X sexo"
)

# sexo X tipo_transporte
test2 <- dados |> select(sexo, tipo_transporte) |> table() |> chisq.test()

pheatmap(test2$residuals,
               display_numbers = TRUE,
               cluster_rows = FALSE,
               cluster_cols = FALSE,
               main = "sexo X tipo_transporte"
)
# consome_comida_calorica X come_entre_refeicoes

test3 <- dados |> select(consome_comida_calorica, come_entre_refeicoes) |> table() |> chisq.test()

pheatmap(test3$residuals,
               display_numbers = TRUE,
               cluster_rows = FALSE,
               cluster_cols = FALSE,
               main = "come_entre_refeicoes X consome_comida_calorica"
)

#tipo_transporte X come_entre_refeicoes
test4 <- dados |> select(come_entre_refeicoes, tipo_transporte) |> table() |> chisq.test()

pheatmap(test4$residuals,
               display_numbers = TRUE,
               cluster_rows = FALSE,
               cluster_cols = FALSE,
               main = "tipo_transporte X come_entre_refeicoes"
)






# Relação entre categóricas após a discretização de n_ref:


# come_entre_refeicoes X n_refeicoes
test2 <- dados |> select(n_refeicoes, come_entre_refeicoes) |> table() |> chisq.test()

pheatmap(test2$residuals,
         display_numbers = TRUE,
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         main = "come_entre_refeicoes X n_refeicoes",
)