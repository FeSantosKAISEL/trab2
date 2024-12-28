# Carregando os pacotes utilizados

library(dplyr)
library(tidyr)
library(readr)
library(tidymodels)
library(ggplot2)
library(RColorBrewer)
library(ggiraph)
library(ggrepel)

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
    theme_minimal() +
    labs(title = "Distribuições das Variáveis Numéricas", x = "Valores", y = "Densidade")
  
  
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

cores<- c("#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", 
          "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", 
          "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D", 
          "#8A7C64", "#599861")

dados |>
  select(!where(is.numeric)) |>
  drop_na() |>
  rownames_to_column() |>
  reshape2::melt(id = 'rowname', value.name = 'value') |>
  group_by(variable, value) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(variable) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = variable, y = prop, fill = interaction(variable, value))) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text_repel(aes(label = paste(scales::percent(prop, accuracy = 1),"\n",value)),
                  position = position_fill(vjust = 0.5),
                  size = 3,
                  col = "black",
                  direction = "y",fontface = 'bold') +
  scale_fill_manual(values = cores) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 35, vjust = 1, hjust = 1, size = 14),
    axis.title = element_blank()
  ) +
  guides(
    fill = "none")

## Gráfico interativo de proporções:

p<-dados |>
  select(!where(is.numeric)) |>
  drop_na() |>
  rownames_to_column() |>
  reshape2::melt(id = 'rowname', value.name = 'value') |>
  group_by(variable, value) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(variable) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = variable,
             y = prop,
             fill = interaction(variable, value)))+ 
  geom_bar_interactive(aes(data_id = value,
                           tooltip = value),
                       stat = "identity",
                       position = "fill"
  ) +
  geom_text_repel(aes(label = scales::percent(prop, accuracy = 1)),
                  position = position_fill(vjust = 0.5),
                  size = 8,
                  col = "black",
                  direction = "y",
                  fontface = 'bold') +
  scale_fill_manual(values = cores) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 35, vjust = 1, hjust = 1, size = 14),
    axis.title = element_blank()
  ) +
  guides(
    fill = "none")


girafe(ggobj = p,
       width_svg = 22,
       height_svg = 10,
       options = list(
         opts_hover(css = 'stroke'),
         opts_hover_key(css = 'stroke;r:8pt;cursor:pointer;'),
         opts_selection(css = 'fill;opacity:1',type = 'single', only_shiny = F),
         opts_selection_key(css = 'r:5pt;opacity:1',type = 'single', only_shiny = F),
         opts_selection_inv(css = 'opacity:0.1'),
         opts_hover_theme(),
         opts_tooltip(
           offx = 0,
           offy = 10,
           css = "background-color:#555555;color:white;padding:10px;
                  border-radius:10px;box-shadow:1px 1px 1px rgba(0.5,0.5,0.5,0.3);
                  font-family:roboto;font-size:15px;", # características da caixa de texto
           opacity = 0.7, # transparência da caixa
           use_fill = F,use_cursor_pos = T)
       )
)

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


### Redução de dimensionalidade com PCA (Ordem de fazer a redução)

pca_rec<-recipe(~. , data = dados_range)|>
  step_pca(all_numeric_predictors(), threshold = 0.9)|>
  step_other(all_nominal_predictors())|>
  step_dummy(all_nominal_predictors())|>
  prep()

dados_pca<-pca_rec|>
  bake(new_data = NULL)


# Reduz de 18 para 16 variáveis (Poucas variáveis numéricas deixam PCA pouco util nesse caso)