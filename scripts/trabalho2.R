# Carregando os pacotes utilizados

library(dplyr)
library(tidyr)
library(readr)
library(tidymodels)
library(ggplot2)
library(RColorBrewer)
library(ggiraph)
library(ggtext)

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
aux <- trabalho2_dados_7 |> select(where(is.numeric))|> drop_na()
num_vars <- ncol(aux)

# Configuração da área de plotagem e margens
par(mfrow = c(ceiling(num_vars / 2), 2), mar = c(4, 4, 2, 1))

# Criação dos histogramas com linha da média
lapply(seq_along(aux), function(i) {
  hist(
    aux[[i]],
    main = colnames(aux)[i],    # Nome da variável
    xlab = "Valores",           # Rótulo do eixo x
    ylab = "Densidade",         # Rótulo do eixo y
    col = "skyblue",            # Cor do histograma
    border = "white",           # Cor das bordas
    freq = F
  )
  abline(v = mean(aux[[i]], na.rm = TRUE), col = "red", lwd = 2) # Linha da média
  abline(v = median(aux[[i]], na.rm = TRUE),col = "#599861", lty = 2, lwd = 2) # Linha da mediana
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


## Violin plot

trabalho2_dados_7|>
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

correlacoes<-trabalho2_dados_7|>
  drop_na()|>
  select(where(is.numeric))|>
  cor(method = 'kendall')

ggcorrplot::ggcorrplot(correlacoes,
                       type = 'lower',
                       hc.order = T,colors = c('red','white','blue'),
                       lab = T,
                       title = 'Matriz de correlação para variáveis numéricas')

## Mostrar o resumo de relações entre as variáveis numéricas:

trabalho2_dados_7|>
  select(where(is.numeric))|>
  drop_na()|>
  GGally::ggpairs(title = 'Resumo das relações entre variáveis numéricas')



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

trabalho2_dados_7 |>
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

p<-trabalho2_dados_7 |>
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

