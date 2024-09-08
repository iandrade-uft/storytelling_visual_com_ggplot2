#install.packages("ggplot2")
library(ggplot2)

ggplot(data = mtcars, 
       aes(x = mpg, 
           y = hp)) +
  geom_point()

ggplot(data = mtcars, 
       aes(x = mpg, 
           y = hp)) +
  geom_point(aes(color = factor(cyl))) +
  labs(title = "Relação entre MPG e HP", 
       x = "Milhas por Galão", 
       y = "Potência (HP)", 
       color = "Cilindros") +
  theme_classic() 


TM <- c(23.4,19.8,12.8,16.3,20.8,17.4,20.0,21.8,21.8,20.6,20.3,20.6,20.4,18.1,20.2,19.3,17.2,20.8,20.7,17.4,21.8,23.8,25.8,26.3,25.3,24.5,21.4,23.3,24.3,21.2,24.3,23.6,23.5,22.3,21.4,19.7,22.1,20.5,22.3,21.8,18.0,21.3,24.1,23.9,23.6,23.3,23.2,22.0,22.4,20.8,20.2,22.6,23.7,19.9,19.7,21.4,22.5,21.4,20.4,24.5,22.7,20.3,23.7,24.0,23.6,21.6,22.0,22.6,21.3,22.5,22.2,26.3,27.2,28.3,25.9,25.8,26.6,24.9,20.8,19.4,21.6,22.2,23.8,20.9,22.9,25.0,23.7,23.8,24.8,24.8,24.8,25.4,24.4,23.5,24.7,25.3,25.2,23.8,22.8,22.2,26.0,27.8,28.1,25.8,26.8,25.3,25.0,26.6,26.4,26.7,26.8,25.5,24.0,23.2,22.6,23.4,24.5,25.7,25.0,26.4,26.2,26.2,26.9,24.7,25.6,25.0,23.7,22.8,25.5,26.3,26.9,25.1,26.7,25.6,24.5,26.2,26.2,24.4,26.3,25.6,24.4,24.0,26.7,28.2,26.3,26.7,25.4,24.8,24.6,26.3,28.7,28.6,26.3,28.6,29.0,28.2,24.3,23.0,22.9,24.6,26.6,28.5,28.0,25.5,23.2,23.7,23.0,22.4,23.6,23.6,23.5,23.5,22.9,23.5)
UR <- c(68,93,86,55,54,51,45,43,55,54,58,57,64,89,73,80,96,71,86,95,74,62,49,43,51,62,86,73,64,95,68,77,86,93,76,63,69,94,88,89,88,67,76,84,71,88,83,83,74,54,51,61,74,97,94,97,66,58,65,56,82,93,66,64,67,65,67,67,63,62,76,51,57,54,80,65,65,65,93,88,63,68,65,98,83,64,67,62,59,78,75,70,63,62,53,46,42,55,60,51,51,47,42,60,62,77,74,58,63,67,66,83,81,87,95,80,71,68,74,69,75,74,75,90,86,91,91,98,84,81,74,82,69,77,84,78,74,87,75,80,89,90,77,73,82,80,82,75,79,70,61,63,74,63,58,62,76,76,74,69,64,56,61,86,94,85,78,91,82,80,81,85,89,84)
TEMPO <- c(1:174)

dados <- data.frame(TEMPO, TM, UR)

ggplot(data = dados, 
       aes(x = TEMPO, 
           y = TM)) +
  geom_line() 

ggplot(data = dados, 
       aes(x = TEMPO, 
           y = TM)) +
  geom_line(color = "blue") 

ggplot(data = dados, 
       aes(x = TEMPO, 
           y = TM)) +
  geom_line(color = "blue", 
            size = 1.2) 

ggplot(data = dados, 
       aes(x = TEMPO, 
           y = TM)) +
  geom_line(color = "blue", 
            size = 1.2, 
            linetype = "dashed") 

ggplot(data = dados, 
       aes(x = TEMPO, 
           y = TM)) +
  geom_line() +
  geom_point() 

ggplot(data = dados, 
       aes(x = TEMPO)) +
  geom_line(aes(y = TM, 
                colour = "Umidade (%)")) +
  geom_line(aes(y = UR, 
                colour = "Temperatura (°C)")) 


# Carregando a base de dados
data(starwars)

# Carregando o pacote ´dplyr´
library(dplyr)

# Resumo estrutural da base de dados
glimpse(starwars)

# Visualizando as primeiras linhas da base de dados
head(starwars)

# Resumindo a distribuição das cores dos olhos
starwars |> 
  group_by(eye_color) |> 
  summarise(n = n())

# Vamos ao gráfico de Barras
starwars |> 
  group_by(eye_color) |> 
  summarise(n = n()) |> 
      ggplot(aes(x = eye_color, 
                 y = n)) +
  geom_bar(stat = "identity") # stat = identity para usar a métrica sem transformar.


## ----------------------------------------------------------------------
starwars |> 
  group_by(eye_color) |> 
  summarise(n = n()) |> 
      ggplot(aes(x = eye_color, 
                 y = n, 
                 fill = eye_color)) +
  geom_bar(stat = "identity") 


# Colorindo as Barras em função da variável categórica
starwars |> 
  group_by(eye_color) |> 
  summarise(n = n()) |> 
  ggplot(aes(x = reorder(eye_color, n), 
             y = n, 
             fill = eye_color)) +
  geom_bar(stat = "identity") 


# Reordenando do maior para o menor
starwars |> 
  group_by(eye_color) |> 
  summarise(n = n()) |> 
  ggplot(aes(x = reorder(eye_color, -n), 
             y = n, 
             fill = eye_color)) +
  geom_bar(stat = "identity") 


## Incluindo Rótulos nas Barras
starwars |> 
  group_by(eye_color) |> 
  summarise(n = n()) |> 
  ggplot(aes(x = reorder(eye_color, -n), 
             y = n,
             label = n)) +
  geom_bar(stat = "identity") +
  geom_label() # este comando adiciona os rótulos 


## Invertendo os Eixos
starwars |> 
  group_by(eye_color) |> 
  summarise(n = n()) |> 
  ggplot(aes(x = reorder(eye_color, n), 
             y = n,
             label = n)) +
  geom_bar(stat = "identity") +
  geom_label() +
  coord_flip() # este comando inverte os eixos


## Mapeando uma terceira variável (gênero)
starwars |> 
  group_by(eye_color, gender) |> 
  summarise(n = n())


## Resume os dados e cria um gráfico de barras empilhadas
starwars |> 
  group_by(eye_color, gender) |>
  summarise(n = n()) |>
  ggplot(aes(x = eye_color, 
             y = n,
             fill = gender, # fill adiciona cores na 3o variável: gender
             label = n)) + 
  geom_bar(stat = "identity") +
  geom_label(position = "stack") # especificar a posição por ser gráfico empilhado


## ----------------------------------------------------------------------
starwars |> 
      filter(eye_color %in% c("brown", "blue")) |>
  group_by(eye_color, gender) |>
  summarise(n = n()) |>
      na.omit() |>
  ggplot(aes(x = eye_color, 
             y = n,
             fill = gender, # fill adiciona cores ao 2o fator: gender
             label = n)) + 
  geom_bar(stat = "identity", position="dodge") +
  geom_label(position = position_dodge(width = 1)) # especificar a posição lado a lado


## Vejamos outro conjunto de dados
library(scales)
library(BatchGetSymbols)
 
bvsp = BatchGetSymbols('^BVSP', 
                       first.date = as.Date('2024-01-01'),
                       last.date = as.Date('2024-09-03'))
 
p <- bvsp$df.tickers |> 
      ggplot(aes(x = ref.date, 
                 y = price.close))+
      geom_line(size = 1.4)+
      scale_x_date(breaks = date_breaks("1 month"),
                   labels = date_format("%b"))+
      labs(x = 'Mês',
           y = 'Pontos')


## ----fig.width = 15, fig.height = 10-----------------------------------
library(cowplot)

plot_grid(p, 
          p + theme_bw(), 
          p + theme_grey(), 
          p + theme_gray(), 
          p + theme_linedraw(), 
          p + theme_light(), 
          p + theme_minimal(), 
          p + theme_classic(), 
          p + theme_dark(),
          labels = c("cowplot", 
                     "bw", 
                     "gray", 
                     "grey", 
                     "linedraw", 
                     "light", 
                     "minimal", 
                     "classic", 
                     "dark"),
          nrow = 3, 
          align = "h", 
          hjust = 0, 
          vjust = 1, 
          label_size = 10)


# Vejamos um período mais longo
bvsp = BatchGetSymbols('^BVSP', 
                       first.date = as.Date('2000-01-01'),
                       last.date = as.Date('2024-09-03'))

p <- bvsp$df.tickers |> 
      ggplot(aes(x = ref.date, 
                 y = price.close))+
      geom_line(size = 1.4)+
      scale_x_date(breaks = date_breaks("1 year"),
                   labels = date_format("%y"))+
      labs(x = 'Ano',
           y = 'Pontos')

## Gráfico de Dispersão (de pontos / scatter)
flores <- iris
flores <- flores %>% 
      rename(comp_sepala = Sepal.Length,
             larg_sepala = Sepal.Width,
             comp_petala = Petal.Length,
             larg_petala = Petal.Width,
             especie = Species)
head(flores)
# Graf 1
ggplot(data = flores,
       mapping = aes(
             x = comp_sepala,
             y = comp_petala))+
      geom_point(aes(shape = especie))

## Mapas
library(tidyverse)
library(geobr)
library(ggspatial)
biomas <- read_biomes(year = 2019)

biomas$name_biome
# Remover os sistemas costeiros
biomas <- biomas  |> 
      filter(name_biome != "Sistema Costeiro")

ggplot() +
      geom_sf(data = biomas,
              aes(fill=name_biome))+
      # Mudar as cores de cada bioma
      scale_fill_manual(values = c("#152",
                                   "#ff8",
                                   "#a62",
                                   "#4c9",
                                   "#2f7",
                                   "#24f"))+
      # Mudando o título da Legenda
      labs(fill = "Biomas")+
      annotation_scale(location = "br",
                       pad_x = unit(.2, "cm"),
                       pad_y = unit(.7, "cm")) +
      annotation_north_arrow(location = "br", 
                             which_north = "true",
                             pad_x = unit(.2, "cm"), 
                             pad_y = unit(1.3, "cm"),
                             style = north_arrow_fancy_orienteering)+
      labs(title = "Biomas do Brasil", 
           x = "Longitude", 
           y = "Latitude") +
      theme_bw() +
      theme(title = element_text(size = 8, face = "bold"),
            legend.title = element_text(size = 6, face = "bold"))+
      annotate(geom = "text", 
               label = "Fonte: IBGE 2020", 
               x = -71, y = -32, size = 3)+
      annotate(geom = "text", 
               label = "Elaborado por: Prof. Dr. Fernando M. Haesbaert", 
               x = -65, y = -33.5, size = 3)



## ----------------------------------------------------------------------
paleta_uft_3a <- c("#fbb840", "#054680", "#07b17f")

paleta_uft_3b <- c("#0d64af", "#f8d84e", "#17dfa6")

paleta_uft_6a <- c("#df8e41", "#fbb840", "#f8d84e", 
                  "#0d64af", "#054680", "#032554") 

paleta_uft_6b <- c("#df8e41", "#fbb840", "#f8d84e", 
                  "#17dfa6", "#07b17f", "#058274") 

paleta_uft_9a <- c("#058274", "#07b17f","#17dfa6", 
                  "#df8e41", "#fbb840", "#f8d84e", 
                  "#0d64af", "#054680", "#032554") 


## ----------------------------------------------------------------------
ggplot(data = mtcars, 
       aes(x = mpg, 
           y = hp)) +
  geom_point()  ggsave("grafico.png") 

