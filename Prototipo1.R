library(tidyverse)
library(workflows)
library(parsnip)
library(recipes)
library(yardstick)
library(glmnet)
library(tidyquant)
library(timetk)
library(readxl)


# Ler dados
coleta <- read_excel("/home/emisvaldo/coleta.xlsx")

# Selecione a data e a contagem
coleta_tbl <- coleta %>%
  select(dteday, cnt) %>%
  rename(horario  = dteday,
         uso = cnt)

# Visualize dados e regiões de treinamento / teste
coleta_tbl %>%
  ggplot(aes(x = horario, y = uso)) +
  geom_rect(xmin = as.numeric(ymd_hms("2021-04-19 00:59:57")),
            xmax = as.numeric(ymd_hms("2021-04-19 02:59:57")),
            ymin = 0, ymax = 100,
            fill = palette_light()[[4]], alpha = 0.01) +
  annotate("text", x = ymd_hms("2021-04-18 16:14:04"), y = 95,
           color = palette_light()[[1]], label = "Região de Treino") +
  annotate("text", x = ymd_hms("2021-04-19 01:55:50"), y = 9,
           color = palette_light()[[1]], label = "Região de Teste") +
  geom_point(alpha = 0.5, color = palette_light()[[1]]) +
  labs(title = "Coleta de % de uso de CPU: coleta a cada segundo", x = "") +
  theme_tq()

# Divida em conjuntos de treinamento e teste
train_tbl <- coleta_tbl %>% filter(horario < ymd_hms("2021-04-18 12:13:00"))
test_tbl  <- coleta_tbl %>% filter(horario >= ymd_hms("2021-04-18 12:13:00"))

# Adicionar assinatura de série temporal
recipe_spec_timeseries <- recipe(uso ~ ., data = train_tbl) %>%
  step_timeseries_signature(horario) 

bake(prep(recipe_spec_timeseries), new_data = train_tbl)

