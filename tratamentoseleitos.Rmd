```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)

# Carregar pacotes
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)

# Carregar os dados e sinalizar primeira linha como título
df1 <- read_excel("df1.xlsx")
df2 <- read_excel("df2.xlsx")

#Primeira linha = titulo
head(df1)
head(df2)

df1$`Mês/Ano` <- as.Date(df1$`Mês/Ano`, format = "%m/%Y")

#Filtrando os dados para 2024
dados1_2024 <-df1 %>%
 filter(format(`Mês/Ano`, "%Y") == ("2024"))

# Total dos grupos
relatorio_padrao <- dados1_2024 %>%
  mutate(
    Mês =format(`Mês/Ano`,"%B"),
         Tratamento = ifelse(is.na(`Forma de organização`), `Sub-grupo`, paste(`Sub-grupo`, `Forma de organização`, sep = "-"))) %>%
  group_by(Tratamento, Mês) %>%
  summarise(Total = n(), .groups = 'drop')

totais_gerais <- relatorio_padrao %>%
  separate(Tratamento, into = c("Sub-grupo", "Forma de organização"), sep = "-", extra = "merge", fill = "right") %>%
  group_by(`Sub-grupo`, Mês) %>%
  summarise(Total = sum(Total), .groups = 'drop') %>%
  mutate(Tratamento = paste(`Sub-grupo`, "Total", sep = "-")) %>%
  select(-`Sub-grupo`)

relatorio_completo <- bind_rows(relatorio_padrao, totais_gerais)

#convertendo formato da tabela
df1_wide <- relatorio_completo %>% 
  pivot_wider(names_from = `Mês`, values_from = Total, values_fill = list(Total = 0)) %>%
  arrange(Tratamento)

print(df1_wide)

#df2
df2$`Mês/Ano` <- as.Date(df2$`Mês/Ano`, format = "%m/%Y")

#define o mês
df2 <- df2 %>%
  mutate(Mês = month(`Mês/Ano`, label = TRUE, abbr = FALSE, locale = "pt_BR"))

#calculo de porcentagem
df2 <- df2 %>%
  mutate(Porcentagem_Ocupacao = (`Paciente dia` / `Leito dia funcional`) * 100)

#agrupar por unidade funcional e mês - calcular a média de porcentagem de ocupação
relatorio_ocupacao <- df2 %>%
  group_by (`Unidade Funcional`, Mês) %>%
  summarise(Media_Ocupacao = mean(Porcentagem_Ocupacao, na.rm = TRUE))

#convertendo formato da tabela
relatorio_ocupacao_wide <- relatorio_ocupacao %>%
  mutate(Media_Ocupacao = round(Media_Ocupacao, 2)) %>% #arredondar porcentagem
  pivot_wider(names_from = `Mês`, values_from = Media_Ocupacao, values_fill = 0) %>%
  arrange(factor(`Unidade Funcional`, levels = paste0("C", 1:15))) #ordenar de c1 a c15
  
#exibir relatorio
print(relatorio_ocupacao)
```
