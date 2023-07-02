# R version 4.2.3
# Platform: x86_64-w64-mingw32/x64 (64-bit)

### PRE PROCESSAMENTO E CONTRUCAO DO BANCO

# pacotes
library(data.table) # fread
library(survival) 
library(tidyverse) # ggplot, dplyr
library(ggfortify) # autoplot
library(gridExtra) # grid.arrange

### variaveis pensadas previamente
# DIRETO
# maternal_age (ok)
# newborn_weight (ok)
# num_prenatal_appointments (ok)
# tp_maternal_rece (ok)
# num_live_births (ok)
# tp_labor (ok)
# tp_maternal_schooling (ok)
# tp_marital_status (ok)

# lendo o banco
df <- fread("C:/Users/User/Downloads/dados_mortalidade_amostra_aleatoria.csv")

# infos gerais
names(df)
summary(df)

# DESCRITIVO (histograma)
# maternal_age
fig1 <- ggplot(data = df, aes(x = maternal_age)) +
  geom_histogram(color = "white") +
  labs(x = "Idade materna", 
       y = "Frequência")

# tp_maternal_schooling
fig2 <- ggplot(data = df, aes(x = tp_maternal_schooling)) +
  geom_histogram(color = "white") +
  labs(x = "Escolaridade materna", 
       y = "Frequência")

# tp_marital_status
fig3 <- ggplot(data = df, aes(tp_marital_status)) +
  geom_histogram(color = "white") +
  labs(x = "Status marital", 
       y = "Frquência")

# tp_maternal_race
fig4 <- ggplot(data = df, aes(tp_maternal_race)) +
geom_histogram(color = "white") +
  labs(x = "Raça materna", 
       y = "Frquência") 

# num_live_births
fig5 <- ggplot(data = df, aes(num_live_births)) +
  geom_histogram(color = "white") +
  labs(x = "Filhos tidos vivos", 
       y = "Frquência") 

# newborn_weight
# distribuicao normal!
fig6 <- ggplot(data = df, aes(newborn_weight)) +
  geom_histogram(color = "white") +
  labs(x = "Peso do recém nascido", 
       y = "Frquência") 

# num_prenatal_appointments
fig7 <- ggplot(data = df, aes(num_prenatal_appointments)) +
  geom_histogram(color = "white") +
  labs(x = "Número de consultas pré-natais", 
       y = "Frquência")

# tp_labor
fig8 <- ggplot(data = df, aes(tp_labor)) +
  geom_histogram(color = "white") +
  labs(x = "Tipo de parto", 
       y = "Frquência")

# is_neonatal_death
fig9 <- ggplot(data = df, aes(is_neonatal_death)) +
  geom_histogram(color = "white") +
  labs(x = "Mortes neonatais", 
       y = "Frquência")

# birth_date
fig10 <- ggplot(data = df, aes(birth_date)) +
  geom_histogram(color = "white") +
  labs(x = "Data do nascimento", 
       y = "Frquência")

# death_date
fig11 <- ggplot(data = df, aes(death_date)) +
  geom_histogram(color = "white") +
  labs(x = "Data do óbito", 
       y = "Frquência")

# birth_year
fig12 <- ggplot(data = df, aes(birth_year)) +
  geom_histogram(color = "white") +
  labs(x = "Ano de nascimento", 
       y = "Frquência")

grid.arrange(fig1, fig2, fig3, fig4, fig5,fig6, 
             fig7, fig8, fig9, fig10, fig11, fig12,
             ncol = 3, nrow = 4) -> fig_hist

### geom_density
fig1 <- ggplot(data = df, aes(x = maternal_age)) +
  geom_density(color = "white", fill = "blue", alpha = 0.5) +
  labs(x = "Idade materna", 
       y = "Densidade")

fig2 <- ggplot(data = df, aes(x = tp_maternal_schooling)) +
  geom_density(color = "white", fill = "red", alpha = 0.5) +
  labs(x = "Escolaridade materna", 
       y = "Densidade")

fig3 <- ggplot(data = df, aes(tp_marital_status)) +
  geom_density(color = "white", fill = "green", alpha = 0.5) +
  labs(x = "Status marital", 
       y = "Densidade")

fig4 <- ggplot(data = df, aes(tp_maternal_race)) +
  geom_density(color = "white", fill = "orange", alpha = 0.5) +
  labs(x = "Raça materna", 
       y = "Densidade")

fig5 <- ggplot(data = df, aes(num_live_births)) +
  geom_density(color = "white", fill = "purple", alpha = 0.5) +
  labs(x = "Filhos tidos vivos", 
       y = "Densidade")

fig6 <- ggplot(data = df, aes(newborn_weight)) +
  geom_density(color = "white", fill = "cyan", alpha = 0.5) +
  labs(x = "Peso do recém nascido", 
       y = "Densidade")

fig7 <- ggplot(data = df, aes(num_prenatal_appointments)) +
  geom_density(color = "white", fill = "yellow", alpha = 0.5) +
  labs(x = "Número de consultas pré-natais", 
       y = "Densidade")

fig8 <- ggplot(data = df, aes(tp_labor)) +
  geom_density(color = "white", fill = "magenta", alpha = 0.5) +
  labs(x = "Tipo de parto", 
       y = "Densidade")

fig9 <- ggplot(data = df, aes(is_neonatal_death)) +
  geom_density(color = "white", fill = "brown", alpha = 0.5) +
  labs(x = "Mortes neonatais", 
       y = "Densidade")

fig10 <- ggplot(data = df, aes(birth_date)) +
  geom_density(color = "white", fill = "pink", alpha = 0.5) +
  labs(x = "Data do nascimento", 
       y = "Densidade")

fig11 <- ggplot(data = df, aes(death_date)) +
  geom_density(color = "white", fill = "darkgray", alpha = 0.5) +
  labs(x = "Data do óbito", 
       y = "Densidade")

fig12 <- ggplot(data = df, aes(birth_year)) +
  geom_density(color = "white", fill = "skyblue", alpha = 0.5) +
  labs(x = "Ano de nascimento", 
       y = "Densidade")

grid.arrange(fig1, fig2, fig3, fig4, fig5,fig6, 
             fig7, fig8, fig9, fig10, fig11, fig12,
             ncol = 3, nrow = 4) -> fig_density
             
# uf
fig_bar <- ggplot(data = df, aes(x = uf)) +
  geom_bar(color = "white") + # usando geom_bar por se tratar de categorica
  labs(x = "Unidades federativas", y = "Frequência")

ggsave(filename = 'C:\\Users\\User\\Desktop\\dm016\\fig_hist.png',
       plot = fig_density,
       width = 12,
       height = 10)

ggsave(filename = 'C:\\Users\\User\\Desktop\\dm016\\fig_bar.png',
       plot = fig_bar,
       width = 6,
       height = 6)

# selecionando data de nascimento e de morte
df2 <- df[, c("V1", "birth_date", "death_date")]

# criando coluna status
df$status <- ifelse(is.na(df$death_date), 0, 1)

# adicionando 28 aos NAs
df$tempo_dias <- ifelse(is.na(df$tempo_dias), 28, df$tempo_dias)

# verificando obitos
df3 <- subset(df, status == 1)

# adicionando tempo inicio
df$inicio <- 0

# adicionando tempo dias
df$tempo_dias <- difftime(df$death_date, df$birth_date, units = "days")

# tempo horas
#df2_f$tempo_horas <- difftime(df2_f$death_date, df2_f$birth_date, units = "hours")

# removendo a palavra 'days'
df$tempo_dias <- gsub("days", "", df$tempo_dias)

# convertendo para numerico
df$tempo_dias <- as.numeric(df$tempo_dias)

# QUAIS OS FATORES ASSOCIADOS A SOVREVIVENCIA DE RECEM NASCIDOS NO BRASIL ENTRE 2014 A 2016?

# classico
Surv(df$tempo_dias, df$status) # NA nao sofreu o evento (+)

# adicionando 28 dias na coluna tempo_dias
df$tempo_dias <- ifelse(is.na(df$tempo_dias), 28, df$tempo_dias)

# obito s e n
df$obito <- ifelse(df$status == 1, 's', 'n')

# sobrevivencia
fig13 <- df |> ggplot(aes(x = tempo_dias)) +
  geom_histogram(color = "white") + 
  labs(x = "tempo_dias",
       y = 'Frequência')

# eventos
fig14 <- ggplot(data = df[df$status == 1, ], 
       aes(x = tempo_dias)) +
  geom_histogram(color = "white") +
  labs(x = "Tempo (dias) [eventos]", 
       y = "Frequência")

ggsave(filename = 'C:\\Users\\User\\Desktop\\dm016\\fig14.png',
       plot = fig14,
       width = 8,
       height = 6)
