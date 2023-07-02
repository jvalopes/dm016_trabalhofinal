
### KAPLAN-MEIER

### variaveis pensadas previamente
# DIRETO
# maternal_age 
# newborn_weight
# num_prenatal_appointments 
# tp_maternal_rece
# num_live_births
# tp_labor
# tp_marital_status
###

# tempo e status
y <- Surv(df$tempo_dias, df$status)
y

# analise km para tempo e status
KM <- survfit(y ~ 1, data = df)
KM

summary(KM)

# s(t)
fig15 <- autoplot(KM, conf.int = T) +
  labs(x = "Tempo (dias)", 
       y = "Probabilidade de sobrevivência",
       title = "A")

# f(t)
fig16 <- autoplot(KM, conf.int = T, fun = 'cumhaz') +
  labs(x = "Tempo (dias)", 
       y = "Distribuição do tempo de vida")


ggsave(filename = 'C:\\Users\\User\\Desktop\\dm016\\fig16.png',
       plot = fig16,
       width = 8,
       height = 6)

# 28 dias
df28 <- df[df$tempo_dias < 28, ] # [r, c]
z <- Surv(df28$tempo_dias, df28$status)
KM2 <- survfit(z ~ 1, data = df28)
KM2

summary(KM2)

# grafico
fig17 <- autoplot(KM2) +
  labs(x = "Tempo < 28 dias",
       y = "Probabilidade de sobrevivência",
       title = "B")

grid.arrange(fig15, fig17, ncol = 2) -> fig15_17

ggsave(filename = 'C:\\Users\\User\\Desktop\\dm016\\fig15_17.png',
       plot = fig15_17,
       width = 8,
       height = 6)

# estimador estimador nelson aalen
sobrev.na <- survfit(coxph(Surv(tempo_dias,status) ~ 1, data = df28)) # aqui uma variação para estimar NA (uma função dentro da outra)

sobrev.na
summary(sobrev.na)

# ambos 
# comparando aalen e km
# s(t) sobrevida

fig18 <- autoplot(KM2, conf.int = T) + 
  labs(x = "Tempo (dias)", 
       y = "Probabilidade de sobrevivência",
       title = "Kaplan-Meier")

fig19 <- autoplot(sobrev.na, conf.int = T) +
  labs(x = "Tempo (dias)", 
       y = "Probabilidade de sobrevivência",
       title = "Nelson-Aalen")

grid.arrange(fig18, fig19, nrow = 1) -> fig18_19
ggsave(filename = 'C:\\Users\\User\\Desktop\\dm016\\fig18_19.png',
       plot = fig18_19,
       width = 8,
       height = 6)

# f(t) risco
fig20 <- autoplot(KM2, 
                  conf.int = T, 
                  fun = "cumhaz") +
  labs(x = "Tempo (dias)", 
       y = "Distribuição do tempo de vida",
       title = "Kaplan-Meier") +
  coord_cartesian(ylim = c(0, 8))

fig21 <- autoplot(sobrev.na, 
                  conf.int = T, 
                  fun = "cumhaz") + 
  labs(x = "Tempo (dias)", 
       y = "Distribuição do tempo de vida",
       title = "Nelson-Aalen") +
  coord_cartesian(ylim = c(0, 8))

grid.arrange(fig20, fig21, nrow = 1) -> fig20_21
ggsave(filename = 'C:\\Users\\User\\Desktop\\dm016\\fig20_21.png',
       plot = fig20_21,
       width = 8,
       height = 6)

summary(KM2)
summary(sobrev.na)

### ESTRATIFICACAO
# idade materna
# categorias idade materna: 12 - 22, 23 - 33, 34 - 44, 45 - 55

# criando das categorias
df_cat_maternal_age <- df |>
  mutate(age_category = cut(maternal_age,
                            breaks = c(12, 22, 33, 44, 55),
                            labels = c("12-22", "23-33", "34-44", "45-55"),
                            include.lowest = TRUE))

# visualizando resultados
ggplot(df_cat_maternal_age, aes(x = age_category)) +
  geom_bar() +
  labs(x = "",
       y = "")

# km estratificado pelas categorias de idade
m <- survfit(Surv(tempo_dias, status) ~ age_category, data = df_cat_maternal_age)
m # identifica categorias com n = 0
summary(m)

# rm n = 0
df_cat_maternal_age <- df_cat_maternal_age[df_cat_maternal_age$age_category != '45-55', ]

# km banco limpo
m <- survfit(Surv(tempo_dias, status) ~ age_category, data = df_cat_maternal_age)

# visualizando resultados (apos rm n = 0)
ggplot(df_cat_maternal_age, aes(x = age_category)) +
  geom_bar() +
  labs(x = "",
       y = "")

# ajustes para o grafico
# ajustar o modelo de sobrevivencia
m <- survfit(Surv(tempo_dias, status) ~ age_category, data = df_cat_maternal_age)

# grafico
fig22 <- autoplot(m, conf.int = TRUE) +
  labs(x = "Tempo (dias)", 
       y = "Probabilidade de sobrevivência") +
  guides(fill = guide_legend(title = "Categorias de idade materna (anos)"),
         color = guide_legend(title = "Categorias de idade materna (anos)")) +
  theme(legend.position = "top")

# f(t)
fig23 <- autoplot(m, conf.int = T, fun = 'cumhaz') +
  labs(x = "Tempo (dias)", 
       y = "Distribuição do tempo de vida") +
  guides(fill = guide_legend(title = "Categorias de idade materna (anos)"),
         color = guide_legend(title = "Categorias de idade materna (anos)")) +
  theme(legend.position = "top")

ggsave(filename = 'C:\\Users\\User\\Desktop\\dm016\\fig22.png',
       plot = fig22,
       width = 8,
       height = 6)

ggsave(filename = 'C:\\Users\\User\\Desktop\\dm016\\fig23.png',
       plot = fig23,
       width = 8,
       height = 6)

### ESTRATIFICACAO
# pesos
# categorias de peso: 0 - 1500, 1501 - 3000, 3001 - 4500, 4501 - 6000 

summary(df$newborn_weight) # visualizando max e min

# criando categorias
df_cat_newborn_wight <- df |>
  mutate(weight_category = cut(newborn_weight,
                               breaks = c(0, 1501, 3001, 4501, 6000),
                               labels = c("0 a 1500", "1501 a 3000", "3001 a 4500", "4501 a 6000"),
                               include.lowest = TRUE))

# visualizando resultados
ggplot(df_cat_newborn_wight, aes(x = weight_category)) +
  geom_bar() +
  labs(x = "",
       y = "")

m2 <- survfit(Surv(tempo_dias, status) ~ weight_category, data = df_cat_newborn_wight)
m2
summary(m2)

# rm n = 0 
df_cat_newborn_wight <- df_cat_newborn_wight[df_cat_newborn_wight$weight_category != '4501 a 6000', ]

# km banco limpo
m2 <- survfit(Surv(tempo_dias, status) ~ weight_category, data = df_cat_newborn_wight)

# apos rm n = 0
ggplot(df_cat_newborn_wight, aes(x = weight_category)) +
  geom_bar() +
  labs(x = "",
       y = "")

# grafico
fig24 <- autoplot(m2, conf.int = T) +
  labs(x = "Tempo (dias)",
       y = "Probabilidade de sobrevivência") +
  guides(fill = guide_legend(title = "Categorias de peso do recém nascido (kg)"),
         color = guide_legend(title = "Categorias de peso do recém nascido (kg)")) +
  theme(legend.position = "top")

# f(t)
fig25 <- autoplot(m2, conf.int = T, fun = 'cumhaz') +
  labs(x = "Tempo (dias)", 
       y = "Distribuição do tempo de vida") +
  guides(fill = guide_legend(title = "Categorias de peso do recém nascido (kg)"),
         color = guide_legend(title = "Categorias de peso do recém nascido (kg)")) +
  theme(legend.position = "top")

ggsave(filename = 'C:\\Users\\User\\Desktop\\dm016\\fig24.png',
       plot = fig24,
       width = 8,
       height = 6)

ggsave(filename = 'C:\\Users\\User\\Desktop\\dm016\\fig25.png',
       plot = fig25,
       width = 8,
       height = 6)

### ESTRATIFICACAO
# numero de consultas pre natais
# categorias de consultas: 1 - 'nenhuma', 2 - 1 a 3 consultas, 3 - 4 a 6 consultas,
# 4 - 7 ou mais consultas

summary(df$num_prenatal_appointments) # visualizando max e min

# criando categorias
df_cat_prenatal_appointments <- df |>
  mutate(appointments_category = cut(num_prenatal_appointments,
                                     breaks = c(0, 1, 4, 7, 10, Inf),
                                     labels = c("Nenhuma", "1 a 3", "4 a 6", "7 a 9", "10 ou mais"),
                                     include.lowest = TRUE))

# visualizando resultados
ggplot(df_cat_prenatal_appointments, aes(x = appointments_category)) +
  geom_bar() +
  labs(x = "",
       y = "")

# numero de consultas
m3 <- survfit(Surv(tempo_dias, status) ~ appointments_category, data = df_cat_prenatal_appointments)
m3
summary(m3)

# ajustar o modelo de sobrevivencia
m3 <- survfit(Surv(tempo_dias, status) ~ appointments_category, data = df_cat_prenatal_appointments)

# grafico
fig26 <- autoplot(m3, conf.int = T) +
  labs(x = "Tempo (dias)", 
       y = "Probabilidade de sobrevivência") +
  guides(fill = guide_legend(title = "Número de consultas pré-natais"),
         color = guide_legend(title = "Número de consultas pré-natais")) +
  theme(legend.position = "top")

# f(t)
fig27 <- autoplot(m3, conf.int = T, fun = 'cumhaz') +
  labs(x = "Tempo (dias)", 
       y = "Distribuição do tempo de vida") +
  guides(fill = guide_legend(title = "Número de consultas pré-natais"),
         color = guide_legend(title = "Número de consultas pré-natais")) +
  theme(legend.position = "top")

ggsave(filename = 'C:\\Users\\User\\Desktop\\dm016\\fig26.png',
       plot = fig26,
       width = 8,
       height = 6)

ggsave(filename = 'C:\\Users\\User\\Desktop\\dm016\\fig27.png',
       plot = fig27,
       width = 8,
       height = 6)

### ESTRATIFICACAO
# raca
# categorias de raca: 1 - branco, 2 - preto, 3 - amarelo, 4 - pardo, 5 - indigena, 9 - ignorado
df$tp_maternal_race
summary(df$tp_maternal_race)

df_cat_maternal_race <- df[df$tp_maternal_race != 9 & df$tp_maternal_race != 3, ]
# removendo as categorias 9
# removendo 3, pq nao sofreu eventos

df_cat_maternal_race <- df_cat_maternal_race %>%
  mutate(tp_maternal_race = case_when(
    tp_maternal_race == 1 ~ "branco",
    tp_maternal_race == 2 ~ "preto",
    tp_maternal_race == 3 ~ "amarelo",
    tp_maternal_race == 4 ~ "pardo",
    tp_maternal_race == 5 ~ "indígena",
    tp_maternal_race == 9 ~ "ignorado",
    TRUE ~ NA_character_  # se nao corresponder a nenhuma categoria, atribui NA
  ))

m4 <- survfit(Surv(tempo_dias, status) ~ tp_maternal_race, data = df_cat_maternal_race)
summary(m4)

fig28 <- autoplot(m4, conf.int = F) +
  labs(x = "Tempo (dias)",
       y = "Probabilidade de sobrevivência") +
  guides(fill = guide_legend(title = "Raça materna"),
         color = guide_legend(title = "Raça materna")) +
  theme(legend.position = "top")

# f(t)
fig29 <- autoplot(m4, conf.int = F, fun = 'cumhaz') +
  labs(x = "Tempo (dias)", 
       y = "Distribuição do tempo de vida") +
  guides(fill = guide_legend(title = "Raça materna"),
         color = guide_legend(title = "Raça materna")) +
  theme(legend.position = "top")

ggsave(filename = 'C:\\Users\\User\\Desktop\\dm016\\fig28.png',
       plot = fig28,
       width = 8,
       height = 6)

ggsave(filename = 'C:\\Users\\User\\Desktop\\dm016\\fig29.png',
       plot = fig29,
       width = 8,
       height = 6)

#### ESTRATIFICACAO
# num. filhos tivos vidos
# categorias de filhos tidos vivos: 1 - 0 a 2, 2 - 3 a 4, 4 - 7 a 8, 5 - 9 a 10,

df$num_live_births
summary(df$num_live_births) # max e min

# criando categorias
df_cat_live_births <- df |>
  mutate(num_liveb_category = cut(num_live_births,
                                  breaks = c(0, 3, 5, 7, 9, Inf),
                                  labels = c("0 a 2", "3 a 4", "5 a 6", "7 a 8", "9 a 10"),
                                  include.lowest = TRUE))

# visualizando resultados
ggplot(df_cat_live_births, aes(x = num_liveb_category)) +
  geom_bar() +
  labs(x = "Número de filhos tidos",
       y = "Quantidade")

# peso do filhos tidos vivos
m5 <- survfit(Surv(tempo_dias, status) ~ num_liveb_category, data = df_cat_live_births)
m5
summary(m5)

# removendo n = 0
df_cat_live_births <- df_cat_live_births[df_cat_live_births$num_liveb_category != "7 a 8" & df_cat_live_births$num_liveb_category != "9 a 10", ]

# visualizando resultados (apos rm n = 0)
ggplot(df_cat_live_births, aes(x = num_liveb_category)) +
  geom_bar() +
  labs(x = "Número de filhos tidos",
       y = "Quantidade")

# apos rm n = 0
m5 <- survfit(Surv(tempo_dias, status) ~ num_liveb_category, data = df_cat_live_births)

# plotar o grafico de sobrevivencia usando autoplot
fig30 <- autoplot(m5, conf.int = F) +
  labs(x = "Tempo (dias)",
       y = "Probabilidade de sobrevivência") +
  guides(fill = guide_legend(title = "Número de filhos tidos vivos"),
         color = guide_legend(title = "Número de filhos tidos vivos")) +
  theme(legend.position = "top")

# f(t)
fig31 <- autoplot(m5, conf.int = F, fun = 'cumhaz') +
  labs(x = "Tempo (dias)", 
       y = "Distribuição do tempo de vida") +
  guides(fill = guide_legend(title = "Número de filhos tidos vivos"),
         color = guide_legend(title = "Número de filhos tidos vivos")) +
  theme(legend.position = "top")

ggsave(filename = 'C:\\Users\\User\\Desktop\\dm016\\fig30.png',
       plot = fig30,
       width = 8,
       height = 6)

ggsave(filename = 'C:\\Users\\User\\Desktop\\dm016\\fig31.png',
       plot = fig31,
       width = 8,
       height = 6)

#### ESTRATIFICACAO
# num. tipo de parto
# categorias de parto: 1 - vaginal, 2 - cesariana e 9 ignorado

df$tp_labor

df_cat_labor <- df[df$tp_labor != 9, ] # removendo as categorias 9

df_cat_labor <- df_cat_labor %>%
  mutate(tp_labor = case_when(
    tp_labor == 1 ~ "vaginal",
    tp_labor == 2 ~ "cesariana",
    TRUE ~ NA_character_))

# tipo de parto
m6 <- survfit(Surv(tempo_dias, status) ~ tp_labor, data = df_cat_labor)
m6
summary(m6)

# plotar o grafico de sobrevivencia usando autoplot
fig32 <- autoplot(m6, conf.int = T) +
  labs(x = "Tempo (dias)", 
       y = "Probabilidade de sobrevivência") +
  guides(fill = guide_legend(title = "Tipo de parto"),
         color = guide_legend(title = "Tipo de parto")) +
  theme(legend.position = "top")

# f(t)
fig33 <- autoplot(m6, conf.int = T, fun = 'cumhaz') +
  labs(x = "Tempo (dias)", 
       y = "Distribuição do tempo de vida") +
  guides(fill = guide_legend(title = "Tipo de parto"),
         color = guide_legend(title = "Tipo de parto")) +
  theme(legend.position = "top")

ggsave(filename = 'C:\\Users\\User\\Desktop\\dm016\\fig32.png',
       plot = fig32,
       width = 8,
       height = 6)

ggsave(filename = 'C:\\Users\\User\\Desktop\\dm016\\fig33.png',
       plot = fig33,
       width = 8,
       height = 6)

#### ESTRATIFICACAO
# educacao
# categorias de educacao: 1 - nenhuma, 2 - 1 a 3 anos, 3 - 4 a 7 anos, 4 - 8 a 11 anos, 5 - 12 e mais, 9 - NA 

df$tp_maternal_schooling

# removendo 9 e 0 (PERGUNTAR PRO  CARLOS SOBRE A CATEGORIA 0)
df_cat_maternal_schooling <- df[df$tp_maternal_schooling != 9 & df$tp_maternal_schooling != 0, ]

df_cat_maternal_schooling <- df_cat_maternal_schooling %>%
  mutate(tp_maternal_schooling = case_when(
    tp_maternal_schooling == 1 ~ "nenhuma",
    tp_maternal_schooling == 2 ~ "1 a 3",
    tp_maternal_schooling == 3 ~ "4 a 7",
    tp_maternal_schooling == 4 ~ "8 a 11",
    tp_maternal_schooling == 5 ~ "12 e mais",
    TRUE ~ NA_character_))

df_cat_maternal_schooling$tp_maternal_schooling <- factor(df_cat_maternal_schooling$tp_maternal_schooling,
                                                          levels = c("nenhuma", 
                                                                     "1 a 3", 
                                                                     "4 a 7", 
                                                                     "8 a 11",
                                                                     "12 e mais"))

m7 <- survfit(Surv(tempo_dias, status) ~ tp_maternal_schooling, data = df_cat_maternal_schooling)
m7
summary(m7)

# plotar o grafico de sobrevivencia usando autoplot
fig34 <- autoplot(m7, conf.int = F) +
  labs(x = "Tempo (dias)", 
       y = "Probabilidade de sobrevivência") +
  guides(fill = guide_legend(title = "Escolaridade materna (anos)"),
         color = guide_legend(title = "Escolaridade materna (anos)")) +
  theme(legend.position = "top")

# f(t)
fig35 <- autoplot(m7, conf.int = F, fun = 'cumhaz') +
  labs(x = "Tempo (dias)", 
       y = "Distribuição do tempo de vida") +
  guides(fill = guide_legend(title = "Escolaridade materna (anos)"),
         color = guide_legend(title = "Escolaridade materna (anos)")) +
  theme(legend.position = "top")

ggsave(filename = 'C:\\Users\\User\\Desktop\\dm016\\fig34.png',
       plot = fig34,
       width = 8,
       height = 6)

ggsave(filename = 'C:\\Users\\User\\Desktop\\dm016\\fig35.png',
       plot = fig35,
       width = 8,
       height = 6)

#### ESTRATIFICACAO
# status marital
# categorias de status marital: 1 - solteiro, 2 - casado, 3 - viúvo, 4 separadado/divorciado judicialmente, 5 - uniao estavel, 9 - ignorado

df$tp_marital_status

df_cat_marital_status <- df[df$tp_marital_status != 9 & df$tp_marital_status != 3, ]

df_cat_marital_status <- df_cat_marital_status %>%
  mutate(tp_marital_status = case_when(
    tp_marital_status == 1 ~ "solteiro",
    tp_marital_status == 2 ~ "casado",
    tp_marital_status == 3 ~ "viúvo",
    tp_marital_status == 4 ~ "separado ou divorciado",
    tp_marital_status == 5 ~ "união estável",
    TRUE ~ NA_character_))

df_cat_marital_status$tp_marital_status <- factor(df_cat_marital_status$tp_marital_status,
                                                  levels = c("solteiro", 
                                                             "casado", 
                                                             "viúvo", 
                                                             "separado ou divorciado",
                                                             "união estável"))
# status marital
m8 <- survfit(Surv(tempo_dias, status) ~ tp_marital_status, data = df_cat_marital_status)
m8
summary(m8)

# plotar o grafico de sobrevivencia usando autoplot
fig36 <- autoplot(m8, conf.int = F) +
  labs(x = "Tempo (dias)",
       y = "Probabilidade de sobrevivência") +
  guides(fill = guide_legend(title = "Status marital"),
         color = guide_legend(title = "Status marital")) +
  theme(legend.position = "top")

# f(t)
fig37 <- autoplot(m8, conf.int = F, fun = 'cumhaz') +
  labs(x = "Tempo (dias)", 
       y = "Distribuição do tempo de vida") +
  guides(fill = guide_legend(title = "Status marital"),
         color = guide_legend(title = "Status marital")) +
  theme(legend.position = "top")

ggsave(filename = 'C:\\Users\\User\\Desktop\\dm016\\fig36.png',
       plot = fig36,
       width = 8,
       height = 6)

ggsave(filename = 'C:\\Users\\User\\Desktop\\dm016\\fig37.png',
       plot = fig37,
       width = 8,
       height = 6)
