
### COX

# montando df
df_cat_all <- df |>
  mutate(weight_category = cut(newborn_weight,
                               breaks = c(0, 1501, 3001, 4501, 6000),
                               labels = c("0 a 1500", "1501 a 3000", "3001 a 4500", "4501 a 6000"),
                               include.lowest = TRUE)) |>
  mutate(appointments_category = cut(num_prenatal_appointments,
                                     breaks = c(0, 1, 4, 7, 10, Inf),
                                     labels = c("Nenhuma", "1 a 3", "4 a 6", "7 a 9", "10 ou mais"),
                                     include.lowest = TRUE)) |>
  mutate(num_liveb_category = cut(num_live_births,
                                  breaks = c(0, 3, 5, 7, 9, Inf),
                                  labels = c("0 a 2", "3 a 4", "5 a 6", "7 a 8", "9 a 10"),
                                  include.lowest = TRUE))

# idade materna
# df_cat_all <- df_cat_all[df_cat_all$age_category != '45-55', ]

# peso
df_cat_all <- df_cat_all[df_cat_all$weight_category != '4501 a 6000', ]

# num. consultas pre natal
# nao precisa remover, nao tem n = 0

# raca
df_cat_all <- df_cat_all[df_cat_all$tp_maternal_race != 9 & df_cat_all$tp_maternal_race != 3, ]

# filhos tidos vivos
df_cat_all <- df_cat_all[df_cat_all$num_liveb_category != "7 a 8" & df_cat_all$num_liveb_category != "9 a 10", ]

# tipo de parto
df_cat_all <- df_cat_all[df_cat_all$tp_labor != 9, ] # removendo as categorias 9

# escolaridade da mae
# removendo 9 e 0 (PERGUNTAR PRO  CARLOS SOBRE A CATEGORIA 0)
df_cat_all <- df_cat_all[df_cat_all$tp_maternal_schooling != 9 & df_cat_all$tp_maternal_schooling != 0, ]

# status marital
df_cat_all <- df_cat_all[df_cat_all$tp_marital_status != 9 & df_cat_all$tp_marital_status != 3, ]

# ou
df_cat_all2 <- df_cat_all |>
  filter(# age_category != '45-55',
         weight_category != '4501 a 6000',
         tp_maternal_race != 9 & tp_maternal_race != 3,
         num_liveb_category != "7 a 8" & num_liveb_category != "9 a 10",
         tp_labor != 9,
         tp_maternal_schooling != 9 & tp_maternal_schooling != 0,
         tp_marital_status != 9 & tp_marital_status != 3)


### COX

# criar novo y com os dados categorizados e limpos, exceto idade materna

# novo tempo e status
y2 <- Surv(df_cat_all$tempo_dias, df_cat_all$status)


# estrategia
#1 seguir significancia do teste peto
#2 incluir variaveis importantes para o tema com base na literatura

# modelo 1
modelo1 <- coxph(y2 ~ maternal_age + appointments_category, 
                 data = df_cat_all2, x = TRUE)
summary(modelo1)

# modelo 2 
modelo2 <- coxph(y2 ~ maternal_age + appointments_category + weight_category, 
                 data = df_cat_all2, x = TRUE)
summary(modelo2)

# modelo 3 
df_cat_all2$tp_maternal_schooling <- factor(df_cat_all2$tp_maternal_schooling,
                                            lev = 1:5, lab = c('none', 
                                                               '1 a 3', 
                                                               '4 a 7',
                                                               '8 a 11',
                                                               '12 ou mais'))

modelo3 <- coxph(y2 ~ maternal_age + appointments_category + weight_category + tp_maternal_schooling, 
                 data = df_cat_all2, x = TRUE)
summary(modelo3)

# modelo 4 inclui variaveis alem do teste peto
# transformando em fator
df_cat_all2$tp_maternal_race <- factor(df_cat_all2$tp_maternal_race,
                                       lev = 1:5, lab = c('branco', 
                                                          'preto', 
                                                          'amarelo',
                                                          'pardo',
                                                          'indigenas'))

modelo4 <- coxph(y2 ~ weight_category + appointments_category + maternal_age + tp_maternal_schooling + tp_maternal_race, 
                 data = df_cat_all2, x = TRUE)
summary(modelo4)

# modelo 5
# transformando em fator
df_cat_all2$tp_labor <- factor(df_cat_all2$tp_labor,
                               lev = 1:2, lab = c('vaginal',
                                                  'cesariana'))

modelo5 <- coxph(y2 ~ weight_category + appointments_category + maternal_age + tp_maternal_schooling + tp_maternal_race + tp_labor, 
                 data = df_cat_all2, x = TRUE)
summary(modelo5)
