
### TESTE PETO

survdiff(Surv(tempo_dias, status) ~ age_category, rho = 1, data = df_cat_maternal_age)

survdiff(Surv(tempo_dias, status) ~ weight_category, rho = 1, data = df_cat_newborn_wight)

survdiff(Surv(tempo_dias, status) ~ appointments_category, rho = 1, data = df_cat_prenatal_appointments)

survdiff(Surv(tempo_dias, status) ~ tp_maternal_race, rho = 1, data = df_cat_maternal_race)

survdiff(Surv(tempo_dias, status) ~ num_liveb_category, rho = 1, data = df_cat_live_births)

survdiff(Surv(tempo_dias, status) ~ tp_labor, rho = 1, data = df_cat_labor)

survdiff(Surv(tempo_dias, status) ~ tp_maternal_schooling, rho = 1, data = df_cat_maternal_schooling)

survdiff(Surv(tempo_dias, status) ~ tp_marital_status, rho = 1, data = df_cat_marital_status)
