
### RESIDUO (SCHOENFELD)

# SCHOENFELD (testa h0)
# ho: nao existe tendencia no tempo
# nao rejeita ho para nenhuma variavel, o risco relativo e o mesmo durante todo tempo de observacao

windows(width = 10, height = 6) 
residuo.sch <- cox.zph(modelo2)
par(mfrow = c(2, 2))
plot(residuo.sch)

dev.off()