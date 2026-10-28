url <- "https://github.com/scentenoo/Articulo_regresion/raw/main/datos.xlsx"
archivo <- tempfile(fileext = ".xlsx")
download.file(url, destfile = archivo, mode = "wb")
datos <- as.data.frame(read_excel(archivo))

names(datos)[tolower(names(datos)) == "teac"] <- "TEAC"

summary(datos$TEAC)
resumen_TEAC <- c(
  n          = length(datos$TEAC),
  media      = mean(datos$TEAC),
  mediana    = median(datos$TEAC),
  desv_est   = sd(datos$TEAC),
  varianza   = var(datos$TEAC),
  minimo     = min(datos$TEAC),
  maximo     = max(datos$TEAC),
  rango      = diff(range(datos$TEAC)),
  coef_var   = 100 * sd(datos$TEAC) / mean(datos$TEAC)   # en porcentaje
)
round(resumen_TEAC, 4)

# Corridas con la menor y la mayor actividad antioxidante
datos[which.min(datos$TEAC), ]
datos[which.max(datos$TEAC), ]

resumen_por_nivel <- function(factor, nombre) {
  m <- tapply(datos$TEAC, factor, mean)
  s <- tapply(datos$TEAC, factor, sd)
  data.frame(factor = nombre, nivel = names(m),
             media = round(m, 4), desv_est = round(s, 4), row.names = NULL)
}
rbind(resumen_por_nivel(datos$temperatura, "temperatura"),
      resumen_por_nivel(datos$tiempo,      "tiempo"),
      resumen_por_nivel(datos$etanol,      "etanol"))

efecto <- function(x) mean(datos$TEAC[x == max(x)]) - mean(datos$TEAC[x == min(x)])
efectos_principales <- c(temperatura = efecto(datos$temperatura),
                         tiempo      = efecto(datos$tiempo),
                         etanol      = efecto(datos$etanol))
round(efectos_principales, 4)


round(cor(datos[, c("temperatura", "tiempo", "etanol", "TEAC")]), 4)

# -----------------------------------------------------------------------------
# 6. Gráficos descriptivos
# -----------------------------------------------------------------------------
# 6.1 Distribución de TEAC
datos$n_altos <- (datos$temperatura == max(datos$temperatura)) +
  (datos$tiempo      == max(datos$tiempo)) +
  (datos$etanol      == max(datos$etanol))
colores <- c("gray40", "steelblue", "darkorange", "firebrick")

par(mfrow = c(1, 2))
stripchart(datos$TEAC, method = "stack", pch = 19, cex = 1.5,
           col = colores[datos$n_altos + 1], offset = 0.8,
           xlab = "TEAC (mg/g fruto seco)", main = "Valores de TEAC (n = 8)")
abline(v = mean(datos$TEAC), lty = 2, col = "gray50")
legend("topleft", legend = 0:3, col = colores, pch = 19, bty = "n",
       title = "Factores en nivel alto", cex = 0.8)
boxplot(datos$TEAC, col = "lightblue", main = "Boxplot de TEAC",
        ylab = "TEAC (mg/g fruto seco)")
par(mfrow = c(1, 1))

# Media de TEAC según el número de factores en nivel alto
tapply(datos$TEAC, datos$n_altos, mean)

#La TEAC va de 1.78 a 5.74 mg/g, con una media de 3.76 y una mediana de 3.76. 
#Como coinciden, la distribución es simétrica. La desviación estándar es 1.35 y
#el coeficiente de variación es 35.9 %. El boxplot no muestra datos atípicos, y
#el rango intercuartílico va de 2.81 a 4.69. Los valores no se reparten de forma
#continua: forman grupos según cuántos factores están en nivel alto. Con 0 factores
#la TEAC es 1.78, con 1 es en promedio 2.86, con 2 es 4.66 y con 3 es 5.74. 


par(mfrow = c(1, 1))

# 6.2 TEAC por corrida experimental
plot(datos$ensayo, datos$TEAC, type = "b", pch = 19, cex = 1.3,
     col = colores[datos$n_altos + 1], ylim = c(1.3, 6.2),
     xlab = "Ensayo (orden estándar del diseño)", ylab = "TEAC (mg/g fruto seco)",
     main = "TEAC en cada corrida del diseño 2^3")
text(datos$ensayo, datos$TEAC, labels = datos$TEAC, pos = 3, cex = 0.8)
text(datos$ensayo, datos$TEAC, labels = paste0("(", datos$n_altos, ")"),
     pos = 1, cex = 0.7, col = "gray30")

#El orden de los ensayos es el orden estándar del diseño, no el orden en que se
#hicieron los experimentos. Por eso la forma de la línea depende de los niveles
#de los factores, no del tiempo. La TEAC mínima es la del ensayo 1, con todos los
#factores en nivel bajo, y la máxima la del ensayo 8, con todos en nivel alto. 
#La caída del ensayo 4 (4.65) al 5 (2.97) ocurre porque se pasa de dos factores
#en nivel alto (temperatura y tiempo) a uno solo (etanol). Las corridas con el 
#mismo número de factores en nivel alto tienen valores muy parecidos: 2.78, 2.82
#y 2.97 con uno, y 4.65, 4.79 y 4.55 con dos. Esto indica que los tres factores 
#tienen un efecto de magnitud similar.



# 6.3 TEAC frente a cada factor
par(mfrow = c(1, 3))
for (v in c("temperatura", "tiempo", "etanol")) {
  etiqueta <- switch(v, temperatura = "Temperatura (K)",
                     tiempo = "Tiempo (min)", etanol = "Etanol (%)")
  plot(datos[[v]], datos$TEAC, pch = 19, col = "darkblue",
       ylim = range(datos$TEAC), xaxt = "n",
       xlab = etiqueta, ylab = "TEAC (mg/g fruto seco)",
       main = paste("TEAC vs", v))
  axis(1, at = unique(datos[[v]]))
  medias <- tapply(datos$TEAC, datos[[v]], mean)
  lines(as.numeric(names(medias)), medias, col = "red", lwd = 2)
  points(as.numeric(names(medias)), medias, col = "red", pch = 4, cex = 2, lwd = 2)
}
par(mfrow = c(1, 1))

#En los tres factores la TEAC media es mayor en el nivel alto. Con la temperatura
#pasa de 3.03 a 4.49, con el tiempo de 3.08 a 4.44 y con el etanol de 3.01 a 4.51.
#Las correlaciones con la TEAC son 0.58, 0.54 y 0.60. Dentro de cada nivel los 
#puntos están muy dispersos: por ejemplo, a 343.15 K la TEAC va de 1.78 a 4.55.
#Esa dispersión no es ruido. Se debe a que los otros dos factores también cambian
#dentro de cada nivel. Por eso ningún factor por sí solo explica bien la respuesta,
#y es necesario un modelo que los incluya a los tres a la vez.


# 6.4 Gráfico de efectos principales
par(mfrow = c(1, 3))
for (v in c("temperatura", "tiempo", "etanol")) {
  etiqueta <- switch(v, temperatura = "Temperatura (K)",
                     tiempo = "Tiempo (min)", etanol = "Etanol (%)")
  medias <- tapply(datos$TEAC, datos[[v]], mean)
  plot(1:2, medias, type = "b", pch = 19, lwd = 2, col = "darkgreen",
       ylim = range(datos$TEAC), xlim = c(0.7, 2.3), xaxt = "n",
       xlab = etiqueta, ylab = "Media de TEAC", main = paste("Efecto principal:", v))
  axis(1, at = 1:2, labels = names(medias))
  abline(h = mean(datos$TEAC), lty = 2, col = "gray50")
  text(1.5, max(datos$TEAC) - 0.2,
       paste("Efecto =", round(diff(medias), 3)), cex = 0.9)
}
par(mfrow = c(1, 1))

#Los tres efectos principales son positivos y de magnitud parecida: 
#etanol 1.505, temperatura 1.460 y tiempo 1.360. Pasar cualquiera de los
#factores del nivel bajo al alto aumenta la TEAC media en unos 1.4 mg/g, que es
#más de una desviación estándar de la respuesta. El etanol tiene el mayor efecto,
#pero la diferencia con los otros dos es pequeña. Con una sola observación por
#tratamiento no hay una estimación del error puro con la cual decidir si esas
#diferencias son reales.

# 6.5 Gráficos de interacción entre pares de factores
par(mfrow = c(1, 3))
pares <- list(c("temperatura", "tiempo"), c("temperatura", "etanol"),
              c("tiempo", "etanol"))
etiq <- c(temperatura = "Temperatura (K)", tiempo = "Tiempo (min)",
          etanol = "Etanol (%)")
for (p in pares) {
  interaction.plot(datos[[p[1]]], datos[[p[2]]], datos$TEAC,
                   xlab = etiq[p[1]], ylab = "Media de TEAC",
                   col = c("blue", "red"), lty = c(2, 1), lwd = 2,
                   type = "b", pch = 19, legend = FALSE, ylim = c(2, 5.5),
                   main = paste(p[1], "x", p[2]))
  legend("topleft", legend = paste(etiq[p[2]], "=", sort(unique(datos[[p[2]]]))),
         col = c("blue", "red"), lty = c(2, 1), lwd = 2, bty = "n", cex = 0.8)
}
par(mfrow = c(1, 1))

#En los tres gráficos las líneas son casi paralelas, lo que indica que las
#interacciones dobles son pequeñas:

#Temperatura × tiempo: el efecto de la temperatura es 1.41 con 5 min y 1.51 con
#15 min. La interacción AB vale 0.05.
#Temperatura × etanol: el efecto de la temperatura es 1.42 sin etanol y 1.51 con 
#80 %. La interacción AC vale 0.045.
#Tiempo × etanol: el efecto del tiempo es 1.46 sin etanol y 1.27 con 80 %. 
#La interacción BC vale −0.095. Es la única con signo negativo: con 80 % de etanol,
#alargar el tiempo de extracción aumenta un poco menos la TEAC.

#Las tres interacciones son pequeñas al lado de los efectos principales, que están
#entre 1.36 y 1.51. Esto respalda un modelo de primer orden sin interacciones
#dobles.
