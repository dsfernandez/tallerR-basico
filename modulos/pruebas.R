# ejemplo uso de if-else concatenado en una función
# American Heart Association
PSx <- function(s,d) {
  if (s <= 90 && d <= 60) {
    "Hipotensión"
  } else if (s < 120 && d <= 80) {
      "Presión Normal"
  } else if (s >= 120 && s <= 129 && d <= 80) {
      "Presión Elevada"
  } else if ((s >= 130 && s <= 139) || (d > 80 && d <= 89)) {
      "Hipertensión Etapa 1"
  } else if (s >= 140 || d >= 90) {
      "Hipertensión Etapa 2"
  } else if (s >= 180 || d >= 120) {
      "Crisis Hipertensiva"
    }
}
  
PSx(125,75)  

# uso de for
# for (i in n)
# for with if else
n <- length(Entrada$Temp)
ID <- seq(1:n)
Temp <- c(38.5,38.2,36.8,38.4,37.1,38,36.8,37.5)
Contacto <- c(0,1,0,1,1,0,0,0)
Estado <- rep("NA",8)
Entrada <- data.frame(ID,Temp,Contacto,Estado)

for (i in ID)
  {
  if (Entrada$Temp[i] < 38 && Entrada$Contacto[i] == 0) {
    Entrada[i,4] <- c("Acceso")
  } else if (Entrada$Temp[i] >= 38 && Entrada$Contacto[i] != 0) {
    Entrada[i,4] <- c("NO-Acceso")
  } else if (Entrada$Temp[i] >= 38 || Entrada$Contacto[i] != 0) {
    Entrada[i,4] <- c("Vigilancia")
  }
}

## tablas
library(kableExtra)
covidPR <- read.csv("modulos/data/covid-sex.csv")
kbl(covidPR, caption = "Table 1. CoviD-19 deaths by sex and age groups") %>%
  kable_styling("striped", full_width = F) %>%
  add_header_above(c("", "Sex" = 2))

## estad descrpt


# leer los datos de archivo externo
ca_ratas <- read.csv("modulos/data/calcio-ratas.csv")
# activar paquete doBy para aplicar función por grupos
library(doBy)
descriptivas <- summaryBy(caplasma ~ list(hormona,sexo), 
                          data = ca_ratas,
                          FUN = function(x) {
                            c(media = mean(x), 
                              var = var(x), 
                              de = sd(x), 
                              es = sd(x)/sqrt(length(x)), 
                              CV = (sd(x)/mean(x))*100) })
# creación de tabla con kableExtra
kbl(descriptivas, caption = "Table 2. Niveles de calcio plasmático en ratas hembra y macho tratadas con estrógeno o no", 
    col.names = c("Hormona", "Sexo", "Media", "Varianza", "Desv. Estándar", "Error Estándar", "CV %"),
    digits = c(2,2,2,2,2)) %>%
  kable_classic(full_width = T) %>%
  add_header_above(c("Tratamientos" = 2, "Estadísticos Ca-plasma, mg/dl" = 5))
  
  
## gt
library(gt)
gt(descriptivas) %>%
  tab_header(
    title = "Table 2. Niveles de calcio plasmático en ratas hembra y macho tratadas con estrógeno o no",
    subtitle = "Cálculos mediante función en doBy"
  ) %>%
  cols_label(
    hormona = html("Hormona"),
    sexo = html("Sexo"),
    caplasma.media = html("Media"),
    caplasma.var = html("Varianza"),
    caplasma.de = html("Desv. Estándar"),
    caplasma.es = html("Error Estándar"),
    caplasma.CV = html("Coef. Var. %")
  ) %>%
  tab_spanner(
    label = "Tratamientos",
    columns = vars(hormona, sexo)
  ) %>%
  tab_spanner(
    label = "Estadísticos",
    columns = vars(caplasma.media, caplasma.var, caplasma.de, caplasma.es, caplasma.CV)
  ) %>%
  fmt_number(
    columns = vars(caplasma.media, caplasma.var, caplasma.de, caplasma.es, caplasma.CV),
    decimals = 2
  )
  
  