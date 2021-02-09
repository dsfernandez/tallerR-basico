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


