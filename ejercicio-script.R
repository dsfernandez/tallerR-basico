n <- floor(rnorm(10000, 500, 100))
t <- table(n)
barplot(t)
class(n)
mode(n)

# guardar una lista de objetos
data <- list(n, t)
save(data, file = "frec.Rdata")
load("frec.Rdata")
frec <- as.data.frame(data[[2]])
frec
class(frec)
mode(frec)
# guardar grafica


# correr script desde File
source("prepa1.R")

