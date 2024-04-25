# variables
BeneficiosAhora <- c()
BeneficiosEsperar <- c()
randNumber <- function() runif(1,0,1)

set.seed(1000)

for (x in 1:10000) {
    Aleatorio <- randNumber()

    BeneficiosAhora <- append(BeneficiosAhora, 34200) # escenario cosechar ahora, siempre es lo mismo

    if (Aleatorio <= 0.5) { # escenario donde llega la tormenta
       Aleatorio2 <- randNumber()

       if (Aleatorio2 <= 0.6) { # escenario no hay botrytis
          BeneficiosEsperar <- append(BeneficiosEsperar, 7200)          
       } else { # escenario si hay botrytis
          BeneficiosEsperar <- append(BeneficiosEsperar, 20640) #13440
       }
 
    } else { #escenario donde no llega la tormenta
       Aleatorio3 <- randNumber()

       if(Aleatorio3 <= 0.4) { #escenario contenido de azucar = 25%
          BeneficiosEsperar <- append(BeneficiosEsperar, 29040) #8400
       } else if (Aleatorio3 > 0.4 & Aleatorio3 <= 0.8) { # escenario azucar = 20%
          BeneficiosEsperar <- append(BeneficiosEsperar, 44640) #7200
       } else { #escenario contenido azucar < 19%
          BeneficiosEsperar <- append(BeneficiosEsperar, 63240) #3000
       }
    }
}

print(head(BeneficiosAhora))
print(head(BeneficiosEsperar))

NumAhora <- c()
NumEspera <- c()
NumEmpate <- c()

for (i in 1:10000) {
    if (BeneficiosAhora[i] > BeneficiosEsperar[i]) { #escenario gana ahora
       NumAhora <- append(NumAhora, BeneficiosAhora[i])
    } else if (BeneficiosAhora[i] < BeneficiosEsperar[i]) { #escenario gana esperar
       NumEspera <- append(NumEspera, BeneficiosEsperar[i])
    } else { #Empate
       NumEmpate <- append(NumEmpate, 0)
    }
}

print(cat("Num veces gana ahora: ", length(NumAhora), "\n"))
print(cat("Num veces gana esperar: ", length(NumEspera), "\n"))
print(cat("Num veces gana empate: ", length(NumEmpate), "\n"))

print(length(NumAhora)/10000)
print(length(NumEspera)/10000)
print(length(NumEmpate)/10000)


