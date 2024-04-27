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
          BeneficiosEsperar <- append(BeneficiosEsperar, 24000)          
       } else { # escenario si hay botrytis
          BeneficiosEsperar <- append(BeneficiosEsperar, 67200) #13440
       }
 
    } else { #escenario donde no llega la tormenta
       Aleatorio3 <- randNumber()

       if (Aleatorio3 <= 0.5) { #escenario contenido azucar 25%
          Aleatorio4 <- randNumber()

          if (Aleatorio4 <= 0.8) { #acidez > 0.7%
             BeneficiosEsperar <- append(BeneficiosEsperar, 42000) #8400
          } else { #acidez < 0.7%
             BeneficiosEsperar <- append(BeneficiosEsperar, 30000) #1500
          }
       } else { #escenario azucar 20%
          Aleatorio5 <- randNumber()

          if (Aleatorio5 <= 0.8) { #acidez > 0.7%
             BeneficiosEsperar <- append(BeneficiosEsperar, 36000) #7200
          } else { #acidez < 0.7%
             BeneficiosEsperar <- append(BeneficiosEsperar, 30000) #1500
          }
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

print(cat("Num veces gana cosechar ahora: ", length(NumAhora), "\n"))
print(cat("Num veces gana esperar cosecha: ", length(NumEspera), "\n"))
print(cat("Num veces gana empate: ", length(NumEmpate), "\n"))

print("% que gana cosechar ahora: ")
print((length(NumAhora)/10000)*100)
print("% que gana esperar para cosechar: ")
print((length(NumEspera)/10000)*100)
print("% que gana empate: ")
print((length(NumEmpate)/10000)*100)


