#declaracion de variables
BeneficiosVC <- c() #venture capital
BeneficiosEsp <- c() #beneficios a esperar
BeneficiosAngel <- c() #beneficios de angeles
randNumber <- function() runif(1,0,1)

set.seed(1000)

for (x in 1:10000) {
    Aleatorio <- randNumber() #simula venta de patentes
    
    if (Aleatorio <= 0.75) { #Escenario donde se vende la patente
       Aleatorio2 <- randNumber()

       if (Aleatorio2 <= 0.6) {
        BeneficiosVC <- append(BeneficiosVC, 107580000)
        BeneficiosEsp <- append(BeneficiosEsp, 163000000)
        BeneficiosAngel <- append(BeneficiosAngel, 146276200)
       } else {        
        #escenario caso diabetes

        BeneficiosVC <- append(BeneficiosVC, 374880000)
        BeneficiosEsp <- append(BeneficiosEsp, 568000000)
        BeneficiosAngel <- append(BeneficiosAngel, 509723200)
       }
    } else {   # se logra vender la patente en el primer itento     
        Aleatorio3 <- randNumber() #simula venta taria

        if(Aleatorio3 <= 0.95) { #Escenario donde se logra la venta tardia
            BeneficiosVC <- append(BeneficiosVC, 170400000)
            BeneficiosEsp <- append(BeneficiosEsp, 126209600)
            BeneficiosAngel <- append(BeneficiosAngel, 254861600)
        } else { #Escenario No se vende nada
            BeneficiosVC <- append(BeneficiosVC, 0)
            BeneficiosEsp <- append(BeneficiosEsp, 0)
            BeneficiosAngel <- append(BeneficiosAngel, 0)
        }
    }
}

print("vc: ")
print(head(BeneficiosVC))
print("esp: ")
print(head(BeneficiosEsp))
print(head(BeneficiosAngel))

# seccion que determina las frecuencias de la opcion ganadora en cada caso.
NumVC <- c()
NumEsp <- c()
NumAngeles <- c()
Empate <- c()

for (count in 1:10000) {
   # checar si la opcion ganadora es venture capital
   if (BeneficiosVC[count] > BeneficiosEsp[count] 
            & BeneficiosVC[count] > BeneficiosAngel[count]) {
      NumVC <- append(NumVC, BeneficiosVC[count])
   # la opcion ganadora es esperar 6 meses
   } else if (BeneficiosEsp[count] > BeneficiosVC[count] 
            & BeneficiosEsp[count] > BeneficiosAngel[count]) {
      NumEsp <- append(NumEsp, BeneficiosEsp[count])
   # checa si la opcion ganadora son angeles
   } else if (BeneficiosAngel[count] > BeneficiosVC[count] 
            & BeneficiosAngel[count] > BeneficiosEsp[count]) {
      NumAngeles <- append(NumAngeles, BeneficiosAngel[count])
   } else {
      Empate <- append(Empate, 0)
   }
}

print(cat("Numero de veces que gana VC", length(NumVC), "\n"))
print(cat("Numero de veces que gana Esperar", length(NumEsp), "\n"))
print(cat("Numero de veces que gana Angeles", length(NumAngeles), "\n"))
print(cat("Numero de veces que hay empate", length(Empate), "\n"))

print(length(NumVC)/10000)
print(length(NumEsp)/10000)
print(length(NumAngeles)/10000)
print(length(Empate)/10000)