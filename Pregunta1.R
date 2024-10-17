#Codi Pregunta 1

x <- c(0,1)
f <- c(0.68, 0.32)

plot(x, f, type="h", ylim=c(0,1), col = "red2")

#Apartat a)
n <- 43
sum(sample(x,n,f, replace = TRUE)) #Fa un experiment aleatori
#Ho convertim a funció:
Y <- function(i){sum(sample(x,n,f, replace = TRUE))}

enquestes <- sapply(1:40, Y) #Fa un nº determinat d'experiments aka enquestes
                             #Cada numeret son 43 respostes
enquestes

#Per saber la % de que doni 13, fem les frequencies relatives quan fem infinites enquestes
enquestes <- sapply(1:400000, Y)
table(enquestes)/400000

#Grafiquem la taula
fr <- table(enquestes)/400000
barplot(fr)
fr["13"]

#Utilitzem la formula de la binomial, definim y,n
dbinom(13,43,0.32) #Probabilitat de obtenir 13 respostes positives en 43 mostres tenint en compte que la 
                   #probabilitat de que sigui positiu és 0.32

#És la probabilitat de que en 43 cases hi hagi al menys 2 teles
y <- 0:43
plot(y, dbinom(y, 43, 0.32), type="h", col="red")

#Apartat b)
dbinom(17,44,0.32)
#Sabem que P(Y<17) = P(Y <= 16) = F(16)
pbinom(16,44,0.32) #El que fem és sumar les xinxetes del plot d'abans fins a 16

#Per la mitjana d'aquesta variable en aquest cas:
qbinom(0.5, 44, 0.32) #Posem 0.5 ja que està a la meitat de la funció F(x)
                      #També és tallar al 2n quartil
#Pel primer quartil:
qbinom(0.25, 44, 0.32)


#Apartat c)

#A Bernoulli: E(x)=p i Var(x)=p(1-p), en Binomial: E(Y)=n·p Var(Y)=n·p(1-p)
#Per l'enunciat: 
n <- 24
p<- 0.68
E <- n*p
E #media
Var <- n*p*(1-p)
Var #varianza
qbinom(0.25,24,0.68) #1r quartil = el quantil quart

y<-0:43
plot(y, pbinom(y,24,0.68), type="s")

#Apartat d)
E <- 43*0.32