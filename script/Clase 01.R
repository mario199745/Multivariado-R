
# Tipos de modelos predictivos vs explicativos --------

#Recursos 
#Book : https://derek-corcoran-barrios.github.io/CursoMulti/_book/CriteriosInfo.html
#Video : https://www.youtube.com/watch?v=SLbgiN5HVyg&t=8s&ab_channel=DerekCorcoran
install.packages("MuMIn")
install.packages("caret",dependencies = T)
install.packages("broom")

library(MuMIn)
library(caret)
library(lattice)
library(ggplot2)
library(broom)

data("mtcars")
#Ejemplo 01 

ggplot(mtcars, aes(x = hp, y = mpg)) + geom_point() + theme_classic()


set.seed(2018)

index <- sample(1:nrow(mtcars), size = round(nrow(mtcars)/2))

Train <- mtcars[index,]

Test <- mtcars[-index,]

#Ajustar el modelo 

Modelo <- lm(mpg ~ hp + I(hp^2), data = Train)

broom::tidy(Modelo)
broom::glance(Modelo)
glance(Modelo)

#Realizamos una predicción en la nueva base de datos 

Test$Pred <- predict(Modelo,newdata = Test)

Train$Pred <- predict(Modelo,newdata = Train)

ggplot(Test,aes(x=hp , y=mpg))+geom_point() + geom_line(aes(y=Pred))+theme_bw()


#Caret : Calcular el R2 con el observado y los predichos 


#Poder predictivo vs explicativo 


postResample(pred = Test$Pred,obs = Test$mpg)
postResample(pred = Train$Pred,obs = Train$mpg)



# Sobreajuste -------------------------------------------------------------

#¿Cuando me puede interessar el maximizar el poder predictivo?

#¿Cuando me puede interessar el maximizar el poder explicativo?
#Generación de hipótesis 
#Interpretación de resultados en base a modelos de hipótesis 



