#Código algoritmo punto 2b del primer parcial de númerico
f <- function(x) x*x
g <- function(x) 1+cos(x)
z <- function(x) (x*x)-1-cos(x) 
  

algoritmo <- function(z, tol){
  
  i <- 1
  x1<- 2
  x2<- 1
  xn <- x1 - (z(x1)*(x1-x2))/(z(x1)-z(x2))
  while (abs(xn-x1)>tol){
    x0<-xn
    x02<-x1
    xn <- x1 - (z(x1)*(x1-x2))/(z(x1)-z(x2))
    x1<-x0
    x2<-x02
    cat("Iteracion: ", i, "Error: ", abs(xn-x1), "X: ",xn)
    i<- i + 1
   }
  
}

algoritmo (z, 1.e-9)