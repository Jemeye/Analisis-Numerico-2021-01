trapezoid = function(fun, a, b, c) {
  
  Integ = integrate(fun,a,b)
  
  y = fun(x)
  r = Integ$value
  k = (b-a)/c
  x = seq(a, b, by=k)

  s = k * (abs(y[1]/2) + abs( sum(y[2:c])) + abs (y [c+1]/2))
  
  result = k * (abs(y[1]/2) + abs( sum(y[2:c])) + abs (y [c+1]/2))
  
  cat("Trapecio: ",result," error: ", abs( r - result))
}

simpson = function(fun, a, b, c) {
 
  Integ = integrate(fun,a,b)
 
  r = Integ$value

    if (c%%2 != 0) stop ("Simpson fin")
    
  z = seq(1, c-1, by = 2)
  l = seq(2, c-2, by = 2)
  k = (b-a)/c
  y = fun(a+(0:c)*k)
  
  abs(k/3 * ( fun(a) + fun(b) + 4*sum (y[z]) + 2*sum(sum (y [l]))))
  result = abs(k/3 * ( fun(a) + fun(b) + 4*sum (y[z]) + 2*sum (sum (y[l]))))
  
  cat("Simpson: ",result, " error: ", abs( r- result), "\n")
}

x = seq(-1, 1, by=0.1)
fun = function(x){
  return(1+sin(exp(3*x)))
}
y = fun(x)
a = -1       
b = 1     
c = 10

simpson(fun,a,b,c)
trapezoid(fun,a,b,c)
