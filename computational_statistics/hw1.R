---------------------------# ¼÷Á¦
  #1 
  
  .2 == .3-.1
all.equal(.2, .3-.1)




#2 
evaluatefunctionsinc = function(xmin,xmax,n){
  x = c(0)
  f = c(0)
  for (i in (0:n)){
    x[i+1] = xmin + i*(xmax-xmin)/n
    f[i+1] = (sin(x[i+1]))/((x[i+1]))
  }
  plot(x,f,type="l",col="blue",xlab="x",ylab="function")
}

evaluatefunctionsinc(-10, 10, 100)
evaluatefunctionsinc(-10^-20, 10^-20, 100)



evaluatefunctionsincwithcheck = function(xmin,xmax,n,epsilon){
  x = c(0)
  f = c(0)
  for (i in (0:n)){
    x[i+1] = xmin + i*(xmax-xmin)/n
    if (abs(x[i+1]) > epsilon){
      f[i+1] = (sin(x[i+1]))/((x[i+1]))
    }
    else{
      f[i+1] = 1
    }
  }
  plot(x,f,type="l",col="blue",xlab="x",ylab="function")
}
evaluatefunctionsincwithcheck(-10^-20, 10^-20, 100, 10^-30)

#3
fiboiterative = function(i){
  if (i <= 2){
    value = 1
  }
  else{
    value1 = 1
    value2 = 1
    for (j in 3:i){
      value = value1 + value2
      value1 = value2
      value2 = value
    }
  }
  return(value)
}

system.time(fiboiterative(10))
system.time(fiboiterative(20))
system.time(fiboiterative(30))
system.time(fiboiterative(40))

fiborecursive = function(i){
  if (i <= 2){
    value = 1
  }
  else{
    return(fiborecursive(i-1)+fiborecursive(i-2))
  }
}

system.time(fiborecursive(10))
system.time(fiborecursive(20))
system.time(fiborecursive(30))
system.time(fiborecursive(40))

