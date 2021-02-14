#1

random_uni<-function(seed){
  x<<-c()
  for(i in 1:100){
    seed <- (16807*seed)%%2147483647
    x[i]<<-seed/2147483647
  }
  x
}
random_uni(2020)
ks.test(random_uni(2020),runif(100))
install.packages("snpar")
library('snpar')
runs.test(random_uni(2020))

#2
length(which(rbinom(1000,6,0.2)>=1))/1000
mean(rbinom(1000,6,0.2)>=1)
0.74-(1-(0.8)^6)


#3
Buffon = function(n, lofneedle, distance)
{
  lofneedle = lofneedle / 2
  distance = distance / 2
  r1 = runif(n)
  r2 = runif(n)
  prob = mean(r1*distance < lofneedle*sin(r2*pi))
  return(prob)
}

f<-function(x){
  ((2/(pi*20))*15*sin(x)/2)
}
integrate(f,0,pi)

result<-c()
result[5]<-Buffon(5000,15,20)
result[4]<-Buffon(1000,15,20)
result[3]<-Buffon(100,15,20)
result[2]<-Buffon(50,15,20)
result[1]<-Buffon(10,15,20)
abs(result-integrate(f,0,pi)$value)
barplot(abs(result-integrate(f,0,pi)$value),main="Buffon",names=c("10","50","100","1000","5000"),xlab="n")
