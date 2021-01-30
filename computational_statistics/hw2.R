
#1
## 최적화

f = function(x)
{
  f = (1-x[1])^2 + 100*(x[2]-x[1]^2)^2
}

df = function(x)
{
  df1 = -2*(1-x[1])+200*(x[2]-(x[1]^2))*(-2*x[1])
  df2 = 200*(x[2]-x[1]^2)
  df = c(df1, df2)
  return(df)
}


Norm = function(u)
{
  return(sqrt(sum(u^2)))
}
# 최대하강법
m = 100
par(mfrow=c(1,2), pty="s")
x1 = x2 = seq(-10.5, 10.5, length=m)
xg = expand.grid(x1, x2)
z = matrix(apply(xg, 1, f), m, m)
xh = NULL; fh = NULL
x0 = c(0, 3); fx0 = f(x0); ni = 0
for (i in 1:10)
{  
  xh = rbind(xh, x0); fh = c(fh, fx0); ni = ni+1
  cat("iteration=", round(i,2))
  cat("  x0=", round(x0,2), "  f(x0)=", round(f(x0),3), "\n")
  d = df(x0)
  for (iters in 1:20)
  {
    x = x0 - d; fx = f(x)
    if (fx < fx0) break
    d = d 
  }
  x0 = x; fx0 = fx
}
contour(x1, x2, z, levels=round(fh, 2))
for (i in 1:(ni-1))
{
  points(xh[i,1], xh[i,2], pch=as.character(i))
  x1=xh[i,1]; y1=xh[i,2]; x2=xh[i+1,1]; y2=xh[i+1,2]
  arrows(x1, y1, x2, y2, length=0.1, col="red", lwd=0.5)
}
points(xh[ni,1], xh[ni,2], pch=as.character(ni))

#1-2

# 뉴튼 랩슨 알고리즘

x1 = x2 = seq(-10.5, 10.5, length=m)
xg = expand.grid(x1, x2)
z = matrix(apply(xg, 1, f), m, m)
xh = NULL; fh = NULL
x0 = c(0,3); fx0 = f(x0); ni = 0
v=function(x){
  v=matrix(c(200,400*x[1],400*x[1],1200*(x[1]^2)-400*x[2]+2),2,2)/(800*(x[1]^2)-800*x[1]+400)
  return(v)
}
solve(matrix(c(-1200,0,0,200),2,2))
for (i in 1:10)
{  
  xh = rbind(xh, as.vector(x0)); fh = c(fh, fx0); ni = ni+1
  cat("iteration=", round(i,2))
  cat("  x0=", round(x0,2), "  f(x0)=", round(f(x0),3), "\n")
  #   d = df(x0)
  d = v(x0) %*% df(x0)
  for (iters in 1:20)
  {
    x = x0 - d; fx = f(x)
    if (fx < fx0) break
    d = d
  }
  if (abs(fx-fx0) < 1e-5) break
  x0 = x; fx0 = fx
}
contour(x1, x2, z, levels=round(fh, 2))
for (i in 1:(ni-1))
{
  points(xh[i,1], xh[i,2], pch=as.character(i))
  x1=xh[i,1]; y1=xh[i,2]; x2=xh[i+1,1]; y2=xh[i+1,2]
  arrows(x1, y1, x2, y2, length=0.1, col="red", lwd=0.5)
}
points(xh[ni,1], xh[ni,2], pch=as.character(ni))


#2-1 이분법

f = function(x){
  if(x>=1)
    f = (x-1)^(1/3)
  else f= -(abs(x-1))^(1/3)
  return(f)
}
Bisection = function(x0, x1, epsilon)
{
  fx0 = f(x0)
  fx1 = f(x1)
  if (fx0*fx1 >0)
    return("wrong initial values")
  error = abs(x1 - x0)
  N = 1
  while (error > epsilon){
    N = N + 1
    error = error/2
    x2 = (x0 + x1)/2
    fx2 = f(x2)
    if (fx0 * fx2 < 0){
      x1 = x2; fx1 = fx2
    } else if(fx0*fx2>0) {
      x0 = x2; fx0 = fx2
    } else break
  }
  return(list(x = x2, n = N,gap=abs(x2-1)))
}
Bisection(-1,3,1e-5)

#2-2 뉴턴법
f = function(x){
  if(x>=1)
    f = (x-1)^(1/3)
  else f= -(abs(x-1))^(1/3)
  return(f)
}
Newton <-function(x0,epsilon,n){
  e <- 1
  N <- 1
  d <- epsilon
  while(e>d){
    N <- N + 1
    if (N > n){
      return("not converge after 100 iterations")
    }
    x1 <- x0 - f(x0)*d/(f(x0+d)-f(x0))
    e <- abs(x1 - x0)
    x0 <- x1
  }
  return(list(x = x1, n = N,gap=abs(x1-1)))
}
Newton(3,1e-5,100)
#----
f <- function(x){
  return((x-1)^(1/3))
}

Newton = function(x0, epsilon, n)
{
  d = epsilon
  e = 1
  N = 1
  while (e > epsilon)
  {
    N = N + 1
    if (N > n){
      return("not converge after 100 iterations")
    }else{
      x1 = x0 - f(x0) * d / (f(x0 + d) - f(x0))
      e = abs(x1 - x0)
      x0 = x1
    }
  }
  return(list(x = x1, n = N, abs(x1-1)))
}
Newton(3,1e-5,36)
x1 = 3 - f(3)*1e-5/(f(3 + 1e-5) - f(3))
x1 = 3 - f(3)*1e-5/((1/3)*(2)^(-2/3))
# 3
## 수치적분

f = function(x)
{
  f = (3/2)*sqrt(x)
}

integrate(f,0,1)
# 직사각형법
Integral = function(a, b, n)
{
  integral = 0
  h = (b - a) / n
  for (i in 1:n)
    integral = integral + h * f(a + (i-1/2) * h)
  
  return(integral)
}
for (i in 1:10){
  print(Integral(0,1,2*i))
  a1[i]<-Integral(0,1,2*i)
}

#참값과의 차이
for (i in 1:10){
  print(abs(1-Integral(0,1,2*i)))
  a2[i]<-abs(1-Integral(0,1,2*i))
}

# 사다리꼴법
Trapezoid = function(a, b, n)
{
  h = (b - a) / n
  integral = (f(a) + f(b)) / 2
  
  x = a
  n1 = n - 1
  for (i in 1:n1)
  {
    x = x + h
    integral = integral + f(x)
  }
  integral = integral * h
  
  return(integral)
}
for (i in 1:10){
  print(Trapezoid(0,1,2*i))
  b1[i]<-Trapezoid(0,1,2*i)
}
#참값과의 차이
for (i in 1:10){
  print(abs(1-Trapezoid(0,1,2*i)))
  b2[i]<-abs(1-Trapezoid(0,1,2*i))
}

# 심슨 적분법
Simpson = function(a, b, n)
{
  h = (b - a) / n
  integral = f(a) + f(b)
  x2 = a
  x3 = a + h
  even = 0
  odd = f(x3)
  h2 = 2 * h
  n1 = n / 2 - 1
  for (i in 1:n1)
  {
    x2 = x2 + h2
    x3 = x3 + h2
    even = even + f(x2)
    odd = odd + f(x3)
  }
  integral = (integral + 4 * odd + 2 * even) * h / 3
  
  return(integral)
}
for (i in 1:10){
  print(Simpson(0,1,i*2))
  c1[i]<-Simpson(0,1,i*2)
}
#참값과의 차이
for (i in 1:10){
  print(abs(1-Simpson(0,1,2*i)))
  c2[i]<-abs(1-Simpson(0,1,2*i))
}

d<-seq(2,20,2)
value<-cbind(d,a1,a2,b1,b2,c1,c2)
colnames(value)<-c("interation",'Integral','|Integral-1|','Trapezoid','|Trapezoid-1|','Simpson','|Simpson-1|')
value
#-----

f = function(x)
{
  f = (x[1] - 1)^2 + (x[2] - 1)^2 - x[1] * x[2]
}

df = function(x)
{
  df1 = 2 * (x[1] - 1) - x[2]
  df2 = 2 * (x[2] - 1) - x[1]
  df = c(df1, df2)
  return(df)
}

Norm = function(u)
{
  return(sqrt(sum(u^2)))
}

# 최대하강법
m = 100
par(mfrow=c(1,2), pty="s")
x1 = x2 = seq(-10.5, 10.5, length=m)
xg = expand.grid(x1, x2)
z = matrix(apply(xg, 1, f), m, m)
xh = NULL; fh = NULL
x0 = c(-10, -3); fx0 = f(x0); ni = 0
for (i in 1:10)
{  
  xh = rbind(xh, x0); fh = c(fh, fx0); ni = ni+1
  cat("iteration=", round(i,2))
  cat("  x0=", round(x0,2), "  f(x0)=", round(f(x0),3), "\n")
  d = df(x0)
  for (iters in 1:20)
  {
    x = x0 - d; fx = f(x)
    if (fx < fx0) break
    d = d / 2
  }
  x0 = x; fx0 = fx
}
contour(x1, x2, z, levels=round(fh, 2))
for (i in 1:(ni-1))
{
  points(xh[i,1], xh[i,2], pch=as.character(i))
  x1=xh[i,1]; y1=xh[i,2]; x2=xh[i+1,1]; y2=xh[i+1,2]
  arrows(x1, y1, x2, y2, length=0.1, col="red", lwd=0.5)
}
points(xh[ni,1], xh[ni,2], pch=as.character(ni))



t<-function(epsilon){
  e = 1
  N = 1
  d = epsilon
  while (e>d){
    print("a")
  }
}
t(1e-5)

ggplot(data.frame(x=c(0,10)),aes(x=x))+stat_function(fun=fun1,geom='line')
3-(3-1)^(1/3)*(1e-5)/((3+(1e-5)-1)^(1/3)-(3-1)^(1/3))
