
## home work 3
install.packages("Rfast")
library("Rfast")
#1 - a

x<-matrix(c(1,1,1,1,2,3,1,3,3,1,5,4,1,5,4,1,7,5),byrow=TRUE, ncol=3)
x
y<-c(2,4,5,8,8,9)
y
x1<-t(x)%*%x
t(chol(x1))

choleskyfactorization<-function(A){
  n=nrow(A)
  L=matrix(0,nrow=n,ncol=n)
  for(i in 1:n){
    L[i,i]<-A[i,i]
    if(i>1){
      for(k in (1:(i-1))){
        L[i,i]<-L[i,i]-L[i,k]*L[i,k]
      }
    }
    L[i,i]<-(L[i,i])^(1/2)
    if(i<n){
      for(j in ((i+1):n)){
        L[j,i] <- A[j,i]
        if(i>1){
          for(k in (1:(i-1))){
            L[j,i]<-L[j,i]-L[j,k]*L[i,k]
          }
        }
        L[j,i]<-L[j,i]/L[i,i]
      }
    }
  }
  return(L)
}
choleskyfactorization(x1)

#1-(b)
H3.svd = svd(x1)
H3.svd$d
H3.svd$u %*% diag(H3.svd$d) %*% t(H3.svd$v)
H3.svd$v %*% diag(1/H3.svd$d) %*% t(H3.svd$u) # inverse
eigen(x1)$values

#1-(C)
H3.qr = qr(x1)
H3.qr
Q = qr.Q(H3.qr)
Q
R = qr.R(H3.qr)
R
Q %*% R
qr.solve(R) %*% t(Q)	# inverse
solve(R)%*%(t(Q)%*%y)

lm(y~x[,2]+x[,3])



H3.qr = qr(x1)
H3.qr
Q = qr.Q(H3.qr)
Q
R = qr.R(H3.qr)
R
Q %*% R
qr.solve(R) %*% t(Q)	# inverse
qr.solve(R) %*% t(Q)%*%t(x)%*%y
solve(R)%*%(t(Q)%*%y)

#2
x<-matrix(c(5,-2,1,3,1,1,4,1,-1),byrow=TRUE,ncol=3)
y<-c(9,2,3)
x2<-cbind(x,y)
gaussianeliminationpartial = function(Ab){
  n = nrow(Ab)
  for (k in (1:(n-1))){
    pivotindex = k
    for (i in ((k+1):n)){
      if (abs(Ab[i,k]) > abs(Ab[pivotindex,k])){
        pivotindex = i
      }
    }
    if (pivotindex != k){
      for (j in (k:(n+1))){
        buffer = Ab[k,j]
        Ab[k,j] = Ab[pivotindex,j]
        Ab[pivotindex,j] = buffer
      }
    }
    for (i in ((k+1):n)){
      mik = Ab[i,k]/Ab[k,k]
      Ab[i,k] = 0
      for (j in ((k+1):(n+1))){
        Ab[i,j] = Ab[i,j] - mik*Ab[k,j]
      }
    }
  }
  return(Ab)
}
gaussianeliminationpartial(x2)
vector<-c()

c<-gaussianeliminationpartial(x2)[3,4]/gaussianeliminationpartial(x2)[3,3]
b<-(gaussianeliminationpartial(x2)[2,4]-gaussianeliminationpartial(x2)[2,3]*c)/gaussianeliminationpartial(x2)[2,2]
a<-(gaussianeliminationpartial(x2)[1,4]-gaussianeliminationpartial(x2)[1,3]*c-gaussianeliminationpartial(x2)[1,2]*b)/gaussianeliminationpartial(x2)[1,1]
a
b
c

solve(x)%*%y

#3
x<-matrix(c(1,1,2,2,-2,-1,-1,1,1),ncol=3)
lufactorization = function(A){
  n = nrow(A)
  L = matrix(0,nrow=n,ncol=n)
  for (k in (1:(n-1))){
    for (i in ((k+1):n)){
      L[i,k] = A[i,k]/A[k,k]
      A[i,k] = 0
      for (j in ((k+1):n)){
        A[i,j] = A[i,j] - L[i,k]*A[k,j]
      }
    }
  }
  for (k in (1:n)) L[k,k] = 1
  return(cbind(L,A))
}
det(lufactorization(x)[1:3,1:3]%*%lufactorization(x)[1:3,4:6])

det(x)

