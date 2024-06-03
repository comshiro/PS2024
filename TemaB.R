#B1
b1 = function(size, R, r) {
  nr = 0
  for (i in 1:size){
    x = runif(1, -13, 13)
    y = runif(1, -13, 13)
    z = runif(1, -3, 3)
   
    if ((z^2+(sqrt(x^2+y^2)-R)^2)<r^2){
      nr = nr + 1 
    }
  }
  
  vol = (2 * (R + r))^2 * (2 * r)
  estimation = vol * nr / size  
  vol = 2*pi^2*R*r^2
  err_abs = abs(estimation - vol)
  err_rel = err_abs / abs(vol)
  
  print("esantion de dimensiune ")
  size
  print("estimare: ")
  estimation
  print("eroarea relativa: ") 
  err_rel
}

b1(20000,10,3)
b1(50000,10,3)
b1(100000,10,3)


#B2
b2=function(N){
  c=0
  for(i in 1:N){
    x=runif(1,0,2)
    y=runif(1,0,2.4 )
    if((2*x>=y)&(y<=6-3*x)){
      c=c+1
    }
  }
  return(4.8*c/N)
}
b2(20000)


#B3
#a
int1=function(N,a,b){
  sum=0
  for(i in 1:N){
    x=runif(1,a,b)
    sum=sum+((2*x-1)/(x^2-x-6))
  }
  return((b-a)*sum/N)
}
estimation = int1(10000,-1,1)
val = log(3)-log(2)
eroare = abs(estimation-val)
print("Estimare int1: ") 
estimation
print("Eroare absolută: ")
eroare


#b
int2 = function(N, a, b) {
  sum = 0
  for (i in 1:N) {
    x = runif(1, a, b) 
    if (3*x - 3 > 0){
    sum = sum + ((x + 4) / sqrt(3*x - 3))
    }
  }
  estimation = sum / N
  return(estimation)
}
estimare = int2(20000, 3, 11)
val = 61.2
eroare = abs(estimare - val)

print("Estimare int2: ")
estimare
print("Eroare int2: ") 
eroare

#c
int3=function(N, a, b){
  sum=0
  for(i in 1:N){
    x=rexp(1, 1)
    sum=sum+x*exp(-x^2)/exp(-x)
  }
  return(sum/N)
}

print("estimare b3/c: ")
int3(20000,0)
print("eroare absoluta: ")
abs(int3(10000,0)-1/2)

