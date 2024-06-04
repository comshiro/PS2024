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
  print(size)
  print("estimare: ")
  print(estimation)
  print("eroarea relativa: ") 
  print(err_rel)
}

b1(20000,10,3)
b1(50000,10,3)
b1(100000,10,3)


#B2
#nodurile sunt (0, 0), (2, 0), (2, 2.4),(0, 2.4)
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
b2(30000)


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
    if (x - 3 > 0){
    sum = sum + ((x + 4) / ((x - 3)^(1/3)))
    }
  }
  estimation = sum / N * (11-3)
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
abs(int3(10000,0)-(1/2))


#B4
#a
n = 1000
p = 0.25
q = 0.01

a = function(nr_start, nr_doriti, n, p, q, nrsim) {
  total=c()
  for (i in 1:nrsim) {
  ani = 0
  nr_initiali=nr_start
  while (nr_initiali < nr_doriti) {
    ani = ani + 1
    noi_users = rbinom(1, n, p)
    users_retragere = rbinom(1, nr_initiali, q)
    nr_initiali = nr_initiali + noi_users - users_retragere
  }
  total=c(total, ani)
  }
  return(mean(total))
}

rezultat = a(10000, 15000, n, p, q,10000)
print("Numărul mediu de ani: ")
rezultat

#b
b = function(nr_sim, nr_initiali, nr_doriti, n, p, q) {
  ok = 0
  
  for (i in 1:nr_sim) {
    ani_totali = 40 + 10/12
    nr_users_final = nr_initiali
    
    for (ani in 1:ani_totali) {
      noi_users = rbinom(1, n, p)
      users_retragere = rbinom(1, nr_users_final, q)
      
      nr_users_final = nr_users_final + noi_users - users_retragere
    }
    
    if (nr_users_final >= nr_doriti) {
      ok = ok + 1
    }
  }
  probabilitate = ok / nr_sim
  
  return(probabilitate)
}

rezultat = b(10000, 10000, 15000, 1000, 0.25, 0.01)
print("Probabilitatea: ")
rezultat

