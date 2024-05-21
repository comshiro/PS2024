
p = numeric(i)
x = numeric(i)
p[1]=runif(1,0,1)
Prob=1-p[1]
x[1]=0;
for (j in 2:i){
  x[j] = j;
  p[j] = runif(1,0,Prob)
  Prob=Prob-p[j]
}

sim_distributie = function(x,p){
  u = runif(1,0,1)
  s=0;
  for( i in 1:length(x))
    {
    s=s+p[i];
    if(s >= u)
      return(x[i])
  }
}


