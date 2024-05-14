disc_area = function(N) {
  N_C = 0;
  for(i in 1:N) {
    x = runif(1, -1, 1);
    y = runif(1, -1, 1);
    if(x*x + y*y <= 1)
      N_C = N_C + 1;
  }
  return(4*N_C/N);
}


sphere_vol = function(N) {
  N_C = 0;
  for(i in 1:N) {
    x = runif(1, -1, 1);
    y = runif(1, -1, 1);
    z = runif(1, -1, 1);
    if(x*x + y*y + z*z <= 1)
      N_C = N_C + 1;
  }
  return(8*N_C/N);
}

abs_err=abs(sphere_vol(10000)-4*pi/3)
abs_err
rel_err= abs_err/(4*pi/3)
rel_err

#I.2

abs_err = function(x,y)
{return(x-y);}
rel_err = function(z,w)
{return(z/w);}

f = function(x)
{
  return(-2*x*x+5*x-2)
}
area = function(N) {
  N_C = 0;
  for(i in 1:N) {
    x = runif(1, 0, 2);
    y = runif(1, 0, 2);
    if(x>=1/2 && y<=f(x))
      N_C = N_C + 1;
  }
  return(4*N_C/N);
}

area(10000)

integrala=function(y,x)
{ return(-2/3*x*x*x+5/2*x*x-2*x - (-2/3*y*y*y+5/2*y*y-2*y)) }

integrala(1/2,2)
rel_err(abs_err(integrala(1/2,2),area(10000)),integrala(1/2,2))


#II.1 
#b
MCintgr = function(N) {
  sum = 0;
  for(i in 1:N) {
    x = runif(1, 1, 4);
    sum = sum + exp(x);
  }
  return(sum/N);
}

MC_integr_average= function(k, N) {
  estimates = vector();
  for(i in 1:k)
    estimates[i] = MCintgr(N);
  print(mean(estimates));
  print(sd(estimates));
}
  
MC_integr_average(30,20000)

#d