
ex9=function(n,p){
  
  x=0:(n-1)
  y=dgeom(x,p)
  barplot(y)
}

ex10=function(n,l){
  
  x=0:(n-1)
  y=dpois(x,l)
  barplot(y)
}