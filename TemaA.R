#a)
calculate_probabilities = function(lambda, p, n, k, m) {
  k_values = seq(k,m)
  poisson_probs <- dpois(k_values, lambda)
  print("Poisson probabilities:")
  print(poisson_probs)
  geometric_probs <- dgeom(k_values, p)
  print("Geometric probabilities:")
  print(geometric_probs)
  binomial_probs <- dbinom(k_values, n, p)
  print("Binomial probabilities:")
  print(binomial_probs)
  probs=matrix(c(poisson_probs, geometric_probs, binomial_probs), nrow=3, ncol=m-k+1, byrow=TRUE)
  return(probs)
}

probs <- calculate_probabilities(3, 0.5, 10, 2, 5)

#b) 
plot = function(lambda, p,n,k,m){
probs = calculate_probabilities(lambda, p,n,k,m)
x=k:m
barplot(probs[1,], names.arg = x, main = "Poisson", xlab = "Valori aleatorii", ylab = "Probabilitate")
barplot(probs[2,], names.arg = x, main = "Geometrica", xlab = "Valori aleatorii", ylab = "Probabilitate")
barplot(probs[3,], names.arg = x, main = "Binomiala", xlab = "Valori aleatorii", ylab = "Probabilitate")
}
plot(3, 0.5, 10, 2, 5)

#c)
poisson = function(lambda){
threshold = 1 - 0.000001
prob=0
k0 = -1
while (prob <= threshold) {
  k0 = k0 + 1
  prob = prob + dpois(k0, lambda)
}
print("Cea mai mică valoare a lui k0 pentru care P(Y <= k0) > 1 - 10^-6 este:")
k0
}

poisson(3)
