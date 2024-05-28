#A1
#a)
calculate_probabilities = function(lambda, p, n, k, m) {
  k_values = seq(k,m)
  poisson_probs = dpois(k_values, lambda)
  print("Poisson probabilities:")
  print(poisson_probs)
  geometric_probs = dgeom(k_values, p)
  print("Geometric probabilities:")
  print(geometric_probs)
  binomial_probs = dbinom(k_values, n, p)
  print("Binomial probabilities:")
  print(binomial_probs)
  probs=matrix(c(poisson_probs, geometric_probs, binomial_probs), nrow=3, ncol=m-k+1, byrow=TRUE)
  return(probs)
}

probs = calculate_probabilities(3, 0.5, 10, 2, 5)

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

#A2
#a)
note_PS = function(file_path){
data = read.table(file_path, header = TRUE)
sample_P = data[['P']]
sample_S = data[['S']]

frequencies_P = as.vector(table(sample_P))
frequencies_relative_P = frequencies_P / sum(frequencies_P)

frequencies_S = as.vector(table(sample_S))
frequencies_relative_S = frequencies_S / sum(frequencies_S)
  
mean_P = mean(sample_P)
mean_S = mean(sample_S)

result = list(
  frequencies_P = frequencies_P,
  frequencies_relative_P = frequencies_relative_P,
  frequencies_S = frequencies_S,
  frequencies_relative_S = frequencies_relative_S,
  mean_P = mean_P,
  mean_S = mean_S
)
}

result = note_PS("E:\\PS\\note_PS")


print("Frecvențele absolute ale eșantioanelor:")
print(result$frequencies_P)
print(result$frequencies_S)

print("Frecvențele relative ale eșantioanelor:")
print(result$frequencies_relative_P)
print(result$frequencies_relative_S)

print("Media eșantionului 'P':")
print(result$mean_P)

print("Media eșantionului 'S':")
print(result$mean_S)

#b)
elim_aberante = function(file_path, sample_name) {
  data = read.table(file_path, header = TRUE)
  
  sample = data[[sample_name]]
  
  Q1 = quantile(sample, 0.25)
  Q3 = quantile(sample, 0.75)
  IQR = Q3 - Q1
  lower_limit = Q1 - 1.5 * IQR
  upper_limit = Q3 + 1.5 * IQR
  
  corrected = sample[sample >= lower_limit & sample <= upper_limit]
  
  breaks = seq(1, 10, by = 1)
  hist(corrected, breaks = breaks, main = paste("Distribuția frecvențelor pentru", sample_name), xlab = sample_name, ylab = "Frecvență")
  
  return(corrected)
}

corrected_P = elim_aberante("E:\\PS\\note_PS", "P")