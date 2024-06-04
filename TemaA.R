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
k0 = 0
while (ppois(k0,lambda) <= threshold) {
  k0 = k0 + 1;
}
print("Cea mai mică valoare a lui k0 pentru care P(Y <= k0) > 1 - 10^-6 este:")
return(k0)
}

print(poisson(3))


#A2
#a)
note_PS = function(file_path){
  data = read.table(file_path, header = TRUE, sep =",")
  sample_P = data[['P']]
  sample_S = data[['S']]
  
  frecvente_P_abs = table(sample_P)
  frecvente_S_abs = table(sample_S)
  
  frecvente_P_rel = as.vector(frecvente_P_abs) / length(sample_P)
  frecvente_S_rel = as.vector(frecvente_S_abs) / length(sample_S)
  
  mean_P = mean(sample_P)
  mean_S = mean(sample_S)
  
  result = list(
    frecvente_P_abs = frecvente_P_abs,
    frecvente_P_rel = frecvente_P_rel,
    frecvente_S_abs = frecvente_S_abs,
    frecvente_S_rel = frecvente_S_rel,
    mean_P = mean_P,
    mean_S = mean_S
  )
}

result = note_PS("note_PS.csv")

print("Frecvențele absolute ale eșantioanelor:")
print(result$frecvente_P_abs)
print(result$frecvente_S_abs)

print("Frecvențele relative ale eșantioanelor:")
print(result$frecvente_P_rel)
print(result$frecvente_S_rel)

print("Media eșantionului 'P':")
print(result$mean_P)

print("Media eșantionului 'S':")
print(result$mean_S)

#b)
elim_aberante = function(file_path, sample_name) {
  data = read.table(file_path, header = TRUE, sep =",")
  sample = as.numeric(data[[sample_name]])
  
  Q1 = quantile(sample, 0.25)
  Q3 = quantile(sample, 0.75)
  IQR = Q3 - Q1
  low = Q1 - 1.5 * IQR
  up = Q3 + 1.5 * IQR
  
  corrected = sample[sample >= low & sample <= up]
  
  breaks = seq(1, 10, by = 1)
  hist(corrected, breaks = breaks, main = paste("Distribuția frecvențelor pentru", sample_name), xlab = sample_name, ylab = "Frecvență")
  
  return(corrected)
}

corrected_P = elim_aberante("note_PS.csv", 'P')
corrected_S = elim_aberante("note_PS.csv", 'S')