# D1
d1 = function(nume_fisier, alfa) {
  date = read.csv(nume_fisier, header = TRUE)
  sample_mean = mean(date$probabilitati)
  n = length(date$probabilitati)
  sigma = sqrt(92.16)
  critical.z = qnorm(1 - alfa / 2, 0, 1)
  
  a = sample_mean - critical.z * sigma / sqrt(n)
  b = sample_mean + critical.z * sigma / sqrt(n)
  
  return(c(a, b))
}

print("Interval de incredere de 95%:")
d1("probabilitati.csv", 0.05)
print("Interval de incredere de 99%:")
d1("probabilitati.csv", 0.01)

# D2
d2 = function(nume_fisier, alfa) {
  data = read.csv(nume_fisier, header = TRUE)
  
  sample_mean = mean(data$statistica)
  sd = sd(data$statistica)
  n = length(data$statistica)
  critical.z = qnorm(1 - alfa / 2, 0, 1)
  marja_eroare = critical.z * sd / sqrt(n)
  
  a = sample_mean - marja_eroare
  b = sample_mean + marja_eroare
  
  return(c(a, b))
}

print("Interval de încredere de 95%:")
d2("statistica.csv", 0.05)
print("Interval de încredere de 99%:")
d2("statistica.csv", 0.01)
