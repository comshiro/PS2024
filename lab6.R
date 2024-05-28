selection_mean = function(filename) {
  x = scan(filename);
  m = mean(x)
}
alfa =0.1
  sample_mean = 
  n = 
  sigma = 5
  critical.z = qnorm(1 - alfa/2, 0, 1)
  a = sample_mean - critical.z*sigma/sqrt(n)
  b = sample_mean + critical.z*sigma/sqrt(n)
  interval = c(a, b)
interval

#II.6
ex1 = function(nume){
  
  date_esantion = scan(nume, what = numeric())
  sigma  = 5
  alfa  = 0.05
  n  = length(date_esantion)
  sample_mean  = mean(date_esantion)
  critical.z  = qnorm(1 - alfa / 2, mean = 0, sd = 1)
  margine_eroare  = critical.z * (sigma / sqrt(n))
  a  = sample_mean - margine_eroare
  b  = sample_mean + margine_eroare
  interval  = c(a, b)
  interval
}

ex1('history.txt')

#III.4


 ex2 = function(nume, alfa){
   date_esantion = scan(nume, what = numeric())
     sample_mean = mean(date_esantion)
     n = length(date_esantion)
     s = sd(date_esantion)
     se = s/sqrt(n)
     critical.t = qt(1 - alfa/2, n - 1)
     a = sample_mean - critical.t*se
     b = sample_mean + critical.t*se
     interval = c(a, b)
     interval
 }
 ex2('history.txt', 0.05)
 ex2('history.txt', 0.01)