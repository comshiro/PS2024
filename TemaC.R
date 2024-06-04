#C1
#a
permutation = function(n){
  U = runif(n, 0, 1)
  perm = order(U)
  return (c(U,perm))
}
permu=permutation(4)
permu

gen_bits = function(n,k){
  bits = matrix(0, nrow=n, ncol=k)
  for (i in 1:n)
    bits[i,]=sample(c(0, 1), k, replace = TRUE)
  return(bits)
}


#b

comp = function(w1, w2){
  for (i in 1:min(length(w1), length(w2))){
    if (w1[i] < w2[i])
      return (TRUE)
    else if (w1[i] > w2[i])
      return (FALSE)
  }
  if (length(w1) < length(w2)){
    for (i in (length(w1) + 1):length(w2)){
      w1 = append(w1, sample(0:1, 1))
      if (w1[i] < w2[i])
        return (TRUE)
      else if (w1[i] > w2[i])
        return (FALSE)
    }
  }
  else if (length(w1) > length(w2)){
    for (i in (length(w2) + 1):length(w1)){
      w2 = append(w2, sample(0:1, 1))
      if (w1[i] < w2[i])
        return (TRUE)
      else if (w1[i] > w2[i])
        return (FALSE)
    }
  }
  if (length(w1) == length(w2)){
    i = length(w1) + 1
    while (TRUE){
      w1 = append(w1, sample(0:1, 1))
      w2 = append(w2, sample(0:1, 1))
      if (w1[i] < w2[i])
        return (TRUE)
      else if (w1[i] > w2[i])
        return (FALSE)
      i = i + 1
    }
  }
}


#c

quickSort = function(matrix) {
  if (nrow(matrix) <= 1) {
    return(matrix)
  }
  pivot_index = sample(1:nrow(matrix), 1)
  pivot = matrix[pivot_index, , drop = FALSE]
  S1 = matrix[apply(matrix, 1, function(row) comp(row, pivot)), , drop = FALSE]
  S2 = matrix[apply(matrix, 1, function(row) !comp(row, pivot)), , drop = FALSE]
  sorted_S1 = quickSort(S1)
  sorted_S2 = quickSort(S2)
  return(rbind(sorted_S1, pivot, sorted_S2))
}

matrix = gen_bits(6, 5)

print("Original Matrix:\n")
print(matrix)


print("\nSorted Matrix:\n")
quickSort(matrix)



#C2
#a
bipartit = function(n, muchii) {
  V = sample(c(0, 1), n, replace = TRUE) #1 pt A 0 pt B
  size = 0
  for (i in 1:n) {
    for (j in 1:n) {
      if (V[i]!= V[j] && muchii[i, j] == 1) {
        size = size + 1
      }
    }
  }
  return(size)
}

n=12
muchii = matrix(0, nrow = 12, ncol = 12)
for (i in 1:12){
  muchii[i, ] = sample(0:1, 12, replace = TRUE)
}

maxim = 0

for (i in 1:1000)
  maxim = max(bipartit(n, muchii), maxim)

print(maxim)

