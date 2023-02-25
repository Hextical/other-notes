set.seed(20780430)
N = 100
n = 50
Y = rexp(N)
ybarSRSWOR = c()
samplevarSRSWOR = c()
ybarSRSWR = c()
samplevarSRSWR = c()
K = 1000
for (k in 1:K) {
  sam1 = sample(N, n, replace = FALSE)
  ysam1 = Y[sam1]
  ybarSRSWOR[k] = mean(ysam1)
  samplevarSRSWOR[k] = var(ysam1)
  
  sam2 = sample(N, n, replace = FALSE)
  ysam2 = Y[sam2]
  ybarSRSWR[k] = mean(ysam2)
  samplevarSRSWR[k] = var(ysam2)
}
# sampledWOR
mean(ybarSRSWOR)
mean(samplevarSRSWOR)

# sampledWR
mean(ybarSRSWR)
mean(samplevarSRSWR)

# true
mean(Y)
var(Y)
  
# var(ybar)
var(ybarSRSWOR)
# var(ybar)=(1-n/N)*sy^2/n
(1-n/N)*mean(samplevarSRSWOR)/n

var(ybarSRSWR)
m <- length(unique(ybarSRSWR))
(1/m-1/N)*mean(ybarSRSWR)

obtain_sys = function(N, n) {
  K = ceiling(N / n)
  k = sample(1:K, 1)
  seq(k, k + K * (n - 1), K)
}



sumat = 0
for (i in 1:N) {
  for (j in 1:N) {
    if (i != j) {
      sumat = sumat + Y[i] ^ 2 - 2 * Y[i] * Y[j] + Y[j] ^ 2
    }
  }
}
sumat

1 / (N - 1) * sum((Y - mean(Y)) ^ 2)
1 / ((2 * N) * (N - 1)) * (sumat)

1/(N-1)*(sum(Y^2)-N*mean(Y)^2)
1 / (2 * N * (N - 1)) * sumat
var(Y)
