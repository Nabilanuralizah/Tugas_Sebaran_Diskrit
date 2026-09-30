#Nabila Nur Alizah 
#Tugas sebaran Diskrit 1

#1.jika rata-rata pelanggan datang ke toko adalah 3 orang per jam,modelkan dengan poisson dan hitung P(X lebih dari sama dengan 5)
# Distribusi Poisson
lambda <- 3
# P(X >= 5)
p <- 1 - ppois(4, lambda)
p

#2.dari 100 bola (20 berwarna merah),diambil 10 tanpa pengembalian.Modelkan jumlah bola merah yang di ambil dengan distribusi yang tepat
# Parameter
N <- 100
K <- 20
n <- 10

# Kemungkinan jumlah bola merah
x <- 0:n

# Probabilitas teoritis
p <- dhyper(x, K, N-K, n)

data.frame(
  Jumlah_Bola_Merah = x,
  Probabilitas = p
)
#melihat distribusi
barplot(
  p,
  names.arg = x,
  xlab = "Jumlah bola merah yang terambil",
  ylab = "Probabilitas",
  main = "Distribusi Hipergeometrik"
)

#3.simulasi 1000 percobaan binomial
# Parameter
n <- 15
p <- 0.4

# Simulasi 1000 percobaan
set.seed(123)
simulasi <- rbinom(1000, size = n, prob = p)

# Histogram hasil simulasi
hist(
  simulasi,
  breaks = seq(-0.5, 15.5, by = 1),
  probability = TRUE,
  col = "black",
  border = "white",
  xlab = "Jumlah sukses (X)",
  ylab = "Probabilitas",
  main = "Simulasi Binomial (n = 15, p = 0.4)"
)

# PMF teoritis
x <- 0:n
pmf <- dbinom(x, size = n, prob = p)

# Tambahkan PMF teoritis
points(
  x, pmf,
  col = "red",
  pch = 19
)

lines(
  x, pmf,
  col = "red",
  lwd = 2
)

legend(
  "topright",
  legend = c("Simulasi", "PMF teoritis"),
  fill = c("black", NA),
  border = c("white", NA),
  col = c(NA, "red"),
  lty = c(NA, 1),
  pch = c(NA, 19)
)
