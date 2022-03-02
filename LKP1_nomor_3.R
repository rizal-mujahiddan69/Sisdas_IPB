library(Deriv)
fungsiku <- function(x) {
  x^3 + sqrt(x^2 + 2 * x)
}
jawaban <- Deriv(fungsiku, "x")
print(jawaban)
