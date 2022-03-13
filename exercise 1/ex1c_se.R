N <- 1000
for(a in 0:4) {
  # estimate theoretical standard error of J (a)
  se_a <- sqrt((4*a**2 + 2) / N)

  # estimate theoretical standard error of J (c)
  se_c <- sqrt((3*exp(a**2) - a**4 - 2*a**2 - 1) / N)

  cat("\n\na =", a)
  cat("\nTheoretical se (a) =", se_a)
  cat("\nTheoretical se (c) =", se_c)
}

