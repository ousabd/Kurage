library(mvnfast) # to generate multivariate normal data

icc_to_error <- function(sd, mu, icc) {
  x <- rmvn(n = 1000,
            mu = c(mu, mu),
            sigma = matrix(c(sd^2, icc * sd^2, icc * sd^2, sd^2), nrow = 2))
  x1 <- x[, 1]
  x2 <- x[, 2]

  sd(x1) |> print()
  cor.test(x1, x2) |> print()
  plot(x) |> print()

  mean(abs((x1 - x2) / x1)) * 100 |> print()
}

a <- icc_to_error(mu = 70, sd = 8, icc = 0.9)
a
library(dplyr)

summarise()
"a",
"b",
"c",
"d",
"e",
"a",
"c",
library(readr)
read_csv(file = file.path(""))