# Confidence RDM Simulation#####
# Packages ----
library(ggplot2)
library(dplyr)

# Simulate data ----
t0 <- 0.3
A <- 2
B <- 3
B_for <- -0.5
B_against <- 0.5
v_correct <- 5
v_incorrect <- 1
s_correct <- 1.75
s_incorrect <- 1
w_conf <- 0
w_time <- 1
w_confovertime <- 0


# No bias
no_bias <- data.frame()
n_trials <- 2000
dt <- 0.001

for (trial in 1:n_trials) {
  X_correct <- runif(1, 0, A)
  X_incorrect <- runif(1, 0, A)
  b <- A + B
  t <- t0
  while (X_correct < b & X_incorrect < b) {
    t <- t + dt
    X_correct <- X_correct + v_correct * dt + rnorm(1,0,sqrt(s_correct^2 * dt))
    X_incorrect <- X_incorrect + v_incorrect * dt + rnorm(1,0,sqrt(s_incorrect^2 * dt))
  }
  Conf <- abs(X_correct - X_incorrect)
  ConfTime <- w_conf * Conf + w_time / sqrt(t) + w_confovertime * Conf / sqrt(t)
  no_bias <- rbind(no_bias,
                   data.frame(RT = t,
                              Response = factor(X_correct > X_incorrect,
                                                levels = c(TRUE, FALSE),
                                                labels = c("Correct", "Incorrect")),
                              Bias = "No",
                              Confidence = Conf,
                              ConfTime = ConfTime))
}

# Biased

bias <- data.frame()

for (trial in 1:n_trials) {
  biased_for <- rbinom(1, 1, 0.75)
  X_correct <- runif(1, 0, A)
  X_incorrect <- runif(1, 0, A)
  if (biased_for) {
    b_correct <- A + B + B_for
    b_incorrect <- A + B + B_against
  }
  else {
    b_correct <- A + B + B_against
    b_incorrect <- A + B + B_for
  }
  t <- t0
  while (X_correct < b_correct & X_incorrect < b_incorrect) {
    t <- t + dt
    X_correct <- X_correct + v_correct * dt + rnorm(1,0,sqrt(s_correct^2 * dt))
    X_incorrect <- X_incorrect + v_incorrect * dt + rnorm(1,0,sqrt(s_incorrect^2 * dt))
  }
  Conf <- if (X_correct >= b_correct) b_incorrect - X_incorrect else b_correct - X_correct
  ConfTime <- w_conf * Conf + w_time / sqrt(t) + w_confovertime * Conf / sqrt(t)
  bias <- rbind(bias,
                   data.frame(RT = t,
                              Response = factor(X_correct > X_incorrect,
                                                levels = c(TRUE, FALSE),
                                                labels = c("Correct", "Incorrect")),
                              Bias = factor(biased_for,
                                            levels = c(1, 0),
                                            labels = c("For", "Against")),
                              Confidence = Conf,
                              ConfTime = ConfTime))
}

# Plot data ----

datRDM <- rbind(no_bias, bias)
datRDM$Bias <- factor(datRDM$Bias, levels = c("No", "For", "Against"))

ggplot(datRDM, aes(Bias, Confidence, color = Response)) +
  geom_boxplot()

ggplot(datRDM, aes(Bias, ConfTime, color = Response)) +
  geom_boxplot()

