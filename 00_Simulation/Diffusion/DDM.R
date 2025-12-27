# 2DSD ####
# Packages ----
library(ggplot2)

# Simulate data ----

boundary_correct <- 2
boundary_incorrect <- 0.5
z_unbiased <- (boundary_correct + boundary_incorrect) / 2
z_for <- 0.7 * (boundary_correct + boundary_incorrect)
z_against <- 0.3 * (boundary_correct + boundary_incorrect)
bias <- 1
driftrate <- 2
t0 <- 0.2
t_conf <- 0.2
s <- 1

# * No bias ====
no_bias <- data.frame()
n_trials <- 2000
dt <- 0.0001

for (trial in 1:n_trials) {
  X <- z_unbiased
  t <- t0
  t2 <- 0
  while(X < boundary_correct & X > boundary_incorrect) {
    X <- X + dt * driftrate + rnorm(1,0,sqrt(s^2 * dt))
    t <- t + dt
  }
  Response = factor(X >= boundary_correct,
                    levels = c(TRUE, FALSE),
                    labels = c("Correct", "Incorrect"))
  while (t2 < t_conf) {
    X <- X + dt * driftrate + rnorm(1,0,sqrt(s^2 * dt))
    t2 <- t2 + dt
  }
  if (Response == "Correct") {
    Confidence <- X - boundary_correct
  } else {
    Confidence <- boundary_incorrect - X
  }
  no_bias <- rbind(no_bias,
                   data.frame(RT = t,
                              Response = Response,
                              Bias = "No",
                              Confidence = Confidence))
}

# * Biased ====
bias <- data.frame()

for (trial in 1:n_trials) {
  biased_for <- rbinom(1, 1, 0.75)
  if (biased_for) X <- z_for else X <- z_against
  t <- t0
  t2 <- 0
  while(X < boundary_correct & X > boundary_incorrect) {
    X <- X + dt * driftrate + rnorm(1,0,sqrt(s^2 * dt))
    t <- t + dt
  }
  Response = factor(X >= boundary_correct,
                    levels = c(TRUE, FALSE),
                    labels = c("Correct", "Incorrect"))
  while (t2 < t_conf) {
    X <- X + dt * driftrate + rnorm(1,0,sqrt(s^2 * dt))
    t2 <- t2 + dt
  }
  if (Response == "Correct") {
    Confidence <- X - boundary_correct
  } else {
    Confidence <- boundary_incorrect - X
  }
  bias <- rbind(bias,
                   data.frame(RT = t,
                              Response = Response,
                              Bias = factor(biased_for,
                                            levels = c(1, 0),
                                            labels = c("For", "Against")),
                              Confidence = Confidence))
}

# * Biased 2 ====
bias2 <- data.frame()

for (trial in 1:n_trials) {
  biased_for <- rbinom(1, 1, 0.75)
  if (biased_for) X <- z_for else X <- z_against
  t <- t0
  t2 <- 0
  while(X < boundary_correct & X > boundary_incorrect) {
    X <- X + dt * driftrate + rnorm(1,0,sqrt(s^2 * dt))
    t <- t + dt
  }
  Response = factor(X >= boundary_correct,
                    levels = c(TRUE, FALSE),
                    labels = c("Correct", "Incorrect"))
  if (biased_for) X <- X + z_for - z_unbiased else X <- X + z_against - z_unbiased
  while (t2 < t_conf) {
    X <- X + dt * driftrate + rnorm(1,0,sqrt(s^2 * dt))
    t2 <- t2 + dt
  }
  if (Response == "Correct") {
    Confidence <- X - boundary_correct
  } else {
    Confidence <- boundary_incorrect - X
  }
  bias2 <- rbind(bias2,
                data.frame(RT = t,
                           Response = Response,
                           Bias = factor(biased_for,
                                         levels = c(1, 0),
                                         labels = c("For", "Against")),
                           Confidence = Confidence))
}

# Plot data ----
dat2dsd <- rbind(no_bias, bias)
dat2dsd$ConfTime <- (boundary_correct - boundary_incorrect) / sqrt(dat2dsd$RT)

ggplot(dat2dsd, aes(Bias, Confidence, color = Response)) +
  geom_boxplot()

ggplot(dat2dsd, aes(Bias, ConfTime, color = Response)) +
  geom_boxplot()


