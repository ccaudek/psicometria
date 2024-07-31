library(cmdstanr)
library(posterior)
library(psycho)  # For SDT calculations
library(pROC)  # For ROC analysis

set.seed(123)
N <- 1000  # number of observations
J <- 50    # number of individuals

# Generate individual effects
true_sigmai <- 0.5
eps <- rnorm(J, 0, true_sigmai)

# Generate data
individual <- sample(1:J, N, replace = TRUE)

# Set parameters for signal and noise distributions
signal_mean <- 0.5  # Reduced d' for less discrimination
noise_mean <- 0
sd <- 1

# Generate signal and noise
signal <- rnorm(N/2, signal_mean, sd)
noise <- rnorm(N/2, noise_mean, sd)

# Combine signal and noise into a single vector
stimuli <- c(signal, noise)

# Generate corresponding truth vector (1 for signal, 0 for noise)
truth <- c(rep(1, N/2), rep(0, N/2))

# Shuffle the stimuli and truth vectors together
shuffled_indices <- sample(N)
stimuli <- stimuli[shuffled_indices]
truth <- truth[shuffled_indices]

# Decision criterion (adjust this to change the bias)
criterion <- (signal_mean + noise_mean) / 2  # unbiased criterion

# Generate responses with some randomness
responses <- rbinom(N, 1, pnorm(stimuli - criterion))

# Calculate probabilities for RL models
# Use a sigmoid function to map stimuli to probabilities
pa <- 1 / (1 + exp(-(stimuli - criterion)))

# Add some individual variability to the probabilities
pa <- plogis(qlogis(pa) + eps[individual])

# Ensure probabilities are within [0, 1]
pa <- pmin(pmax(pa, 0.001), 0.999)

# Prepare data for Stan
stan_data <- list(
  N = N,
  J = J,
  accepted = responses,
  individual = individual,
  pa = pa
)

# Calculate SDT metrics
hits <- sum(responses[truth == 1])
fas <- sum(responses[truth == 0])
misses <- sum(1 - responses[truth == 1])
crs <- sum(1 - responses[truth == 0])

print(paste("Hits:", hits, "False Alarms:", fas, "Misses:", misses, "Correct Rejections:", crs))

# Estimate SDT parameters from the data
sdt_params <- dprime(n_hit = hits, n_fa = fas, n_miss = misses, n_cr = crs)
print("Estimated SDT Parameters:")
print(sdt_params)


# Compile models
rescorla_wagner_model <- cmdstan_model("rw.stan")

# Fit models
rw_fit <- rescorla_wagner_model$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 2000
)

calculate_predictions <- function(fit, data) {
  v_samples <- fit$draws("v", format = "matrix")
  eps_samples <- fit$draws("eps", format = "matrix")
  
  n_samples <- nrow(v_samples)
  predictions <- matrix(0, nrow = n_samples, ncol = length(data$pa))
  
  for (i in 1:n_samples) {
    v <- v_samples[i, 1]
    eps <- eps_samples[i, ]
    for (n in 1:length(data$pa)) {
      p <- plogis(eps[data$individual[n]]) * v + (1 - v) * data$pa[n]
      predictions[i, n] <- p
    }
  }
  
  colMeans(predictions)
}

# Calculate RL predictions
rl_pred_rw <- calculate_predictions(rw_fit, stan_data)

# Calculate SDT predictions using estimated parameters
sdt_pred <- pnorm(stimuli, mean = sdt_params$dprime / 2, sd = 1)

# Calculate correlations
cor_rl_sdt_rw <- cor(rl_pred_rw, sdt_pred)
print(paste("Correlation between RW RL predictions and SDT predictions:", cor_rl_sdt_rw))

# Calculate accuracy
rl_accuracy_rw <- mean((rl_pred_rw > 0.5) == responses)
sdt_accuracy <- mean((sdt_pred > 0.5) == responses)

print(paste("RW RL model accuracy:", rl_accuracy_rw))
print(paste("SDT model accuracy:", sdt_accuracy))

# ROC analysis
roc_rl_rw <- roc(responses, rl_pred_rw)
roc_sdt <- roc(responses, sdt_pred)

print(paste("RW RL model AUC:", auc(roc_rl_rw)))
print(paste("SDT model AUC:", auc(roc_sdt)))

# Plot ROC curves
plot(roc_rl_rw, main = "ROC Curves Comparison", col = "blue")
plot(roc_sdt, add = TRUE, col = "red")
legend("bottomright", legend = c("RW RL Model",  "SDT Model"), 
       col = c("blue", "red"), lwd = 2)

# Compare parameters
rl_v_rw <- mean(rw_fit$draws("v", format = "matrix"))
rl_sigmai_rw <- mean(rw_fit$draws("sigmai", format = "matrix"))

print(paste("RW RL model v:", rl_v_rw, "sigmai:", rl_sigmai_rw))
print(paste("SDT d-prime:", sdt_params$dprime, "criterion:", sdt_params$c))








############################################################################


set.seed(123)
N <- 1000  # number of observations
J <- 50    # number of individuals

# Generate individual effects
true_sigmai <- 0.5
eps <- rnorm(J, 0, true_sigmai)

# Generate data
individual <- sample(1:J, N, replace = TRUE)

# Set parameters for signal and noise distributions
signal_mean <- 0.5  # Reduced d' for less discrimination
noise_mean <- 0
sd <- 1

# Generate signal and noise
signal <- rnorm(N/2, signal_mean, sd)
noise <- rnorm(N/2, noise_mean, sd)

# Combine signal and noise into a single vector
stimuli <- c(signal, noise)

# Generate corresponding truth vector (1 for signal, 0 for noise)
truth <- c(rep(1, N/2), rep(0, N/2))

# Shuffle the stimuli and truth vectors together
shuffled_indices <- sample(N)
stimuli <- stimuli[shuffled_indices]
truth <- truth[shuffled_indices]

# Decision criterion (adjust this to change the bias)
criterion <- (signal_mean + noise_mean) / 2  # unbiased criterion

# Generate responses with some randomness
responses <- rbinom(N, 1, pnorm(stimuli - criterion))

# Calculate probabilities for RL models
# Use a sigmoid function to map stimuli to probabilities
pa <- 1 / (1 + exp(-(stimuli - criterion)))

# Add some individual variability to the probabilities
pa <- plogis(qlogis(pa) + eps[individual])

# Ensure probabilities are within [0, 1]
pa <- pmin(pmax(pa, 0.001), 0.999)

# Prepare data for Stan
stan_data <- list(
  N = N,
  J = J,
  accepted = responses,
  individual = individual,
  pa = pa
)

# Calculate SDT metrics
hits <- sum(responses[truth == 1])
fas <- sum(responses[truth == 0])
misses <- sum(1 - responses[truth == 1])
crs <- sum(1 - responses[truth == 0])

print(paste("Hits:", hits, "False Alarms:", fas, "Misses:", misses, "Correct Rejections:", crs))

# Estimate SDT parameters from the data
sdt_params <- dprime(n_hit = hits, n_fa = fas, n_miss = misses, n_cr = crs)
print("Estimated SDT Parameters:")
print(sdt_params)


# Compile models (assuming you have the Stan model files)
thompson_model <- cmdstan_model("thompson_model.stan")
softmax_model <- cmdstan_model("softmax_model.stan")

# Fit models
thompson_fit <- thompson_model$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 2000
)

softmax_fit <- softmax_model$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 2000
)


calculate_predictions <- function(fit, data) {
  v_samples <- fit$draws("v", format = "matrix")
  eps_samples <- fit$draws("eps", format = "matrix")
  
  n_samples <- nrow(v_samples)
  predictions <- matrix(0, nrow = n_samples, ncol = length(data$pa))
  
  for (i in 1:n_samples) {
    v <- v_samples[i, 1]
    eps <- eps_samples[i, ]
    for (n in 1:length(data$pa)) {
      p <- plogis(eps[data$individual[n]]) * v + (1 - v) * data$pa[n]
      predictions[i, n] <- p
    }
  }
  
  colMeans(predictions)
}

# Calculate RL predictions
rl_pred_thompson <- calculate_predictions(thompson_fit, stan_data)
rl_pred_softmax <- calculate_predictions(softmax_fit, stan_data)

# Calculate SDT predictions using estimated parameters
sdt_pred <- pnorm(stimuli, mean = sdt_params$dprime/2, sd = 1)

# Calculate correlations
cor_rl_sdt_thompson <- cor(rl_pred_thompson, sdt_pred)
cor_rl_sdt_softmax <- cor(rl_pred_softmax, sdt_pred)
print(paste("Correlation between Thompson RL predictions and SDT predictions:", cor_rl_sdt_thompson))
print(paste("Correlation between Softmax RL predictions and SDT predictions:", cor_rl_sdt_softmax))

# Calculate accuracy
rl_accuracy_thompson <- mean((rl_pred_thompson > 0.5) == responses)
rl_accuracy_softmax <- mean((rl_pred_softmax > 0.5) == responses)
sdt_accuracy <- mean((sdt_pred > 0.5) == responses)

print(paste("Thompson RL model accuracy:", rl_accuracy_thompson))
print(paste("Softmax RL model accuracy:", rl_accuracy_softmax))
print(paste("SDT model accuracy:", sdt_accuracy))

# ROC analysis
roc_rl_thompson <- roc(responses, rl_pred_thompson)
roc_rl_softmax <- roc(responses, rl_pred_softmax)
roc_sdt <- roc(responses, sdt_pred)

print(paste("Thompson RL model AUC:", auc(roc_rl_thompson)))
print(paste("Softmax RL model AUC:", auc(roc_rl_softmax)))
print(paste("SDT model AUC:", auc(roc_sdt)))

# Plot ROC curves
plot(roc_rl_thompson, main = "ROC Curves Comparison", col = "blue")
plot(roc_rl_softmax, add = TRUE, col = "green")
plot(roc_sdt, add = TRUE, col = "red")
legend("bottomright", legend = c("Thompson RL Model", "Softmax RL Model", "SDT Model"), 
       col = c("blue", "green", "red"), lwd = 2)

# Compare parameters
rl_v_thompson <- mean(thompson_fit$draws("v", format = "matrix"))
rl_sigmai_thompson <- mean(thompson_fit$draws("sigmai", format = "matrix"))
rl_v_softmax <- mean(softmax_fit$draws("v", format = "matrix"))
rl_sigmai_softmax <- mean(softmax_fit$draws("sigmai", format = "matrix"))

print(paste("Thompson RL model v:", rl_v_thompson, "sigmai:", rl_sigmai_thompson))
print(paste("Softmax RL model v:", rl_v_softmax, "sigmai:", rl_sigmai_softmax))
print(paste("SDT d-prime:", sdt_params$dprime, "criterion:", sdt_params$c))







