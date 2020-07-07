##### Rodando o BVAR para modelo Mensal ####
from = "2016-01"
base_dados_2016 = base_dados_M[paste0(from,"/")] # selecionar para o período de interesse
b_base = ts(base_dados_2016, start = c(2016,01), freq = 12)
data <- gen_var(b_base, p = 3, deterministic = "none")

y <- data$Y[, 1:length(data$Y[1,])]
x <- data$Z[, 1:length(data$Z[1,])]

#Reset random number generator for reproducibility
set.seed(1234)

iter <- 25000*4 # Number of iterations of the Gibbs sampler
burnin <- 25000 # Number of burn-in draws
store <- iter - burnin

t <- ncol(y) # Number of observations
k <- nrow(y) # Number of endogenous variables
m <- k * nrow(x) # Number of estimated coefficients
k0 <- k * (k - 1) / 2 # Number of structural coefficients

# Set (uninformative) priors
a_mu_prior <- matrix(0, m) # Vector of prior parameter means
a_v_i_prior <- diag(0, m) # Inverse of the prior covariance matrix

a0_mu_prior <- matrix(0, k0) # Vector of prior parameter means
a0_v_i_prior <- diag(0, k0) # Inverse of the prior covariance matrix

sigma_df_prior <- 0 # Prior degrees of freedom
sigma_scale_prior <- rep(0, k)
sigma_df_post <- t + sigma_df_prior

# Initial values
a0 <- diag(1, k)
omega_i <- rWishart(1, t, solve(tcrossprod(y)))[,, 1]
omega <- solve(omega_i)
sigma_i <- diag(1, k)
diag(sigma_i) <- diag(omega_i)
sigma <- solve(sigma_i)

# Data containers for posterior draws
draws_a <- matrix(NA, m, store)
draws_a0 <- matrix(NA, k^2, store)
draws_omega <- matrix(NA, k^2, store)

# Start Gibbs sampler
for (draw in 1:iter) {
  # Draw conditional mean parameters
  a <- post_normal(y, x, omega_i, a_mu_prior, a_v_i_prior)
  
  # Structural coefficients
  y_tilde <- y - matrix(a, k) %*% x # Obtain residuals
  for (j in 2:k) {
    # Preparing the data
    y_tilde_temp <- matrix(y_tilde[j, ], 1)
    x0_temp <- matrix(-y_tilde[1:(j - 1),], j - 1)
    a0_sigma_i_temp <-  matrix(sigma_i[j, j])
    pos_temp <- (j - 1) * (j - 2) / 2 + 1:(j - 1)
    mu_temp <- matrix(a0_mu_prior[pos_temp,])
    v_i_temp <- matrix(a0_v_i_prior[pos_temp, pos_temp], j - 1)
    
    # Draw structural coefficients
    a0_temp <- post_normal(y = y_tilde_temp, x = x0_temp, sigma_i = a0_sigma_i_temp,
                           a_prior = mu_temp, v_i_prior = v_i_temp)
    
    # Update A0 matrix
    a0[j, 1:(j - 1)] <- a0_temp
  }
  
  # Draw variances
  y_star <- a0 %*% y_tilde
  sigma_scale_post <- sigma_scale_prior + rowSums(y_star^2)
  for (j in 1:k) {
    sigma_i[j, j] <- rgamma(1, shape = sigma_df_post / 2, rate = sigma_scale_post[j] / 2)
  }
  sigma <- solve(sigma_i)
  
  a0_i <- solve(a0)
  omega <- a0_i %*% tcrossprod(sigma, a0_i) 
  omega_i <- solve(omega)
  
  # Store draws
  if (draw > burnin) {
    draws_a[, draw - burnin] <- a
    draws_a0[, draw - burnin] <- a0
    draws_omega[, draw - burnin] <- sigma
  }
}

A <- rowMeans(draws_a) # Obtain means for every row
A <- matrix(A, k) # Transform mean vector into a matrix
A <- round(A, 3) # Round values
dimnames(A) <- list(dimnames(y)[[1]], dimnames(x)[[1]]) # Rename matrix dimensions

A # Print

A0 <- rowMeans(draws_a0) # Obtain means for every row
A0 <- matrix(A0, k) # Transform mean vector into a matrix
A0 <- round(A0, 3) # Round values
dimnames(A0) <- list(dimnames(y)[[1]], dimnames(y)[[1]]) # Rename matrix dimensions

solve(A0)

Sigma <- rowMeans(draws_omega) # Obtain means for every row
Sigma <- matrix(Sigma, k) # Transform mean vector into a matrix
Sigma <- round(Sigma * 10^4, 2) # Round values
dimnames(Sigma) <- list(dimnames(y)[[1]], dimnames(y)[[1]]) # Rename matrix dimensions

Sigma 


bvar_est_P <- bvar(y = y, x = x, 
                   A = draws_a[1:c(length(draws_a[,1])-length(sigma[1,])),],
                   C = draws_a[c(length(draws_a[,1])-length(sigma[1,])+1)
                               :length(draws_a[,1]),],
                   A0 = draws_a0,
                   Sigma = draws_omega)

bvar_est_P <- thin(bvar_est_P, thin = 5)

FEVD_return <- bvartools::fevd(bvar_est_P, 
                               response = "dibc_br_M",n.ahead = 60,type = "sir")
plot(FEVD_return*100, main = "FEVD-Return")

IRFB_ibc_br <- bvartools::irf(bvar_est_P, impulse = "return_M", response = "dibc_br_M",
                              n.ahead = 60,type = "sir")

plot(IRFB_ibc_br
     , main = "IRF Bayesiano de Impulso do Retorno sobre PIB", xlab = "Period",
     ylab = "IBC-Br")

##### Rodando o BVAR para modelo Trimestral ####
from = "2016-01"
base_dados_2016 = base_dados_var_ag[paste0(from,"/")] # selecionar para o período de interesse
b_base = ts(base_dados_2016, start = c(2016,01), freq = 4)
data <- gen_var(b_base, p = 1, deterministic = "const")

y <- data$Y[, 1:length(data$Y[1,])]
x <- data$Z[, 1:length(data$Z[1,])]

#Reset random number generator for reproducibility
set.seed(1234)

iter <- 25000 # Number of iterations of the Gibbs sampler
burnin <- 25000/4 # Number of burn-in draws
store <- iter - burnin

t <- ncol(y) # Number of observations
k <- nrow(y) # Number of endogenous variables
m <- k * nrow(x) # Number of estimated coefficients
k0 <- k * (k - 1) / 2 # Number of structural coefficients

# Set (uninformative) priors
a_mu_prior <- matrix(0, m) # Vector of prior parameter means
a_v_i_prior <- diag(0, m) # Inverse of the prior covariance matrix

a0_mu_prior <- matrix(0, k0) # Vector of prior parameter means
a0_v_i_prior <- diag(0, k0) # Inverse of the prior covariance matrix

sigma_df_prior <- 0 # Prior degrees of freedom
sigma_scale_prior <- rep(0, k)
sigma_df_post <- t + sigma_df_prior

# Initial values
a0 <- diag(1, k)
omega_i <- rWishart(1, t, solve(tcrossprod(y)))[,, 1]
omega <- solve(omega_i)
sigma_i <- diag(1, k)
diag(sigma_i) <- diag(omega_i)
sigma <- solve(sigma_i)

# Data containers for posterior draws
draws_a <- matrix(NA, m, store)
draws_a0 <- matrix(NA, k^2, store)
draws_omega <- matrix(NA, k^2, store)

# Start Gibbs sampler
for (draw in 1:iter) {
  # Draw conditional mean parameters
  a <- post_normal(y, x, omega_i, a_mu_prior, a_v_i_prior)
  
  # Structural coefficients
  y_tilde <- y - matrix(a, k) %*% x # Obtain residuals
  for (j in 2:k) {
    # Preparing the data
    y_tilde_temp <- matrix(y_tilde[j, ], 1)
    x0_temp <- matrix(-y_tilde[1:(j - 1),], j - 1)
    a0_sigma_i_temp <-  matrix(sigma_i[j, j])
    pos_temp <- (j - 1) * (j - 2) / 2 + 1:(j - 1)
    mu_temp <- matrix(a0_mu_prior[pos_temp,])
    v_i_temp <- matrix(a0_v_i_prior[pos_temp, pos_temp], j - 1)
    
    # Draw structural coefficients
    a0_temp <- post_normal(y = y_tilde_temp, x = x0_temp, sigma_i = a0_sigma_i_temp,
                           a_prior = mu_temp, v_i_prior = v_i_temp)
    
    # Update A0 matrix
    a0[j, 1:(j - 1)] <- a0_temp
  }
  
  # Draw variances
  y_star <- a0 %*% y_tilde
  sigma_scale_post <- sigma_scale_prior + rowSums(y_star^2)
  for (j in 1:k) {
    sigma_i[j, j] <- rgamma(1, shape = sigma_df_post / 2, rate = sigma_scale_post[j] / 2)
  }
  sigma <- solve(sigma_i)
  
  a0_i <- solve(a0)
  omega <- a0_i %*% tcrossprod(sigma, a0_i) 
  omega_i <- solve(omega)
  
  # Store draws
  if (draw > burnin) {
    draws_a[, draw - burnin] <- a
    draws_a0[, draw - burnin] <- a0
    draws_omega[, draw - burnin] <- sigma
  }
}

A <- rowMeans(draws_a) # Obtain means for every row
A <- matrix(A, k) # Transform mean vector into a matrix
A <- round(A, 3) # Round values
dimnames(A) <- list(dimnames(y)[[1]], dimnames(x)[[1]]) # Rename matrix dimensions

A # Print

A0 <- rowMeans(draws_a0) # Obtain means for every row
A0 <- matrix(A0, k) # Transform mean vector into a matrix
A0 <- round(A0, 3) # Round values
dimnames(A0) <- list(dimnames(y)[[1]], dimnames(y)[[1]]) # Rename matrix dimensions

solve(A0)

bvar_est_P <- bvar(y = y, x = x, 
                   A = draws_a[1:c(length(draws_a[,1])-length(sigma[1,])),],
                   C = draws_a[c(length(draws_a[,1])-length(sigma[1,])+1)
                               :length(draws_a[,1]),],
                   A0 = draws_a0,
                   Sigma = draws_omega)

bvar_est_P <- thin(bvar_est_P, thin = 5)

FEVD_invest <- bvartools::fevd(bvar_est_P, response = "invest",n.ahead = 20,type = "sir")
plot(FEVD_invest*100, main = "FEVD-PIB_BR")

IRFB_invest <- bvartools::irf(bvar_est_P, impulse = "return_T", response = "invest",
                              n.ahead = 20,type = "sir")

plot(IRFB_invest
     , main = "IRF Bayesiano de Impulso do Retorno sobre Investimento"
     , xlab = "Period",
     ylab = "IBC-Br")

FEVD_PIB_br <- bvartools::fevd(bvar_est_P, response = "PIB_br",n.ahead = 20,type = "sir")
plot(FEVD_PIB_br*100, main = "FEVD_PIB_br")

IRFB_PIB_br <- bvartools::irf(bvar_est_P, impulse = "return_T", response = "PIB_br",
                              n.ahead = 20,type = "sir")

plot(IRFB_PIB_br
     , main = "IRF Bayesiano de Impulso do Retorno sobre Investimento"
     , xlab = "Period",
     ylab = "IBC-Br")
