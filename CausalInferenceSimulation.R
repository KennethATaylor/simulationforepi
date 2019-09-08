
# Simulate a truth
id <- 1:20
Ya1 <- c(T, T, T, F, F,
		 T, T, F, F, T, 
		 F, F, F, F, T,
		 T, T, T, F, F)
Ya0 <- c(T, F, T, F, F,
		 T, F, T, F, T, 
		 F, F, T, F, F,
		 T, F, T, F, F)

# Create the cohort
(cohort <- data.frame(id, Ya1, Ya0))

# Compute the true populaton average casual effect
mean(cohort$Ya1) - mean(cohort$Ya0)

# Note that we can identify individual causal response types because this is a simulation
cohort$response_type <- ifelse(Ya1&Ya0, 
								"doomed",
								ifelse(Ya1&!Ya0, 
										"causal",
										ifelse(!Ya1&Ya0,
											"preventative",
											"immune")))
table(cohort$response_type)


# Now assign some treatment
cohort$A <- c(T, T, T, T, T,
              T, T, T, T, T,
              F, F, F, F, F,
              F, F, F, F, F)
# Compute the observed outcome under treatment
cohort$Y <- ifelse(cohort$A, cohort$Ya1, cohort$Ya0)
# Note that the conditional on treatment observed association does not represent the causal effect
mean(cohort$Y[cohort$A==1]) - mean(cohort$Y[cohort$A == 0])

# response types are not evenly distributed
table(cohort$response_type, cohort$A)


# What would have happened if we assigned treatment so it was very imbalanced?
cohort$A <- c(T, T, T, F, F,
              T, T, F, F, T,
              F, F, F, F, T,
              T, T, T, F, F)
# Compute the observed outcome under this treatment regime
cohort$Y <- ifelse(cohort$A, cohort$Ya1, cohort$Ya0)

# Now the association of treatment and outcome is very strong
mean(cohort$Y[cohort$A==1]) - mean(cohort$Y[cohort$A == 0])
# Because the response types are totally imbalanced
table(cohort$response_type, cohort$A)

# What actual treatment was perfectly balanced?
cohort$A <- c(T, T, F, F, F,
              T, F, F, F, T,
              T, T, T, T, F,
              F, T, F, F, T)
table(cohort$response_type, cohort$A)

# Now the observed association is the causal effect
cohort$Y <- ifelse(cohort$A, cohort$Ya1, cohort$Ya0)
mean(cohort$Y[cohort$A==1]) - mean(cohort$Y[cohort$A == 0])

# So what if we randomize treatment?
set.seed(12345)
cohort$A <- rbinom(20, 1, .5) == 1
cohort$Y <- ifelse(cohort$A, cohort$Ya1, cohort$Ya0)
# Our observed estimate was not the truth
mean(cohort$Y[cohort$A==1]) - mean(cohort$Y[cohort$A == 0])


# Let's compute the outcome under randomization 500 times
set.seed(12345)
reps <- 500
result <- vector(length=reps)
for (i in 1:length(result)) {
  A <- rbinom(20, 1, .5) == 1
  Y <- ifelse(A, cohort$Ya1, cohort$Ya0)
  # Closer to, but not exactly, the truth
  result[i] <- mean(Y[A==1]) - mean(Y[A == 0])
} 

# Asymptotically approaching truth
results_over_runs <- data.frame(
  run <- 1:reps,
  cumulative_mean <- cumsum(result)/seq_along(result)
)

# It takes quite a while to converge to the causal effect
plot(results_over_runs$run, results_over_runs$cumulative_mean)

