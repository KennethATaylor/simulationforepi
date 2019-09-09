library(ggplot2)
library(lme4)

eitc <- read.csv("/Users/sjm2186/OneDrive/EITC/UKCPR_National_Welfare_Data_Update_060519.csv")
eitc <- eitc[1:1938,]
tail(eitc)
str(eitc)





simulate_data <- function(sim_criteria=list()) {
	# Get Simulation parameters
  if ("subjects_per_state" %in% names(sim_criteria)) {
    if (is.numeric(sim_criteria$subjects_per_state)) {
      subjects_per_state <- sim_criteria$subjects_per_state
    }
  }
	subjects_per_state <- ifelse("subjects_per_state" %in% names(sim_criteria), sim_criteria$subjects_per_state, 200)
	baseline_outcome_mean <- ifelse("baseline_outcome_mean" %in% names(sim_criteria), sim_criteria$baseline_outcome_mean, 300)
	baseline_outcome_sd <- ifelse("baseline_outcome_sd" %in% names(sim_criteria), sim_criteria$baseline_outcome_sd, 25)
	baseline_year <- ifelse("baseline_year" %in% names(sim_criteria), sim_criteria$baseline_year, 2000)
	n_followup_years <- ifelse("n_followup_years" %in% names(sim_criteria), sim_criteria$n_followup_years, 17)
	state_effect_sd <- ifelse("state_effect_sd" %in% names(sim_criteria), sim_criteria$state_effect_sd, 25)
	n_states <- ifelse("n_states" %in% names(sim_criteria), sim_criteria$n_states, 51)
	year_effect_sd <- ifelse("year_effect_sd" %in% names(sim_criteria), sim_criteria$year_effect_sd, 5)
	per_subject_year_change_sd <- ifelse("per_subject_year_change_sd" %in% names(sim_criteria), sim_criteria$per_subject_year_change_sd, 5)
	prob_eitc_eligible <- ifelse("prob_eitc_eligible" %in% names(sim_criteria), sim_criteria$prob_eitc_eligible, 0.1)
	eitc_effect_per_percent <- ifelse("eitc_effect_per_percent" %in% names(sim_criteria), sim_criteria$eitc_effect_per_percent, -300)
	prob_family_type <- ifelse("prob_family_type" %in% names(sim_criteria), sim_criteria$prob_family_type, 0.5)
	
		
	if (baseline_year < 1980) { stop("Baseline year too high")}
	if (n_followup_years + baseline_year > 2017) { stop("Final year out of EITC range")}
	
	state_effect <- rnorm(n_states, 0, state_effect_sd)
	year_effect <- rnorm(n_years, 0, year_effect_sd)
	
	n_total_subjects <- n_states * subjects_per_state
	n_years <- n_followup_years + 1

	outcome_vals <- vector(length=n_total_subjects * n_years)
	outcome_underlying_vals <- vector(length=n_total_subjects * n_years)
	eitc_eligible <- vector(length=n_total_subjects * n_years)
	eitc_rate <- vector(length=n_total_subjects * n_years)
	realized_eitc_effect <- vector(length=n_total_subjects * n_years)
	family_type <- vector(length=n_total_subjects * n_years)
	max_benefit <- vector(length=n_total_subjects * n_years)
	
	for (i in 1:n_states) {
		baseline_outcome <- rnorm(subjects_per_state, baseline_outcome_mean, baseline_outcome_sd)
		baseline_idx <- ((i-1)*subjects_per_state*n_years + 1):((i-1)*subjects_per_state*n_years + subjects_per_state)
		eitc_eligible[baseline_idx] <- rbinom(subjects_per_state, 1, prob_eitc_eligible)
		family_type[baseline_idx] <- rbinom(subjects_per_state, 1, prob_family_type)
		state_rate <- eitc$State.EITC.Rate[eitc$state == i & eitc$year == baseline_year]
		eitc_effect <- eitc_eligible[baseline_idx] * state_rate * eitc_effect_per_percent
		family_type_multiplier <- ifelse(family_type[baseline_idx], 2, 1)
		max_benefit[baseline_idx] <- state_rate * eitc_effect_per_percent * family_type_multiplier
		eitc_effect <- eitc_effect * family_type_multiplier
		outcome_underlying_vals[baseline_idx] <- baseline_outcome
		outcome_vals[baseline_idx] <- baseline_outcome + state_effect[i] + year_effect[1] + eitc_effect
		for (j in 1:n_followup_years) {
			year <- baseline_year + j
			this_year_idx <- ((i-1)*subjects_per_state*n_years + (j-1)*subjects_per_state + 1):((i-1)*subjects_per_state*n_years + j*subjects_per_state)
			this_year_outcome_underlying <- outcome_underlying_vals[this_year_idx]
			next_year_idx <- this_year_idx + subjects_per_state
			random_change <- rnorm(subjects_per_state, 0, per_subject_year_change_sd)
			outcome_underlying_vals[next_year_idx] <- this_year_outcome_underlying + random_change
			state_rate <- eitc$State.EITC.Rate[eitc$state == i & eitc$year == year]
			eitc_effect <- as.numeric(eitc_eligible[this_year_idx]) * state_rate * eitc_effect_per_percent
			family_type_multiplier <- ifelse(family_type[this_year_idx], 2, 1)
			eitc_effect <- eitc_effect * family_type_multiplier
			max_benefit[next_year_idx] <- state_rate * eitc_effect_per_percent * family_type_multiplier
			outcome_vals[next_year_idx] <- outcome_underlying_vals[next_year_idx] + state_effect[i] + year_effect[j] + eitc_effect		
			eitc_rate[next_year_idx] <- state_rate
			# For the moment, EITC eligibility does not change over time
			eitc_eligible[next_year_idx] <- eitc_eligible[this_year_idx]
			# ditto with family type
			family_type[next_year_idx] <- family_type[this_year_idx]
			realized_eitc_effect[next_year_idx] <- eitc_effect
		}
	}


	id_in_state <- rep(rep(1:subjects_per_state, n_years), n_states)
	state <- rep(1:n_states, each=(subjects_per_state * n_years))
	id <- sprintf("%s_%s", state, id_in_state)
	year <- rep(baseline_year:(baseline_year+n_followup_years), n_states, each=subjects_per_state)

 	sim_data <- data.frame(
		id = id,
		state = factor(state),
		year = year,
		eitc_eligible = eitc_eligible,
		family_type = family_type,
		max_benefit = max_benefit,
		eitc_rate = eitc_rate,
		realized_eitc_effect = realized_eitc_effect,
		outcome = outcome_vals
	)
	
	return(sim_data)
}

# People: 1000/state
subjects_per_state <- 1000
# Proportion EITC eligible
prob_eitc_eligible <- .1
baseline_outcome_mean <- 300
baseline_outcome_sd <- 25
eitc_effect_per_percent <- -25
per_subject_year_change_sd <- 5

n_states <- 51
baseline_year <- 2000
n_followup_years <- 17
n_years <- n_followup_years + 1


sim_data <- simulate_data(list(eitc_effect_per_percent = eitc_effect_per_percent, 
                               prob_eitc_eligible=prob_eitc_eligible))
head(sim_data)
head(sim_data[sim_data$realized_eitc_effect < 0,])
head(sim_data[sim_data$realized_eitc_effect == 0 & sim_data$max_benefit != 0,])

	
# 8: DC
# 9: Delaware
# 10: Florida
three_states <- sim_data[sim_data$state %in% c(8, 9, 10),]

number_to_name <- c("DE", "DC", "FL")
names(number_to_name) <- 8:10 

ggplot(three_states) + 
	aes(x=year, y=outcome, group=id, color=state, linetype=as.factor(eitc_eligible)) + 
	geom_line() + 
	facet_wrap(state ~ .,labeller = as_labeller(number_to_name)) + 
	scale_color_brewer(palette="Dark2", labels=c("DE", "DC", "FL")) 

sim_data$any_eitc <- sim_data$eitc_rate > 0

summary(lm(outcome ~ eitc_rate + state + as.factor(year), data=sim_data))
summary(lm(outcome ~ any_eitc + state + as.factor(year), data=sim_data))
summary(lm(outcome ~ max_benefit + state + as.factor(year), data=sim_data))
summary(lm(outcome ~ max_benefit + state + as.factor(year), data=sim_data[runif(nrow(sim_data)) < 0.1,]))

sim_data_all_eligible <- simulate_data(list(prob_eitc_eligible = 1))
summary(lm(outcome ~ eitc_rate + state + as.factor(year), data=sim_data))$coefficients["eitc_rate", "Estimate"]
summary(lm(outcome ~ eitc_rate + state + as.factor(year), data=sim_data_all_eligible))$coefficients["eitc_rate", "Estimate"]


proportion_eligible <- seq(0, 1, by = 0.1)
effect_estimate <- vector(length=length(proportion_eligible))
for (i in 1:length(proportion_eligible)) {
	dataset <- simulate_data(list(prob_eitc_eligible = proportion_eligible[i]))
	effect_estimate[i] <- summary(lm(outcome ~ eitc_rate + state + as.factor(year), data=dataset))$coefficients["eitc_rate", "Estimate"]
}
effect_estimate_by_eligiblity <- data.frame (
	eligible = proportion_eligible,
	estimate = effect_estimate
)

ggplot(effect_estimate_by_eligiblity) + aes(x= eligible, y= estimate) + geom_point() + geom_line() + geom_hline(yintercept=-300) + annotate("text", label="true individual effect", x=0.1, y=-280)




############### Allowing for different numbers of people per state ###################

population_structure <- c(
  rep(200, 51)
)



get_global_person_id <- function(state, within_state_id) {
  return(sprintf("%03d%010d", state, within_state_id))
}
get_state_id_from_global_person_id <- function(person_id) {
  return(strtoi(substr(person_id, 1, 3), base=10))
}
get_state_person_id_from_global_person_id <- function(person_id) {
  return(strtoi(substr(person_id, 4, 13), base=10))
}
get_global_person_id(53, 1234)
get_state_id_from_global_person_id(get_global_person_id(53, 1234))
get_state_person_id_from_global_person_id(get_global_person_id(53, 1234))

get_index_for_person <- function(global_person_id, year){
  state_index <- get_state_id_from_global_person_id(global_person_id)
  within_state_id <- get_state_person_id_from_global_person_id(global_person_id)
  return(get_index_for_person_by_state(state_index, within_state_id, year))
}

get_index_for_person_by_state <- function(state_index, within_state_id, year){
  if (year < 2000) { stop("Year 0 is 2000")}
#  if (year > 2019) { stop("Future simulation not supported")}
  year_after_2000 <- year - 2000
  total_people_per_year <- sum(population_structure)
  people_in_prior_states <- 0
  if (state_index > 1) {
    people_in_prior_states <- sum(population_structure[1:(state_index-1)])
  }
  index <- year_after_2000*total_people_per_year + people_in_prior_states + within_state_id
  return(index)
}
get_index_for_person_by_state(1, 2, 2001)
get_index_for_person_by_state(2, 1, 2001)

get_subject_count_for_state <- function(state_index) {
  population_structure[state_index]
}

get_subject_count_for_state(2)



get_index_for_person(get_global_person_id(1, 67), 2001)
get_index_for_person(get_global_person_id(1, 67), 2000)
  
get_state_range_for_year <- function(state_index, year) {
  start <- get_index_for_person_by_state(state_index, 1, year)
  end <- get_index_for_person_by_state(state_index, get_subject_count_for_state(state_index), year)
  return (start:end)
}

get_state_range_for_year(1, 2003)

simulate_data_2 <- function(sim_criteria=list()) {

    
  baseline_outcome_mean <- ifelse("baseline_outcome_mean" %in% names(sim_criteria), sim_criteria$baseline_outcome_mean, 300)
  baseline_outcome_sd <- ifelse("baseline_outcome_sd" %in% names(sim_criteria), sim_criteria$baseline_outcome_sd, 25)
  baseline_year <- ifelse("baseline_year" %in% names(sim_criteria), sim_criteria$baseline_year, 2000)
  n_followup_years <- ifelse("n_followup_years" %in% names(sim_criteria), sim_criteria$n_followup_years, 17)
  state_effect_sd <- ifelse("state_effect_sd" %in% names(sim_criteria), sim_criteria$state_effect_sd, 25)
  n_states <- ifelse("n_states" %in% names(sim_criteria), sim_criteria$n_states, 51)
  year_effect_sd <- ifelse("year_effect_sd" %in% names(sim_criteria), sim_criteria$year_effect_sd, 5)
  per_subject_year_change_sd <- ifelse("per_subject_year_change_sd" %in% names(sim_criteria), sim_criteria$per_subject_year_change_sd, 5)
  prob_eitc_eligible <- ifelse("prob_eitc_eligible" %in% names(sim_criteria), sim_criteria$prob_eitc_eligible, 0.1)
  eitc_effect_per_percent <- ifelse("eitc_effect_per_percent" %in% names(sim_criteria), sim_criteria$eitc_effect_per_percent, -300)
  
  if (baseline_year < 1980) { stop("Baseline year too high")}
#  if (n_followup_years + baseline_year > 2017) { stop("Final year out of EITC range")}
  
  state_effect <- rnorm(n_states, 0, state_effect_sd)
  year_effect <- rnorm(n_followup_years+1, 0, year_effect_sd)
  
  n_total_subjects <- sum(population_structure)
  n_years <- n_followup_years + 1
  
  outcome_vals <- vector(length=n_total_subjects * n_years)
  outcome_underlying_vals <- vector(length=n_total_subjects * n_years)
  eitc_eligible <- vector(length=n_total_subjects * n_years)
  eitc_rate <- vector(length=n_total_subjects * n_years)
  realized_eitc_effect <- vector(length=n_total_subjects * n_years)
  
  for (i in 1:n_states) {
    subjects_in_state <- get_subject_count_for_state(i)
    baseline_outcome <- rnorm(subjects_in_state, baseline_outcome_mean, baseline_outcome_sd)
    baseline_idx <- get_state_range_for_year(i, baseline_year)
    #    baseline_idx <- get_index_for_person_by_state(i, 1, 2000):get_index_for_person_by_state(i, subjects_in_state, 2000)
    eitc_eligible[baseline_idx] <- rbinom(subjects_in_state, 1, prob_eitc_eligible)
    state_rate <- eitc$State.EITC.Rate[eitc$state == i & eitc$year == baseline_year]
    eitc_effect <- eitc_eligible[baseline_idx] * state_rate * eitc_effect_per_percent
    outcome_underlying_vals[baseline_idx] <- baseline_outcome
    outcome_vals[baseline_idx] <- baseline_outcome + state_effect[i] + year_effect[1] + eitc_effect
    for (j in 1:n_followup_years) {
      year <- baseline_year + j
      this_year_idx <- get_state_range_for_year(i, year-1)
      this_year_outcome_underlying <- outcome_underlying_vals[this_year_idx]
      next_year_idx <- get_state_range_for_year(i, year)
      random_change <- rnorm(subjects_in_state, 0, per_subject_year_change_sd)
      outcome_underlying_vals[next_year_idx] <- this_year_outcome_underlying + random_change
      state_rate <- eitc$State.EITC.Rate[eitc$state == i & eitc$year == year]
      # HACK for going into future
      if (length(state_rate) == 0) { 
        if (year < 2018) { stop("unexpected NA in state rate")}
        state_rate <- eitc$State.EITC.Rate[eitc$state == i & eitc$year == 2017]
      }
      eitc_effect <- as.numeric(eitc_eligible[this_year_idx]) * state_rate * eitc_effect_per_percent
      outcome_vals[next_year_idx] <- outcome_underlying_vals[next_year_idx] + state_effect[i] + year_effect[j] + eitc_effect		
      eitc_rate[next_year_idx] <- state_rate
      # For the moment, EITC eligibility does not change over time
      eitc_eligible[next_year_idx] <- eitc_eligible[this_year_idx]
      realized_eitc_effect[next_year_idx] <- eitc_effect
    }
  }
  
  
  within_state_ids <- rep(unlist(lapply(population_structure, function(x) {return(seq(1, x))})), n_years)
  state_ids <- rep(1:n_states, times=population_structure)
  id <- rep(sprintf("%03d%010d", state_ids, within_state_ids))
  state <- rep(state_ids, times=n_years)
  year <- rep(baseline_year:(baseline_year+n_followup_years), each=n_total_subjects)
  true_year_fixed_effect <- rep(year_effect, each=sum(population_structure))
  
  sim_data <- data.frame(
    id = id,
    state = factor(state),
    year = year,
    true_year_fixed_effect = true_year_fixed_effect,
    eitc_eligible = eitc_eligible,
    eitc_rate = eitc_rate,
    realized_eitc_effect = realized_eitc_effect,
    outcome = outcome_vals
  )
  
  return(sim_data)
}
sim_data <- simulate_data_2(list(n_states = 51))

sim_data_all_eligible <- simulate_data_2(list(prob_eitc_eligible = 1))
summary(lm(outcome ~ eitc_rate + state + as.factor(year), data=sim_data_all_eligible))$coefficients["eitc_rate", "Estimate"]

effect_size <- seq(0, -200, by=-20)
result <- vector(length=length(effect_size))
for (i in 1:length(effect_size)) {
  sim_data_all_eligible <- simulate_data_2(list(prob_eitc_eligible = 1, eitc_effect_per_percent = effect_size[i]))
  result[i] <- summary(lm(outcome ~ eitc_rate + state + as.factor(year), data=sim_data_all_eligible))$coefficients["eitc_rate", "Estimate"]
  print(paste("completed simulation ", i))
}
cbind(effect_size, result)
# Looks like it's about 10-20% biased towards the null, compared with the true effect -- was that true of the old simulation
result_old <- vector(length=length(effect_size))
for (i in 1:length(effect_size)) {
  sim_data_all_eligible <- simulate_data(list(prob_eitc_eligible = 1, eitc_effect_per_percent = effect_size[i]))
  result_old[i] <- summary(lm(outcome ~ eitc_rate + state + as.factor(year), data=sim_data_all_eligible))$coefficients["eitc_rate", "Estimate"]
  print(paste("completed simulation ", i))
}
cbind(effect_size, result_old)
# Yes

# Some effect of population size?
population_structure <- c(  rep(2000, 51))

result <- vector(length=length(effect_size))
for (i in 1:length(effect_size)) {
  sim_data_all_eligible <- simulate_data_2(list(prob_eitc_eligible = 1, eitc_effect_per_percent = effect_size[i]))
  result[i] <- summary(lm(outcome ~ eitc_rate + state + as.factor(year), data=sim_data_all_eligible))$coefficients["eitc_rate", "Estimate"]
  print(paste("completed simulation ", i))
}
cbind(effect_size, result)
# No


# Back to a reasonable size, adding household random effect
population_structure <- c(  rep(200, 51))
result <- vector(length=length(effect_size))
for (i in 1:length(effect_size)) {
  sim_data_all_eligible <- simulate_data_2(list(prob_eitc_eligible = 1, eitc_effect_per_percent = effect_size[i]))
  result[i] <- summary(lmer(outcome ~ eitc_rate + state + as.factor(year) + (1 | id), data=sim_data_all_eligible))$coefficients["eitc_rate", "Estimate"]
  print(paste("completed simulation ", i))
}
cbind(effect_size, result)


# That's not it, so let's go back to ignoring the within-person correlation so it'll be fast 
# Let's take go fewer years
result <- vector(length=length(effect_size))
for (i in 1:length(effect_size)) {
  sim_data_all_eligible <- simulate_data_2(list(prob_eitc_eligible = 1, n_followup_years=5, eitc_effect_per_percent = effect_size[i]))
  result[i] <- summary(lm(outcome ~ eitc_rate + state + as.factor(year), data=sim_data_all_eligible))$coefficients["eitc_rate", "Estimate"]
  print(paste("completed simulation ", i))
}
cbind(effect_size, round(result, 2)/5)

# Smaller effect

years <- 1:10
result <- vector(length=length(years))
for (i in 1:length(years)) {
  sim_data_all_eligible <- simulate_data_2(list(prob_eitc_eligible = 1, n_followup_years=years[i], eitc_effect_per_percent = -80))
  result[i] <- summary(lm(outcome ~ eitc_rate + as.factor(state) + as.factor(year), data=sim_data_all_eligible))$coefficients["eitc_rate", "Estimate"]
  print(paste("completed simulation ", i))
}
cbind(years, round(result, 2))

tail(sim_data_all_eligible[sim_data_all_eligible$eitc_rate > 0.1,])

sim_data_all_eligible[sim_data_all_eligible$id == "0470000000195",]
sim_data_all_eligible[sim_data_all_eligible$id == "0470000000196",]
sim_data_all_eligible[sim_data_all_eligible$id == "0470000000197",]
nrow(sim_data_all_eligible)

years <- 1:8
result <- vector(length=length(years))
for (i in 1:length(years)) {
  sim_data_all_eligible <- simulate_data(list(prob_eitc_eligible = 1, n_followup_years=years[i], eitc_effect_per_percent = -20))
  result[i] <- summary(lm(outcome ~ eitc_rate + state + as.factor(year), data=sim_data_all_eligible))$coefficients["eitc_rate", "Estimate"]
  print(paste("completed simulation ", i))
}
cbind(years, round(result, 2))


sim_data_short <- simulate_data_2(list(prob_eitc_eligible = 1, n_followup_years=5, eitc_effect_per_percent = -300))
sim_data_long <- simulate_data_2(list(prob_eitc_eligible = 1, n_followup_years=50, eitc_effect_per_percent = -1000))
summary(lm(outcome ~ eitc_rate + as.factor(state) + as.factor(year), data=sim_data_short))
summary(lm(outcome ~ eitc_rate + as.factor(state) + as.factor(year), data=sim_data_long))
summary(lm(outcome ~ eitc_rate + as.factor(state) + as.factor(year), data=sim_data_long[sim_data_long$year < 2006,]))

three_states <- sim_data_long[sim_data_long$state %in% c(8, 9, 10),]
number_to_name <- c("DE", "DC", "FL")
names(number_to_name) <- 8:10 

ggplot(three_states) + 
  aes(x=year, y=outcome, group=id, color=state, linetype=as.factor(eitc_eligible)) + 
  geom_line() + 
  facet_wrap(state ~ .,labeller = as_labeller(number_to_name)) + 
  scale_color_brewer(palette="Dark2", labels=c("DE", "DC", "FL")) 


unlist(lapply(population_structure, function(x) {return(seq(1, x))}))

sim_data_long <- simulate_data_2(list(prob_eitc_eligible = 1, n_followup_years=15, year_effect_sd=1, eitc_effect_per_percent = 300))

sim_data_long <- simulate_data_2(list(prob_eitc_eligible = 1, n_followup_years=100, year_effect_sd=1, eitc_effect_per_percent = -1000))

years <- 1:40
year_1_fixed_effect <- vector(length=length(years))
eitc_effect <- vector(length=length(years))
for (i in years) {
  last_year <- 2001 + i
  dataset <- subset(sim_data_long, year < last_year)
  model <- lm(outcome ~ eitc_rate + as.factor(state) + as.factor(year), data=dataset)
  year_1_fixed_effect[i] <- summary(model)$coefficients["as.factor(year)2001", "Estimate"]
#  model <- lm(outcome ~ eitc_rate + as.factor(state) + year, data=dataset)
#  year_1_fixed_effect[i] <- summary(model)$coefficients["year", "Estimate"]
  eitc_effect[i] <- summary(model)$coefficients["eitc_rate", "Estimate"]
  print(paste("Year", i))
}

(results <- data.frame(years_of_data=years, 
      estimated_year_1_fixed_effect = year_1_fixed_effect, 
      true_year_1_fixed_effect = sim_data_long$true_year_fixed_effect[1], 
      estimated_eitc_effect = eitc_effect, 
      true_eitc_effect = sim_data_long$realized_eitc_effect[1]))

ggplot(results) + 
  aes(x=years_of_data, y=estimated_eitc_effect) + 
  geom_hline(yintercept=-1000) + 
  geom_point()

ggplot(results) + 
  aes(x=years_of_data, y=estimated_year_1_fixed_effect) + 
  geom_hline(yintercept=results$true_year_1_fixed_effect[1]) + 
  geom_point()

