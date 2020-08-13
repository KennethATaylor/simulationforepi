##### BIG DATA AND PUBLIC HEALTH SIMULATION EXERCISE ######
# CONTACT: Steve Mooney, smooney27@gmail.com
###########################################################

# Uncomment this line if you do not have the epitools package installed
# install.packages("epitools", dependencies = T)
library(epitools)
# install.packages("dplyr", dependencies = T)
library(dplyr)

# Simple simulation
simulate_population <- function(population_size, exposed_fraction, unexposed_risk, relative_risk) {
  if (population_size <= 0) {
    stop("simulate_population: population_size must be greater than zero")
  }
  if (exposed_fraction < 0 | exposed_fraction > 1) {
    stop("simulate_population: exposed_fraction not between zero and one")
  }
  if (relative_risk <= 0) {
    stop("simulate_population: relative_risk must be greater than zero")
  }
  unexposed_count <- floor(population_size * (1 - exposed_fraction))
  exposed_count <- ceiling(population_size * exposed_fraction)
  diseased_unexposed_count <- floor(unexposed_count * unexposed_risk)
  nondiseased_unexposed_count <- unexposed_count - diseased_unexposed_count
  diseased_exposed_count <- floor(exposed_count * unexposed_risk * relative_risk)
  nondiseased_exposed_count <- exposed_count - diseased_exposed_count
  exposure <- c(rep(F, unexposed_count), rep(T, exposed_count))
  disease <- c(rep(F, nondiseased_unexposed_count), rep(T, diseased_unexposed_count),
               rep(F, nondiseased_exposed_count), rep(T, diseased_exposed_count))
  return(data.frame(id=1:population_size, exposure, disease))  
}

ten_people <- simulate_population(10, .5, .2, 2)

# Make sure this simulates us a population with the characteristics we expect
ten_people

# relative risk == 2
table(ten_people$exposure, ten_people$disease)
riskratio.wald(table(ten_people$exposure, ten_people$disease))$measure

###### Part 1: How much does a bigger data set add precision?
million_people_population <- simulate_population(1000000, .5, .2, 2)

# So of course the estimates get closer to the truth (and the confidence intervals 
# get tighter) as we sample more people
sample_1000 <- sample_n(million_people_population, 1000)
riskratio.wald(table(sample_1000$exposure, sample_1000$disease))$measure
sample_1000 <- sample_n(million_people_population, 1000)
riskratio.wald(table(sample_1000$exposure, sample_1000$disease))$measure

# More precison with 10000 people
sample_10000 <- sample_n(million_people_population, 10000)
riskratio.wald(table(sample_10000$exposure, sample_10000$disease))$measure
sample_10000 <- sample_n(million_people_population, 10000)
riskratio.wald(table(sample_10000$exposure, sample_10000$disease))$measure

# And even more with 100000
sample_100000 <- sample_n(million_people_population, 100000)
riskratio.wald(table(sample_100000$exposure, sample_100000$disease))$measure
sample_100000 <- sample_n(million_people_population, 100000)
riskratio.wald(table(sample_100000$exposure, sample_100000$disease))$measure

######################################################################
# Selection bias
######################################################################
# now consider what happens when sample from a selected subset
# (Because we created the dataset in order, the first 50000 are 
# all non-diseased/non-exposed people)

# Within this selected subset, the true risk ratio is 1.8
selected_subset <- million_people_population[50001:1000000,]
riskratio.wald(table(selected_subset$exposure, selected_subset$disease))$measure

# The smaller sample includeds the truth in its confidence interval where as the larger samples do not
set.seed(1234)
selected_sample_1000 <- sample_n(selected_subset, 1000)
selected_sample_10000 <- sample_n(selected_subset, 10000)
selected_sample_100000 <- sample_n(selected_subset, 100000)
riskratio.wald(table(selected_sample_1000$exposure, selected_sample_1000$disease))$measure
riskratio.wald(table(selected_sample_10000$exposure, selected_sample_10000$disease))$measure
riskratio.wald(table(selected_sample_100000$exposure, selected_sample_100000$disease))$measure
# Note that this is two sources of error (selection bias and sampling error) that happen to work together
# to confirm the need for caution -- not an improvement on the result!


###############################################################################
# Non-differential misclassification
###############################################################################
# Suppose we randomly misclassify 5% of the population
misclassify_5_percent_of_exposure <- million_people_population
to_misclassify <- sample(1:1000000, 50000, replace = F)
misclassify_5_percent_of_exposure$exposure[to_misclassify] <- !misclassify_5_percent_of_exposure$exposure[to_misclassify]
riskratio.wald(table(misclassify_5_percent_of_exposure$exposure, misclassify_5_percent_of_exposure$disease))$measure

# Now suppose we randomly misclassify 5% of disease
misclassify_5_percent_of_disease <- million_people_population
to_misclassify <- sample(1:1000000, 50000, replace = F)
misclassify_5_percent_of_disease$disease[to_misclassify] <- !misclassify_5_percent_of_disease$disease[to_misclassify]
riskratio.wald(table(misclassify_5_percent_of_disease$exposure, misclassify_5_percent_of_disease$disease))$measure

# Now suppose exposure is rarer -- 10% of population
ten_percent_exposed_population <- simulate_population(1000000, .1, .2, 2)
riskratio.wald(table(ten_percent_exposed_population$exposure, ten_percent_exposed_population$disease))$measure

misclassify_5_percent_of_rare_exposure <- ten_percent_exposed_population
to_misclassify <- sample(1:1000000, 50000, replace = F)
misclassify_5_percent_of_rare_exposure$exposure[to_misclassify] <- !misclassify_5_percent_of_rare_exposure$exposure[to_misclassify]
riskratio.wald(table(misclassify_5_percent_of_rare_exposure$exposure, misclassify_5_percent_of_rare_exposure$disease))$measure

# The more common scenario is probably mostly one way (e.g. failing to identify exposure)
missed_5_percent_of_rare_exposure <- ten_percent_exposed_population
to_misclassify <- sample(which(ten_percent_exposed_population$exposure), 5000, replace = F)
missed_5_percent_of_rare_exposure$exposure[to_misclassify] <- F
riskratio.wald(table(missed_5_percent_of_rare_exposure$exposure, missed_5_percent_of_rare_exposure$disease))$measure

###############################################################################
# Differential misclassification
###############################################################################
# Suppose 5% of the exposed/diseased had their exposure misclassified, as did 1% of all other categories
table(ten_percent_exposed_population$exposure, ten_percent_exposed_population$disease)
differential_misclassification <- ten_percent_exposed_population
exposed_diseased_to_misclassify <- sample(which(ten_percent_exposed_population$exposure & 
                                        ten_percent_exposed_population$disease), 1600, replace = F)
exposed_nondiseased_to_misclassify <- sample(which(ten_percent_exposed_population$exposure & 
                                          !ten_percent_exposed_population$disease), 600, replace = F)
nonexposed_diseased_to_misclassify <- sample(which(!ten_percent_exposed_population$exposure & 
                                             ten_percent_exposed_population$disease), 1800, replace = F)
nonexposed_nondiseased_to_misclassify <- sample(which(!ten_percent_exposed_population$exposure & 
                                                        !ten_percent_exposed_population$disease), 7200, replace = F)
differential_misclassification$exposure[exposed_diseased_to_misclassify] <- F
differential_misclassification$exposure[exposed_nondiseased_to_misclassify] <- F
differential_misclassification$exposure[nonexposed_nondiseased_to_misclassify] <- T
differential_misclassification$exposure[nonexposed_diseased_to_misclassify] <- T
table(differential_misclassification$exposure, differential_misclassification$disease)
riskratio.wald(table(differential_misclassification$exposure, differential_misclassification$disease))$measure


###############################################################################
# Validation study
###############################################################################

differential_misclassification

length(which(ten_percent_exposed_population$exposure & ten_percent_exposed_population$disease))
