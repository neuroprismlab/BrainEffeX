# UKB fMRI Atlas Data

# read in csv data file
# amplitude, trait
# trait chosen here is depression sum score (trait 213)
ukb = read.csv('~/Library/CloudStorage/GoogleDrive-halleeninet@gmail.com/My Drive/NeuroPRISM/Effect Size Calculator/Data_HS/amplitude.trait213.csv')

# identify whether data is from rest or task
condition = "REST"

# identify sample size based on condition:
if (condition == "REST") {
    n = 33795
} else if (condition == "TASK") {
    n = 28907
}

# create a new column, named d, that holds cohen's d
# using the formula for one-sample t statistic to sample cohen's d_s estimation:
# d(sample) = t/sqrt(n)

ukb$d <- ukb$T / sqrt(n)

# check the results visually - histogram
hist(ukb$d)