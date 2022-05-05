#### Load required packages ####

library(foreign)
library(tidyr)
library(dplyr)
library(data.table)

#### Create a database with just the necessary variables ####

# 1.1 Load original dataset

mydata <- read.spss("Total Dataset.sav",
					use.value.labels = FALSE,
					to.data.frame = TRUE)

# 1.2 Select only variables of interest

dat_subset_1 <- select(mydata, id, day, psp1, psp2, psp3,
					   psp.mean, ssa1, ssa2, ssa3,
					   ssa4, ssa5, ssa6, ssa7, ssa.mean)

# 1.3 Filter for days 7 - 11 only

dat_subset_2 <- subset(dat_subset_1, day>=7 & day<=11)

# 1.4 Convert to fully long format

dat_long_1 <- gather(dat_subset_2, key = var, value = score, psp1:ssa.mean)

# 1.5 Combine day and var into a new variable

dat_long_1$var.c <- paste(dat_long_1$var, dat_long_1$day)

dat_long_2 <- select(dat_long_1, id, var.c, score)

View(dat_long_2)

# 1.6 Convert to wide format

dat_wide <- spread(dat_long_2, var.c, score)

# 1.7 Correct variable names

name.orig <- colnames(dat_wide)

name.new <- make.names(name.orig, unique = TRUE)

setnames(dat_wide, old = name.orig, new = name.new)

# 1.8 This data set now contains all variables of interest in wide format.

View(dat_wide)

# 1.9 Convert back to long format, with variables of item and score.

# This format is similar, but not identical, to dat_long_2 above.

dat_long <- gather(dat_wide, item, score, psp.mean.10:ssa7.9, factor_key=TRUE)

View (dat_long)

write.csv(dat_wide, file = "Abridged Comp Data.csv")

