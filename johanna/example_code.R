# Example code
# Load packages
library(tidyverse)
library(tableone)
library(readr)
library(stringr)

# Source the functions in functions.R
source("johanna/functions.R")

# 1. Import the data
#
# Read the dataset and codebook from github
data <- import_csv_from_github("https://github.com/titco/titco-I/blob/master/titco-I-full-dataset-v1.csv")
# data <- import_csv_from_github("https://github.com/titco/titco-I/blob/master/titco-I-limited-dataset-v1.csv")
codebook <- import_csv_from_github("https://github.com/titco/titco-I/blob/master/codebook-titco-I-v1.csv")

# 2. Do data-cleaning and create variables needed for analysis. Handling missing would also be added here.
#
# There are some variables that are a bit problematic to handle in the TITCO dataset, a few of them are updated here to show different
# approaches on how to handle them.
# age, >89 is converted to numbers only to be able to handle it as numeric. That is age 89 contain 89 and above.
# spo2_o2_1 and spo2_o2_2 is in the codebook defined as quantitative variables, but in the data it's recorded as categorical Yes/no.

# Set the variable spo2_o2_1 and spo2_o2_2 is updated in the codebook to be handled as qualitative.
codebook <- codebook %>% rows_update(tibble(name = "spo2_o2_1", type = "qualitative")) %>%
  rows_update(tibble(name = "spo2_o2_2", type = "qualitative"))

# Updates to the dataset
data <- data %>%
  # Convert age to numeric
  mutate(age = parse_number(age)) %>%
  # Add id column to the dataset
  mutate(id = row_number()) %>%
  # Add our outcome, if patient has a recorded length in the ICU then treated_icu = 1
  mutate(treated_icu = case_when(licu > 24 ~ 1,
                                        licu < 25 ~ 0))

# 3. Set seed and split into training and test samples.
# The seed allows us to always get the same randomized sample when we run the code.
set.seed(42)
# Use 60% of the data for training
train_data <- data %>% sample_frac(.60)
# Use the remaining 40% as test data
test_data <- anti_join(data, train_data, by = 'id')
# Optional, Verify that no rows overlap, should return 0
count(train_data %>% filter(id %in% test_data$id))

# 4. Compare variables between ICU and non-ICU patients
# Define the categorical and continuous variables
cat_variables <- codebook %>% filter(type %in% "qualitative") %>%
  # We filter out pid, not to include that in our analysis
  filter(name != "pid") %>% pull(name)
cont_variables <- codebook %>% filter(type %in% "quantitative") %>%
  # We filter out licu since this is our outcome
  filter(!name %in% c("licu")) %>% pull(name)

# Look at the data using table one
tableone::CreateContTable(data = train_data, vars = cont_variables, strata = "treated_icu")
tableone::CreateCatTable(data = train_data, vars = cat_variables, strata = "treated_icu")

