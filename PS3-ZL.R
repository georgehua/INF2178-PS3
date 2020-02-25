library(tidyverse)
library(skimr)
library(splitstackshape)

data <- read_csv("./survey_results_public.csv")

data <- data %>%
  mutate(id = row_number())

#narrow down to professional developers
#remove n.a. in ConvertedComp
cleaned_data <- data %>%
  filter(
    Country == "United States",
    Employment == "Employed full-time",
    MainBranch == "I am a developer by profession",
    CurrencySymbol == "USD",
    !is.na(ConvertedComp)
  )

#convert YearsCode to numeric
cleaned_data$YearsCode <- as.numeric(cleaned_data$YearsCode)

#n.a. will occur after conversion, so remove the n.a.
cleaned_data <- cleaned_data %>%
  filter(
    !is.na(YearsCode)
  )

cleaned_data$language_count <- sapply(cleaned_data$LanguageWorkedWith,
                                    function(x) lengths(strsplit(x, split = ";")))

#use skim to see check the stats of a column 
#and decide whether to use it as IV
skim(cleaned_data$WorkLoc)

model_1 <- lm(ConvertedComp ~ CodeRev, data = cleaned_data)
broom::glance(model_1)
broom::tidy(model_1)


model_exp <- lm(ConvertedComp ~ YearsCode, data = cleaned_data)
broom::glance(model_exp)

model_job <- lm(ConvertedComp ~ CareerSat + JobSat + WorkLoc + WorkWeekHrs, data = cleaned_data)
broom::glance(model_job)
broom::tidy(model_job)

model_overall <- lm(ConvertedComp ~ YearsCode + CareerSat + JobSat + WorkLoc + WorkWeekHrs, data = cleaned_data)
broom::glance(model_overall)
view(model_overall)
