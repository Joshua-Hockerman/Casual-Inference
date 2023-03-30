library(haven)
titanic <- read_dta("titanic.dta")
View(titanic)

# Step one: stratify the data by sex and age
df <- data.frame(titanic)

adult_male <- subset(df, (age == 1) & (sex == 1))
adult_female <- subset(df, (age == 1) & (sex == 0))
child_male <- subset(df, (age == 0) & (sex == 1))
child_female <- subset(df, (age == 0) & (sex == 0))

fcam_survival_rate <- (count(adult_male))