# ------------------------------------------------------------------------------
# perfect_doctor.R
# Complete the assignment
# ------------------------------------------------------------------------------

# load libraries
library(haven)  # Read Stata .dta files
library(fixest) # Runs regressions
library(data.table)

# load data
doctor <- read_dta("doctor.dta")
doctor_df <- data.table(doctor)

doctor_df[, delta := y1 - y0]

# The bad doctor assigns the first 50000 people to vents
# The perfect doctor knows which treatment is most effective
bad_doctor_df <- copy(doctor_df)
perfect_doctor_df <- copy(doctor_df)

bad_doctor_df[, vents := (person <= 50000)]
perfect_doctor_df[, vents := (delta > 0)]

# Calculate all aggregate Causal parameters
bd_ate <- bad_doctor_df[,mean(delta)]
bd_att <- bad_doctor_df[vents == TRUE, mean(delta)]
bd_atu <- bad_doctor_df[vents == FALSE, mean(delta)]

pd_ate <- perfect_doctor_df[,mean(delta)]
pd_att <- perfect_doctor_df[vents == TRUE, mean(delta)]
pd_atu <- perfect_doctor_df[vents == FALSE, mean(delta)]

cat(sprintf("Bad Doctor ATE = %.03f\n", bd_ate))
cat(sprintf("Bad Doctor ATT = %.03f\n", bd_att))
cat(sprintf("Bad Doctor ATU = %.03f\n", bd_atu))

cat(sprintf("Perfect Doctor ATE = %.03f\n", pd_ate))
cat(sprintf("Perfect Doctor ATT = %.03f\n", pd_att))
cat(sprintf("Perfect Doctor ATU = %.03f\n", pd_atu))

# Use the switching equation to select realized outcomes from potential outcomes
# based on treatment assignment
bad_doctor_df[,y := vents * y1 + (1 - vents) * y0]
perfect_doctor_df[,y := vents * y1 + (1- vents) * y0]

# Calculate EY0 for the vent group and no vent group for selection bias calculation
bd_ey01 <- bad_doctor_df[vents == TRUE, mean(y0)]
bd_ey00 <- bad_doctor_df[vents == FALSE, mean(y0)]

pd_ey01 <- perfect_doctor_df[vents == TRUE, mean(y0)]
pd_ey00 <- perfect_doctor_df[vents==FALSE, mean(y0)]

# Now calculate the slection bias for the bad and perfect doctor
bd_selection_bias <- (bd_ey01 - bd_ey00)
pd_selection_bias <- (pd_ey01 - pd_ey00)

cat(sprintf(
  "Bad Doctor Selection Bias: %.03f - %.03f = %.03f\n",
  bd_ey01, bd_ey00, bd_selection_bias
))

cat(sprintf(
  "Perfect Doctor Selection Bias: %.03f - %.03f = %.03f\n",
  pd_ey01, pd_ey00, pd_selection_bias
))

# Now calculate the share of units treated with vents
bd_pi <- mean(bad_doctor_df$vents)
pd_pi <- mean(perfect_doctor_df$vents)

cat(sprintf("Bad Doctor ratio: %.03f \n Good Doctor ratio: %.03f",
            bd_pi, pd_pi))

# Manually calculate the SDO between groups
bd_ey1 <- bad_doctor_df[vents == TRUE, mean(y)]
bd_ey0 <- bad_doctor_df[vents == FALSE, mean(y)]
bd_sdo <- bd_ey1 - bd_ey0
cat(sprintf(
  "Bad Doctor Simple Difference in Outcomes: %.03f - %.03f = %.03f\n",
  bd_ey1, bd_ey0, bd_sdo
))

pd_ey1 <- perfect_doctor_df[vents == TRUE, mean(y)]
pd_ey0 <- perfect_doctor_df[vents == FALSE, mean(y)]
pd_sdo <- pd_ey1 - pd_ey0
cat(sprintf(
  "Perfect Doctor Simple Difference in Outcomes: %.03f - %.03f = %.03f\n",
  pd_ey1, pd_ey0, pd_sdo
))

# Run a fixed-effects OLS for both bad and perfect datasets
bd_reg <- feols(
  y ~ vents, data=bad_doctor_df,
  vcov="hc1"
)
print(bd_reg)

pd_reg <- feols(
  y ~ vents, data=perfect_doctor_df,
  vcov="hc1"
)
print(pd_reg)

# Were we able to estimate the ATE, ATT, or ATU using teh SDO?
bd_sdo_check <- bd_ate + bd_selection_bias + (1 - bd_pi) * (bd_att - bd_atu)
cat(sprintf(
  "Bad Doctor SDO Check: %.03f\n",
  bd_sdo_check
))

pd_sdo_check <- pd_ate + pd_selection_bias + (1 - pd_pi) * (pd_att - pd_atu)
cat(sprintf(
  "Perfect Doctor SDO Check: %.03f\n",
  pd_sdo_check
))



