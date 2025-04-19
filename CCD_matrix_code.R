# Load package
library(rsm)

# Define variable names and levels for coding
# pH: center = 7, range = ±2
# Temp: center = 35, range = ±10
# Conc: center = 100, range = ±50

# Create a CCD with 3 variables
ccd_base <- ccd(
  ~ x1 + x2 + x3,
  n0 = c(6, 0),  # 6 center points, 4 axial points
  alpha = "rotatable",
  randomize = FALSE
)

# Decode the variables into actual levels
ccd_actual <- data.frame(
  Run = 1:nrow(ccd_base),
  pH   = ccd_base$x1 * 2 + 7,
  Temp = ccd_base$x2 * 10 + 35,
  Conc = ccd_base$x3 * 75 + 175
)

# Show the 20-run design matrix
print(ccd_actual)
