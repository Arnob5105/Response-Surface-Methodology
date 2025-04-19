rm(list=ls())

# Install and load required packages
if (!require("rsm")) install.packages("rsm")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("plotly")) install.packages("plotly")
library(rsm)
library(ggplot2)
library(plotly)

# Sample data structure (replace with your own data)
# Create a data frame with your experimental design and results
# This example follows the Central Composite Design (CCD) from the paper

setwd("C:/Users/ARNOB/Downloads/RSM/BB")

# Example data structure:
my_data <- read.csv('BB.csv', encoding = "latin1",header = TRUE)

# Encode the data for RSM analysis# Encode the data for RSM analysis# Encode the data for RSM analysis
coded_data <- coded.data(my_data, 
                         x1 ~ (pH - 7)/2,           # Coding for pH
                         x2 ~ (Temperature - 35)/10, # Coding for Temperature
                         x3 ~ (Concentration - 100)/50) # Coding for Concentration

# Fit the second-order (quadratic) model
rsm_model <- rsm(Decolorization ~ SO(x1, x2, x3), data = coded_data)

# Display the summary of the model
summary(rsm_model)

# polish model
#rsm_model <- rsm(Decolorization ~ FO(x1, x2, x3) + PQ(x1, x2, x3), data = coded_data)

# Predict values using the model
my_data$Predicted <- predict(rsm_model)

# View predicted values
print(my_data$Predicted)


# ANOVA analysis
anova(rsm_model)

summary(anova(rsm_model))



# Calculate R-squared values
rsquared <- summary(rsm_model)$r.squared
adj_rsquared <- summary(rsm_model)$adj.r.squared
cat("R-squared:", rsquared, "\n")
cat("Adjusted R-squared:", adj_rsquared, "\n")


# error model
# With this code:
# First, create a factor for the unique design points
coded_data$design_point <- factor(paste(coded_data$x1, coded_data$x2, coded_data$x3))

# Then fit a model with the design points as a random effect
pure_error_model <- lm(Decolorization ~ SO(x1, x2, x3) + design_point, data = coded_data)

# Now perform the lack of fit test
anova(pure_error_model, rsm_model)



# Alternative approach to find optimal conditions
# Create a grid of points
grid_points <- expand.grid(
  x1 = seq(-2, 2, length.out = 20),
  x2 = seq(-2, 2, length.out = 20),
  x3 = seq(-2, 2, length.out = 20)
)

# Predict response at each point
predictions <- predict(rsm_model, newdata = grid_points)

# Find the point with maximum response
best_index <- which.max(predictions)
optimal_coded <- grid_points[best_index, ]
print(optimal_coded)

# Convert to actual values
optimal_actual <- data.frame(
  pH = optimal_coded$x1 * 2 + 7,
  Temperature = optimal_coded$x2 * 10 + 35,
  Concentration = optimal_coded$x3 * 75 + 175,
  Predicted_Decolorization = predictions[best_index]
)
print(optimal_actual)



# Calculate residuals (observed - predicted values)
residuals <- rsm_model$residuals

# Calculate Signal (mean response)
signal <- mean(my_data$Decolorization)

# Calculate Noise (standard deviation of residuals)
noise <- sd(residuals)

# Calculate SNR
snr <- signal / noise
print(snr)





# 
# # Generate response surface plots
# # 1. Contour plots for pH vs Temperature (at mean Concentration)
# par(mfrow = c(1, 1))
# contour(rsm_model, ~ x1 + x2, at = list(x3 = 0), 
#         main = "Contour Plot: pH vs Temperature")
# 
# # 2. Contour plots for pH vs Concentration (at mean Temperature)
# contour(rsm_model, ~ x1 + x3, at = list(x2 = 0), 
#         main = "Contour Plot: pH vs Concentration")
# 
# # 3. Contour plots for Temperature vs Concentration (at mean pH)
# contour(rsm_model, ~ x2 + x3, at = list(x1 = 0), 
#         main = "Contour Plot: Temperature vs Concentration")
# 
# # 3D Surface plots using plotly
# # Convert coded values back to original scale for better interpretation
# x1_seq <- seq(-1.682, 1.682, length = 30)
# x2_seq <- seq(-1.682, 1.682, length = 30)
# x3_seq <- seq(-1.682, 1.682, length = 30)
# 
# # pH vs Temperature (at mean Concentration)
# grid_x1_x2 <- expand.grid(x1 = x1_seq, x2 = x2_seq, x3 = 0)
# pred_x1_x2 <- predict(rsm_model, newdata = grid_x1_x2)
# z_matrix_x1_x2 <- matrix(pred_x1_x2, nrow = length(x1_seq), ncol = length(x2_seq))
# 
# # Convert coded values to actual values for axis labels
# pH_seq <- x1_seq * 2 + 7
# temp_seq <- x2_seq * 10 + 35
# 
# # Create 3D surface plot
# plot_ly(x = pH_seq, y = temp_seq, z = z_matrix_x1_x2) %>%
#   add_surface() %>%
#   layout(
#     title = "3D Response Surface: pH vs Temperature",
#     scene = list(
#       xaxis = list(title = "pH"),
#       yaxis = list(title = "Temperature (°C)"),
#       zaxis = list(title = "Decolorization (%)")
#     )
#   )

# # Similar code can be used for other variable combinations (pH vs Concentration, Temperature vs Concentration)
# 
# # Predict decolorization at specific conditions
# new_conditions <- expand.grid(
#   x1 = c(-1, 0, 1),  # Coded pH values
#   x2 = c(-1, 0, 1),  # Coded Temperature values
#   x3 = c(-1, 0, 1)   # Coded Concentration values
# )
# predicted_values <- predict(rsm_model, newdata = new_conditions, se.fit = TRUE)
# new_conditions$predicted <- predicted_values$fit
# new_conditions$se <- predicted_values$se.fit
# 
# # Convert coded values to actual values for better interpretation
# new_conditions$actual_pH <- new_conditions$x1 * 2 + 7
# new_conditions$actual_Temperature <- new_conditions$x2 * 10 + 35
# new_conditions$actual_Concentration <- new_conditions$x3 * 75 + 175
# 
# print(new_conditions[, c("actual_pH", "actual_Temperature", "actual_Concentration", "predicted", "se")])
# 
# # Find conditions for maximum decolorization
# max_point <- predict(rsm_model, newdata = stationary_point$xs, se.fit = TRUE)
# cat("Maximum predicted decolorization:", max_point$fit, "±", max_point$se.fit, "\n")
# cat("at coded conditions:", stationary_point$xs, "\n")
# 
# # Convert to actual values
# actual_optimal_pH <- stationary_point$xs[1] * 2 + 7
# actual_optimal_Temperature <- stationary_point$xs[2] * 10 + 35
# actual_optimal_Concentration <- stationary_point$xs[3] * 75 + 175
# 
# cat("Optimal conditions:\n")
# cat("pH:", actual_optimal_pH, "\n")
# cat("Temperature:", actual_optimal_Temperature, "°C\n")
# cat("Concentration:", actual_optimal_Concentration, "mg/L\n")




# # Install and load required packages
# if (!require("rsm")) install.packages("rsm")
# if (!require("ggplot2")) install.packages("ggplot2")
# if (!require("plotly")) install.packages("plotly")
# if (!require("viridis")) install.packages("viridis")  # For better color palettes
# library(rsm)
 library(ggplot2)
 library(plotly)
 library(viridis)

# Assuming you've already fit your RSM model as shown in previous code
# rsm_model <- rsm(Decolorization ~ SO(x1, x2, x3), data = coded_data)

# Create high-quality 2D contour plots and 3D surface plots similar to the paper
# -----------------------------------------------------------------------

# Create a fine grid for smooth plots
grid_size <- 100
x1_range <- seq(-2, 2, length.out = grid_size)  # Coded pH
x2_range <- seq(-2, 2, length.out = grid_size)  # Coded Temperature
x3_range <- seq(-2, 2, length.out = grid_size)  # Coded Concentration

# 1. pH vs Temperature (at mean Concentration)
# -----------------------------------------------------------------------
# Create grid for prediction
grid_x1_x2 <- expand.grid(x1 = x1_range, x2 = x2_range, x3 = 0)
pred_x1_x2 <- predict(rsm_model, newdata = grid_x1_x2)

# Convert to matrix for contour and surface plots
z_matrix_x1_x2 <- matrix(pred_x1_x2, nrow = length(x1_range), ncol = length(x2_range))

# Convert coded values to actual values for axis labels
pH_range <- x1_range * 2 + 7        # Assuming coding: x1 = (pH - 7)/2
temp_range <- x2_range * 10 + 35    # Assuming coding: x2 = (temp - 35)/10

# 2D Contour plot with ggplot2 (similar to paper)
df_x1_x2 <- data.frame(
  pH = rep(pH_range, each = length(temp_range)),
  Temperature = rep(temp_range, times = length(pH_range)),
  Decolorization = as.vector(z_matrix_x1_x2)
)

p1 <- ggplot(df_x1_x2, aes(x = pH, y = Temperature, z = Decolorization)) +
  geom_contour_filled(bins = 15) +
  scale_fill_viridis_d(option = "D", name = "Decolorization (%)") +
  geom_contour(color = "black", alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Contour Plot: pH vs Temperature",
    x = "pH",
    y = "Temperature (°C)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )
p1
# Save the plot
#ggsave("C:/Users/ARNOB/Downloads/pH_vs_Temperature_contour.png", p1, width = 8, height = 6, dpi = 300)

# 3D Surface plot with plotly (similar to paper)
fig1 <- plot_ly(
  x = pH_range, 
  y = temp_range, 
  z = z_matrix_x1_x2,
  type = "surface",
  colorscale = "Viridis"
) %>%
  layout(
    title = "3D Response Surface: pH vs Temperature",
    scene = list(
      xaxis = list(title = "pH"),
      yaxis = list(title = "Temperature (°C)"),
      zaxis = list(title = "Decolorization (%)"),
      camera = list(eye = list(x = 1.5, y = 1.5, z = 1.2))
    )
  )
fig1
# Save the 3D plot
htmlwidgets::saveWidget(fig1, "C:/Users/ARNOB/Downloads/pH_vs_Temperature_3D.html")

# 2. pH vs Concentration (at mean Temperature)
# -----------------------------------------------------------------------
# Create grid for prediction
grid_x1_x3 <- expand.grid(x1 = x1_range, x2 = 0, x3 = x3_range)
pred_x1_x3 <- predict(rsm_model, newdata = grid_x1_x3)

# Convert to matrix for contour and surface plots
z_matrix_x1_x3 <- matrix(pred_x1_x3, nrow = length(x1_range), ncol = length(x3_range))

# Convert coded values to actual values for axis labels
conc_range <- x3_range * 75 + 175   # Assuming coding: x3 = (conc - 175)/75

# 2D Contour plot with ggplot2
df_x1_x3 <- data.frame(
  pH = rep(pH_range, each = length(conc_range)),
  Concentration = rep(conc_range, times = length(pH_range)),
  Decolorization = as.vector(z_matrix_x1_x3)
)

p2 <- ggplot(df_x1_x3, aes(x = pH, y = Concentration, z = Decolorization)) +
  geom_contour_filled(bins = 15) +
  scale_fill_viridis_d(option = "D", name = "Decolorization (%)") +
  geom_contour(color = "black", alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Contour Plot: pH vs Concentration",
    x = "pH",
    y = "Concentration (mg/L)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

p2
# Save the plot
#ggsave("C:/Users/ARNOB/Downloads/pH_vs_Concentration_contour.png", p2, width = 8, height = 6, dpi = 300)

# 3D Surface plot with plotly
fig2 <- plot_ly(
  x = pH_range, 
  y = conc_range, 
  z = z_matrix_x1_x3,
  type = "surface",
  colorscale = "Viridis"
) %>%
  layout(
    title = "3D Response Surface: pH vs Concentration",
    scene = list(
      xaxis = list(title = "pH"),
      yaxis = list(title = "Concentration (mg/L)"),
      zaxis = list(title = "Decolorization (%)"),
      camera = list(eye = list(x = 1.5, y = 1.5, z = 1.2))
    )
  )

# Save the 3D plot
htmlwidgets::saveWidget(fig2, "C:/Users/ARNOB/Downloads/pH_vs_Concentration_3D.html")

# 3. Temperature vs Concentration (at mean pH)
# -----------------------------------------------------------------------
# Create grid for prediction
grid_x2_x3 <- expand.grid(x1 = 0, x2 = x2_range, x3 = x3_range)
pred_x2_x3 <- predict(rsm_model, newdata = grid_x2_x3)

# Convert to matrix for contour and surface plots
z_matrix_x2_x3 <- matrix(pred_x2_x3, nrow = length(x2_range), ncol = length(x3_range))

# 2D Contour plot with ggplot2
df_x2_x3 <- data.frame(
  Temperature = rep(temp_range, each = length(conc_range)),
  Concentration = rep(conc_range, times = length(temp_range)),
  Decolorization = as.vector(z_matrix_x2_x3)
)

p3 <- ggplot(df_x2_x3, aes(x = Temperature, y = Concentration, z = Decolorization)) +
  geom_contour_filled(bins = 15) +
  scale_fill_viridis_d(option = "D", name = "Decolorization (%)") +
  geom_contour(color = "black", alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Contour Plot: Temperature vs Concentration",
    x = "Temperature (°C)",
    y = "Concentration (mg/L)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )
p3
# Save the plot
#ggsave("Temperature_vs_Concentration_contour.png", p3, width = 8, height = 6, dpi = 300)

# 3D Surface plot with plotly
fig3 <- plot_ly(
  x = temp_range, 
  y = conc_range, 
  z = z_matrix_x2_x3,
  type = "surface",
  colorscale = "Viridis"
) %>%
  layout(
    title = "3D Response Surface: Temperature vs Concentration",
    scene = list(
      xaxis = list(title = "Temperature (°C)"),
      yaxis = list(title = "Concentration (mg/L)"),
      zaxis = list(title = "Decolorization (%)"),
      camera = list(eye = list(x = 1.5, y = 1.5, z = 1.2))
    )
  )

# Save the 3D plot
htmlwidgets::saveWidget(fig3, "C:/Users/ARNOB/Downloads/Temperature_vs_Concentration_3D.html")

# Create a combined figure with all three contour plots (similar to paper layout)
combined_plot <- gridExtra::grid.arrange(p1, p2, p3, ncol = 3)
ggsave("C:/Users/ARNOB/Downloads/Combined_Contour_Plots.png", combined_plot, width = 18, height = 6, dpi = 300)

# Print information about saved files
cat("Plots have been saved to the following files:\n")
cat("- pH_vs_Temperature_contour.png\n")
cat("- pH_vs_Temperature_3D.html\n")
cat("- pH_vs_Concentration_contour.png\n")
cat("- pH_vs_Concentration_3D.html\n")
cat("- Temperature_vs_Concentration_contour.png\n")
cat("- Temperature_vs_Concentration_3D.html\n")
cat("- Combined_Contour_Plots.png\n")


# Code to combine the three 3D plots into a single figure
# This requires the plotly and htmlwidgets packages

# Make sure you've already created the individual 3D plots (fig1, fig2, fig3)
# from the previous code I provided

# Create a subplot with all three 3D plots
# combined_3d_plot <- plotly::subplot(
#   fig1, fig2, fig3,
#   nrows = 1,
#   margin = 0.05,
#   shareX = FALSE,
#   shareY = FALSE
# ) %>%
#   layout(
#     title = "Combined 3D Response Surface Plots",
#     scene = list(domain = list(x = c(0, 0.33), y = c(0, 1))),
#     scene2 = list(domain = list(x = c(0.33, 0.66), y = c(0, 1))),
#     scene3 = list(domain = list(x = c(0.66, 1), y = c(0, 1)))
#   )
# 
# # Save the combined 3D plot
# htmlwidgets::saveWidget(combined_3d_plot, "C:/Users/ARNOB/Downloads/Combined_3D_Plots.html", selfcontained = TRUE)
# 
# # Print information about saved file
# cat("Combined 3D plot has been saved to: Combined_3D_Plots.html\n")
