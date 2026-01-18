# --- 1. Define the Data ---
ozone_x <- c(0.02, 0.07, 0.11, 0.15)
yield_y <- c(242, 237, 231, 201)

# --- 2. Fit standard Linear Regression (Parts 1 & 2) ---
model <- lm(yield_y ~ ozone_x)
beta_0 <- coef(model)[1]
beta_1 <- coef(model)[2]

cat("Regression Equation: y =", beta_0, "+ (", beta_1, ") * x\n\n")

# --- 3. 95% Confidence Intervals (Part 3) ---
conf_intervals <- confint(model, level = 0.95)
print("95% Confidence Intervals for Beta0 and Beta1:")
print(conf_intervals)

# --- 4. Hypothesis Testing (Parts 4 & 5) ---
# Check summary for p-values
# Part 4 asks for alpha = 0.1 for Beta1
# Part 5 asks for Beta0
summary_model <- summary(model)
print(summary_model)

# --- 5. Regression Through the Origin (Part 6) ---
# '0 +' removes the intercept from the model
model_origin <- lm(yield_y ~ 0 + ozone_x)
anova_origin <- anova(model_origin)

print("ANOVA Table for Regression Through the Origin:")
print(anova_origin)

# Plotting the data and the standard regression line
plot(ozone_x, yield_y, 
     main="Ozone Exposure vs Soy Bean Yield",
     xlab="Ozone (PPM)", ylab="Yield (gm/plant)",
     pch=16, col="darkgreen")
abline(model, col="red", lwd=2)

