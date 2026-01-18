# 1. Define the data
x <- c(70, 74, 72, 68, 58, 84, 82, 64, 80, 61)
y <- c(77, 94, 88, 80, 71, 76, 88, 80, 90, 69)

# 2. Generate the Scatter Plot 
plot(x, y, 
     main = "Scatter Plot with Regression Line",
     xlab = "X Variable", 
     ylab = "Y Variable", 
     pch = 19, col = "blue")

# 3. Calculate the Linear Regression Model 
model <- lm(y ~ x)
summary(model)

# Add the regression line to the plot
abline(model, col = "red", lwd = 2)

# Extract coefficients
a <- round(coef(model)[1], 2) # Intercept
b <- round(coef(model)[2], 2) # Slope

cat("Regression Equation: Y =", a, "+", b, "X\n")

# 4. Interpretation
cat("Interpretation: For every 1 unit increase in X, Y increases by", b, "units.\n")

# 5. Predictions (Question 3)
# Predicting for X = 65 (as in your first example)
pred_65 <- a + (b * 65)
cat("Prediction for X = 65: Y =", pred_65, "\n")
