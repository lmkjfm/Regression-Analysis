# --- 1. Define the Data ---
bill <- c(98.84, 33.46, 63.66, 50.68, 107.34)
tip <- c(14.56, 8.77, 9.81, 8.91, 17.33)

# --- i) Draw a scatter plot ---
plot(bill, tip, 
     main="Scatter Plot of Bill vs Tip", 
     xlab="Bill Amount ($)", 
     ylab="Tip Amount ($)", 
     pch=19, col="blue")
# Add the regression line to the plot
abline(lm(tip ~ bill), col="red", lwd=2)

# --- ii) Fit the simple linear regression model ---
model <- lm(tip ~ bill)
summary(model) # This displays the coefficients, p-values, and R-squared

# --- iii) Predict the tip when Bill = 60 ---
new_data <- data.frame(bill = 60)
prediction <- predict(model, newdata = new_data)
cat("Predicted tip for a $60 bill:", prediction, "\n")

# --- iv) Find Pearson correlation coefficient ---
correlation <- cor(bill, tip)
cat("Pearson Correlation Coefficient:", correlation, "\n")

# --- v) Hypothesis tests and Confidence Intervals ---
# The p-values are shown in the summary(model) output above.
# To get specific 95% confidence intervals:
conf_intervals <- confint(model, level = 0.95)
print("95% Confidence Intervals:")
print(conf_intervals)
