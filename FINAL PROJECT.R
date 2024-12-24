install.packages("readxl")
library(readxl)

# Input the data
data <- read_excel("C:/Users/DELL/Downloads/The Effect of Learning Styles and Practice Duration on Skill Improvement.xlsx")

# Ensure df is a data frame and view the first few rows
df <- read_excel("C:/Users/DELL/Downloads/The Effect of Learning Styles and Practice Duration on Skill Improvement.xlsx")
str(df)  # Display the structure of the data
head(df)  # Display the first 6 rows of the data

# Run an ANOVA model (adjust for column names)
model <- aov(`Skill_Improvement (%)` ~ Learning_Style, data = df)

# Summarize the model results
summary(model)

# Normality of residuals: Shapiro-Wilk test
shapiro.test(residuals(model))

# Levene's Test for Homogeneity of Variance (from the car package)
library(car)
leveneTest(`Skill_Improvement (%)` ~ Learning_Style, data = df)

# Check Independence of Observations
mod <- lm(`Skill_Improvement (%)` ~ `Practice_Hours/Week` + Learning_Style, data = df)

# View the summary of the model
summary(mod)

# Plot Residuals vs Fitted Values
plot(mod$residuals, mod$fitted.values, main="Residuals vs Fitted Values")
abline(h=0, col="red")

# Durbin-Watson Test for autocorrelation
install.packages("lmtest")
library(lmtest)
dwtest(mod)

# Plot ACF for residuals
acf(mod$residuals, main="Autocorrelation of Residuals")

# Scatter plot between Practice_Hours/Week and Skill_Improvement (%)
ggplot(df, aes(x = `Practice_Hours/Week`, y = `Skill_Improvement (%)`)) +
  geom_point(aes(color = Learning_Style), size = 3) +
  labs(title = "Scatter Plot: Practice Hours vs Skill Improvement",
       x = "Practice Hours per Week",
       y = "Skill Improvement (%)") +
  theme_minimal() +
  scale_color_manual(values = c("Visual" = "blue", "Auditory" = "red", "Kinesthetic" = "green"))

# Boxplot for Skill Improvement by Learning Style
ggplot(df, aes(x = Learning_Style, y = `Skill_Improvement (%)`, fill = Learning_Style)) +
  geom_boxplot() +
  labs(title = "Boxplot: Skill Improvement by Learning Style",
       x = "Learning Style",
       y = "Skill Improvement (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("Visual" = "blue", "Auditory" = "red", "Kinesthetic" = "green"))


