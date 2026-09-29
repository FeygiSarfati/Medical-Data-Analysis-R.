# Question 1
df <- read.csv("diabetes.csv")

# Question 2
summary(df)
mean(df$BMI)  ## we use the $ sign to select a specific column from the table

# Question 3
install.packages("dplyr") ## Installing a library
library(dplyr)

df %>%
  group_by(Outcome) %>%
  summarise(
    min_BMI = min(BMI),
    max_BMI = max(BMI),
    mean_BMI = mean(BMI),
    median_BMI = median(BMI)
  )

# Question 4
df <- df %>%  ## Ensures the original table loaded in memory is updated
  mutate(Age_Group = ifelse(Age < 50, "Under 50", "50+"))
table(df$Age_Group) ## Verification that the new row was created.

# Question 5
plot(x = df$Glucose, y = df$BMI,
     xlab = "Glucose",
     ylab = "BMI",
     main = "Glucose vs BMI")

# Question 6
cols_to_change <- c("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI")

df[cols_to_change] <- lapply(df[cols_to_change], function(x) ifelse(x == 0, NA, x))

colSums(is.na(df[cols_to_change])) ## I added a test line.

# Question 7
summary(df)  ## Updates the statistical summary and displays the NA count.
colSums(is.na(df)) ## Directly returns the exact number of NAs for each column.

# Question 8
df_under50 <- df %>% filter(Age < 50)  ## Filters the rows according to the condition.
df_above50 <- df %>% filter(Age >= 50)

write.csv(df_under50, "diabetes_under50.csv", row.names = FALSE) ## Ensures that an unnecessary index coiumn is not added.
write.csv(df_above50, "diabetes_above50.csv", row.names = FALSE)

# Question 9
result_indices <- c() ## Creates an empty vector to stote matching rows.

for (i in 1:nrow(df)) {   ## A loop that iterates over each row in the table.
  glucose_val <- df$Glucose[i]
  insulin_val <- df$Insulin[i]
  
  if (!is.na(glucose_val) & !is.na(insulin_val) & insulin_val > 0) {
    if ((glucose_val / insulin_val) < 1.0) {
      result_indices <- c(result_indices, i)
    }
  }
}

result_indices

# Question 10
df %>%
  group_by(Age_Group) %>% ## Using the column created in step 4, calculate the average for each group.
  summarise(
    mean_pregnancies = mean(Pregnancies)
  )