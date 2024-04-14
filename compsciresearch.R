data <- Comp_Sci_Research_Data_R
seconds  <- data$`5-Second Interval`
y <- data$`%user AWS Normal`
y1 <- data$`%system AWS Normal`
y2 <- data$`%user Azure Normal`
y3  <- data$`%system Azure Normal`
y4 <- data$`%user AWS Stressed`
y5 <- data$`%system AWS Stressed`
y6 <- data$`%user Azure Stressed`
y7 <- data$`%system Azure Stressed`
y8 <- data$`%idle AWS Normal`
y9 <- data$`%idle Azure Normal`
y10 <- data$`%idle AWS Stressed`
y11 <- data$`%idle Azure Stressed`

mean(y)
mean(y1)

##Below is the code for graphing line plot between %user and %system conditions on AWS##
# Install and load required packages
install.packages("ggplot2")  # Uncomment this line if you haven't installed ggplot2 yet
library(ggplot2)

# Your data
x <- c(seconds)
y <- c(y)
y1 <- c(y1)
y4 <- c(y4)
y5 <- c(y5)

# Create a data frame
df <- data.frame(x = x, y = y, y1 = y1, y4 = y4, y5 = y5)

# Convert data to long format for ggplot
df_long <- tidyr::gather(df, key = "variable", value = "value", -x)

# Plot
ggplot(df_long, aes(x = x, y = value, color = variable)) +
  geom_line() +
  labs(x = "Seconds", y = "% of Usage", title = "AWS %user vs. %system in Normal and Stressed Conditions") +
  ylim(0,100) +
  scale_color_discrete(labels = c("%user Normal", "%system Normal", "%user Stressed", "%system Stressed")) +
  theme_minimal()

hist(y-y1)
hist(y4-y5)

##Paired t-test for significance between y4 and y5, AWS Stressed %user and %system
y4 <- c(y4)
y5 <- c(y5)

#Perform t-test
t_test_result <- t.test(y4, y5, paired = TRUE)

#Print t-test
print(t_test_result)

##Paired t-test for significance between y-y1 and y4-y5. AWS Normal and Stressed
# Data
y <- c(y)
y1 <- c(y1)
y4 <- c(y4)
y5 <- c(y5)

#Differences
diff_y_y1 <- y - y1
diff_y4_y5 <- y4 - y5

# Perform paired t-test
t_test_result <- t.test(diff_y_y1, diff_y4_y5, paired = TRUE)

# Print the results
print(t_test_result)


##Bar Graph of Means for y, y1 AWS Normal Conditions
# Install and load required packages
library(ggplot2)

# Sample data
y_means <- c(mean(y), mean(y1))
variables <- c("%user", "%system")

# Create a data frame
df_means <- data.frame(variable = variables, mean_value = y_means)

# Plot
ggplot(df_means, aes(x = variable, y = mean_value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(x = "Mean Usage", y = "% of Usage", title = "%user vs. $system AWS- Normal Conditions") +
  theme_minimal() +
  ylim(0, 0.1) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

##Bar Graph of Means for y4, y5 AWS Stressed Conditions
library(ggplot2)

# Sample data
y_means <- c(mean(y4), mean(y5))
variables <- c("%user", "%system")

# Create a data frame
df_means <- data.frame(variable = variables, mean_value = y_means)

# Plot
ggplot(df_means, aes(x = variable, y = mean_value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(x = "Mean Usage", y = "% of Usage", title = "%user vs. $system AWS- Stressed Conditions") +
  theme_minimal() +
  ylim(0, 100) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

##Graph comparions for Azure
library(ggplot2)

# Your data
x <- c(seconds)
y <- c(y2)
y1 <- c(y3)
y4 <- c(y6)
y5 <- c(y7)

# Create a data frame
df <- data.frame(x = x, y2 = y2, y3 = y3, y6 = y6, y7 = y7)

# Convert data to long format for ggplot
df_long <- tidyr::gather(df, key = "variable", value = "value", -x)

# Plot
ggplot(df_long, aes(x = x, y = value, color = variable)) +
  geom_line() +
  labs(x = "Seconds", y = "% of Usage", title = "Azure %user vs. %system in Normal and Stressed Conditions") +
  ylim(0,100) +
  scale_color_discrete(labels = c("%user Normal", "%system Normal", "%user Stressed", "%system Stressed")) +
  theme_minimal()

##Bar Graph of Means for y2, y3 Azure Normal Conditions
library(ggplot2)

# Sample data
y_means <- c(mean(y2), mean(y3))
variables <- c("%user", "%system")

# Create a data frame
df_means <- data.frame(variable = variables, mean_value = y_means)

# Plot
ggplot(df_means, aes(x = variable, y = mean_value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(x = "Mean Usage", y = "% of Usage", title = "%user vs. $system Azure- Normal Conditions") +
  theme_minimal() +
  ylim(0, 0.15) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))


##Bar graph for means of y6 and y7, Azure Stressed
y6 <- data$`%user Azure Stressed`
y7 <- data$`%system Azure Stressed`

library(ggplot2)

#Calculate Standard errors
# Sample data
y6 <- data$`%user Azure Stressed`
y7 <- data$`%system Azure Stressed`

# Calculate standard errors
y6_sem <- sd(y6) / sqrt(length(y6))
y7_sem <- sd(y7) / sqrt(length(y7))

# Create a data frame for mean values
y_means <- c(mean(y6), mean(y7))
variables <- c("%user", "%system")
df_means <- data.frame(variable = variables, mean_value = y_means)

# Create a data frame for standard errors
df_sem <- data.frame(variable = variables, sem = c(y6_sem, y7_sem))

# Plot with error bars
library(ggplot2)
ggplot(df_means, aes(x = variable, y = mean_value, fill = variable)) +
  geom_bar(stat = "identity") +
  geom_errorbar(data = df_sem, aes(ymin = mean_value - sem, ymax = mean_value + sem, 
                                   color = variable), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Mean Usage", y = "% of Usage", title = "%user vs. %system Azure - Stressed Conditions") +
  theme_minimal() +
  ylim(0, 100) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))




##Code to find the mean difference between normal and stressed differences of Azure and AWS
y8 <- c(y8)
y9 <- c(y9)
y10 <- c(y10)
y11 <- c(y11)

#Differences
diff_y8_y10 <- y8 - y10
diff_y9_y11 <- y9 - y11

# Perform paired t-test
t_test_result <- t.test(diff_y8_y10, diff_y9_y11, paired = TRUE)

# Print the results
print(t_test_result)


##Mean Difference
mean(y8-y10)
mean(y9-y11)

##Difference between stressed %idle
t_test_result <- t.test(y10,y11)

##Print results
print(t_test_result)


hist(y8)
hist(y9)
hist(y10)
hist(y11)

##Significance in Stressed Conditions
mean(y4-y5)
mean(y6-y7)

diff_y4_y5 <- y4-y5
diff_y6_y7 <- y6-y7

t_test_rest <- t.test(diff_y4_y5, diff_y6_y7)

print(t_test_result)


# Load the ggplot2 package
library(ggplot2)

y10 <- data$`%idle AWS Stressed`
y11 <- data$`%idle Azure Stressed`
# Calculate means
mean_y10 <- mean(y10)
mean_y11 <- mean(y11)

# Create a data frame for means
means_df <- data.frame(Variable = c("%idle AWS Stressed", "%idle Azure Stressed"), Mean = c(mean_y10, mean_y11))

# Create bar chart
ggplot(means_df, aes(x = Variable, y = Mean, fill = Variable)) +
  geom_bar(stat = "identity") +
  labs(x = "Variable", y = "Mean %usage", fill = "Variable", title = "Average %idle of AWS and Azure Under Stressed Conditions")

mean(y10)
mean(y11)


t_test_result <- t.test(y10, y11)

print(t_test_result)



#%idle Stressed Conditions of AWS and Azure 
library(ggplot2)

y10 <- data$`%idle AWS Stressed`
y11 <- data$`%idle Azure Stressed`
# Calculate means
mean_y10 <- mean(y10)
mean_y11 <- mean(y11)

# Create a data frame for means
means_df <- data.frame(Variable = c("%idle AWS Stressed", "%idle Azure Stressed"), Mean = c(mean_y10, mean_y11))

# Create bar chart
ggplot(means_df, aes(x = Variable, y = Mean, fill = Variable)) +
  geom_bar(stat = "identity") +
  labs(x = "Variable", y = "Mean %usage", fill = "Variable", title = "Average %idle of AWS and Azure Under Stressed Conditions")

mean(y10)
mean(y11)


t_test_result <- t.test(y10, y11)

print(t_test_result)