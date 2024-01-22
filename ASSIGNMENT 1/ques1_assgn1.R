# Load the Iris dataset
data(iris)

# Perform one-way ANOVA for each variable
anova_results <- lapply(iris[, 1:4], function(variable) {
  aov_result <- aov(variable ~ Species, data = iris)
  return(summary(aov_result))
})

# Display the ANOVA results
print("ANOVA Results for Sepal Length:")
print(anova_results$Sepal.Length)

print("ANOVA Results for Sepal Width:")
print(anova_results$Sepal.Width)

print("ANOVA Results for Petal Length:")
print(anova_results$Petal.Length)

print("ANOVA Results for Petal Width:")
print(anova_results$Petal.Width)



# Kruskal-Wallis test for each variable
kruskal_results <- lapply(iris[, 1:4], function(variable) {
  kruskal_result <- kruskal.test(variable ~ Species, data = iris)
  return(kruskal_result)
})

# Display the Kruskal-Wallis test results
print("Kruskal-Wallis Test Results for Sepal Length:")
print(kruskal_results$Sepal.Length)

print("Kruskal-Wallis Test Results for Sepal Width:")
print(kruskal_results$Sepal.Width)

print("Kruskal-Wallis Test Results for Petal Length:")
print(kruskal_results$Petal.Length)

print("Kruskal-Wallis Test Results for Petal Width:")
print(kruskal_results$Petal.Width)

cor(iris[,1:4])

# Load necessary libraries
library(MASS)
library(reshape2)
library(ggplot2)

# Load the Iris dataset
data(iris)

# Extract numerical variables
iris_numeric <- iris[, 1:4]

# Rename columns to lowercase to match typical column names in the Iris dataset
colnames(iris_numeric) <- c("sepal.length", "sepal.width", "petal.length", "petal.width")

# Function to calculate half-space depth for a given variable
calculate_half_space_depth <- function(data, variable) {
  depth_values <- depth(data[, variable])$hdepth
  return(data.frame(value = depth_values, Species = data$Species))
}

# Calculate half-space depth for each variable
depth_results <- lapply(colnames(iris_numeric), function(variable) {
  calculate_half_space_depth(iris, variable)
})

# Combine depth values for all variables
depth_df <- do.call(rbind, depth_results)

# Visualize half-space depth values using boxplots
ggplot(melt(depth_df, id.vars = "Species"), aes(x = variable, y = value, fill = Species)) +
  geom_boxplot() +
  labs(title = "Half-Space Depth Values for Iris Dataset",
       x = "Variable",
       y = "Half-Space Depth") +
  theme_minimal()


dat <- iris[,1:4]
y <- iris$Species
half1 <- depth.halfspace(x=dat,data= dat[y=="setosa",])
half2 <- depth.halfspace(x=dat, data=dat[y=="versicolor",])
half3 <- depth.halfspace(dat, dat[y=="virginica",])


plot(half2,half1)
