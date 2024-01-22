library(ggplot2)
library(ks)

set.seed(1001)
gpa_study_hours <- read.csv("gpa_study_hours_data.csv")
d <- gpa_study_hours
x <- gpa_study_hours$gpa
y <- gpa_study_hours$study_hours

kd <- ks::kde(d, compute.cont = TRUE)

# Define contour levels
cont_levels <- seq(10, 90, by = 10)

# Create a list to store contour data frames
contour_list <- vector("list", length = length(cont_levels))

# Generate contour lines for each level and store in the list
for (i in seq_along(cont_levels)) {
  level <- cont_levels[i]
  contour_list[[i]] <- data.frame(with(kd, contourLines(x = eval.points[[1]], y = eval.points[[2]],
                                                        z = estimate, levels = cont[paste0(level, "%")])[[1]]),
                                  level = level)
}

# Combine all contour data frames into a single data frame
contour_data <- do.call(rbind, contour_list)

# Plot the data using ggplot
ggplot(data = d, aes(x, y))  + geom_point()+
  geom_path(aes(x, y, group = as.factor(level), col = as.factor(level.1)), data = contour_data) +
  theme_bw() +
  scale_color_discrete(name = "Contour Level", breaks = cont_levels)

#colored plot
ggplot(d, aes(gpa,study_hours))+
  stat_density_2d(aes(fill=..level..), geom="polygon",color="white")+
  scale_fill_gradient(low="lightblue", high="darkblue")+
  geom_point(alpha=0.5, size=2)+
  theme_minimal()




##finding the mode numerically
library(MASS)
kde <- kde2d(d$gpa, d$study_hours, n = 193)
mode_index <- which(kde$z == max(kde$z), arr.ind = TRUE)
mode_values <- c(kde$x[mode_index[1, 1]], kde$y[mode_index[1, 2]])
mode_values

#finding the skewness and kurtosis numerically
# Calculate skewness
library(moments)
skewness_values <- skewness(d)

# Calculate kurtosis
kurtosis_values <- kurtosis(d)
