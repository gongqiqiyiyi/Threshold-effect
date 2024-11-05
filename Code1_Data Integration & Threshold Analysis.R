library(dplyr)
library(readxl)
library(writexl)
library(ggplot2)
#Read in the case data to analyze
data <- read_excel("supplementary dataset2.xlsx")

#standardize the data
normalized_data <- data %>%
  group_by(StudyID) %>%
  mutate(Biomass_normalized = (Biomass - min(Biomass)) / (max(Biomass) - min(Biomass)))

normalized_data <- data %>%
  group_by(StudyID) %>%
  mutate(Biomass_normalized = Biomass/ max(Biomass))

normalized_data <- data %>%
  group_by(StudyID) %>%
  mutate(Biomass_normalized = (Biomass - mean(Biomass)) / sd(Biomass))




#######Data integration analysis
normalized_data <- read_excel("supplementary dataset2.xlsx")
segmented_model <- segmented(lm(Biomass_normalized ~ Naddition, data = normalized_data), seg.Z = ~Naddition)


summary_result <- summary(segmented_model)
summary_result


##Predict and create a data box that contains the upper and lower limits of the interval
prediction_intervals <- predict(segmented_model, interval = "prediction", level = 0.95, newdata = normalized_data)
confidence_intervals <- predict(segmented_model, interval = "confidence", level = 0.95, newdata = normalized_data)

prediction_intervals_df <- data.frame(Naddition = normalized_data$Naddition,
                                      lower = prediction_intervals[, "lwr"],
                                      upper = prediction_intervals[, "upr"],
                                      Biomass_normalized = normalized_data$Biomass_normalized)
confidence_intervals_df <- data.frame(Naddition = normalized_data$Naddition,
                                      lower = confidence_intervals[, "lwr"],
                                      upper = confidence_intervals[, "upr"],
                                      Biomass_normalized = normalized_data$Biomass_normalized)  # 添加 Biomass_normalized 列
#plot the results
label_position <- data.frame(x = max(normalized_data$Naddition) - 2, y = min(normalized_data$Biomass_normalized) + 0.3)

ggplot(normalized_data, aes(x = Naddition, y = Biomass_normalized)) +
  geom_point(color = "green4", alpha = 0.4, size = 4) +
  geom_ribbon(data = confidence_intervals_df, 
              aes(x = Naddition, ymin = lower, ymax = upper), 
              fill = "orangered2", alpha = 0.4) +  
  geom_ribbon(data = prediction_intervals_df, 
              aes(x = Naddition, ymin = lower, ymax = upper), 
              fill = "lightgrey", alpha = 0.2) +  
  geom_smooth(data = data.frame(Naddition = segmented_model$model$Naddition),
              aes(y = predict(segmented_model, newdata = data.frame(Naddition = Naddition))), 
              se = FALSE, color = "brown2",  size = 1.2) +  
  geom_text(data = label_position,
            aes(x = x, y = y, 
                label = paste( "Threshold:", 10,"\n", 
                               "Seg. Slope 1:", round(summary_result$coefficients[2, 1], 3), "\n",
                               "Seg. Slope 2:", round(summary_result$coefficients[2, 2], 3), "\n",
                               "P-value:", format.pval(summary_result$coefficients[2, 4], digits = 4),"\n",
                               "R-squared:", round(summary_result$r.squared, 3))),  
            vjust = 1, hjust = 1, size = 5, color = "black") +  
  geom_vline(xintercept = 10, linetype = "dotted", color = "orangered2", size = 1.2, alpha = 0.8) +  
  labs(x = "Naddition",
    y = "Biomass_normalized (Z score)", family="Arial") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 15), 
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())





#######Threshold analysis for each case

library(chngpt)

mydata <- read.csv("fittingtemp5.csv", header = TRUE, sep = ",")


unique_groups <- unique(mydata$Group)
num_groups <- length(unique_groups)

par(mfrow = c(ceiling(num_groups / 5), 5), mar = c(2, 3, 2, 1))

for (i in 1:num_groups) {
  group <- unique_groups[i]
  group_data <- subset(mydata, Group == group)
  fit <- chngptm.xy(group_data$Naddition, group_data$Biomass, type = "segmented")
  threshold_value <- fit$chngpt
  plot(fit, 1,lcol="#EE443199", col = "#63D07F", pch = 19, cex = 2, xlab = "Naddition", ylab = "Biomass")
  abline(v = threshold_value, col = "#EE4431", lty = 2)
  mtext(text = paste("No.", group), side = 3, line = 0, cex = 1, col = "black")
  mtext(text = paste(threshold_value), side = 3, line = -6, cex = 1, col = "#B9181A")