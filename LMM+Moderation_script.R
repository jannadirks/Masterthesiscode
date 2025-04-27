#Clear working environment
rm(list = ls())
#set wd 
setwd("C:/Users/janna/Documents/Masterthesis")

#load BISQ and CESD data 
df <- read.csv2("BISQ_CESD.csv")
df_Acti <- read.csv2("ACTI_CESD.csv")

#Collect timepoints of assesment from the BISQ-data 
setwd("C:/Users/janna/Documents/Masterthesis/Abweichungen")
dates <- read.csv2("Collection_Dates.csv")

setwd("C:/Users/janna/Documents/Masterthesis")

#extract days since birth for BISQ
dates_BISQ <- data.frame(dates$daysF)
dates_BISQ <- na.omit(dates_BISQ)

#extract days since birth from ACTI 
dates_ACTI <- data.frame(dates$daysA)
dates_ACTI <- na.omit(dates_ACTI)

#add in days since birth into model data frame for BISQ
df$daysF <- dates_BISQ$dates.daysF

library(nlme)

####MODEL CREATION BISQ####

#model with random intercept only 
lme_model_BISQ_intercept <- nlme::lme(
  fixed = value ~ daysF,  # Fixed Effects
  random = ~ 1 | maternal_id,  # Random intercept only
  data = df,
  method = "REML"  # Restricted Maximum Likelihood
)

summary(lme_model_BISQ_intercept)


##model with time as categorical
#no correlation 
#nlme::lme necessary to use function from right package 
lme_model_test <- nlme::lme(
  fixed = value ~ time_numeric,
  random = list(maternal_id= pdDiag(~ time_numeric)),  
  data = df,
  method = "REML"
)
summary(lme_model_test)


#Model mit time as continious
#no correlation 
lme_model_test_daysF <- nlme::lme(
  fixed = value ~ daysF,  # Fixed Effects
  random = list(maternal_id= pdDiag(~ daysF)),  # exclusion of correlation between the random parameters 
  data = df,
  method = "REML",
)
summary(lme_model_test_daysF)


#model time continious with corrleation
lme_model_BISQ_cs <- nlme::lme(
  fixed = value ~ daysF,  # Fixed Effects
  random = ~ daysF | maternal_id, 
  data = df,
  method = "REML"
)
summary(lme_model_BISQ_cs)

##days as continous chosen becaue high scattering in data
# categoires would not align with underlying data structure, especially in Acigraphy data (see differences)
# not clearly sorted into categorics (M1-M12), due to difference in planned and real assesment timepoint 


####MODEL CREATION ACTI DATA ####
dates_ACTI <- dates_ACTI[-c(4, 5, 6), ]
dates_ACTI <- data.frame(dates_ACTI)
df_Acti$daysA <- dates_ACTI$dates_ACTI

# random intercept only 
lme_model_ACTI_intercept <- nlme::lme(
  fixed = value ~ daysA,  # Fixed Effects
  random = ~ 1 | maternal_id,  # Random intercept only
  data = df_Acti,
  method = "REML"  # Restricted Maximum Likelihood
)

summary(lme_model_ACTI_intercept)


#Test random slope and intercept, time continious
lme_model_test_daysA <- nlme::lme(
  fixed = value ~ daysA,
  random = list(maternal_id= pdDiag(~ daysA)),  # no correlation between random parameters 
  data = df_Acti,
  method = "REML"
)
summary(lme_model_test_daysA)

#Test time continious with corrleation
lme_model_ACTI_cs <- nlme::lme(
  fixed = value ~ daysA,  # Fixed Effects
  random = ~ daysA | maternal_id, 
  data = df_Acti,
  method = "REML"
)
summary(lme_model_ACTI_cs)

###AIC and BIC 
#BISQ
# contious, random intercept: AIC: 570.2503, BIC: 581.7511    
# continous, no correlation:AIC: 572.2503, BIC: 586.6263      
# continious, correlation: AIC: 566.3568, BIC: 583.608    
#Acti
# continious, random intercept: AIC: 425.3026, BIC:436.5837    
# continous, no correlation: AIC: 422.3411 BIC: 436. 4425
# continious, correlation: AIC: 423.533, BIC: 440.4547

###Likelihood ratio test 
#model intercept vs model slope und intercept 
library(lme4)
anova(lme_model_BISQ_intercept, lme_model_BISQ_cs)
#  p = 0.0193 
# inclusion of slope significant improvement 
# model correlation in vs out BISQ 
anova (lme_model_BISQ_cs, lme_model_test_daysF)
# p value = 0.005 
# but maybe due to overfitting


####Extract slopes and intercepts####
#Excract parameters
random_effects_BISQ_cs <- ranef(lme_model_test_daysF)
random_effects_ACTI_cs <- ranef(lme_model_test_daysA)

#exract fixed parameters 
#Fixed effects model time continious
fixed_effects_BISQ_cs <- data.frame(fixef(lme_model_test_daysF))
print(fixed_effects_BISQ_cs)
fixed_effects_ACTI_cs <- data.frame(fixef(lme_model_test_daysA))
print(fixed_effects_ACTI_cs)

#extract fixed slope time continious
fixed_slope_ACTI_cs <- fixed_effects_ACTI_cs["daysA", 1]  # Wert für ACTI
fixed_slope_BISQ_cs <- fixed_effects_BISQ_cs["daysF", 1]  # Wert für BISQ

####Calculate exact slope####
#create slope df
#time continous
merged_slopes_cs <- data.frame(
  maternal_id = rownames(random_effects_ACTI_cs),  
  slope_1 = random_effects_BISQ_cs$daysF,
  slope_2 = random_effects_ACTI_cs$daysA
)

#calculate actual slope time continious
merged_slopes_cs$actual_slope_1 <- merged_slopes_cs$slope_1 + fixed_slope_BISQ_cs
merged_slopes_cs$actual_slope_2 <- merged_slopes_cs$slope_2 + fixed_slope_ACTI_cs


####CORRELATION OF SLOPES####
#correlation model time continious, random slope and intercept
result_slope_cs <- cor.test(merged_slopes_cs$actual_slope_1, merged_slopes_cs$actual_slope_2)
print(result_slope_cs)

####Calculate actual intercepts####
#Create íntercept df continious
merged_intercepts_cs <- data.frame(
  maternal_id = rownames(random_effects_ACTI_cs),
  intercept_1 = random_effects_BISQ_cs$`(Intercept)`,
  intercept_2 = random_effects_ACTI_cs$`(Intercept)`
)

#extract fixed intercept
fixed_intercept_ACTI_cs <- fixed_effects_ACTI_cs["(Intercept)", 1]  # Wert für ACTI
fixed_intercept_BISQ_cs <- fixed_effects_BISQ_cs["(Intercept", 1]  # Wert für BISQ

#calculate real intercepts
merged_intercepts_cs$actual_intercept_1 <- merged_intercepts_cs$intercept_1 + fixed_intercept_BISQ_cs
merged_intercepts_cs$actual_intercept_2 <- merged_intercepts_cs$intercept_2 + fixed_intercept_ACTI_cs

####CORRELATE INTERCEPTS####

#correlation model time continious
result_intercept_cs <- cor.test(merged_intercepts_cs$actual_intercept_1, merged_intercepts_cs$actual_intercept_2)
print(result_intercept_cs)

####Correlation Slope Plot#### 
library(ggplot2)
library(ggpubr)

#normalize data 
merged_slopes_cs$actual_slope_1_scaled <- scale(merged_slopes_cs$actual_slope_1)
merged_slopes_cs$actual_slope_2_scaled <- scale(merged_slopes_cs$actual_slope_2)

# Scatterplot for regression 
ggplot(merged_slopes_cs, aes(x = actual_slope_1_scaled, y = actual_slope_2_scaled)) +
  geom_point(alpha = 0.7, color = "blue", size = 2) +            # Punkte
  geom_smooth(method = "lm", color = "red", se = FALSE, size = 1) +  # Regressionslinie
  labs(
    title = "Plot of the Correlation between the slopes of the different models",
    x = "Slopes BISQ (z-score)",
    y = "Slopes Actigraphy (z-score)"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 18), # Größe der Achsentitel
    axis.text = element_text(size = 18)  # Größe der Achsenticks
  ) +
  annotate("text",
           x = min(merged_slopes_cs$actual_slope_1_scaled, na.rm = TRUE),
           y = max(merged_slopes_cs$actual_slope_2_scaled, na.rm = TRUE),
           label = paste0("r = ", round(result_slope_cs$estimate, 2), 
                          "\n p = ", format.pval(result_slope_cs$p.value, digits = 3), "*"),
           hjust = 0, size = 7, color = "black")

ggsave("Correlationslope_plot.png", width = 6, height = 8, dpi = 500)  # Breiteres Bild für Präsentationen


####correlation Plot Intercept####
#normalize values to make range better and plot more clear 
merged_intercepts_cs$actual_intercept_1_scaled <- scale(merged_intercepts_cs$actual_intercept_1)
merged_intercepts_cs$actual_intercept_2_scaled <- scale(merged_intercepts_cs$actual_intercept_2)

# Scatterplot for correlation
ggplot(merged_intercepts_cs, aes(x = actual_intercept_1_scaled, y = actual_intercept_2_scaled)) +
  geom_point(alpha = 0.7, color = "blue", size = 2) +            # Punkte
  geom_smooth(method = "lm", color = "red", se = FALSE, size = 1) +  # Regressionslinie
  labs(
    title = "Plot of the correlation between the intercepts of the different models",
    x = "Intercepts BISQ (z-scores) ",
    y = "Intercepts Actigraphy (z-scores)"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 22), # Größe der Achsentitel
    axis.text = element_text(size = 22)  # Größe der Achsenticks
  ) +
  annotate("text",
           x = min(merged_intercepts_cs$actual_intercept_1_scaled, na.rm = TRUE),
           y = max(merged_intercepts_cs$actual_intercept_2_scaled, na.rm = TRUE),
           label = paste0("r = ", round(result_intercept_cs$estimate, 2), 
                          "\n p = ", format.pval(result_intercept_cs$p.value, digits = 3)),
           hjust = 0, size = 8, color = "black")

ggsave("Correlationslope_plot_intercept.png", width = 8, height = 9, dpi = 500)  # Breiteres Bild für Präsentationen



####MODERATION ANALYSIS SLOPE#####
###Depression scores integrieren 
#load CESD table 
setwd("C:/Users/janna/Documents/Masterthesis/CESD")
CESD <- read.csv2("CESD_data.csv")

library(dplyr)
# calculate mean_cesd (maternal_id)
#df_CESD_mean <- CESD %>%
 # group_by(maternal_id) %>%             # Gruppiere nach maternal_id
  #summarize(mean_cesd = mean(cesd_mean_imp, na.rm = TRUE))  # Berechne den Mittelwert, NA-Werte werden ignoriert
#write.csv2(df_CESD_mean, "CESD_overall_mean.csv", row.names = FALSE) #save as table 

#Plot CESD daten 
# Convert timepoint to an ordered factor
CESD$timepoint <- factor(CESD$timepoint, levels = c("M01", "M03", "M06", "M09", "M12")
# Create the spaghetti plot
ggplot(CESD, aes(x = timepoint, y = cesd_total, group = maternal_id, color = as.factor(maternal_id))) +
  geom_line(alpha = 0.7) +  # Draw lines
  geom_point(size = 1.5) +  # Add points
  labs(
    title = "Plot of CESD Total Scores for each participant for each timepoint",
    x = "Timepoint",
    y = "CESD Total Score",
    color = "Maternal ID"
  ) +
  theme_minimal()
  theme(legend.position = "none")  # Optionally remove legend for clarity

#save plot                     
ggsave("Spagetti_plot_CESD.png", dpi = 300, width = 8, height = 5)

#change WD                    
setwd("C:/Users/janna/Documents/Masterthesis")

#Moderation analysis slopes 
# merge data of slopes and CESD 
merged_data_slope <- merge(merged_slopes_cs, CESD, by = "maternal_id")
merged_data_slope$mean_cesd_scaled <- scale(merged_data_slope$cesd_total_imp)

#formulate linear model (BISQ-slope ~ Acti-slope * CESD-mean)
moderation_model <- lm(actual_slope_1 ~ actual_slope_2 * mean_cesd_scaled, data = merged_data_slope)
summary(moderation_model)


####MODERATION ANALYSIS INTERCEPT####
# merge intercept and CESD data 
merged_data_intercept <- merge(merged_intercepts_cs, CESD, by = "maternal_id")
merged_data_intercept$mean_cesd_scaled <- scale(merged_data_slope$cesd_total_imp)

# formulate model (BISQ-intercept ~ Acti-Intercept*CESD-mean
moderation_model_ic <- lm(actual_intercept_1_scaled ~ actual_intercept_2_scaled * mean_cesd_scaled, data = merged_data_intercept)
summary(moderation_model_ic)

#### Creation of moderation plots ####                         
# Plot of intercept moderation effect using CESD-Categories
ggplot(merged_data_intercept, aes(x = actual_intercept_2_scaled, y = actual_intercept_1_scaled, color = cesd_category)) +
  geom_point() +  
  geom_line(aes(y = predicted), size = 1) +  
  labs(title = "Moderation Plot",
       x = "Intercepts Actigraphy (scaled)",
       y = "Intercepts BISQ (scaled)",
       color = "CESD Category") +
  theme_minimal() 

#Plot of slope moderation effect using CESD-Categories
ggplot(merged_data_slope, aes(x = actual_slope_2_scaled, y = actual_slope_1_scaled, color = cesd_category)) +
  geom_point() +  
  geom_line(aes(y = predicted), size = 1) +  
  labs(title = "Moderation Plot",
       x = "Slopes Actigraphy (scaled)",
       y = "Slopes BISQ (scaled)",
       color = "CESD Category") +
  theme_minimal() 
