#Clear working environment
rm(list = ls())
#set wd 
setwd("C:/Users/janna/Documents/Masterthesis")

#load data 
df <- read.csv2("BISQ_CESD.csv")

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
  random = list(maternal_id= pdDiag(~ time_numeric)),  # entfernt die Korrelation zwischen Intercept und Steigung
  data = df,
  method = "REML"
)
summary(lme_model_test)


#Model mit time as continious
#no correlation 
lme_model_test_daysF <- nlme::lme(
  fixed = value ~ daysF,  # Fixed Effects
  random = list(maternal_id= pdDiag(~ daysF)),  # Keine korrelation
  data = df,
  method = "REML",
)
summary(lme_model_test_daysF)


#Test time continious with corrleation
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


# pdSymm und ohne Argmuent selber output --> schätz kovarianz zwischen intercept und slope  
##pdDiag bessere Wahl, pdSymm/Standart --> sehr sehr hohe korrelation, überanpassung und instabilität
# nur wenige Probanden, bessere wahl das einfachere Modell zu nehmen 
# überanpassung kann cleaness der parameter beeinflussen, 
# da die aber clean sein sollten zur weiterverarbeitung --> lieber anderes modell nehmen 
# Anahme: Intercept und slope sind unabhängig voneinander, MÜtter mit höherem Intercept haben keinen 
# steileren slope oder sowas 

# Diskussion: Mehr schlaf am anfang könnte mehr abfall bedeuten? 
# mehr Probanden zu späterem zeitpunkt --> mehr Modellkomplexität möglich
# gucken ob pdSymm besser passt 
# Diskussion: zeit als catgeorische vairable wieder einfügen, 
# besonders bei Probanden die besseres alignment zwischen expected and real data collection timepoint haben 


####MODEL CREATION ACTI DATA ####

df_Acti <- read.csv2("ACTI_CESD.csv")
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


#Test random slope and intercept, time categorical 
lme_model_test_ACTI <- nlme::lme(
  fixed = value ~ time_numeric,
  random = list(maternal_id= pdDiag(~ time_numeric)),  # entfernt die Korrelation zwischen Intercept und Steigung
  data = df_Acti,
  method = "REML"
)
summary(lme_model_test_ACTI)

#Test random slope and intercept, time continious
lme_model_test_daysA <- nlme::lme(
  fixed = value ~ daysA,
  random = list(maternal_id= pdDiag(~ daysA)),  # entfernt die Korrelation zwischen Intercept und Steigung
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
anova (lme_model_ACTI_cs, lme_model_test_daysA)
# p-value = 0.3687 (not significant) # no improvement of correlation in or out 


####Extract slopes and intercepts####
#Excract parameters
random_effects_BISQ <- ranef(lme_model_test)
random_effects_ACTI <- ranef(lme_model_test_ACTI)

random_effects_BISQ_cs <- ranef(lme_model_test_daysF)
random_effects_ACTI_cs <- ranef(lme_model_test_daysA)

#exract fixed parameters 
#Fixed effects model time categorical
fixed_effects_BISQ <- data.frame(fixef(lme_model_test))
print(fixed_effects_BISQ)
fixed_effects_ACTI <- data.frame(fixef(lme_model_test_ACTI))
print(fixed_effects_ACTI)

#Fixed effects model time continious
fixed_effects_BISQ_cs <- data.frame(fixef(lme_model_test_daysF))
print(fixed_effects_BISQ_cs)
fixed_effects_ACTI_cs <- data.frame(fixef(lme_model_test_daysA))
print(fixed_effects_ACTI_cs)

####Calculate exact slope####
#create slope df
#time continous
merged_slopes_cs <- data.frame(
  maternal_id = rownames(random_effects_ACTI_cs),  # oder die Spalte, die die ID enthält
  slope_1 = random_effects_BISQ_cs$daysF,
  slope_2 = random_effects_ACTI_cs$daysA
)

#time categrocial 
merged_slopes <- data.frame(
  maternal_id = rownames(random_effects_BISQ),  # oder die Spalte, die die ID enthält
  slope_1 = random_effects_BISQ$time_numeric,
  slope_2 = random_effects_ACTI$time_numeric
)

#extract fixed slope time categorical  
fixed_slope_ACTI <- fixed_effects_ACTI["time_numeric", 1]  # Wert für ACTI
fixed_slope_BISQ <- fixed_effects_BISQ["time_numeric", 1]  # Wert für BISQ


#extract fixed slope time continious
fixed_slope_ACTI_cs <- fixed_effects_ACTI_cs["daysA", 1]  # Wert für ACTI
fixed_slope_BISQ_cs <- fixed_effects_BISQ_cs["daysF", 1]  # Wert für BISQ


#calculate actual slope time categorical
merged_slopes$actual_slope_1 <- merged_slopes$slope_1 + fixed_slope_BISQ
merged_slopes$actual_slope_2 <- merged_slopes$slope_2 + fixed_slope_ACTI

#calculate actual slope time continious
merged_slopes_cs$actual_slope_1 <- merged_slopes_cs$slope_1 + fixed_slope_BISQ_cs
merged_slopes_cs$actual_slope_2 <- merged_slopes_cs$slope_2 + fixed_slope_ACTI_cs


####CORRELATION OF SLOPES####
#correlation model time categorical, random slope and intercept
result_slope <- cor.test(merged_slopes$actual_slope_1, merged_slopes$actual_slope_2)
print(result_slope)

# Vergleich mit einem Alpha-Niveau
alpha <- 0.05
if (result_slope$p.value < alpha) {
  print("Die Korrelation von Model 1 ist signifikant.")
} else {
  print("Die Korrelation von Model 1 ist nicht signifikant.")
}


#correlation model time continious, random slope and intercept
result_slope_cs <- cor.test(merged_slopes_cs$actual_slope_1, merged_slopes_cs$actual_slope_2)
print(result_slope_cs)

# Vergleich mit einem Alpha-Niveau
alpha <- 0.05
if (result_slope_cs$p.value < alpha) {
  print("Die Korrelation von Model 2 ist signifikant.")
} else {
  print("Die Korrelation von Model 2 ist nicht signifikant.")
}
###signifikante Korrelation zwischen den slopes in Model 2 (time continious, rs and ri)

####Calculate actual intercepts####
#create intercept df
merged_intercepts <- data.frame(
  maternal_id = rownames(random_effects_ACTI),
  intercept_1 = random_effects_BISQ$`(Intercept)`,
  intercept_2 = random_effects_ACTI$`(Intercept)`
)

#Create íntercept df continious
merged_intercepts_cs <- data.frame(
  maternal_id = rownames(random_effects_ACTI_cs),
  intercept_1 = random_effects_BISQ_cs$`(Intercept)`,
  intercept_2 = random_effects_ACTI_cs$`(Intercept)`
)

#extract fixed intercept
#categroical
fixed_intercept_ACTI <- fixed_effects_ACTI["(Intercept)", 1]  # Wert für ACTI
fixed_intercept_BISQ <- fixed_effects_BISQ["(Intercept", 1]  # Wert für BISQ

#calculate real intercepts
merged_intercepts$actual_intercept_1 <- merged_intercepts$intercept_1 + fixed_intercept_BISQ
merged_intercepts$actual_intercept_2 <- merged_intercepts$intercept_2 + fixed_intercept_ACTI

#continious
fixed_intercept_ACTI_cs <- fixed_effects_ACTI_cs["(Intercept)", 1]  # Wert für ACTI
fixed_intercept_BISQ_cs <- fixed_effects_BISQ_cs["(Intercept", 1]  # Wert für BISQ

#calculate real intercepts
merged_intercepts_cs$actual_intercept_1 <- merged_intercepts_cs$intercept_1 + fixed_intercept_BISQ_cs
merged_intercepts_cs$actual_intercept_2 <- merged_intercepts_cs$intercept_2 + fixed_intercept_ACTI_cs

####CORRELATE INTERCEPTS####
#correlation model time categorical
result_intercept <- cor.test(merged_intercepts$actual_intercept_1, merged_intercepts$actual_intercept_2)
print(result_intercept)
# Vergleich mit einem Alpha-Niveau
alpha <- 0.05
if (result_intercept$p.value < alpha) {
  print("Die Korrelation der Intercepts von Model 1 ist signifikant.")
} else {
  print("Die Korrelation der Intercepts von Model 1 ist nicht signifikant.")
}


#correlation model time continious
result_intercept_cs <- cor.test(merged_intercepts_cs$actual_intercept_1, merged_intercepts_cs$actual_intercept_2)
print(result_intercept_cs)
# Vergleich mit einem Alpha-Niveau
alpha <- 0.05
if (result_intercept_cs$p.value < alpha) {
  print("Die Korrelation der Intercepts von Model 2 ist signifikant.")
} else {
  print("Die Korrelation der Intercepts von Model 2  ist nicht signifikant.")
}



####Correlation Slope Plot#### 
library(ggplot2)
library(ggpubr)

#normalize data because values are very small for actual_slope_1, to make correlation 
#more visible in diagram 
merged_slopes_cs$actual_slope_1_scaled <- scale(merged_slopes_cs$actual_slope_1)
merged_slopes_cs$actual_slope_2_scaled <- scale(merged_slopes_cs$actual_slope_2)

corr_scaled <- cor.test(merged_slopes_cs$actual_slope_1_scaled, merged_slopes_cs$actual_slope_2_scaled)
print(corr_scaled)

# Scatterplot mit Regressionslinie und Korrelationsinfo
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


# Scatterplot mit Regressionslinie und Korrelationsinfo
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
# Berechne den Durchschnitt CESD-Score für jeden Probanden (maternal_id)
#df_CESD_mean <- CESD %>%
 # group_by(maternal_id) %>%             # Gruppiere nach maternal_id
  #summarize(mean_cesd = mean(cesd_mean_imp, na.rm = TRUE))  # Berechne den Mittelwert, NA-Werte werden ignoriert

# Zeige die berechneten Mittelwerte
#print(df_CESD_mean)

#write.csv2(df_CESD_mean, "CESD_overall_mean.csv", row.names = FALSE) #save as table 
#Plot CESD daten 
# Convert timepoint to an ordered factor
CESD$timepoint <- factor(CESD$timepoint, levels = c("M01", "M03", "M06", "M09", "M12"))

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

  ggsave("Spagetti_plot_CESD.png", dpi = 300, width = 8, height = 5)
  
setwd("C:/Users/janna/Documents/Masterthesis")

# Zusammenführen der Daten nach "maternal_id" und ggf. "timepoint", wenn du eine zeitpunktbezogene Analyse machen willst
merged_data_slope <- merge(merged_slopes_cs, CESD, by = "maternal_id")
merged_data_slope$mean_cesd_scaled <- scale(merged_data_slope$cesd_total_imp)

# Lineares Modell mit Interaktionsterm
#modell: slope BISQ ~ slope Acti*CESD mean

moderation_model <- lm(actual_slope_1 ~ actual_slope_2 * mean_cesd_scaled, data = merged_data_slope)

# Zusammenfassung des Modells
summary(moderation_model)

##Keine signifikante moderation von CESD auf BISQ-ACTI interaction, p =0.37



####MODERATION ANALYSIS INTERCEPT####
# Zusammenführen der Daten nach "maternal_id" und ggf. "timepoint", wenn du eine zeitpunktbezogene Analyse machen willst
merged_data_intercept <- merge(merged_intercepts_cs, CESD, by = "maternal_id")
merged_data_intercept$mean_cesd_scaled <- scale(merged_data_slope$cesd_total_imp)

# Lineares Modell mit Interaktionsterm
moderation_model_ic <- lm(actual_intercept_1_scaled ~ actual_intercept_2_scaled * mean_cesd_scaled, data = merged_data_intercept)
# Zusammenfassung des Modells
summary(moderation_model_ic)
# Lineares Modell mit Interaktionsterm
merged_data_intercept$cesd_category <- as.factor(merged_data_intercept$cesd_category)
moderation_model_ic_cat <- lm(actual_intercept_1_scaled ~ actual_intercept_2_scaled * cesd_category, data = merged_data_intercept)
summary(moderation_model_ic_cat)

merged_data_intercept$predicted <- predict(moderation_model_ic_cat)

# Moderationseffekt plotten
ggplot(merged_data_intercept, aes(x = actual_intercept_2_scaled, y = actual_intercept_1_scaled, color = cesd_category)) +
  geom_point() +  # Streudiagramm der Daten
  geom_line(aes(y = predicted), size = 1) +  # Modellvorhersagen
  labs(title = "Moderation Plot",
       x = "Intercepts Actigraphy (scaled)",
       y = "Intercepts BISQ (scaled)",
       color = "CESD Category") +
  theme_minimal() 

# Moderationseffekt plotten slope
ggplot(merged_data_slope, aes(x = actual_slope_2_scaled, y = actual_slope_1_scaled, color = cesd_category)) +
  geom_point() +  # Streudiagramm der Daten
  geom_line(aes(y = predicted), size = 1) +  # Modellvorhersagen
  labs(title = "Moderation Plot",
       x = "Slopes Actigraphy (scaled)",
       y = "Slopes BISQ (scaled)",
       color = "CESD Category") +
  theme_minimal() 
