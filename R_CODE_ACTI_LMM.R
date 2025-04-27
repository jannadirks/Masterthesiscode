###ACTIGRAPHY LINEAR MIXED MODEL####
#set wd 
setwd("C:/Users/janna/Documents/Masterthesis/Abweichungen")

ACTI_DATA <- data<- read.csv2('Acti_Average_withDates.csv', header = TRUE)
str(ACTI_DATA$Mean_sleepActi_M1_h)

setwd("C:/Users/janna/Documents/Masterthesis")

ACTI_DATA [ACTI_DATA == 0] <- NA



# mean and sd of M1 over all participants 
M1_mean_acti_h <- mean(ACTI_DATA$Mean_sleepActi_M1_h, na.rm = TRUE)
M1_sd_acti_h <- sd(ACTI_DATA$Mean_sleepActi_M1_h, na.rm = TRUE)

# mean and sd of M3 over all participants 
M3_mean_acti_h <- mean(ACTI_DATA$Mean_sleepActi_M3_h, na.rm = TRUE)
M3_sd_acti_h <- sd(ACTI_DATA$Mean_sleepActi_M3_h, na.rm = TRUE)

# mean and sd of M6 over all participants 
M6_mean_acti_h <- mean(ACTI_DATA$Mean_sleepActi_M6_h, na.rm = TRUE)
M6_sd_acti_h <- sd(ACTI_DATA$Mean_sleepActi_M6_h, na.rm = TRUE)

# mean and sd of M9 over all participants 
M9_mean_acti_h <- mean(ACTI_DATA$Mean_sleepActi_M9_h, na.rm = TRUE)
M9_sd_acti_h <- sd(ACTI_DATA$Mean_sleepActi_M9_h, na.rm = TRUE)

# mean and sd of M12 over all participants 
M12_mean_acti_h <- mean(ACTI_DATA$Mean_sleepActi_M12_h, na.rm = TRUE)
M12_sd_acti_h <- sd(ACTI_DATA$Mean_sleepActi_M12_h, na.rm = TRUE)

#create Table 
description_acti <- data.frame(
  Measurement = c("M1", "M3", "M6", "M9", "M12"),
  Mean = c(M1_mean_acti_h, M3_mean_acti_h, M6_mean_acti_h, M9_mean_acti_h, M12_mean_acti_h),
  SD = c(M1_sd_acti_h, M3_sd_acti_h, M6_sd_acti_h, M9_sd_acti_h, M12_sd_acti_h)
)
#round on second decimal
description_acti$Mean <- round(description_acti$Mean,2)
description_acti$SD <- round(description_acti$SD,2)

write.csv2(description_acti, "ACTI_description.csv", row.names = TRUE)


##Create LMM DATA 
LMM_Data_ACTI <- data.frame(ACTI_DATA$Maternal_ID, 
                       ACTI_DATA$Mean_sleepActi_M1_h, 
                       ACTI_DATA$Mean_sleepActi_M3_h,
                       ACTI_DATA$Mean_sleepActi_M6_h,
                       ACTI_DATA$Mean_sleepActi_M9_h,
                       ACTI_DATA$Mean_sleepActi_M12_h
                       )

#Create New column names for easier access
new_colnames <- c("Maternal_ID",  
                  "M01",   
                  "M03",   
                  "M06",   
                  "M09",   
                  "M12")  

colnames(LMM_Data_ACTI) <- new_colnames
colnames(LMM_Data_ACTI)

#pivot to long format 
LMM_long_data_ACTI <- LMM_Data_ACTI%>%
  pivot_longer(
    cols = c(M01,M03, M06, M09, M12),   
    names_to = "time",                   
    values_to = "value",                 
    values_drop_na = TRUE                
  )

LMM_long_data_ACTI$Maternal_ID <- unlist(LMM_long_data_ACTI$Maternal_ID)

write.csv2(LMM_long_data_ACTI, "Acti_sleep.csv",row.names = FALSE)
LMM_long_data_ACTI$Maternal_ID <- as.factor(LMM_long_data_ACTI$Maternal_ID)

# Rename coloum Maternal_ID to maternal_id
LMM_long_data_ACTI <- LMM_long_data_ACTI %>%
  rename(maternal_id = Maternal_ID)

#turn M01-M12 in 1-12 (numeric values)
LMM_long_data_Acti_test<- LMM_long_data_ACTI %>%
  mutate(
    time_numeric = case_when(
      time == "M01" ~ 1,
      time == "M03" ~ 3,
      time == "M06" ~ 6,
      time == "M09" ~ 9,
      time == "M12" ~ 12,
      TRUE ~ as.numeric(time) # für andere Fälle, falls vorhanden
    )
  )
head(LMM_long_data_Acti_test)

write.csv2(LMM_long_data_Acti_test, "Average_ACTI.csv", row.names = FALSE)

#Deskriptive Statistiken für 'value' nach 'timepoint'
description_by_timepoint_ACTI <- describeBy(LMM_long_data_Acti_test$value, group = LMM_long_data_Acti_test$time)

# Umwandlung der Ausgabe in ein DataFrame (es wird eine Liste von DataFrames für jede Gruppe erzeugt)
description_by_timepoint_ACTI_df <- do.call(rbind, description_by_timepoint_ACTI)

# Speichern der deskriptiven Statistiken als CSV-Datei
write.csv2(description_by_timepoint_ACTI_df, "ACTIdescription_by_timepoint.csv", row.names = FALSE)


#Plotte die Datenpunkte nach ID 
library(ggplot2)
ggplot(LMM_long_data_Acti_test, aes(x = time_numeric, y = value, color = maternal_id)) +
  geom_point() +                 
  geom_line(aes(group = maternal_id)) +   # one line for each ID
  labs(x = "Assesment timepoint", y = "Sleep in hours", title = "Sleep over measurement period (Actigraphy)") +
  ylim(8.5, 22) +
  scale_x_continuous(breaks = c(3, 6, 9, 12)) +# limits for axis values 
  theme_minimal()+
  theme(plot.title = element_text(size = 14, family = "Calibri"), #Title size and font
  axis.title.x = element_text(size = 12, family = "Calibri"),  #x-lab size and font
  axis.title.y = element_text(size = 12, family = "Calibri"))  #y-lab size and font

ggsave("ACTI_Spagetti-plot.png", plot = last_plot(), width = 6, height = 4.5, dpi = 300)


###Linear Mixed Models mit LMER (MAYBE NOT USED)####
##Linear Mixed Models
# LMM Modell with random intercepts 
lmm_model_Acti_test <- lmer(value ~ time_numeric + (1 | maternal_id), data = LMM_long_data_Acti_test)
summary(lmm_model_Acti_test)

# model with random slope and intercept 
lmm_model_Acti_test_rs<- lmer(value ~ time_numeric + (time_numeric | maternal_id), data = LMM_long_data_Acti_test)
summary(lmm_model_Acti_test_rs)


##LMM with time as contious factor 
#Add in time in days for each assesment calculated in different script 
ACTI_days <- long_Actidates[, c("Maternal_ID", "daysA")]
ACTI_days[ACTI_days == 0] <- NA #turn all 0 into NA
ACTI_days <- na.omit(ACTI_days) #delete all NAs
ACTI_days <- ACTI_days[-2, ] #1402 M3 date delete --> no data
ACTI_days <- ACTI_days[-4, ] #1402 M9 date delete --> no data 

#Append time in days since birth to the LMM_Acti data frame 
LMM_long_data_ACTI$daysA <- ACTI_days$daysA
LMM_long_data_ACTI$time_numeric <- LMM_long_data_Acti_test$time_numeric
write.csv2(LMM_long_data_ACTI,"output_datei_Acti.csv", row.names = FALSE)

# LMM with days (continous) instead of Ms, random intercept but fixed slope  
LMM_Acti_days_continous <- lmer(value ~ daysA + (1 | maternal_id), data = LMM_long_data_ACTI)
summary(LMM_Acti_days_continous)

# LMM with randome slope und random intercept 
LMM_ACTI_days_continous_rs <- lmer(value ~ daysA + (1 + daysA | maternal_id), data = LMM_long_data_ACTI)

summary(LMM_ACTI_days_continous_rs)

#Extract model parameters
aic_lmm_model_Acti <- AIC(lmm_model_Acti_test)
aic_lmm_model_Acti_rs <- AIC(lmm_model_Acti_test_rs)
aic_LMM_Acti_days_continous <- AIC(LMM_Acti_days_continous)
aic_LMM_ACTI_days_continous_rs <- AIC(LMM_ACTI_days_continous_rs)

bic_lmm_model_Acti <- BIC(lmm_model_Acti_test)
bic_lmm_model_Acti <- BIC(lmm_model_Acti_test_rs)
bic_LMM_Acti_days_continous <- BIC(LMM_Acti_days_continous)
bic_LMM_ACTI_days_continous_rs <- BIC(LMM_ACTI_days_continous_rs)

cat("AIC Model 1:", aic_lmm_model_Acti, "\n")
cat("AIC Model 2:", aic_lmm_model_Acti_rs, "\n")
cat("AIC Model 3:", aic_LMM_Acti_days_continous, "\n")
cat("AIC Model 4:", aic_LMM_ACTI_days_continous_rs, "\n")

cat("BIC Model 1:", bic_lmm_model_Acti, "\n")
cat("BIC Model 2:", aic_lmm_model_Acti_rs, "\n")
cat("BIC Model 3:", bic_LMM_Acti_days_continous, "\n")
cat("BIC Model 4:", bic_LMM_ACTI_days_continous_rs, "\n")

##Create Data frame 
model_comparison_ACT <- data.frame(
  Modell = c("lmm_model_Acti", "LMM_Acti_days_continous", "LMM_ACTI_days_continous_rs"),
  AIC = c(aic_lmm_model_Acti, aic_LMM_Acti_days_continous, aic_LMM_ACTI_days_continous_rs),
  BIC = c(bic_lmm_model_Acti, bic_LMM_Acti_days_continous, bic_LMM_ACTI_days_continous_rs)
)
write.csv2(model_comparison_ACT, "BICAIC_ACTI.csv")


#Extract random slopes and intercepts 
random_effects_BISQ_lmr<- ranef(lmm_model_test_rs)
random_effects_ACTI_lmr<- ranef(lmm_model_Acti_test_rs)
slope_BISQ <- random_effects_BISQ_lmr$maternal_id[,"time_numeric"]
slope_Acti <- random_effects_ACTI_lmr$maternal_id[,"time_numeric"]
intercept_BISQ <- random_effects_BISQ_lmr$maternal_id[,"(Intercept)"]
intercept_ACTI <- random_effects_ACTI_lmr$maternal_id[,"(Intercept)"]

#create slope df
merged_slopes_new <- data.frame(
  maternal_id = LMM_Data_ACTI$Maternal_ID,  # oder die Spalte, die die ID enthält
  slope_1 = slope_BISQ,
  slope_2 = slope_Acti
)

##correlate slopes 
cor(merged_slopes_new$slope_1, merged_slopes_new$slope_2)

#create intercept df
merged_intercepts_new <- data.frame(
  maternal_id = LMM_Data_ACTI$Maternal_ID,  # oder die Spalte, die die ID enthält
  intercept_1 = intercept_BISQ,
  intercept_2 = intercept_ACTI
)

##correlate intercepts 
cor(merged_intercepts_new$intercept_1, merged_intercepts_new$intercept_2)

#Significance testing slope correlation
result_slope <- cor.test(merged_slopes_new$slope_1, merged_slopes_new$slope_2)
print(result_slope)

#Significance testing intercept correlation
result_intercept <- cor.test(merged_intercepts_new$intercept_1, merged_intercepts_new$intercept_2)
print(result_intercept)



###Correlation for random intercept but fixed slope 
#Extract random slopes and intercepts 
random_effects_BISQ<- ranef(lmm_model_test)
random_effects_ACTI<- ranef(lmm_model_Acti_test)
fixed_effects_BISQ<- fixef(lmm_model_test)
fixed_effects_ACTI<- fixef(lmm_model_Acti_test)
intercept_BISQ <- random_effects_BISQ$maternal_id[,"(Intercept)"]
intercept_ACTI <- random_effects_ACTI$maternal_id[,"(Intercept)"]