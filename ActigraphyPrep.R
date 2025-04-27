###ACTIGRAPHY Preproccesing####
#set wd 
setwd("C:/Users/janna/Documents/Masterthesis")

#read in Actigraphy averages 
ACTI_DATA <- data<- read.csv2('Acti_Average_withDates.csv', header = TRUE)
#replace O with NAs
ACTI_DATA [ACTI_DATA == 0] <- NA

# calculate mean and sd of M1 over all participants 
M1_mean_acti_h <- mean(ACTI_DATA$Mean_sleepActi_M1_h, na.rm = TRUE)
M1_sd_acti_h <- sd(ACTI_DATA$Mean_sleepActi_M1_h, na.rm = TRUE)

# calculate mean and sd of M3 over all participants 
M3_mean_acti_h <- mean(ACTI_DATA$Mean_sleepActi_M3_h, na.rm = TRUE)
M3_sd_acti_h <- sd(ACTI_DATA$Mean_sleepActi_M3_h, na.rm = TRUE)

# calculate mean and sd of M6 over all participants 
M6_mean_acti_h <- mean(ACTI_DATA$Mean_sleepActi_M6_h, na.rm = TRUE)
M6_sd_acti_h <- sd(ACTI_DATA$Mean_sleepActi_M6_h, na.rm = TRUE)

# calculate mean and sd of M9 over all participants 
M9_mean_acti_h <- mean(ACTI_DATA$Mean_sleepActi_M9_h, na.rm = TRUE)
M9_sd_acti_h <- sd(ACTI_DATA$Mean_sleepActi_M9_h, na.rm = TRUE)

#calculate mean and sd of M12 over all participants 
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
write.csv2(LMM_long_data_ACTI, "Acti_sleep.csv",row.names = FALSE) #export as CSV data 

#turn ID into factor
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

#Plotte of Data according to ID in Spagettiplot 
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
