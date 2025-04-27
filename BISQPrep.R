#Clear working environment
rm(list = ls())

#set wd 
setwd("C:/Users/janna/Documents/Masterthesis/Abweichungen")

#Read in data of Actigraph to collect the same ids 
Actidates<- read.table('Daten_ACTI.csv', header = TRUE, sep = ';')

#set wd 
setwd("C:/Users/janna/Documents/Masterthesis")


#load packages 
library(readr)
library(psych)
library(lubridate)
library(chron)
library(ggplot2)
library(lavaan)
library(brokenstick)
library(data.table)
library(cowplot)
library(lme4) # for the analysis
#library(lmerTest)
library(car)
library(IrregLong)
library(datawizard) #for winsorizing
library(tidyverse) #for by_group function
library(dplyr)
library(tidyr)

#For text 
install.packages("extrafont")
library(extrafont)

#Read in data
data<- read.table('BISQ_aktuell.csv', header = TRUE, sep = ';')
#describe data
describe(data)
write.csv2(describe(data), file = "BISQ_descr.csv")

#rename Timepoints
data$timepoint <- data$redcap_event_name #rename item
data$timepoint[data$timepoint == 'm1_arm_1'] <- 'M01'
data$timepoint[data$timepoint == 'm3_arm_1'] <- 'M03'
data$timepoint[data$timepoint == 'm6_arm_1'] <- 'M06'
data$timepoint[data$timepoint == 'm9_arm_1'] <- 'M09'
data$timepoint[data$timepoint == 'm12a_arm_1'] <- 'M12'

##NIGHTSLEEP 
#Transform to duration
data$bisq_sleepnight <- as.duration(hms(data$bisq_3_sleepnicht_197)) #as.duration returns duration in seconds

#Set to NA if duration greater than 12h,  because NIGHT = 7pm-7am (only 12 hours) 
data$bisq_sleepnight_12_dich <- ifelse(data$bisq_sleepnight <= 43200, 'under', 'over')
data$bisq_sleepnight_12 <- ifelse(data$bisq_sleepnight <= 43200, data$bisq_sleepnight, NA)

##DAYSLEEP##
data$bisq_sleepday <- as.duration(hms(data$bisq_4_sleepday_719))

#Set to NA if duration greater than 12h because DAY = 7a-7pm (only 12 hours)
data$bisq_sleepday_12_dich <- ifelse(data$bisq_sleepday <= 43200, 'under', 'over')
data$bisq_sleepday_12 <- ifelse(data$bisq_sleepday <= 43200, data$bisq_sleepday, NA)

##TOTAL SLEEP##
#Calculated as sum of day and night sleep 
data$bisq_sleeptotal <- rowSums(data[, c("bisq_sleepnight_12", "bisq_sleepday_12")],na.rm=F)

#Safe Sleep times as hour values instead of seconds 
data$bisq_sleepnight_h <- as.numeric(data$bisq_sleepnight_12/3600)
data$bisq_sleepday_h <- as.numeric(data$bisq_sleepday_12/3600)
data$bisq_sleeptotal_h <- as.numeric(data$bisq_sleeptotal/3600)

#Only Select relevant IDs for which we have Actigraphy data 
IDs <- as.table(Actidates$Maternal_ID)
print(IDs)
data_filtered <- subset(data, maternal_id %in% IDs)

#export df with only bisq_sleep total 
BISQ_sleep_total <- data.frame(
  maternal_id = data_filtered$maternal_id,
  timepoint = data_filtered$timepoint,
  bisq_sleeptotal_h = data_filtered$bisq_sleeptotal_h
)
BISQ_sleep_total <- na.omit(BISQ_sleep_total)
# save as csv file 
write.csv2(BISQ_sleep_total, "BISQsleep.csv", row.names = FALSE)

#Deskriptive Statistiken für 'value' nach 'timepoint'
description_by_timepoint <- describeBy(data_filtered$bisq_sleeptotal_h, group = data_filtered$timepoint)

# Umwandlung der Ausgabe in ein DataFrame (es wird eine Liste von DataFrames für jede Gruppe erzeugt)
description_by_timepoint_df <- do.call(rbind, description_by_timepoint)

# Speichern der deskriptiven Statistiken als CSV-Datei
write.csv2(description_by_timepoint_df, "BISQdescription_by_timepoint.csv", row.names = FALSE)

description_by_timepoint_df$vars <- c(1,3,6,9,12)
description_by_timepoint_ACTI_df$vars <- c(1,3,6,9,12)

#Plot of average for each timepoint BISQ alone
ggplot(description_by_timepoint_df, aes(x = vars, y = mean)) +
  geom_line(color = "blue", size = 1) +  # Linie in Blau
  geom_point(color = "blue", size = 3) +  # Punkte in Blau
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "blue") +  # Error Bars
  labs(x = "Timepoint (Months)", y = "Mean Sleep (Hours)", title = "Mean Sleep over Time BISQ") +
  theme_minimal()  # Schlichte Darstellung

#### Include correct Error bars####
#BISQ
description_by_timepoint_df <- description_by_timepoint_df %>%
  mutate(
    lower_ci = mean - sd,
    upper_ci = mean + sd
  )
#ACTI 
description_by_timepoint_ACTI_df <- description_by_timepoint_ACTI_df %>%
  mutate(
    lower_ci = mean - sd,
    upper_ci = mean + sd
  )

#Plot BISQ and ACTI 
ggplot() +
  # BISQ-Data
  geom_line(data = description_by_timepoint_df, aes(x = vars, y = mean, color = "BISQ"), size = 1) +
  geom_point(data = description_by_timepoint_df, aes(x = vars, y = mean, color = "BISQ"), size = 3) +
  geom_errorbar(data = description_by_timepoint_df, aes(x = vars, ymin = lower_ci, ymax = upper_ci, color = "BISQ"), width = 0.2) +
  
  # Actigraphy-Data
  geom_line(data = description_by_timepoint_ACTI_df, aes(x = vars, y = mean, color = "Actigraphy"), size = 1) +
  geom_point(data = description_by_timepoint_ACTI_df, aes(x = vars, y = mean, color = "Actigraphy"), size = 3) +
  geom_errorbar(data = description_by_timepoint_ACTI_df, aes(x = vars, ymin = lower_ci, ymax = upper_ci, color = "Actigraphy"), width = 0.2) +
  labs(
    x = "Timepoint (Months)", 
    y = "Mean Sleep (Hours)", 
    title = "Mean Sleep over Time: BISQ vs. Actigraphy",
    color = "Method"  # Titel der Legende
  ) +
  scale_x_continuous(breaks = c(1, 3, 6, 9, 12)) +
  scale_y_continuous(limits = c(12, 19)) +
  scale_color_manual(values = c("BISQ" = "blue", "Actigraphy" = "red")) +
  
  theme_minimal()  

ggsave("BISQ_ACTIplot.png", plot = last_plot(), width = 6, height = 4, dpi = 300)


#####LMM Preparation#####
#Wide format
wide_data <- pivot_wider(data_filtered, names_from = timepoint, values_from = bisq_sleeptotal_h)

# Anzeige des Ergebnisses
print(wide_data)

# collect only needed collumns 
smaller_df <- wide_data[, c("maternal_id", "M01", "M03", "M06", "M09", "M12")]
print(smaller_df)
smaller_df_neu <- smaller_df %>%
  mutate(across(starts_with("M"), ~ map(.x, ~ if (is.null(.x)) NA else .x)))
print(smaller_df_neu) # change 0 to NA
                                        
# exclude rows with 5 0
rows_to_remove <- which(rowSums(is.na(smaller_df_neu)) == 5)
smaller_clean <- smaller_df_neu[-rows_to_remove, ]

# change everything to numeric 
smaller_clean$M01 <- sapply(smaller_clean$M01, function(x) ifelse(length(x) > 0, x[[1]], NA))
smaller_clean$M03 <- sapply(smaller_clean$M03, function(x) ifelse(length(x) > 0, x[[1]], NA))
smaller_clean$M06 <- sapply(smaller_clean$M06, function(x) ifelse(length(x) > 0, x[[1]], NA))
smaller_clean$M09 <- sapply(smaller_clean$M09, function(x) ifelse(length(x) > 0, x[[1]], NA))
smaller_clean$M12 <- sapply(smaller_clean$M12, function(x) ifelse(length(x) > 0, x[[1]], NA))

# Gruppieren nach maternal_id und zusammenfassen
LMM_data <- smaller_clean%>%
  group_by(maternal_id) %>%
  summarise(across(everything(), ~ if(all(is.na(.))) NA else .[!is.na(.)][1]))

#Delete last row (NA)
LMM_data <- LMM_data[-nrow(LMM_data), ]
LMM_data$maternal_id <- unlist(LMM_data$maternal_id)
write.csv2(LMM_data, "BISQ_average_each.csv", row.names = FALSE)

#turn into long format for LMMs              
LMM_long_data <- LMM_data %>%
  pivot_longer(
    cols = c(M01, M03, M06, M09, M12),   
    names_to = "time",                  
    values_to = "value",                  
    values_drop_na = TRUE                 
  )

#turn ID into factor 
LMM_long_data$maternal_id <- unlist(LMM_long_data$maternal_id)
LMM_long_data$maternal_id<- as.factor(LMM_long_data$maternal_id)


####Spagettiplot####
#Plot of Data for all assesments for each ID  
  ggplot(LMM_long_data, aes(x = time_numeric, y = value, color = maternal_id)) +
    geom_point() +                 
    geom_line(aes(group = maternal_id)) +   # one line for each ID
    labs(x = "Assessment timepoint", y = "Sleep in hours", title = "Sleep over measurement period (BISQ)") +
    ylim(8.5, 22) + 
    scale_x_continuous(breaks = c(3, 6, 9, 12)) + # limits for axis values
    theme_minimal() + # use minimal theme with white background
    theme(plot.title = element_text(size = 14, family = "Calibri"), # Title size and font
          axis.title.x = element_text(size = 12, family = "Calibri"),  # x-lab size and font
          axis.title.y = element_text(size = 12, family = "Calibri"))  # y-lab size and font
  
  ggsave("BISQ_Spagetti-plot.png", plot = last_plot(), width = 6, height = 4.5, dpi = 300)
  
