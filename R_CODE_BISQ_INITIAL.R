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

data$timepoint <- data$redcap_event_name

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

#Blot BISQ alone
ggplot(description_by_timepoint_df, aes(x = vars, y = mean)) +
  geom_line(color = "blue", size = 1) +  # Linie in Blau
  geom_point(color = "blue", size = 3) +  # Punkte in Blau
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "blue") +  # Error Bars
  labs(x = "Timepoint (Months)", y = "Mean Sleep (Hours)", title = "Mean Sleep over Time BISQ") +
  theme_minimal()  # Schlichte Darstellung

#### Einfügen von Errorbars####

#standartabweichung berechnen 
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
  # Linie und Punkte für BISQ-Daten
  geom_line(data = description_by_timepoint_df, aes(x = vars, y = mean, color = "BISQ"), size = 1) +
  geom_point(data = description_by_timepoint_df, aes(x = vars, y = mean, color = "BISQ"), size = 3) +
  geom_errorbar(data = description_by_timepoint_df, aes(x = vars, ymin = lower_ci, ymax = upper_ci, color = "BISQ"), width = 0.2) +
  
  # Linie und Punkte für Actigraphy-Daten
  geom_line(data = description_by_timepoint_ACTI_df, aes(x = vars, y = mean, color = "Actigraphy"), size = 1) +
  geom_point(data = description_by_timepoint_ACTI_df, aes(x = vars, y = mean, color = "Actigraphy"), size = 3) +
  geom_errorbar(data = description_by_timepoint_ACTI_df, aes(x = vars, ymin = lower_ci, ymax = upper_ci, color = "Actigraphy"), width = 0.2) +
  
  # Achsenbeschriftung und Titel
  labs(
    x = "Timepoint (Months)", 
    y = "Mean Sleep (Hours)", 
    title = "Mean Sleep over Time: BISQ vs. Actigraphy",
    color = "Method"  # Titel der Legende
  ) +
  
  # X- und Y-Achsen mit spezifischen Limits und Breaks
  scale_x_continuous(breaks = c(1, 3, 6, 9, 12)) +
  scale_y_continuous(limits = c(12, 19)) +
  
  # Manuelle Farbanpassung für Legende
  scale_color_manual(values = c("BISQ" = "blue", "Actigraphy" = "red")) +
  
  theme_minimal()  # Schlichte Darstellung


ggsave("BISQ_ACTIplot.png", plot = last_plot(), width = 6, height = 4, dpi = 300)

##DESCRIPTIVE ANALYSIS EACH TIMEPOINT##
#install.packages("dplyr")


#Create subdatenset for M1
subdatenset_m1 <- data_filtered %>%
  filter(timepoint == "M01") %>%
  select(maternal_id, bisq_sleepnight_12, bisq_sleepday_12, bisq_sleeptotal)

#Create subdatenset for M3
subdatenset_m3 <- data_filtered %>%
  filter(timepoint == "M03") %>%
  select(maternal_id, bisq_sleepnight_12, bisq_sleepday_12, bisq_sleeptotal)

#Create subdatenset for M6
subdatenset_m6 <- data_filtered %>%
  filter(timepoint == "M06") %>%
  select(maternal_id, bisq_sleepnight_12, bisq_sleepday_12, bisq_sleeptotal)


#Create subdatenset for M9
subdatenset_m9 <- data_filtered %>%
  filter(timepoint == "M09") %>%
  select(maternal_id, bisq_sleepnight_12, bisq_sleepday_12, bisq_sleeptotal)

#Create subdatenset for M12
subdatenset_m12 <- data_filtered %>%
  filter(timepoint == "M12") %>%
  select(maternal_id, bisq_sleepnight_12, bisq_sleepday_12, bisq_sleeptotal)


#print them out 
head(subdatenset_m1)
head(subdatenset_m3)
head(subdatenset_m6)
head(subdatenset_m9)
head(subdatenset_m12)

##Descriptiv Analysis M1##
# Filtere die Daten, um nur die Zeilen ohne NA-Werte in der Spalte bisq_sleeptotal zu behalten
M1_filtered_data <- subdatenset_m1 %>%
  filter(!is.na(bisq_sleeptotal))

# Mean of bisq_sleeptotal (total sleep at M1)
M1_mean_gesamtschlafdauer <- M1_filtered_data %>%
  summarise(mean_schlafdauer_TOTAL = mean(bisq_sleeptotal)/3600)
print(M1_mean_gesamtschlafdauer)

M1_sd_gesamtschlafdauer <- M1_filtered_data %>%
  summarise(sd_schlafdauer_TOTAL = sd(bisq_sleeptotal)/3600)
print(M1_sd_gesamtschlafdauer)

#Mean of Day sleep at M1
M1_mean_gesamtschlafdauer_TAG <- M1_filtered_data %>%
  summarise(mean_schlafdauer_TAG = mean(bisq_sleepday_12)/3600)
print(M1_mean_gesamtschlafdauer_TAG)

M1_sd_gesamtschlafdauer_TAG <- M1_filtered_data %>%
  summarise(sd_schlafdauer_TAG = sd(bisq_sleepday_12)/3600)
print(M1_sd_gesamtschlafdauer_TAG)

#Mean of bisq_sleepnight_12
M1_mean_gesamtschlafdauer_NACHT <- M1_filtered_data %>%
  summarise(mean_schlafdauer_NACHT = mean(bisq_sleepnight_12)/3600)
print(M1_mean_gesamtschlafdauer_NACHT)

M1_sd_gesamtschlafdauer_NACHT <- M1_filtered_data %>%
  summarise(sd_schlafdauer_NACHT = sd(bisq_sleepnight_12)/3600)
print(M1_sd_gesamtschlafdauer_NACHT)

M1_mean <- data.frame(M1_mean_gesamtschlafdauer, M1_sd_gesamtschlafdauer, 
                      M1_mean_gesamtschlafdauer_NACHT,M1_sd_gesamtschlafdauer_NACHT,
                      M1_mean_gesamtschlafdauer_TAG, M1_sd_gesamtschlafdauer_TAG)

##Descriptiv Analysis M3##
# Filtere die Daten, um nur die Zeilen ohne NA-Werte in der Spalte bisq_sleeptotal zu behalten
M3_filtered_data <- subdatenset_m3 %>%
  filter(!is.na(bisq_sleeptotal))

# Berechne den Mittelwert der Spalte bisq_sleeptotal
M3_mean_gesamtschlafdauer <- M3_filtered_data %>%
  summarise(mean_schlafdauer_TOTAL = mean(bisq_sleeptotal)/3600)
print(M3_mean_gesamtschlafdauer)

M3_sd_gesamtschlafdauer <- M3_filtered_data %>%
  summarise(sd_schlafdauer_TOTAL = sd(bisq_sleeptotal)/3600)
print(M3_sd_gesamtschlafdauer)

#Mean of bisq_sleepday_12
M3_mean_gesamtschlafdauer_TAG <- M3_filtered_data %>%
  summarise(mean_schlafdauer_TAG = mean(bisq_sleepday_12)/3600)
print(M3_mean_gesamtschlafdauer_TAG)

M3_sd_gesamtschlafdauer_TAG <- M3_filtered_data %>%
  summarise(sd_schlafdauer_TAG = sd(bisq_sleepday_12)/3600)
print(M3_sd_gesamtschlafdauer_TAG)

#Mean of bisq_sleepnight_12
M3_mean_gesamtschlafdauer_NACHT <- M3_filtered_data %>%
  summarise(mean_schlafdauer_NACHT = mean(bisq_sleepnight_12)/3600)
print(M3_mean_gesamtschlafdauer_NACHT)

M3_sd_gesamtschlafdauer_NACHT <- M3_filtered_data %>%
  summarise(sd_schlafdauer_NACHT = sd(bisq_sleepnight_12)/3600)
print(M3_sd_gesamtschlafdauer_NACHT)

M3_mean <- data.frame(M3_mean_gesamtschlafdauer, M3_sd_gesamtschlafdauer, 
                      M3_mean_gesamtschlafdauer_NACHT,M3_sd_gesamtschlafdauer_NACHT,
                      M3_mean_gesamtschlafdauer_TAG, M3_sd_gesamtschlafdauer_TAG)

##Descriptiv Analysis M6##
# Filtere die Daten, um nur die Zeilen ohne NA-Werte in der Spalte bisq_sleeptotal zu behalten
M6_filtered_data <- subdatenset_m6 %>%
  filter(!is.na(bisq_sleeptotal))

# Berechne den Mittelwert der Spalte bisq_sleeptotal
M6_mean_gesamtschlafdauer <- M6_filtered_data %>%
  summarise(mean_schlafdauer_TOTAL = mean(bisq_sleeptotal)/3600)
print(M6_mean_gesamtschlafdauer)

#SD of bisq_total
M6_sd_gesamtschlafdauer <- M6_filtered_data %>%
  summarise(sd_schlafdauer_TOTAL = sd(bisq_sleeptotal)/3600)
print(M6_sd_gesamtschlafdauer)

#Mean of bisq_sleepday_12
M6_mean_gesamtschlafdauer_TAG <- M6_filtered_data %>%
  summarise(mean_schlafdauer_TAG = mean(bisq_sleepday_12)/3600)
print(M6_mean_gesamtschlafdauer_TAG)

#SD of bisq_sleepday_12
M6_sd_gesamtschlafdauer_TAG <- M6_filtered_data %>%
  summarise(sd_schlafdauer_TAG = sd(bisq_sleepday_12)/3600)
print(M6_sd_gesamtschlafdauer_TAG)

#Mean of bisq_sleepnight_12
M6_mean_gesamtschlafdauer_NACHT <- M6_filtered_data %>%
  summarise(mean_schlafdauer_NACHT = mean(bisq_sleepnight_12)/3600)
print(M6_mean_gesamtschlafdauer_NACHT)

#SD of bisq_sleepnight_12
M6_sd_gesamtschlafdauer_NACHT <- M6_filtered_data %>%
  summarise(sd_schlafdauer_NACHT = sd(bisq_sleepnight_12)/3600)
print(M6_sd_gesamtschlafdauer_NACHT)

M6_mean <- data.frame(M6_mean_gesamtschlafdauer, M6_sd_gesamtschlafdauer, 
                      M6_mean_gesamtschlafdauer_NACHT,M6_sd_gesamtschlafdauer_NACHT,
                      M6_mean_gesamtschlafdauer_TAG, M6_sd_gesamtschlafdauer_TAG)

##Descriptiv Analysis M9##
# Filtere die Daten, um nur die Zeilen ohne NA-Werte in der Spalte bisq_sleeptotal zu behalten
M9_filtered_data <- subdatenset_m9 %>%
  filter(!is.na(bisq_sleeptotal))

# Berechne den Mittelwert der Spalte bisq_sleeptotal
M9_mean_gesamtschlafdauer <- M9_filtered_data %>%
  summarise(mean_schlafdauer_TOTAL = mean(bisq_sleeptotal)/3600)
print(M9_mean_gesamtschlafdauer)

#SD of bisq_sleeptotal 
M9_sd_gesamtschlafdauer <- M9_filtered_data %>%
  summarise(sd_schlafdauer_TOTAL = sd(bisq_sleeptotal)/3600)
print(M9_sd_gesamtschlafdauer)


#Mean of bisq_sleepday_12
M9_mean_gesamtschlafdauer_TAG <- M9_filtered_data %>%
  summarise(mean_schlafdauer_TAG = mean(bisq_sleepday_12)/3600)
print(M9_mean_gesamtschlafdauer_TAG)

#SD of bisq_sleepday_12
M9_sd_gesamtschlafdauer_TAG <- M9_filtered_data %>%
  summarise(sd_schlafdauer_TAG = sd(bisq_sleepday_12)/3600)
print(M9_sd_gesamtschlafdauer_TAG)

#Mean of bisq_sleepnight_12
M9_mean_gesamtschlafdauer_NACHT <- M9_filtered_data %>%
  summarise(mean_schlafdauer_NACHT = mean(bisq_sleepnight_12)/3600)
print(M9_mean_gesamtschlafdauer_NACHT)

#SD of bisq_sleepnight_12
M9_sd_gesamtschlafdauer_NACHT <- M9_filtered_data %>%
  summarise(sd_schlafdauer_NACHT = sd(bisq_sleepnight_12)/3600)
print(M9_sd_gesamtschlafdauer_NACHT)

M9_mean <- data.frame(M9_mean_gesamtschlafdauer, M9_sd_gesamtschlafdauer, 
                      M9_mean_gesamtschlafdauer_NACHT,M9_sd_gesamtschlafdauer_NACHT,
                      M9_mean_gesamtschlafdauer_TAG, M9_sd_gesamtschlafdauer_TAG)

##Descriptiv Analysis M12##
# Filtere die Daten, um nur die Zeilen ohne NA-Werte in der Spalte bisq_sleeptotal zu behalten
M12_filtered_data <- subdatenset_m12 %>%
  filter(!is.na(bisq_sleeptotal))

# Berechne den Mittelwert der Spalte bisq_sleeptotal
M12_mean_gesamtschlafdauer <- M12_filtered_data %>%
  summarise(mean_schlafdauer_TOTAL = mean(bisq_sleeptotal)/3600)
print(M12_mean_gesamtschlafdauer)

#SD of bisq_sleeptotal 
M12_sd_gesamtschlafdauer <- M12_filtered_data %>%
  summarise(sd_schlafdauer_TOTAL = sd(bisq_sleeptotal)/3600)
print(M12_sd_gesamtschlafdauer)

#Mean of bisq_sleepday_12
M12_mean_gesamtschlafdauer_TAG <- M12_filtered_data %>%
  summarise(mean_schlafdauer_TAG = mean(bisq_sleepday_12)/3600)
print(M12_mean_gesamtschlafdauer_TAG)

#SD of bisq_sleepday_12 
M12_sd_gesamtschlafdauer_TAG <- M12_filtered_data %>%
  summarise(sd_schlafdauer_TAG = sd(bisq_sleepday_12)/3600)
print(M12_sd_gesamtschlafdauer_TAG)

#Mean of bisq_sleepnight_12
M12_mean_gesamtschlafdauer_NACHT <- M12_filtered_data %>%
  summarise(mean_schlafdauer_NACHT = mean(bisq_sleepnight_12)/3600)
print(M12_mean_gesamtschlafdauer_NACHT)

#SD of bisq_sleepnight_12
M12_sd_gesamtschlafdauer_NACHT <- M12_filtered_data %>%
  summarise(sd_schlafdauer_NACHT = sd(bisq_sleepnight_12)/3600)
print(M12_sd_gesamtschlafdauer_NACHT)

M12_mean <- data.frame(M12_mean_gesamtschlafdauer, M12_sd_gesamtschlafdauer, 
                      M12_mean_gesamtschlafdauer_NACHT,M12_sd_gesamtschlafdauer_NACHT,
                      M12_mean_gesamtschlafdauer_TAG, M12_sd_gesamtschlafdauer_TAG)

#Create description table 
description <- rbind(M1_mean, M3_mean, M6_mean, M9_mean, M12_mean)
print(description)
# Spaltennamen anpassen
rownames(description) <- c("M1", "M3", "M6", "M9", "M12")

# Ergebnis anzeigen
print(description)

description$mean_schlafdauer_TOTAL<- round(description$mean_schlafdauer_TOTAL, 2)
description$sd_schlafdauer_TOTAL <- round(description$sd_schlafdauer_TOTAL, 2)
description$mean_schlafdauer_NACHT <- round(description$mean_schlafdauer_NACHT , 2)
description$sd_schlafdauer_NACHT  <- round(description$sd_schlafdauer_NACHT, 2)
description$mean_schlafdauer_TAG <- round(description$mean_schlafdauer_TAG, 2)
description$sd_schlafdauer_TAG <- round(description$sd_schlafdauer_TAG, 2)

write.csv2(description, "BSIQ_description.csv", row.names = TRUE)

#LMM vorbereitung
# Daten umwandeln ins Wide Format
wide_data <- pivot_wider(data_filtered, names_from = timepoint, values_from = bisq_sleeptotal_h)

# Anzeige des Ergebnisses
print(wide_data)

# Auswahl der gewünschten Spalten
smaller_df <- wide_data[, c("maternal_id", "M01", "M03", "M06", "M09", "M12")]
print(smaller_df)

# Alle 'NULL' Zeichenfolgen in NA umwandeln

smaller_df_neu <- smaller_df %>%
  mutate(across(starts_with("M"), ~ map(.x, ~ if (is.null(.x)) NA else .x)))

print(smaller_df_neu)


# Zeilen mit genau 5 NA finden
rows_to_remove <- which(rowSums(is.na(smaller_df_neu)) == 5)

#Zeilen entfernen
smaller_clean <- smaller_df_neu[-rows_to_remove, ]


# Umwandlung der Listen in numerische Vektoren (falls möglich)
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

LMM_long_data <- LMM_data %>%
  pivot_longer(
    cols = c(M01, M03, M06, M09, M12),   
    names_to = "time",                  
    values_to = "value",                  
    values_drop_na = TRUE                 
  )

#turn ID into factor for plotting 
LMM_long_data$maternal_id <- unlist(LMM_long_data$maternal_id)
LMM_long_data$maternal_id<- as.factor(LMM_long_data$maternal_id)

#turn M01-M12 in 1-12 (numeric values)
LMM_long_data_test<- LMM_long_data %>%
  mutate(
    time_numeric = case_when(
      timepoint == "M01" ~ 1,
      timepoint == "M03" ~ 3,
      timepoint == "M06" ~ 6,
      timepoint == "M09" ~ 9,
      timepoint == "M12" ~ 12,
      TRUE ~ as.numeric(timepoint) 
    ))

head(LMM_long_data_test)

####Spagettiplot####
#Plot of Data for all assesments for each ID  
  ggplot(LMM_long_data_test, aes(x = time_numeric, y = value, color = maternal_id)) +
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
  
  ###LMER MODELS (MAYBE NOT USED')####
#Test for model with 1,3,6,9,12 time
lmm_model_test <- lmer(value ~ time_numeric + (1 | maternal_id), data = LMM_long_data_test)
summary(lmm_model_test)

#Model random slope und random intercept 
lmm_model_test_rs <- lmer(value ~ time_numeric + (1 + time_numeric | maternal_id), data = LMM_long_data_test)
summary(lmm_model_test_rs)

# Zusammenfassung des Modells anzeigen
summary(lmm_model_test_rs)
aic_lmm_model_test_rs <- AIC(lmm_model_test_rs)
bic_lmm_model_test_rs <- BIC(lmm_model_test_rs)
aic_lmm_model_test <- AIC(lmm_model_test)
bic_lmm_model_test <- BIC(lmm_model_test)
cat("AIC Model:", aic_lmm_model_test_rs, "\n")
cat("BIC Model:", bic_lmm_model_test_rs, "\n")
cat("AIC Model2:", aic_lmm_model_test, "\n")
cat("BIC Model2:", bic_lmm_model_test, "\n")


aic_lmm_model_test_no_corr <- AIC(lmm_model_test_no_corr)
bic_lmm_model_test_no_corr <- BIC(lmm_model_test_no_corr)
cat("AIC Model:", aic_lmm_model_test_no_corr, "\n")
cat("BIC Model:", bic_lmm_model_test_no_corr, "\n")

# days integrieren 
BISQ_days <- long_BISQ[, c("Maternal_ID", "daysF")]
days_BISQ_clean <- na.omit(BISQ_days)

LMM_long_data$daysF <- days_BISQ_clean$daysF #include days
LMM_long_data$time_numeric <- LMM_long_data_test$time_numeric #include time numeric 

#Export csv file 
write.csv2(LMM_long_data, "output_dateiBISQ.csv", row.names = FALSE)

# LMM mit days anstatt Ms 
# LMM erstellen
LMM_days_continous <- lmer(value ~ daysF + (1 | maternal_id), data = LMM_long_data)
summary(LMM_days_continous)

#Extract AIC and BIC for comparison
aic_LMM_days_continous <- AIC(LMM_days_continous)
bic_LMM_days_continous <- BIC(LMM_days_continous)
#Print values of AIC and BIC for that model
cat("AIC Model 2:", aic_LMM_days_continous, "\n")
cat("BIC Model 2:", bic_LMM_days_continous, "\n")


# LMM with continous time, randome slope und random intercept 
LMM_days_continous_rs <- lmer(value ~ daysF + (1 + daysF | maternal_id), data = LMM_long_data)
summary(LMM_days_continous_rs)

#Extract model parameter
aic_LMM_days_continous_rs <- AIC(LMM_days_continous_rs)
bic_LMM_days_continous_rs <- BIC(LMM_days_continous_rs)
#Print AIC and BIC
cat("AIC Model 3:", aic_LMM_days_continous_rs, "\n")
cat("BIC Model 3:", bic_LMM_days_continous_rs, "\n")

#extract data frame 
model_comparison_BISQ <- data.frame(
  Modell = c("lmm_model_test","lmm_model_test_rs", "LMM_days_continous", "LMM_days_continous_rs"),
  AIC = c(aic_lmm_model_test,aic_lmm_model_test_rs, aic_LMM_days_continous, aic_LMM_days_continous_rs),
  BIC = c(bic_lmm_model_test,bic_lmm_model_test_rs, bic_LMM_days_continous, bic_LMM_days_continous_rs)
)

write.csv2(model_comparison_BISQ, "BICAIC_BISQ.csv")

# Mit continous time und rS/rI besser als nur mit rI und continous time 
# mit grouped time und nur rI immer noch besser # mit grouped und rS/rI nicht aufstellbar 


#Zusammenfügen BISQ und CESD Daten 
# Merge der CESD-Daten in den neuen Datensatz
merged_data_BISQ_CESD <- LMM_long_data_test %>%
  left_join(average_scores, by = "maternal_id")
write.csv2(merged_data_BISQ_CESD, "BISQ_CESD.csv")

#Zusammenfügen ACTI und CESD Daten 
setwd("C:/Users/janna/Documents/Masterthesis")
df_Acti <- read.csv2("output_datei_Acti.csv")
df_Acti$maternal_id <- as.character(df_Acti$maternal_id)
merged_data_ACTI_CESD <- df_Acti %>%
  left_join(average_scores, by = "maternal_id")
write.csv2(merged_data_ACTI_CESD, "ACTI_CESD.csv")


# Ergebnisse anzeigen
print(merged_data_BISQ_CESD)

#Analyse: Depression ja nein auf schlaf 
# Visualisierung der Verteilung von 'value' für jede 'cesd_category'
ggplot(merged_data_BISQ_CESD, aes(x = factor(cesd_category), y = value)) +
  geom_boxplot() +
  labs(x = "CESD Category (0 = No, 1 = Yes)", y = "Value") +
  theme_minimal

linear_model <- lm(value ~ cesd_category, data = merged_data_BISQ_CESD)
summary(linear_model)

ggplot(merged_data_BISQ_CESD, aes(x = value, y = cesd_total_imp)) +
  geom_point(aes(color = factor(cesd_category)), size = 3) +  # Scatterplot der realen Daten
  geom_smooth(data = subset(merged_data_BISQ_CESD, cesd_category == 0),
              aes(x = value, y = cesd_total_imp, color = "Depression Nein"),
              method = "lm", se = FALSE) +  # Regressionslinie für cesd_category == 0
  geom_smooth(data = subset(merged_data_BISQ_CESD, cesd_category == 1),
              aes(x = value, y = cesd_total_imp, color = "Depression Ja"),
              method = "lm", se = FALSE) +  # Regressionslinie für cesd_category == 1
  labs(x = "Schlaf in Stunden", y = "CESD Total Wert", color = "Depression Kategorie") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"))  # Farben für die Kategorien anpassen





#####Significance comparison BISQ and Acti data#####
Acti_data <- read.csv2("Acti_sleep.csv")
# Pakete laden (falls nicht installiert, erst mit install.packages() installieren)
library(dplyr)

# Die beiden Datensätze zusammenführen (inner_join behält nur übereinstimmende Werte)
merged_data_ttest <- inner_join(BISQ_sleep_total, Acti_data, 
                          by = c("maternal_id" = "maternal_id", "timepoint" = "time"))


# Beispiel: Shapiro-Wilk-Test auf Normalverteilung
shapiro_test_result <- shapiro.test(merged_data_ttest$value)

# Ergebnis ausgeben
print(shapiro_test_result)

# Beispiel: Shapiro-Wilk-Test auf Normalverteilung
shapiro_test_result <- shapiro.test(merged_data_ttest$bisq_sleeptotal_h)

# Ergebnis ausgeben
print(shapiro_test_result)

# Wilcoxon signed-rank Test für jeden Messzeitpunkt berechnen
results_wilcoxon <- merged_data_ttest %>%
  group_by(timepoint) %>%
  summarise(wilcoxon_test = list(wilcox.test(bisq_sleeptotal_h, value, paired = TRUE)))

# Extrahiere p-Werte aus den Wilcoxon-Tests
results_wilcoxon_pvalues <- merged_data_ttest %>%
  group_by(timepoint) %>%
  summarise(p_value = map_dbl(list(wilcox.test(bisq_sleeptotal_h, value, paired = TRUE)), ~ .x$p.value))

# Ergebnisse anzeigen
print(results_wilcoxon_pvalues)


#test SD
# Umstrukturierung der Daten in lange Form, einschließlich der Zeitpunkte
merged_data_long <- merged_data_ttest %>%
  pivot_longer(cols = c("value", "bisq_sleeptotal_h"), 
               names_to = "method", 
               values_to = "score")

# Überprüfen der neuen Struktur
head(merged_data_long)

#levene test für SD 
levene_test_results <- merged_data_long %>%
  group_by(timepoint) %>%
  do({
    leveneTest(score ~ method, data = .)
  })

# Ergebnisse anzeigen
print(levene_test_results)
