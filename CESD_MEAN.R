
setwd("C:/Users/janna/Documents/Masterthesis/CESD")


#My Syntax
library(readr)
library(psych)

#Read in data

cesd<- read_csv("CESD.csv", col_types = cols())

#filter for selected IDs in BISQ and Acti 
cesd <- cesd %>%
  filter(maternal_id %in% df_Acti$maternal_id)

write.csv2(cesd, file = "cesd_cvs2.csv")
write.csv2(describe(cesd), file = "cesd_descr.csv")
cesd_descr<- read_csv2("cesd_descr.csv", col_types = cols())

description <- describe(cesd)


#Recoding items

cesd[,c("cesd_4_asgood_r", "cesd_8_hopeful_r", "cesd_12_happy_r", "cesd_16_enjoyedlife_r")]<-
  3-cesd[,c("cesd_4_asgood", "cesd_8_hopeful", "cesd_12_happy", "cesd_16_enjoyedlife")]


r<-cor(cesd[,c("cesd_4_asgood", "cesd_8_hopeful", "cesd_12_happy", "cesd_16_enjoyedlife", "cesd_4_asgood_r", "cesd_8_hopeful_r", "cesd_12_happy_r", "cesd_16_enjoyedlife_r")], use = "complete.obs")
round(r,2) #check that reverse coded items now have a correlation of -1 with original item

cesd_items<-cesd[, c("cesd_1_bothered", 
                     "cesd_2_poorappetite",
                     "cesd_3_blues",
                     "cesd_4_asgood_r",
                     "cesd_5_troublefocusing",
                     "cesd_6_depressed",
                     "cesd_7_everythinganeffort",
                     "cesd_8_hopeful_r",
                     "cesd_9_failure",
                     "cesd_10_fearful",
                     "cesd_11_restlesssleep",
                     "cesd_12_happy_r",
                     "cesd_13_talkedless",
                     "cesd_14_lonely",
                     "cesd_15_peopleunfriendly",
                     "cesd_16_enjoyedlife_r",
                     "cesd_17_cryingspells",
                     "cesd_18_feltsad",
                     "cesd_19_feltdisliked",
                     "cesd_20_couldntgetgoing"
)]

cesd$cesd_count_na<-rowSums(is.na(cesd_items)) #Number missings on this scale

table(cesd$cesd_count_na)  

cesd$cesd_mean_NAex <-rowMeans(cesd_items, na.rm=F) #Mean with missings excluded

cesd$cesd_mean_imp <- ifelse(cesd$cesd_count_na <= 2, rowMeans(cesd_items, na.rm=T), NA) #Mean including rows with 10% missing or less


#Name timepoints according to redcap data 

cesd$timepoint <- cesd$redcap_event_name

cesd$timepoint[cesd$timepoint == 'm1_arm_1'] <- 'M01'
cesd$timepoint[cesd$timepoint == 'm3_arm_1'] <- 'M03'
cesd$timepoint[cesd$timepoint == 'm6_arm_1'] <- 'M06'
cesd$timepoint[cesd$timepoint == 'm9_arm_1'] <- 'M09'
cesd$timepoint[cesd$timepoint == 'm12a_arm_1'] <- 'M12'

#Create new table that only contains ID, mean and timepoint 
CESD_Mean_each_timepoint <- cesd[, c("maternal_id", "timepoint", "cesd_mean_NAex", "cesd_mean_imp")]


#Delete all NA 
CESD_selected <- na.omit(CESD_Mean_each_timepoint)

write.csv2(CESD_selected, "CESD_Mean_each_timepoint.csv ", row.names = FALSE)


#Multiply each avlue for each ID by 20 
CESD_selected <- CESD_selected %>%
  group_by(maternal_id) %>%
  mutate(cesd_total = cesd_mean_imp * 20)


#Mean and SD for Depression score 
description_CESD_total_monthly <- describeBy(CESD_selected$cesd_total, group = CESD_selected$timepoint)

description_CESD_total_monthly_df <- do.call(rbind, description_CESD_total_monthly)

# Speichern der Daten als CSV-Datei
write.csv2(description_CESD_total_monthly_df, "Description_CESD.csv", row.names = FALSE)

#Average of each ID over all timepoints times 20 (number of items)
average_scores <- CESD_selected %>%
  group_by(maternal_id) %>%
  summarise(cesd_mean_imp_avg = mean(cesd_mean_imp, na.rm = TRUE)) %>%
  mutate(cesd_total_imp = cesd_mean_imp_avg * 20)



# CutOff of CESD: 16 
# put in 1 if over 16 and 0 if under 
average_scores <- average_scores %>%
  mutate(cesd_category = ifelse(cesd_total_imp > 16, 1, 0))

##median split 
#calculate median 
median_wert <- median(average_scores$cesd_total_imp)

# Median-Split durchführen
average_scores$cesd_category_median <- ifelse(average_scores$cesd_total_imp > median_wert, 1, 0)

# count times of 1 and 0 for descrption 
table(average_scores$cesd_category)


#merge average_scores und CESD_selected data frame 
# Merge (Join) der average_scores in den ursprünglichen Datensatz
CESDdata_with_scores <- CESD_selected%>%
  left_join(average_scores, by = "maternal_id")

library(ggplot2)

ggplot(CESDdata_with_scores, aes(x = timepoint, y = cesd_total, group = maternal_id, color = as.factor(maternal_id))) +
  geom_line() +
  geom_point() +
  labs(
    x = "Timepoint",
    y = "CESD Total",
    color = "Maternal ID",
    title = "CESD Total Scores Over Time by Maternal ID"
  ) +
  theme_minimal()

#turn timepoints into factors 
CESDdata_with_scores$timepoint <- factor(CESDdata_with_scores$timepoint, levels = c("M01", "M03", "M06", "M09", "M12"))

#table for averages for each timepoint 
cesd_summary <- CESDdata_with_scores %>%
  group_by(timepoint) %>%
  summarise(
    mean_cesd = mean(cesd_total, na.rm = TRUE),
    sd_cesd = sd(cesd_total, na.rm = TRUE),
    n = n()
  )



# Ergebnisse anzeigen
print(CESDdata_with_scores)

write.csv2(CESDdata_with_scores, "CESD_data.csv ", row.names = FALSE)

# Rename die Spalte 'time' zu 'timepoint'
LMM_long_data <- LMM_long_data %>%
  rename(timepoint = time)

#merge CESD_total und mean sleep BISQ per timepoint 
df_merged <- merge(LMM_long_data, CESDdata_with_scores[, c("maternal_id", "timepoint", "cesd_total", "cesd_category")], by = c("maternal_id", "timepoint"), all = TRUE)

#plot mean sleep value and mean CESD value per timepoint per id 
library(ggplot2)

#Zusammenhang BISQ und Depression 
png("Scatterplot_BISQ_CESD.png", width = 2000, height = 1500, res = 300)
ggplot(df_merged, aes(x = cesd_total, y = value)) + 
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) + # Regressionslinie
  labs(title = "Zusammenhang zwischen Value und CESD Total",
       x = "CESD total score", y = "Sleep in hours") +
  theme_minimal()
dev.off()

df_merged <- na.omit(df_merged)#delete all rowas with NAs 
cor.test(df_merged$value, df_merged$cesd_total, use = "complete.obs")

####CESD Category####
#Save Category as factor 
df_merged$cesd_category <- as.factor(df_merged$cesd_category)

# Mean and sd dependet on category 
aggregate(value ~ cesd_category, data = df_merged, FUN = function(x) c(mean = mean(x), sd = sd(x)))

#T-test (oder ähnliches)
#Normalverteilung
shapiro.test(df_merged$value[df_merged$cesd_category == 0]) # Gruppe 0 # not present?
#normalverteuling
shapiro.test(df_merged$value[df_merged$cesd_category == 1]) # Gruppe 1 # present 
library(car)

#variance homeogenity 
leveneTest(value ~ cesd_category, data = df_merged)

#no t-test because == 0 no normalverteilung present 
wilcox.test(value ~ cesd_category, data = df_merged)

#BOXPLOT according to 1 or 2
png("Boxplot_BISQ_CESD.png", width = 3000, height = 3000, res = 300)
ggplot(df_merged, aes(x = factor(cesd_category), y = value)) +
  geom_boxplot() +
  labs(x = "CESD Category", y = "Average Sleep (h) BISQ") +
  theme_minimal()
dev.off()

# BISQ and Category 
png("gg_plot_BISQ_CESD.png", width = 3000, height = 3000, res = 300)
ggplot(df_merged, aes(x = cesd_total, y = value)) +
  geom_point(aes(color = factor(cesd_category)), size = 3) +  # Scatterplot der realen Daten
  geom_smooth(data = subset(df_merged, cesd_category == 0),
              aes(x = cesd_total, y = value, color = "Depression Nein"),
              method = "lm", se = FALSE) +  # Regressionslinie für cesd_category == 0
  geom_smooth(data = subset(df_merged, cesd_category == 1),
              aes(x = cesd_total, y = value, color = "Depression Ja"),
              method = "lm", se = FALSE) +  # Regressionslinie für cesd_category == 1
  labs(x = "CESD Total Wert", y = "Sleep average (h) BISQ", color = "Depression Kategorie") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red", "blue", "red"))  # Farben für die Kategorien und Linien anpassen
dev.off()



##Acti
#set wd 
setwd("C:/Users/janna/Documents/Masterthesis")
df_Acti <- read.csv2("output_datei_Acti.csv")
setwd("C:/Users/janna/Documents/Masterthesis/CESD")

# Rename die Spalte 'time' zu 'timepoint'
df_Acti <- df_Acti %>%
  rename(timepoint = time)

#merge CESD_total und mean sleep BISQ per timepoint 
df_merged_ACTI <- merge(df_Acti, CESDdata_with_scores[, c("maternal_id", "timepoint", "cesd_total", "cesd_category")], by = c("maternal_id", "timepoint"), all = TRUE)

df_merged_ACTI <- na.omit(df_merged_ACTI)

cor.test(df_merged_ACTI$value, df_merged_ACTI$cesd_total, use = "complete.obs")

#Zusammenhang ACTI und Depression 
png("Scatterplot_ACTI_CESD.png", width = 2000, height = 1500, res = 300)
ggplot(df_merged_ACTI, aes(x = cesd_total, y = value)) + 
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) + # Regressionslinie
  labs(title = "Zusammenhang zwischen Sleep Acti und CESD Total",
       x = "CESD total score", y = "Sleep in hours") +
  theme_minimal()
dev.off()

## Mean and sd dependet on category 
aggregate(value ~ cesd_category, data = df_merged_ACTI, FUN = function(x) c(mean = mean(x), sd = sd(x)))

###BOXPLOT according to 1 or 2
png("Boxplot_ACTI_CESD.png", width = 2000, height = 1500, res = 300)
ggplot(df_merged_ACTI, aes(x = factor(cesd_category), y = value)) +
  geom_boxplot() +
  labs(x = "CESD Category", y = "Average Sleep (h) Actigraphy") +
  theme_minimal()
dev.off()

#Plot cateorical 
# Scatterplot mit Regressionslinien für cesd_category 0 und 1
ggplot(df_merged_ACTI, aes(x = cesd_total, y = value)) +  # Achsen vertauscht
  geom_point(aes(color = factor(cesd_category)), size = 3) +  # Scatterplot der realen Daten
  geom_smooth(data = subset(df_merged_ACTI, cesd_category == 0),
              aes(x = cesd_total, y = value, color = "Depression Nein"),
              method = "lm", se = FALSE) +  # Regressionslinie für cesd_category == 0
  geom_smooth(data = subset(df_merged_ACTI, cesd_category == 1),
              aes(x = cesd_total, y = value, color = "Depression Ja"),
              method = "lm", se = FALSE) +  # Regressionslinie für cesd_category == 1
  labs(x = "CESD Total Wert", y = "Schlaf in Stunden", color = "Depression Kategorie") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red", "blue", "red"))  # Farben für die Kategorien anpassen

