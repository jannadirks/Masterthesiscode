
#set working directory 
setwd("C:/Users/janna/Documents/Masterthesis/CESD")

#load packages 
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


#Recoding inverted items

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


#rename timepoints according to redcap data 

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
#save this table 
write.csv2(CESD_selected, "CESD_Mean_each_timepoint.csv ", row.names = FALSE)

#Mean and SD for Depression score 
description_CESD_total_monthly <- describeBy(CESD_selected$cesd_total, group = CESD_selected$timepoint)

description_CESD_total_monthly_df <- do.call(rbind, description_CESD_total_monthly)

# save as table 
write.csv2(description_CESD_total_monthly_df, "Description_CESD.csv", row.names = FALSE)

#Average of each ID over all timepoints times 20 (number of items)
average_scores <- CESD_selected %>%
  group_by(maternal_id) %>%
  summarise(cesd_mean_imp_avg = mean(cesd_mean_imp, na.rm = TRUE)) %>%
  mutate(cesd_total_imp = cesd_mean_imp_avg * 20)

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

#plot the CESD Data in a spagetti plot for each ID 
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






