#Broken Stick Model 

#Clear working environment
rm(list = ls())

#set wd 
setwd("C:/Users/janna/Documents/Masterthesis/Abweichungen")

#Read in data
df<- read.table('Acti_Average_withDates.csv', header = TRUE, sep = ';')
Actidates <- data.frame(df$Maternal_ID, df$Date_M1_Acti,df$Date_M3_Acti, df$Date_M6_Acti, df$Date_M9_Acti, df$Date_M12)
df2 <- read.table("Daten_ACTI.csv", header = TRUE, sep = ';')
df2 <- df2[-nrow(df2), ]
Actidates$birth <- df2$birth
Actidates[Actidates == 0] <- NA
# Umbenennen der Spalten
colnames(Actidates) <- c("maternal_id", "ACTI_Date_M1", "ACTI_Date_M3", "ACTI_Date_M6", "ACTI_Date_M9", "ACTI_Date_M12", "birth")


# Schritt 1: Umwandeln der Spalte in ein Datumsobjekt (vorausgesetzt, die Spalte ist im Format "YYYY-MM-DD")
Actidates$birth <- as.Date(Actidates$birth, "%d.%m.%Y")
Actidates$ACTI_Date_M1 <- as.Date(Actidates$ACTI_Date_M1, "%d.%m.%Y")
Actidates$ACTI_Date_M3 <- as.Date(Actidates$ACTI_Date_M3, "%d.%m.%Y")
Actidates$ACTI_Date_M6 <- as.Date(Actidates$ACTI_Date_M6, "%d.%m.%Y")
Actidates$ACTI_Date_M9 <- as.Date(Actidates$ACTI_Date_M9, "%d.%m.%Y")
Actidates$ACTI_Date_M12 <- as.Date(Actidates$ACTI_Date_M12, "%d.%m.%Y")


#abziehen voneinander 
#Actidates$diffbirth <- Actidates$birth - Actidates$birth
Actidates$diffM1 <- Actidates$ACTI_Date_M1 - Actidates$birth
Actidates$diffM3 <- Actidates$ACTI_Date_M3 - Actidates$birth
Actidates$diffM6 <- Actidates$ACTI_Date_M6 - Actidates$birth
Actidates$diffM9 <- Actidates$ACTI_Date_M9 - Actidates$birth
Actidates$diffM12 <- Actidates$ACTI_Date_M12 - Actidates$birth


library(dplyr)
library(tidyr)

# Konvertieren Sie die Zeitdifferenzen in numerische Werte und entfernen Sie die "days"
convert_to_days <- function(x) {
  as.numeric(gsub(" days", "", x))
}

# Die Zeitdifferenzen in numerische Werte umwandeln und in langes Format bringen
long_Actidates <- Actidates %>%
  mutate(across(starts_with("diff"), ~ replace(., is.na(.), NA))) %>%
  pivot_longer(cols = starts_with("diff"), 
               names_to = "diff_type", 
               values_to = "daysA") %>%
  mutate(days = as.numeric(daysA))  # Konvertieren in numerisch

long_Actidates$maternal_id <- factor(long_Actidates$maternal_id)
long_Actidates_clean <- na.omit(long_Actidates)#delete NAs

library(ggplot2)

# Abacus-Plot erstellen
ggplot(long_Actidates, aes(x = daysA , y = maternal_id)) +
  geom_point(color = "red") + 
  labs(title = "Deviation of Actigraphy Data", 
       x = "Time since Birth in Months", y = "Maternal ID") +
  geom_vline(xintercept = c(30, 90, 180, 270, 360),  # Zeitpunkte für vertikale Linien
             linetype = "solid",                     # Linienstil
             size = 0.5,                            # Linienstärke
             color = "black") +  
  scale_x_continuous(limits = c(0, 470),
                     breaks = c(0, 30, 90, 180, 270, 360),
                     labels = c("B","M1", "M3", "M6", "M9", "M12")) +
  theme_minimal() +  # Einfaches und sauberes Thema
  theme(axis.text.y = element_text(size = 12),  # Adjust y-axis text size
        plot.title = element_text(size = 16, hjust = 0.5))  # Center the title and adjust size

ggsave("abacus_plot_ACTIS.png", width = 10, height = 15)


### BISQ DATES 

#Read in data
BISQdates<- read.table('BQIS_DATES_selected.csv', header = TRUE, sep = ';')

# Umwandeln ins Wide Format
BISQ_wide <- BISQdates %>%
  pivot_wider(names_from = redcap_event_name, values_from = bisq_date)

BISQ_wide$birth <- Actidates$birth
# Spaltennamen umbenennen: "m1_arm_1" zu "M1", "m3_arm_1" zu "M3" usw.
colnames(BISQ_wide) <- gsub(pattern = "m(\\d+)_arm_1", replacement = "M\\1", colnames(BISQ_wide))
# Spalte "m12a_arm_1" in "M12" umbenennen
colnames(BISQ_wide)[colnames(BISQ_wide) == "m12a_arm_1"] <- "M12"

#Umwandeln der Spalte in ein Datumsobjekt 
BISQ_wide$birth <- as.Date(BISQ_wide$birth, "%d.%m.%Y")
BISQ_wide$M1 <- as.Date(BISQ_wide$M1, "%d.%m.%Y")
BISQ_wide$M3 <- as.Date(BISQ_wide$M3, "%d.%m.%Y")
BISQ_wide$M6 <- as.Date(BISQ_wide$M6, "%d.%m.%Y")
BISQ_wide$M9 <- as.Date(BISQ_wide$M9, "%d.%m.%Y")
BISQ_wide$M12 <- as.Date(BISQ_wide$M12, "%d.%m.%Y")

#abziehen voneinander 
#BISQ_wide$diffbirth <- BISQ_wide$birth - BISQ_wide$birth
BISQ_wide$diffM1 <- BISQ_wide$M1 - BISQ_wide$birth
BISQ_wide$diffM3 <- BISQ_wide$M3 - BISQ_wide$birth
BISQ_wide$diffM6 <- BISQ_wide$M6 - BISQ_wide$birth
BISQ_wide$diffM9 <- BISQ_wide$M9 - BISQ_wide$birth
BISQ_wide$diffM12 <- BISQ_wide$M12 - BISQ_wide$birth


# pivot to long format, Turn difference in days into numeric value 
long_BISQ <- BISQ_wide %>%
  mutate(across(starts_with("diff"), ~ replace(., is.na(.), NA))) %>%
  pivot_longer(cols = starts_with("diff"), 
               names_to = "diff_type", 
               values_to = "daysF") %>%
  mutate(days = as.numeric(daysF))  # Konvertieren in numerisch

long_BISQ$maternal_id <- factor(long_BISQ$maternal_id)


# Abacus-Plot erstellen
ggplot(long_BISQ, aes(x = daysF , y = maternal_id)) +
  geom_point(color = "blue") + 
  labs(title = "Deviation of BISQ Data", 
       x = "Time since Birth in Months", y = "Maternal ID") +
  geom_vline(xintercept = c(30, 90, 180, 270, 360),  # Zeitpunkte für vertikale Linien
             linetype = "solid",                     # Linienstil
             size = 0.5,                            # Linienstärke
             color = "black") +  
  scale_x_continuous(limits = c(0, 470),
                     breaks = c(0, 30, 90, 180, 270, 360),
                     labels = c("B","M1", "M3", "M6", "M9", "M12")) +
  theme_minimal() +  # Einfaches und sauberes Thema
  theme(axis.text.y = element_text(size = 12),  # Adjust y-axis text size
        plot.title = element_text(size = 16, hjust = 0.5))  # Center the title and adjust size


#Neuer Datensatz mit DIFF von beiden drin 
colnames(long_BISQ)[colnames(long_BISQ) == "diff_type"] <- "diff_typeF"
colnames(long_Actidates)[colnames(long_Actidates) == "diff_type"] <- "diff_typeA"


# Neue Daten zusammenfassen
neuer_datensatz <- long_BISQ %>%
  select(maternal_id, diff_typeF, daysF)%>%
  bind_cols(long_Actidates %>% select(diff_typeA, daysA))


# Abacus-Plot erstellen
# Abacuss-Plot erstellen
ggplot(neuer_datensatz, aes(y = maternal_id)) +
  geom_point(aes(x = daysF, color = "Date BISQ"), position = position_jitter(width = 0.1), size = 1.25) +
  geom_point(aes(x = daysA, color = "Date Acigraphy"), position = position_jitter(width = 0.1), size = 1.25) +
  geom_vline(xintercept = c(30, 90, 180, 270, 360),  # Zeitpunkte für vertikale Linien
             linetype = "solid",                     # Linienstil
             size = 0.5,                            # Linienstärke
             color = "black") +       
  scale_color_manual(values = c("Date BISQ" = "blue", "Date Acigraphy" = "red")) +
  scale_x_continuous(limits = c(0, 470),
                     breaks = c(0, 30, 90, 180, 270, 360),
                     labels = c("B","M1", "M3", "M6", "M9", "M12")) +
  labs(title = "Deviation Actirgaphy and Deviation BISQ combined",
       y = "Maternal ID",
       x = "Time since birth in months",
       color = "Variablen") +
  theme_minimal() +
  theme(legend.position = "top")

ggsave("Abweichungen.png", plot = last_plot(), width = 6, height = 5, dpi = 300)

write.csv2(neuer_datensatz, "Collection_Dates.csv")

