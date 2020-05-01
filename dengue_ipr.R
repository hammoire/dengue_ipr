# Analysis, tables and figures for IPR ------------------------------------
#Load install/load libraries if needed
# install.packages("tidyverse")
# install.packages("pheatmap")
# install.packages("RColorBrewer")
library(tidyverse)
library(pheatmap)
library(RColorBrewer)

#Ensure file path is set correctly
ipr_form <- read_csv("~/Desktop/ipr_form.csv")
ipr_symptom <- read_csv("~/Desktop/ipr_symptom.csv")
ipr_matrix_imputed <- read_csv("~/Desktop/ipr_matrix_imputed.csv")


# Table 1 -----------------------------------------------------------------
all_part <- {ipr_form %>% 
    group_by(case_name, first_symptom, age, sex, hospitalized, index_contact, febrile, symptomatic, diagnosis) %>% 
    summarise(n = n(),
              day_capture = first(day_illness),
              fever = any(fever_yn, na.rm = TRUE),
              headache =  any(headache_yn >6, na.rm = TRUE),
              retro_orbital = any(retroorbital_yn, na.rm = TRUE),
              nau_vom = any(nausea_yn|vomiting_yn, na.rm = TRUE),
              musc_join = any(musclepain_yn|jointpain_yn, na.rm = TRUE),
              rash = any(petechiae_yn|purpura_yn|maculopaprash_yn|erysipelas_yn, na.rm = TRUE),
              abdominalpain_severe = any(abdompain_intensity > 6, na.rm = TRUE),
              hemetem_severe = any(hematemesis_yn, na.rm = TRUE),
              gumbleed_severe = any(gumbleed_yn, na.rm = TRUE),
              vomiting_severe = any(vomiting_frequency >= 6, na.rm = TRUE)) %>% 
    mutate(dengue = fever & (headache + retro_orbital + nau_vom + musc_join + rash) >= 2,
           warning = abdominalpain_severe + hemetem_severe + gumbleed_severe + vomiting_severe > 0) %>% 
    ungroup() %>% 
    summarise(`Number of cases` = n(),
              `Number of surveys` = sum(n),
              `Sex (%)` = "",
              Male = str_c(sum(sex == "Male"), " (", 100*round(sum(sex == "Male")/n(), digits = 2), ")"),
              Female = str_c(sum(sex == "Female"), " (", 100*round(sum(sex == "Female")/n(), digits = 2), ")"),
              `Median age (IQR)` = str_c(round(median(age), digits = 2), " (", quantile(age, 0.25), "-", quantile(age, 0.75),")"),
              `Day at diagnosis (IQR)` = str_c(round(median(day_capture, na.rm = TRUE), digits = 2), " (", quantile(day_capture, 0.25, na.rm = TRUE), "-", quantile(day_capture, 0.75, na.rm = TRUE),")"),
              `Serotype (%)` = "",
              DEN2 = str_c(sum(diagnosis == "deng2"), " (", 100*round(sum(diagnosis == "deng2")/n(), digits = 2), ")"),
              DEN3 = str_c(sum(diagnosis == "deng3"), " (", 100*round(sum(diagnosis == "deng3")/n(), digits = 2), ")"),
              `WHO dengue criteria (%)` = str_c(sum(dengue), " (", 100*round(sum(dengue)/n(), digits = 2), ")"),
              `Warning signs (%)` = str_c(sum(warning), " (", 100*round(sum(warning)/n(), digits = 2), ")"),
              `Hospitalized (%)` = str_c(sum(hospitalized), " (", 100*round(sum(hospitalized)/n(), digits = 2), ")")) %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(Total = V1) %>% 
    mutate(Characteristic = row.names(.)) %>% 
    select(Characteristic, everything()) }

index_contact_part <- {ipr_form %>% 
    group_by(case_name, first_symptom, age, sex, hospitalized, index_contact, febrile, symptomatic, diagnosis) %>% 
    summarise(n = n(),
              day_capture = first(day_illness),
              fever = any(fever_yn, na.rm = TRUE),
              headache =  any(headache_yn >6, na.rm = TRUE),
              retro_orbital = any(retroorbital_yn, na.rm = TRUE),
              nau_vom = any(nausea_yn|vomiting_yn, na.rm = TRUE),
              musc_join = any(musclepain_yn|jointpain_yn, na.rm = TRUE),
              rash = any(petechiae_yn|purpura_yn|maculopaprash_yn|erysipelas_yn, na.rm = TRUE),
              abdominalpain_severe = any(abdompain_intensity > 6, na.rm = TRUE),
              hemetem_severe = any(hematemesis_yn, na.rm = TRUE),
              gumbleed_severe = any(gumbleed_yn, na.rm = TRUE),
              vomiting_severe = any(vomiting_frequency >= 6, na.rm = TRUE)) %>% 
    mutate(dengue = fever & (headache + retro_orbital + nau_vom + musc_join + rash) >= 2,
           warning = abdominalpain_severe + hemetem_severe + gumbleed_severe + vomiting_severe > 0) %>% 
    ungroup() %>% 
    group_by(index_contact) %>% 
    summarise(`Number of cases` = n(),
              `Number of surveys` = sum(n),
              `Sex (%)` = "",
              Male = str_c(sum(sex == "Male"), " (", 100*round(sum(sex == "Male")/n(), digits = 2), ")"),
              Female = str_c(sum(sex == "Female"), " (", 100*round(sum(sex == "Female")/n(), digits = 2), ")"),
              `Median age (IQR)` = str_c(round(median(age), digits = 2), " (", quantile(age, 0.25), "-", quantile(age, 0.75),")"),
              `Day at diagnosis (IQR)` = str_c(round(median(day_capture, na.rm = TRUE), digits = 2), " (", quantile(day_capture, 0.25, na.rm = TRUE), "-", quantile(day_capture, 0.75, na.rm = TRUE),")"),
              `Serotype (%)` = "",
              DEN2 = str_c(sum(diagnosis == "deng2"), " (", 100*round(sum(diagnosis == "deng2")/n(), digits = 2), ")"),
              DEN3 = str_c(sum(diagnosis == "deng3"), " (", 100*round(sum(diagnosis == "deng3")/n(), digits = 2), ")"),
              `WHO dengue criteria (%)` = str_c(sum(dengue), " (", 100*round(sum(dengue)/n(), digits = 2), ")"),
              `Warning signs (%)` = str_c(sum(warning), " (", 100*round(sum(warning)/n(), digits = 2), ")"),
              `Hospitalized (%)` = str_c(sum(hospitalized), " (", 100*round(sum(hospitalized)/n(), digits = 2), ")")) %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(Contact = V1, Index = V2) %>% 
    mutate(Characteristic = row.names(.)) %>% 
    select(Index, Contact)}
bind_cols(all_part, index_contact_part[-1,])


# Appendix. Figure 1 ----------------------------------------------------------------

colors_eid <- c("#FF7F00", "#E41A1C", "#377EB8", "#F781BF", "#4DAF4A", "#FFFF33", "#984EA3", "#A65628", "#999999")

index_contact_df <- {ipr_symptom %>%
#Modify symptoms for plot labels
  mutate(symptom = case_when(symptom == "abdominalpain" ~ "abdominal pain",
                             symptom == "bodypain" ~ "body pain",
                             symptom == "bonepain" ~ "bone pain",
                             symptom == "jointpain" ~ "joint pain",
                             symptom == "musclepain" ~ "muscle pain",
                             symptom == "chestpain" ~ "chest pain",
                             symptom == "retroorbital" ~ "retro-orbital pain",
                             symptom == "badtaste" ~ "bad taste",
                             symptom == "maculopaprash" ~ "macularpap rash",
                             symptom == "gumbleed" ~ "gum bleed",
                             symptom == "nasalbleed" ~ "nasal bleed",
                             symptom == "vaginalbleed" ~ "vaginal bleed",
                             symptom == "urinebleed" ~ "urine bleed",
                             symptom == "sorethroat" ~ "sore throat",
                             TRUE ~ symptom)) %>% 
  group_by(study_code, group, symptom, index_contact) %>%
  summarise(present = any(present, na.rm = TRUE)) }

#Define number of females to modify vaginal bleed denominator to take account of only F
fems <- sum(ipr_form %>% select(case_name, sex) %>% distinct() %>% pull(sex) == "Female")

# Figure 1
index_contact_df %>% 
  group_by(group, symptom) %>%
  summarise(num_parts = length(unique(study_code)), symptom_freq = sum(present, na.rm = TRUE)) %>%
  mutate(num_parts = ifelse(symptom == "vaginalbleed", fems, num_parts),
         pc = 100*(symptom_freq/num_parts)) %>% 
  arrange(desc(pc)) %>% 
  ggplot(aes(x = fct_reorder(symptom, -symptom_freq), y = pc, fill = group)) +
  geom_col(col = "black", size = 0.25) +
  # geom_text(aes(label = round(pc, 0)), vjust = -1.5, size = 2.5) +
  labs(x = "Symptom",
       y = "%",
       fill = "Group") +
  scale_y_continuous(limits = c(0, 103)) +
  scale_fill_manual(values = colors_eid, breaks = c("constitutional","fever", "headache","musculoskeletal", "abdominal", "other", "cutaneous", "respiratory", "bleeding")) +
  theme_bw() +
  theme(panel.background = element_rect(size = 1000),
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = 0.375),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(vjust = -0.5, size = 12),
        axis.title.y = element_text(vjust = 1, size = 12),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(0.25,0.25,0.5,0.25),"cm"),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.background = element_blank(),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.4, "cm"))




# Appendix Figure 2 ----------------------------------------------------------------

#Symptom duration
yn_vars <- {c("abdominal_yn", "anorexia_yn_imputed", "badtaste_yn_imputed", 
              "bodypain_yn", "bonepain_yn", "chestpain_yn_imputed", "chills_yn", 
              "congestion_yn_imputed", "cough_yn_imputed", "diarrhea_yn_imputed", 
              "earache_yn_imputed", "erysipelas_yn_imputed", "faint_yn_imputed", 
              "fever_yn", "gumbleed_yn_imputed", "headache_yn", "hematemesis_yn_imputed", 
              "itch_yn_imputed", "jaundice_yn_imputed", "jointpain_yn", "maculopaprash_yn_imputed", 
              "malaise_yn_imputed", "melena_yn_imputed", "musclepain_yn", "nasalbleed_yn_imputed", 
              "nausea_yn_imputed", "petechiae_yn_imputed", "photophobia_yn_imputed", 
              "purpura_yn_imputed", "retroorbital_yn", "sorethroat_yn", "sputum_yn_imputed", 
              "urinebleed_yn_imputed", "vaginalbleed_yn_imputed", "vomiting_yn_imputed", 
              "weakness_yn")}


#Issues with this data:
#For non intensity symptoms and sorethroat the exact start date of symptoms is not known. 
#These yn variables have therefore been imputed. 
#Derving a probability of having the symptom on that day
#2 ways to generate duration data 1. sum all probabilities 2. State that they had symptom only if probab >= 0.5
#In the end the distributions look basically the same


#Plot of symptom durations (all symptoms)
all_duration_plot <- ipr_matrix_imputed %>% 
  select(study_code, contains("_yn_impute")) %>% 
  group_by(study_code) %>% 
  summarise_if(is.logical, sum) %>% 
  gather(symptom, duration, -study_code) %>% 
  group_by(study_code) %>% 
  summarise(max_duration = max(duration)) %>% 
  group_by(max_duration) %>% 
  count() %>% 
  mutate(title = "All symptoms") %>%
  ggplot(aes(x = max_duration, y = n)) +
  geom_col(fill = "#f0f0f0", color = "black", size = 0.25) +
  facet_wrap(~title) +
  scale_x_continuous(breaks = seq(from = 0, to = 13, by = 2)) +
  scale_y_continuous(breaks = seq(from = 0, to = 14, by = 2)) +
  labs(x = "Duration (days)",
       y = "No. participants") +
  theme_bw() +
  theme(axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        strip.text = element_text(size = 13),
        strip.background = element_rect(fill = "white", size = 0.25),
        panel.border  = element_rect(size = 0.25))

#Plot of individual symptoms durations
individual_vars <- c("study_code","Fever" = "fever_yn_imputed",  
                     "Headache" = "headache_yn_imputed", 
                     "Body pain" = "bodypain_yn_imputed", 
                     "Abdominal pain" = "abdominal_yn_imputed")



indiv_duration_df <- ipr_matrix_imputed %>% 
  select(study_code, individual_vars) %>% 
  group_by(study_code) %>% 
  summarise_if(is.logical, sum) %>% 
  gather(symptom, duration, -study_code) %>% 
  mutate(symptom = factor(symptom, levels = c("Fever", "Headache", "Body pain", "Abdominal pain"))) %>% 
  group_by(duration, symptom) %>% 
  count() 

indiv_duration_plot <- ggplot(indiv_duration_df, aes(x = duration, y= n)) +
  geom_col(color = "white") +
  facet_wrap(~symptom) +
  scale_x_continuous(breaks = seq(from = 0, to = 13, by = 2)) +
  labs(x = "Duration (days)",
       y = "No. participants") +
  theme_bw() +
  theme(axis.title.x  = element_text(size = 12),
        axis.title.y  = element_blank(),
        strip.text = element_text(size = 12),
        strip.background = element_rect(fill = "white", size = 0.25),
        panel.border  = element_rect(size = 0.25)) +
  geom_col(data = indiv_duration_df %>% filter(symptom == "Fever"), fill = "#4DAF4A", size = 0.15, color = "black") +
  geom_col(data = indiv_duration_df %>% filter(symptom == "Abdominal pain"), fill = "#FF7F00", size = 0.15, color = "black") +
  geom_col(data = indiv_duration_df %>% filter(symptom == "Body pain"), fill = "#984EA3", size = 0.15, color = "black") + 
  geom_col(data = indiv_duration_df %>% filter(symptom == "Headache"), fill = "#FFFF33", size = 0.15, color = "black")

#Combine into a single plot
cowplot::plot_grid(all_duration_plot, indiv_duration_plot, align = 'h', axis = 'tb')




# Main text Figure 1 ----------------------------------------------------------------

#Partial
heatmap_partial_vars <-  c("malaise_yn_imputed", "fever_yn_imputed", 
                           "chills_yn_imputed", "weakness_yn_imputed", 
                           "headache_yn_imputed", "retroorbital_yn_imputed", 
                           "bodypain_yn_imputed", "bonepain_yn_imputed", 
                           "jointpain_yn_imputed", "musclepain_yn_imputed", 
                           "abdominal_yn_imputed", "vomiting_yn_imputed", 
                           "diarrhea_yn_imputed")

ipr_matrix_imputed %>% 
  select(study_code, day_illness, heatmap_partial_vars) %>% 
  mutate(day_illness = as.integer(day_illness)) %>% 
  select(study_code, day_illness, contains("_yn")) %>% 
  gather(symptom, present, -study_code, -day_illness) %>% 
  mutate(symptom = str_replace(symptom, "_yn_imputed", "")) %>%
  mutate(symptom = case_when(symptom == "abdominal" ~ "abdominal pain",
                             symptom == "bodypain" ~ "body pain",
                             symptom == "bonepain" ~ "bone pain",
                             symptom == "jointpain" ~ "joint pain",
                             symptom == "musclepain" ~ "muscle pain",
                             symptom == "retroorbital" ~ "retro-orbital pain",
                             TRUE ~ symptom)) %>% 
  group_by(symptom) %>% 
  mutate(all = sum(present)) %>% 
  group_by(day_illness, symptom) %>% 
  summarise(num_part = n(), prop = mean(present), num_present = sum(present), all = first(all)) %>% 
  ggplot(aes(x = factor(day_illness), y = fct_reorder(symptom, all), fill = prop)) +
  geom_raster() +
  geom_text(aes(label = num_present), size = 2.5) +
  scale_fill_distiller(palette = "Spectral") +
  labs(x = "Day of illness",
       fill = "Proportion") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 10, size = 20), 
        axis.title.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.ticks = element_blank(),
        plot.background = element_rect(size = 100), 
        panel.background = element_blank()) +
  coord_cartesian(clip = "off")


# Main text Figure 2 ----------------------------------------------------------------

#Create data frame for labels
max_intensity_label <- ipr_symptom %>% 
  filter(!is.na(intensity)) %>% 
  group_by(study_code, symptom) %>% 
  summarise(any = any(present)) %>% 
  group_by(symptom) %>% 
  summarise(num = sum(any), pc = round(100*(num/n()), digits = 1)) %>% 
  mutate(symptom = case_when(symptom == "abdominalpain" ~ "abdominal pain",
                             symptom == "bodypain" ~ "body pain",
                             symptom == "bonepain" ~ "bone pain",
                             symptom == "bodypain" ~ "body pain",
                             symptom == "jointpain" ~ "joint pain",
                             symptom == "musclepain" ~ "muscle pain",
                             symptom == "retroorbital" ~ "retro-orbital pain",
                             symptom == "sorethroat" ~ "sore throat",
                             TRUE ~ symptom),
         symptom = factor(symptom, levels = c("malaise", "weakness", "fever", "chills", "headache", "retro-orbital pain",
                                              "body pain", "bone pain", "muscle pain", "joint pain","abdominal pain", "sore throat")))

max_intensity_df <- ipr_symptom %>% 
  filter(!is.na(intensity), intensity > 0) %>% 
  group_by(study_code, symptom, group) %>% 
  summarise(max_intensity = max(intensity, na.rm = TRUE)) %>% 
  group_by(max_intensity, symptom, group) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(symptom = case_when(symptom == "abdominalpain" ~ "abdominal pain",
                             symptom == "bodypain" ~ "body pain",
                             symptom == "bonepain" ~ "bone pain",
                             symptom == "bodypain" ~ "body pain",
                             symptom == "jointpain" ~ "joint pain",
                             symptom == "musclepain" ~ "muscle pain",
                             symptom == "retroorbital" ~ "retro-orbital pain",
                             symptom == "sorethroat" ~ "sore throat",
                             TRUE ~ symptom),
         symptom = factor(symptom, levels = c("malaise", "weakness", "fever", "chills", "headache", "retro-orbital pain",
                                              "body pain", "bone pain", "muscle pain", "joint pain","abdominal pain", "sore throat"))) 


ggplot(max_intensity_df, aes(x = max_intensity, y = n)) +
  geom_col(data = max_intensity_df %>% filter(symptom == "abdominal pain"), fill = "#FF7F00", col = "black", size = 0.1) +
  geom_col(data = max_intensity_df %>% filter(symptom == "body pain"), fill = "#984EA3", col = "black", size = 0.1) +
  geom_col(data = max_intensity_df %>% filter(symptom == "bone pain"), fill = "#984EA3", col = "black", size = 0.1) +
  geom_col(data = max_intensity_df %>% filter(symptom == "chills"), fill = "#4DAF4A", col = "black", size = 0.1) +
  geom_col(data = max_intensity_df %>% filter(symptom == "fever"), fill = "#4DAF4A", col = "black", size = 0.1) +
  geom_col(data = max_intensity_df %>% filter(symptom == "headache"), fill = "#FFFF33", col = "black", size = 0.1) +
  geom_col(data = max_intensity_df %>% filter(symptom == "joint pain"), fill = "#984EA3", col = "black", size = 0.1) +
  geom_col(data = max_intensity_df %>% filter(symptom == "malaise"), fill = "#377EB8", col = "black", size = 0.1) +
  geom_col(data = max_intensity_df %>% filter(symptom == "muscle pain"), fill = "#984EA3", col = "black", size = 0.1) +
  geom_col(data = max_intensity_df %>% filter(symptom == "retro-orbital pain"), fill = "#FFFF33", col = "black", size = 0.1) +
  geom_col(data = max_intensity_df %>% filter(symptom == "sore throat"), fill = "#A65628", col = "black", size = 0.1) +
  geom_col(data = max_intensity_df %>% filter(symptom == "weakness"), fill = "#377EB8", col = "black", size = 0.1) +
  geom_text(data = max_intensity_label,  aes(label = str_c("total: ", num, " (", pc, "%)"), x = 5, y = 20), size = 3) +
  facet_wrap(~symptom) +
  scale_x_continuous(breaks = 0:10) +
  scale_y_continuous(breaks = seq(5, 20, 5)) +
  labs(x = "Max reported intensity",
       y = "No. participants") +
  theme_bw() +
  theme(axis.title  = element_text(size = 14),
        strip.text = element_text(size = 13),
        strip.background = element_rect(fill = "white", size = 0.25),
        panel.border  = element_rect(size = 0.25))




# Main text Figure 3 ----------------------------------------------------------------

traject_vars <-  c("malaise_intensity_reordered_imputed", "fever_intensity_reordered_imputed", 
                   "weakness_intensity_reordered_imputed", "headache_intensity_reordered_imputed",  
                   "bodypain_intensity_reordered_imputed",  "abdominal_intensity_reordered_imputed")



sample_traj <- ipr_matrix_imputed %>% 
  select(study_code, day_illness, traject_vars) %>%
  gather(symptom, intensity, -study_code, -day_illness) %>% 
  mutate(symptom = str_replace(symptom, "_.*", ""),
         symptom = case_when(symptom == "abdominal" ~ "abdominal pain",
                             symptom == "bodypain" ~ "body pain",
                             TRUE ~ symptom),
         symptom = factor(symptom, levels = c("malaise","weakness",  "fever", "headache", "body pain", "abdominal pain"))) %>% 
  #This line randonmly selects 10 participants. These intensity trajectories are then displayed.
  #Using all particpants is a mess
  filter(study_code %in% sample(unique(ipr_matrix_imputed$study_code), size = 10, replace = FALSE))

ipr_matrix_imputed %>% 
  select(study_code, day_illness, traject_vars) %>%
  gather(symptom, intensity, -study_code, -day_illness) %>% 
  mutate(symptom = str_replace(symptom, "_.*", ""),
         symptom = case_when(symptom == "abdominal" ~ "abdominal pain",
                             symptom == "bodypain" ~ "body pain",
                             TRUE ~ symptom),
         symptom = factor(symptom, levels = c("malaise","weakness",  "fever", "headache", "body pain", "abdominal pain"))) %>% 
  ggplot(aes(x = factor(day_illness), y = intensity)) +
  geom_boxplot(outlier.size = 0.25) +
  geom_line(data = sample_traj,
            aes(x = factor(day_illness), 
                y = intensity, 
                color = study_code, 
                group = factor(study_code)), alpha = 0.6, size = 0.5) +
  facet_wrap(~symptom, nrow = 2) +
  scale_y_continuous(breaks = seq(from = 0, to = 10, by = 2)) +
  scale_x_discrete(breaks = seq(from = 0, to = 14, by = 2)) +
  labs(x = "Day of illness",
       y = "Intensity (0-10)") +
  theme_bw() +
  theme(axis.title  = element_text(size = 14),
        strip.text = element_text(size = 13),
        strip.background = element_rect(fill = "white", size = 0.25),
        panel.border  = element_rect(size = 0.25),
        legend.position="none") 

# Main text Figure 4 ----------------------------------------------------------------
#Symptom intensity correlation matrix
intensity_matrix <- ipr_matrix_imputed %>% 
  ungroup() %>% 
  select(ends_with("intensity")) %>%
  rename_if(is.numeric, list(~str_replace(., "_intensity", ""))) %>%
  rename("abdominal pain" = "abdominal",
         "body pain" = "bodypain",
         "muscle pain" = "musclepain",
         "bone pain" = "bonepain",
         "joint pain" = "jointpain",
         "retro-orbital pain" = "retroorbital") %>% 
  as.matrix() %>% 
  cor(use = "pairwise.complete.obs", method = "spearman") 

pheatmap(intensity_matrix, color = colorRampPalette(rev(brewer.pal(n = 7, name = "YlOrRd")))(100)[100:1],
                            display_numbers = TRUE, number_color = "black", fontsize = 11,
                            border_color = "white", clustering_method = "single",
                            cellwidth = 30, cellheight = 30, treeheight_col = 80, treeheight_row = 0,
                            angle_col = "270")


# Models and table 2 ------------------------------------------------------
# 1. Logistic regression model: response var = major_activity_change (T/F), 
                            #explanatory var = each symptom intensity (age and sex as covariates)

#Define intensity vars for model
int_vars <- str_subset(names(ipr_matrix_imputed), "intensity\\b") #keep intensity vars
#Define yn vars for model (used in part 2)
yn_vars <- str_c(str_replace(int_vars, "_intensity", ""), "_yn") #keep yn vars

#Dateframe for models  
ipr_model <- ipr_matrix_imputed %>% 
  ungroup() %>% 
  mutate(major_activity_change = activity_change_numeric > 1) %>% #Create new binary outcome variable
  select(major_activity_change, age, sex, int_vars, yn_vars) %>% 
  filter(!is.na(major_activity_change))

#Replace the occasional NA (in all these exmaples intensity was 0 so replace with FALSE)
ipr_model[16:27] <- map(ipr_model[16:27], function(x){
  x[is.na(x)] <- FALSE
  x
})


# Create function that can be used to iterate over all symptoms and apply LR model
act_change_intensity_mods <- function(x, df) {
  int <- x
  df <- df %>% 
    select(major_activity_change, int = int, age, sex)
  # Create LR models --------------------------------------------------------
  mod_int <- glm(major_activity_change ~ int + age + sex, data = df, family = binomial) 
  or <- exp(coef(mod_int))[2]
  ci <- exp(confint(mod_int, level = 0.99))
  t(ci)
  ci_low <- ci[2]
  ci_high <- ci[6]
  symptom = str_replace(x, "_intensity", "")
  aic <- AIC(mod_int)
  data.frame(symptom = symptom, OR = or, CI_2.5 = ci_low,  CI_97.5 = ci_high)
  
}

#Iterate over each symptom
int_v_activity <- map_df(int_vars, act_change_intensity_mods, ipr_model) %>% 
  arrange(desc(OR)) %>% 
  mutate(symptom = case_when(symptom == "abdominal" ~ "abdo pain",
                             symptom == "bodypain" ~ "body pain",
                             symptom == "bonepain" ~ "bone pain",
                             symptom == "jointpain" ~ "joint pain",
                             symptom == "musclepain" ~ "muscle pain",
                             symptom == "retroorbital" ~ "retroorb pain",
                             TRUE ~ symptom),
         symptom = factor(symptom, levels = c("malaise", "weakness", "fever", "chills", "headache", "retroorb pain",
                                              "body pain", "bone pain", "muscle pain", "joint pain","abdo pain", "sorethroat")))

# 2. Compare the value of using intesnity of over simple absence or presence of symptoms as an 
# explantory variable for major_activity_change
# Compare AIC for intensity model and AIC from absence/presence to determine best model

#Loop over intesnity vars to create table of delta aic and LR_test p values
mods_all <- list(mod_int, mod_yn)
OR_CI <- exp(cbind(coef(mod_int), confint(mod_int))) %>% 
  as.tibble() %>% 
  rename(OR = V1, ci2.5 = `2.5 %`, ci97.5 = `97.5 %`) %>%
  mutate(var = c("intercept", "intensity","age","sex"),
         or_ci  =  str_c(round(OR, 2), " (", round(ci2.5, 2), "-",  round(ci97.5, 2), ")")) %>% 
  filter(var == "intensity") %>% pull(or_ci)

print(OR_CI)
#Evaulate model using likelihood ratio test
an <- anova(mod_yn_int, test = "Chisq") #Demonstrates that addding intensity on top of YN is still significnantly improved 
LR_test <- an$`Pr(>Chi)`[3]

#Check AIC
mod_aic <- map_dbl(mods_all, AIC)
delta_aic = mod_aic[2] - mod_aic[1]
data.frame(symptom = str_replace(x, "_intensity", ""),
           or_ci = OR_CI,
           aic_int = mod_aic[1],
           aic_yn = mod_aic[2],
           delta_aic = delta_aic,
           LR_test = LR_test,
           sig = LR_test < 0.05)
}

binary_v_int <- map_df(int_vars, activity_change_mods, ipr_model) %>% 
  arrange(desc(delta_aic)) %>% 
  mutate_if(is.numeric, ~round(.x, 3))

#Table 2 is a combination of int_v_activity and binary_v_int

# Appendix table 2 & 3 ------------------------------------------------------------

# Index versus contact
index_contact_df <- ipr_symptom %>%
  mutate(symptom = case_when(symptom == "abdominalpain" ~ "abdominal pain",
                             symptom == "bodypain" ~ "body pain",
                             symptom == "bonepain" ~ "bone pain",
                             symptom == "jointpain" ~ "joint pain",
                             symptom == "musclepain" ~ "muscle pain",
                             symptom == "chestpain" ~ "chest pain",
                             symptom == "retroorbital" ~ "retro-orbital pain",
                             symptom == "badtaste" ~ "bad taste",
                             symptom == "maculopaprash" ~ "macularpap rash",
                             symptom == "gumbleed" ~ "gum bleed",
                             symptom == "nasalbleed" ~ "nasal bleed",
                             symptom == "vaginalbleed" ~ "vaginal bleed",
                             symptom == "urinebleed" ~ "urine bleed",
                             symptom == "sorethroat" ~ "sore throat",
                             TRUE ~ symptom)) %>% 
  group_by(study_code, group, symptom, index_contact) %>%
  summarise(present = any(present, na.rm = TRUE))

# Compare total number of symptoms experienced by index and contacts
num_symp <- index_contact_df %>% 
  group_by(study_code, index_contact) %>% 
  summarise(total = sum(present)) 

#t-test to compare total number of symptoms experienced
all_symp_t <- t.test(total ~ index_contact, data =  num_symp) 
index_cont_t_all <- tibble(symptom = "all", 
                           p_value = all_symp_t$p.value,
                           sig_95 = all_symp_t$p.value < 0.05,
                           sig_99 = all_symp_t$p.value < 0.01)


index_v_contact_all_tt <- index_contact_df %>%  
  group_by(study_code, index_contact) %>% 
  summarise(num_symptoms = n(), symptom_freq = sum(present, na.rm = TRUE)) %>% 
  group_by(index_contact) %>% 
  summarise(mean_num = mean(symptom_freq, na.rm = T), sd = sd(symptom_freq, na.rm = T)) %>%
  unite(n_present, sd, mean_num) %>% 
  spread(index_contact, n_present) %>% 
  separate(contact_sample, into = c("contact_sd", "contact_mean"), sep = "_") %>% 
  separate(index, into = c("index_sd", "index_mean"), sep = "_") %>%
  mutate(contact_mean = as.numeric(contact_mean),
         contact_sd = as.numeric(contact_sd),
         index_mean = as.numeric(index_mean),
         index_sd = as.numeric(index_sd),
         index = str_c(round(index_mean, 1), " (", round(index_sd, digits = 3), ")"),
         contact = str_c(round(contact_mean, 1), " (", round(contact_sd, digits = 3), ")")) %>% 
  mutate(symptom = "all", group = NA) %>% 
  select(symptom, group, everything(), -contact_mean, -contact_sd, -index_mean, -index_sd) %>% 
  left_join(index_cont_t_all, by = "symptom") %>% 
  arrange(p_value) %>% 
  select(-group)


#Compare proporation of people reporting each symptom for index and contact cases using Fisher's exact test
#Extract p value from each symptom
#Note Bonferroni correction applied (/36 due to 36 symptoms)
index_cont_fishy_individial <- map_df(unique(index_contact_df$symptom), function(x){
  ic <- index_contact_df$index_contact[index_contact_df$symptom == x]
  sy <- index_contact_df$present[index_contact_df$symptom == x]
  fishy <- fisher.test(ic, sy)
  p <- fishy$p.value
  sig_95 <- fishy$p.value < 0.05/36
  sig_99 <- fishy$p.value < 0.01/36
  tibble(symptom = x, p_value = p, sig_95 = sig_95, sig_99 = sig_99)
})

index_v_contact_individual_fisher <- index_contact_df %>% 
  group_by(group, symptom, index_contact) %>%
  summarise(num_parts = length(unique(study_code)), symptom_freq = sum(present, na.rm = TRUE)) %>% 
  unite(n_present, num_parts, symptom_freq) %>% 
  spread(index_contact, n_present) %>% 
  separate(contact_sample, into = c("contact_n", "contact_present")) %>% 
  separate(index, into = c("index_n", "index_present")) %>% 
  mutate(contact_n = as.numeric(contact_n),
         contact_present = as.numeric(contact_present),
         index_n = as.numeric(index_n),
         index_present = as.numeric(index_present),
         index = str_c(index_present, " (", 100*round(index_present/index_n, digits = 3), ")"),
         contact = str_c(contact_present, " (", 100*round(contact_present/contact_n, digits = 3), ")")) %>%
  select(symptom, everything(), -contact_n, -contact_present, -index_n, -index_present) %>% 
  left_join(index_cont_fishy_individial, by = "symptom") %>% 
  arrange(p_value)

