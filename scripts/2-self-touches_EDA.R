library(plyr)
library(tidyverse)
library(ggpubr)
library(lubridate)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggforce)


# Retrieving current directory
dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

root_dir <- getwd()
project_directory <- dirname(root_dir)
curated_data_dir <- file.path(project_directory, 'data')


## Read working session data ####
working_all_df_raw <- read.csv(sprintf("%s/1_WorkingSession_TimeTouches_raw.csv", curated_data_dir))


# EDA in Raw Touches ####
# show the unique touches with their counts
working_all_df_raw$Touches %>%
  table() %>%
  sort(decreasing = T)

## ignore left or right in the touches ####
working_all_df_raw$Touches2 <- str_replace_all(working_all_df_raw$Touches, "Left", "")
working_all_df_raw$Touches2 <- str_replace_all(working_all_df_raw$Touches2, "Right", "")

# remove dublicates in Touches2
working_all_df_raw$Touches3 <- sapply(strsplit(working_all_df_raw$Touches2, ",\\s*"), function(x) {
  paste(unique(trimws(x)), collapse = ", ")
})


working_all_df_raw$Touches3 %>%
  table() %>%
  sort(decreasing = T)


#### ENDS ####



# Create Touches 4 using the Touches3,
#
# "1" <- "Face, Cheek, Chin, Nose"
# "2" <- "Face, Cheek, Chin"
# "3"	<- "Face, Chin"
# "4"	<- "Face, Cheek"
# "5" <- "All other touches"
# "6" <- ""


# "FCheChiN" <-"1" <- "Face, Cheek, Chin, Nose"
# "FCheChi"  <-"2" <- "Face, Cheek, Chin"
# "FChi"	   <-"3" <- "Face, Chin"
# "FChe"	   <-"4" <- "Face, Cheek"
# "Others"   <-"5" <- "All other touches"
# "NoTouch"<-"6" <- ""



working_all_df_raw$Touches4 <- NA

# if Touches3 is "" , Touches4 = NoTouch # No touches
working_all_df_raw$Touches4 <- ifelse(
  working_all_df_raw$Touches3 == "",
  "NoTouch", working_all_df_raw$Touches4
)

# if "Face, Cheek, Chin, Nose" in Touches3, Touches4 = 1
# 39661 times
working_all_df_raw$Touches4 <- ifelse(
  working_all_df_raw$Touches3 == "Face, Cheek, Chin, Nose", # FCheChiN
  "FCheChiN", working_all_df_raw$Touches4
)

# 650 times
working_all_df_raw$Touches4 <- ifelse(
  working_all_df_raw$Touches3 == "Face, Chin, Cheek, Nose",
  "FCheChiN", working_all_df_raw$Touches4
)
# 40 times
working_all_df_raw$Touches4 <- ifelse(
  working_all_df_raw$Touches3 == "Face, Cheek, Nose, Chin",
  "FCheChiN", working_all_df_raw$Touches4
)
# 720 times
working_all_df_raw$Touches4 <- ifelse(
  working_all_df_raw$Touches3 == "Face, Chin, Nose, Cheek",
  "FCheChiN", working_all_df_raw$Touches4
)

# only 2 time
working_all_df_raw$Touches4 <- ifelse(
  working_all_df_raw$Touches3 == "Face, Nose, Cheek, Chin",
  "FCheChiN", working_all_df_raw$Touches4
)

# no result
working_all_df_raw$Touches4 <- ifelse(
  working_all_df_raw$Touches3 == "Face, Nose, Chin, Cheek",
  "FCheChiN", working_all_df_raw$Touches4
)






# FCheChi
working_all_df_raw$Touches4 <- ifelse(
  working_all_df_raw$Touches3 == "Face, Cheek, Chin", # FCheChi
  "FCheChi",
  working_all_df_raw$Touches4
)

working_all_df_raw$Touches4 <- ifelse(
  working_all_df_raw$Touches3 == "Face, Chin, Cheek",
  "FCheChi",
  working_all_df_raw$Touches4
)




working_all_df_raw$Touches4 <- ifelse(
  working_all_df_raw$Touches3 == "Face, Chin", # FChi
  "FChi",
  working_all_df_raw$Touches4
)

working_all_df_raw$Touches4 <- ifelse(
  working_all_df_raw$Touches3 == "Face, Cheek",
  "FChe",
  working_all_df_raw$Touches4
)





table(working_all_df_raw$Touches4)


sum(is.na(working_all_df_raw$Touches4))


# Replace NA in Touches4 ("All others" ) with Others
working_all_df_raw$Touches4[is.na(working_all_df_raw$Touches4)] <- "Others" # Others
table(working_all_df_raw$Touches4)


# save it as txt file
# working_all_df_raw$Touches4 %>% table() %>% sort(decreasing = T) %>%
#   write.table(sprintf("%s/1_WS_TimeTouches_raw_touches_freq.txt", rep_dir), sep = "\t")
#### ENDS ####


# Create the frequency table for the Touches4 each ID and Day

model_df_touches <- working_all_df_raw %>%
  group_by(ID, Day, Touches4) %>%
  dplyr::summarise(count = dplyr::n()) %>%
  ungroup() %>%
  arrange(ID, Day, desc(count))


# str(model_df_touches)


# cast data based on Touches4
model_df_touches_wide <- model_df_touches %>%
  spread(Touches4, count, fill = 0)

# save as csv file
# write.csv(model_df_touches_wide, "../data/WorkingSession_TimeTouches_model.csv", row.names = F)
write.csv(model_df_touches_wide, sprintf("%s/WorkingSession_TimeTouches_model.csv", curated_data_dir), row.names = F)


model_df_touches2 <- model_df_touches %>%
  group_by(ID, Day) %>%
  # make count as percentage within each ID and Day
  mutate(
    perc = count / sum(count) * 100,
    perc_total = sum(perc)
  )

model_df_touches2_wide <- model_df_touches2 %>%
  dplyr::select(-count, -perc_total) %>%
  spread(Touches4, perc, fill = 0)


# save as csv file
write.csv(model_df_touches2_wide, sprintf("%s/Touches_Daily.csv", curated_data_dir), row.names = F)


# Plotting the Touches4 in percentage ####
model_df_touches2 <- model_df_touches %>%
  group_by(ID, Day) %>%
  # make count as percentage within each ID and Day
  mutate(
    perc = count / sum(count) * 100,
    perc_total = sum(perc)
  )


# Without x axis label first rows
my_theme <- theme_bw() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12, face = "bold"),
    strip.text = element_text(size = 10),
    strip.background = element_rect(fill = "white"),
    legend.position = "none"
  )

# With  x-y axis label
my_theme1 <- my_theme +
  theme(
    axis.text.x = element_text(size = 12, face = "bold")
  )


# With  x-y axis label
my_theme2 <- my_theme1 +
  theme(
    axis.text.x = element_text(size = 8, face = "bold"),
    axis.text.y = element_text(size = 8, face = "bold")
  )


unique(model_df_touches$Touches4)


# you should say Che instead of FChe
model_df_touches$Touches4 <- model_df_touches$Touches4 %>% str_replace_all("FChe", "Che")

# you should say Chi instead of FChi
model_df_touches$Touches4 <- model_df_touches$Touches4 %>% str_replace_all("FChi", "Chi")

# you should say CheChi instead of FCheChi
model_df_touches$Touches4 <- model_df_touches$Touches4 %>% str_replace_all("FCheChi", "CheChi")

# you should say CheChiN instead of FCheChiN
model_df_touches$Touches4 <- model_df_touches$Touches4 %>% str_replace_all("FCheChiN", "CheChiN")

model_df_touches$Touches4 <- factor(model_df_touches$Touches4, levels = c("NoTouch", "Che", "Chi", "CheChi", "CheChiN", "Others"))

unique(model_df_touches$Touches4)





touches_color <- c(
  "NoTouch" = "grey", "Che" = "#FFC067", "Chi" = "darkolivegreen",
  "CheChi" = "deepskyblue", "CheChiN" = "orangered", "Others" = "deeppink3"
)


# create stack plot of Touches4
stack_plot <- model_df_touches %>%
  ggplot(aes(x = ID, y = count, fill = Touches4)) +
  scale_fill_manual(values = alpha(touches_color, 0.9)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~Day, scales = "free_y", nrow = 4, strip.position = "right") +
  my_theme1 +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, color = "white"), # White text
    strip.background = element_rect(fill = "black"), # Black background
    plot.margin = margin(1, 1, 1, 1)
  ) +
  labs(title = "", x = "", y = "") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14, face = "bold"),
    legend.key = element_rect(color = "black"), # Add black outline to legend keys
    legend.direction = "horizontal",
    legend.key.width = unit(0.5, "cm")
  ) +
  guides(
    fill = guide_legend(
      nrow = 1, # Single row for legend
      byrow = TRUE, # Fill by row
      label.theme = element_text(margin = margin(l = 8, r = 10, unit = "pt")), # space after each label
      override.aes = list(alpha = 0.7, color = "black") # Apply transparency in legend
    )
  )


stack_plot


ggsave("../plots/Figure4.pdf", stack_plot, width = 7, height = 7)


# Read the working session data with Touches

working_all_df <- read.csv(sprintf("%s/2_WorkingSession_TimeTouches_curated.csv", curated_data_dir))

names(working_all_df)
unique(working_all_df$Touches)
str(working_all_df)

working_all_df <- working_all_df %>%
  filter(Touches != "Face")

unique(working_all_df$Touches)

# Ensure Touches2 exists before modifying it
if (!"Touches2" %in% colnames(working_all_df)) {
  working_all_df$Touches2 <- NA
}

# Apply the condition safely
working_all_df$Touches2 <- ifelse(
  working_all_df$Touches == "" | is.na(working_all_df$Touches),
  "No Touch",
  working_all_df$Touches
)

unique(working_all_df$Touches2)


working_all_df$Touches2 <- factor(working_all_df$Touches2,
  levels = c(
    "No Touch", "Chin", "Nose", "Forehead",
    "Left Eye", "Right Eye", "Left Cheek", "Right Cheek"
  )
)



# Figure 3 | T001 used in the paper

tmp_id <- sprintf("T%03d", 1)

working_all_df_tmp <- working_all_df %>%
  filter(ID == tmp_id)


# Count Plots ####
hist_tmp_v3 <- working_all_df_tmp %>%
  filter(Touches != "Face") %>%
  filter(Touches2 != "No Touch") %>%
  ggplot(aes(x = Touches2)) +
  facet_wrap(~ ID + Day, ncol = 4, nrow = 5) +
  labs(title = "", x = "", y = "# sFST") +
  geom_bar(stat = "count", color = "black", fill = "orange") +
  theme_bw() +
  # remove grid
  theme(
    panel.grid.major = element_blank() # ,
    # panel.grid.minor = element_blank()
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 8, color = "white"), # White text
    strip.background = element_rect(fill = "black"), # Black background
    plot.margin = margin(1, 1, 1, 1)
  )


pdf(sprintf("../plots/Figure3.pdf"), width = 10, height = 3)
print(hist_tmp_v3)
dev.off()
