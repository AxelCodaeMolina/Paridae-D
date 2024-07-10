# Packages ####
library(ggplot2) # graphs
library(MASS) # glm.nb
library(emmeans) # pairwise comparisons
library(car) # Anova
library(RVAideMemoire) #plot resid
library(dplyr)
library(viridis) # For dynamic color palettes

# Load and prepare the data ####
load("~/CTitsSyntax.Rdata")

# Create the Treatment variable and ensure factors
data <- data %>%
  mutate(Treatment = factor(paste(Treatment_Notes, Treatment_Syntax, sep = " "))) %>%
  mutate(Treatment = relevel(Treatment, ref = "CONTROL CONTROL"))

data$Treatment <- as.factor(data$Treatment)
data$Treatment_Notes <- as.factor(data$Treatment_Notes)
data$Treatment_Syntax <- as.factor(data$Treatment_Syntax)
data$Treatment <- relevel(data$Treatment, ref = "CONTROL CONTROL")
data$Treatment_Notes <- relevel(data$Treatment_Notes, ref = "CONTROL")
data$Treatment_Syntax <- relevel(data$Treatment_Syntax, ref = "CONTROL")

# Filter data for models
data_ct_mob <- data %>%
  group_by(Treatment) %>%
  filter(sum(Number_CT_Mob != 0) >= 15) %>%
  ungroup()

data_other_mob <- data %>%
  group_by(Treatment) %>%
  filter(sum(Number_Other_Mob != 0) >= 15) %>%
  ungroup()

# Fit the models ####
model_MobCT <- glm(MobCT ~ Treatment, data = data, family = binomial)
model_MobOther <- glm(MobOther ~ Treatment, data = data, family = binomial)
model_number_ct_mob <- glm.nb(Number_CT_Mob ~ Treatment, data = data_ct_mob)
model_number_other_mob <- glm.nb(Number_Other_Mob ~ Treatment, data = data_other_mob)

model_MobCT_inter <- glm(MobCT ~ Treatment_Notes * Treatment_Syntax, data = data, family = binomial)
model_MobOther_inter <- glm(MobOther ~ Treatment_Notes * Treatment_Syntax, data = data, family = binomial)
model_number_ct_mob_inter <- glm.nb(Number_CT_Mob ~ Treatment_Syntax, data = data_ct_mob)
model_number_other_mob_inter <- glm.nb(Number_Other_Mob ~ Treatment_Notes * Treatment_Syntax, data = data_other_mob)

# Perform ANOVA type II ####
anova_MobCT <- Anova(model_MobCT, type = "II")
anova_MobOther <- Anova(model_MobOther, type = "II")
anova_number_ct_mob <- Anova(model_number_ct_mob, type = "II")
anova_number_other_mob <- Anova(model_number_other_mob, type = "II")

anova_MobCT_inter <- Anova(model_MobCT_inter, type = "II")
anova_MobOther_inter <- Anova(model_MobOther_inter, type = "II")
anova_number_ct_mob_inter <- Anova(model_number_ct_mob_inter, type = "II")
anova_number_other_mob_inter <- Anova(model_number_other_mob_inter, type = "II")

# Print ANOVA results
print("ANOVA Type II for MobCT ~ Treatment:")
print(anova_MobCT)
print("ANOVA Type II for MobOther ~ Treatment:")
print(anova_MobOther)
print("ANOVA Type II for Number_CT_Mob ~ Treatment:")
print(anova_number_ct_mob)
print("ANOVA Type II for Number_Other_Mob ~ Treatment:")
print(anova_number_other_mob)

print("ANOVA Type II for MobCT ~ Treatment_Notes * Treatment_Syntax:")
print(anova_MobCT_inter)
print("ANOVA Type II for MobOther ~ Treatment_Notes * Treatment_Syntax:")
print(anova_MobOther_inter)
print("ANOVA Type II for Number_CT_Mob ~ Treatment_Syntax:")
print(anova_number_ct_mob_inter)
print("ANOVA Type II for Number_Other_Mob ~ Treatment_Notes * Treatment_Syntax:")
print(anova_number_other_mob_inter)

# Pairwise comparisons with Bonferroni corrections ####
pairwise_mobct <- emmeans(model_MobCT, ~ Treatment)
pairwise_mobct_contrasts <- contrast(pairwise_mobct, method = "trt.vs.ctrl", ref = "CONTROL CONTROL", adjust = "bonferroni")

pairwise_mobother <- emmeans(model_MobOther, ~ Treatment)
pairwise_mobother_contrasts <- contrast(pairwise_mobother, method = "trt.vs.ctrl", ref = "CONTROL CONTROL", adjust = "bonferroni")

pairwise_number_ct_mob <- emmeans(model_number_ct_mob, pairwise ~ Treatment, adjust = "bonferroni")
pairwise_number_other_mob <- emmeans(model_number_other_mob, pairwise ~ Treatment, adjust = "bonferroni")

pairwise_mobct_inter <- emmeans(model_MobCT_inter, ~ Treatment_Notes * Treatment_Syntax)
pairwise_mobct_inter_contrasts <- contrast(pairwise_mobct_inter, method = "trt.vs.ctrl", ref = "CONTROL CONTROL", adjust = "bonferroni")

pairwise_mobother_inter <- emmeans(model_MobOther_inter, ~ Treatment_Notes * Treatment_Syntax)
pairwise_mobother_inter_contrasts <- contrast(pairwise_mobother_inter, method = "trt.vs.ctrl", ref = "CONTROL CONTROL", adjust = "bonferroni")

pairwise_number_ct_mob_inter <- emmeans(model_number_ct_mob_inter, pairwise ~ Treatment_Syntax, adjust = "bonferroni")
pairwise_number_other_mob_inter <- emmeans(model_number_other_mob_inter, pairwise ~ Treatment_Notes * Treatment_Syntax, adjust = "bonferroni")

# Filter out non-estimable contrasts
pairwise_mobct_inter_contrasts <- as.data.frame(pairwise_mobct_inter_contrasts)
pairwise_mobct_inter_contrasts <- pairwise_mobct_inter_contrasts[!is.na(pairwise_mobct_inter_contrasts$estimate), ]

pairwise_mobother_inter_contrasts <- as.data.frame(pairwise_mobother_inter_contrasts)
pairwise_mobother_inter_contrasts <- pairwise_mobother_inter_contrasts[!is.na(pairwise_mobother_inter_contrasts$estimate), ]

# Print pairwise comparison results 
print("Pairwise comparisons for MobCT ~ Treatment:")
print(pairwise_mobct_contrasts)
print("Pairwise comparisons for MobOther ~ Treatment:")
print(pairwise_mobother_contrasts)
print("Pairwise comparisons for Number_CT_Mob ~ Treatment:")
print(pairwise_number_ct_mob$contrasts)
print("Pairwise comparisons for Number_Other_Mob ~ Treatment:")
print(pairwise_number_other_mob$contrasts)

print("Pairwise comparisons for MobCT ~ Treatment_Notes * Treatment_Syntax:")
print(pairwise_mobct_inter_contrasts)
print("Pairwise comparisons for MobOther ~ Treatment_Notes * Treatment_Syntax:")
print(pairwise_mobother_inter_contrasts)
print("Pairwise comparisons for Number_CT_Mob ~ Treatment_Syntax:")
print(pairwise_number_ct_mob_inter$contrasts)
print("Pairwise comparisons for Number_Other_Mob ~ Treatment_Notes * Treatment_Syntax:")
print(pairwise_number_other_mob_inter$contrasts)


# Summary function with confidence interval for plots ####

summarySE <- function(data = NULL, measurevar, groupvars = NULL, na.rm = FALSE, conf.interval = 0.95) {
  length2 <- function(x, na.rm = FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  data_summary <- data %>%
    group_by(across(all_of(groupvars))) %>%
    summarise(N = length2(get(measurevar), na.rm = na.rm),
              mean = mean(get(measurevar), na.rm = na.rm),
              sd = sd(get(measurevar), na.rm = na.rm), .groups = 'drop') %>%
    mutate(se = sd / sqrt(N),
           ciMult = qt(conf.interval / 2 + 0.5, N - 1),
           ci = se * ciMult) %>%
    rename(!!measurevar := mean)
  
  return(data_summary)
}

# Calculate summary and create plots ####

# Modify the Treatment column
data <- data %>%
  mutate(Treatment = recode(Treatment, "CONTROL CONTROL" = "Non-mobbing"))

data_ct_mob <- data_ct_mob %>%
  mutate(Treatment = recode(Treatment, "CONTROL CONTROL" = "Non-mobbing"))

data_other_mob <- data_other_mob %>%
  mutate(Treatment = recode(Treatment, "CONTROL CONTROL" = "Non-mobbing"))

# Colors
viridis_palette <- viridis(9)
custom_colors <- c(
  "CT CT" = viridis_palette[1], 
  "CT FD" = viridis_palette[2],  
  "CT Rev" = viridis_palette[3],
  "GT CT" = viridis_palette[4],  
  "GT FD" = viridis_palette[5],  
  "GT Rev" = viridis_palette[6], 
  "ARTIF CT" = viridis_palette[7], 
  "ARTIF FD" = viridis_palette[8], 
  "ARTIF Rev" = viridis_palette[9] 
)

# Function to create plots for binary outcomes
create_binary_plot <- function(data, measurevar, y_label) {
  tgc <- summarySE(data, measurevar = measurevar, groupvars = "Treatment")
  
  ggplot(tgc, aes(x = Treatment, y = .data[[measurevar]])) +
    geom_point(aes(color = Treatment), size = 3, position = position_dodge(0.6)) +
    geom_errorbar(aes(ymin = .data[[measurevar]] - ci, ymax = .data[[measurevar]] + ci, color = Treatment), 
                  width = 0.2, position = position_dodge(0.6)) +
    scale_color_manual(values = custom_colors) +
    theme_minimal() +
    labs (x = "Playback treatment", y = y_label) +
    theme(legend.position = "none")
}

# Function to create plots for count outcomes
create_count_plot <- function(data, measurevar, y_label) {
  tgc <- summarySE(data, measurevar = measurevar, groupvars = "Treatment")
  
  ggplot(data, aes(x = Treatment, y = .data[[measurevar]])) +
    geom_boxplot(aes(fill = Treatment), position = position_dodge(0.6), width = 0.5) +
    scale_fill_manual(values = custom_colors) +
    geom_errorbar(data = tgc, aes(ymin = .data[[measurevar]] - ci, ymax = .data[[measurevar]] + ci, group = Treatment), 
                  colour = "black", width = 0.05, position = position_dodge(0.6)) +
    geom_point(data = tgc, position = position_dodge(width = 0.6)) +
    theme_minimal() +
    labs(x = "Playback treatment", y = y_label)
}

# Plots for binary outcomes
p_MobCT <- create_binary_plot(data, "MobCT", "Proportion of mobbing coal tits")
print(p_MobCT)
ggsave(filename = "p_MobCT.png", plot = p_MobCT, width = 8, height = 6, bg = "white")

p_MobOther <- create_binary_plot(data, "MobOther", "Proportion of mobbing non-coal tits birds")
print(p_MobOther)
ggsave(filename = "p_MobOther.png", plot = p_MobOther, width = 8, height = 6, bg = "white")


# Plots for count outcomes
p_Number_CT_Mob <- create_count_plot(data_ct_mob, "Number_CT_Mob", "Number of mobbing coal tits")
print(p_Number_CT_Mob)
ggsave(filename = "p_Number_CT_Mob.png", plot = p_Number_CT_Mob, width = 8, height = 6, bg = "white")


p_Number_Other_Mob <- create_count_plot(data_other_mob, "Number_Other_Mob", "Number of mobbing non-coal tits birds")
print(p_Number_Other_Mob)
ggsave(filename = "p_Number_Other_Mob.png", plot = p_Number_Other_Mob, width = 8, height = 6, bg = "white")

