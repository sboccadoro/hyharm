library(readxl)
library(lme4)
library(nlme)
library(sjPlot)
library(emmeans)
library(ggplot2) 
library(ggprism)
library(ggpubr)
library(effects)
library(patchwork)
library(robustlmm)
#________________________Import data________________________________________________________________________________________________________________________

hyharm <- read_excel("Your path/APIM_data_real.xlsx")
View(hyharm)
summary(hyharm)

hyharm <- read_excel("Your path/APIM_data_sim.xlsx")
View(hyharm)

#________________________Recode variables________________________________________________________________________________________________________________________
hyharm[hyharm == "T"] <- NA
hyharm$Pair_id <- factor(hyharm$Pair_id)
hyharm$Trial <- as.numeric(hyharm$Time) 
hyharm$Trial.z <- (hyharm$Trial - mean(hyharm$Trial))/sd(hyharm$Trial)
hyharm$Run <- factor(hyharm$Run)
hyharm$Actor_outcome <- factor(hyharm$Actor_outcome, levels = c(1, 2), labels = c("Self-harm", "Aggression"))
hyharm$Actor_predictor <- factor(hyharm$Actor_predictor, levels = c(1, 2), labels = c("Self-harm", "Aggression"))
hyharm$Partner_predictor <- factor(hyharm$Partner_predictor, levels = c(1, 2), labels = c("No-provocation", "Provocation"))
hyharm$Reward <- factor(hyharm$Reward, levels = c(1, 2, 3), labels = c("Self>Other", "Equal", "Other>Self"))
hyharm$BPAQ <- as.numeric(hyharm$BPAQ)
hyharm$BPAQ.z <- (hyharm$BPAQ - mean(hyharm$BPAQ))/sd(hyharm$BPAQ)
hyharm$SQ <- as.numeric(hyharm$SQ)
hyharm$SQ.z <- (hyharm$SQ - mean(hyharm$SQ, na.rm=TRUE))/sd(hyharm$SQ, na.rm=TRUE)  
hyharm$SP <- as.numeric(hyharm$SP)
hyharm$SP.z <- (hyharm$SP - mean(hyharm$SP, na.rm=TRUE))/sd(hyharm$SP, na.rm=TRUE)
hyharm$SR <- as.numeric(hyharm$SR)
hyharm$SR.z <- (hyharm$SR - mean(hyharm$SR))/sd(hyharm$SR)
hyharm$rTPJ_outcome <- as.numeric(hyharm$rTPJ_outcome)
hyharm$dmPFC_outcome <- as.numeric(hyharm$dmPFC_outcome)
hyharm$Precuneus_outcome <- as.numeric(hyharm$Precuneus_outcome)
hyharm$actor_rTPJ <- as.numeric(hyharm$actor_rTPJ)
hyharm$actor_dmPFC <- as.numeric(hyharm$actor_dmPFC)
hyharm$actor_Precuneus <- as.numeric(hyharm$actor_Precuneus)
hyharm$partner_rTPJ <- as.numeric(hyharm$partner_rTPJ)
hyharm$partner_dmPFC <- as.numeric(hyharm$partner_dmPFC)
hyharm$partner_Precuneus <- as.numeric(hyharm$partner_Precuneus)
hyharm$rTPJ_outcome.z <- (hyharm$rTPJ_outcome - mean(hyharm$rTPJ_outcome, na.rm = TRUE))/sd(hyharm$rTPJ_outcome, na.rm = TRUE) 
hyharm$dmPFC_outcome.z <- (hyharm$dmPFC_outcome - mean(hyharm$dmPFC_outcome, na.rm = TRUE))/sd(hyharm$dmPFC_outcome, na.rm = TRUE) 
hyharm$Precuneus_outcome.z <- (hyharm$Precuneus_outcome - mean(hyharm$Precuneus_outcome, na.rm = TRUE))/sd(hyharm$Precuneus_outcome, na.rm = TRUE) 
hyharm$actor_rTPJ.z <- (hyharm$actor_rTPJ - mean(hyharm$actor_rTPJ, na.rm = TRUE))/sd(hyharm$actor_rTPJ, na.rm = TRUE) 
hyharm$partner_rTPJ.z <- (hyharm$partner_rTPJ - mean(hyharm$partner_rTPJ, na.rm = TRUE))/sd(hyharm$partner_rTPJ, na.rm = TRUE) 
hyharm$actor_dmPFC.z <- (hyharm$actor_dmPFC - mean(hyharm$actor_dmPFC, na.rm = TRUE))/sd(hyharm$actor_dmPFC, na.rm = TRUE) 
hyharm$partner_dmPFC.z <- (hyharm$partner_dmPFC - mean(hyharm$partner_dmPFC, na.rm = TRUE))/sd(hyharm$partner_dmPFC, na.rm = TRUE) 
hyharm$actor_Precuneus.z <- (hyharm$actor_Precuneus - mean(hyharm$actor_Precuneus, na.rm = TRUE))/sd(hyharm$actor_Precuneus, na.rm = TRUE) 
hyharm$partner_Precuneus.z <- (hyharm$partner_Precuneus - mean(hyharm$partner_Precuneus, na.rm = TRUE))/sd(hyharm$partner_Precuneus, na.rm = TRUE) 

#______________________________BEHAVIORAL MODELS________________________________________________________________________________________________________________________________________

#___________________________________null model selection____________________________________________________________________________________________________________
null_behav1 <- glmer(Actor_outcome ~ 1 + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))
null_behav2 <- glmer(Actor_outcome ~ 1 + (1+Trial.z|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

anova(null_behav1, null_behav2)

#Models without questionnaires

apim_behav1 <- glmer(Actor_outcome ~ Actor_predictor + Reward + Partner_predictor + Run
                     + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_behav2 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor + Run
                     + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))
tab_model(apim_behav2)
summary(apim_behav2)    #simulated
tab_model(apim_behav2, file = "model_table_behav_sim.doc")

apim_behav3 <- glmer(Actor_outcome ~ Actor_predictor + Partner_predictor*Reward + Run
                     + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_behav4 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor*Reward + Run
                     + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_behav5 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor*Reward + Actor_predictor*Run
                     + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))
tab_model(apim_behav5)
summary(apim_behav5)    #real
tab_model(apim_behav5, file = "model_table_behav_real.doc")

apim_behav6 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor*Reward + Partner_predictor*Run
                     + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_behav7 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor*Reward + Partner_predictor*Run + Actor_predictor*Run
                     + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))


anova(apim_behav1, apim_behav2, apim_behav3, apim_behav4, apim_behav5, apim_behav6, apim_behav7) #real
anova(apim_behav5, apim_behav7)

#Models with questionnaires for real interaction blocks

apim_quest1 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor*Reward + Actor_predictor*Run 
               + BPAQ.z + SQ.z + SR.z + SP.z + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_quest2 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor*Reward + Actor_predictor*Run 
               + BPAQ.z + SQ.z + SR.z + SP.z*Partner_predictor + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))
summary(apim_quest2)
tab_model(apim_quest2)
tab_model(apim_quest2, file = "model_table_quest_real.doc")


apim_quest3 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor*Reward + Actor_predictor*Run 
                     + BPAQ.z*Partner_predictor + SQ.z + SR.z + SP.z + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_quest4 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor*Reward + Actor_predictor*Run 
               + BPAQ.z*Partner_predictor + SQ.z + SR.z + SP.z*Partner_predictor + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

anova(apim_quest1, apim_quest2, apim_quest3, apim_quest4)

apim_quest5 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor*Reward + Actor_predictor*Run 
                + BPAQ.z + SQ.z*Reward + SR.z  + SP.z + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_quest6 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor*Reward + Actor_predictor*Run 
                + BPAQ.z + SQ.z + SR.z*Reward  + SP.z + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_quest7 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor*Reward + Actor_predictor*Run 
                + BPAQ.z + SQ.z*Reward + SR.z*Reward  + SP.z + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_quest8 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor*Reward + Actor_predictor*Run 
               + BPAQ.z*Partner_predictor + SQ.z*Reward + SR.z + SP.z + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_quest9 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor*Reward + Actor_predictor*Run 
               + BPAQ.z*Partner_predictor + SQ.z + SR.z*Reward + SP.z + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_quest10 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor*Reward + Actor_predictor*Run 
               + BPAQ.z + SQ.z*Reward + SR.z + SP.z*Partner_predictor + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_quest11 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor*Reward + Actor_predictor*Run 
                + BPAQ.z + SQ.z+ SR.z*Reward + SP.z*Partner_predictor + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_quest12 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor*Reward + Actor_predictor*Run 
                + BPAQ.z*Partner_predictor + SQ.z*Reward + SR.z + SP.z*Partner_predictor + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_quest13 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor*Reward + Actor_predictor*Run 
                + BPAQ.z*Partner_predictor + SQ.z+ SR.z*Reward  + SP.z*Partner_predictor + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_quest14 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor*Reward + Actor_predictor*Run 
                + BPAQ.z*Partner_predictor + SQ.z*Reward + SR.z*Reward  + SP.z + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_quest15 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor*Reward + Actor_predictor*Run 
                + BPAQ.z + SQ.z*Reward + SR.z*Reward  + SP.z*Partner_predictor + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_quest16 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor*Reward + Actor_predictor*Run 
                + BPAQ.z*Partner_predictor + SQ.z*Reward + SR.z*Reward  + SP.z*Partner_predictor + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

anova(apim_quest1, apim_quest2, apim_quest3, apim_quest4, apim_quest5, apim_quest6, apim_quest7, apim_quest8, 
      apim_quest9, apim_quest10, apim_quest11, apim_quest12, apim_quest13, apim_quest14, apim_quest15, apim_quest16)
anova(apim_quest2, apim_quest4)

#models with questionnaires for simulated interaction blocks

apim_quest1 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor + Run + BPAQ.z + SQ.z + SR.z + SP.z
                        + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))
summary(apim_quest1)
tab_model(apim_quest1)
tab_model(apim_quest1, file = "model_table_quest_sim.doc")

apim_quest2 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor + Run + BPAQ.z + SQ.z + SR.z + SP.z*Partner_predictor
                     + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_quest3 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor + Run + BPAQ.z*Partner_predictor + SQ.z + SR.z + SP.z
                     + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_quest4 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor + Run + BPAQ.z*Partner_predictor + SQ.z + SR.z + SP.z*Partner_predictor
                    + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

anova(apim_quest1, apim_quest2, apim_quest3, apim_quest4)


apim_quest5 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor + Run + BPAQ.z + SQ.z*Reward + SR.z + SP.z
                    + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_quest6 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor + Run + BPAQ.z + SQ.z+ SR.z*Reward + SP.z
                     + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_quest7 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor + Run + BPAQ.z + SQ.z*Reward + SR.z*Reward + SP.z
                     + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_quest8 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor + Run + BPAQ.z*Partner_predictor + SQ.z*Reward + SR.z + SP.z
                     + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_quest9 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor + Run + BPAQ.z*Partner_predictor + SQ.z + SR.z*Reward + SP.z
                     + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_quest10 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor + Run + BPAQ.z + SQ.z*Reward + SR.z + SP.z*Partner_predictor
                     + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_quest11 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor + Run + BPAQ.z + SQ.z + SR.z*Reward + SP.z*Partner_predictor
                      + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_quest12 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor + Run + BPAQ.z*Partner_predictor + SQ.z*Reward + SR.z + SP.z*Partner_predictor
                      + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_quest13 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor + Run + BPAQ.z*Partner_predictor + SQ.z + SR.z*Reward + SP.z*Partner_predictor
                      + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_quest14 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor + Run + BPAQ.z*Partner_predictor + SQ.z*Reward + SR.z*Reward + SP.z
                      + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_quest15 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor + Run + BPAQ.z + SQ.z*Reward + SR.z*Reward + SP.z*Partner_predictor
                      + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

apim_quest16 <- glmer(Actor_outcome ~ Actor_predictor*Reward + Partner_predictor + Run + BPAQ.z*Partner_predictor + SQ.z*Reward + SR.z*Reward + SP.z*Partner_predictor
                      + (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))

anova(apim_quest1, apim_quest2, apim_quest3, apim_quest4, apim_quest5, apim_quest6, apim_quest7, apim_quest8, 
      apim_quest9, apim_quest10, apim_quest11, apim_quest12, apim_quest13, apim_quest14, apim_quest15, apim_quest16)

#post-hoc tests

pairs(emmeans(apim_behav5, ~ Reward), adjust = "tukey", type = "response")
pairs(emmeans(apim_behav2, ~ Reward), adjust = "tukey", type = "response")

emmeans(apim_behav5, pairwise ~ Partner_predictor|Reward, adjust = "tukey", type = "response")
emmeans(apim_behav5, pairwise ~ Reward|Partner_predictor, adjust = "tukey", type = "response")

emmeans(apim_behav5, pairwise ~ Run|Actor_predictor, adjust = "tukey", type = "response")
emmeans(apim_behav5, pairwise ~ Actor_predictor|Run, adjust = "tukey", type = "response")

emmeans(apim_behav2, pairwise ~ Actor_predictor|Reward, adjust = "tukey", type = "response")
emmeans(apim_behav2, pairwise ~ Reward|Actor_predictor, adjust = "tukey", type = "response")

emtrends(apim_quest2, pairwise ~ Partner_predictor, var = "SP.z")

#___________________________________Plotting_____________________________________________________________________________________

#Plotting interaction Partner_predictor*Reward
summary(apim_behav5)
emm <- emmeans(apim_behav5, ~ Partner_predictor*Reward, type = "response")
emm_df <- as.data.frame(emm)

A <- ggplot(emm_df, aes(x = as.factor(Reward), y = prob, group = Partner_predictor, color = Partner_predictor)) +
            geom_line(linewidth = 1.2) + geom_point(size = 3) + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
            scale_color_manual(values = c("No-provocation" = "blue", "Provocation" = "red")) +  
            labs(title = "Partner effect x Reward condition on Aggression",
            x = "Reward", y = "Predicted probability of Aggression", color = "Partner_predictor") +
            theme_prism() + theme(plot.title = element_text(size = 22, face = "bold"),
            axis.title = element_text(size = 18), axis.text = element_text(size = 16),
            legend.position = "top", legend.title = element_text(size = 16), legend.text = element_text(size = 16))
plot(A)

#Plotting interaction Actor_predictor*Reward
summary(apim_behav2)
emm <- emmeans(apim_behav2, ~ Actor_predictor*Reward, type = "response")
emm_df <- as.data.frame(emm)

B <- ggplot(emm_df, aes(x = as.factor(Reward), y = prob, group = Actor_predictor, color = Actor_predictor)) +
  geom_line(linewidth = 1.2) + geom_point(linewidth = 3) + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  scale_color_manual(values = c("Self-harm" = "blue", "Aggression" = "red")) +  
  labs(title = "Actor effect x Reward condition on Aggression",
       x = "Reward", y = "Predicted probability of Aggression", color = "Actor_predictor") +
  theme_prism() + theme(plot.title = element_text(size = 22, face = "bold"),
                        axis.title = element_text(size = 18), axis.text = element_text(size = 16),
                        legend.position = "top", legend.title = element_text(size = 16), legend.text = element_text(size = 16))
plot(B)


#Plotting interaction Actor_predictor*Run
emm_actor_run <- emmeans(apim_behav5, ~ Actor_predictor*Run, type = "response")
emm_actor_run_df <- as.data.frame(emm_actor_run)

C <- ggplot(emm_actor_run_df, aes(x = as.factor(Run), y = prob, fill = Actor_predictor)) +
       geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
       geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, position = position_dodge(width = 0.7)) +
       scale_fill_manual(values = c("blue", "red")) +
       labs(title = "Actor effect x Run on Aggression", x = "Run", y = "Predicted probability of Aggression",
       fill = "Actor_predictor") +
       theme_prism() + theme(plot.title = element_text(size = 20, face = "bold"),
       axis.title = element_text(size = 16), axis.text = element_text(size = 14),
       legend.position = "top", legend.title = element_text(size = 14), legend.text = element_text(size = 14))
plot(C)

# Blank plot
D <- ggplot() + theme_void() + theme(panel.background = element_rect(fill = "white", color = NA),
                                     plot.background = element_rect(fill = "white", color = NA),
                                     plot.margin = margin(30, 30, 30, 30))

# Add margins
A <- A + theme(plot.margin = margin(30, 30, 30, 30))  
B <- B + theme(plot.margin = margin(30, 30, 30, 30))  
C <- C + theme(plot.margin = margin(30, 30, 30, 30))  
D <- D + theme(plot.margin = margin(30, 30, 30, 30)) 

# Combine plots
combined_plot <- ggarrange(A, B, C, D, ncol = 2, nrow = 2, labels = c("A)", "B)", "C)", ""), 
                           align = "hv", legend = "top",  
                           widths = c(1.5, 1.5), heights = c(1.5, 1.5))

combined_plot <- combined_plot + theme(plot.margin = margin(20, 20, 20, 20), 
                                       plot.title = element_text(size = 20, face = "bold"),
                                       axis.title = element_text(size = 16), 
                                       axis.text = element_text(size = 14),
                                       legend.title = element_text(size = 14),
                                       legend.text = element_text(size = 12),
                                       strip.text = element_text(size = 25, face = "bold"), 
                                       panel.grid = element_blank(),
                                       panel.border = element_blank())

ggsave("combined_plot.png", combined_plot, width = 16, height = 14, dpi = 300)


#plot interaction between Sensitivity to Punishment and Partner
ef2 <- effect(term = "Partner_predictor*SP.z", apim_quest2, multiline = TRUE)
fig3 <- as.data.frame(ef2)
Figure3 <- ggplot(fig3, aes(SP.z, fit, group = Partner_predictor)) +
  geom_line(aes(color = Partner_predictor, linetype = Partner_predictor), alpha = 1, size = 1.2) +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se, fill = Partner_predictor), alpha = 0.3) +
  labs(x = expression(bold("Sensitivity to punishment (SP z scores)")), y = expression(bold("Probability of Aggression")), 
       title = expression(bold("Partner effect X Sensitivity to punishment on Aggression"))) +
  scale_color_manual(values = c("blue", "red"), name = expression(bold("Partner effect")), 
                     labels = c("No-provocation", "Provocation")) +
  scale_fill_manual(values = c("blue", "red"), name = expression(bold("95% CI")), 
                    labels = c("No-provocation", "Provocation")) +
  scale_linetype_manual(values = c("solid", "dashed"), name = expression(bold("Partner effect")), 
                        labels = c("No-provocation", "Provocation")) + theme_prism() +
  theme(legend.position = "right",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.3))
plot(Figure3)

#plot effect of SQ.z (real)
ef1 <- effect("SQ.z", apim_quest2, KR=T)
y <- as.data.frame(ef1)
g <- ggplot(y, aes(SQ.z, fit)) + geom_line(size = 1.2, aes(color = "blue4")) + geom_ribbon(aes(ymin=fit-se, ymax=fit+se), fill = "blue4", alpha = 0.3)
g1 <- g + theme_prism() + theme(axis.text=element_text(size=14), axis.text.x = element_text(face="bold"),
                axis.title=element_text(size=16,face="bold")) + theme(plot.title = element_text(size=20, face = "bold", hjust = 0.5))
A1 <- g1 + labs(x="Selfishness (SQ z scores)", y="Predicted probability of Aggression") + ggtitle("Effect of selfishness (SQ) on Aggression") + scale_color_manual(values=c("blue4")) + theme(legend.position = "none")
plot(A1)

#plot effect of SR.z (real)
ef1 <- effect("SR.z", apim_quest2, KR=T)
y <- as.data.frame(ef1)
g <- ggplot(y, aes(SR.z, fit)) + geom_line(size = 1.2, aes(color = "blue4")) + geom_ribbon(aes(ymin=fit-se, ymax=fit+se), fill = "blue4", alpha = 0.3)
g1 <- g + theme_prism() + theme(axis.text=element_text(size=14), axis.text.x = element_text(face="bold"),
                axis.title=element_text(size=16,face="bold")) + theme(plot.title = element_text(size=20, face = "bold", hjust = 0.5))
B1<- g1 + labs(x="Sensitivity to Reward (SR z scores)", y="Predicted probability of Aggression") + ggtitle("Effect of sensitivity to reward (SR) on Aggression") + scale_color_manual(values=c("blue4")) + theme(legend.position = "none")
plot(B1)

C1 <- ggplot() + theme_void() + theme(panel.background = element_rect(fill = "white", color = NA),
                                      plot.background = element_rect(fill = "white", color = NA),
                                      plot.margin = margin(30, 30, 30, 30))

ggsave("Figure3.tiff", Figure3, width = 10, height = 8, dpi = 300)

#plot effect of BPAQ,z (sim)
ef1 <- effect("BPAQ.z", apim_quest1, KR=T)
y <- as.data.frame(ef1)
g <- ggplot(y, aes(BPAQ.z, fit)) + geom_line(size = 1.2, aes(color = "blue4")) + geom_ribbon(aes(ymin=fit-se, ymax=fit+se), fill = "blue4", alpha = 0.3)
g1 <- g +  theme_prism() + theme(axis.text=element_text(size=14), axis.text.x = element_text(face="bold"),
                                 axis.title=element_text(size=16,face="bold")) + theme(plot.title = element_text(size=20, face = "bold", hjust = 0.5))
C2 <- g1 + labs(x="Trait aggression (BPAQ z scores)", y="Predicted probability of Aggression") + ggtitle("Effect of trait aggression (BPAQ) on Aggression") + scale_color_manual(values=c("blue4")) + theme(legend.position = "none")
plot(C2)

#plot effect of SQ.z (sim)
ef1 <- effect("SQ.z", apim_quest1, KR=T)
y <- as.data.frame(ef1)
g <- ggplot(y, aes(SQ.z, fit)) + geom_line(size = 1.2, aes(color = "blue4")) + geom_ribbon(aes(ymin=fit-se, ymax=fit+se), fill = "blue4", alpha = 0.3)
g1 <- g + theme_prism() + theme(axis.text=element_text(size=14), axis.text.x = element_text(face="bold"),
                                axis.title=element_text(size=16,face="bold")) + theme(plot.title = element_text(size=20, face = "bold", hjust = 0.5))
A2 <- g1 + labs(x="Selfishness (SQ z scores)", y="Predicted probability of Aggression") + ggtitle("Effect of selfishness (SQ) on Aggression") + scale_color_manual(values=c("blue4")) + theme(legend.position = "none")
plot(A2)

#plot effect of SR.z (sim)
ef1 <- effect("SR.z", apim_quest1, KR=T)
y <- as.data.frame(ef1)
g <- ggplot(y, aes(SR.z, fit)) + geom_line(size = 1.2, aes(color = "blue4")) + geom_ribbon(aes(ymin=fit-se, ymax=fit+se), fill = "blue4", alpha = 0.3)
g1 <- g + theme_prism() + theme(axis.text=element_text(size=14), axis.text.x = element_text(face="bold"),
                                axis.title=element_text(size=16,face="bold")) + theme(plot.title = element_text(size=20, face = "bold", hjust = 0.5))
B2<- g1 + labs(x="Sensitivity to Reward (SR z scores)", y="Predicted probability of Aggression") + ggtitle("Effect of sensitivity to reward (SR) on Aggression") + scale_color_manual(values=c("blue4")) + theme(legend.position = "none")
plot(B2)

# Combine plots
A1 <- A1 + labs(tag = "A)") + theme(legend.position = "none", plot.margin = margin(10, 10, 10, 10))
B1 <- B1 + labs(tag = "C)") + theme(legend.position = "none", plot.margin = margin(10, 10, 10, 10))
C1 <- C1 + labs(tag = "E)") + theme(legend.position = "none", plot.margin = margin(10, 10, 10, 10))
A2 <- A2 + labs(tag = "B)") + theme(legend.position = "none", plot.margin = margin(10, 10, 10, 10))
B2 <- B2 + labs(tag = "D)") + theme(legend.position = "none", plot.margin = margin(10, 10, 10, 10))
C2 <- C2 + labs(tag = "F)") + theme(legend.position = "none", plot.margin = margin(10, 10, 10, 10))
row1 <- A1 + A2
row2 <- B1 + B2
row3 <- C1 + C2

final_plot <- (row1 / row2 / row3) + plot_layout(guides = "collect") &
               theme(legend.position = "none", 
                plot.margin = margin(t = 10, r = 30, b = 10, l = 10), axis.text = element_text(size = 12),
                axis.title = element_text(size = 14), plot.title = element_text(size = 14, face = "bold"),
                strip.text = element_text(size = 14, face = "bold"),
                plot.tag = element_text(face = "bold", size = 14))
print(final_plot)
ggsave("final_plot1.png", final_plot, width = 13, height = 12, dpi = 300)


#______________________________NEURAL MODELS________________________________________________________________________________________________________________________________________

# ________________________Null models selection_______________________________________________________________________________________________________________________________________

null_func1 <- lmer(rTPJ_outcome.z ~ 1 + (1|Pair_id), data = hyharm)
null_func2 <- lmer(rTPJ_outcome.z ~ 1 + (1 + Trial.z|Pair_id), data = hyharm)

anova(null_func1, null_func2)

null_func1 <- lmer(dmPFC_outcome.z ~ 1 + (1|Pair_id), data = hyharm)
null_func2 <- lmer(dmPFC_outcome.z ~ 1 + (1 + Trial.z|Pair_id), data = hyharm)
anova(null_func1, null_func2)

null_func1 <- lmer(Precuneus_outcome.z ~ 1 + (1|Pair_id), data = hyharm)
null_func2 <- lmer(Precuneus_outcome.z ~ 1 + (1 + Trial.z|Pair_id), data = hyharm)
anova(null_func1, null_func2)

shapiro.test(residuals(null_func1))
shapiro.test(residuals(null_func2))

# ________________________Neural models_______________________________________________________________________________________________________________________________________

m_rTPJ <- rlmer(rTPJ_outcome.z ~ actor_rTPJ.z + partner_rTPJ.z + (1|Pair_id), 
                data = hyharm, na.action = na.omit)
tab_model(m_rTPJ, show.se = TRUE, show.stat = "t")
tab_model(m_rTPJ,show.se = TRUE,show.stat = TRUE,show.p = TRUE,show.ci = FALSE, file = "model_table_rTPJ.doc")


m_dmPFC <- rlmer(dmPFC_outcome.z ~ actor_dmPFC.z + partner_dmPFC.z + (1+Trial.z|Pair_id), #real
                 data = hyharm, na.action = na.omit)
tab_model(m_dmPFC, show.se = TRUE, show.stat = "t")
tab_model(m_dmPFC,show.se = TRUE,show.stat = TRUE,show.p = TRUE,show.ci = FALSE, file = "model_table_dmPFC.doc")


m_dmPFC <- rlmer(dmPFC_outcome.z ~ actor_dmPFC.z + partner_dmPFC.z + (1|Pair_id),  #sim
                 data = hyharm, na.action = na.omit)
tab_model(m_dmPFC, show.se = TRUE, show.stat = "t")
tab_model(m_dmPFC,show.se = TRUE,show.stat = TRUE,show.p = TRUE,show.ci = FALSE, file = "model_table_dmPFC.doc")


m_PCu<- rlmer(Precuneus_outcome.z ~ actor_Precuneus.z + partner_Precuneus.z + (1|Pair_id),
              data = hyharm, na.action = na.omit)
tab_model(m_PCu, show.se = TRUE, show.stat = "t")
tab_model(m_PCu,show.se = TRUE,show.stat = TRUE,show.p = TRUE,show.ci = FALSE, file = "model_table_PCu.doc")

#----------------------------------------Brain-behavioral model----------------------------------------------

apim_brain_behav <- glmer(Actor_outcome ~ actor_rTPJ.z + partner_rTPJ.z + actor_dmPFC.z + partner_dmPFC.z + actor_Precuneus.z
                          + partner_Precuneus.z ++ (1|Pair_id), data = hyharm, family = binomial, na.action = na.omit, 
                          control = glmerControl(optimizer = "bobyqa"))
tab_model(apim_brain_behav)
summary(apim_brain_behav)