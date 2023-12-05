## Analysis code for TB Japan by Kawatsu et al. 2023
## Created by A Schwalb
## Distributed under CC BY 4.0

# Packages ====
pacman::p_load(rio, here, tidyverse, ggplot2, scales, cowplot, gridExtra, RColorBrewer, patchwork)

# Data ====
TBnot <- import(here("data","TBnot.xlsx")) # TB notifications
TBprev <- import(here("data","TBprev.xlsx")) # TB prevalence

# Data curation ====
TBnot$pop <- round(TBnot$pop, digits = 0) # Round population data
TBprev$tb <- round(TBprev$tb, digits = 0) # Round prevalence data

# Uncertainty estimates
# a. TB notifications
for(i in 1:nrow(TBnot)) { # For loop for estimating uncertainty
  prop <- prop.test(TBnot$tb[i],TBnot$pop[i], conf.level = 0.95)
  TBnot$value[i] <- prop$estimate*1e+05 # Point estimate
  TBnot$lower[i] <- prop$conf.int[1]*1e+05 # Lower bound
  TBnot$upper[i] <- prop$conf.int[2]*1e+05 # Upper bound
}
rm(prop,i) # Clean objects

# b. TB prevalence
for(i in 1:nrow(TBprev)) { # For loop for estimating uncertainty
  prop <- prop.test(TBprev$tb[i],TBprev$pop[i], conf.level = 0.95)
  TBprev$value[i] <- prop$estimate*1e+05 # Point estimate
  TBprev$lower[i] <- prop$conf.int[1]*1e+05 # Lower bound
  TBprev$upper[i] <- prop$conf.int[2]*1e+05 # Upper bound
}
rm(prop,i) # Clean objects

# Data visualisation ====
TB_JPN <- TBnot %>%
  rbind(TBprev) %>%
  mutate(acats = case_when(
    age_gp %in% c("All") ~ "All",
    age_gp %in% c("0-19","20-39") ~ "0-39",
    age_gp %in% c("40-59","60+") ~ "40-60+")) %>% 
  mutate(acats = factor(acats, levels=c('All', '0-39', '40-60+'))) %>% 
  mutate(age_gp = factor(age_gp))

#display.brewer.all(colorblindFriendly = TRUE)
#display.brewer.pal(5,"RdYlBu")

acats_lab1 <- c("All age groups",
                "0-19 and 20-39 years old",
                "40-59 and over 60 years old")
names(acats_lab1) <- c("All","0-39","40-60+")

acats_lab2 <- c("All age groups",
                "0-19 and 20-39 years old",
                "40-59 and over 60 years old")
names(acats_lab2) <- c("All","0-39","40-60+")

# Figure
Fprev <- ggplot(data = filter(TB_JPN, measure == "prev"), mapping = aes(x=year, y=value, group=age_gp)) +
  facet_grid(cols = vars(acats), labeller = labeller(acats = acats_lab1)) +
  geom_line(data = filter(TB_JPN, adjust == "unadj"), aes(group=age_gp), linetype=1, linewidth=0.5, alpha=0.4) +
  geom_point(aes(shape=age_gp), size= 1, fill="black") +
  geom_errorbar(aes(ymin=lower, ymax=upper, group=age_gp), linewidth = 0.5, alpha = 0.8, width = 0.8) +
  geom_line(data = filter(TB_JPN, adjust == "adj"), aes(group=age_gp), linetype=2, linewidth=0.5, alpha=0.4) +
  geom_point(data = filter(TB_JPN,adjust == "adj"), aes(shape=age_gp), size= 1,) +
  geom_errorbar(data = filter(TB_JPN, adjust == "adj"), aes(ymin=lower, ymax=upper, group=age_gp), linewidth = 0.5, alpha = 0.8, width = 0.8) +
  scale_shape_manual(values=c(23, 25, 22, 24, 21)) +
  scale_x_continuous("Year", expand=c(0, 0), limits = c(1952.5, 1973.5), breaks = c(1953,1958,1963,1968,1973)) +
  scale_y_continuous(expand=c(0, 0), limits=c(0,8000), breaks = seq(0,8000,1000)) +
  scale_colour_grey(start = 0.1, end = 0.6) +
  labs(y=expression(atop(bold("TB prevalence"),atop(italic("Rate per 100,000 inhabitants"))))) +
  theme_bw() +
  theme(legend.position="none", axis.text.x = element_text(size=6),
        axis.title.x=element_blank(), panel.spacing = unit(4,"mm")) 

Fnot <- ggplot(data = filter(TB_JPN, measure == "not"), mapping = aes(x=year, y=value, group=age_gp)) +
  facet_grid(cols = vars(acats), labeller = labeller(acats = acats_lab2)) +
  geom_point(aes(shape=age_gp), size= 1, fill="black") +
  geom_line(linetype=1, linewidth=0.5, alpha=0.4) +
  geom_ribbon(aes(ymin=lower, ymax=upper), colour = NA, alpha=0.3) +
  geom_smooth(data = filter(TB_JPN, measure == "not" & (acats %in% c("All", "0-39"))), 
              method="glm", method.args=list(family=gaussian(link="log")),
              colour='black', linetype=2, linewidth=0.2, alpha=0.3) +
  geom_smooth(data = filter(TB_JPN, measure == "not" & acats == "40-60+" & year >= 1960), 
              method="glm", method.args=list(family=gaussian(link="log")),
              colour='black', linetype=2, linewidth=0.2, alpha=0.3) +
  scale_shape_manual(values=c(23, 25, 22, 24, 21)) +
  scale_x_continuous("Year", expand=c(0, 0), limits = c(1949.5, 1980.5), breaks = seq(1950,1980,5)) +
  scale_y_continuous(expand=c(0, 0), limits=c(0,1250), breaks = seq(0,1200,200)) +
  scale_colour_grey() +
  scale_fill_grey() +
  labs(shape="Age group", y=expression(atop(bold("TB notifications"),atop(italic("Rate per 100,000 inhabitants"))))) +
  theme_bw() +
  theme(legend.position="bottom", axis.text.x = element_text(size=6), 
        legend.key.size = unit(10,"points"), panel.spacing = unit(4,"mm"))

Fig <- Fprev / Fnot

png("plots/TB_JPN.png", units="cm", width=20, height=18, res=300, pointsize=2)
print(Fig)
dev.off()

# Exploring decline
TB_JPNfilt <- filter(TB_JPN, measure == "not" & age_gp == "20-39")
TB_JPNfilt <- filter(TB_JPN, measure == "not" & age_gp == "60+" & year >= 1960)

model <- glm(value ~ year, data = TB_JPNfilt, family = gaussian(link = "log"))

coeff <- coef(model)
rate <- -coeff["year"]
ci <- confint(model)["year", ]

print(paste("Rate of Decline:", rate))
print(paste("Confidence Interval:", ci))
