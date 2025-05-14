
library(tidyverse)
library(openxlsx)
library(scales)

source("code/LCI_HatcheryTM_analysis.R")

# Set Theme ---------------------------------------------------------------

theme_set(theme_bw() + 
            theme(text = element_text(family = "serif"),
                  plot.title = element_text(size = 11, hjust = 0.5),
                  legend.title = element_text(size = 11),
                  legend.text = element_text(size = 11),
                  strip.text = element_text(size = 11),
                  axis.title = element_text(size = 11),
                  axis.text = element_text(size = 11),
                  plot.caption = element_text(size = 11, lineheight = 1, hjust = -1),
                  panel.grid.major.y = element_line(color = "gray90", linetype = 1, linewidth = 0.5),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  strip.background = element_blank(),
                  legend.position = "inside",
                  legend.position.inside = c(0.8,0.15)))

# catch.comparison.HATCHAREA.SW.unsampled <- harvest.N_i.SW %>% 
#   unite("ID",c(Year,StatWk,Gear,Species,Source), sep = "_") %>% 
#   filter(ID %in% unsampled.stratum.SW$ID) %>% 
#   separate(ID, into = c("Year","StatWk","Gear","Species","Source"), sep = "_") %>% 
#   group_by(Species,Gear,Year,StatWk,Source) %>% 
#   summarize(Catch = sum(N_i)) %>% 
#   filter(Source != "HCR") %>% 
#   left_join(select(catch.comparison.HATCHAREA,-Catch),
#             by = join_by(Species,Gear,Year,Source),
#             relationship = "many-to-many") %>% 
#   mutate(Condition = "Unsampled")

figures.df <- catch.comparison.HATCHAREA.SW %>% 
  # mutate(Condition = "Sampled") %>% 
  # full_join(catch.comparison.HATCHAREA.SW.unsampled,
  #           by = join_by(Species, Gear, Year, StatWk, Source, Catch, HatcheryArea, C.hat_H, C.hat_H_VAR, C.hat_H_se, lower95CI,
  #                        upper95CI, HatcheryProportion, Condition)) %>% 
  select(-c(C.hat_H,C.hat_H_VAR,C.hat_H_se,lower95CI,upper95CI)) %>%
  pivot_wider(names_from = HatcheryArea, values_from = HatcheryProportion) %>% 
  mutate("Not Marked" = 1 - (LCI + PWS + KOD + Other)) %>% 
  pivot_longer(cols = c(LCI,PWS,KOD,Other,"Not Marked"), names_to = "HatcheryArea", values_to = "CatchProportion") %>% 
  mutate(C.hat_H = round(Catch*CatchProportion,0),
         HatcheryArea = as.factor(HatcheryArea)) %>% 
  mutate(HatcheryArea = fct_relevel(HatcheryArea,c("Other","PWS","KOD","LCI","Not Marked")))

# Figure 6 ----------------------------------------------------------------
## Sockeye CCP SGN

fig6 <- figures.df %>% 
  filter(Species == "Sockeye" & Gear == "SGN" & Source == "CCP") %>% 
  ggplot(aes(x = StatWk, y = C.hat_H/1000)) +
  geom_col(aes(fill = HatcheryArea), color = "black") +
  ylab("Harvest (thousands)") +
  xlab("Statistical Week") +
  labs(title = "SGN Sockeye Salmon", fill = "Mark Legend") +
  scale_x_discrete(limits = factor(seq(23,33,1)), breaks = seq(23,33,1)) +
  scale_y_continuous(limits = c(0,5.3), breaks = seq(0,5,1), expand = c(0,0)) +
  scale_fill_grey(start = 0, end = 1) +
  facet_wrap(facets = vars(Year), ncol = 2, scales = "free_x")

# ggsave("output/figures/fig6.png", plot = fig6, dpi = "retina", width = 6.5, height = 8, units = "in")

# Figure 7 ----------------------------------------------------------------
## Sockeye CCP PS

fig7 <- figures.df %>% 
  filter(Species == "Sockeye" & Gear == "PS" & Source == "CCP") %>% 
  ggplot(aes(x = StatWk, y = C.hat_H/1000)) +
  geom_col(aes(fill = HatcheryArea), color = "black") +
  ylab("Harvest (thousands)") +
  xlab("Statistical Week") +
  labs(title = "PS Sockeye Salmon", fill = "Mark Legend") +
  scale_x_discrete(limits = factor(seq(25,32,1)), breaks = seq(25,32,1)) +
  scale_y_continuous(limits = c(0,33), breaks = seq(0,30,5), expand = c(0,0)) +
  scale_fill_grey(start = 0, end = 1) +
  facet_wrap(facets = vars(Year), ncol = 2, scales = "free_x")

# ggsave("output/figures/fig7.png", plot = fig7, dpi = "retina", width = 6.5, height = 8, units = "in")

# Figure 8 ----------------------------------------------------------------
## Pink CCP SGN

fig8 <- figures.df %>% 
  filter(Species == "Pink" & Gear == "SGN" & Source == "CCP") %>% 
  ggplot(aes(x = StatWk, y = C.hat_H/1000)) +
  geom_col(aes(fill = HatcheryArea), color = "black") +
  ylab("Harvest (thousands)") +
  xlab("Statistical Week") +
  labs(title = "SGN Pink Salmon", fill = "Mark Legend") +
  scale_x_discrete(limits = factor(seq(27,33,1)), breaks = seq(27,33,1)) +
  scale_y_continuous(limits = c(0,16), breaks = seq(0,15,3), expand = c(0,0)) +
  scale_fill_grey(start = 0, end = 1) +
  facet_wrap(facets = vars(Year), ncol = 2, scales = "free_x")


# ggsave("output/figures/fig8.png", plot = fig8, dpi = "retina", width = 6.5, height = 8, units = "in")

# Figure 9 ----------------------------------------------------------------
## Pink CCP PS

fig9 <- figures.df %>% 
  filter(Species == "Pink" & Gear == "PS" & Source == "CCP") %>% 
  ggplot(aes(x = StatWk, y = C.hat_H/1000)) +
  geom_col(aes(fill = HatcheryArea), color = "black") +
  ylab("Harvest (thousands)") +
  xlab("Statistical Week") +
  labs(title = "PS Pink Salmon", fill = "Mark Legend") +
  scale_x_discrete(limits = factor(seq(27,36,1)), breaks = seq(27,36,1)) +
  scale_y_continuous(limits = c(0,150), breaks = seq(0,150,25), expand = c(0,0)) +
  scale_fill_grey(start = 0, end = 1) +
  facet_wrap(facets = vars(Year), ncol = 2, scales = "free_x")

# ggsave("output/figures/fig9.png", plot = fig9, dpi = "retina", width = 6.5, height = 8, units = "in")

# Table 2 -----------------------------------------------------------------

table2 <- otolith.TM.data %>% 
  group_by(Year, Species, Gear, Source) %>% 
  summarise(N.events = n(), N.samples = sum(Marked + NotMarked)) %>% 
  pivot_wider(names_from = Source, values_from = c(N.events, N.samples), values_fill = 0) %>% 
  mutate(N.events_Total = N.events_CCP + N.events_HCR, N.samples_Total = N.samples_CCP + N.samples_HCR) %>% 
  arrange(desc(Gear)) %>% arrange(desc(Species)) %>% arrange(Year) %>% 
  select(Year, Species, Gear, N.events_CCP, N.samples_CCP, N.events_HCR, N.samples_HCR, N.events_Total, N.samples_Total)


# Table 3 -----------------------------------------------------------------

table3 <- sampled.harvest.CLEAN %>% 
  filter(Species == "Sockeye") %>% 
  mutate(Sampled_Harvest = ifelse(!is.na(HomerID), CommercialHarvest, NA),
         Unsampled_Harvest = ifelse(is.na(HomerID), CommercialHarvest, NA)) %>% 
  group_by(Year, Source, Gear, StatWk, StatArea) %>% 
  summarise(Sampled_Harvest = unique(Sampled_Harvest), Unsampled_Harvest = unique(Unsampled_Harvest)) %>%
  group_by(Year, Source, Gear) %>% 
  summarise(n_Sampled = sum(!is.na(Sampled_Harvest)),
            Harvest_Sampled = sum(Sampled_Harvest, na.rm = T),
            n_Unsampled = sum(!is.na(Unsampled_Harvest)),
            Harvest_Unsampled = sum(Unsampled_Harvest, na.rm = T)) %>% 
  mutate(Harvest_Total = Harvest_Sampled + Harvest_Unsampled,
         Percent_Sampled = round(Harvest_Sampled/Harvest_Total,3)) %>% 
  arrange(desc(Gear)) %>% arrange(desc(Source)) %>%arrange(Year)

# Table 4 -----------------------------------------------------------------

table4 <- sampled.harvest.CLEAN %>% 
  filter(Species == "Pink") %>% 
  mutate(Sampled_Harvest = ifelse(!is.na(HomerID), CommercialHarvest, NA),
         Unsampled_Harvest = ifelse(is.na(HomerID), CommercialHarvest, NA)) %>% 
  group_by(Year, Source, Gear, StatWk, StatArea) %>% 
  summarise(Sampled_Harvest = unique(Sampled_Harvest), Unsampled_Harvest = unique(Unsampled_Harvest)) %>%
  group_by(Year, Source, Gear) %>% 
  summarise(n_Sampled = sum(!is.na(Sampled_Harvest)),
            Harvest_Sampled = sum(Sampled_Harvest, na.rm = T),
            n_Unsampled = sum(!is.na(Unsampled_Harvest)),
            Harvest_Unsampled = sum(Unsampled_Harvest, na.rm = T)) %>% 
  mutate(Harvest_Total = Harvest_Sampled + Harvest_Unsampled,
         Percent_Sampled = round(Harvest_Sampled/Harvest_Total,3)) %>% 
  arrange(desc(Gear)) %>% arrange(desc(Source)) %>%arrange(Year)

# Table 5 -----------------------------------------------------------------
hatchery_order = c("LCI","PWS","KOD","Other")

table5 <- select(table2, c(Year,Species,Gear,N.samples_CCP)) %>% 
  right_join(catch.comparison.HATCHAREA) %>% 
  filter(Species == "Sockeye" & Source == "CCP" & Gear == "SGN") %>% 
  select(-c(C.hat_H_VAR,C.hat_H_se)) %>% 
  mutate(N.samples_CCP = number(N.samples_CCP, big.mark = ","),
         lower95CI = ifelse(lower95CI < 0,0,lower95CI),
         n_est = paste0(number(C.hat_H, big.mark = ",")," (",number(lower95CI, big.mark = ","),"-",number(upper95CI, big.mark = ","),")"),
         proportion_est = number(HatcheryProportion*100, accuracy = 0.1, suffix = "%")) %>% 
  select(-c(C.hat_H,lower95CI,upper95CI,HatcheryProportion)) %>% 
  left_join(select(catch.comparison.TOTAL, c(Species,Gear,Year,Source,Catch,C.hat_H,lower95CI,upper95CI))) %>% 
  mutate(Est_TotalHatchery = paste0(number(C.hat_H, big.mark = ",")," (",number(lower95CI, big.mark = ","),"-",number(upper95CI, big.mark = ","),")"),
         Est_ProportionHatchery = number(C.hat_H*100/Catch, accuracy = 0.1, suffix = "%"),
         Est_TotalWild = paste0(number(Catch - C.hat_H, big.mark = ",")," (",number(Catch - upper95CI, big.mark = ","),"-",number(Catch - lower95CI, big.mark = ","),")"),
         Est_ProportionWild = number((Catch - C.hat_H)*100/Catch, accuracy = 0.1, suffix = "%"),
         Catch = number(Catch, big.mark = ",")) %>% 
  ungroup() %>% 
  select(-c(Species,Gear,Source,C.hat_H,lower95CI,upper95CI)) %>% 
  arrange(factor(HatcheryArea, levels = hatchery_order)) %>% arrange(Year)

# Table 6 -----------------------------------------------------------------

table6 <- select(table2, c(Year,Species,Gear,N.samples_CCP)) %>% 
  right_join(catch.comparison.HATCHAREA) %>% 
  filter(Species == "Sockeye" & Source == "CCP" & Gear == "PS") %>% 
  select(-c(C.hat_H_VAR,C.hat_H_se)) %>% 
  mutate(N.samples_CCP = number(N.samples_CCP, big.mark = ","),
         lower95CI = ifelse(lower95CI < 0,0,lower95CI),
         n_est = paste0(number(C.hat_H, big.mark = ",")," (",number(lower95CI, big.mark = ","),"-",number(upper95CI, big.mark = ","),")"),
         proportion_est = number(HatcheryProportion*100, accuracy = 0.1, suffix = "%")) %>% 
  select(-c(C.hat_H,lower95CI,upper95CI,HatcheryProportion)) %>% 
  left_join(select(catch.comparison.TOTAL, c(Species,Gear,Year,Source,Catch,C.hat_H,lower95CI,upper95CI))) %>% 
  mutate(Est_TotalHatchery = paste0(number(C.hat_H, big.mark = ",")," (",number(lower95CI, big.mark = ","),"-",number(upper95CI, big.mark = ","),")"),
         Est_ProportionHatchery = number(C.hat_H*100/Catch, accuracy = 0.1, suffix = "%"),
         Est_TotalWild = paste0(number(Catch - C.hat_H, big.mark = ",")," (",number(Catch - upper95CI, big.mark = ","),"-",number(Catch - lower95CI, big.mark = ","),")"),
         Est_ProportionWild = number((Catch - C.hat_H)*100/Catch, accuracy = 0.1, suffix = "%"),
         Catch = number(Catch, big.mark = ",")) %>% 
  ungroup() %>% 
  select(-c(Species,Gear,Source,C.hat_H,lower95CI,upper95CI)) %>% 
  arrange(factor(HatcheryArea, levels = hatchery_order)) %>% arrange(Year)

# Table 7 -----------------------------------------------------------------

table7 <- select(table2, c(Year,Species,Gear,N.samples_CCP)) %>% 
  right_join(catch.comparison.HATCHAREA) %>% 
  filter(Species == "Pink" & Source == "CCP" & Gear == "SGN") %>% 
  select(-c(C.hat_H_VAR,C.hat_H_se)) %>% 
  mutate(N.samples_CCP = number(N.samples_CCP, big.mark = ","),
         lower95CI = ifelse(lower95CI < 0,0,lower95CI),
         n_est = paste0(number(C.hat_H, big.mark = ",")," (",number(lower95CI, big.mark = ","),"-",number(upper95CI, big.mark = ","),")"),
         proportion_est = number(HatcheryProportion*100, accuracy = 0.1, suffix = "%")) %>% 
  select(-c(C.hat_H,lower95CI,upper95CI,HatcheryProportion)) %>% 
  left_join(select(catch.comparison.TOTAL, c(Species,Gear,Year,Source,Catch,C.hat_H,lower95CI,upper95CI))) %>% 
  mutate(Est_TotalHatchery = paste0(number(C.hat_H, big.mark = ",")," (",number(lower95CI, big.mark = ","),"-",number(upper95CI, big.mark = ","),")"),
         Est_ProportionHatchery = number(C.hat_H*100/Catch, accuracy = 0.1, suffix = "%"),
         Est_TotalWild = paste0(number(Catch - C.hat_H, big.mark = ",")," (",number(Catch - upper95CI, big.mark = ","),"-",number(Catch - lower95CI, big.mark = ","),")"),
         Est_ProportionWild = number((Catch - C.hat_H)*100/Catch, accuracy = 0.1, suffix = "%"),
         Catch = number(Catch, big.mark = ",")) %>% 
  ungroup() %>% 
  select(-c(Species,Gear,Source,C.hat_H,lower95CI,upper95CI)) %>% 
  arrange(factor(HatcheryArea, levels = hatchery_order)) %>% arrange(Year)

# Table 8 -----------------------------------------------------------------

table8 <- select(table2, c(Year,Species,Gear,N.samples_CCP)) %>% 
  right_join(catch.comparison.HATCHAREA) %>% 
  filter(Species == "Pink" & Source == "CCP" & Gear == "PS") %>% 
  select(-c(C.hat_H_VAR,C.hat_H_se)) %>% 
  mutate(N.samples_CCP = number(N.samples_CCP, big.mark = ","),
         lower95CI = ifelse(lower95CI < 0,0,lower95CI),
         n_est = paste0(number(C.hat_H, big.mark = ",")," (",number(lower95CI, big.mark = ","),"-",number(upper95CI, big.mark = ","),")"),
         proportion_est = number(HatcheryProportion*100, accuracy = 0.1, suffix = "%")) %>% 
  select(-c(C.hat_H,lower95CI,upper95CI,HatcheryProportion)) %>% 
  left_join(select(catch.comparison.TOTAL, c(Species,Gear,Year,Source,Catch,C.hat_H,lower95CI,upper95CI))) %>% 
  mutate(Est_TotalHatchery = paste0(number(C.hat_H, big.mark = ",")," (",number(lower95CI, big.mark = ","),"-",number(upper95CI, big.mark = ","),")"),
         Est_ProportionHatchery = number(C.hat_H*100/Catch, accuracy = 0.1, suffix = "%"),
         Est_TotalWild = paste0(number(Catch - C.hat_H, big.mark = ",")," (",number(Catch - upper95CI, big.mark = ","),"-",number(Catch - lower95CI, big.mark = ","),")"),
         Est_ProportionWild = number((Catch - C.hat_H)*100/Catch, accuracy = 0.1, suffix = "%"),
         Catch = number(Catch, big.mark = ",")) %>% 
  ungroup() %>% 
  select(-c(Species,Gear,Source,C.hat_H,lower95CI,upper95CI)) %>% 
  arrange(factor(HatcheryArea, levels = hatchery_order)) %>% arrange(Year)


# Appendix B --------------------------------------------------------------
## Sockeye C.hat_hi ----------------------------------------------------------------

# Create table of # of otoliths with HatcheryMark (h) in sample from a given stratum (n_i):
reds.sample.o_hi <- reds.clean %>% 
  select(-c(Rcvd,Preps,NotMarked,Marked,LCI,PWS,KOD,Other)) %>% # Remove unnecessary columns
  pivot_longer(cols = -c("Year","StatWk","Gear","Species","StatArea","HomerSampleID","Source"), names_to = "Hatchery", values_to = "MarkedOtoliths") %>% # Pivot table to LONG format, with one count of the # of MarkedOtoliths per STRATA*SUBDISTRICT*HATCHERYMARK combination
  replace_na(list(MarkedOtoliths = 0)) %>% # Replace NA values with 0
  rename(HomerID = HomerSampleID) %>% 
  mutate(Hatchery = as.factor(Hatchery)) %>% 
  group_by(Year,Gear,Source,StatArea,Hatchery) %>% 
  summarise(MarkedOtoliths = sum(MarkedOtoliths))

# Create table of total # of otoliths sampled from a given stratum (n_i):
reds.sample.n_i <- reds.clean %>% 
  group_by(Year,Gear,Source,StatArea) %>% # Group into stratum (i)
  summarise(SampledOtoliths = sum(NotMarked) + sum(Marked)) # SampledOtoliths = total # of otoliths sampled

# Create table of total # of fish commercially harvested in stratum (i):
reds.harvest.N_i <- sampled.harvest.CLEAN %>%
  filter(Species == "Sockeye") %>% 
  group_by(Year,StatWk,Gear,Source,StatArea) %>% 
  summarise(CommercialHarvest = sum(unique(CommercialHarvest)))

# Create a table of the otolith-derived estimated of the contribution of hatchery (h) to period-gear-district stratum (i):
reds.C.hat_hi <- reds.sample.o_hi %>% 
  full_join(reds.sample.n_i,
            by = join_by(Year,Gear,Source,StatArea)) %>% 
  full_join(reds.harvest.N_i,
            by = join_by(Year,Gear,Source,StatArea),
            relationship = "many-to-many") %>% 
  filter(!is.na(Hatchery)) %>%      # Removing instances where Hatchery = NA, aka when a stratum had commercial harvest occur but no sampling occurred
  mutate(ProportionMarked = round(MarkedOtoliths/SampledOtoliths, 3), # Calculate hatchery proportion of sample
         C.hat_hi = ProportionMarked*CommercialHarvest,# EQUATION 1: Multiply hatchery proportion by the total commercial harvest for estimate hatchery proportion in commercial catch
         C.hat_hi_VAR = (CommercialHarvest^2)*(1/(SampledOtoliths-1))*(MarkedOtoliths/SampledOtoliths)*(1 - (MarkedOtoliths/SampledOtoliths))) # EQUATION 2: Variance estimate of the hatchery-derived commercial harvest contribution

## Sockeye C.hat_Sh ----------------------------------------------------------------

reds.C.hat_Sh <- reds.C.hat_hi %>% 
  group_by(Year,Gear,Source,Hatchery) %>% 
  summarize(C.hat_Sh = sum(round(C.hat_hi,0)), # EQUATION 3: Estimate of contribution of hatchery mark (h) to all sampled CCP harvests.
            C.hat_Sh_VAR = sum(C.hat_hi_VAR)) # EQUATION 4: Variance estimate of C.hat_Sh

## Sockeye C.hat_Uh ----------------------------------------------------------------

reds.unsampled.N_i <- reds.sample.o_hi %>%
  full_join(reds.sample.n_i,
            by = join_by(Year,Gear,Source,StatArea)) %>%
  full_join(reds.harvest.N_i,
            by = join_by(Year,Gear,Source,StatArea),
            relationship = "many-to-many") %>%
  filter(is.na(Hatchery)) %>% # Selecting instances where Hatchery = NA, aka when a stratum had commercial harvest occur but no sampling occurred
  mutate(N_i = CommercialHarvest) %>% 
  select(-c(Hatchery,MarkedOtoliths,SampledOtoliths,CommercialHarvest))

reds.unsampled.C.hat_Uh <- reds.C.hat_hi %>% 
  group_by(Year,Gear,Source,Hatchery) %>% 
  summarise(C.hat_hj = sum(round(C.hat_hi,0)), N_j = sum(CommercialHarvest),Proportion = C.hat_hj/N_j)

reds.C.hat_Uh <- reds.unsampled.N_i %>% 
  left_join(reds.unsampled.C.hat_Uh,
            by = join_by(Year,Gear,Source),
            relationship = "many-to-many") %>% 
  group_by(Year,Gear,Source,Hatchery) %>% 
  summarise(C.hat_Uh = round(sum(N_i)*unique(Proportion),0),
            C.hat_Uh_VAR = sum(N_i^2 * (1/(sum(N_j)-1)) * (sum(C.hat_hj)/sum(N_j)) * (1 - (sum(C.hat_hj)/sum(N_j)))   ))

# NA's occur in this data frame when there is commercial harvest from a stratum but no fish were sampled during that Year_StatWk from which to estimate hatchery proportion for that stratum
# New data set is created to save this observations and estimate the hatchery contribution to them in Part 6
reds.unsampled.stratum <- reds.C.hat_Uh %>% 
  filter(is.na(Hatchery)) %>% 
  select(Year,Gear,Source)

reds.C.hat_Uh <- reds.C.hat_Uh %>% 
  filter(!is.na(Hatchery))

## Sockeye C.hat_h -----------------------------------------------------------------

reds.C.hat_h <- reds.C.hat_Sh %>% 
  full_join(reds.C.hat_Uh,
            by = join_by(Year,Gear,Source,Hatchery)) %>% 
  replace_na(list(C.hat_Uh = 0)) %>% 
  mutate(C.hat_h = C.hat_Sh + C.hat_Uh)

## Sockeye C.hat_H -----------------------------------------------------------------

appendixB <- reds.C.hat_h %>% 
  group_by(Hatchery) %>% 
  summarise(C.hat_H = sum(C.hat_h, na.rm = T))

# Appendix C --------------------------------------------------------------
## Pink C.hat_hi ----------------------------------------------------------------

# Create table of # of otoliths with HatcheryMark (h) in sample from a given stratum (n_i):
pink.sample.o_hi <- pink.clean %>% 
  select(-c(Rcvd,Preps,NotMarked,Marked,LCI,PWS,KOD,Other)) %>% # Remove unnecessary columns
  pivot_longer(cols = -c("Year","StatWk","Gear","Species","StatArea","HomerSampleID","Source"), names_to = "Hatchery", values_to = "MarkedOtoliths") %>% # Pivot table to LONG format, with one count of the # of MarkedOtoliths per STRATA*SUBDISTRICT*HATCHERYMARK combination
  replace_na(list(MarkedOtoliths = 0)) %>% # Replace NA values with 0
  rename(HomerID = HomerSampleID) %>% 
  mutate(Hatchery = as.factor(Hatchery)) %>% 
  group_by(Year,Gear,Source,StatArea,Hatchery) %>% 
  summarise(MarkedOtoliths = sum(MarkedOtoliths))

# Create table of total # of otoliths sampled from a given stratum (n_i):
pink.sample.n_i <- pink.clean %>% 
  group_by(Year,Gear,Source,StatArea) %>% # Group into stratum (i)
  summarise(SampledOtoliths = sum(NotMarked) + sum(Marked)) # SampledOtoliths = total # of otoliths sampled

# Create table of total # of fish commercially harvested in stratum (i):
pink.harvest.N_i <- sampled.harvest.CLEAN %>%
  filter(Species == "Pink") %>% 
  group_by(Year,StatWk,Gear,Source,StatArea) %>% 
  summarise(CommercialHarvest = sum(unique(CommercialHarvest)))

# Create a table of the otolith-derived estimated of the contribution of hatchery (h) to period-gear-district stratum (i):
pink.C.hat_hi <- pink.sample.o_hi %>% 
  full_join(pink.sample.n_i,
            by = join_by(Year,Gear,Source,StatArea)) %>% 
  full_join(pink.harvest.N_i,
            by = join_by(Year,Gear,Source,StatArea),
            relationship = "many-to-many") %>% 
  filter(!is.na(Hatchery)) %>%      # Removing instances where Hatchery = NA, aka when a stratum had commercial harvest occur but no sampling occurred
  mutate(ProportionMarked = round(MarkedOtoliths/SampledOtoliths, 3), # Calculate hatchery proportion of sample
         C.hat_hi = ProportionMarked*CommercialHarvest,# EQUATION 1: Multiply hatchery proportion by the total commercial harvest for estimate hatchery proportion in commercial catch
         C.hat_hi_VAR = (CommercialHarvest^2)*(1/(SampledOtoliths-1))*(MarkedOtoliths/SampledOtoliths)*(1 - (MarkedOtoliths/SampledOtoliths))) # EQUATION 2: Variance estimate of the hatchery-derived commercial harvest contribution

## Pink C.hat_Sh ----------------------------------------------------------------

pink.C.hat_Sh <- pink.C.hat_hi %>% 
  group_by(Year,Gear,Source,Hatchery) %>% 
  summarize(C.hat_Sh = sum(round(C.hat_hi,0)), # EQUATION 3: Estimate of contribution of hatchery mark (h) to all sampled CCP harvests.
            C.hat_Sh_VAR = sum(C.hat_hi_VAR)) # EQUATION 4: Variance estimate of C.hat_Sh

## Pink C.hat_Uh ----------------------------------------------------------------

pink.unsampled.N_i <- pink.sample.o_hi %>%
  full_join(pink.sample.n_i,
            by = join_by(Year,Gear,Source,StatArea)) %>%
  full_join(pink.harvest.N_i,
            by = join_by(Year,Gear,Source,StatArea),
            relationship = "many-to-many") %>%
  filter(is.na(Hatchery)) %>% # Selecting instances where Hatchery = NA, aka when a stratum had commercial harvest occur but no sampling occurred
  mutate(N_i = CommercialHarvest) %>% 
  select(-c(Hatchery,MarkedOtoliths,SampledOtoliths,CommercialHarvest))

pink.unsampled.C.hat_Uh <- pink.C.hat_hi %>% 
  group_by(Year,Gear,Source,Hatchery) %>% 
  summarise(C.hat_hj = sum(round(C.hat_hi,0)), N_j = sum(CommercialHarvest),Proportion = C.hat_hj/N_j)

pink.C.hat_Uh <- pink.unsampled.N_i %>% 
  left_join(pink.unsampled.C.hat_Uh,
            by = join_by(Year,Gear,Source),
            relationship = "many-to-many") %>% 
  group_by(Year,Gear,Source,Hatchery) %>% 
  summarise(C.hat_Uh = round(sum(N_i)*unique(Proportion),0),
            C.hat_Uh_VAR = sum(N_i^2 * (1/(sum(N_j)-1)) * (sum(C.hat_hj)/sum(N_j)) * (1 - (sum(C.hat_hj)/sum(N_j)))   ))

# NA's occur in this data frame when there is commercial harvest from a stratum but no fish were sampled during that Year_StatWk from which to estimate hatchery proportion for that stratum
# New data set is created to save this observations and estimate the hatchery contribution to them in Part 6
pink.unsampled.stratum <- pink.C.hat_Uh %>% 
  filter(is.na(Hatchery)) %>% 
  select(Year,Gear,Source)

pink.C.hat_Uh <- pink.C.hat_Uh %>% 
  filter(!is.na(Hatchery))

## Pink C.hat_h -----------------------------------------------------------------

pink.C.hat_h <- pink.C.hat_Sh %>% 
  full_join(pink.C.hat_Uh,
            by = join_by(Year,Gear,Source,Hatchery)) %>% 
  replace_na(list(C.hat_Uh = 0)) %>% 
  mutate(C.hat_h = C.hat_Sh + C.hat_Uh)

## Pink C.hat_H -----------------------------------------------------------------

appendixC <- pink.C.hat_h %>% 
  group_by(Hatchery) %>% 
  summarise(C.hat_H = sum(C.hat_h, na.rm = T))

# Appendix D --------------------------------------------------------------

appendixD <- C.hat_hi %>% 
  group_by(Year,Gear,Species,Source,StatArea,HatcheryArea) %>% 
  summarise(n_i = unique(n_i), Proportion = unique(o_hi)/unique(n_i), N_i = sum(N_i)) %>% 
  mutate(Proportion = round(Proportion,3),Contribution = round(Proportion*N_i,0)) %>% 
  pivot_wider(names_from = HatcheryArea, values_from = c(Proportion,Contribution))

## Sockeye CCP SGN
appendixD1 <- appendixD %>% 
  ungroup() %>% 
  filter(Species == "Sockeye" & Source == "CCP" & Gear == "SGN") %>% 
  arrange(StatArea) %>% 
  group_by(Year,Species,Source,Gear,StatArea,N_i,n_i,Proportion_LCI,Contribution_LCI,Proportion_PWS,Contribution_PWS,Proportion_KOD,Contribution_KOD,Proportion_Other,Contribution_Other) %>% 
  summarise(Proportion_Wild = 1 - sum(Proportion_LCI,Proportion_PWS,Proportion_KOD,Proportion_Other),
            Contribution_Wild = N_i - sum(Contribution_LCI,Contribution_PWS,Contribution_KOD,Contribution_Other))

## Sockeye CCP PS
appendixD2 <- appendixD %>% 
  ungroup() %>% 
  filter(Species == "Sockeye" & Source == "CCP" & Gear == "PS") %>% 
  arrange(StatArea) %>% 
  group_by(Year,Species,Source,Gear,StatArea,N_i,n_i,Proportion_LCI,Contribution_LCI,Proportion_PWS,Contribution_PWS,Proportion_KOD,Contribution_KOD,Proportion_Other,Contribution_Other) %>% 
  summarise(Proportion_Wild = 1 - sum(Proportion_LCI,Proportion_PWS,Proportion_KOD,Proportion_Other),
            Contribution_Wild = N_i - sum(Contribution_LCI,Contribution_PWS,Contribution_KOD,Contribution_Other))

## Sockeye HCR PS
appendixD3 <- appendixD %>% 
  ungroup() %>% 
  filter(Species == "Sockeye" & Source == "HCR" & Gear == "PS") %>% 
  arrange(StatArea) %>% 
  group_by(Year,Species,Source,Gear,StatArea,N_i,n_i,Proportion_LCI,Contribution_LCI,Proportion_PWS,Contribution_PWS,Proportion_KOD,Contribution_KOD,Proportion_Other,Contribution_Other) %>% 
  summarise(Proportion_Wild = 1 - sum(Proportion_LCI,Proportion_PWS,Proportion_KOD,Proportion_Other),
            Contribution_Wild = N_i - sum(Contribution_LCI,Contribution_PWS,Contribution_KOD,Contribution_Other))

## Pink CCP SGN
appendixD4 <- appendixD %>% 
  ungroup() %>% 
  filter(Species == "Pink" & Source == "CCP" & Gear == "SGN") %>% 
  arrange(StatArea) %>% 
  group_by(Year,Species,Source,Gear,StatArea,N_i,n_i,Proportion_LCI,Contribution_LCI,Proportion_PWS,Contribution_PWS,Proportion_KOD,Contribution_KOD,Proportion_Other,Contribution_Other) %>% 
  summarise(Proportion_Wild = 1 - sum(Proportion_LCI,Proportion_PWS,Proportion_KOD,Proportion_Other),
            Contribution_Wild = N_i - sum(Contribution_LCI,Contribution_PWS,Contribution_KOD,Contribution_Other))

## Pink CCP PS
appendixD5 <- appendixD %>% 
  ungroup() %>% 
  filter(Species == "Pink" & Source == "CCP" & Gear == "PS") %>% 
  arrange(StatArea) %>% 
  group_by(Year,Species,Source,Gear,StatArea,N_i,n_i,Proportion_LCI,Contribution_LCI,Proportion_PWS,Contribution_PWS,Proportion_KOD,Contribution_KOD,Proportion_Other,Contribution_Other) %>% 
  summarise(Proportion_Wild = 1 - sum(Proportion_LCI,Proportion_PWS,Proportion_KOD,Proportion_Other),
            Contribution_Wild = N_i - sum(Contribution_LCI,Contribution_PWS,Contribution_KOD,Contribution_Other))

## Pink HCR PS
appendixD6 <- appendixD %>% 
  ungroup() %>% 
  filter(Species == "Pink" & Source == "HCR" & Gear == "PS") %>% 
  arrange(StatArea) %>% 
  group_by(Year,Species,Source,Gear,StatArea,N_i,n_i,Proportion_LCI,Contribution_LCI,Proportion_PWS,Contribution_PWS,Proportion_KOD,Contribution_KOD,Proportion_Other,Contribution_Other) %>% 
  summarise(Proportion_Wild = 1 - sum(Proportion_LCI,Proportion_PWS,Proportion_KOD,Proportion_Other),
            Contribution_Wild = N_i - sum(Contribution_LCI,Contribution_PWS,Contribution_KOD,Contribution_Other))

# Appendix E --------------------------------------------------------------

appendixE <- otolith.TM.data %>% 
  group_by(Year,Species,Source,Gear,StatArea,StatWk) %>% 
  summarise(Preps = sum(Preps),Marked = sum(Marked),NotMarked = sum(NotMarked),
            LCI = sum(LCI),PWS = sum(PWS),KOD = sum(KOD),Other = sum(Other)) %>% 
  mutate(StatWk = as.factor(StatWk)) %>% 
  left_join(harvest.N_i.SW,
            by = join_by(Year,Species,Source,Gear,StatArea,StatWk)) %>% 
  mutate(n_i = Marked + NotMarked,
         Proportion_LCI = round(LCI/(Marked+NotMarked),3),
         Contribution_LCI = round(Proportion_LCI*N_i,0),
         Proportion_PWS = round(PWS/(Marked+NotMarked),3),
         Contribution_PWS = round(Proportion_PWS*N_i,0),
         Proportion_KOD = round(KOD/(Marked+NotMarked),3),
         Contribution_KOD = round(Proportion_KOD*N_i,0),
         Proportion_Other = round(Other/(Marked+NotMarked),3),
         Contribution_Other = round(Proportion_Other*N_i,0),
         Proportion_Wild = round(NotMarked/(Marked+NotMarked),3),
         Contribution_Wild = round(Proportion_Wild*N_i,0)) %>% 
  select(-c(Preps,Marked,NotMarked,LCI,PWS,KOD,Other))

## Sockeye CCP SGN
appendixE1 <- appendixE %>% 
  ungroup() %>% 
  filter(Species == "Sockeye" & Source == "CCP" & Gear == "SGN") %>% 
  arrange(StatArea)

## Sockeye CCP PS
appendixE2 <- appendixE %>% 
  ungroup() %>% 
  filter(Species == "Sockeye" & Source == "CCP" & Gear == "PS") %>% 
  arrange(StatArea)

## Sockeye HCR PS
appendixE3 <- appendixE %>% 
  ungroup() %>% 
  filter(Species == "Sockeye" & Source == "HCR" & Gear == "PS") %>% 
  arrange(StatArea)

## Pink CCP SGN
appendixE4 <- appendixE %>% 
  ungroup() %>% 
  filter(Species == "Pink" & Source == "CCP" & Gear == "SGN") %>% 
  arrange(StatArea)

## Pink CCP PS
appendixE5 <- appendixE %>% 
  ungroup() %>% 
  filter(Species == "Pink" & Source == "CCP" & Gear == "PS") %>% 
  arrange(StatArea)

## Pink HCR PS
appendixE6 <- appendixE %>% 
  ungroup() %>% 
  filter(Species == "Pink" & Source == "HCR" & Gear == "PS") %>% 
  arrange(StatArea) 

# Appendix F --------------------------------------------------------------

appendixF <- C.hat_Sh %>% 
  mutate(C.hat_Sh = round(C.hat_Sh,0),C.hat_Sh_se = round(sqrt(C.hat_Sh_VAR),0)) %>% 
  select(-C.hat_Sh_VAR) %>% 
  pivot_wider(names_from = HatcheryArea, values_from = c(C.hat_Sh,C.hat_Sh_se)) %>% 
  select(Species,Year,Source,Gear,C.hat_Sh_LCI,C.hat_Sh_se_LCI,C.hat_Sh_PWS,C.hat_Sh_se_PWS,C.hat_Sh_KOD,C.hat_Sh_se_KOD,C.hat_Sh_Other,C.hat_Sh_se_Other) %>% 
  arrange(desc(Gear)) %>% arrange(Source) %>% arrange(Year) %>% arrange(Species)

# Appendix G --------------------------------------------------------------

appendixG <- C.hat_Uh %>% 
  mutate(C.hat_Uh = round(C.hat_Uh,0),C.hat_Uh_se = round(sqrt(C.hat_Uh_VAR),0)) %>% 
  select(-C.hat_Uh_VAR) %>% 
  pivot_wider(names_from = HatcheryArea, values_from = c(C.hat_Uh,C.hat_Uh_se)) %>% 
  select(Species,Year,Source,Gear,C.hat_Uh_LCI,C.hat_Uh_se_LCI,C.hat_Uh_PWS,C.hat_Uh_se_PWS,C.hat_Uh_KOD,C.hat_Uh_se_KOD,C.hat_Uh_Other,C.hat_Uh_se_Other) %>% 
  arrange(desc(Gear)) %>% arrange(Source) %>% arrange(Year) %>% arrange(Species)

# Appendix H --------------------------------------------------------------

appendixH <- C.hat_H.HATCHAREA %>% 
  select(-C.hat_H_VAR) %>% 
  rename("C.hat_h" = C.hat_H,
         "C.hat_h_se" = C.hat_H_se) %>% 
  mutate(C.hat_h = round(C.hat_h,0),
         C.hat_h_se = round(C.hat_h_se,0),
         lower95CI = ifelse(round(lower95CI,0)<0,0,round(lower95CI,0)),
         upper95CI = round(upper95CI,0)) %>% 
  pivot_wider(names_from = HatcheryArea, values_from = c(C.hat_h,C.hat_h_se,lower95CI,upper95CI)) %>% 
  select(Species,Year,Source,Gear,C.hat_h_LCI,C.hat_h_se_LCI,lower95CI_LCI,upper95CI_LCI,C.hat_h_PWS,C.hat_h_se_PWS,lower95CI_PWS,upper95CI_PWS,C.hat_h_KOD,C.hat_h_se_KOD,lower95CI_KOD,upper95CI_KOD,C.hat_h_Other,C.hat_h_se_Other,lower95CI_Other,upper95CI_Other) %>% 
  arrange(desc(Gear)) %>% arrange(Source) %>% arrange(Year) %>% arrange(Species)

# Appendix I --------------------------------------------------------------

appendixI <- C.hat_H.TOTAL %>% 
  mutate(C.hat_H = round(C.hat_H,0),
         C.hat_H_se = round(C.hat_H_se,0),
         lower95CI = round(lower95CI,0),
         upper95CI = round(upper95CI,0)) %>% 
  select(Species,Year,Source,Gear,C.hat_H,C.hat_H_se,lower95CI,upper95CI) %>% 
  arrange(desc(Gear)) %>% arrange(Source) %>% arrange(Year) %>% arrange(Species)


# Save tables as Excel workbook -------------------------------------------

sheets <- list(
  "table2" = table2,
  "table3" = table3,
  "table4" = table4,
  "table5" = table5,
  "table6" = table6,
  "table7" = table7,
  "table8" = table8,
  "appendixB" = appendixB,
  "appendixC" = appendixC,
  "appendixD1" = appendixD1,
  "appendixD2" = appendixD2,
  "appendixD3" = appendixD3,
  "appendixD4" = appendixD4,
  "appendixD5" = appendixD5,
  "appendixD6" = appendixD6,
  "appendixE1" = appendixE1,
  "appendixE2" = appendixE2,
  "appendixE3" = appendixE3,
  "appendixE4" = appendixE4,
  "appendixE5" = appendixE5,
  "appendixE6" = appendixE6,
  "appendixF" = appendixF,
  "appendixG" = appendixG,
  "appendixH" = appendixH,
  "appendixI" = appendixI
)

# write.xlsx(sheets, "output/tables/report_tables_RAW.xlsx")
