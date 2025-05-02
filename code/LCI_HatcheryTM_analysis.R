# ---
#   title: "Analysis of LCI Thermal Mark Hatchery Otoliths, 2018-2022"
#   author: "Jonah Bacon"
#   date: 23 April 2025
# ---
  
# This code analyzes data collected from 2018-2022 as part of the Thermal Mark Hatchery Salmon Otoliths in Lower Cook Inlet project 
# (REGIONAL OPERATIONAL PLAN NO. ROP.CF.2A.2023.02; Otis et al. 2023). Data, scripts, and associated files are located in the Homer LAN network: 
# ```O:\DCF\SALMON\OTOLITH_STUDIES\1_ANALYSES\2018-2022```. Data are first cleaned, edited (when commercial harvest was misreported 
# or errors in recording occurred), and then analyzed for Pink Salmon followed by identical analysis for Sockeye Salmon.

# References:
# Otis, E. O., G. J. Hollowell, and X. Zhang. 2023. Recovery and analysis of thermal mark hatchery salmon otoliths in Lower Cook Inlet, 2023–2024. 
#     Alaska Department of Fish and Game, Division of Commercial Fisheries, Regional Operational Plan No. ROP.CF.2A.2023.02, Anchorage.

# Load Libraries ----
library(tidyverse)

# Prepare Data ----
## Load Data ----

# Load data files from ```O:\DCF\SALMON\OTOLITH_STUDIES\1_ANALYSES\2018-2022\input```.

pink.marks <- read.csv("input/PinkMarks.csv")
reds.marks <- read.csv("input/RedMarks.csv")
harvest <- read.csv("input/tblHarvest.csv")
sample <- read.csv("input/tblHomerSample.csv")
ChangeStatArea <- read.csv("input/ChangeStatArea.csv")
MissingHarvest <- read.csv("input/MissingHarvest.csv")

## Clean Data ----
### Otolith Mark Data ----

pink.marks$StatArea <- as.character(pink.marks$StatArea)
pink.clean <- pink.marks %>% 
  filter(Source %in% c("COMMERCIAL COM PROP","COST RECOVERY")) %>%
  select(-c(Agency,FisheryName,Species,SubDistrict,AStreamCode,Stock,Harvest,SampleDate,SurveySite)) %>% 
  separate(StatArea, into = c("StatArea","Extra"), sep = 5) %>% 
  mutate(StatArea = as.integer(StatArea)) %>% 
  filter(StatArea > 10000) %>% 
  select(-Extra) %>% 
  mutate(# StatArea = if_else(Gear == "SET GILLNET", "24100", StatArea),
         StatArea = ifelse(HomerSampleID == "18PSD45P",24107,StatArea), # Specific correction
         StatArea = ifelse(HomerSampleID == "19GSD086P",24106,StatArea), # Specific correction, don't need any longer?
         HomerSampleID = ifelse(HomerSampleID == "19WBS108P","19GBT110P",HomerSampleID),
         StatArea = ifelse(HomerSampleID == "19GBT110P",24118,StatArea), # Specific correction
         StatArea = ifelse(HomerSampleID == "21GMX034P",24117,StatArea), # Specific correction
         StatArea = ifelse(HomerSampleID == "21GMX023P",24118,StatArea), # Specific correction
         StatArea = ifelse(HomerSampleID == "21GMX067P",24117,StatArea), # Specific correction
         Gear = if_else(Gear == "PURSE SEINE", "PS", Gear),
         Gear = if_else(Gear == "SET GILLNET", "SGN", Gear),
         Source = if_else(Source == "COMMERCIAL COM PROP", "CCP", Source),
         Source = if_else(Source == "COST RECOVERY", "HCR", Source)) %>% 
  mutate(Year = as.factor(Year),Gear = as.factor(Gear),StatArea = as.factor(StatArea),Source = as.factor(Source),Species = "Pink") %>% 
  rename(StatWk = StatWeek)

reds.clean <- reds.marks %>% 
  filter(Source %in% c("COMMERCIAL COM PROP","COST RECOVERY")) %>%
  select(-c(Agency,FisheryName,Species,SubDistrict,AStreamCode,Stock,Harvest,SampleDate,SurveySite)) %>% 
  separate(StatArea, into = c("StatArea","Extra"), sep = 5) %>% 
  select(-Extra) %>% 
  mutate(Gear = if_else(Gear == "PURSE SEINE", "PS", Gear),
         Gear = if_else(Gear == "SET GILLNET", "SGN", Gear),
         Source = if_else(Source == "COMMERCIAL COM PROP", "CCP", Source),
         Source = if_else(Source == "COST RECOVERY", "HCR", Source),
         StatArea = ifelse(StatArea == "241-1",24118,StatArea),
         StatArea = ifelse(HomerSampleID == "18PSD06S",24190,StatArea),
         StatArea = ifelse(HomerSampleID == "18PSD03S",24190,StatArea),
         StatArea = ifelse(HomerSampleID == "18GHC10S",24115,StatArea),
         StatArea = ifelse(HomerSampleID == "18GPG11S",24130,StatArea),
         StatArea = ifelse(HomerSampleID == "18GPG06S",24130,StatArea),
         StatArea = ifelse(HomerSampleID == "18GPG03S",24130,StatArea),
         StatArea = ifelse(HomerSampleID == "18PSD06S",24190,StatArea),
         StatArea = ifelse(HomerSampleID == "18PSD03S",24190,StatArea),
         StatArea = ifelse(HomerSampleID == "19GBT109S",24118,StatArea),
         StatArea = ifelse(HomerSampleID == "21GMX066S",24117,StatArea)) %>% 
  mutate(StatWeek = ifelse(HomerSampleID == "22GMX005S",25,StatWeek),
         Year = as.factor(Year),Gear = as.factor(Gear),StatArea = as.factor(StatArea),Source = as.factor(Source),Species = "Sockeye") %>% 
  rename(StatWk = StatWeek)

otolith.TM.data <- pink.clean %>% 
  select(HomerSampleID,Year,StatWk,Gear,Species,Source,StatArea,Preps,NotMarked,Marked,LCI,PWS,KOD,Other) %>% 
  full_join(select(reds.clean, c(HomerSampleID,Year,StatWk,Gear,Species,Source,StatArea,Preps,NotMarked,Marked,LCI,PWS,KOD,Other)))

### Harvest Data ----

harvest.df <- harvest %>% 
  select(-c(ID,DOL.Month,District,Subdistrict,Stat.Area.Name,Landed.Weight..sum.,Business.Name)) %>% 
  filter(Species.Name %in% c("salmon, sockeye","salmon, pink"),Gear.Name %in% c("Purse seine","Set gillnet"),Harvest.Name %in% c("State managed fishery","Private Hatchery - Fishing")) %>% 
  filter(str_detect(Stat.Area,"^241")) %>% 
  mutate(Gear.Name = if_else(Gear.Name == "Purse seine", "PS", Gear.Name),
         Gear.Name = if_else(Gear.Name == "Set gillnet", "SGN", Gear.Name),
         Species.Name = if_else(Species.Name == "salmon, sockeye", "Sockeye", Species.Name),
         Species.Name = if_else(Species.Name == "salmon, pink", "Pink", Species.Name),
         Harvest.Name = if_else(Harvest.Name == "State managed fishery", "CCP", Harvest.Name),
         Harvest.Name = if_else(Harvest.Name == "Private Hatchery - Fishing", "HCR", Harvest.Name)) %>% 
  rename(Year = DOL.Year,StatWk = Stat.Week,Gear = Gear.Name,Species = Species.Name, StatArea = Stat.Area,Source = Harvest.Name) %>% 
  group_by(Year,StatWk,Gear,Species,StatArea,Source)  %>% 
  summarize(CommercialHarvest = sum(Number.Of.Animals..sum.)) %>% 
  mutate(Year = as.factor(Year),StatWk = as.factor(StatWk),Gear = as.factor(Gear),StatArea = as.factor(StatArea),Species = as.factor(Species),Source = as.factor(Source))

sample.df <- sample %>% 
  filter(HarvType %in% c("CCP","HCR")) %>% 
  select(-c(ID,Agency,SampleDate,SampleType,HarvLoc,District)) %>% 
  mutate(Year = as.factor(Year),StatWk = as.factor(StatWk),Gear = as.factor(Gear),SubDist = as.factor(SubDist),Species = as.factor(Species),HarvType = as.factor(HarvType)) %>% 
  filter(!(HomerID == "18CPTpinks")) # Remove samples related to a Pink Salmon stomach contents pilot study

sampled.harvest <- full_join(harvest.df,sample.df,  # joining sample data frame to harvest data frame
              by = join_by(Year == Year,
                           StatWk == StatWk,
                           Gear == Gear,
                         StatArea == SubDist,
                         Species == Species,
                         Source == HarvType))

## Change Sample Area ----

sampled.harvest.CHANGESTATAREA <- sampled.harvest %>% 
  filter(!(HomerID %in% c("18GPG03S","18GPG06S","18GPG11S","18PSD03S","19GSD086P","20GBT063S","21GMX066S","21GMX067P") & is.na(CommercialHarvest))) %>% 
  mutate(HomerID = ifelse(Year == "2019" & StatWk == 31 & Gear == "SGN" & Species == "Pink","19GSD086P",HomerID),
         HomerID = ifelse(Year == "2018" & StatWk == 24 & Gear == "SGN" & Species == "Sockeye" & StatArea == "24130","18GPG03S",HomerID),
         HomerID = ifelse(Year == "2018" & StatWk == 25 & Gear == "SGN" & Species == "Sockeye" & StatArea == "24130","18GPG06S",HomerID),
         HomerID = ifelse(Year == "2018" & StatWk == 26 & Gear == "SGN" & Species == "Sockeye" & StatArea == "24130","18GPG11S",HomerID),
         HomerID = ifelse(Year == "2018" & StatWk == 25 & Gear == "PS" & Species == "Sockeye" & StatArea == "24190","18PSD03S",HomerID),
         HomerID = ifelse(Year == "2020" & StatWk == 31 & Gear == "SGN" & Species == "Sockeye" & StatArea == "24106","20GBT063S",HomerID)) %>% 
  mutate(StatArea = ifelse(HomerID == "18GBT02S" & is.na(CommercialHarvest),"24117",as.character(StatArea)),
         StatArea = ifelse(HomerID == "18PSD06S" & is.na(CommercialHarvest),"24190",StatArea),
         StatArea = ifelse(HomerID == "20PSD041S" & is.na(CommercialHarvest),"24191",StatArea),
         CommercialHarvest = ifelse(HomerID == "18GBT02S" & is.na(CommercialHarvest),250,CommercialHarvest),
         CommercialHarvest = ifelse(HomerID == "18PSD06S" & is.na(CommercialHarvest),5160,CommercialHarvest),
         CommercialHarvest = ifelse(HomerID == "20PSD041S" & is.na(CommercialHarvest),3693,CommercialHarvest))

## Missing Harvest ----

missing.harvest.ID <- filter(sampled.harvest.CHANGESTATAREA, is.na(CommercialHarvest))$HomerID

pink.missing.harvest <- pink.clean %>% 
  filter(HomerSampleID %in% missing.harvest.ID) %>% 
  group_by(HomerSampleID) %>% 
  summarise(MissingHarvest = Preps)

reds.missing.harvest <- reds.clean %>% 
  filter(HomerSampleID %in% missing.harvest.ID) %>% 
  group_by(HomerSampleID) %>% 
  summarise(MissingHarvest = Preps)

add.missing.harvest <- full_join(pink.missing.harvest,reds.missing.harvest)

### Subtract Harvest ----

# Manually assign which statistical area (SubDist) that the added harvest for a sample ID (HomerID) should be subtracted from based upon our knowledge of where a catch was likely to have been reported from:
sample.missing <- filter(sample, HomerID %in% missing.harvest.ID) %>% 
  mutate(SubStatArea = ifelse(HomerID == "18PSD13S",24190,NA),
         SubStatArea = ifelse(HomerID == "18PSD25S",24106,SubStatArea),
         SubStatArea = ifelse(HomerID == "18PSD26P",24106,SubStatArea),
         SubStatArea = ifelse(HomerID == "18PSD37P",24106,SubStatArea),
         SubStatArea = ifelse(HomerID == "18PSD43P",24115,SubStatArea),
         SubStatArea = ifelse(HomerID == "19PSD088S",24191,SubStatArea),
         SubStatArea = ifelse(HomerID == "19PSD089P",24191,SubStatArea),
         SubStatArea = ifelse(HomerID == "20PSD012S",24190,SubStatArea),
         SubStatArea = ifelse(HomerID == "21PSD062P",24106,SubStatArea),
         SubStatArea = ifelse(HomerID == "21PSD068S",24190,SubStatArea),
         SubStatArea = ifelse(HomerID == "21PSD069P",24190,SubStatArea),
         SubStatArea = ifelse(HomerID == "21PSD071P",24106,SubStatArea),
         SubStatArea = ifelse(HomerID == "22PSD009S",24191,SubStatArea),
         SubStatArea = ifelse(HomerID == "22PSD039S",24193,SubStatArea),
         SubStatArea = ifelse(HomerID == "22PSD040P",24193,SubStatArea))


# Create data frame that has the final commercial harvest value for the strata that are reduced to account for allocating the sample from unreported strata   
subtract.missing.harvest <- harvest %>% 
  select(-c(ID,DOL.Month,District,Subdistrict,Stat.Area.Name,Landed.Weight..sum.,Business.Name)) %>% 
  filter(Species.Name %in% c("salmon, sockeye","salmon, pink"),Gear.Name %in% c("Purse seine","Set gillnet"),Harvest.Name %in% c("State managed fishery","Private Hatchery - Fishing")) %>% 
  mutate(Gear.Name = if_else(Gear.Name == "Purse seine", "PS", Gear.Name),
         Gear.Name = if_else(Gear.Name == "Set gillnet", "SGN", Gear.Name),
         Species.Name = if_else(Species.Name == "salmon, sockeye", "Sockeye", Species.Name),
         Species.Name = if_else(Species.Name == "salmon, pink", "Pink", Species.Name),
         Harvest.Name = if_else(Harvest.Name == "State managed fishery", "CCP", Harvest.Name),
         Harvest.Name = if_else(Harvest.Name == "Private Hatchery - Fishing", "HCR", Harvest.Name)) %>% 
  filter(Gear.Name != "SGN") %>% 
  group_by(DOL.Year,Stat.Week,Gear.Name,Species.Name,Harvest.Name,Date.of.Landing,Stat.Area)  %>% 
  summarize(CommercialHarvest = sum(Number.Of.Animals..sum.)) %>% 
  right_join(sample.missing,
             by = join_by(Species.Name == Species,
                          Stat.Area == SubStatArea,
                          Date.of.Landing == SampleDate,
                          Harvest.Name == HarvType)) %>% 
  full_join(add.missing.harvest,
            by = join_by(HomerID == HomerSampleID)) %>%
  mutate(Final.CommercialHarvest = CommercialHarvest - MissingHarvest) %>%
  select(DOL.Year,Stat.Week,Gear.Name,Species.Name,Harvest.Name,Date.of.Landing,Stat.Area,Final.CommercialHarvest)

# Recreate harvest data frame. Join with the data frame above (subtract.missing.harvest) that accounts for the reallocation of harvest to unreported strata
harvest.df <- harvest %>% 
  select(-c(ID,DOL.Month,District,Subdistrict,Stat.Area.Name,Landed.Weight..sum.,Business.Name)) %>% 
  filter(Species.Name %in% c("salmon, sockeye","salmon, pink"),Gear.Name %in% c("Purse seine","Set gillnet"),Harvest.Name %in% c("State managed fishery","Private Hatchery - Fishing")) %>% 
  mutate(Gear.Name = if_else(Gear.Name == "Purse seine", "PS", Gear.Name),
         Gear.Name = if_else(Gear.Name == "Set gillnet", "SGN", Gear.Name),
         Species.Name = if_else(Species.Name == "salmon, sockeye", "Sockeye", Species.Name),
         Species.Name = if_else(Species.Name == "salmon, pink", "Pink", Species.Name),
         Harvest.Name = if_else(Harvest.Name == "State managed fishery", "CCP", Harvest.Name),
         Harvest.Name = if_else(Harvest.Name == "Private Hatchery - Fishing", "HCR", Harvest.Name)) %>% 
  group_by(DOL.Year,Stat.Week,Gear.Name,Species.Name,Harvest.Name,Date.of.Landing,Stat.Area)  %>% 
  summarize(CommercialHarvest = sum(Number.Of.Animals..sum.)) %>% 
  full_join(subtract.missing.harvest,
            by = join_by(DOL.Year, Stat.Week, Gear.Name, Species.Name, Harvest.Name, Date.of.Landing, Stat.Area)) %>% 
  mutate(CommercialHarvest = ifelse(!is.na(Final.CommercialHarvest),Final.CommercialHarvest,CommercialHarvest)) %>% 
  select(-Final.CommercialHarvest) %>% 
  rename(Year = DOL.Year, StatWk = Stat.Week, Gear = Gear.Name, Species = Species.Name, StatArea = Stat.Area, Source = Harvest.Name) %>% 
  group_by(Year,StatWk,Gear,Species,StatArea,Source) %>% 
  summarise(CommercialHarvest = sum(CommercialHarvest)) %>% 
  mutate(Year = as.factor(Year),StatWk = as.factor(StatWk),Gear = as.factor(Gear),StatArea = as.factor(StatArea),Species = as.factor(Species),Source = as.factor(Source))

# Rejoin harvest dataframe with sample dataframe
sampled.harvest.SUBTRACTED <- full_join(harvest.df,sampled.harvest.CHANGESTATAREA,  # joining sample data frame to harvest data frame
              by = join_by(Year == Year, 
                           StatWk == StatWk,
                           Gear == Gear,
                         StatArea == StatArea,
                         Species == Species,
                         Source == Source)) %>% 
  mutate(CommercialHarvest = CommercialHarvest.x) %>% 
  select(-c(CommercialHarvest.x,CommercialHarvest.y))

### Add Harvest ----

# Add in samples for strata that were sampled but no catch was reported:
sampled.harvest.ADDED <- sampled.harvest.SUBTRACTED %>% 
  full_join(add.missing.harvest, by = join_by(HomerID == HomerSampleID)) %>% 
  mutate(CommercialHarvest = ifelse(is.na(CommercialHarvest),MissingHarvest,CommercialHarvest)) %>% 
  select(-MissingHarvest) %>% 
  mutate(CommercialHarvest = ifelse(Year == "2021" & StatWk == "31" & Gear == "PS" & Species == "Pink" & StatArea == 24191,55+76,CommercialHarvest)) ## Individual edit of data where two HomerID samples were added to the same strata commercial harvest

## Sampled Harvest Data ----

sampled.harvest.CLEAN <- sampled.harvest.ADDED %>% # Rename sampled.harvest data frame to more intuitive name
  mutate(StatArea = as.factor(StatArea)) %>% 
  filter(str_detect(StatArea,"^241"))

# 1 - Estimate of C.hat~hi~ ----

# Create table of # of otoliths from hatchery-area (h) in sample from a given stratum (n_i):
sample.o_hi <- otolith.TM.data %>% 
  pivot_longer(cols = c("LCI","PWS","KOD","Other"), names_to = "HatcheryArea", values_to = "MarkedOtoliths") %>% # Pivot table to LONG format, with one count of the # of MarkedOtoliths per Strata*HatcheryArea combination
  rename(HomerID = HomerSampleID) %>% 
  mutate(HatcheryArea = as.factor(HatcheryArea)) %>% 
  group_by(Year,Gear,Species,Source,StatArea,HatcheryArea) %>% 
  summarise(o_hi = sum(MarkedOtoliths))

# Create table of total # of otoliths sampled from a given stratum (n_i):
sample.n_i <- otolith.TM.data %>% 
  group_by(Year,Gear,Species,Source,StatArea) %>% # Group into stratum (i)
  summarise(n_i = sum(NotMarked) + sum(Marked)) # SampledOtoliths = total # of otoliths sampled

# Create table of total # of fish commercially harvested in stratum (i):
harvest.N_i <- sampled.harvest.CLEAN %>%
  group_by(Year,StatWk,Gear,Species,Source,StatArea) %>% 
  summarise(N_i = sum(unique(CommercialHarvest)))

# Create a table of the otolith-derived estimated of the contribution of hatchery-area (h) to period-gear-district stratum (i):
C.hat_hi <- sample.o_hi %>% 
  full_join(sample.n_i,
            by = join_by(Year,Gear,Species,Source,StatArea)) %>% 
  full_join(harvest.N_i,
            by = join_by(Year,Gear,Species,Source,StatArea)) %>% 
  filter(!is.na(HatcheryArea)) %>% # Removing instances where HatcheryArea = NA, aka when a stratum had commercial harvest occur but no sampling occurred
  mutate(ProportionMarked = o_hi/n_i, # Calculate hatchery proportion of sample
         C.hat_hi = ProportionMarked*N_i,# EQUATION 1: Multiply hatchery proportion by the total commercial harvest for estimate hatchery proportion in commercial catch
         C.hat_hi_VAR = (N_i^2)*(1/(n_i-1))*(o_hi/n_i)*(1 - (o_hi/n_i))) # EQUATION 2: Variance estimate of the hatchery-derived commercial harvest contribution

# 2 - Estimate of C.hat~Sh~ ----

C.hat_Sh <- C.hat_hi %>% 
  group_by(Year,Gear,Species,Source,HatcheryArea) %>% 
  summarize(C.hat_Sh = sum(C.hat_hi), # EQUATION 3: Estimate of contribution of hatchery-area (h) to all sampled harvests within a StatWk, combined across all sampled StatArea's.
            C.hat_Sh_VAR = sum(C.hat_hi_VAR)) # EQUATION 4: Variance estimate of C.hat_Sh

# 3 - Estimate of C.hat~Uh~ ----

unsampled.N_i <- sample.o_hi %>%
  full_join(sample.n_i,
            by = join_by(Year,Gear,Species,Source,StatArea)) %>%
  full_join(harvest.N_i,
            by = join_by(Year,Gear,Species,Source,StatArea)) %>%
  filter(is.na(HatcheryArea)) %>% # Selecting instances where HatcheryArea = NA, aka when a stratum had commercial harvest occur but no sampling occurred
  select(-c(HatcheryArea,o_hi,n_i))

unsampled.C.hat_Uh <- C.hat_hi %>% 
  group_by(Year,Gear,Species,Source,HatcheryArea) %>% 
  summarise(C.hat_hj = sum(C.hat_hi), N_j = sum(N_i),ProportionMarked = C.hat_hj/N_j)

C.hat_Uh <- unsampled.N_i %>% 
  left_join(unsampled.C.hat_Uh,
            by = join_by(Year,Gear,Species,Source),
            relationship = "many-to-many") %>% 
  group_by(Year,Gear,Species,Source,HatcheryArea) %>% 
  summarise(C.hat_Uh = sum(N_i)*unique(ProportionMarked), # EQUATION 5: Estimate the contribution of hatchery-area (h) to unsampled StatArea's
            C.hat_Uh_VAR = as.integer(sum(N_i^2 * (1/(sum(N_j)-1)) * (sum(C.hat_hj)/sum(N_j)) * (1 - (sum(C.hat_hj)/sum(N_j)))   ))) %>% # EQUATION 6: Variance estimate of C.hat_Uh
  filter(!is.na(HatcheryArea))

# NA's occur in the above data frame (C.hat_Uh) when there is commercial harvest from a stratum (Year,Gear,Species,Fishery) but no fish were sampled in that stratum (across any StatArea's) from which to estimate hatchery proportion...
# These unsampled stratum observations are removed from the C.hat_Uh data frame because there is no way to estimate the hatchery contribution within...

# And these unsampled stratum observations are subsetted to their own data frame: 
unsampled.stratum <- unsampled.N_i %>% 
  left_join(unsampled.C.hat_Uh,
            by = join_by(Year,Gear,Species,Source),
            relationship = "many-to-many") %>% 
  group_by(Year,Gear,Species,Source,HatcheryArea) %>% 
  summarise(C.hat_Uh = sum(N_i)*unique(ProportionMarked),
            C.hat_Uh_VAR = sum(N_i^2 * (1/(sum(N_j)-1)) * (sum(C.hat_hj)/sum(N_j)) * (1 - (sum(C.hat_hj)/sum(N_j)))   )) %>% 
  filter(is.na(HatcheryArea)) %>% 
  select(Year,Gear,Species,Source) %>% 
  unite("ID",c(Year,Gear,Species,Source), sep = "_")

# 4 - Estimate of C.hat~h~ ----

C.hat_h <- C.hat_Sh %>% 
  full_join(C.hat_Uh,
            by = join_by(Year,Gear,Species,Source,HatcheryArea)) %>% 
  replace_na(list(C.hat_Uh = 0, C.hat_Uh_VAR = 0)) %>% 
  mutate(C.hat_h = C.hat_Sh + C.hat_Uh, # EQUATION 7: Estimate of contribution of hatchery-area (h) to all strata
         C.hat_h_VAR = C.hat_Sh_VAR + C.hat_Uh_VAR) # EQUATION 8: Variance estimate of C.hat_h

# 5 - Estimate of C.hat~H~ ----

C.hat_H.HATCHAREA <- C.hat_h %>% 
  group_by(Species,Gear,Year,Source,HatcheryArea) %>% 
  summarise(C.hat_H = sum(C.hat_h),
            C.hat_H_VAR = sum(C.hat_h_VAR)) %>% 
  mutate(C.hat_H_se = as.integer(sqrt(C.hat_H_VAR)),
         lower95CI = C.hat_H - 1.96*C.hat_H_se,
         upper95CI = C.hat_H + 1.96*C.hat_H_se)

C.hat_H.TOTAL <- C.hat_h %>% 
  group_by(Species,Gear,Year,Source) %>% 
  summarise(C.hat_H = sum(C.hat_h),
            C.hat_H_VAR = sum(C.hat_h_VAR)) %>% 
  mutate(C.hat_H_se = as.integer(sqrt(C.hat_H_VAR)),
         lower95CI = C.hat_H - 1.96*C.hat_H_se,
         upper95CI = C.hat_H + 1.96*C.hat_H_se)

# 6 - Catch Proportion Comparison ----

catch.comparison.HATCHAREA <- harvest.N_i %>% 
  unite("ID",c(Year,Gear,Species,Source), sep = "_") %>% 
  filter(!(ID %in% unsampled.stratum$ID)) %>% 
  separate(ID, into = c("Year","Gear","Species","Source"), sep = "_") %>% 
  group_by(Species,Gear,Year,Source) %>% 
  summarize(Catch = sum(N_i)) %>% 
  left_join(C.hat_H.HATCHAREA,
            by = join_by(Species,Gear,Year,Source)) %>% 
  mutate(HatcheryProportion = C.hat_H/Catch)

catch.comparison.TOTAL <- harvest.N_i %>% 
  unite("ID",c(Year,Gear,Species,Source), sep = "_") %>% 
  filter(!(ID %in% unsampled.stratum$ID)) %>% 
  separate(ID, into = c("Year","Gear","Species","Source"), sep = "_") %>% 
  group_by(Species,Gear,Year,Source) %>% 
  summarize(Catch = sum(N_i)) %>% 
  left_join(C.hat_H.TOTAL,
            by = join_by(Species,Gear,Year,Source)) %>% 
  mutate(HatcheryProportion = C.hat_H/Catch)

save.image(file = "code/LCI_HatcheryTM_analysis-data.RData")
