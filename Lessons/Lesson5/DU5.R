install.packages("lubridate")
library(lubridate)

library(ggplot2)
knitr::opts_chunk$set(echo = TRUE)


setwd("C:/Users/userPC/Documents/GeneralInsurance_Class/Data")
dt_Claims <- read.csv("lesson5_Claims.csv") %>% distinct(NrClaim, .keep_all = TRUE)
dt_pol_w_claims <- left_join(dt_Policy, 
                             dt_Claims, 
                             by = c("NrPolicy", "NrObject")
)


dt_pol_w_claims <- 
  dt_pol_w_claims %>% mutate(Time_Exposure = lubridate::dmy(Dt_Exp_End) - lubridate::dmy(Dt_Exp_Start))

dt_pol_w_claims <- 
  dt_pol_w_claims %>% 
  mutate(Ult_Loss = Paid + Reserves,
         Burning_Cost = ifelse(is.na(Ult_Loss), 0,  Ult_Loss / as.integer(Time_Exposure))
  )

########### Najskor sa zameriame na premennu Customer Type
dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = Customer_Type)) + 
  geom_jitter()
# Vidime, ze typ C ma jedneho outliera a zaroven ma vacsiu disperziu ako typ S, preto ocakavame, ze typ C bude mat vacsi vplyv na BC

dt_pol_w_claims %>% 
  group_by(Customer_Type) %>% 
  summarise(BC_avg = mean(Burning_Cost, na.rm  = TRUE),
            BC_median = median(Burning_Cost, na.rm = TRUE),
            cnt = n()) %>% 
  arrange(desc(BC_avg))
# Typ C ma aj vacsi priemer BC ako typ S, teda ocakavame ze bude mat vacsi vplyv na BC

dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = Customer_Type)) + 
  geom_boxplot() +
  ylim(0, 100)
# C ma aj vyssie BC ako S, pricom S ma menej vyssich BC

model_ct <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
              formula = Burning_Cost ~ Customer_Type,
              family = Gamma())
summary(model_ct)
# zjavne do regresie vstupuje iba typ S, no nie je signifikantny
# Teda na zaklade tohto modelu nevieme predikovat vysku BC

########### Dalej budeme analyzovat premennu Veh_type1 - typy vozidiel podla "pouzitia"

dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = Veh_type1)) + 
  geom_jitter()

# kedze mame vela typov, obrazok nie je velmi citatelny, no vidime, ze vacsiu disperziu ma typ "private car", "commercial car <3100 kg" a "commercial car <3500 kg" => asi budu vplyvat viac

dt_pol_w_claims %>% 
  group_by(Veh_type1) %>% 
  summarise(BC_avg = mean(Burning_Cost, na.rm  = TRUE),
            BC_median = median(Burning_Cost, na.rm = TRUE),
            cnt = n()) %>% 
  arrange(desc(BC_avg))
# najvyssiu strednu hodnotu ma taxi, no to moze byt sposobene malym poctom dat 

dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = Veh_type1)) + 
  geom_boxplot() +
  ylim(0, 100)
# vidime, ze asi private car, commercial car <3100 kg, commercial car <3500 kg, pripadne articulated vehicle a driving school car budu vyrazne ovplyvnovat BC

model_vt <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
              formula = Burning_Cost ~ Veh_type1,
              family = Gamma())

summary(model_vt)
# nase tusenie sa potvrdilo, teda oba typy commercial car, private car, driving school car su signifikantne a dokonca aj taxi category A => vplyvaju na BC
# Na zaklade tohto modelu vieme dobre predikovat vysku BC pre jednotlive typy aut

########### Skombinujme obe premenne

model <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
                formula = Burning_Cost ~ Customer_Type +  Veh_type1,
                family = Gamma())

summary(model)

# Kombinaciou oboch premennych dostavame rovnky zaver: customer_type nie je signifikantny a typy commercial car, private car, driving school car a taxi category A su
# Lepsimi modelmi na modelovanie BC by boli kombinacie roznych faktorov, pripadne vsetkych a nasledne porovnanie podla niektoreho z porovnavacich kriterii


