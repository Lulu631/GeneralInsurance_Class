# Z technik som si vybrala pridavanie novej premennej a odstranenie premennej
# Komentare su napisane pri kazdom modeli
# Hodnoty mse su napisane v komentaroch pri kazdom vypocte mse
# Na konci kodu je zhrnutie

#`{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
setwd("C:/Users/userPC/Documents/GeneralInsurance_Class/Data")
dt_pol_w_claims <- readRDS("lesson6_dt_pol_w_claims.rds")
set.seed(58742) # to fix randomizer
ind <- sample(2, nrow(dt_pol_w_claims), replace=TRUE, prob=c(0.80, 0.20)) # generate random indicator to split by
dt_pol_w_claims <- mutate(dt_pol_w_claims,
                          data_status = ifelse(ind == 1, 
                                               "Training",
                                               ifelse(ind == 2, 
                                                      "Validation", 
                                                      "Unseen")
                          )
)

train <- dt_pol_w_claims %>% filter(data_status == "Training")
val <- dt_pol_w_claims %>% filter(data_status == "Validation")

mse <- function(prediction, actual){
  return(sum((prediction-actual)^2, na.rm = TRUE)/length(prediction))
}
##### Zacneme s modelmi z predoslej domacej ulohy
##### Najskor sa pozrieme len na premennu Customer type 
model1 <- glm(data = train,
              formula = Burning_Cost ~ Customer_Type,
              family = Gamma())
summary(model1)

mse(predict(model1, train, type = "response"), train$Burning_Cost) #201.5855
mse(predict(model1, val, type = "response"), val$Burning_Cost) # 285.7319

##### Dalej sa pozrieme len na premennu Veh_type1
model2 <- glm(data = train,
              formula = Burning_Cost ~ Veh_type1,
              family = Gamma())
summary(model2)

mse(predict(model2, train, type = "response"), train$Burning_Cost) #193.4222
mse(predict(model2, val, type = "response"), val$Burning_Cost) # 286.7513

# vidime, ze mse train je pre model2 mensie ako pre model1, no v pripade mse val je to naopak => nevieme, povedat, ktory je lepsi

##### Pozrime sa na kombinaciu Customer_Type a Veh_type1
model3 <- glm(data = train,
              formula = Burning_Cost ~ Customer_Type + Veh_type1,
              family = Gamma())
summary(model3)

mse(predict(model3, train, type = "response"), train$Burning_Cost) # 193.4458
mse(predict(model3, val, type = "response"), val$Burning_Cost) # 286.6611

# v tomto pripade je mse train pre model3 mensie ako pre model1, no vacsie ako pre model2 a mse val pre model3 je mensie ako pre model2, no opat vacsie ako pre model1

############ 1. Pridavanie novych premennych 
##### K nasim dvom povodnym premennym pridame premennu D_age 
# mame tusenie, ze tato premenna by mohla na regresiu mat znacny vplyv, nakolko vek vodica ovplyvnuje jeho rekacne schopnosti
model4 <- glm(data = train,
              formula = Burning_Cost ~ Customer_Type + Veh_type1 + D_age,
              family = Gamma())
summary(model4)

mse(predict(model4, train, type = "response"), train$Burning_Cost) # 191.5118
mse(predict(model4, val, type = "response"), val$Burning_Cost) # 282.8096

# nase tusenie sa potvrdilo, nakolko obe mse su pre model4 mensie ako pre ktorykolvek z predoslych modelov, preto je model4 najlepsi z tychto styroch modelov

############ 2. Odstranovanie premennych
# Najskor pridame dalsie premenne, no vynechame Veh_type1
model5 <- glm(data = train,
              formula = Burning_Cost ~ Customer_Type + Veh_type2 + Time_on_book + D_age  + Construct_year + Capacity + Nr_of_seats + BonusMalus + D_ZIP + Nr_payments + Sum_insured,
              family = Gamma())
summary(model5)
mse(predict(model5, train, type = "response"), train$Burning_Cost) # 189.9754
mse(predict(model5, val, type = "response"), val$Burning_Cost) # 295.2468
# mse train nam sice kleslo pre tento model, no mse val nam znacne stuplo

# teraz vynechame Customer_Type
model6 <- glm(data = train,
              formula = Burning_Cost ~ Veh_type1 + Veh_type2 + Time_on_book + D_age  + Construct_year + Capacity + Nr_of_seats + BonusMalus + D_ZIP + Nr_payments + Sum_insured,
              family = Gamma())
summary(model6)
mse(predict(model6, train, type = "response"), train$Burning_Cost) # 186.7041
mse(predict(model6, val, type = "response"), val$Burning_Cost) # 291.1775
## mse train aj mse val vyslo mensie ako pri model6 ale aj tak je stale vyssie ako pri predoslych modeloch a pri oboch mse pre tento model nam vyskocila warning message, ze predikcia moze byt zavadzajuca

# Zaver: Analyzovali sme mse pre modeli so samostatnymi premennymi Customer_Type a Veh_type1 a taktiez aj ich kombinaciu, pricom nevieme urcit, ktory z nich je lepsi.
#        1. Pridali sme premennu D_age, nakolko vek vodica ovplyvni jeho soferovanie a teda aj jeho "nehodovost". Tento krok sa javil ako spravny a vysledny model bol najlepsi spomedzi predoslych.
#        2. Pridali sme k modelu viacero dalsich premennych, no postupne odstranili nase povodne premenne, teda Customer_Type (model5) a Veh_type1 (model6).
#           Tieto premenne sme sa rozhodli odstranit kvoli tomu, ze ked tvorili model samostatne, mali velku chybovost. Ak vy sme porovnavali iba tieto dva modely,
#           prisli by sme na to, ze model s odstranenim Custtomer_Type je lepsi ako model s odstranenim Veh_type1, no ak porovname tieto modely aj s predoslymi, nevieme povedat, ktory model je lepsi, 
#           pretoze model6 ma najlepsie mse train , no jedno z najvyssich mse val. Problemom taktiez je, ze tento model moze byt "misleading".
#       Samozrejme, je mozne, ze nejaka ina kombinacia, pripadne ine techniky mozu viest k lepsim modelom, no toto su modely, ktore sme skusali v tejto nalayze.