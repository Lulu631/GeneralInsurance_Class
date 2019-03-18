library(dplyr)
library(ChainLadder)
setwd("C:/Users/userPC/Documents/GeneralInsurance_Class/Data")
dt_KPI <- read.csv("lesson2_KPI.csv")
dt_LatestView <- read.csv("lesson4_latestView.csv")
dt_PaidCase <- read.csv("lesson4_PaidCase.csv")

summary(dt_PaidCase) #case - rezerva, paid - vyplatene, ay - vznik nehody, dy - kedy su s tym spojene naklady, SumOfamount - naklady

Paid_HH_sml_triangle1 <- dt_PaidCase %>% filter(Business=="House"& ClaimSize == "Small" & dataset_type=="PAID") %>% as.triangle(Paid_HH_sml, origin = "ay", dev = "dy",value = "SumOfamount")
plot(Paid_HH_sml_triangle1)
plot(predict(chainladder(Paid_HH_sml_triangle1)))
ata(Paid_HH_sml_triangle1)
# nic nezvycajne. Od cca 4=5 roku stabilne naklady

Paid_HH_sml_triangle2 <- dt_PaidCase %>% filter(Business=="House"& ClaimSize == "Large" & dataset_type=="PAID") %>% as.triangle(Paid_HH_sml, origin = "ay", dev = "dy",value = "SumOfamount")
plot(Paid_HH_sml_triangle2)
plot(predict(chainladder(Paid_HH_sml_triangle2)))
ata(Paid_HH_sml_triangle2)
# v roku 2012 boli naklady spojene s rokom 2011 viac ako raz take ako v roku 2011. Vsetky sa ale pribliyne od 3-5 roku stabilizovali

Paid_HH_sml_triangle3 <- dt_PaidCase %>% filter(Business=="3rd Party"& ClaimSize == "Small" & dataset_type=="PAID") %>% as.triangle(Paid_HH_sml, origin = "ay", dev = "dy",value = "SumOfamount")
plot(Paid_HH_sml_triangle3)
plot(predict(chainladder(Paid_HH_sml_triangle3)))
ata(Paid_HH_sml_triangle3)
# naklady spojene s udalostami v akomkolvek roku s narastajucim poctom rokov rastu. Nekonverguju

Paid_HH_sml_triangle4 <- dt_PaidCase %>% filter(Business=="3rd Party"& ClaimSize == "Large" & dataset_type=="PAID") %>% as.triangle(Paid_HH_sml, origin = "ay", dev = "dy",value = "SumOfamount")
plot(Paid_HH_sml_triangle4)
plot(predict(chainladder(Paid_HH_sml_triangle4)))
ata(Paid_HH_sml_triangle4)
# na rozdiel od ostatnych, kedy sa predikovane hodnoty drzali "pokope", predikovana hodnoty pre naklady spojene s rokom 2016 vyskocili na viac ako 7 nasobok

############# Celkove naroky
Paid_HH_sml_triangle5 <- dt_PaidCase %>% filter(Business=="House"& ClaimSize == "Small") %>% as.triangle(Paid_HH_sml, origin = "ay", dev = "dy",value = "SumOfamount")
plot(Paid_HH_sml_triangle5)
plot(predict(chainladder(Paid_HH_sml_triangle5)))
ata(Paid_HH_sml_triangle5)
# celkove naklady budu s pribudajucim casom asi pomaly klesat

Paid_HH_sml_triangle6 <- dt_PaidCase %>% filter(Business=="House"& ClaimSize == "Large") %>% as.triangle(Paid_HH_sml, origin = "ay", dev = "dy",value = "SumOfamount")
plot(Paid_HH_sml_triangle6)
plot(predict(chainladder(Paid_HH_sml_triangle6)))
ata(Paid_HH_sml_triangle6)
# pomerne stabilne uz od zaciatku => po vyplateni skody na zaciatku, sa uz +/- dalej nevyplaca

Paid_HH_sml_triangle7 <- dt_PaidCase %>% filter(Business=="3rd Party"& ClaimSize == "Small") %>% as.triangle(Paid_HH_sml, origin = "ay", dev = "dy",value = "SumOfamount")
plot(Paid_HH_sml_triangle7)
plot(predict(chainladder(Paid_HH_sml_triangle7)))
ata(Paid_HH_sml_triangle7)
# dost nestabilne, zjavne je tu vysoka volatilita

Paid_HH_sml_triangle8 <- dt_PaidCase %>% filter(Business=="3rd Party"& ClaimSize == "Large") %>% as.triangle(Paid_HH_sml, origin = "ay", dev = "dy",value = "SumOfamount")
plot(Paid_HH_sml_triangle8)
plot(predict(chainladder(Paid_HH_sml_triangle8)))
ata(Paid_HH_sml_triangle8)
# Vyvoj nakladov spojenych s danym vznikom poistnej udalosti sa vyvijaju relativne rovnako. Az na rok 2016, kedz su predikovane naklady znacne nizsie ako pri ostatnych

# Zjavne naklady pri House su short tail, pretoze skonverguju relativne rychlo (cca 4 rok po vzniku), pricom pri 3rd Party to trva dlhsien najma pri malych skodach, ak vobec, preto su long tail. 
# Tento fakt sa da logicky odovodnit aj tym, oprava poskodenia budovy sa vykona jednorazovo, aj ked moze trvat dlhsie, no oprava poskodenia auta po autonehode, pripadne liecba poskodenych moze trvat dlhsi cas.
# Rozdiel medzi Small a Large je v tom, ze pri Large vydavky skonverguju rychlejsie ako pri Small (ak konverguju).

###### Pre pripad, ze by bolo treba robit s CASE (rezervou)
#Paid_HH_sml_triangle9 <- dt_PaidCase %>% filter(Business=="House"& ClaimSize == "Small" & dataset_type=="CASE") %>% as.triangle(Paid_HH_sml, origin = "ay", dev = "dy",value = "SumOfamount")
#plot(Paid_HH_sml_triangle9)
#plot(predict(chainladder(Paid_HH_sml_triangle9)))
#ata(Paid_HH_sml_triangle9)

#Paid_HH_sml_triangle10 <- dt_PaidCase %>% filter(Business=="House"& ClaimSize == "Large" & dataset_type=="CASE") %>% as.triangle(Paid_HH_sml, origin = "ay", dev = "dy",value = "SumOfamount")
#plot(Paid_HH_sml_triangle10)
#plot(predict(chainladder(Paid_HH_sml_triangle10)))
#ata(Paid_HH_sml_triangle10)

#Paid_HH_sml_triangle11 <- dt_PaidCase %>% filter(Business=="3rd Party"& ClaimSize == "Small" & dataset_type=="CASE") %>% as.triangle(Paid_HH_sml, origin = "ay", dev = "dy",value = "SumOfamount")
#plot(Paid_HH_sml_triangle11)
#plot(predict(chainladder(Paid_HH_sml_triangle11)))
#ata(Paid_HH_sml_triangle11)

#Paid_HH_sml_triangle12 <- dt_PaidCase %>% filter(Business=="3rd Party"& ClaimSize == "Large" & dataset_type=="CASE") %>% as.triangle(Paid_HH_sml, origin = "ay", dev = "dy",value = "SumOfamount")
#plot(Paid_HH_sml_triangle12)
#plot(predict(chainladder(Paid_HH_sml_triangle12)))
#ata(Paid_HH_sml_triangle12)
