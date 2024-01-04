############################################################       
#0: clear all and load data
############################################################      
cat("\014")
rm(list=ls())

library(ks)
library(robustHD)
library(dplyr)
library(plm)
library(ggplot2)
library(DescTools)

library(broom)
library(huxtable)
library(fixest)
library(modelsummary)
library(tibble)

library(xtable)


library(scales)

#navigate to folder with functions
setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups")
#functions
source("function_subsets.R")
source("function_plots.R")
source("function_regressions.R")



############################################################       
#1: Load data
############################################################   

############# 1.a load data ##############
#navigate to with datra
setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/data")

#compustat
Dset <- read.csv("COMPUSTAT_simp.csv")
#usercost
Ucost <- read.csv("usercost.csv")


################## 1.b Clean data ##########################
#back to functions
setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/R")


Dset<-Variable_Gen(Dset,Ucost)

Data<- Clean_adv(Dset)
#clean and combine data



################## 1.c Load Naics ##########################

#load naisc codes
naics <- read.csv("2022_NAICS_Structure.csv")

colnames(naics) <- c("change", "naics_n", "industry")


############################################################       
#0: Density plots
############################################################ 

#advertising density
  xad_density<-ggplot(Data, aes(x=Adr_MC))+
    geom_density()+
    theme(text = element_text(size = 20))+
    labs(x = "Advertising/Marginal Cost (Log-scale)", y = "Density") +
    scale_x_continuous(trans = log10_trans(),
      limits = c(.00001, 30),labels = comma)
    #xad/sales
  xad_density #print
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex")
      #ggsave("xad_density.pdf",  xad_density, width = 10, height = 9, units = "in")
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/R")



  MU_density<-ggplot()+
    geom_density(data=Dset, aes(x=MU_1,color="Full Sample"))+
    geom_density(data=Data, aes(x=MU_1,color="XAD Reported"))+
    theme(text = element_text(size = 20))+
    labs(x = "Markup (Log-scale)", y = "Density") +
    scale_x_continuous(trans = log10_trans(),
      limits = c(.001, 50),labels = comma)+
      theme(legend.position='bottom')

  #markups
   MU_density #print
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex")
      #ggsave("MU_density.pdf",  MU_density, width = 10, height = 9, units = "in")
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/R")


#zero_subset <- Data %>%
 # filter(is.na(Adr_MC))
#zero_subset_sectorknown <- zero_subset %>%
 # filter(!is.na(naics))
##1149/82441= 1.39% report 0 advertising
#plot(density(zero_subset_sectorknown$naics))
#sector_known_subset <- Data %>%
 # filter(!is.na(naics))
#plot(density(sector_known_subset$naics))


############################################################       
#0: Aggregate Markup plots
############################################################ 

agg_mu_all<-agg_mu(Dset)
agg_mu_insamp<-agg_mu(Data)
agg_mu_rew<-agg_mu_reweight(Data,Dset)

agg_mu_plot=ggplot()+
  geom_line(data=agg_mu_all,aes(y=Ag_MU,x=year,color="Full Sample"))+
  geom_line(data=agg_mu_insamp,aes(y=Ag_MU,x=year,color="XAD Reported"))+
  geom_line(data=agg_mu_rew,aes(y=Ag_MU,x=year,color="Reweighted"))+
  theme(text = element_text(size = 20))+
  labs(x = "Year", y = "Sales Weighted Markup") +
  theme(legend.position='bottom')

agg_mu_plot
    #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex")
    #ggsave("agg_mu_plot.pdf",  agg_mu_plot, width = 10, height = 9, units = "in")
    #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/R")



############################################################       
#0: Scatter plots
############################################################  

two_d_data=Industry_n_dig(Data,naics,2)
#add naics industries

  plot_all<-MU_advert_plot(Data,"All",1000)
  #generate full sample scatter plot

  Retail <- two_d_data %>%
      filter(industry=="Retail TradeT")
      scatter_Retail<-MU_advert_plot(Retail,"Retail",1000)
  Manuf <- two_d_data %>%
      filter(industry=="ManufacturingT")
      scatter_Manuf<-MU_advert_plot(Manuf,"Manufacturing",1000)
  Fininc <- two_d_data %>%
      filter(industry=="Finance and InsuranceT")
      scatter_Finance<-MU_advert_plot(Fininc,"Finance and Insurance",1000)
  #generate sub sample scatter plots

#save images
setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex")
  #make sure to return wd
    plot_all
      #ggsave("scatter_all.pdf", plot_all, width = 9, height = 9, units = "in")
    scatter_Manuf
      #ggsave("scatter_Manuf.pdf",scatter_Manuf, width = 9, height = 9, units = "in")
    scatter_Retail
      #ggsave("scatter_Retail.pdf",  scatter_Retail, width = 9, height = 9, units = "in")
    scatter_Finance
      #ggsave("scatter_Finance.pdf", scatter_Finance, width = 9, height = 9, units = "in")
setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/R")



############################################################       
#0: Test table
############################################################  

testtable<-Test_table(Data,naics,2)
testtable[5,1]="Administrative and Support"
lign=c("l","c","c","c","c","c")

print(xtable(testtable,align=lign),include.rownames=FALSE)

############################################################       
#2: 2 digit naics 
############################################################  

#run regressions and save output
Results_2Digit<-Regression_output_N(Data,naics,2)
  #[1] clean table, [2] ugly table, [3] model 9 sector coefficients (well named), [4] model 9 year coefficients (well named), [5] all models
#[6] latex clean table

table_1<- Results_2Digit[1]

table_1

sector_coef_2d <- data.frame(data.frame(Results_2Digit[3]))
year_coef_2d <- data.frame(data.frame(Results_2Digit[4]))

table_1_latex<-Results_2Digit[6]
####################2.a plots#######


#time trend plot
time_co_plot_2d<-
  ggplot(year_coef_2d, aes(x = year, y = fit, group=year)) +
  geom_boxplot() +
  geom_errorbar(aes(ymin = fit - 1.96*se, ymax = fit + 1.96*se),
                width = 0.2, color = "darkblue") +
  geom_hline(aes(yintercept=0), colour="black", linetype="dashed", size = 1) + 
  geom_text(aes( 1985, 0, label = "2022 Reference", vjust = -1), size = 4, colour="black")+
  labs(x = "Year", y = "Time Compoent of Advertising Efficency ") +
  ggtitle("Advertising Efficiency Over Time With 95% Confidence Intervals (2022 Reference year)")+
  guides(fill = guide_legend(title = NULL))+
  theme_minimal()


time_co_plot_2d
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex")
      #ggsave("time_co_plot_2d.pdf", time_co_plot_2d, width = 9, height = 7, units = "in")
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/R")


#sector plot

# coefficents plot
industry_co_plot_2d<-Efficency_plot_2_digit(sector_coef_2d)

industry_co_plot_2d
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex")
      #ggsave("industry_co_plot_2d.pdf", industry_co_plot_2d, width = 9, height = 7, units = "in")
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/R")





############################################################       
#3: 3 digit naics 
############################################################  

#run regressions and save output
Results_3Digit<-Regression_output_N(Data,naics,3)
#[1] clean table, [2] ugly table, [3] model 9 sector coefficients (well named), [4] model 9 year coefficients (well named), [5] all models,
      #[6] latex clean table #[7] industry count data
      
table_3dig<- Results_3Digit[1]
sector_coef_3d <- data.frame(data.frame(Results_3Digit[3]))
year_coef_3d <- data.frame(data.frame(Results_3Digit[4]))


table_3dig_latex<- Results_3Digit[6]

Ind_count3<-Results_3Digit[7]
####################3.a plots#######


#time trend plot
time_co_plot_3d<-
  ggplot(year_coef_3d, aes(x = year, y = fit, group=year)) +
  geom_boxplot() +
  geom_errorbar(aes(ymin = fit - 1.96*se, ymax = fit + 1.96*se),
                width = 0.2, color = "darkblue") +
  geom_hline(aes(yintercept=0), colour="black", linetype="dashed", size = 1) + 
  geom_text(aes( 1985, 0, label = "2022 Reference", vjust = -1), size = 4, colour="black")+
  labs(x = "Year", y = "Time Compoent of Advertising Efficency ") +
  ggtitle("Advertising Efficiency Over Time With 95% Confidence Intervals (2022 Reference year)")+
  guides(fill = guide_legend(title = NULL))+
  theme_minimal()


time_co_plot_3d
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex")
      #ggsave("time_co_plot_3d.pdf", time_co_plot_3d, width = 9, height = 7, units = "in")
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/R")

#sector plot

# coefficents plot
industry_co_plot_3d<-Efficency_plot_3_digit(sector_coef_3d)

industry_co_plot_3d

#limited coefficent plot
industry_co_plot_3d_limit<-Efficency_plot(sector_coef_3d,Ind_count3,100)


industry_co_plot_3d_limit




############################################################       
#4: 4 digit naics 
############################################################  

#run regressions and save output
Results_4Digit<-Regression_output_N(Data,naics,4)
#[1] clean table, [2] ugly table, [3] model 9 sector coefficients (well named), [4] model 9 year coefficients (well named), [5] all models

table_4dig<- Results_4Digit[1]
sector_coef_4d <- data.frame(data.frame(Results_4Digit[3]))
year_coef_4d <- data.frame(data.frame(Results_4Digit[4]))

####################4.a plots#######


#time trend plot
time_co_plot_4d<-
  ggplot(year_coef_4d, aes(x = year, y = fit, group=year)) +
  geom_boxplot() +
  geom_errorbar(aes(ymin = fit - 1.96*se, ymax = fit + 1.96*se),
                width = 0.2, color = "darkblue") +
  geom_hline(aes(yintercept=0), colour="black", linetype="dashed", size = 1) + 
  geom_text(aes( 1985, 0, label = "2022 Reference", vjust = -1), size = 4, colour="black")+
  labs(x = "Year", y = "Time Compoent of Advertising Efficency ") +
  ggtitle("Advertising Efficiency Over Time With 95% Confidence Intervals (2022 Reference year)")+
  guides(fill = guide_legend(title = NULL))+
  theme_minimal()


time_co_plot_4d



############################################################       
#5: Sector and time
############################################################  

output_list<-Sector_time_coefs_N(Data,naics,4)










########################
## to do

# get MU_1:time: sector and plot against time grouped by sector
  #different trends by sector?
  #actually first just do for bigger sectors
    #can prolly just use existing function if i filter by sector first

# regression table of time trend and intercept 
  #1 regression for each sector

#work on slides



















############################ play ground ###################################
## Model test
model_test <- feols(
  Adr_MC ~ i(fyear,ref=2022,industry)*MU_1 - MU_1| industry +fyear, 
  data = Dset_2_dig
)



## model test
model_test <- feols(
  Adr_MC ~ i(fyear,MU_1)| fyear, 
  data = subset(Dset_2_dig, industry== "InformationT")
)

#create ugly table and save
modelsummary(model_test,stars=TRUE,fmt = 5)

#possible downward trend "ManufacturingT"                                                           
#mostly noise "Wholesale TradeT"                                                         
#clear DOWNWARD trend "Retail TradeT"                                                            
#very bumpy but seems to have trend "InformationT"  
# interesting, less than pretty tho: "Finance and InsuranceT"  
#noise "Accommodation and Food ServicesT"   
# noisey upward trend "Other Services (except Public Administration)T"

#might be better to do this in decades


## model test
model_test <- feols(
  Adr_MC ~ i(industry,MU_1)*time |  fyear, 
  cluster="industry",
  data = Dset_2_dig
)

## model test
model_test <- feols(
  Adr_MC ~ i(industry,MU_1)*time | fyear, 
  cluster="industry",
  data = Dset_3_dig
)

#create ugly table and save
modelsummary(model_test,stars=TRUE)
 



unique(Dset_2_dig$industry)

Dset_2_dig %>% count(industry)














#lets just code it up at the decade level, plot and see the lookskies





## model test
model_test_retail <- feols(
  Adr_MC ~ i(fyear,ref=2022,MU_1)+MU_1| fyear,
  cluster="GVKEY",
  data = subset(Dset_2_dig, industry== "Retail TradeT")
)

## model test
model_test_manuf <- feols(
  Adr_MC ~ i(fyear,ref=2022,MU_1)+MU_1| fyear, 
  cluster="GVKEY",
  data = subset(Dset_2_dig, industry== "ManufacturingT")
)

## model test
model_test_whole <- feols(
  Adr_MC ~ i(fyear,ref=2022,MU_1)+MU_1| fyear, 
  cluster="GVKEY",
  data = subset(Dset_2_dig, industry== "Wholesale TradeT")
)

## model test
model_test_inf <- feols(
  Adr_MC ~ i(fyear,ref=2022,MU_1)+MU_1| fyear, 
  cluster="GVKEY",
  data = subset(Dset_2_dig, industry== "InformationT")
)

## model test
model_test_fin <- feols(
  Adr_MC ~ i(fyear,ref=2022,MU_1)+MU_1| fyear, 
  cluster="GVKEY",
  data = subset(Dset_2_dig, industry== "Finance and InsuranceT")
)

## model test
model_test_os <- feols(
  Adr_MC ~ i(fyear,ref=2022,MU_1)+MU_1| fyear, 
  cluster="GVKEY",
  data = subset(Dset_2_dig, industry== "Other Services (except Public Administration)T")

)

models_test=list("Retail"=model_test_retail,"Manufactoring"=model_test_manuf,"Wholesale"=model_test_whole,
                 "Information"=model_test_inf,"Finance and Insurance"=model_test_fin,"Other Services"=model_test_os)

#create ugly table and save
modelsummary(models_test,stars=TRUE,fmt = 5)

#possible downward trend "ManufacturingT"                                                           
#mostly noise "Wholesale TradeT"                                                         
#clear DOWNWARD trend "Retail TradeT"                                                            
#very bumpy but seems to have trend "InformationT"  
# interesting, less than pretty tho: "Finance and InsuranceT"  
#noise "Accommodation and Food ServicesT"   
# noisey upward trend "Other Services (except Public Administration)T"

#might be better to do this in decades
  







## model test
model_test <- feols(
  Adr_MC ~0+ i(fyear,ref = 2022,industry)*MU_1 + i(industry,MU_1) - MU_1 |  fyear + industry, 
  cluster="GVKEY",
  data = Dset_2_dig
)

model_test

#create ugly table and save
modelsummary(model_test,stars=TRUE)

## model test
model_test <- feols(
  Adr_MC ~0+ i(fyear,ref = 2022,industry)*MU_1 + i(industry,MU_1) - MU_1, 
  data = Dset_2_dig
)

#create ugly table and save
modelsummary(model_test,stars=TRUE)

  