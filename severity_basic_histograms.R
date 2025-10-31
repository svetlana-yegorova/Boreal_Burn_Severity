#library 
library(dplyr)
library(ggplot2)
library(cowplot)
library(tidyr)

#data 
bs_df<-readRDS("./Documents/Boreal_Burn_Severity/outputs/full_bs.RDS")

# filter out the missing data (where eitehr severity 1 or severity 2 is zero)

bs_df_clean<-bs_df|>
  filter(severity_1!=0&severity_2!=0)

bs_df_long<-pivot_longer(bs_df_clean, cols = c(severity_1, severity_2), 
                         names_to="order", values_to="severity")


# overall distribution of first and second severities: 

bs_df_long|>
  slice_sample(prop=0.01)|>
  ggplot()+
  geom_histogram(aes(x=severity, fill=order), position="dodge")+
  theme_bw()
# second fires tend to have slightly lower percentage/proportion 
# of high severiy pixels


# if first severity is low, moderate or high then second severity is...
bs_df_clean|>
  filter(severity_1>1 & severity_2>1)|>
  ggplot()+
  geom_histogram(aes(x=severity_2, fill=as.factor(severity_1)), position="dodge", binwidth = 0.25)+
  scale_fill_manual(values = c("2" = "forestgreen", "3" = "blue", "4" = "red"))+

theme_classic()


# fires that burn at low severity at first fire are more likely to burn at low severity
# in the second fire.

# to do: 
# 1) change the x axis to read "low", "moderate", "high" instead of numbers. 
# 2) convert y-axis to percentage

# 3) switching topics - what proportion of pixels are missing data? 


# how does the relationship between the first and second burn severity vary by
# fire interval? 

bs_df_clean<-bs_df_clean|>
  mutate(interval_class = case_when(interval<= 10 ~ "0_10", 
                                  interval>10&interval<=20 ~ "10_20", 
                                  interval>20&interval<=30 ~ "20_30", 
                                  interval>30 ~ ">30"))

bs_df_clean|>
  filter(severity_1>1 & severity_2>1)|>
  ggplot()+
  geom_histogram(aes(x=severity_2, fill=as.factor(severity_1)), position="dodge", binwidth = 0.25)+
  scale_fill_manual(values = c("2" = "forestgreen", "3" = "blue", "4" = "red"))+
 facet_wrap(~as.factor(interval_class))+
  
  theme_classic()
