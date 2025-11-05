#library 
library(dplyr)
library(ggplot2)
library(cowplot)
library(tidyr)
library(ggstats)

#data 
bs_df<-readRDS("./Documents/Boreal_Burn_Severity/outputs/full_bs.RDS")

# filter out the missing data (where eitehr severity 1 or severity 2 is zero)

bs_df_clean<-bs_df|>
  filter(severity_1!=0&severity_2!=0)

bs_df_long<-pivot_longer(bs_df_clean, cols = c(severity_1, severity_2), 
                         names_to="order", values_to="severity")


# overall distribution of first and second severities: 
set_theme(theme_classic())

bs_df_long|>
  # slice_sample(prop=0.01)|>
  ggplot()+
  geom_histogram(aes(severity, fill=order), position="dodge", binwidth = 0.25)+
  scale_y_continuous(labels = scales::label_percent(scale = 1/nrow(bs_df_long)* 100)) +
  scale_x_continuous(labels = c("unburned", "low", "moderate", "high"))+
  ylab("% of total")+
  # theme_classic()
  # +
  theme_update(legend.title = element_blank() )
# # second fires tend to have slightly lower percentage/proportion 
# of high severiy pixels
ggsave("./Documents/Boreal_Burn_Severity/outputs/visuals/overall_severity1&2_severity.png", 
       width=7, height=4, units="in")

# if first severity is low, moderate or high then second severity is...
bs_df_clean|>
  mutate(sev_1f=factor(severity_1, 
                       levels=c(1, 2, 3, 4),
                       labels=c("unburned", "low", "moderate", "high") ))|>
  # slice_sample(prop=0.01) |>
  filter(severity_1>1 & severity_2>1)|>
  ggplot()+
  geom_histogram(aes(x=severity_2, fill=sev_1f), position="dodge", binwidth = 0.25)+
  scale_fill_manual(values = c(
   "low"="lightgreen",
    "moderate" = "lightblue",  "high" = "lightpink"))+
  scale_y_continuous(labels = scales::label_percent(scale = 1/nrow(bs_df_long)* 100)) +
  scale_x_continuous(
    breaks = c( 2, 3, 4),
    labels = c("low", "moderate", "high"))+
  xlab("Severity of second fire")+
  ylab("")+
  guides(fill = guide_legend(title = "severity of \n 1st fire"))+
  # scale_fill_manual(name="severity of \n1st fire", 
  #                   values=c(2, 3, 4), 
  #                   labels=c("low", "moderate", "high"))+
  # labs(fill="severity of \n1st fire")+
  # theme(legend.title = element_blank() )
theme_classic()
# ggsave("./Documents/Boreal_Burn_Severity/outputs/visuals/1_to_2nd_severity_change_overall.png", 
#        width=7, height=4, units="in")

# fires that burn at low severity at first fire are more likely to burn at low severity
# in the second fire.

# to do: 
# 1) change the x axis to read "low", "moderate", "high" instead of numbers. 
# 2) convert y-axis to percentage

# 3) switching topics - what proportion of pixels are missing data? 


# how does the relationship between the first and second burn severity vary by
# fire interval? 

bs_df_clean<-bs_df_clean|>
  mutate(interval_class = case_when(interval<= 10 ~ 1, 
                                  interval>10&interval<=20 ~ 2, 
                                  interval>20 ~ 3))|>
mutate(interval_name=factor(interval_class, levels=c(1, 2, 3),
                                 labels=c("<10", "10-20", "20<")))

bs_df_clean|>
  mutate(sev_1f=factor(severity_1, 
                       levels=c(1, 2, 3, 4),
                       labels=c("unburned", "low", "moderate", "high") ))|>
  # slice_sample(prop=0.01) |>
  filter(severity_1>1 & severity_2>1)|>
  ggplot()+
  geom_histogram(aes(x=severity_2, fill=sev_1f), position="dodge", binwidth = 0.5)+
  scale_fill_manual(values = c(
    "low"="lightgreen",
    "moderate" = "lightblue",  "high" = "lightpink"))+
  # scale_y_continuous(labels = scales::label_percent(scale = 1/nrow(bs_df_clean)* 100)) +
  scale_x_continuous(
    breaks = c( 2, 3, 4),
    labels = c("low", "moderate", "high"))+
  labs(title="Time since 1st fire")+
  xlab("Severity of second fire")+
  ylab("")+
  guides(fill = guide_legend(title = "severity of \n 1st fire"))+
  # scale_fill_manual(name="severity of \n1st fire", 
  #                   values=c(2, 3, 4), 
  #                   labels=c("low", "moderate", "high"))+
  # labs(fill="severity of \n1st fire")+
  # theme(legend.title = element_blank() )+ 
  facet_wrap(~as.factor(interval_name))+
  # theme_void()+
  theme(rect=element_rect(color="grey"),
    axis.text.x=element_text(angle=50, vjust=(0.5)), 
    axis.line = element_line(colour = "grey"),
    legend.title = element_text(colour = "black"), 
    panel.background = element_rect(fill = "lightgrey"), 
    plot.title = element_text(size=10), 
    plot.title.position = "panel"
    
  )
    
    
ggsave("./Documents/Boreal_Burn_Severity/outputs/visuals/2nd_severity_by_reburn_interval&1st_severity.png",
       width=7, height=4, units="in")



# bs_df_clean|>
#   filter(severity_1>1 & severity_2>1)|>
#   ggplot()+
#   geom_histogram(aes(x=severity_2, fill=as.factor(severity_1)), position="dodge", binwidth = 0.25)+
#   scale_fill_manual(values = c("2" = "forestgreen", "3" = "blue", "4" = "red"))+
#  facet_wrap(~as.factor(interval_class))+
#   
#   theme_classic()

# do areas that burn at higher severity are pre-disposed to hs burning, unless
# they are completely fuel limited (i.e., in the first 10 years since fire?)

# what's next? 
# 1) make graphs nicer (done) 
# 2) think of merging >30 class with 20-30 class (done)
# 3) think of whether we need to track down the missing severity data
# 4) stratify by vegetation type
# 5) look into doing logistic regression analysis, i.e., calculating probabilities
# of high severity in fire 2 as a function of severity 1 and interval since 1st fire. 

# what is the distribution of fire intervals? 

bs_df_clean|>
  slice_sample(prop=0.01)|>
  ggplot()+
  geom_bar(aes(x=interval), stat="count") +
  xlab("time since 1st fire")
