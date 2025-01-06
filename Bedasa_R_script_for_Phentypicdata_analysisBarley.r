######################################################################################
######################################################################################
####Fiel phenotypic data anlysis-Barley2025####
####                                       #####
#import data in excel
Sys.which("make")
install.packages("readxl")
library(readxl)
install.packages("Rcmdr")#important for to import excel data
library("Rcmdr")
install.packages("dplyr")####support "-"
library("dplyr") 
install.packages("agricolae")
library(agricolae)
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages("car")
install.packages("Rtools")
library(Rtools)
library(car)
designs<-apropos("design")
designs[substr(designs,1,6)=="design"]
#import data #excel
#set working directory
setwd("C:/Berdasa_Barley_Feild & RNA Data_2024/Field_Data_2024/field_data_analysis_R")
getwd()

library(readxl)
field_data<- read_excel("C:/Berdasa_Barley_Feild & RNA Data_2024/Field_Data_2024/field_data_analysis_R/phenotypic_data_barley_RNA.xlsx",
                                               sheet = NULL, col_names = TRUE)
View(field_data)
summary(field_data)
str(field_data)
field_data$treatment<-as.factor(as.numeric(field_data$treatment))
field_data$replication<-as.factor(as.numeric(field_data$replication))
str(field_data)
View(field_data)
summary(field_data)
field_data$treatment

##extract boxplot for all traits
df_filtered <- field_data %>% select(!c(treatment, replication))#exclude treatments and replications
boxplot(df_filtered, col=c("plum1","red","yellow","orange","purple1", "green", "deeppink1"))
boxplot(field_data$TSW)
max(field_data$TSW)
min(field_data$TSW)
mean(field_data$TSW)

# Assuming 'field_data' the dataframe with 'treatment' and 'NSPS' columns
plot(x = field_data$treatment, y = field_data$TSW, 
     xlab = "treatment",# Suppresses x-axis label 
     ylab = "NSPS", 
     main = "NSPS by Treatment",
     xaxt = "n")# Suppresses x-axis labels

#density plot
plot(density(field_data$TSW))

######anova_CRD####anova_CRD### anova_CRD--------####
######anova_CRD####anova_CRD### anova_CRD--------####

res.aov<- aov(NSPS ~ treatment + replication,data = field_data) #important 
summary(res.aov) 

#-- Normality test using Shapiro-Wilk test

aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals)#shapiro usually works on residuals
                               #p-value should be non-significant then=data is normal

# Homogeneity test for variances

bartlett.test(TSW ~ treatment, data = field_data)  #to be homogeneity, p-value should be non-significant


##LSD test_mean comparison ########
#The LSD test is most appropriate for making planned pair comparisons but, strictly speaking, 
#LSD testis not valid for comparing all possible pairs of means

lsd_result <- LSD.test(res.aov, "treatment", group = TRUE, alpha = 0.01, p.adj = "bonferroni")#bonferroni is used to adjust p.value by MSD
                                                              #MSD minimum significant different
print(lsd_result)
write.csv(lsd_result$groups, "msd_group.csv") 

####################################################################################################
####Histogram for Mid-parent and better parent heterosis-Barley2025##################
####Histogram for Mid-parent and better parent heterosis-Barley2025##################
####Histogram for Mid-parent and better parent heterosis-Barley2025##################
####Histogram for Mid-parent and better parent heterosis-Barley2025##################
install.packages("readxl")
library(readxl)
install.packages("Rcmdr")#important for to import excel data
library("Rcmdr")
install.packages("dplyr")
library("dplyr") 
install.packages("agricolae")
library(agricolae)
##if i want to make graph using ggplot
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
install.packages("reshape")
library(reshape2)
install.packages("dendextend")
library(dendextend)
install.packages("car")
library(car)
install.packages("ggpubr") ### the package used to visualize the relationship between variables
library("ggpubr")
install.packages("ggrepel")##is used to visualize plots in overlapped way
library(ggrepel)
install.packages("patchwork")
library(patchwork)
setwd("C:/Berdasa_Barley_Feild & RNA Data_2024/Field_Data_2024/field_data_analysis_R")
getwd()

library(readxl)
heterosis_data<- read_excel("C:/Berdasa_Barley_Feild & RNA Data_2024/Field_Data_2024/field_data_analysis_R/MPH_BPH_barley14F1hybrids_data.xlsx",
                        sheet = NULL, col_names = TRUE)
View(heterosis_data)
str(heterosis_data)
heterosis_data$row_type<-as.character(as.numeric(heterosis_data$row_type))
str(heterosis_data)

install.packages("tidyr")
library(tidyr)


#######DTF
# Reshape the data into long format
heterosis_data_long <- heterosis_data %>%
  pivot_longer(cols = c(DTF_MPH, DTF_BPH), names_to = "Variable", values_to = "Value")

dtf<-ggplot(heterosis_data_long, aes(x = crosses, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 0.91) +
  geom_vline(xintercept = 0, color = "black",linewidth = 0.2) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey", linewidth =  0.2),
    axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5), # Border added
    plot.title.position = "plot",
    plot.title = element_text(size = 14, hjust = 0.5, vjust = 1.5),
    plot.margin = margin(t = 6, r = 11, b = 11, l = 11), # Adjust margins
    legend.position.inside = c(0.9, 0.90), 
    legend.position = c(0.9, 0.90),                 
    ) +
  labs(
    x = "",
    y = "Level of Heterosis (%)",
    fill = "",
  ) +
  scale_fill_manual(values = c("DTF_MPH" = "red", "DTF_BPH" = "blue")) +
  scale_y_continuous(limits = c(-5,15 ), breaks = seq(-5, 15, by = 5))+
  annotate(
    "text",
    x = 1,                          # Adjust based on the x-axis range
    y = 13,                          # Position the title near the top within the border
    label = "Days to flowering",    # Title text
    size = 6, color = "black", fontface = "plain", hjust = 0
  )
  

################PLH
# Reshape the data into long format
heterosis_data_long <- heterosis_data %>%
  pivot_longer(cols = c(PLH_MPH,PLH_BPH), names_to = "Variable", values_to = "Value")

# Plot the data with y-axis limit

plh<-ggplot(heterosis_data_long, aes(x = crosses, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 0.91) +
  geom_vline(xintercept = 0, color = "black",linewidth = 0.2) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey", linewidth =  0.2),
    axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5), # Border added
    plot.title.position = "plot",
    plot.title = element_text(size = 14, hjust = 0.5, vjust = 1.5),
    plot.margin = margin(t = 6, r = 11, b = 11, l = 11), # Adjust margins
    legend.position.inside = c(0.9, 0.9), 
    legend.position = c(0.9, 0.9),                 
  ) +
  labs(
    x = "",
    y = "Level of Heterosis (%)",
    fill = "",
  ) +
  scale_fill_manual(values = c("PLH_MPH" = "red", "PLH_BPH" = "blue")) +
  scale_y_continuous(limits = c(-10,35 ), breaks = seq(-10, 35, by = 5))+
  annotate(
    "text",
    x = 1,                          # Adjust based on the x-axis range
    y = 32,                          # Position the title near the top within the border
    label = "Plant height",    # Title text
    size = 6, color = "black", hjust = 0
  )



################SPL
# Reshape the data into long format
heterosis_data_long <- heterosis_data %>%
  pivot_longer(cols = c(SPL_MPH,SPL_BPH), names_to = "Variable", values_to = "Value")

# Plot the data with y-axis limit
spl<-ggplot(heterosis_data_long, aes(x = crosses, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 0.91) +
  geom_vline(xintercept = 0, color = "black",linewidth = 0.2) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey", linewidth =  0.2),
    axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5), # Border added
    plot.title.position = "plot",
    plot.title = element_text(size = 14, hjust = 0.5, vjust = 1.5),
    plot.margin = margin(t = 6, r = 11, b = 11, l = 11), # Adjust margins
    legend.position.inside = c(0.9, 0.9), 
    legend.position = c(0.9, 0.9),                 
  ) +
  labs(
    x = "",
    y = "Level of Heterosis (%)",
    fill = "",
  ) +
  scale_fill_manual(values = c("SPL_MPH" = "red", "SPL_BPH" = "blue")) +
  scale_y_continuous(limits = c(0,50 ), breaks = seq(0, 50, by = 5))+
  annotate(
    "text",
    x = 1,                          # Adjust based on the x-axis range
    y = 47,                          # Position the title near the top within the border
    label = "Spike length",    # Title text
    size = 6, color = "black", fontface = "plain", hjust = 0
  )



########NSPS

# Reshape the data into long format
heterosis_data_long <- heterosis_data %>%
  pivot_longer(cols = c(NSPS_MPH,NSPS_BPH), names_to = "Variable", values_to = "Value")

# Plot the data with y-axis limit
nsps<-ggplot(heterosis_data_long, aes(x = crosses, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 0.91) +
  geom_vline(xintercept = 0, color = "black",linewidth = 0.2) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey", linewidth =  0.2),
    axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5), # Border added
    plot.title.position = "plot",
    plot.title = element_text(size = 14, hjust = 0.5, vjust = 1.5),
    plot.margin = margin(t = 6, r = 11, b = 11, l = 11), # Adjust margins
    legend.position.inside = c(0.9, 0.95), 
    legend.position = c(0.9, 0.95),                 
  ) +
  labs(
    x = "",
    y = "Level of Heterosis (%)",
    fill = "",
  ) +
  scale_fill_manual(values = c("NSPS_MPH" = "red", "NSPS_BPH" = "blue")) +
  scale_y_continuous(limits = c(-25,40 ), breaks = seq(-25, 40, by = 5))+
  annotate(
    "text",
    x = 2,                          # Adjust based on the x-axis range
    y = 38,                          # Position the title near the top within the border
    label = "Number of seeds per spike",    # Title text
    size = 6, color = "black", fontface = "plain", hjust = 0
  )


##############TSW
# Reshape the data into long format
heterosis_data_long <- heterosis_data %>%
  pivot_longer(cols = c(TSW_MPH,TSW_BPH), names_to = "Variable", values_to = "Value")

# Plot the data with y-axis limit
tsw<-ggplot(heterosis_data_long, aes(x = crosses, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 0.91) +
  geom_vline(xintercept = 0, color = "black",linewidth = 0.2) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey", linewidth =  0.2),
    axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5), # Border added
    plot.title.position = "plot",
    plot.title = element_text(size = 14, hjust = 0.5, vjust = 1.5),
    plot.margin = margin(t = 6, r = 11, b = 11, l = 11), # Adjust margins
    legend.position.inside = c(0.9, 0.95), 
    legend.position = c(0.9, 0.95),                 
  ) +
  labs(
    x = "",
    y = "Level of Heterosis (%)",
    fill = "",
  ) +
  scale_fill_manual(values = c("TSW_MPH" = "red", "TSW_BPH" = "blue")) +
  scale_y_continuous(limits = c(-20, 30 ), breaks = seq(-20, 30, by = 5))+
  annotate(
    "text",
    x = 3,                          # Adjust based on the x-axis range
    y = 28,                          # Position the title near the top within the border
    label = "Thousand seed weight",    # Title text
    size = 6, color = "black", fontface = "plain", hjust = 0
  )




#########combined_histogram##########
install.packages("patchwork")
library(patchwork)

plot<-dtf/plh/spl|nsps/tsw

# Save the plot
ggsave("my_plot.png", plot = plot, width = 14, height = 9, dpi = 300)



#######################################################################################################

############mean performance of 33barley genotypes#############################
install.packages("readxl")
library(readxl)
install.packages("Rcmdr")#important for to import excel data
library("Rcmdr")
install.packages("dplyr")
library("dplyr") 
install.packages("agricolae")
library(agricolae)
##if i want to make graph using ggplot
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
install.packages("reshape")
library(reshape2)
install.packages("dendextend")
library(dendextend)
install.packages("car")
library(car)
install.packages("ggpubr") ### the package used to visualize the relationship between variables
library("ggpubr")
install.packages("ggrepel")##is used to visualize plots in overlapped way
library(ggrepel)
install.packages("patchwork")
library(patchwork)
setwd("C:/Berdasa_Barley_Feild & RNA Data_2024/Field_Data_2024/field_data_analysis_R")
getwd()

library(readxl)
pheno_data<- read_excel("C:/Berdasa_Barley_Feild & RNA Data_2024/Field_Data_2024/field_data_analysis_R/Phentypic_data33_mean_RNA.xlsx",
                            sheet = NULL, col_names = TRUE)
View(pheno_data)
str(pheno_data)
pheno_data$row_type<-as.character(as.numeric(pheno_data$row_type))

str(pheno_data)

install.packages("tidyr")
library(tidyr)

###Phenotypic_DTF

dtf<-ggplot(pheno_data, aes(x = crosses, y = DTF)) +
  geom_bar(stat = "identity", fill = "red2", color = "black", alpha = 0.8) +
  geom_vline(xintercept = 0, color = "black", size = 0.2) +
  theme_minimal() +
  theme( panel.grid = element_blank(),
         panel.grid.major.y = element_line(color = "grey", size = 0.5),
         axis.text.x = element_text(angle = 45, vjust = 1.1, hjust = 1),
         panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)) + # Border added +
  labs(x = "", y = "Number of days", title = "")+
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 20)) +
  annotate(
    "text",
    x = 1,                          # Adjust based on the x-axis range
    y = 118,                          # Position the title near the top within the border
    label = "Days to flowering(days)",    # Title text
    size = 6, color = "black", fontface = "plain", hjust = 0
  )



###Phenotypic_PLH

plh<-ggplot(pheno_data, aes(x = crosses, y = PLH)) +
  geom_bar(stat = "identity", fill = "green2", color = "black", alpha = 0.8) +
  geom_vline(xintercept = 0, color = "black", size = 0.2) +
  theme_minimal() +
  theme( panel.grid = element_blank(),
         panel.grid.major.y = element_line(color = "grey", size = 0.5),
         axis.text.x = element_text(angle = 45, vjust = 1.1, hjust = 1),
         panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)) + # Border added +
  labs(x = "", y = "Height", title = "")+
  scale_y_continuous(limits = c(0, 130), breaks = seq(0, 130, by = 20)) +
  annotate(
    "text",
    x = 1,                          # Adjust based on the x-axis range
    y = 129,                          # Position the title near the top within the border
    label = "Plant height(cm)",    # Title text
    size = 6, color = "black", fontface = "plain", hjust = 0
  )


###Phenotypic_SPL

spl<-ggplot(pheno_data, aes(x = crosses, y = SPL)) +
  geom_bar(stat = "identity", fill = "magenta", color = "black", alpha = 0.8) +
  geom_vline(xintercept = 0, color = "black", size = 0.2) +
  theme_minimal() +
  theme( panel.grid = element_blank(),
         panel.grid.major.y = element_line(color = "grey", size = 0.5),
         axis.text.x = element_text(angle = 45, vjust = 1.1, hjust = 1),
         panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)) + # Border added +
  labs(x = "", y = "Length", title = "")+
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  annotate(
    "text",
    x = 1,                          # Adjust based on the x-axis range
    y = 9,                          # Position the title near the top within the border
    label = "Spike length(cm)",    # Title text
    size = 6, color = "black", fontface = "plain", hjust = 0
  )


##Phenotypic_NSPS

nsps<-ggplot(pheno_data, aes(x = crosses, y = NSPS)) +
  geom_bar(stat = "identity", fill = "blue1", color = "black", alpha = 0.8) +
  geom_vline(xintercept = 0, color = "black", size = 0.2) +
  theme_minimal() +
  theme( panel.grid = element_blank(),
         panel.grid.major.y = element_line(color = "grey", size = 0.5),
         axis.text.x = element_text(angle = 45, vjust = 1.1, hjust = 1),
         panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)) + # Border added +
  labs(x = "", y = "Count", title = "")+
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 5)) +
  annotate(
    "text",
    x = 1,                          # Adjust based on the x-axis range
    y = 48,                          # Position the title near the top within the border
    label = "Number of seeds per spike",    # Title text
    size = 6, color = "black", fontface = "plain", hjust = 0
  )

##Phenotypic_TSW
tsw<-ggplot(pheno_data, aes(x = crosses, y = TSW)) +
  geom_bar(stat = "identity", fill = "cyan1", color = "black", alpha = 0.8) +
  geom_vline(xintercept = 0, color = "black", size = 0.2) +
  theme_minimal() +
  theme( panel.grid = element_blank(),
         panel.grid.major.y = element_line(color = "grey", size = 0.5),
         axis.text.x = element_text(angle = 45, vjust = 1.1, hjust = 1),
         panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)) + # Border added +
  labs(x = "", y = "weight", title = "")+
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 20)) +
  annotate(
    "text",
    x = 1,                          # Adjust based on the x-axis range
    y = 58,                          # Position the title near the top within the border
    label = "Thousand seed weight(g)",    # Title text
    size = 6, color = "black", fontface = "plain", hjust = 0
  )


#########combined_histogram##########
install.packages("patchwork")
library(patchwork)

plot<-dtf/plh/spl|nsps/tsw

# Save the plot
ggsave("pheno_plot.png", plot = plot, width = 16, height = 10, dpi = 300)


#########################################################################################
###################################################################
###################################################################
