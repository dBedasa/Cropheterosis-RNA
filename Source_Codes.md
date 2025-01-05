#######R_script for data analysis####

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

#set working directory
setwd("C:/Berdasa_Barley_Feild & RNA Data_2024/Field_Data_2024/field_data_analysis_R")
getwd()

library(readr)
HV013_sequence_lengths_with_header <- read_csv("HV013_sequence_lengths_with_header.txt")
View(HV013_sequence_lengths_with_header)

# Load the data from the text file
library(readr)

data13 <- read.table("HV013_sequence_lengths_with_header.txt", header = TRUE, col.names = c("Length", "Frequency"))
View(data13)
sum(is.na(data13$Length))  # Check for NA in Length column
sum(is.infinite(data13$Length))  # Check for infinite values in Length column
sum(is.na(data13$Frequency))  # Check for NA in Frequency column
sum(is.infinite(data13$Frequency))  # Check for infinite values in Frequency column

# Expand the data according to the count column
expanded_data13 <- rep(data13$Length, data13$Frequency)

# Create the histogram
HV013B <- ggplot() +
   geom_histogram(aes(x = expanded_data13), bins = 33, fill = "#00BFFF", color = "black", alpha = 0.7) +
   labs(title = "HV013 Histogram of read Lengths before tramming$fltering", x = "Read Length", y = "Frequency") +
   theme_minimal() +
   scale_x_continuous(limits = c(0, 3000), breaks = seq(0, 3000, by = 200)) +
   stat_bin(aes(x = expanded_data13, label = ..count..), geom = "text", vjust = 0, bins = 33, size = 2.5) +  # Add text labels
   geom_hline(yintercept = 0, color = "black", size = 0.5) +  # Horizontal line at y = 0
   geom_vline(xintercept = 0, color = "black", size = 0.5) +  # Vertical line at x = 0
   theme(
     axis.text.y = element_text(hjust = 1, margin = margin(r = 0)),  # Move y-axis labels closer
     axis.title.y = element_text(margin = margin(r = 10))             # Move y-axis title closer
   )


   ###for a script


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




##################################################
####Box_plot

pheno_box <- pheno_data %>%
  pivot_longer(cols = c(DTF, PLH, SPL, NSPS,TSW), 
               names_to = "Trait", 
               values_to = "Value")

box<-ggplot(pheno_box, aes(x = Trait, y = Value, fill = Trait)) + 
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape = 8, 
               size = 2, color = "blue") +
  theme_minimal() +
  labs(title = "",
       x = "Trait",
       y = "Value",
       axis.text.x = element_text(vjust = 1.2, hjust = 1))
       #we don't need extention for a data we are going to updoad to out Github


