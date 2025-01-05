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