#!/bin/bash

# Get the week number from the user
read -p "What week are you working on? " week_number

# Create a folder for the week
mkdir -p 2024/2024-week$week_number

# Create subfolders for data and plots
mkdir 2024/2024-week$week_number/data
mkdir 2024/2024-week$week_number/plots

# Create a README file
touch 2024/2024-week$week_number/README.md
echo "# Week $week_number plot

![](plots/plot_2024w$week_number.png)" > 2024/2024-week$week_number/README.md

# Create a script file
touch 2024/2024-week$week_number/plot_2024w$week_number.R
echo "# TidyTuesday - 2024 Week $week_number
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(tidytuesdayR)

# Download and read datatsets ----
# Sources:

data <- tidytuesdayR::tt_load(2024, week = $week_number)


# Export plot
ggsave(\"2024/2024-week$week_number/plots/plot_2024w$week_number.png\",
    width = 2000,
    height = 1800,
    units = \"px\")" > 2024/2024-week$week_number/plot_2024w$week_number.R

# Print donde message
echo "Created folder for week $week_number"
