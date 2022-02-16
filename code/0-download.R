# load libraries
library(tidyverse)

# download game data
setwd("~/Dropbox (Penn)/__SPRING 2022/STAT401")
data <- read.csv(file = 'combined.csv')

# write raw data to file
write.csv(x = data, file = "data/raw/data_raw.csv")
