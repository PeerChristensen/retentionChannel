
library(tidyverse)

df <- read_csv("retention_test.csv",header)

colnames(df) <- c("Customer_Key","Date_Key","Medium","Source","TrafficChannel","End_Date")
