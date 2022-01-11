load("../Data/All_Dcrab.Rdata")
library(ggplot2)

# Shell Condition 
Total_data_N$Shell.Cond <- ifelse(Total_data_N$Shell.Cond == "Slightly soft", "New", ifelse(Total_data_N$Shell.Cond == "Soft", "New", ifelse(Total_data_N$Shell.Cond =="New shell, soft legs", "New", ifelse(Total_data_N$Shell.Cond =="Hard shell, hard legs", "Old shell", ifelse(Total_data_N$Shell.Cond =="", NA, Total_data_N$Shell.Cond)) )))

ggplot(Total_data_N[Total_data_N$Shell.Cond != "NA",], aes(Date, fill = Shell.Cond))+
  geom_bar(position = "stack") +
  ggtitle("Shell Condition")
