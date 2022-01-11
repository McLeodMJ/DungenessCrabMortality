

load("../Data/All_Dcrab.Rdata")
library(patchwork)
library(ggplot2)


Total_data_N$Size <- ifelse(Total_data_N$CW < 150,"Juv", ifelse(Total_data_N$CW >= 150 & Total_data_N$CW <=159,"Sub", ifelse(Total_data_N$CW >=160, "Legal", NA)))

#all of the crabs based on size
All_sizes <- ggplot(Total_data_N[Total_data_N$Size != "NA",], aes(Date, fill = Size))+
  geom_bar(position = "stack", width = 60, size = 2) +
  ggtitle("Males & Females") 

#males based on size class
Male_sizes <- ggplot(Total_data_N[Total_data_N$Sex ==1, ], aes(Date, fill = Size))+
  geom_bar(position = "stack", width= 60, size = 2)+
  ggtitle("Males Only")

#Join the plots
All_sizes + Male_sizes


