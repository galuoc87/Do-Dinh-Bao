

# Contruct the summary table based on the internal

# Run DPLYR

library(dplyr)

# Rung the Tidyr

library(tidyr)

# Create the frequency table with intervals
Fequency.table<- RoRoTrem.Analysis %>%
 mutate(Shipment.Nu = cut(Gross.weight.Kg/1000, breaks = seq(10, 27, by = 1))) %>%
group_by(Shipment.Nu) %>%
summarise(n())


# export to excel file

write.csv(Fequency.table, file = "C:/Users/Do Dinh Bao/Desktop/Frequency.Shipment2.csv", row.names = FALSE)




# mutate(Shipment.Nu=ifelse(Gross.weight.Kg<10),"[<10]",
#ifelse((Gross.weight.Kg >= 10)*(Gross.weight.Kg < 15),))

