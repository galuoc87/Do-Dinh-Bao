# Try to install package tidyverse

install.packages("tidyverse")

# Call functions in "tivyverse" every day

library(tidyverse)

#Add library: stringr

library(stringr)

#Add lubridate
library(lubridate)

# Add geosphere to calculate the Geographic distance:


library(sp)
library(geosphere)

# Create the summary table for total weight and freight cost for T1:

Sumtab_T1_A <- df.success.transportation %>%
  mutate(T1.Shipment.lookup = str_c(Fiscal.Unit,T1.T2.y,Shipment.type,sep = ".")) %>%
  filter( T1.Shipment.lookup %in% c("7032.T1.TNF",
                                    "7032.T1.TRANSFERENCIA",
                                    "7032.T1.VENDA",
                                    "7264.T2.TNF")
  )
  
  Sumtab_T1_A_1 <- Sumtab_T1_A %>%
  group_by(Shipment.type) %>%
  summarise(sum(actual.Gross.weight.Kg)/1000,
            sum(actual.Net.freight.cost)/1000)
# Extract to excel file
write.csv(Sumtab_T1_A_1, file = "C:/Users/Bao/Desktop/SumT1araras.csv", row.names = FALSE,fileEncoding="UTF-8")



#Summary table for vehicle type from Araras
Sumtab_T1_A_2 <- Sumtab_T1_A %>%
  group_by(Shipment.Document) %>%
  mutate (Totalvolume = sum(actual.Gross.weight.Kg),
          Rankweight =row_number()) %>%
mutate (correct.vol = ifelse(Rankweight ==1 ,Totalvolume,0 )) %>%
mutate(Shipment.Nu = cut(correct.vol/1000, breaks = seq(10, 43, by = 0.5))) %>%
  group_by(Shipment.Nu) %>%
   mutate(n= n(),na.rm=TRUE) %>%
  spread(key = Vehicle.SAP.code, value = n , fill = 0) %>%
ungroup
  
  
 # Summary table()
  Sumtab_T1_A_2 <- Sumtab_T1_A_2 %>%
  group_by(Shipment.Nu) %>%
summarise(TRUCK = mean(BR020),
          CARRETA = (mean(BR004)+mean(BR022)+mean(BR024)+mean(BR275) +mean (BR362)),
          RODOTREM = mean(BR012)
               )
  # Extract to excel file
  write.csv(Sumtab_T1_A_2, file = "C:/Users/Bao/Desktop/SumT1_2araras.csv", row.names = FALSE,fileEncoding="UTF-8")



  # Create the summary table for total volume in MT for each shipment type 
  
  
  Sumtab_T1_A_2 <- Sumtab_T1_A %>%
  group_by(Shipment.Document) %>%
    mutate (Totalvolume = sum(actual.Gross.weight.Kg),
            Rankweight =row_number()) %>%
    mutate (correct.vol = ifelse(Rankweight ==1 ,Totalvolume,0 ))
  mutate(Shipment.Nu = cut(correct.vol/1000, breaks = seq(10, 43, by = 0.5))) %>%
    group_by(Shipment.Nu) %>%
    mutate(n= n(),na.rm=TRUE) %>%
    spread(key = Vehicle.SAP.code, value = n , fill = 0) %>%
    ungroup
  
  # Summary table()
  Sumtab_T1_A_2 <- Sumtab_T1_A_2 %>%
    group_by(Shipment.Nu) %>%
    summarise(VAN = mean(BR300),
              CARRETA = (mean(BR024)+mean(BR275))
  
  write.csv(Sumtab_T1_G_3, file = "C:/Users/Bao/Desktop/SumT1_3garanhus.csv", row.names = FALSE,fileEncoding="UTF-8")
  
  # Create the daily demand for vehicle from Araras
    
    Sumtab_T1_A_3 <- Sumtab_T1_A %>%
    group_by(Shipment.Document) %>%
    mutate (Totalvolume = sum(actual.Gross.weight.Kg),
            Rankweight =row_number()) %>%
    mutate (Occurance = ifelse(Rankweight ==1 ,1,0 )) %>%
    group_by(transport.Date) %>%
    filter(Rankweight ==1)%>%
    summarise(n())
  
  write.csv(Sumtab_T1_A_3, file = "C:/Users/Bao/Desktop/SumT1_3araras.csv", row.names = FALSE,fileEncoding="UTF-8")
    
    # Create the filling rate per month in Araras
  Sumtab_T1_A_4 <- Sumtab_T1_A %>%
    group_by(Shipment.Document) %>%
    mutate (Totalvolume = sum(actual.Gross.weight.Kg),
            Rankweight =row_number()) %>%
    mutate (cor.vol = ifelse(Rankweight ==1 ,Totalvolume,0 )) %>%
    mutate (filling.rate = ifelse(Rankweight ==1 ,( Totalvolume / df.trucksize$`Capacity.(KG)`[match(Vehicle.SAP.code,df.trucksize$SAP.code)]*100),0 )) %>%
    mutate( month = format(transport.Date,"%m"),
          year = format(transport.Date,"%y") )%>%
    group_by(filling.rate) %>%
    mutate(avg= mean(filling.rate[filling.rate>0])) %>%
  spread(key = Shipment.type, value = avg , fill = 0)
      
    # Summary table
    Sumtab_T1_A_4 <- Sumtab_T1_A_4 %>%
    group_by(month,year) %>%
    summarise(Sale = mean(VENDA[VENDA>0]),
              Transfer = mean(TRANSFERENCIA[TRANSFERENCIA>0]))
      
    write.csv(Sumtab_T1_A_4, file = "C:/Users/Bao/Desktop/SumT1_4araras.csv", row.names = FALSE,fileEncoding="UTF-8")
      

  
  
  
  
  
  
  
  





# Create the summary table for total weight and freight cost for T1 from Garanhuns

Sumtab_T1_G <- df.success.transportation %>%
  mutate(T1.Shipment.lookup = str_c(Fiscal.Unit,T1.T2.y,Shipment.type,sep = ".")) %>%
  filter( T1.Shipment.lookup %in% c("4744.T1.TNF",
                                    "4744.T1.TRANSFERENCIA",
                                    "4744.T1.VENDA"))
  
  Sumtab_T1_G_1 <- Sumtab_T1_G %>%
  group_by(Shipment.type) %>%
  summarise(sum(actual.Gross.weight.Kg)/1000,
            sum(actual.Net.freight.cost)/1000)

# Extract to excel file
write.csv(Sumtab_T1_G_1, file = "C:/Users/Bao/Desktop/SumT1Garanhus.csv", row.names = FALSE)


#Summary table for vehicle type from Garanhus
Sumtab_T1_G_2 <- Sumtab_T1_G %>%
  group_by(Shipment.Document) %>%
  mutate (Totalvolume = sum(actual.Gross.weight.Kg),
          Rankweight =row_number()) %>%
  mutate (correct.vol = ifelse(Rankweight ==1 ,Totalvolume,0 )) %>%
  mutate(Shipment.Nu = cut(correct.vol/1000, breaks = seq(10, 43, by = 0.5))) %>%
  group_by(Shipment.Nu) %>%
  mutate(n= n(),na.rm=TRUE) %>%
  spread(key = Vehicle.SAP.code, value = n , fill = 0) %>%
  ungroup



# Summary table()
Sumtab_T1_G_2 <- Sumtab_T1_G_2 %>%
  group_by(Shipment.Nu) %>%
  summarise(VAN = mean(BR300),
            CARRETA = (mean(BR024)+mean(BR275))
  )
# Extract to excel file
write.csv(Sumtab_T1_G_2, file = "C:/Users/Bao/Desktop/SumT1_2garanhus.csv", row.names = FALSE,fileEncoding="UTF-8")

# Create the daily demand for vehicle from Garanhus

Sumtab_T1_G_3 <- Sumtab_T1_G %>%
  group_by(Shipment.Document) %>%
  mutate (Totalvolume = sum(actual.Gross.weight.Kg),
          Rankweight =row_number()) %>%
  mutate (Occurance = ifelse(Rankweight ==1 ,1,0 )) %>%
  group_by(transport.Date) %>%
  filter(Rankweight ==1)%>%
  summarise(n())

write.csv(Sumtab_T1_G_3, file = "C:/Users/Bao/Desktop/SumT1_3garanhus.csv", row.names = FALSE,fileEncoding="UTF-8")




# Create the filling rate per month in Araras
Sumtab_T1_G_4 <- Sumtab_T1_G %>%
  group_by(Shipment.Document) %>%
  mutate (Totalvolume = sum(actual.Gross.weight.Kg),
          Rankweight =row_number()) %>%
  mutate (cor.vol = ifelse(Rankweight ==1 ,Totalvolume,0 )) %>%
  mutate (filling.rate = ifelse(Rankweight ==1 ,( Totalvolume / df.trucksize$`Capacity.(KG)`[match(Vehicle.SAP.code,df.trucksize$SAP.code)]*100),0 )) %>%
  mutate( month = format(transport.Date,"%m"),
          year = format(transaction.Date,"%y") )%>%
  filter(!(filling.rate>1000))%>%
  group_by(filling.rate) %>%
  mutate(avg= mean(filling.rate[filling.rate>0])) %>%
  spread(key = Shipment.type, value = avg , fill = 0)

# Summary table
Sumtab_T1_G_4 <- Sumtab_T1_G_4 %>%
  group_by(month,year) %>%
  summarise(Sale = mean(VENDA[VENDA>0]),
            Transfer = mean(TRANSFERENCIA[TRANSFERENCIA>0]))

write.csv(RoRoTrem.Analysis, file = "C:/Users/Bao/Desktop/roro.csv", row.names = FALSE,fileEncoding="UTF-8")
