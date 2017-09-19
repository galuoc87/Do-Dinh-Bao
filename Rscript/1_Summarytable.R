# Try to install package tidyverse

install.packages("tidyverse")

# Call functions in "tivyverse" every day

library(tidyverse)

#Add library: stringr

library(stringr)

# import the CSV file of Transporation (without the detail SKUs)- converted by Matthieu

df.destination <- read_csv("D:\\Goolge Drive\\FOBRA\\02. Project Documents\\04. Data Analysis\\Analysis BAO\\Data\\transportation.FY2017.unique.del_V03.csv")

# used Youssef verison as the reference
# (remove)Add the pallet and CBM to the cleansed Transportation Data

#df.transportation <- df.transportation %>%
 # mutate(Chave.CUSTLIST = str_c(Fiscal.Unit,Dest.Code,sep = ".")) %>%
  #left_join(Sum.Ship.Order)
  
  
  
  #df.transportation <- df.transportation %>%
  #group_by(Shipment.Document) %>%
  #mutate(Total.frieght = sum(actual.Net.freight.cost),
  #       Occurance = n()
   #      )
  
df.success.transportation <- df.transportation %>%
  group_by(Shipment.Document)%>%
  arrange(Shipment.Document) %>%
  mutate(Totalfrieghtcost = sum(actual.Net.freight.cost),
         Occurance = n(),
         Rank = row_number()
  ) %>%
  filter(!(is.na(actual.pallet) | Totalfrieghtcost <= 0))
  #write.csv(df.success.transportation, file = "C:/Users/Bao/Desktop/DPA_Raw.Transport.csv", row.names = FALSE)
  #write.csv(df.transportation, file = "C:/Users/Bao/Desktop/DPA_Physical Delivery.csv", row.names = FALSE)
  
# Create the summary table started from Customer Type > Document type > Shipment Type > Cost Category > Destination > Vehicle Typewith the sum of cost and weight
# toghether the count of time
# Remember to add the name for each columns

Summary.transport <- df.transportation %>%
  
  group_by(Customer.type = T1.T2.x,
           Shipment.Order = Shipment.Document,
           Shipment.type = Shipment.type,
           Cost.Category = Cost.category,
           Destination.sap = Dest.City..SAP.,
           Vehicle.type = Vehicle.SAP.code) %>%

  summarise(Total.Freight.Cost=sum(actual.Net.freight.cost,na.rm=TRUE)
            ,Total.weight=sum(actual.Gross.weight.Kg,na.rm=TRUE)
            ,Num.of.occurrent=(n()))




  # Try to reduce the size of the data by focus only for T1 and TNF 
# Try to sort data from Decs
 
Sort.customer<-filter(Summary.transport, Customer.type %in% c("T1","T2")) %>%
  arrange(Total.Freight.Cost) %>%

# Acording to the discussion: Disregards the value belongs to Transporte - pela referencia.
# For the Invoice switch:
# T1/T2 field "T1" or "T2" with shypment type "TNF" is the first leg.
# T1/T2 field "TNF" is the second leg.
  

 # Add the combination of the customer and shipment type)

  mutate(Cbind_Custype_Shiptype = str_c(Customer.type,Shipment.type,sep="-")) %>%

# Acording to the discussion: Disregards the value belongs to Transporte - pela referencia.
  
  filter(Total.Freight.Cost>0) %>%
  

# There could be another method to do by Ungrouping (see page 72)
  
  
  # List of Customer for lookup.

Shipment.avai.T1 <- tibble(
  "Unique.order" = unique(Sort.customer$Shipment.Order),
  "Check.avai" = rep(1,times=n_distinct(Sort.customer$Shipment.Order))
)



# Create the new data with filtering from Shipment Avalibale size.

df.trans.forT1 <- df.transportation %>%
  left_join(Shipment.avai.T1,c("Shipment.Document" = "Unique.order")) %>%
  
  # then filter the column Check.avai
  
  filter(Check.avai==1)
