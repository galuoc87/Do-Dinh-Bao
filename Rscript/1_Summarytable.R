# Try to install package tidyverse

install.packages("tidyverse")

# Call functions in "tivyverse" every day

library(tidyverse)

#Add library: stringr

library(stringr)

# import the CSV file of Transporation (without the detail SKUs)- converted by Matthieu

df.transportation <- read_csv("D:\\Goolge Drive\\FOBRA\\02. Project Documents\\04. Data Analysis\\Analysis BAO\\Data\\DPA_Transport-db-fy17_V01_17Aug2017_MAT.csv",skip=1)


# Create the summary table started from Customer Type > Document type > Shipment Type > Cost Category > Destination > Vehicle Typewith the sum of cost and weight
# toghether the count of time
# Remember to add the name for each columns

Summary.transport <- df.transportation %>%
  
  group_by(Customer.type = df.transportation$`T1.T2`,
           Shipment.Order = df.transportation$`Documento.de.Custo`,
           Shipment.type = df.transportation$`Shipment.type`,
           Cost.Category = df.transportation$`Cost.category`,
           Destination.sap = df.transportation$`Dest.City (SAP)`,
           Vehicle.type = df.transportation$Vehicle.SAP.code) %>%

  summarise(Total.Freight.Cost=sum(Net.freight.cost,na.rm=TRUE)
            ,Total.weight=sum(Gross.weight.Kg,na.rm=TRUE)
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
  left_join(Shipment.avai.T1,c("Documento.de.Custo" = "Unique.order")) %>%
  
  # then filter the column Check.avai
  
  filter(Check.avai==1)
