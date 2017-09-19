# Try to install package tidyverse

install.packages("tidyverse")

# Call functions in "tivyverse" every day

library(tidyverse)

#Add library: stringr

library(stringr)

#Add lubridate
library(lubridate)

# Using the data base df.trans.forT1 as a base.
# Step 1. Take all historical T1 shipments 
#(both transfer and to customer) 

# Choose the Single delivery
#Try to filter the single delivery
#By look at the columns Documento.de.Custo.
#Filter the unique shipment.

Single.delivery <- df.trans.forT1 %>%
  
  group_by(Shipment.Document) %>%
  summarize (Occurence = n()) %>%
  filter(Occurence==1)


#merge with sample.T1.Delivery.

Sample.T1.delivery <-df.trans.forT1 %>%
  left_join(Single.delivery) %>%
  
  # then filter the column Occurance
  #Arrange the 
  
  filter(Occurence==1)%>%
mutate(Cbind_Custype_Shiptype = str_c(T1.T2.x,Shipment.type,sep="-")) 


#and exclude all historical rodotrem shipments (if any)

Sample.T1.1.Delivery <- Sample.T1.delivery %>%
filter(
  #T1.T2=="T1",
  # Shipment.type==c("VENDA","")
  Cbind_Custype_Shiptype %in% c("T1-VENDA","T1-TRANSFERENCIA"), 
  Dest.City..SAP. != "Rio de Janeiro",
  !Dest.State %in% c("AM","RO","RR","AP","AC","PA"),
  Cost.category=="Transporte",
  Fiscal.Unit=="7032") %>% #Araras DC
  
  #Vehicle.SAP.code=="BR024")%>%
  
  # Arrange the records in the same date
  
# Try to fix the string of date
  
  #mutate(Dates = mdy(str_replace(Date,"\", \"",", 20"))) %>%
  
  # Now we can arrage in the proper ways.
  arrange(transport.Date,Dest.City..SAP.) %>%
  
# Choose the suitable Truck for the weight.
  mutate(Suit.Truck.Size=
           (actual.Gross.weight.Kg>0)*(actual.Gross.weight.Kg<=1300)*1300
         +(actual.Gross.weight.Kg>1300)*(actual.Gross.weight.Kg<=3000)*3000
         +(actual.Gross.weight.Kg>3000)*(actual.Gross.weight.Kg<=6000)*6000
         +(actual.Gross.weight.Kg>6000)*(actual.Gross.weight.Kg<=10800)*10800
         +(actual.Gross.weight.Kg>10800)*(actual.Gross.weight.Kg<=27000)*27000
         +(actual.Gross.weight.Kg>27000)*42000) %>%
    
    # Find the name of the truck size  
    mutate(Suit.Truckcode=df.trucksize$SAP.Replace.Code[match(Suit.Truck.Size,df.trucksize$`Capacity.(KG)`)]) %>%
    
    
 # Create the lookup columns in transport data following the format
  #1208-ARARAS-TRANSFERENCIA-BR275-100223303
  # Origin Code (1208) 
  #-> Desination City (ARARAS in capital) 
  #-> Type of Transport (TRANSFERENCIA)
  #-> Vehicle Type SAP (BR275) 
  #-> Carrer Name (100223303)
  
  mutate(Lookup.trans= paste(Fiscal.Unit,str_to_upper(Dest.City..SAP.),Vehicle.SAP.code,sep="-")) %>%
  
  # Calculate the Current transportation Cost.
  
    mutate(Current.trans.cost=df.cost.transportation$TARIFA.FRETE[match(Lookup.trans,df.cost.transportation$Look.Up)]) %>%
  
    
    # Create the lookup columns in transport data following the format
    mutate(Suit.Lookup.trans= paste(Fiscal.Unit,str_to_upper(Dest.City..SAP.),Suit.Truckcode,sep="-")) %>%
  
    # Calculate the suit transportation Cost.
    mutate(Suit_trans.cost=df.cost.transportation$TARIFA.FRETE[match(Suit.Lookup.trans,df.cost.transportation$Look.Up)])
  