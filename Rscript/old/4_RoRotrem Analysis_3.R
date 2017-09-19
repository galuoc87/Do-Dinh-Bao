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

#Create the RoRotrem Analysis by arrange the Data 
#for 
RoRoTrem.Analysis <- 
  as_tibble(Sample.T1.1.Delivery) %>%
  
  # Choose the record which is below 21 Tons.
  filter(!is.na(Current.trans.cost))%>%
  filter(actual.Gross.weight.Kg <= 21200)
  
  
  # Insert the distance per Customer.
  # by create the lookup table
 # mutate(Cust.Look.up=df.customer$`Cidade Recebedor Mercadoria (SAP)`) %>%
  # Lookup with customer data frame
  RoRoTrem.Analysis <-RoRoTrem.Analysis %>%
  rowwise() %>%
  mutate(Distance.GPS = distm (c(df.GPS.site$lng[match(Fiscal.Unit,df.GPS.site$`Planta Origem`)], 
                                 df.GPS.site$lat[match(Fiscal.Unit,df.GPS.site$`Planta Origem`)]), 
                               c(df.GPS.customer$lng[match(Dest.Code,df.GPS.customer$`SAP Recebedor Mercadoria`)],
                                 df.GPS.customer$lat[match(Dest.Code,df.GPS.customer$`SAP Recebedor Mercadoria`)]
                               ), fun = distHaversine)/1000
  ) %>%
  
  mutate(First.geography=df.customer$Microrregiao[match(str_to_upper(Dest.City..SAP.),df.customer$`Cidade Recebedor Mercadoria`)]) %>%
  
  mutate(Second.geography=df.customer$Messorregiao[match(str_to_upper(Dest.City..SAP.),df.customer$`Cidade Recebedor Mercadoria`)]) %>%
  
  
  # Start to arrange the table by Date, 
  arrange(transaction.Date,Distance.GPS) %>%

#  Create the Acccumulative weight by date.
group_by(transaction.Date,First.geography) %>%
  mutate(Cumweight=cumsum(actual.Gross.weight.Kg)) %>%
  ungroup() %>%
  
  

# Add the Ranks for the date
group_by(transaction.Date,First.geography) %>%
  mutate(Rank.Date=row_number()) %>%
  mutate(Max.Date=max(Rank.Date)) %>%
  ungroup() 

  # Link with SKus to have CBM, Qty, MT and Pallet.
  
  #RoRoTrem.Analysis <- RoRoTrem.Analysis %>%
   # left_join(Sum.Ship.Order2)
 
   
  
  #Consolidation Case to group the volume per customer by, 
# 2 shipments into 1 
  
    RoRoTrem.Analysis <- RoRoTrem.Analysis %>%
    group_by(transaction.Date) %>%
  mutate(Consolidation =ifelse(Max.Date==1 | Rank.Date==2, Cumweight,
    ifelse((Rank.Date %% 2 )==0,
           Cumweight - lag(Cumweight, n=2L, default = 0), 
    ifelse(
      Rank.Date==Max.Date, 
      Cumweight - lag(Cumweight, n=1L, default = 0), 
      0
      )
    ) 
    )
  )
    
  
  # of Roro Truck
RoRoTrem.Analysis <-RoRoTrem.Analysis %>%
 mutate(Nu.RoRo = floor(Consolidation / 42000)) %>%
 mutate(Remaining.Qty = Consolidation - Nu.RoRo *42000) %>%
  mutate(S.Truck.Vol=
           (Remaining.Qty>0)*(Remaining.Qty<=1300)*1300
         +(Remaining.Qty>1300)*(Remaining.Qty<=3000)*3000
         +(Remaining.Qty>3000)*(Remaining.Qty<=6000)*6000
         +(Remaining.Qty>6000)*(Remaining.Qty<=10800)*10800
         +(Remaining.Qty>10800)*(Remaining.Qty<=27000)*27000
         +(Remaining.Qty>27000)*42000) %>% # Assign the suitatble truck for remaining

mutate(S.Truck.code=df.trucksize$SAP.Replace.Code[match(S.Truck.Vol,df.trucksize$`Capacity.(KG)`)]) %>%
  
  # Create the lookup columns in transport data following the format
  #1208-ARARAS-TRANSFERENCIA-BR275-100223303
  # Origin Code (1208) 
  #-> Desination City (ARARAS in capital) 
  #-> Type of Transport (TRANSFERENCIA)
  #-> Vehicle Type SAP (BR275) 
  #-> Carrer Name (100223303)
  
  mutate(S.Lookup.re= paste(Fiscal.Unit,str_to_upper(Dest.City..SAP.),S.Truck.code,sep="-")) %>%
  
  mutate(S.Lookup.RO= paste(Fiscal.Unit,str_to_upper(Dest.City..SAP.),"BR012",sep="-")) %>%
  
  # Calculate the  transportation Cost.
  mutate(S_trans.cost=ifelse(Consolidation ==0,0,
                             (df.cost.transportation$TARIFA.FRETE[match(S.Lookup.re,df.cost.transportation$Look.Up)])
                             +(df.cost.transportation$TARIFA.FRETE[match(S.Lookup.RO,df.cost.transportation$Look.Up)])*Nu.RoRo))

# Check error
Check.erro.Rodo = RoRoTrem.Analysis %>%
  filter(is.na(S_trans.cost))


sum(RoRoTrem.Analysis$S_trans.cost)
sum(RoRoTrem.Analysis$Current.trans.cost)
# export to Excel File:

write.csv(RoRoTrem.Analysis, file = "C:/Users/Bao/Desktop/Rodo.Analysis5.csv", row.names = FALSE)