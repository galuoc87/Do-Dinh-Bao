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

#add the padr

install.packages("padr")

library(padr)


# Processed with the list of customer (narrowed for the centralized Delivery)

#great the working path

setwd("D:/Goolge Drive/FOBRA/02. Project Documents/04. Data Analysis/Analysis BAO/Data/Master Data/") #company

setwd("D:/Google Drive/FOBRA/02. Project Documents/04. Data Analysis/Analysis BAO/Data/Master Data/") #home

# Add the list of shorted customer.

df.cus.t2.centralized <- read_csv("Customer_Master_v02.csv") 

df.cus.t2.centralized <- df.cus.t2.centralized %>%
  mutate(Check.avai = rep(1,times=n()))

# Keep only 2 columns Des.Code and Check.avai

df.cus.t2.centralized <- df.cus.t2.centralized %>%
  select(Dest.Code,Check.avai)

# Add to the file 

df.success.transportation <- df.transportation %>%
  group_by(Shipment.Document)%>%
  arrange(Shipment.Document) %>%
  mutate(Totalfrieghtcost = sum(actual.Net.freight.cost),
         Occurance = n(),
         Rank = row_number()
  ) %>%
  filter(!(is.na(actual.pallet) | Totalfrieghtcost <= 0))


# add the group of customer to Df.success.transportation

df.t2.cen.shipment <- df.success.transportation  %>%
  left_join(df.cus.t2.centralized, by = "Dest.Code") %>%
filter(!is.na(Check.avai)) %>%
  
  filter(!(actual.pallet==0))

#add the distance

df.t2.cen.shipment <- df.t2.cen.shipment %>%
  rowwise() %>%
  mutate(Distance.GPS = distm (c(df.GPS.site$lng[match(Fiscal.Unit,df.GPS.site$`Planta Origem`)], 
                                 df.GPS.site$lat[match(Fiscal.Unit,df.GPS.site$`Planta Origem`)]), 
                               c(df.GPS.customer$lng[match(Dest.Code,df.GPS.customer$`SAP Recebedor Mercadoria`)],
                                 df.GPS.customer$lat[match(Dest.Code,df.GPS.customer$`SAP Recebedor Mercadoria`)]
                               ), fun = distHaversine)/1000
  )

# Consolidated multidrop.

df.t2.cen.shipment <- df.t2.cen.shipment %>%
  arrange(Shipment.Document,Distance.GPS) %>%
  group_by(Shipment.Document) %>%
  mutate (Totalvolume = sum(actual.Gross.weight.Kg),
          Totalvol.pallet = sum(actual.pallet),
          Totalvol.trancost= sum(actual.Net.freight.cost),
          Occurance = n(),
          Rank =row_number()) %>%
  mutate (consol.weight = ifelse(Rank ==Occurance ,Totalvolume,0 ),
          consol.pallet = ifelse(Rank ==Occurance ,Totalvol.pallet,0 ),
          consol.trancost= ifelse(Rank ==Occurance ,Totalvol.trancost,0 )
          ) %>%
  mutate (filling.rate = ifelse(Rank ==Occurance,( Totalvolume / df.trucksize$`Capacity.(KG)`[match(Vehicle.SAP.code,df.trucksize$SAP.code)]*100),0 )) %>%
  ungroup()
# calculate total transporatation cost

sum(df.t2.cen.shipment$actual.Net.freight.cost) #9598381

# Calculate the theoritica transportation cost

df.t2.cen.shipment <- df.t2.cen.shipment %>%
  mutate(consol.fiscal = ifelse(Rank ==Occurance ,Fiscal.Unit,0 ),
         consol.city = ifelse(Rank ==Occurance ,str_to_upper(Dest.City..SAP.),0 ),
         consol.truck.type = ifelse(Rank ==Occurance ,Vehicle.SAP.code,0 )
         ) %>%
  mutate(Lookup.current = paste(consol.fiscal,consol.city,consol.truck.type,sep="-"),
          Current.convert.transcost =ifelse( consol.pallet ==0,0,
            df.cost.transportation$TARIFA.FRETE[match(Lookup.current,df.cost.transportation$Look.Up)]
                                            )
                                            )
#check error in transport cost 
  Check.erro.Rodo = df.t2.cen.shipment %>%
  filter(is.na(Current.convert.transcost))
write.csv(df.t2.cen.shipment, file = "C:/Users/Bao/Desktop/T2basedline.csv", row.names = FALSE)

write.csv(Check.erro.Rodo, file = "C:/Users/Bao/Desktop/errorRodo.csv", row.names = FALSE)
sum(df.t2.cen.shipment$Current.convert.transcost) #15,062,304


# Try to filter T1 and T2 only:
df.t2.cen.shipment <- df.t2.cen.shipment %>%
  filter(!(T1.T2.x == "BRO"))


sum(df.t2.cen.shipment$actual.Net.freight.cost)
sum(df.t2.cen.shipment$Current.convert.transcost)

write.csv(df.t2.cen.shipment_2, file = "C:/Users/Bao/Desktop/T2 consolidation.csv", row.names = FALSE)

# Filter the toval total pallet below 0.8 pallet
df.t2.cen.shipment_2 <- df.t2.cen.shipment %>%
  filter((Totalvol.pallet > 0.8)) # will adjust here

sum(df.t2.cen.shipment_2$actual.Net.freight.cost) # 8044561
sum(df.t2.cen.shipment_2$Current.convert.transcost) # 10334165

# Do the creation of the DC

df.t2.cen.shipment_3 <- df.t2.cen.shipment_2 %>%
  group_by(Dest.State,Dest.City..SAP.,Dest.Code) %>%
  summarise(Vol.weight = sum(actual.Gross.weight.Kg),
         Vol.Pallet = sum(actual.pallet)) %>%
group_by(Dest.State)%>%
mutate(  Max.volume = max(Vol.weight),
         Availabe = ifelse(Vol.weight == Max.volume,1,0 )
         )%>%
ungroup()



df.t2.cen.shipment_3 <- df.t2.cen.shipment_3 %>%
filter((Availabe ==1)) %>%
select(Dest.State,Dest.City..SAP.,Dest.Code)


# Create the group of date.

days_group <- df.t2.cen.shipment_2 %>%
  select(transport.Date) %>%
  distinct(transport.Date) %>%
  pad()

days_group <-
  days_group %>%
  mutate(day.nb = 1:nrow(days_group)) %>%
  mutate(day.group = floor((7-1+day.nb)/7)) %>% #adjust 
  select(transport.Date, day.group)


# Add back date-group to replace the 

df.t2.cen.shipment_2 <- df.t2.cen.shipment_2 %>%
  left_join(days_group, by ="transport.Date")

#create the number of shipment.
df.t2.cen.shipment_2 <- df.t2.cen.shipment_2 %>%
  mutate(drop = rep(1,times=n()) )

# Group the demand by day_group and the state
df.t2.cen.shipment_4 <- df.t2.cen.shipment_2 %>%
  group_by(day.group,Dest.State) %>%
    summarise(Total.shipment = sum(drop),
    vol.Kg = sum(actual.Gross.weight.Kg),
    vol.pallet = sum(actual.pallet),
    vol.trancost= sum(actual.Net.freight.cost),
    vol.trancost1= sum(Current.convert.transcost)
) %>%
ungroup()



# add the origin to the new set
df.t2.cen.shipment_4 <- df.t2.cen.shipment_4 %>%
  mutate(Fiscal = rep(7032,times=n()) )

# Add the destination of Warehouse.

df.t2.cen.shipment_4 <- df.t2.cen.shipment_4 %>%
  left_join(df.t2.cen.shipment_3, by = "Dest.State")

# Add distance GPS
df.t2.cen.shipment_4 <- df.t2.cen.shipment_4 %>%
  rowwise() %>%
  mutate(Distance.GPS = distm (c(df.GPS.site$lng[match(Fiscal,df.GPS.site$`Planta Origem`)], 
                                 df.GPS.site$lat[match(Fiscal,df.GPS.site$`Planta Origem`)]), 
                               c(df.GPS.customer$lng[match(Dest.Code,df.GPS.customer$`SAP Recebedor Mercadoria`)],
                                 df.GPS.customer$lat[match(Dest.Code,df.GPS.customer$`SAP Recebedor Mercadoria`)]
                               ), fun = distHaversine)/1000
  )


# Calculate the transportation cost.
# Check number of Rodotrem

df.t2.cen.shipment_4 <- df.t2.cen.shipment_4 %>%
  mutate(Nu.RoRo = floor(vol.pallet / 48),
         Remaining = vol.pallet - Nu.RoRo *48,
         Lookup.rodo = paste(Fiscal,str_to_upper(Dest.City..SAP.),"BR012",sep="-"),
         Cost.rodo = (df.cost.transportation$TARIFA.FRETE[match( Lookup.rodo ,df.cost.transportation$Look.Up)])*Nu.RoRo
  ) %>%
  
  
  
  
  # Calculate the transportation for consolidation at the state
  mutate(S.Truck.size =          (Remaining>0)*(Remaining<=1.5)*1
         +(Remaining>1.5)*(Remaining<=4.5)*4
         +(Remaining>4.5)*(Remaining<=8.5)*8
         +(Remaining>8.5)*(Remaining<=12.5)*12
         +(Remaining>12.5)*(Remaining<=30)*28
         #(Re.pal.state>0)*(Re.pal.state<=28)*28
         +(Remaining>30)*48) # Assign the suitatble truck for remaining

df.t2.cen.shipment_4 <- df.t2.cen.shipment_4 %>%
  mutate(S.Truck.code= df.trucksize$SAP.Replace.Code[match(S.Truck.size,df.trucksize$Pallets)]) %>%
  mutate(Lookup.other = paste(Fiscal,str_to_upper(Dest.City..SAP.),S.Truck.code,sep="-"),
         cost.other = df.cost.transportation$TARIFA.FRETE[match(Lookup.other,df.cost.transportation$Look.Up)], 
         new.shipment = ifelse(S.Truck.size>0,1,0)
         )

#check error in transport cost 
Check.erro.Rodo = df.t2.cen.shipment_4 %>%
  filter(is.na(Cost.rodo))


write.csv(Check.erro.Rodo, file = "C:/Users/Bao/Desktop/errorRodo.csv", row.names = FALSE)

# Create the summary table

sumtab_T2.centralized <-  df.t2.cen.shipment_4 %>%
  group_by(Dest.State) %>%
  summarise(Old.Shipment = sum(Total.shipment),
            New.Shipment = sum(Nu.RoRo) + sum(new.shipment),
            Old.cost = sum(vol.trancost1),
            New.cost = sum(cost.other) + sum(Cost.rodo),
            cost.diff =  New.cost - Old.cost,
            total.weight = sum(vol.Kg )
  )
sum(sumtab_T2.centralized$cost.diff)
write.csv(df.t2.cen.shipment_4, file = "C:/Users/Bao/Desktop/T2.centralized.at.state.csv", row.names = FALSE)