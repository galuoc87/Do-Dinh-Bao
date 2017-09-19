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
  select(Dest.Code,Group,Check.avai)

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
          consol.trancost= ifelse(Rank ==Occurance ,Totalvol.trancost,0 ),
          Cost.of.first.leg = ifelse( Fiscal.Unit == 7032 | Fiscal.Unit == 7264,0,
                                      df.cost.kg.1st.leg$`Cost per Kg`[match(Dest.State,df.cost.kg.1st.leg$state)]*actual.Gross.weight.Kg)
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
df.t2.cen.shipment_2 <- df.t2.cen.shipment
  #filter((Totalvol.pallet > 0.8)) # will adjust here

sum(df.t2.cen.shipment_2$actual.Net.freight.cost) # 8044561
sum(df.t2.cen.shipment_2$Current.convert.transcost) # 10334165

# Do the creation of the DC

df.t2.cen.shipment_3 <- df.t2.cen.shipment_2 %>%
  group_by(Dest.State,Dest.City..SAP.,Group,Dest.Code) %>%
  summarise(Vol.weight = sum(actual.Gross.weight.Kg),
            Vol.Pallet = sum(actual.pallet)) %>%
  group_by(Dest.State,Group)%>%
  mutate(  Max.volume = max(Vol.weight),
           Availabe = ifelse(Vol.weight == Max.volume,1,0 )
  )%>%
  ungroup()



df.t2.cen.shipment_3 <- df.t2.cen.shipment_3 %>%
  filter((Availabe ==1)) %>%
  select(Dest.State,Dest.City..SAP.,Group,Dest.Code)


# Create the group of date.

days_group <- df.t2.cen.shipment_2 %>%
  select(transport.Date) %>%
  distinct(transport.Date) %>%
  pad()

days_group <-
  days_group %>%
  mutate(day.nb = 1:nrow(days_group)) %>%
  mutate(day.group = floor((14-1+day.nb)/14)) %>% #adjust 
  select(transport.Date, day.group)


# Add back date-group to replace the 

df.t2.cen.shipment_2 <- df.t2.cen.shipment_2 %>%
  left_join(days_group, by ="transport.Date")

#create the number of shipment.
df.t2.cen.shipment_2 <- df.t2.cen.shipment_2 %>%
  mutate(drop = rep(1,times=n()) )

# Group the demand by day_group and the state
df.t2.cen.shipment_4 <- df.t2.cen.shipment_2 %>%
  group_by(day.group,Dest.State,Group) %>%
  summarise(Total.shipment = sum(drop),
            vol.Kg = sum(actual.Gross.weight.Kg),
            vol.pallet = sum(actual.pallet),
            vol.trancost= sum(actual.Net.freight.cost),
            vol.trancost1= sum(Current.convert.transcost),
            Cost.1st.leg = sum(Cost.of.first.leg)
  ) %>%
  ungroup()





# add the origin to the new set
df.t2.cen.shipment_4 <- df.t2.cen.shipment_4 %>%
  mutate(Fiscal = rep(7032,times=n()) )

# Add the destination of Warehouse.

df.t2.cen.shipment_4 <- df.t2.cen.shipment_4 %>%
  left_join(df.t2.cen.shipment_3, by = c("Dest.State","Group"))

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
  mutate(S.Truck.size =          (Remaining>0)*(Remaining<=1)*1
         +(Remaining>1)*(Remaining<=4)*4
         +(Remaining>4)*(Remaining<=8)*8
         +(Remaining>8)*(Remaining<=12)*12
         +(Remaining>12)*(Remaining<=28)*28
         #(Re.pal.state>0)*(Re.pal.state<=28)*28
         +(Remaining>28)*48) # Assign the suitatble truck for remaining




df.t2.cen.shipment_4 <- df.t2.cen.shipment_4 %>%
  mutate(S.Truck.code= df.trucksize$SAP.Replace.Code[match(S.Truck.size,df.trucksize$Pallets)]) %>%
  mutate(Lookup.other = paste(Fiscal,str_to_upper(Dest.City..SAP.),S.Truck.code,sep="-"),
         cost.other = df.cost.transportation$TARIFA.FRETE[match(Lookup.other,df.cost.transportation$Look.Up)], 
         new.shipment = ifelse(S.Truck.size>0,1,0),
         Filling.rate.Kg = vol.Kg/(df.trucksize$`Capacity.(KG)`[match(S.Truck.code,df.trucksize$SAP.Replace.Code)]+Nu.RoRo*42000),
         Filling.rate.Pallet = vol.pallet/(S.Truck.size+48*Nu.RoRo)
  )

#check error in transport cost 
Check.erro.Rodo = df.t2.cen.shipment_4 %>%
  filter(is.na(Cost.rodo))
write.csv(Check.erro.Rodo, file = "errorRodo1.csv", row.names = FALSE)



Check.erro.Rodo = df.t2.cen.shipment_4 %>%
  filter(is.na(cost.other))
write.csv(Check.erro.Rodo, file = "errorRodo2.csv", row.names = FALSE)

# Add the cost for the first leg

setwd("C:/Users/Do Dinh Bao/Desktop/") #home

setwd("C:/Users/Bao/Desktop/") #company



df.cost.kg.1st.leg<- read_csv("Cost.kg.1st.leg.csv") 

# Add the cost for the first leg

#df.t2.cen.shipment_4 <- df.t2.cen.shipment_4 %>%
 #mutate(Cosf.1st.leg = df.cost.kg.1st.leg$`Cost per Kg`[match(Dest.State,df.cost.kg.1st.leg$state)]*vol.Kg
 #)

# Create the summary table

sumtab_T2.centralized <-  df.t2.cen.shipment_4 %>%
  group_by(Group,Dest.State) %>%
  summarise(Old.Shipment = sum(Total.shipment),
            New.Shipment = sum(Nu.RoRo) + sum(new.shipment),
            Old.cost = sum(vol.trancost),
            New.cost = sum(cost.other) + sum(Cost.rodo),
            Cost.firstleg= sum(Cost.1st.leg),
            cost.diff.without.1st =  New.cost - Old.cost,
            cost.diff.with.1st = New.cost - Old.cost - Cost.firstleg,
            total.weight = sum(vol.Kg ),
            fill.rate.kg = mean(Filling.rate.Kg),
            fill.rate.pallet = mean(Filling.rate.Pallet),
            Distance = mean(Distance.GPS),
            Avaliable = ifelse(cost.diff.without.1st >0,1,0)
  )

write_csv (sumtab_T2.centralized, "t2.centralized.summary.14day.csv" )
sum(sumtab_T2.centralized$cost.diff)
write.csv(df.t2.cen.shipment_4, file = "T2.centralized.at.state.14day.csv", row.names = FALSE)

write_csv (df.t2.cen.shipment, "t2.centralized.basedline.csv" )


# Create th look_kup table to filter at master data.

sumtab_T2.centralized_4 <- sumtab_T2.centralized %>%
  select(Group,Dest.State,Avaliable)




# Create the Truck Profile.
sumtab_T2.centralized <-  df.t2.cen.shipment_4 %>%
  group_by(Group,Dest.State,S.Truck.code) %>%
  summarise(
            Shipment = n()
  )
#create shipment profile

  sumtab_T2.centralized_2 <- spread( sumtab_T2.centralized , key = S.Truck.code , value = Shipment )
  sumtab_T2.centralized_2[is.na(sumtab_T2.centralized_2)] <-0
  
  
  sumtab_T2.centralized_3 <- df.t2.cen.shipment_4 %>%
    group_by(Group,Dest.State) %>%
    summarise(
      BR012.1 = sum(Nu.RoRo)
)
  
  
  sumtab_T2.centralized_2 <- sumtab_T2.centralized_2 %>%
    left_join(sumtab_T2.centralized_3 )
  sumtab_T2.centralized_2[is.na(sumtab_T2.centralized_2)] <-0
  
  
  sumtab_T2.centralized_2 <- sumtab_T2.centralized_2 %>%
    mutate(BR012.2 =BR012+BR012.1)%>%
    select(Group,Dest.State,BR300,BR063,BR065,BR020,BR024,BR012.2)

  write.csv(sumtab_T2.centralized_2, file = "C:/Users/Bao/Desktop/Truck-profile.csv", row.names = FALSE)



















# [Step]Filter and keep only the possitive value
df.t2.cen.shipment_4 <- df.t2.cen.shipment_4 %>%
  group_by(Group,Dest.State) %>%
  mutate( Old.cost = sum(vol.trancost1),
             New.cost = sum(cost.other) + sum(Cost.rodo),
             cost.diff =  New.cost - Old.cost
  )

df.t2.cen.shipment_5 <- df.t2.cen.shipment_4 %>%
  filter(cost.diff>0)

#[Step]add the state which can consolidatied

df.t2.cen.shipment_5<- df.t2.cen.shipment_5 %>%
  left_join(df.destination, by = "Dest.State")

# Create the consolidation case.
df.t2.cen.shipment_5<- df.t2.cen.shipment_5 %>%
  arrange(day.group,Group,Replace.Dest.State,Distance.GPS) %>%
  group_by(day.group,Group,Replace.Dest.State) %>%
  mutate(Rank.Date=row_number(Distance.GPS)) %>%
  mutate(Max.Date=max(Rank.Date)) %>%
  mutate(Cumpallet=cumsum(Remaining)) %>% # add cum pallet
  ungroup()


# Consolidated volume


df.t2.cen.shipment_5<- df.t2.cen.shipment_5 %>%
group_by(day.group) %>%
  mutate(Consol.state =ifelse(Max.Date==1 | Rank.Date==2, Cumpallet,
                              ifelse((Rank.Date %% 2 )==0,
                                     Cumpallet - lag(Cumpallet, n=2L, default = 0), 
                                     ifelse(
                                       Rank.Date==Max.Date, 
                                       Cumpallet - lag(Cumpallet, n=1L, default = 0), 
                                       0
                                     )
                              ) 
  )
  ) %>%
  ungroup()

df.t2.cen.shipment_5<- df.t2.cen.shipment_5 %>%
  mutate(Nu.RoRo.state = floor(Consol.state / 50),
Re.pal.state = Consol.state - Nu.RoRo.state *50
)

df.t2.cen.shipment_5<- df.t2.cen.shipment_5 %>%
  mutate(Lookup.state = paste(Fiscal,str_to_upper(Dest.City..SAP.),"BR012",sep="-"),
         Cost.state = (df.cost.transportation$TARIFA.FRETE[match(Lookup.state,df.cost.transportation$Look.Up)])*Nu.RoRo.state
  ) %>%
  
  
  # [Step]Calculate other  transportation for remaining qty at the state
  mutate(S.Truck.size.state = ifelse( Re.pal.state ==0 ,0,
                                      (Re.pal.state>0)*(Re.pal.state<=1.5)*1
                                      +(Re.pal.state>1.5)*(Re.pal.state<=4.5)*4
                                      +(Re.pal.state>4.5)*(Re.pal.state<=8.5)*8
                                      +(Re.pal.state>8.5)*(Re.pal.state<=12.5)*12
                                      +(Re.pal.state>12.5)*(Re.pal.state<=30)*28
                                      #(Re.pal.state>0)*(Re.pal.state<=28)*28
                                      +(Re.pal.state>30)*48 # Assign the suitatble truck for remaining
  )) 

df.t2.cen.shipment_5<- df.t2.cen.shipment_5 %>%
  mutate(S.Truck.code.state= ifelse(Re.pal.state ==0 ,0,
                                    df.trucksize$SAP.Replace.Code[match(S.Truck.size.state,df.trucksize$Pallets)])) %>%
  mutate(Lookup.state.other = paste(Fiscal,str_to_upper(Dest.City..SAP.),S.Truck.code.state,sep="-"),
         Other.cost.state = ifelse(Re.pal.state ==0 ,0,
                                   df.cost.transportation$TARIFA.FRETE[match(Lookup.state.other,df.cost.transportation$Look.Up)]
         )
  )



# Summary table for the consolidation

sumtab_T2.centralized_4 <-  df.t2.cen.shipment_5 %>%
  group_by(Group,Replace.Dest.State) %>%
  summarise(Old.Shipment = sum(Total.shipment),
            New.Shipment = sum(Nu.RoRo) + sum(new.shipment),
            Old.cost = sum(vol.trancost1),
            New.cost = sum(Cost.rodo)
            +sum(Cost.state)
            +sum(Other.cost.state),
            cost.diff =  New.cost - Old.cost,
            total.weight = sum(vol.Kg ),
            Distance = mean(Distance.GPS)
  )

sum(sumtab_T2.centralized_4$cost.diff)






# Second appoarch for the negative saving, we increase 3 days leadtimes.

df.t2.cen.shipment_6 <- df.t2.cen.shipment_2 %>%
  left_join(sumtab_T2.centralized)

df.t2.cen.shipment_6 <- df.t2.cen.shipment_6 %>%
  filter(Avaliable == 1)
# add Date group 

days_group <- df.t2.cen.shipment_6 %>%
  select(transport.Date) %>%
  distinct(transport.Date) %>%
  pad()

days_group <-
  days_group %>%
  mutate(day.nb = 1:nrow(days_group)) %>%
  mutate(day.group = floor((14-1+day.nb)/14)) %>% #adjust 
  select(transport.Date, day.group)

# Add Date group back to new data frame
df.t2.cen.shipment_6 <- df.t2.cen.shipment_6 %>%
  left_join(days_group, by ="transport.Date")

# group the volume by day.group 14 days


df.t2.cen.shipment_6 <- df.t2.cen.shipment_6 %>%
  group_by(day.group,Dest.State,Group) %>%
  summarise(Total.shipment = sum(drop),
            vol.Kg = sum(actual.Gross.weight.Kg),
            vol.pallet = sum(actual.pallet),
            vol.trancost= sum(actual.Net.freight.cost),
            vol.trancost1= sum(Current.convert.transcost)
  ) %>%
  ungroup()


# add the origin to the new set
df.t2.cen.shipment_6 <- df.t2.cen.shipment_6 %>%
  mutate(Fiscal = rep(7032,times=n()) )

# Add the destination of Warehouse.

df.t2.cen.shipment_6 <- df.t2.cen.shipment_6 %>%
  left_join(df.t2.cen.shipment_3, by = c("Dest.State","Group"))

# Add distance GPS
df.t2.cen.shipment_6 <- df.t2.cen.shipment_6 %>%
  rowwise() %>%
  mutate(Distance.GPS = distm (c(df.GPS.site$lng[match(Fiscal,df.GPS.site$`Planta Origem`)], 
                                 df.GPS.site$lat[match(Fiscal,df.GPS.site$`Planta Origem`)]), 
                               c(df.GPS.customer$lng[match(Dest.Code,df.GPS.customer$`SAP Recebedor Mercadoria`)],
                                 df.GPS.customer$lat[match(Dest.Code,df.GPS.customer$`SAP Recebedor Mercadoria`)]
                               ), fun = distHaversine)/1000
  )


# Calculate the transportation cost.
# Check number of Rodotrem

df.t2.cen.shipment_6 <- df.t2.cen.shipment_6 %>%
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

df.t2.cen.shipment_6 <- df.t2.cen.shipment_6 %>%
  mutate(S.Truck.code= df.trucksize$SAP.Replace.Code[match(S.Truck.size,df.trucksize$Pallets)]) %>%
  mutate(Lookup.other = paste(Fiscal,str_to_upper(Dest.City..SAP.),S.Truck.code,sep="-"),
         cost.other = df.cost.transportation$TARIFA.FRETE[match(Lookup.other,df.cost.transportation$Look.Up)], 
         new.shipment = ifelse(S.Truck.size>0,1,0)
  )

# Create the summary table

sumtab_T2.centralized_5 <-  df.t2.cen.shipment_6 %>%
  group_by(Group,Dest.State) %>%
  summarise(Old.Shipment = sum(Total.shipment),
            New.Shipment = sum(Nu.RoRo) + sum(new.shipment),
            Old.cost = sum(vol.trancost1),
            New.cost = sum(cost.other) + sum(Cost.rodo),
            cost.diff =  New.cost - Old.cost,
            total.weight = sum(vol.Kg ),
            Distance = mean(Distance.GPS),
            Avaliable = ifelse(cost.diff >0,1,0)
  )

write_csv (df.t2.cen.shipment_6, "C:/Users/Bao/Desktop/t2.centralized.detail. 14day.lt.csv" )

write_csv (sumtab_T2.centralized_5, "C:/Users/Bao/Desktop/t2.centralized.summary.for 14day.lt.csv" )