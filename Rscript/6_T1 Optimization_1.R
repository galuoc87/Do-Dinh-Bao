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

#Create the grouping date by arrange the Data 

T1.Leadtime <- 
  as_tibble(Sample.T1.1.Delivery) %>%
  
  # Choose the record which is below 21 Tons.
  filter(!is.na(Current.trans.cost),
         !actual.pallet <=2)

#[step0] Create the group_by date


   days_group <- T1.Leadtime %>%
  select(transport.Date) %>%
  distinct(transport.Date) %>%
  pad()

days_group <-
  days_group %>%
  mutate(day.nb = 1:nrow(days_group)) %>%
  mutate(day.group = floor((2-1+day.nb)/2)) %>% #adjust 
  select(transport.Date, day.group)
  #
 
# [step0]Link the group by days back to the  T1 by increasing leadtime.
T1.Leadtime <- T1.Leadtime %>%
  left_join(days_group, by ="transport.Date")
# [Step 1]Lookup with customer data frame
T1.Leadtime <- T1.Leadtime %>%
  rowwise() %>%
  mutate(Distance.GPS = distm (c(df.GPS.site$lng[match(Fiscal.Unit,df.GPS.site$`Planta Origem`)], 
                                 df.GPS.site$lat[match(Fiscal.Unit,df.GPS.site$`Planta Origem`)]), 
                               c(df.GPS.customer$lng[match(Dest.Code,df.GPS.customer$`SAP Recebedor Mercadoria`)],
                                 df.GPS.customer$lat[match(Dest.Code,df.GPS.customer$`SAP Recebedor Mercadoria`)]
                               ), fun = distHaversine)/1000
  ) %>%
  
  mutate(First.geography=df.customer$Microrregiao[match(str_to_upper(Dest.City..SAP.),df.customer$`Cidade Recebedor Mercadoria`)]) %>%
  
  mutate(Second.geography=df.customer$Messorregiao[match(str_to_upper(Dest.City..SAP.),df.customer$`Cidade Recebedor Mercadoria`)])

# [Step 1] add the filling rate to the table:

T1.Leadtime <- T1.Leadtime %>%
  group_by(Shipment.Document) %>%
  mutate (Totalvolume = sum(actual.Gross.weight.Kg),
          Rankweight =row_number()) %>%
  mutate (cor.vol = ifelse(Rankweight ==1 ,Totalvolume,0 )) %>%
  mutate (filling.rate = ifelse(Rankweight ==1 ,( Totalvolume / df.trucksize$`Capacity.(KG)`[match(Vehicle.SAP.code,df.trucksize$SAP.code)]*100),0 )) %>%
  ungroup() %>%
  # [Step 1]Start to arrange the table by Date, 
  arrange(transaction.Date,Distance.GPS) %>%
  
  # [Step 1]Consolidate the quantity per customer
  group_by(day.group,Fiscal.Unit,Dest.City..SAP.,Dest.State,First.geography,Second.geography,Dest.Code) %>%
  summarise(current.frei.cost = sum(Current.trans.cost),
            Distance.gps = mean(Distance.GPS),
            Total.pallet = sum(actual.pallet),
            Total.weight= sum(actual.Gross.weight.Kg),
            filling.rate = mean(filling.rate[filling.rate>0]),
            Nu.shipment = sum (Rankweight[Rankweight==1])
  ) %>%
  ungroup()


#[Step 1]arrage by date, distance, and frirst geography
T1.Leadtime <- T1.Leadtime %>%
  arrange(day.group,Distance.gps,First.geography)


# [Step 1]Check number of Rodotrem truck daily:
T1.Leadtime <- T1.Leadtime %>%
  mutate(Nu.RoRo.cust = floor(Total.pallet / 48),
         Re.pal.cust = Total.pallet - Nu.RoRo.cust *48,
         Lookup.cust = paste(Fiscal.Unit,str_to_upper(Dest.City..SAP.),"BR012",sep="-"),
         Cost.cust = (df.cost.transportation$TARIFA.FRETE[match(Lookup.cust,df.cost.transportation$Look.Up)])*Nu.RoRo.cust
  ) %>%
  
  # [Step 1]Calculate the transportation for consolidation at the state
  mutate(S.Truck.size.cust =          (Re.pal.cust>0)*(Re.pal.cust<=1.5)*1
         +(Re.pal.cust>1.5)*(Re.pal.cust<=4.5)*4
         +(Re.pal.cust>4.5)*(Re.pal.cust<=8.5)*8
         +(Re.pal.cust>8.5)*(Re.pal.cust<=12.5)*12
         +(Re.pal.cust>12.5)*(Re.pal.cust<=30)*28
         #(Re.pal.state>0)*(Re.pal.state<=28)*28
         +(Re.pal.cust>30)*48) # Assign the suitatble truck for remaining

T1.Leadtime <- T1.Leadtime %>%
  mutate(S.Truck.code.cust= df.trucksize$SAP.Replace.Code[match(S.Truck.size.cust,df.trucksize$Pallets)]) %>%
  mutate(Lookup.cust.other = paste(Fiscal.Unit,str_to_upper(Dest.City..SAP.),S.Truck.code.cust,sep="-"),
         Other.cost.cust = df.cost.transportation$TARIFA.FRETE[match(Lookup.cust.other,df.cost.transportation$Look.Up)])

# [Step 1]Calculate the number of shipment and cost saving
T1.Leadtime <- T1.Leadtime %>%
  mutate(shipment.cust = ifelse (Re.pal.cust > 0,1,0),
         Shipment.cust.rodo = ifelse (S.Truck.code.cust == "BR012",1,0),
         cost.diff.cust = Other.cost.cust + Cost.cust-current.frei.cost
  )

# [Step 1]sumamry table

Sumtab_RoDotrem_4 <- T1.Leadtime %>%
  group_by(Dest.State,Dest.Code) %>%
  summarise(Current.cost = sum(current.frei.cost),
            Current.shipment = sum (Nu.shipment),
            Total.weight.tons = sum (Total.weight)/1000,
            Filling.rate = mean(filling.rate[filling.rate>0])/100,
            New.cost = sum(Other.cost.cust)+sum(Cost.cust),
            Total.Rodo.shipment =
              sum(Nu.RoRo.cust),
            Other.shipment = sum(shipment.cust),
            #new.filling.rate = mean (Total.Weight)/((mean(Total.Rodo.micro) + 
            #                                         sum(Nu.RoRo.state)+
            #                                        sum(Nu.RoRo.final))*42000+sum(final.weight)),
            #final.vol.tons = sum(final.weight)/1000,
            Cost.diff = New.cost - Current.cost,
            Distance = mean(Distance.gps)
            #Total.volumne = Total.Rodo.shipment*42+final.vol.tons
  )%>%
  ungroup()%>%
  arrange(Dest.State,Distance)
setwd("C:/Users/Do Dinh Bao/Desktop/") #home

setwd("C:/Users/Bao/Desktop/") #company


write.csv(Sumtab_RoDotrem_4, file = "Rodotrem.customer.csv", row.names = FALSE)


# Step 2: Filter Out the customer with expensive transportation cost (cost dif >10,000 BRL yearly)
T1.Leadtime_s2<-T1.Leadtime %>%
  filter(
    !Dest.Code %in% c(723332,723325,3202973, 915409,1933857)
  )

# [Step2]-create indication at the Microrregiao level:

  T1.Leadtime_s2<-T1.Leadtime_s2 %>%
  group_by(day.group,First.geography) %>%
  arrange(day.group,First.geography,desc(Re.pal.cust),Distance.gps) %>%
  mutate(Rank.Date=row_number()) %>%
  mutate(Max.Date=max(Rank.Date)) %>%
  mutate(Cumpallet=cumsum(Re.pal.cust)) %>%
  ungroup()
  
  
  # [Step2]consolidation volume at microressal level
  
  T1.Leadtime_s2<-T1.Leadtime_s2 %>%
    group_by(day.group) %>%
    mutate(Consol.micro =ifelse(Max.Date==1 | Rank.Date==2, Cumpallet,
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
  
  
  # [Step2]Check number of Rodotrem for case of consolidation in Microrregiao level:
  
  
  T1.Leadtime_s2<-T1.Leadtime_s2 %>%
    mutate(Nu.RoRo.micro = ifelse(Consol.micro> 50*0.85,1,floor(Consol.micro / 50)),
           Re.pal.micro = ifelse((Consol.micro - Nu.RoRo.micro *50) <0,0,Consol.micro - Nu.RoRo.micro *50),
           Lookup.micro = paste(Fiscal.Unit,str_to_upper(Dest.City..SAP.),"BR012",sep="-"),
           Cost.micro = (df.cost.transportation$TARIFA.FRETE[match(Lookup.micro,df.cost.transportation$Look.Up)])*Nu.RoRo.micro
    ) %>%
    
    
    # [Step 2]Calculate the transportation for consolidation at the state
    mutate(S.Truck.size.micro = ifelse( Re.pal.micro ==0 ,0,
                                        (Re.pal.micro>0)*(Re.pal.micro<=1.5)*1
                                        +(Re.pal.micro>1.5)*(Re.pal.micro<=4.5)*4
                                        +(Re.pal.micro>4.5)*(Re.pal.micro<=8.5)*8
                                        +(Re.pal.micro>8.5)*(Re.pal.micro<=12.5)*12
                                        +(Re.pal.micro>12.5)*(Re.pal.micro<=30)*28
                                        #(Re.pal.state>0)*(Re.pal.state<=28)*28
                                        +(Re.pal.micro>30)*48 # Assign the suitatble truck for remaining
    )) 
  
  T1.Leadtime_s2<-T1.Leadtime_s2 %>%
    mutate(S.Truck.code.micro= ifelse(Re.pal.micro ==0 ,0,
                                      df.trucksize$SAP.Replace.Code[match(S.Truck.size.micro,df.trucksize$Pallets)])) %>%
    mutate(Lookup.micro.other = paste(Fiscal.Unit,str_to_upper(Dest.City..SAP.),S.Truck.code.micro,sep="-"),
           Other.cost.micro = ifelse(Re.pal.micro ==0 ,0,
                                     df.cost.transportation$TARIFA.FRETE[match(Lookup.micro.other,df.cost.transportation$Look.Up)]))
  
  
  
  
  # [Step 2]Calculate the number of shipment and cost saving
  T1.Leadtime_s2<-T1.Leadtime_s2 %>%
    mutate(shipment.micro = ifelse (Re.pal.micro > 0,1,0),
           Shipment.micro.rodo = ifelse (S.Truck.code.micro == "BR012",1,0),
           cost.diff.micro = Other.cost.micro + Cost.cust+Cost.micro-current.frei.cost
    )
  
  #[Step2] Summary table
  
  Sumtab_RoDotrem_5 <-T1.Leadtime_s2 %>%
    group_by(Dest.State,First.geography) %>%
    summarise(Current.cost = sum(current.frei.cost),
              Current.shipment = sum (Nu.shipment),
              Total.weight.tons = sum (Total.weight)/1000,
              Filling.rate = mean(filling.rate)/100,
              New.cost = sum(Other.cost.micro)+sum(Cost.cust)+sum(Cost.micro),
              Total.Rodo.shipment =
                sum(Nu.RoRo.micro)+
                sum(Nu.RoRo.cust),
              Other.shipment = sum(shipment.micro),
              #new.filling.rate = mean (Total.Weight)/((mean(Total.Rodo.micro) + 
              #                                         sum(Nu.RoRo.state)+
              #                                        sum(Nu.RoRo.final))*42000+sum(final.weight)),
              #final.vol.tons = sum(final.weight)/1000,
              Cost.diff = New.cost - Current.cost,
              Distance = mean(Distance.gps)
              #Total.volumne = Total.Rodo.shipment*42+final.vol.tons
    )%>%
    ungroup()%>%
    arrange(Dest.State,Distance)
  #Total.volumne = Total.Rodo.shipment*42+final.vol.tons
  
  
  write.csv(Sumtab_RoDotrem_5, file = "Rodotrem.city.csv", row.names = FALSE)
  
  
  setwd("C:/Users/Do Dinh Bao/Desktop/") #home
  
  setwd("C:/Users/Bao/Desktop/") #company
  
  
  #[Step3] filter the microrregião with high cost (>6,000 BRL 1 y)- Teresina, Porto Seguro , Campo Grande , Fortaleza 
  
  
  T1.Leadtime_s3 <-  T1.Leadtime_s2 %>%
    filter(
      !First.geography %in% c("Teresina") #, "Porto Seguro" , "Campo Grande" , "Fortaleza")
    )
  #[Step3] Create the new data frame for consolidation in State
  
  T1.Leadtime_s3 <- T1.Leadtime_s3 %>%
    group_by(day.group,Fiscal.Unit,Dest.City..SAP.,Dest.State) %>%
    summarise(current.frei.cost = sum(current.frei.cost),
              Distance.gps = mean(Distance.gps),
              Total.pallet = sum(Re.pal.micro),
              Cost.cust = sum (Cost.cust),
              Cost.micro = sum(Cost.micro),
              Nu.Rodo.Micro = sum(Nu.RoRo.cust)+ sum(Nu.RoRo.micro),
              Filling.rate = mean(filling.rate),
              Nu.Shipment = sum(Nu.shipment),
              Weight = sum(Total.weight)
    )%>%
    ungroup() %>%
    #[Step3] Add all the cost at the state level
    group_by(Dest.State)%>%
    mutate(Total.cost.cust = sum (Cost.cust),
           Total.cost.micro = sum (Cost.micro),
           Total.current = sum (current.frei.cost),
           Total.Rodo.micro = sum(Nu.Rodo.Micro),
           Total.cu.shipment = sum(Nu.Shipment),
           Total.Weight = sum(Weight),
           Total.filling= mean(Filling.rate)
    ) %>%
    ungroup()
  #[Step3] add the group of the destination can go together:
  
  T1.Leadtime_s3 <- T1.Leadtime_s3 %>%
    left_join(df.destination, by = "Dest.State")
  
  
  
  #[step4] to complaet sept4, run here
  #[Step3]create the indicators   
  T1.Leadtime_s3 <- T1.Leadtime_s3 %>%
    arrange(day.group,Dest.State,Distance.gps) %>%
    group_by(day.group,Dest.State) %>%# Change the Des.state or replace des.state
    filter(Total.pallet>0) %>% # remove the record with 0 value
    mutate(Rank.Date=row_number(Distance.gps)) %>%
    mutate(Max.Date=max(Rank.Date)) %>%
    mutate(Cumpallet=cumsum(Total.pallet)) %>% # add cum pallet
    ungroup()
  
  
  #[Step 3]consolidation at the state level 
  
  T1.Leadtime_s3 <- T1.Leadtime_s3 %>%
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
  
  # [Step 3]number of Rodotrem for case of consolidation in state level:
  
  
  T1.Leadtime_s3 <- T1.Leadtime_s3 %>%
    mutate(Nu.RoRo.state = ifelse(Consol.state > 50*0.85,1,floor(Consol.state / 50)),
           Consolidation.case = ifelse((Rank.Date %% 2 )==0 ,1,0))
  
  T1.Leadtime_s3 <- T1.Leadtime_s3 %>%
    mutate( Re.pal.state = ifelse((Consol.state - Nu.RoRo.state *50) <0,0,Consol.state - Nu.RoRo.state *50),
            Final.remaing.qty = ifelse( Consolidation.case == 1 & Consol.state <50 ,0, Re.pal.state)
    ) %>%
    
    #[step3] Calculate the cost for Rodotrem at state
    mutate(Lookup.state = paste(Fiscal.Unit,str_to_upper(Dest.City..SAP.),"BR012",sep="-"),
           Cost.state = (df.cost.transportation$TARIFA.FRETE[match(Lookup.state,df.cost.transportation$Look.Up)])*Nu.RoRo.state
    ) %>%
    
    
    # [Step 3]Calculate other  transportation for remaining qty at the state
    mutate(S.Truck.size.state = ifelse( Re.pal.state ==0 ,0,
                                        (Re.pal.state>0)*(Re.pal.state<=1.5)*1
                                        +(Re.pal.state>1.5)*(Re.pal.state<=4.5)*4
                                        +(Re.pal.state>4.5)*(Re.pal.state<=8.5)*8
                                        +(Re.pal.state>8.5)*(Re.pal.state<=12.5)*12
                                        +(Re.pal.state>12.5)*(Re.pal.state<=30)*28
                                        #(Re.pal.state>0)*(Re.pal.state<=28)*28
                                        +(Re.pal.state>30)*48 # Assign the suitatble truck for remaining
    )) 
  
  T1.Leadtime_s3 <- T1.Leadtime_s3 %>%
    mutate(S.Truck.code.state= ifelse(Re.pal.state ==0 ,0,
                                      df.trucksize$SAP.Replace.Code[match(S.Truck.size.state,df.trucksize$Pallets)])) %>%
    mutate(Lookup.state.other = paste(Fiscal.Unit,str_to_upper(Dest.City..SAP.),S.Truck.code.state,sep="-"),
           Other.cost.state = ifelse(Re.pal.state ==0 ,0,
                                     df.cost.transportation$TARIFA.FRETE[match(Lookup.state.other,df.cost.transportation$Look.Up)]
           )
    )

  
  
  # [Step 3]Calculate the number of shipment and cost saving
  T1.Leadtime_s3 <- T1.Leadtime_s3 %>%
    mutate(shipment.state = ifelse (Re.pal.state > 0,1,0),
           Shipment.state.rodo = ifelse (S.Truck.code.state == "BR012",1,0)
    )
  
  # [Step 3]Summary table to calculate 
  
  Sumtab_RoDotrem_7 <- T1.Leadtime_s3 %>%
    
    group_by(Dest.State) %>%
    summarise(Current.cost= mean(Total.current),
              Current.shipment = mean (Total.cu.shipment),
              Total.weight.tons = mean (Total.Weight)/1000,
              Filling.rate = mean(Total.filling)/100,
              New.cost = mean(Total.cost.micro) + 
                mean(Total.cost.cust)+
                sum(Cost.state)+
                sum(Other.cost.state),
              Total.Rodo.shipment = mean(Total.Rodo.micro) + 
                sum(Nu.RoRo.state),
              Other.shipment = sum(shipment.state),
              Cost.diff = New.cost - Current.cost,
              Distance = mean(Distance.gps)
    )%>%
    ungroup()%>%
    arrange(Distance)
  
  sum(Sumtab_RoDotrem_3$Cost.diff)
  sum(Sumtab_RoDotrem_3$Nu.Roro.Shipment)
  sum(Sumtab_RoDotrem_3$Other.shipments)
  
  
  setwd("C:/Users/Do Dinh Bao/Desktop/") #home
  
  setwd("C:/Users/Bao/Desktop/") #company
  
  
  write.csv(Sumtab_RoDotrem_7, file = "Rodotrem.state.csv", row.names = FALSE)
  
  