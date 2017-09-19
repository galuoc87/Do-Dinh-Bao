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


# Summary table per Microrregiao:

Sumtab_cost_microrregiao <- RoRoTrem.Analysis %>%

group_by(First.geography) %>%
summarise(Actual.Freight = sum(actual.Net.freight.cost),
          Current.convert.Freight = sum(Current.trans.cost),
          Consolidated.Freight = sum (S_trans.cost),
          Different.cost = sum(Cost.different)
          ) %>%
arrange(Different.cost)

write.csv(Sumtab_cost_microrregiao, file = "C:/Users/Bao/Desktop/Microrregiao summary.csv", row.names = FALSE)



# # Summary table for consolidation case in Microrregiao

Sumtab_cost_microrregiao <- RoRoTrem.Analysis %>%
  
  group_by(First.geography,Dest.Name) %>%
  summarise(Actual.Freight = sum(actual.Net.freight.cost),
            Current.convert.Freight = sum(Current.trans.cost),
            Consolidated.Freight = sum (S_trans.cost),
            Different.cost = sum(Cost.different),
            Nu.of.Shipment = n()
  ) %>%
  arrange(First.geography,Different.cost)

write.csv(Sumtab_cost_microrregiao, file = "C:/Users/Bao/Desktop/Microrregiao customer list.csv", row.names = FALSE)






# Add the distance from GPS coordinators
# add the customer master with GPS coordinators:

df.GPS.customer<- read_csv("D:\\Goolge Drive\\FOBRA\\02. Project Documents\\04. Data Analysis\\Analysis BAO\\Data\\Master Data\\GPS active clients.csv")
df.GPS.site<- read_csv("D:\\Goolge Drive\\FOBRA\\02. Project Documents\\04. Data Analysis\\Analysis BAO\\Data\\Master Data\\FOBRA_Sites-gmaps_V02_17Aug2017_MAT.csv")


RoRoTrem.Analysis <-RoRoTrem.Analysis %>%
  rowwise() %>%
   mutate(Distance.GPS = distm (c(df.GPS.site$lng[match(Fiscal.Unit,df.GPS.site$`Planta Origem`)], 
                                  df.GPS.site$lat[match(Fiscal.Unit,df.GPS.site$`Planta Origem`)]), 
                               c(df.GPS.customer$lng[match(Dest.Code,df.GPS.customer$`SAP Recebedor Mercadoria`)],
                                 df.GPS.customer$lat[match(Dest.Code,df.GPS.customer$`SAP Recebedor Mercadoria`)]
                                 ), fun = distHaversine)/1000
                             ) %>%
  # Create the filling rate
  mutate (Filling.rate= actual.Gross.weight.Kg*100/df.trucksize$`Capacity.(KG)`[match(Vehicle.SAP.code,df.trucksize$SAP.code)]
          )%>%

  # Add the class of the Shipment distance
mutate(Distance.Type = cut(Distance.GPS/1000, breaks = seq(0, 6, by = 1)
                         )
       )
# Create the Summary chart current weight following by Truck Type.
  ggplot(RoRoTrem.Analysis) + 
    geom_histogram( mapping = aes( x= actual.Gross.weight.Kg, fill = Vehicle.SAP.code))
  
  # Create the Summary chart consolidation following by Truck Type.

  ggplot(RoRoTrem.Analysis) + 
    geom_histogram( mapping = aes( x= Consolidation, fill = S.Truck.code))
  
  
  # Create the Summary chart current shipment  following by distance rank
  ggplot(RoRoTrem.Analysis) + 
    geom_histogram( mapping = aes( x= actual.Gross.weight.Kg, fill = Distance.Type))
  
  # Create the Plot for the distance and Truck Type
  
  
  ggplot(RoRoTrem.Analysis) +
    geom_point(mapping = aes(x = Distance.GPS,y = Filling.rate, color = Vehicle.SAP.code))
  
  # Create the Plot for the distance and Truck Type
  
  
  ggplot(RoRoTrem.Analysis) +
    geom_point(mapping = aes(x = Distance.GPS,y = Filling.rate, color = Carrier.name)) + 
  facet_wrap(~Carrier.name)
  
  
  
  # Summary slide for Rodotrem Analysis.
  
  Sumtab_RoDotrem <- RoRoTrem.Analysis %>%
    mutate (filling.rate =  actual.Gross.weight.Kg / df.trucksize$`Capacity.(KG)`[match(Vehicle.SAP.code,df.trucksize$SAP.code)]) %>%
    group_by(Fiscal.Unit, Vehicle.SAP.code,Vehicle.description) %>%
  summarise(Nu.shipment = n(),
            Total.Shipped.Volume.Ton = sum(actual.Gross.weight.Kg),
            Total.transport.cost.BRL = sum(actual.Net.freight.cost),
            Avg.Filling.rate = round(mean(filling.rate[filling.rate>0]),digits=2)                              
            ) %>%
    mutate(Cost.per.Kg = Total.transport.cost.BRL / Total.Shipped.Volume.Ton  )
  
  write.csv(Sumtab_RoDotrem, file = "C:/Users/Bao/Desktop/Rodotrem summary1.csv", row.names = FALSE,fileEncoding="UTF-8")
  
  # Summary for truck type (bar chart with line chart)
  
  ggplot(Sumtab_RoDotrem)  + 
    geom_bar(aes(x=Vehicle.SAP.code, y=Nu.shipment),stat="identity", fill="pink", colour="sienna3")+
    geom_line(aes(x=Vehicle.SAP.code, y=Avg.Filling.rate*max(Sumtab_RoDotrem$Nu.shipment)),group=1) + 
    geom_text(aes(label=Avg.Filling.rate, x=Vehicle.SAP.code, y=Avg.Filling.rate*max(Sumtab_RoDotrem$Nu.shipment)), colour="black") +
    geom_text(aes(label=Nu.shipment, x=Vehicle.SAP.code, y=Nu.shipment), colour="black", vjust= -0.2) +
    scale_y_continuous(sec.axis = sec_axis(~./max(Sumtab_RoDotrem$Nu.shipment)))
  
  
  # Summary slide for  consolidation Rodotrem Analysis.
  
  Sumtab_RoDotrem_2 <- RoRoTrem.Analysis %>%
    mutate (filling.rate =  Consolidation / (S.Truck.Vol + Nu.RoRo*42000)) %>%
    filter (!is.na(filling.rate))  %>%
        group_by(Fiscal.Unit, S.Truck.code) %>%
    summarise(Nu.shipment.consolidate = n(),
              Total.Shipped.Volume.Ton.A = sum(actual.Gross.weight.Kg),
              Total.transport.cost.BRL.A = sum(actual.Net.freight.cost),
              Avg.Filling.rate.A = round(mean(filling.rate[filling.rate>0], na.rm = TRUE),digits=2)                              
    ) %>%
    mutate(Cost.per.Kg.A = Total.transport.cost.BRL.A / Total.Shipped.Volume.Ton.A  ) %>%
     left_join(Sumtab_RoDotrem, c( "S.Truck.code" = "Vehicle.SAP.code"), fill = 0)  
  Sumtab_RoDotrem_2[is.na(Sumtab_RoDotrem_2)] <- 0
  
  
  write.csv(Sumtab_RoDotrem_2, file = "C:/Users/Bao/Desktop/Rodotrem summary_3.csv", row.names = FALSE,fileEncoding="UTF-8")
  # Create the new table to lookup for chart.
  
  Sumtab_RoDotrem_2_1 <- Sumtab_RoDotrem_2 %>%
    select(Fiscal.Unit.x,S.Truck.code,Nu.shipment,Nu.shipment.consolidate) %>%
  gather(Nu.shipment,Nu.shipment.consolidate, key ="Type", value ="Shipment") %>%
    filter(!(S.Truck.code %in% c("BR300",0,"BR063","BR065")))

  
  
  Sumtab_RoDotrem_2_2 <- Sumtab_RoDotrem_2 %>%
    select(Fiscal.Unit.x,S.Truck.code,Avg.Filling.rate,Avg.Filling.rate.A) %>%
    gather(Avg.Filling.rate,Avg.Filling.rate,Avg.Filling.rate.A, key ="Type", value ="Filling.rate") %>%
  filter(!(S.Truck.code %in% c("BR300",0,"BR063","BR065")))
  Sumtab_RoDotrem_2_2[1,4] <-NA
  
  # Summary for truck type (bar chart with line chart)
  
  ggplot()  + 
    geom_bar(data = Sumtab_RoDotrem_2_1,aes(x=S.Truck.code, y=Shipment,fill=Type),stat="identity", position = position_dodge())   +
    geom_line(data= Sumtab_RoDotrem_2_2, aes(x=S.Truck.code, y=Filling.rate*max(Sumtab_RoDotrem_2_1$Shipment),group=Type,colour = Type ),position = position_dodge()) + 
    geom_text(data = Sumtab_RoDotrem_2_2,aes(label=Filling.rate, x=S.Truck.code, y=Filling.rate*max(Sumtab_RoDotrem_2_1$Shipment)),colour="black") +
    geom_text(data = Sumtab_RoDotrem_2_1,aes(label=Shipment, x=S.Truck.code, y=Shipment,fill=Type),vjust= -0.2, colour="black",position = position_dodge(.9)) +
    scale_y_continuous(sec.axis = sec_axis(~./max(Sumtab_RoDotrem_2_1$Shipment)))
  
  