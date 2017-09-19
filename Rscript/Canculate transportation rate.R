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



# Calculate transportation rate per route from historical data
# Following the structure
# Origin Code (1208) 
#-> Desination City (ARARAS in capital) 
#-> Vehicle Type SAP (BR275) 

df.cost.transportation2 <- df.success.transportation %>%
mutate(Look.Up = str_c(Fiscal.Unit,str_to_upper(Dest.City..SAP.),Vehicle.SAP.code,sep = "-")) %>%
rowwise() %>%
  mutate(Distance.GPS = distm (c(df.GPS.site$lng[match(Fiscal.Unit,df.GPS.site$`Planta Origem`)], 
                                           df.GPS.site$lat[match(Fiscal.Unit,df.GPS.site$`Planta Origem`)]), 
                                         c(df.GPS.customer$lng[match(Dest.Code,df.GPS.customer$`SAP Recebedor Mercadoria`)],
                                           df.GPS.customer$lat[match(Dest.Code,df.GPS.customer$`SAP Recebedor Mercadoria`)]
                                         ), fun = distHaversine)/1000
            ) %>%
  
filter(!is.na(Look.Up))
  # Consolidate in to 1 cost
  df.cost.transportation2 <- df.cost.transportation2 %>%
  group_by(Shipment.Document)%>%
  arrange(Shipment.Document,desc(Distance.GPS)) %>%
  mutate(Totalfrieghtcost = sum(actual.Net.freight.cost),
         Occurance = n(),
         Rank = row_number()
         ) %>%
    mutate (cor.Freight.cost = ifelse(Rank ==1 ,Totalfrieghtcost,0 ))

  # Remove the outlier
  df.cost.transportation2 <- df.cost.transportation2 %>%
    select(Shipment.Document,Look.Up,cor.Freight.cost, Distance.GPS) %>%
    filter(cor.Freight.cost > 0) %>%
  group_by(Look.Up) %>%
  mutate(Q1 =quantile(cor.Freight.cost, probs=0.25 ),
            Median =quantile(cor.Freight.cost, probs=0.5 ),
            Q3 =quantile(cor.Freight.cost, probs=0.75),
            avg=mean(cor.Freight.cost),
            n=n()
  ) %>%
  mutate ( Lower.limits = Q1- (Q3 - Q1)*1.5, 
           Upper.limits= (Q3 - Q1)*1.5+Q3
           ) %>%
    mutate (Rev =  ifelse((cor.Freight.cost > Upper.limits | cor.Freight.cost <  Lower.limits),1,0 )) %>%
  filter(Rev == 0) %>%
    group_by(Look.Up)%>%
  summarise(TARIFA.FRETE = quantile(cor.Freight.cost, probs=0.5 ),
            GPS.distance = mean(Distance.GPS))
  
  write.csv(df.cost.transportation2, file = "C:/Users/Bao/Desktop/RouteCost.csv", row.names = FALSE)