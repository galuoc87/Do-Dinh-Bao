# Try to install package tidyverse

install.packages("tidyverse")

# Call functions in "tivyverse" every day

library(tidyverse)

#Add library: stringr

library(stringr)

#Add lubridate
library(lubridate)

# Adding the CSV file for the Product Master.

df.SKU.Master <- read_csv("D:/Goolge Drive/FOBRA/02. Project Documents/04. Data Analysis/Analysis BAO/Data/Master Data/SKU.csv")


# Adding the CSV file for the transportation with SKUs 

df.Transport.SKus <- read_csv("D:/Goolge Drive/FOBRA/02. Project Documents/04. Data Analysis/Analysis BAO/Data/DPA_Transportation DB FY2017 by product_YBO.csv")

spec(df.Transport.SKus)

df.Transport.SKus <- read_csv("D:/Goolge Drive/FOBRA/02. Project Documents/04. Data Analysis/Analysis BAO/Data/DPA_Transportation DB FY2017 by product_YBO.csv",
                              col_types = cols(
                                `Data Pedido` = col_character(),
                                `Categoria Pedido` = col_character(),
                                `Documento de Custo` = col_integer(),
                                Transporte = col_double(),
                                Pedido = col_double(),
                                Referencia = col_character(),
                                `SAP Tipo de Condicao` = col_character(),
                                `Tipo de Condicao - Nivel 1` = col_character(),
                                `Tipo de Condicao - Nivel 2` = col_character(),
                                `Tipo de Condicao - Nivel 3` = col_character(),
                                Planta = col_character(),
                                `Chave CUSTLIST` = col_character(),
                                `SAP Transportadora` = col_integer(),
                                `SAP Veiculo` = col_character(),
                                `SAP Produto` = col_integer(),
                                `Qtd (CX)` = col_integer(),
                                `Frete Liquido (Nova Premissa CCS)` = col_double(),
                                `Peso Bruto` = col_double(),
                                `Distancia Distribuida` = col_double()
                              )
                              )
# Add the Pallets and Mp3 to the master file with Transport and Skus.
# To add the Pallet we look at the columns: Peso BRUTO por PAL (columns R) because in Transactional file we look at columns Peso Bruto
# To add the M3 we look at the columns: UNI Volume (cm^3) (column I) and UNI por CX (column J) 
# because in Transactional file we ignore Qtd (CX) which lead to wrong manipulation.


# Adjust In Skus master file first by link the unit per case and Unit m3 and Kg
df.SKU.Master <- df.SKU.Master %>%
  
  mutate( CBM.per.KG = (`UNI Volume (cm^3)`/10^6)/`UNI Peso Bruto (Kg)`
  )

# Link CBM, Pallet, and Kg to Transactional Data SKU
df.Transport.SKus <- df.Transport.SKus %>%
  mutate(Pallet = `Peso Bruto`/df.SKU.Master$`Peso BRUTO por PAL`[match(`SAP Produto`,df.SKU.Master$`SAP Produto`)]) %>%
  
  mutate(CBM = `Peso Bruto`*df.SKU.Master$CBM.per.KG[match(`SAP Produto`,df.SKU.Master$`SAP Produto`)]) %>%
  group_by(`Documento de Custo`,Pedido,`SAP Produto`) %>%
  mutate(TotalKg = sum(`Peso Bruto`),
         Occurance = n()
  )


# Check the No value SKUs. (Optionals)

Missing.info <- df.Transport.SKus %>%
  filter(!(is.na(`Peso Bruto`) | `Peso Bruto`==0
  )
  )%>%
  filter(is.na(Pallet) | Pallet == 0) %>%
  group_by(`SAP Produto`)%>%
  summarise(n())


# Create the delivery order by filling out the Delivery order cancellation with each other

df.success.delivery <- df.Transport.SKus %>%
  filter(TotalKg > 0)


write.csv(df.success.delivery, file = "C:/Users/Bao/Desktop/DPA_Physical Shipment with SKUs.csv", row.names = FALSE)
write.csv(df.Transport.SKus, file = "C:/Users/Bao/Desktop/DPA_Raw.Transport.with.SKUs.csv", row.names = FALSE)
# Create the summary table for Shipment order
# Use the Shipment.Oder, Delivery Order, and Customer List as the based
Sum.Ship.Order <- df.Transport.SKus %>%
  group_by(`Documento de Custo`,Pedido, `Chave CUSTLIST`)%>%
  summarise(KG = sum(`Peso Bruto`),
            pallet = sum(Pallet),
            cbm = sum (CBM))
names(Sum.Ship.Order)[1] <- "Shipment.Document"
names(Sum.Ship.Order)[2] <- "Delivery.Document"
names(Sum.Ship.Order)[3] <- "Chave.CUSTLIST"



#Use the Shipment.Oder, Delivery Order as the based
Sum.Ship.Order2 <- df.Transport.SKus %>%
  group_by(`Documento de Custo`,Pedido)%>%
  summarise(KG = sum(`Peso Bruto`),
            pallet = sum(Pallet),
            cbm = sum (CBM))
names(Sum.Ship.Order2)[1] <- "Shipment.Document"
names(Sum.Ship.Order2)[2] <- "Delivery.Document"


#check the Delivery order and SKUs.

check.deliery.customer <- df.Transport.SKus %>%
  group_by(Pedido,`Chave CUSTLIST`) %>%
  summarise(Count.nu = n()) %>%
  filter(Count.nu > 1)


write.csv(check.deliery.customer, file = "C:/Users/Bao/Desktop/checkdelivery.csv", row.names = FALSE)
  

# Export the nesscessary records.

Need.record <- df.Transport.SKus %>%
  filter(`Documento de Custo`== 18548785)

write.csv(Need.record, file = "C:/Users/Bao/Desktop/18548785.csv", row.names = FALSE)