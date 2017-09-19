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
  
# Add the Pallets and Mp3 to the master file with Transport and Skus.
  # To add the Pallet we look at the columns: CX por PAL (columns 0) because in Transactional file we saw Qtd (CX)
  # To add the M3 we look at the columns: UNI Volume (cm^3) (column I) and UNI por CX (column J)
  # because in Transactional file we have only Qtd (CX) which need convert to Unit
  # To add the KG we look at the columns: UNI Peso Bruto (Kg) and UNI por CX
  # because in Transactional file we have only Qtd (CX) which need convert to Unit
 
  # Adjust In Skus master file first by link the unit per case and Unit m3 and Kg
   df.SKU.Master <- df.SKU.Master %>%
    
    mutate( Case.CBM = `UNI Volume (cm^3)` * `UNI por CX`/10^6
      ) %>%
  mutate( Case.KG = `UNI Peso Bruto (Kg)` * `UNI por CX`
  )
  
   # Link CBM, Pallet, and Kg to Transactional Data SKU
  df.Transport.SKus <- df.Transport.SKus %>%
    mutate(Pallet = `Qtd (CX)`/df.SKU.Master$`CX por PAL`[match(`SAP Produto`,df.SKU.Master$`SAP Produto`)]) %>%
  
  mutate(CBM = df.SKU.Master$Case.CBM[match(`SAP Produto`,df.SKU.Master$`SAP Produto`)]*`Qtd (CX)`) %>%
  
    mutate(Kg = df.SKU.Master$Case.KG[match(`SAP Produto`,df.SKU.Master$`SAP Produto`)]*`Qtd (CX)`)
    
  
  # Check the No value SKUs. (Optionals)
  
  Missing.info <- df.Transport.SKus %>%
    filter(!(is.na(`Qtd (CX)`) | `Qtd (CX)`==0
             )
             )%>%
    filter(is.na(Pallet) | Pallet == 0) %>%
    group_by(`SAP Produto`)%>%
  summarise(n())
  
  # Create the summary table for Shipment order
  
  Sum.Ship.Order <- df.Transport.SKus %>%
    group_by(`Documento de Custo`)%>%
  summarise(Qty = sum(`Qtd (CX)`),
            pallet = sum(Pallet),
            KG = sum (Kg),
            cbm = sum (CBM))
  
  # Export the nesscessary records.
  
  Need.record <- df.Transport.SKus %>%
    filter(`Documento de Custo`== 18548785)
  
  write.csv(Need.record, file = "C:/Users/Bao/Desktop/18548785.csv", row.names = FALSE)
    
  # ggplot(data.m, aes(Names, value)) +   
  #geom_bar(aes(fill = variable), position = "dodge", stat="identity")
  #data.m <- melt(data, id.vars='Names') https://stackoverflow.com/questions/18158461/grouped-bar-plot-in-ggplot
  #https://stackoverflow.com/questions/17303573/ggplot-multiple-grouping-bar
  
