# Try to install package tidyverse

install.packages("tidyverse")

# Call functions in "tivyverse" every day

library(tidyverse)

#Add library: stringr

library(stringr)

#Add lubridate
library(lubridate)


#great the working path

setwd("D:/Goolge Drive/FOBRA/02. Project Documents/04. Data Analysis/Analysis BAO/Data/Master Data/") #company

setwd("D:/Google Drive/FOBRA/02. Project Documents/04. Data Analysis/Analysis BAO/Data/Master Data/") #home

# Adding the CSV file: DPA___Relatório_de_Tarifa-br

df.cost.transportation <- read_csv("RouteCost.csv") 

# Create the matching reference later to use. Following the structure
# Origin Code (1208) 
#-> Desination City (ARARAS in capital) 
#-> Type of Transport (TRANSFERENCIA)
#-> Vehicle Type SAP (BR275) 
#-> Carrer Name (100223303)

  mutate(Look.Up=paste(str_sub(SAP.ORIGEM,-4,-1),DESTINO.CIDADE,TIPO.REMESSA,SAP.VEICULO,SAP.TRANSPORTADORA,sep="-"))
