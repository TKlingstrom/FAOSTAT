# FAOSTAT
This is a small repository for code to analyse data from FAOSTAT (https://www.fao.org/faostat/en/#data)

# Terms and conditions
All data in the workbook has been collected from FAOSTAT and is shared by Food and Agriculture Organization of the United Nations under a AttributionNonCommercial-ShareAlike 3.0 IGO (CC BY-NC- SA 3.0 IGO) as per their Open Data Licensing Policy described under their terms and conditions (https://www.fao.org/contact-us/terms/en/) and put into this workbook 2024-06-27. 

The code used to process data is available at Github at https://github.com/TKlingstrom/FAOSTAT and shared under a MIT license. 

## Data in the workbook
All data in Supplementary material 1 is collected from. FAO (2024). FAOSTAT. Rome, Italy: Food and Agriculture Organization of the United Nations (FAO). Retrieved from https://www.fao.org/faostat/en/#data on 2024-06-27

The following settings and datasets were used:

### Producer price
Prices --> Producer Prices
Filter:  Select all
Elements: Producer Price (USD/tonne)
Items: Meat of cattle with the bone, fresh or chilled, Meat of chickens, fresh or chilled, Meat of goat, fresh or chilled, Meat of pig witht he bone, fresh or chilled, Meat of sheep, fresh or chilled, Raw milk of cattle, Raw milk of goats, Raw milk of sheep.
Years: 2022
Months: Annual value

Output type: Table
File type: XLS
Thousand separator in 'Show Data': None:
Output Formatting Options: Flags, Notes, Codes, Units (checked), Null values (not checked). 

### Stocks
Production --> Crops and livestock products
Filter:  Select all
Elements: Stocks
Items: Live animals > (Cattle, Chickens, Goats, Sheep, Swine / pigs).
Years: 2022

Output type: Table
File type: XLS
Thousand separator in 'Show Data': None:
Output Formatting Options: Flags, Notes, Codes, Units (checked), Null values (not checked). 

### ProductionVolume (please note that unfortunate naming mean that Production Volume access the FAOSTAT table named Production Quantity to obtain data on nr of tonnes produced)
Production --> Crops and livestock products
Filter:  Select all
Elements: Production Qauntity
Items: Livestock primary > (Hen eggs in shell, fresh, Meat of cattle with the bone, fresh or chilled, Meat of chickens, fresh or chilled, Meat of goat, fresh or chilled, Meat of pig with the bone, fresh or chilled, Meat of sheep, fresh or chilled, Raw milk of cattle, Raw milk of Goats, Raw milk of sheep).
Years: 2022

Output type: Table
File type: XLS
Thousand separator in 'Show Data': None:
Output Formatting Options: Flags, Notes, Codes, Units (checked), Null values (not checked). 

### ProductionQuantity (please not that unfortunate naming mean that ProductionQuantity access data on the number of Producing Animals)
Production --> Crops and livestock products
Filter:  Select all
Elements: Producing Animals/Slaughtered
Items: Livestock primary > (Hen eggs in shell, fresh, Meat of cattle with the bone, fresh or chilled, Meat of chickens, fresh or chilled, Meat of goat, fresh or chilled, Meat of pig witht he bone, fresh or chilled, Meat of sheep, fresh or chilled, Raw milk of cattle, Raw milk of Goats, Raw milk of sheep).
Years: 2022
Output type: Table
File type: XLS
Thousand separator in 'Show Data': None:
Output Formatting Options: Flags, Notes, Codes, Units (checked), Null values (not checked). 
