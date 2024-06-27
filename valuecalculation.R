#Note that you may need to install the packages below prior to loading them in the library. 
library(dplyr)
library(ggplot2)
library(broom)
library(writexl)
library(readxl)

#If this code is cloned from the Github repository the Supplementary material 1 xlsx file is located in the working directoy and ready to read.

#Load stock data from Supplementary material 1 and ensuring that the relevant column is numeric.
StocksFAOSTAT2022 <- read_excel("Supplementary Material 1..xlsx", sheet = "Stocks")
StocksFAOSTAT2022$Value <- as.numeric(StocksFAOSTAT2022$Value)

ProductionVolumeFAOSTAT2022 <-read_excel("Supplementary Material 1..xlsx", sheet = "ProductionVolume")
ProductionVolumeFAOSTAT2022$Value <- as.numeric(ProductionVolumeFAOSTAT2022$Value)


ProductionQuantityFAOSTAT2022 <-read_excel("Supplementary Material 1..xlsx", sheet = "ProductionQuantity")
ProductionQuantityFAOSTAT2022$Value <- as.numeric(ProductionQuantityFAOSTAT2022$Value)


ProducerPriceFAOSTAT2022 <-read_excel("Supplementary Material 1..xlsx", sheet = "ProducerPrice")
ProducerPriceFAOSTAT2022$Value <- as.numeric(ProducerPriceFAOSTAT2022$Value)

# As a sanity check the FAO data and calculated values were compared to the official national statistics provided by the Natural Resources Institute Finland.
# Finland was selected for the comparison as they provide official reports on the values calculated from FAOSTAT below and have data registered in FAOSTAT.
# https://www.luke.fi/en/statistics/meat-production/meat-production-2022
# https://www.luke.fi/en/statistics/producer-prices-of-agricultural-and-horticultural-products/producer-prices-of-agricultural-products-52022-provisional)

# This selects the necessary columns from ProductionVolumeFAOSTAT2022 (reported production of each item in tonnes) and adds the specie as a new column.
volume_df <- ProductionVolumeFAOSTAT2022 %>%
  mutate(Specie = if_else(Item == "Hen eggs in shell, fresh", "Chickens", "" )) %>%
  mutate(Specie = if_else(Item == "Meat of chickens, fresh or chilled", "Chickens", Specie )) %>%
  mutate(Specie = if_else(Item == "Raw milk of cattle", "Cattle", Specie )) %>%
  mutate(Specie = if_else(Item == "Meat of cattle with the bone, fresh or chilled", "Cattle", Specie )) %>%
  mutate(Specie = if_else(Item == "Raw milk of goats", "Goats", Specie )) %>%
  mutate(Specie = if_else(Item == "Meat of goat, fresh or chilled", "Goats", Specie )) %>%
  mutate(Specie = if_else(Item == "Raw milk of sheep", "Sheep", Specie )) %>%
  mutate(Specie = if_else(Item == "Meat of sheep, fresh or chilled", "Sheep", Specie )) %>%
  mutate(Specie = if_else(Item == "Meat of pig with the bone, fresh or chilled", "Swine / pigs", Specie )) %>%
  select(Area, Item, Specie, Value, Unit) %>%
  rename(`Production` = Value)

# Creates a Producer price data frame from FAOSTAT data and name the value column 'USD/tonne'
price_df <- ProducerPriceFAOSTAT2022 %>%
  select(Area, Item, Value) %>%
  rename(`USD/tonne` = Value)

# Merges the production volume  the dataframes on the common keys Area (country) and Item (the registered product)
# Eggs can be reported in both the number of eggs and the total weight, rows with the number of eggs are removed.
volume_price_df <- volume_df %>%
  inner_join(price_df, by = c("Area", "Item")) %>%
  # Remove rows where Unit is "1000 No"
  filter(Unit != "1000 No") %>%
  # Create the new column ValueUSD
  mutate(ValueUSD = Production * `USD/tonne`)

# Selects the necessary columns from ProductionQuantityFAOSTAT2022 to report the number of animals used in production in each country.
producing_animals_df <- ProductionQuantityFAOSTAT2022 %>%
  select(Area, Item, Unit, Value) %>%
  mutate(Value = if_else(Unit == "1000 An", Value * 1000, Value)) %>%
  mutate(Unit = if_else(Unit == "1000 An", "An", Unit)) %>%
  rename(ProducingAnimals = Value) %>%
  filter(ProducingAnimals != 0)

# Creates a new data frame containing the Production volume (in tonnes), the price (in USD per tonne) and the number of animals involved in production.
volume_price_producing_df <- volume_price_df %>%
  inner_join(producing_animals_df %>% select(Area, Item, ProducingAnimals), by = c("Area", "Item"))

# Creates a new column where the value per animal involved in production is calculated.
volume_price_producing_value_df <- volume_price_producing_df %>%
  mutate(ValuePerHead = ValueUSD / ProducingAnimals)

# Creates a new dataframe for the total number of animals per specie in each country.
stock_df <- StocksFAOSTAT2022 %>%
  select(Area, Item, Unit, Value) %>%
  mutate(Value = if_else(Unit == "1000 An", Value * 1000, Value)) %>%
  mutate(Unit = if_else(Unit == "1000 An", "An", Unit)) %>%
  rename(Stock = Value) %>%
  rename(Specie = Item)

# Adds the number of animals per species to create a new dataframe
volume_price_producing_value_stock_df <- volume_price_producing_value_df %>%
  inner_join(stock_df %>% select(Area, Specie, Stock), by = c("Area", "Specie"))
  
# This statement calculates a cullrate based on the number of animals slaughtered each year. 
# This value becomes highly misleading for species where a significant amount of animals is used for non-meat purposes.
volume_price_producing_value_stock_cullrate_df <- volume_price_producing_value_stock_df %>%
  mutate(Cullrate = ProducingAnimals/Stock)

# Summarisation of data to get an overview of the dataset.
summary <- volume_price_producing_value_stock_cullrate_df %>%
  group_by(Item) %>%
  summarise(ValuePerHead = median(ValuePerHead), Cullrate = median(Cullrate), LifeExpectancy = 1/Cullrate, NrAnimals= sum(Production), NrCountries = n_distinct(Area))

summary_mean <- volume_price_producing_value_stock_cullrate_df %>%
  group_by(Item) %>%
  summarise(ValuePerHead = mean(ValuePerHead, na.rm = TRUE))


# Output 1 (depreciated in favour of output 5 for the published paper), A box plot of value per head and item calculated on the entire FAOSTAT dataset.

# Creates the box plot
figure1 <- ggplot(volume_price_producing_value_stock_cullrate_df, aes(x = Item, y = ValuePerHead, fill = Item)) +
  geom_boxplot() +
  labs(title = "Value Per Head by Item", x = "Item", y = "Annual Value Per Head (USD)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "none", 
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  ylim(0, 5000)

  
# Saves the plot as a PNG file with 600 DPI
#  ggsave("OldFigure1.png", plot = figure1, dpi = 600, width = 8, height = 6, units = "in")  
  
# Output 2 (table 1 in the article), a summary of stats calculated on the entire FAOSTAT dataset.

 value_stock_countries_summary <-  volume_price_producing_value_stock_cullrate_df %>%
   group_by(Item) %>%
   summarise(USDPerHead = median(ValuePerHead), Sd = sd(ValuePerHead, na.rm = TRUE), NrAnimals= sum(ProducingAnimals), NrCountries = n_distinct(Area))

 # Modify column names and write values in a suitable table
 value_stock_countries_summary_table <- value_stock_countries_summary %>%
   mutate(
     `USD per head` = round(USDPerHead, 1),  # Format USDPerHead to one decimal
     SD = round(Sd, 1),  # Format SD to one decimal
     `NrAnimals (million)` = round(NrAnimals / 1000000, 1)  # Scale NrAnimals and rename
   ) %>%
   select(`Item`, `USD per head`, `SD`, `NrAnimals (million)`, `NrCountries`)  # Select columns with adjusted names
 
 write_xlsx(value_stock_countries_summary_table, path = "Table 1, value_per_head.xlsx")

# Output 3 (table2 in the article), a summary of stats calculated on data from countries inside the internal EU market. 

    #The below codes rerun the final part of the experiment but using only data from EU countries
  eu_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", 
                    "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", 
                    "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", 
                    "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")
  
  # Filter the dataframe to keep only rows with an EU country in the Area column
  volume_price_producing_value_stock_cullrate_EU_df <- volume_price_producing_value_stock_cullrate_df %>%
    filter(Area %in% eu_countries)
  
  volume_price_producing_value_stock_cullrate_nonEU_df <- volume_price_producing_value_stock_cullrate_df %>%
    filter(!Area %in% eu_countries)
  
  value_stock_countries_summary_EU <-  volume_price_producing_value_stock_cullrate_EU_df %>%
    group_by(Item) %>%
    summarise(USDPerHead = median(ValuePerHead), Sd = sd(ValuePerHead, na.rm = TRUE), NrAnimals= sum(ProducingAnimals), NrCountries = n_distinct(Area))
  
  # Modify column names and write values in a suitable table
  value_stock_countries_summary_EU_table <- value_stock_countries_summary_EU %>%
    mutate(
      `USD per head` = round(USDPerHead, 1),  # Format USDPerHead to one decimal
      SD = round(Sd, 1),  # Format SD to one decimal
      `NrAnimals (million)` = round(NrAnimals / 1000000, 1)  # Scale NrAnimals and rename
    ) %>%
    select(`Item`, `USD per head`, `SD`, `NrAnimals (million)`, `NrCountries`)  # Select columns with adjusted names

write_xlsx(value_stock_countries_summary_EU_table, path = "Table 2, value_per_head_EU.xlsx")


# Output 4 (not used in article), A box plot of value per head calculated on European countries in the dataset. As expected variance is much smaller.

# Creates the box plot
figureX <- ggplot(volume_price_producing_value_stock_cullrate_EU_df, aes(x = Item, y = ValuePerHead, fill = Item)) +
  geom_boxplot() +
  labs(title = "Value Per Head by Item", x = "Item", y = "Annual Value Per Head (USD)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "none", 
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  ylim(0, 5000)


# Saves the plot as a PNG file with 600 DPI
#ggsave("figureX.png", plot = figureX, dpi = 600, width = 8, height = 6, units = "in")  

  ggplot(volume_price_producing_value_stock_cullrate_EU_df, aes(x = Item, y = ValuePerHead, fill = Item)) +
    geom_boxplot() +
    labs(title = "Value Per Head by Item", x = "Item", y = "Annual Value Per Head (USD), EU Countries only") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
    ylim(0, 5000)
  
  # Combines dataframes with a new column indicating EU status
  combined_df <- bind_rows(
    volume_price_producing_value_stock_cullrate_EU_df %>%
      mutate(EU = TRUE),
    volume_price_producing_value_stock_cullrate_nonEU_df %>%
      mutate(EU = FALSE)
  )
  
  #Output 5 (figure 1), comparison of value per head between markets inside and outside the EU internal market.
 
  # Perform t-tests grouped by 'Item' on 'ValuePerHead' and organize results
  ttest_ValueperHead <- combined_df %>%
    group_by(Item) %>%
    do(tidy(t.test(ValuePerHead ~ EU, data = .)))
  
  # Display the resulting t-test results
  print(ttest_ValueperHead)

  # Modify EU column to factor with descriptive levels
  combined_df$EU <- factor(combined_df$EU, levels = c(FALSE, TRUE), labels = c("All Countries", "EU Members"))
  
  # Shorten names to make the facets look better.
 combined_df <- combined_df %>%
    mutate(Item = if_else(Item == "Meat of chickens, fresh or chilled", "Meat of chickens", Item )) %>%
    mutate(Item = if_else(Item == "Meat of cattle with the bone, fresh or chilled", "Meat of cattle", Item )) %>%
    mutate(Item = if_else(Item == "Meat of goat, fresh or chilled", "Meat of goat", Item )) %>%
    mutate(Item = if_else(Item == "Meat of sheep, fresh or chilled", "Meat of sheep", Item )) %>%
    mutate(Item = if_else(Item == "Meat of pig with the bone, fresh or chilled", "Meat of pig", Item ))
  
  # Creates violin plots for ValuePerHead by Item and EU status with customized colors and legend
  violin_plot <- ggplot(combined_df, aes(x = EU, y = ValuePerHead, fill = EU)) +
    geom_violin(trim = FALSE) +
    facet_wrap(~ Item, scales = "free") +
    theme_minimal() +
    labs(
      title = "Distribution of Value Per Head by Item produced and EU Status",
      x = "EU Status",
      y = "Value Per Head",
      fill = "Membership"
    ) +
    scale_fill_manual(
      values = c("All Countries" = rgb(73, 173, 196, maxColorValue = 255), 
                 "EU Members" = rgb(255, 165, 0, maxColorValue = 255)), # Orange color for EU members
      labels = c("All Countries", "EU Members")
    ) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      strip.text = element_text(
        size = 10, face = "bold", color = "black", hjust = 0.5, margin = margin(t = 12),
        # Allow multiline text
        vjust = 0.5, lineheight = 0.9
      ) +
      facet_wrap(~ grp, labeller = label_wrap_gen(width=10))
    )
  
  # Save the plot as a PNG file with 600 DPI
  ggsave("figure1.png", plot = violin_plot, dpi = 600, width = 8, height = 6, units = "in")
