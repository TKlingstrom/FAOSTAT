library(dplyr)
library(ggplot2)
library(broom)

SalesPriceFAOSTAT2022 <- read_csv("SalesPriceFAOSTAT2022.csv")
StocksFAOSTAT2022 <- read_csv("StocksFAOSTAT2022.csv")
ProductionVolumeFAOSTAT2022V2 <- read_excel("ProductionVolumeFAOSTAT2022V2.xlsx")
ProductionAmountFAOSTAT2022 <- read_delim("ProductionAmountFAOSTAT2022.csv", 
                                            +     delim = "\t", escape_double = FALSE, 
                                            +     trim_ws = TRUE)
ProductionPriceFAOSTAT2022 <- read_csv("ProductionPriceFAOSTAT2022.csv")

# Selecting the necessary columns from ProductionVolumeFAOSTAT2022V2
volume_df <- ProductionVolumeFAOSTAT2022V2 %>%
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

# Renaming the 'Value' column from ProductionPriceFAOSTAT2022 to 'USD/tonne'
price_df <- ProductionPriceFAOSTAT2022 %>%
  select(Area, Item, Value) %>%
  rename(`USD/tonne` = Value)

# Merging the dataframes on common keys (Area, Item)
volume_price_df <- volume_df %>%
  inner_join(price_df, by = c("Area", "Item")) %>%
  # Remove rows where Unit is "1000 No"
  filter(Unit != "1000 No") %>%
  # Create the new column ValueUSD
  mutate(ValueUSD = Production * `USD/tonne`)

# Select the necessary columns from ProductionAmountFAOSTAT2022 and adjust the values
producing_animals_df <- ProductionAmountFAOSTAT2022 %>%
  select(Area, Item, Unit, Value) %>%
  mutate(Value = if_else(Unit == "1000 An", Value * 1000, Value)) %>%
  mutate(Unit = if_else(Unit == "1000 An", "An", Unit)) %>%
  rename(ProducingAnimals = Value) %>%
  filter(ProducingAnimals != 0)

# Merge with the volume_price_df on common keys (Area, Item)
volume_price_producing_df <- volume_price_df %>%
  inner_join(producing_animals_df %>% select(Area, Item, ProducingAnimals), by = c("Area", "Item"))

# Create a new column ValuePerHead
volume_price_producing_value_df <- volume_price_producing_df %>%
  mutate(ValuePerHead = ValueUSD / ProducingAnimals)

# Create a new dataframe for animal stocks in each country. 
stock_df <- StocksFAOSTAT2022 %>%
  select(Area, Item, Unit, Value) %>%
  mutate(Value = if_else(Unit == "1000 An", Value * 1000, Value)) %>%
  mutate(Unit = if_else(Unit == "1000 An", "An", Unit)) %>%
  rename(Stock = Value) %>%
  rename(Specie = Item)

# Add stock volumn to the main dataframe
volume_price_producing_value_stock_df <- volume_price_producing_value_df %>%
  inner_join(stock_df %>% select(Area, Specie, Stock), by = c("Area", "Specie"))
  
# Add stock volumn to the main dataframe
volume_price_producing_value_stock_df <- volume_price_producing_value_df %>%
  inner_join(stock_df %>% select(Area, Specie, Stock), by = c("Area", "Specie"))

# Add stock volumn to the main dataframe
volume_price_producing_value_stock_cullrate_df <- volume_price_producing_value_stock_df %>%
  mutate(Cullrate = ProducingAnimals/Stock)

# Summarisation of data to get an overview of the dataset.
summary <- volume_price_producing_value_stock_cullrate_df %>%
  group_by(Item) %>%
  summarise(Cullrate = median(Cullrate), ValuePerHead = median(ValuePerHead), LifeExpectancy = 1/Cullrate, NrAnimals= sum(Production), NrCountries = n_distinct(Area))

grouped <- volume_price_producing_value_stock_cullrate_df %>%
  group_by(Item) %>%
  summarize(ValuePerHead = mean(ValuePerHead, na.rm = TRUE))


# Output 1, box plot of value per head and item calculated on the entire FAOSTAT dataset.

# Create the box plot
ggplot(volume_price_producing_value_stock_cullrate_df, aes(x = Item, y = ValuePerHead, fill = Item)) +
  geom_boxplot() +
  labs(title = "Value Per Head by Item", x = "Item", y = "Annual Value Per Head (USD)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  ylim(0, 5000)
  theme(legend.position = "none")  # Optionally, remove the legend as the fill colors correspond to the same items on the x-axis
  
# Output 2, a summary of stats calculated on the entire FAOSTAT dataset.

 value_stock_countries_summary <-  volume_price_producing_value_stock_cullrate_df %>%
   group_by(Item) %>%
   summarise(USDPerHead = median(ValuePerHead), Sd = sd(ValuePerHead, na.rm = TRUE), NrAnimals= sum(ProducingAnimals), NrCountries = n_distinct(Area))

 write.csv2(value_stock_countries_summary, file = "Table 1, value_per_head.csv") 

# Output 3 (table2), a summary of stats calculated on data from countries inside the internal EU market. 

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
    summarise(USDperHead = median(ValuePerHead), StandardDeviation = sd(ValuePerHead, na.rm = TRUE), NrAnimals= sum(ProducingAnimals), NrCountries = n_distinct(Area))

write.csv2(value_stock_countries_summary_EU, file = "Table 2, value_per_head_EU.csv")

# Output 4 (not used), Box plot of value per head calculated on European countries in the dataset
  ggplot(volume_price_producing_value_stock_cullrate_EU_df, aes(x = Item, y = ValuePerHead, fill = Item)) +
    geom_boxplot() +
    labs(title = "Value Per Head by Item", x = "Item", y = "Annual Value Per Head (USD), EU Countries only") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
    ylim(0, 5000)
  
  # Combine dataframes with a new column indicating EU status
  combined_df <- bind_rows(
    volume_price_producing_value_stock_cullrate_EU_df %>%
      mutate(EU = TRUE),
    volume_price_producing_value_stock_cullrate_nonEU_df %>%
      mutate(EU = FALSE)
  )
  
  #Output 5 (not), comparison of distribution between markets inside and outside the EU internal market.
  
  # Perform t-tests grouped by 'Item' on 'ValuePerHead' and organize results
  ttest_ValueperHead <- combined_df %>%
    group_by(Item) %>%
    do(
      tidy(
        t.test(
          ValuePerHead ~ EU, 
          data = .
        )
      )
    )
  
  # Display the resulting t-test results
  print(ttest_ValueperHead)
  
  # Create violin plots for ValuePerHead by Item and EU status
  violin_plot <- ggplot(combined_df, aes(x = EU, y = ValuePerHead, fill = EU)) +
    geom_violin(trim = FALSE) +
    facet_wrap(~ Item, scales = "free") +
    theme_minimal() +
    labs(title = "Distribution of ValuePerHead by Item and EU Status",
         x = "EU Status",
         y = "ValuePerHead") +
  
  print(violin_plot)
  
  
