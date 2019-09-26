library(tidyverse)
library(tidyr)
library(stringr)
library(ggplot2)
library(plotly)

women_shoes

women_shoes$brand <- tolower(women_shoes$brand)
women_shoes$brand <- gsub("-"," ",women_shoes$brand)
for (i in 1: nrow(women_shoes)) {
  if (women_shoes$brand[i] == "aeorosoles") {
    women_shoes$brand[i] <- "aerosoles"
  }
  if (women_shoes$brand[i] == "bamboo Brand" ) {
    women_shoes$brand[i] <- "bamboo"
  }
  if (women_shoes$brand[i] ==  "baretraps") {
    women_shoes$brand[i] <- "bare traps"
  }
  if (women_shoes$brand[i] == "beacon" ) {
    women_shoes$brand[i] <- "beacon shoes"
  }
  if (women_shoes$brand[i] == "benjamin walk" ) {
    women_shoes$brand[i] <- "benjamin walk dyeables"
  }
  if (women_shoes$brand[i] == "carlos by carlo santana" ) {
    women_shoes$brand[i] <- "carlos"
  } else if (women_shoes$brand[i] == "carlos by carlos santana") {
    women_shoes$brand[i] <- "carlos"
  }
  if (women_shoes$brand[i] == "cat footwear") {
    women_shoes$brand[i] <- "cat"
  }
  if (women_shoes$brand[i] == "charles by charles david") {
    women_shoes$brand[i] <- "charles david"
  }
  if (women_shoes$brand[i] == "cityclassified" ) {
    women_shoes$brand[i] <- "city classified"
  }
  if (women_shoes$brand[i] == "diba" ) {
    women_shoes$brand[i] <- "diba true"
  }
  if (women_shoes$brand[i] == "dr. scholl's shoes" ) {
    women_shoes$brand[i] <- "dr. scholl's"
  }
  if (women_shoes$brand[i] == "drew shoe" ) {
    women_shoes$brand[i] <- "drew"
  }
  if (women_shoes$brand[i] ==  "ellie shoes") {
    women_shoes$brand[i] <- "ellie"
  }
  if (women_shoes$brand[i] == "fergie footwear" ) {
    women_shoes$brand[i] <- "fergie"
  } else if (women_shoes$brand[i] == "fergie shoes") {
    women_shoes$brand[i] <- "fergie"
  }
  if (women_shoes$brand[i] ==  "genuine grip footwear") {
    women_shoes$brand[i] <- "genuine grip"
  }
  if (women_shoes$brand[i] == "j.rene") {
    women_shoes$brand[i] <- "j. renee"
  }
  if (women_shoes$brand[i] == "jbu by jambu" ) {
    women_shoes$brand[i] <- "jbu"
  }
  if (women_shoes$brand[i] ==  "koolaburra by ugg" ) {
    women_shoes$brand[i] <- "koolaburra"
  }
  if (women_shoes$brand[i] == "l'artiste by spring step" ) {
    women_shoes$brand[i] <- "lartiste"
  } else if (women_shoes$brand[i] == "lartiste by spring step") {
    women_shoes$brand[i] <- "lartiste"
  }
  if (women_shoes$brand[i] == "lauren by ralph lauren" ) {
    women_shoes$brand[i] <- "ralph lauren"
  } else if (women_shoes$brand[i] ==  "lauren ralph lauren" ) {
    women_shoes$brand[i] <- "ralph lauren"
  }
  if (women_shoes$brand[i] == "life stride" ) {
    women_shoes$brand[i] <- "lifestride"
  } else if (women_shoes$brand[i] ==   "lifestride shoes" ) {
    women_shoes$brand[i] <- "lifestride"
  }
  if (women_shoes$brand[i] == "michael michael kors" ) {
    women_shoes$brand[i] <- "michael kors"
  }
  if (women_shoes$brand[i] == "pleaserusa"  ) {
    women_shoes$brand[i] <- "pleaser usa"
  }
  if (women_shoes$brand[i] == "skechers work") {
    women_shoes$brand[i] <- "skechers"
  }
  if (women_shoes$brand[i] ==  "soda shoes"  ) {
    women_shoes$brand[i] <-"soda"
  }
  if (women_shoes$brand[i] ==   "soft style by hush puppies"  ) {
    women_shoes$brand[i] <-"soft style"
  }
  if (women_shoes$brand[i] == "softwalk footwear" ) {
    women_shoes$brand[i] <-"softwalk"
  }
  if (women_shoes$brand[i] ==  "sorel footwear" ) {
    women_shoes$brand[i] <-"sorel"
  }
  if (women_shoes$brand[i] ==  "sperry top  sider"  ) {
    women_shoes$brand[i] <- "sperry top sider"
  } else if (women_shoes$brand[i] ==  "sperry" ) {
    women_shoes$brand[i] <- "sperry top sider"
  }
  if (women_shoes$brand[i] ==  "summitfashions" ) {
    women_shoes$brand[i] <-"summit fashions"
  }
  if (women_shoes$brand[i] ==   "toms shoes"  ) {
    women_shoes$brand[i] <- "toms"
  }
  if (women_shoes$brand[i] ==  "ugg australia"  ) {
    women_shoes$brand[i] <-"ugg"
  }
  if (women_shoes$brand[i] ==  "vionic with orthaheel technology" ) {
    women_shoes$brand[i] <- "vionic"
  }
}

women_shoes2 <- transform(women_shoes, lowname = tolower(name))
women_shoes2 %>% 
  group_by(id) %>% 
  mutate(
    prices = (prices.amountMax + prices.amountMin)/2
  ) %>% 
  select(id, dateAdded, brand, lowname, prices, prices.offer) -> new_df
head(new_df)

tmp <- new_df %>% 
  ggplot(aes(x = 1, y = prices)) +
  geom_boxplot(color = "#CDBCB6") +
  coord_flip() +
  theme_classic()

tmp0 <- new_df %>% 
  filter(prices < 500) %>% 
  ggplot(aes(x = 1, y = prices)) +
  geom_boxplot(color = "#CDBCB6") +
  coord_flip() +
  theme_classic()

test1 <- new_df %>% 
  mutate(
    category = case_when(
      str_detect(lowname, pattern = "boot") ~ "boots",
      str_detect(lowname, pattern = "pump") ~ "pump",
      str_detect(lowname, pattern =  "sneaker") ~ "sneaker",
      str_detect(lowname, pattern = "sandal") ~ "sandal",
      str_detect(lowname, pattern = "flat") ~ "flat",
      str_detect(lowname, pattern = "slipper") ~ "slipper",
      str_detect(lowname, pattern = "loafer") ~ "loafer",
      str_detect(lowname, pattern = "clog") ~ "clog"
    ),
    discount = case_when(
      str_detect(prices.offer, pattern = "off") ~ "Yes",
      TRUE ~ "No"
    ) 
  ) %>% 
  filter(prices < 500)

test1$category[is.na(test1$category)] <- "others"

test1 %>% 
  select(id, dateAdded, brand, lowname, prices, category, discount) %>% 
  arrange(desc(prices))-> new_df2
head(new_df2)

ggplotly()

new_df2 %>% 
  group_by(brand) %>% 
  summarize(
    avg_price = mean(prices),
    med_price = median(prices),
    max_price = max(prices),
    min_price = min(prices),
    count = n()
  ) %>% 
  arrange(desc(avg_price)) -> avg_price_by_brand
head(avg_price_by_brand)
dim(avg_price_by_brand)

new_df2 %>% 
  group_by(category) %>% 
  summarize(
    avg_price_by_category = mean(prices, na.rm = TRUE),
    count= n()
  ) %>% 
  select(category, count, avg_price_by_category) %>% 
  arrange(avg_price_by_category)-> avg_price_category





tmp1 <- avg_price_category %>% 
  filter(category != "others") %>% 
  ggplot(aes(x = category, y = avg_price_by_category, size = count)) +
  geom_point(color = "darkseagreen3") +
  theme_bw() +
  xlab("Category of Shoes") +
  ylab("Average Price in Each Category") +
  labs(title = "The Count of Average Price in Different Categories") +
  theme(plot.title = element_text(hjust = 0))

tmp2 <- new_df2 %>% 
  filter(category != "others") %>% 
  ggplot(aes(x = category, y = prices))+
  stat_summary(fun.y = "mean", fun.ymin = min, fun.ymax = max, colour = "#7C7F91") +
  geom_hline(yintercept=100, linetype="dashed", color = "red")+
  xlab("Category of Shoes") +
  ylab("Prices") +
  labs(title = "Summary of Prices in Different Categories of Shoes") +
  theme(plot.title = element_text(hjust = 0))+
  theme_classic()
ggplotly(tmp2)

tmp3 <- avg_price_by_brand %>% 
  ggplot(aes(x = avg_price)) +
  geom_histogram(binwidth = 10, fill = "#C3CED9", color = "white") +
  theme_classic() +
  labs(title = "The Distribution of Average Price by Brand") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Average Price by Distinct Brands")
ggplotly(tmp3)



  
new_df2 %>% 
  filter(category != "others", prices < 200) %>% 
  ggplot(aes(x = category, y = prices)) +
  geom_boxplot(color = "#7C7F91")+
  theme_classic()+
  labs(title = "Price Among Categories")-> price_category
ggplotly(price_category)

library(lubridate)

new_df2$dateAdded

library(magrittr)
library(dplyr)
library(lubridate)

as.Date(new_df2$dateAdded, format = "%y-%m-%d") ->new_df2$dateAdded

new_df2 %>% 
  separate(dateAdded, into = c("year","month", "day")) %>% 
  unite(year, month, year) -> datetmp

datetmp$year -> as.Date(datetmp$year)


format(new_df2$dateAdded, "%Y-%m") -> new_df2$dateAdded



new_df2%>% 
  group_by(dateAdded, category) %>% 
  summarize(
    avgprice_by_ym = mean(prices)
  ) %>% 
  arrange(dateAdded) -> avg_p_by_year

avg_p_by_year %>% 
  filter(category == "boots") -> boots
avg_p_by_year %>% 
  filter(category == "flat") -> flat
avg_p_by_year %>% 
  filter(category == "pump") -> pump

p = ggplot()+
  geom_line(data = boots, aes(x = dateAdded, y = avgprice_by_ym, color = category, group = 1))+
  geom_line(data = pump, aes(x = dateAdded, y = avgprice_by_ym, color = category, group = 1))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Trend of Avg.Price in Both Boots and Pump") +
  xlab("Year-Month") +
  ylab("Average Price") 
                                                                     
ggplotly(p)


  


    


mutate(year = as.Date(year, "%m.%Y", sep ="-"))
  filter(category == "boots") %>% 
  group_by(year, month) %>% 
  ggplot(aes(x = ))
  


 
  
=

  
