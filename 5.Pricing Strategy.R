

# Initially we would want to look at how average price per catgeory is changing against demand over time

dem_pat <- cp %>% filter(Category != 'None'& !ym %in% c('2016-07', '2018-08')) %>%
  group_by(ym, Category) %>% summarise(total_demand = sum(Qty))
ggplot(dem_pat, aes(x = ym, y = total_demand, group = 1)) + 
  geom_line(color = 'maroon', size = 1) +
  facet_wrap(~Category, scale = 'free') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_smooth(formula = y ~ x, method = 'lm' ,color = 'yellow') +
  ggtitle('Change in average price over time') + xlab('Year-Month') + ylab('Average price')


# The demand of Coffee except a few seasonal drops has stayed pretty much constant which is the main
# product under consideration
# The demand of Teaand beans has been increasing over time overall
# The demand of cereal, beers and non-caffeinated drinks has been dropping

# We need to establish of the price of the products has any role to play in these demand changes over time
price_pat <- cp %>% filter(Category != 'None'& !ym %in% c('2016-07', '2018-08')) %>%
  group_by(ym, Category) %>% summarise(avg_price = mean(price))
ggplot(price_pat, aes(x = ym, y = avg_price, group = 1)) + 
  geom_line(color = 'maroon', size = 1) +
  facet_wrap(~Category, scale = 'free') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('Change in demand over time') + xlab('Year-Month') + ylab('Monthly quantity')

# Despite increase in tea price, the demand has been going up
# The price of coffee has dropped but demand has not shown significant increase
# price of food has dropped and so has the demand
# Price of caffinated drinks shows an overall drop but demand is also dropping
# Price of beans as a food item has been fluctuating


# Provided the demand rise in tea despite price change,
# constant demand of key product coffee
# profitability from beans and to lift demand of cereal and non caffeinate drink for profitability
# we need to understand if price change affects the customer tenure negatively
# This will help us in retaining the loyal custometrs as they drive maximum revenue

# Hence determining relevant customer metrics
# We will consider only the customers that have tenure between 1 month to 4 month period to remove
# extreme loyalists which might skew the results
price_analysis <- cp %>% 
  filter(!is.na(Customer.ID) & Category != 'None') %>% 
  group_by(Customer.ID) %>% 
  summarise(first_purchase = min(Date_time),
            last_purchase = max(Date_time),
            distinct_trans = n_distinct(Date_time),
            lifetime_net = sum(Net.Sales),
            lifetime_purs = sum(Qty),
            lifetime_disc = sum(Discounts),
            most_pur_cat = man_mode(Category),
            most_pur_item = man_mode(Item),
            refunds = sum(Event.Type == 'Refund'),
            unique_items = n_distinct(Item)) %>% 
  mutate(tenure = as.Date(last_purchase,  '%Y-%m-%d')  - as.Date(first_purchase, '%Y-%m-%d'),
         avg_trans = lifetime_net / distinct_trans) %>% 
  filter(tenure >= 30 & tenure <= 120 & !most_pur_cat %in% c('Beers', 'Cereal', 'Extras'))

# We observe that coffee is highest preferred product among these customers
ggplot(price_analysis, aes(x = most_pur_cat)) +
  geom_histogram(stat = 'count', fill = 'maroon', color = 'yellow') +
  ggtitle('Preferred categoris among revisiting customers') + xlab('Category') + ylab('ANo. of customers')

# Getting product prices in different periods
prod_prices <- cp %>% group_by(Category, Date_time) %>% summarise(old_price = mean(price),
                                                                  new_price = mean(price))

# Generating price for product when customer first visited and last visited the store
price_analysis <- merge(price_analysis, prod_prices[,c('Category', 'Date_time', 'old_price')],
                        by.x = c('first_purchase', 'most_pur_cat'), by.y = c('Date_time', 'Category'), all.x = TRUE)
price_analysis <- merge(price_analysis, prod_prices[,c('Category', 'Date_time', 'new_price')],
                        by.x = c('last_purchase', 'most_pur_cat'), by.y = c('Date_time', 'Category'), all.x = TRUE)


# Replacing NAs wth average price category
avg_lt_price <- cp %>% group_by(Category) %>% summarise(avg_lt = mean(price))
price_analysis <- merge(price_analysis, avg_lt_price, by.x = 'most_pur_cat', by.y = 'Category')
price_analysis$old_price <- ifelse(is.na(price_analysis$old_price), price_analysis$avg_lt, price_analysis$old_price)
price_analysis$new_price <- ifelse(is.na(price_analysis$new_price), price_analysis$avg_lt, price_analysis$new_price)

# Calculating price change
price_analysis$price_change <- price_analysis$new_price - price_analysis$old_price
price_analysis$tenure <- as.numeric(price_analysis$tenure)

ggplot(price_analysis, aes(x = price_change, y = tenure)) +
  geom_point(color = 'maroon') +
  geom_smooth(formula = y ~ x, method = 'lm', color = 'yellow') +
  facet_wrap(~ most_pur_cat)+
  ggtitle('Tenure against price change') + xlab('Price change') + ylab('Tenure')

# Based on the graph, we can conclude that Beers and Cereal could be disregarded 

# A mixed model was implmented to determine impact of price change based on category
# At the same time, we also wanted to observe the effect of lifetime unque items purchased on
# the tenure of the customer
price_rec <- lmer(tenure ~ unique_items + (1 + price_change | most_pur_cat),
                  data = price_analysis, control = lmerControl(optimizer ="Nelder_Mead"))
summary(price_rec)
coef(price_rec)

price_analysis$preds <- predict(price_rec, data=price_analysis)

price_analysis$ape <- abs(price_analysis$preds - price_analysis$tenure) / price_analysis$tenure
mean(price_analysis$ape)
