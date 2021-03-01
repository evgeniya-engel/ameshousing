library(mice)
library(VIM)
library(dplyr)
library(ggplot2)

df = read.csv('train.csv')
VIM::aggr(df[, 1:20])
missing = mice::md.pattern(df, rotate.names = TRUE)
n_rows = nrow(df)
x = subset(tail(missing, 1) != 0, select = -c(82)) #mask by non null values
missingness = rbind(tail(missing[, x], 1), tail(missing[, x], 1) * 100 /n_rows) # abs num and % of missing values

df %>% filter(is.na(FireplaceQu) == TRUE)
colnames(missingness)

lf = df %>% filter(is.na(Fence) == TRUE)
x = rbind(colnames(df[2:21], df[22:41], df[42:61]), df[62:81])

# x1 = colnames(df[2:21])
# x2 = colnames(df[22:41])
# x3 = colnames(df[42:61])
# x4 = colnames(df[62:81])
# 
# y = rbind(x1, x2, x3, x4)     
# write.csv(y, 'rows.csv')

jane = df[42:61]

df %>% ggplot() + geom_histogram(aes(x = SalePrice), stat='bin', bins = 40, col = "black", fill = "skyblue") +
  theme_bw() + scale_x_continuous(labels = scales::dollar)

df %>% ggplot() + geom_histogram(aes(x = GrLivArea), stat='bin', bins = 40, col = "black", fill = "skyblue") +
  theme_bw() + facet_wrap(~ Neighborhood)

df %>% ggplot() + geom_histogram(aes(x = log(SalePrice)), stat='bin', bins = 40, col = "black", fill = "skyblue") +
  theme_bw()
### GrLivingArea by Neighborhood
df %>% ggplot() + geom_density(aes(x = GrLivArea, col = Neighborhood, fill = Neighborhood), alpha = 0.1) +
  theme_bw() + facet_wrap(~ Neighborhood)
df %>% ggplot() + geom_histogram(aes(x = GrLivArea, fill = df$Neighborhood), stat='bin', bins = 40, position = 'dodge') +
  theme_bw()

### YearBuilt by Neighborhood
df %>% ggplot() + geom_density(aes(x = YearBuilt, col = Neighborhood, fill = Neighborhood), alpha = 0.1) +
  theme_bw() + facet_wrap(~ Neighborhood)

### AGE vs Year Remodelled
df %>% ggplot() + geom_point(aes(x = 2021 - YearBuilt, y = YearRemodAdd, col = OverallCond))
### Year built vs Overall Cond
df %>% ggplot() + geom_point(aes(x = OverallCond, y = YearBuilt))



df %>% ggplot() + geom_point(aes(x = GrLivArea, y = SalePrice, col = Neighborhood))  +
  stat_smooth(model = lm, aes(x = GrLivArea, y = SalePrice), colour="red") +
  scale_y_continuous(labels = scales::dollar)#+ 
  facet_wrap(~ Neighborhood)

new_order = with(df, reorder(Neighborhood, SalePrice, median , na.rm=T))
df$Neighborhood = factor(df$Neighborhood, levels = levels(new_order))
df %>% ggplot() + geom_boxplot(aes(y = Neighborhood, x = SalePrice, col = Neighborhood)) + theme_bw() +
  scale_x_continuous(labels = scales::dollar)

################################
### CATEGORICAL DISTRIBUTION ###
################################

df %>% group_by(HouseStyle) %>% summarize(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  mutate(HouseStyle = factor(HouseStyle, levels = (HouseStyle))) %>%
  ggplot() + geom_bar(aes(x = HouseStyle, y = Count), stat = 'identity', col = "black", fill = "skyblue") + theme_bw()

df %>% group_by(MSZoning) %>% summarize(Count = n()) %>% 
  arrange(desc(MSZoning)) %>% 
  mutate(HouseStyle = factor(MSZoning, levels = (MSZoning))) %>%
  ggplot() + geom_bar(aes(x = MSZoning, y = Count), stat = 'identity', col = "black", fill = "skyblue") + theme_bw()

df %>% group_by(Electrical) %>% summarize(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  mutate(Electrical = factor(Electrical, levels = (Electrical))) %>%
  ggplot() + geom_bar(aes(x = Electrical, y = Count), stat = 'identity', col = "black", fill = "skyblue") + theme_bw()

df %>% group_by(KitchenQual) %>% summarize(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  mutate(KitchenQual = factor(KitchenQual, levels = (KitchenQual))) %>%
  ggplot() + geom_bar(aes(x = KitchenQual, y = Count), stat = 'identity', col = "black", fill = "skyblue") + theme_bw()

df %>% group_by(Functional) %>% summarize(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  mutate(Functional = factor(Functional, levels = (Functional))) %>%
  ggplot() + geom_bar(aes(x = Functional, y = Count), stat = 'identity', col = "black", fill = "skyblue") + theme_bw()

df %>% group_by(FireplaceQu) %>% summarize(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  mutate(FireplaceQu = factor(FireplaceQu, levels = (FireplaceQu))) %>%
  ggplot() + geom_bar(aes(x = FireplaceQu, y = Count), stat = 'identity', col = "black", fill = "skyblue") + theme_bw()

df %>% group_by(GarageFinish) %>% summarize(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  mutate(GarageFinish = factor(GarageFinish, levels = (GarageFinish))) %>%
  ggplot() + geom_bar(aes(x = GarageFinish, y = Count), stat = 'identity', col = "black", fill = "skyblue") + theme_bw()

# catVsSales = function(dataframe, cname){
#   dataframe = dataframe %>% group_by(dataframe$cname) %>% summarize(Count = n()) %>% 
#     arrange(desc(Count)) %>% 
#     mutate(cname = factor(cname, levels = (cname)))
#     ggplot() + geom_bar(aes(x = dataframe$cname, y = dataframe$Count), stat = 'identity', col = "black", fill = "skyblue") + theme_bw()
# }
# 
# catVsSales(df, 'Electrical')

##################################
### CATEGORICAL VS SALES PRICE ###
##################################

# catVsSales = function(dataframe, cname){
#   new_order = with(dataframe, reorder(dataframe$cname, SalePrice, median , na.rm=T))
#   dataframe$cname = factor(dataframe$cname, levels = levels(new_order))
#   ggplot(dataframe) + geom_boxplot(aes(x = dataframe$cname, y = dataframe$SalePrice, col = dataframe$cname)) + theme_bw() +
#     scale_y_continuous(labels = scales::dollar) 
# }
# 
# catVsSales(df, 'Electrical')

new_order = with(df, reorder(Electrical, SalePrice, median , na.rm=T))
df$Electrical = factor(df$Electrical, levels = levels(new_order))
df %>% ggplot() + geom_boxplot(aes(x = Electrical, y = SalePrice, col = Electrical)) + theme_bw() +
  scale_y_continuous(labels = scales::dollar)

new_order = with(df, reorder(KitchenQual, SalePrice, median , na.rm=T))
df$KitchenQual = factor(df$KitchenQual, levels = levels(new_order))
df %>% ggplot() + geom_boxplot(aes(x = KitchenQual, y = SalePrice, col = KitchenQual)) + theme_bw() +
  scale_y_continuous(labels = scales::dollar)

new_order = with(df, reorder(Functional, SalePrice, median , na.rm=T))
df$Functional = factor(df$Functional, levels = levels(new_order))
df %>% ggplot() + geom_boxplot(aes(x = Functional, y = SalePrice, col = Functional)) + theme_bw() +
  scale_y_continuous(labels = scales::dollar)

new_order = with(df, reorder(FireplaceQu, SalePrice, median , na.rm=T))
df$FireplaceQu = factor(df$FireplaceQu, levels = levels(new_order))
df %>% ggplot() + geom_boxplot(aes(x = FireplaceQu, y = SalePrice, col = FireplaceQu)) + theme_bw() +
  scale_y_continuous(labels = scales::dollar)

new_order = with(df, reorder(GarageFinish, SalePrice, median , na.rm=T))
df$GarageFinish = factor(df$GarageFinish, levels = levels(new_order))
df %>% ggplot() + geom_boxplot(aes(x = GarageFinish, y = SalePrice, col = GarageFinish)) + theme_bw() +
  scale_y_continuous(labels = scales::dollar)

##############################
### DISCRETE VS SALE PRICE ###
##############################
new_order = with(df, reorder(TotRmsAbvGrd, SalePrice, median , na.rm=T))
df$TotRmsAbvGrd = factor(df$TotRmsAbvGrd, levels = levels(new_order))
df %>% ggplot() + geom_boxplot(aes(x = TotRmsAbvGrd, y = SalePrice, col = TotRmsAbvGrd)) + theme_bw() +
  scale_y_continuous(labels = scales::dollar)

new_order = with(df, reorder(BedroomAbvGr, SalePrice, median , na.rm=T))
df$BedroomAbvGr = factor(df$BedroomAbvGr, levels = levels(new_order))
df %>% ggplot() + geom_boxplot(aes(x = BedroomAbvGr, y = SalePrice, col = BedroomAbvGr)) + theme_bw() +
  scale_y_continuous(labels = scales::dollar)

new_order = with(df, reorder(Fireplaces, SalePrice, median , na.rm=T))
df$Fireplaces = factor(df$Fireplaces, levels = levels(new_order))
df %>% ggplot() + geom_boxplot(aes(x = Fireplaces, y = SalePrice, col = Fireplaces)) + theme_bw() +
  scale_y_continuous(labels = scales::dollar)

#TotalSF vs total rooms
df %>% mutate(TotalSF = TotalBsmtSF +X1stFlrSF+X2ndFlrSF) %>%
  ggplot() + geom_boxplot(aes(x = TotRmsAbvGrd, y = TotalSF, col = TotRmsAbvGrd)) + theme_bw()

df %>% ggplot() + geom_boxplot(aes(x = TotRmsAbvGrd, y = GrLivArea, col = TotRmsAbvGrd)) + theme_bw()





x = df %>% summarize(unique(MSZoning))


