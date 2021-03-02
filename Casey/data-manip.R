housing = read.csv('./data/Ames_Housing_Price_Data.csv')
realestate = read.csv('./data/Ames_Real_Estate_Data.csv')
housing$X = NULL


#### data messing ####

linreg = lm(SalePrice ~ I(GrLivArea^2) + YearBuilt, data = housing)
summary(linreg) # r2 = .6554
plot(linreg)


df = data.frame(housing$YearBuilt, housing$GrLivArea, housing$YrSold)
cor(df)

library(dplyr)
housing = housing %>% 
  mutate(HouseStyle = as.factor(HouseStyle),
         BldgType = as.factor(BldgType))

summary(housing$BldgType)

library(ggplot2)
ggplot(data = housing) + geom_density(aes(x = SalePrice, fill = BldgType))

cor(housing, use = "complete.obs")

features = sapply(housing, class)
features[features == 'integer']

features = housing %>% 
  select(GrLivArea, SalePrice, OverallQual, YearBuilt, YearRemodAdd)
cor(features)

model = lm(SalePrice ~ ., data = features)
summary(model) #.7632
summary(linreg)
model = lm(SalePrice ~ log(GrLivArea), data = features)
summary(model)

model2 = lm(log(SalePrice) ~ GrLivArea, data = features)
summary(model2)
plot(model2)

model3 = lm(log(SalePrice) ~ GrLivArea + YearBuilt, data = features)
summary(model3) # .7129

ggplot(data = features, aes(y = log(SalePrice), x = GrLivArea)) + geom_point() + geom_smooth(method = "lm")
ggplot(data = features, aes(y = log(SalePrice), x = YearBuilt)) + geom_point() + geom_smooth(method = "lm")

#### EDA ####
mycols = housing[,24:43]
library(dplyr)
mycols = mycols %>% mutate(RoofStyle = as.factor(RoofStyle), RoofMatl = as.factor(RoofMatl),
                  Exterior1st = as.factor(Exterior1st), Exterior2nd = as.factor(Exterior2nd),
                  MasVnrType = as.factor(MasVnrType), ExterQual = as.factor(ExterQual),
                  ExterCond = as.factor(ExterCond), Foundation = as.factor(Foundation),
                  BsmtQual = as.factor(BsmtQual), BsmtCond = as.factor(BsmtCond),
                  BsmtExposure = as.factor(BsmtExposure), BsmtFinType1 = as.factor(BsmtFinType1),
                  BsmtFinType2 = as.factor(BsmtFinType2), Heating = as.factor(Heating),
                  HeatingQC = as.factor(HeatingQC))

summary(mycols$ExterQual)

numeric_cols = mycols %>% select (MasVnrArea,BsmtFinSF1,BsmtFinSF2, BsmtUnfSF, TotalBsmtSF)
cor(numeric_cols, use = "complete.obs")
# Basement SFs are multicollinear
cor(x = housing$SalePrice, y = numeric_cols, use = "complete.obs")
# TotalBsmtSf is highly correlated w/ SalePrice (0.65)
# MasVnrArea is also correlated with SalePrice (0.5)

 
#### New cols ####
df = housing %>% 
  select (SalePrice, Condition1, Condition2, BldgType, HouseStyle, YearBuilt, YearRemodAdd, 
          TotalBsmtSF, HeatingQC, CentralAir) %>%
  mutate(Condition1 = as.factor(Condition1), Condition2 = as.factor(Condition2),
         BldgType = as.factor(BldgType), HouseStyle = as.factor(HouseStyle),
         HeatingQC = as.factor(HeatingQC), CentralAir = as.factor(CentralAir))

summary(df)

# Central Air: (163 N, 2417 Y) - I would say this makes a difference
library(ggplot2)
ggplot(data = df) + geom_density(aes(x = SalePrice, color = CentralAir))
ggplot(data = df) + geom_boxplot(aes(y = SalePrice, color = CentralAir))

df = df %>% mutate(CentralAir = ifelse(CentralAir == 'Y', 1, 0))

# TotalBsmtSF
df = df %>% mutate(Basement = ifelse(TotalBsmtSF > 0, 1, 0))

# YearRemodAdd
df = df %>% mutate(Remodel = ifelse(YearBuilt == YearRemodAdd, 0, 1))

# HeatingQC
ggplot(data = df) + geom_boxplot(aes(y = SalePrice, color = HeatingQC))
ggplot(data = df) + geom_density(aes(x = SalePrice, color = HeatingQC))

# HouseStyle
summary(df$HouseStyle)
ggplot(data = df) + geom_boxplot(aes(y = SalePrice, color = HouseStyle))
df = df %>% mutate(Style = 
                     ifelse(HouseStyle %in% c("2Story","2.5Fin","2.5Unf"), "Two", "One"))
ggplot(data = df) + geom_boxplot(aes(y = SalePrice, color = Style))
ggplot(data = df) + geom_density(aes(y = SalePrice, color = Style))


# BldgType
summary(df$BldgType)
# 2FmCon & Duplex are both instances where you don't own the whole building
ggplot(data = df) + geom_boxplot(aes(y = SalePrice, fill = BldgType))
ggplot(data = df) + geom_density(aes(x = SalePrice, color = BldgType))

df = df %>% mutate(isSingleFam = ifelse(BldgType == "1Fam", "Y","N"))
ggplot(data = df) + geom_boxplot(aes(y = SalePrice, fill = isSingleFam))



# Condition1, Condition2,

conds = df %>% transmute(Condition1 = as.character(Condition1), 
                         Condition2 = as.character(Condition2), 
                         SalePrice=SalePrice)
conds = conds %>%  mutate(Cond1 = ifelse(conds$Condition1 %in% c("RRNn","RRNe","RRAe", "RRAn"), "Railroad",
                              ifelse(conds$Condition1 %in% c("PosN", "PosA"), "PositiveFeature",
                                     ifelse(conds$Condition1 %in% c('Artery','Feedr'), "AdjacentStreet",
                                            "Norm"))))
conds = conds %>% mutate(Cond2 = ifelse(conds$Condition2 %in% c("RRNn","RRNe","RRAe", "RRAn"), "Railroad",
                                 ifelse(conds$Condition2 %in% c("PosN", "PosA"), "PositiveFeature",
                                        ifelse(conds$Condition2 %in% c('Artery','Feedr'), "AdjacentStreet",
                                               "Norm"))))

ggplot(data = conds) + geom_boxplot(aes(y = SalePrice, fill = Cond1))
ggplot(data = conds) + geom_boxplot(aes(y = SalePrice, fill = Cond2))

conds = conds %>% mutate(Cond1 = as.factor(Cond1), Cond2 = as.factor(Cond2))





