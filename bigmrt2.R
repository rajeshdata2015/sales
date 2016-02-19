train = read.csv("train.csv", header = TRUE)
test = read.csv("test.csv", header = TRUE)

train$tale = 1
test$tale = 0
test$Item_Outlet_Sales = NA
dt = rbind(train,test)
str(dt)

# Data Exploration

# 1. Identify Missing Values

sapply(dt,function(df){sum(is.na(df))})
# Item_Weight = 2439
# Item_Outlet_Sales = 5681

# 2. Detailed summary of attributes
sapply(dt,function(df){summary(df)})

# Impute mv in Item_Weight by its mean
dt$Item_Weight[is.na(dt$Item_Weight) == TRUE] = mean(dt$Item_Weight,na.rm = TRUE)

# Replace LF, low fat to Low Fat in Item_Fat_Content
str(dt$Item_Fat_Content)
dt$Item_Fat_Content=casefold(dt$Item_Fat_Content,upper = TRUE)
dt$Item_Fat_Content=sub("LF","LOW_FAT",dt$Item_Fat_Content)
dt$Item_Fat_Content=sub("LOW FAT","LOW_FAT",dt$Item_Fat_Content)
dt$Item_Fat_Content=sub("REGULAR","REG",dt$Item_Fat_Content)
dt$Item_Fat_Content[dt$Item_Type == "Health and Hygiene"] = "Non_Edible"
dt$Item_Fat_Content[dt$Item_Type == "Household"] = "Non_Edible"
dt$Item_Fat_Content[dt$Item_Type == "Others"] = "Non_Edible"

#Item_Type
# combine food items into "food"=Breads+Breakfast+Dairy+Frozenfoos+F&veg+Meat+Seafood+Snackfoosa+starchyfoods
#"drinks"=canned+harddrinks+softdrinks
#others = health+household
dt$Item_Type = as.character(dt$Item_Type)
dt$Item_Type[dt$Item_Type == "Dairy"] = dt$Item_Type[dt$Item_Type == "Meat"] = dt$Item_Type[dt$Item_Type == "Fruits and Vegetables"] = dt$Item_Type[dt$Item_Type == "Breads"] = dt$Item_Type[dt$Item_Type == "Breakfast"] = dt$Item_Type[dt$Item_Type == "Frozen Foods"] = "Food"
dt$Item_Type[dt$Item_Type == "Snack Foods"] = dt$Item_Type[dt$Item_Type == "Starchy Foods"] = dt$Item_Type[dt$Item_Type == "Seafood"] = "Food"
dt$Item_Type[dt$Item_Type == "Soft Drinks"] = dt$Item_Type[dt$Item_Type == "Hard Drinks"] = dt$Item_Type[dt$Item_Type == "Canned"] = "Drinks"
dt$Item_Type = as.factor(dt$Item_Type)

#Item_Visibility cant be 0
boxplot(dt$Item_Visibility)
dt$Item_Visibility[dt$Item_Visibility == 0] = mean(dt$Item_Visibility,na.rm = TRUE)
#imputing Item_visi 0 with mean of item_type
library(sqldf)
vis_itemid = sqldf("SELECT Item_Identifier, Avg(Item_Visibility) AS Itemid_Vis_Avge  FROM dt GROUP BY Item_Identifier")
vis_Outid = sqldf("SELECT Outlet_Identifier, Avg(Item_Visibility) AS outlet_Vis_Avge  FROM dt GROUP BY Outlet_Identifier")
vis_item = sqldf("SELECT Item_Type, Avg(Item_Visibility) AS Item_Vis_Avge  FROM dt GROUP BY Item_Type")

t_1 = merge(dt,vis_item,by.x = "Item_Type",by.y = "Item_Type")
t_1 = merge(t_1,vis_Outid,by.x = "Outlet_Identifier",by.y = "Outlet_Identifier")
t_1 = merge(t_1,vis_itemid,by.x = "Item_Identifier",by.y = "Item_Identifier")

#Outlet_Establishment_Year  into new variable
t_1$Outlet_Establishment_Year_dur = 2013 - t_1$Outlet_Establishment_Year
#dummy variables for visibility
t_1$itmtype_visi_dummy = t_1$Item_Visibility/t_1$Item_Vis_Avge
#View(t_1)
t_1$outid_visi_dummy = t_1$Item_Visibility/t_1$outlet_Vis_Avge
t_1$itmid_vis_dummy = t_1$Item_Visibility/t_1$Itemid_Vis_Avge
dt$Outlet_yrs_old = 2013 - dt$Outlet_Establishment_Year

for(t in unique(t_1$Item_Fat_Content)) {
  t_1[paste("Fat_Dummy",t,sep="")] <- ifelse(t_1$Item_Fat_Content==t,1,0)
}

for(t in unique(t_1$Outlet_Size)) {
  t_1[paste("outlet_sze",t,sep="")] <- ifelse(t_1$Outlet_Size==t,1,0)
}


for(t in unique(t_1$Item_Type)) {
  t_1[paste("Item_Typ",t,sep="")] <- ifelse(t_1$Item_Type==t,1,0)
}


for(t in unique(t_1$Outlet_Location_Type)) {
  t_1[paste("Outlet_Loc_tpe",t,sep="")] <- ifelse(t_1$Outlet_Location_Type==t,1,0)
}

t_2 = t_1[,names(t_1)%in% c("Item_Identifier","Item_Weight","Item_MRP","Outlet_Type","itmid_vis_dummy","outid_visi_dummy","Fat_DummyLOW FAT"
                            ,"Fat_DummyREG","Fat_DummyNon Edible","Outlet_Establishment_Year_dur","outlet_szeMedium","outlet_szeHigh"
                            ,"outlet_szeSmall","tale","Item_Outlet_Sales","Outlet_Identifier","Item_Type","Outlet_Location_Type")]


train_f = t_2[t_2$tale == 1,]
test_f = t_2[t_2$tale == 0,]
#train_f$Outlet_Identifier = NULL
set.seed(1234)
train_f$random <- runif(nrow(train_f))
train_70 <- train_f[train_f$random <= 0.7,] 
train_30 <- train_f[train_f$random > 0.7,] 

train_70$Item_Identifier=NULL
train_70$random=NULL
train_70$tale = NULL
test_f$random = NULL
train_f$random =NULL
train_f$tale = NULL

library(randomForest)
r <- randomForest(train_70$Item_Outlet_Sales~.,data=train_70, maxnodes=500)
summary(r)
pred = predict(r,train_30)

sqrt(mean((pred-train_30$Item_Outlet_Sales)^2,na.rm = TRUE)/nrow(train_30))

train_f$Item_Identifier=NULL
r <- randomForest(train_f$Item_Outlet_Sales~.,data=train_f, maxnodes=500)
summary(r)
plot(r$importance)
pred = predict(r,test_f)

test_f$Item_Outlet_Sales = pred


SampleSubmission <- test_f[,c("Item_Identifier","Outlet_Identifier","Item_Outlet_Sales")]
write.csv(SampleSubmission,"SampleSubmission.csv",row.names=FALSE)
