acc_error<- function(actual,pred){
  RMSE= sqrt(mean((actual-pred)^2))
  vec=c(RMSE) 
  names(vec)= c("RMSE")
  return(vec)
}


planning = read.csv('1_packaging_planning_data.csv')
attach(planning)

library(dplyr)

# Remove numerical values = 0
planning = filter(planning, product_x_dim != 0 & packaging_x_dim != 0 & load_unit_x_dim != 0 & product_y_dim != 0 &
                    product_zdim != 0 & packaging_y_dim != 0 & packaging_z_dim != 0 & load_unit_y_dim != 0 , load_unit_z_dim != 0,
                  packaging_weight != 0, product_weight != 0)
attach(planning)


planning$packaging_volume = as.numeric(packaging_x_dim) * as.numeric(packaging_y_dim) * as.numeric(packaging_z_dim)
planning$load_unit_volume = as.numeric(load_unit_x_dim) * as.numeric(load_unit_y_dim) * as.numeric(load_unit_z_dim)

colnames(planning)
attach(planning)



# Remove categorical dependent variables
loadUnit = subset(planning, select = -c(load_unit_is_special, load_unit_is_oneway))
attach(loadUnit)

# Remove other numerical dependent variables and product variables
x_dim_loadUnit = subset(loadUnit, select = -c(load_unit_x_dim, load_unit_y_dim, load_unit_z_dim, load_unit_weight,
                                              load_unit_volume, product_kogr_number, product_quality_index, product_y_dim,
                                              product_generic_family_name, product_module_number, product_supplier_number,
                                              product_zdim, product_is_dangerous_good, product_name, product_weight,
                                              product_is_esp, product_number, product_x_dim, packagings_per_load_unit))
attach(x_dim_loadUnit)

x_dim_loadUnit["packaging_x_dim"] = log(packaging_x_dim)
x_dim_loadUnit["packaging_y_dim"] = log(packaging_y_dim)
x_dim_loadUnit["packaging_z_dim"] = log(packaging_z_dim)
x_dim_loadUnit["packaging_volume"] = log(packaging_volume)
x_dim_loadUnit["packaging_load_capacity"] = log(packaging_load_capacity)
x_dim_loadUnit["packaging_weight"] = log(packaging_weight)
x_dim_loadUnit["products_per_packaging"] = log(products_per_packaging)
x_dim_loadUnit["packagings_per_load_unit"] = log(packagings_per_load_unit)

attach(x_dim_loadUnit)

colnames(x_dim_loadUnit)
# Association between load_unit_y_dim & other qualitative variables
par(mfrow=c(2,2))
boxplot(packagings_per_load_unit~packaging_raw_material_name, col="cadetblue4")
boxplot(packagings_per_load_unit~packaging_is_oneway, col="cadetblue4")
boxplot(packagings_per_load_unit~packaging_is_special, col="cadetblue4")

effect_rawMaterial = aov(packagings_per_load_unit~packaging_raw_material_name)
summary(effect_rawMaterial)
effect_packagingSpecial = aov(packagings_per_load_unit~packaging_is_special)
summary(effect_packagingSpecial)
effect_packagingOneWay = aov(packagings_per_load_unit~packaging_is_oneway)
summary(effect_packagingOneWay)


# Association between packagings_per_load_unit & other quantitative variables

num_var = x_dim_loadUnit[, c("packaging_z_dim", "packaging_volume", "packaging_weight", "packaging_x_dim", "packaging_load_capacity",
                             "packaging_y_dim", "products_per_packaging")]
cor(packagings_per_load_unit, num_var)

par(mfrow=c(3,3))
plot(packagings_per_load_unit~packaging_z_dim, col = 'cadetblue4', main = 'Scatterplot for packagings_per_load_unit and packaging_z_dim')
abline(lm(packagings_per_load_unit~packaging_z_dim), col = 'red')

plot(packagings_per_load_unit~products_per_packaging, col = 'cadetblue4', main = 'Scatterplot for packagings_per_load_unit and products_per_packaging')
abline(lm(packagings_per_load_unit~products_per_packaging), col = 'red')

plot(packagings_per_load_unit~packaging_volume, col = 'cadetblue4', main = 'Scatterplot for packagings_per_load_unit and packaging_volume')
abline(lm(packagings_per_load_unit~packaging_volume), col = 'red')

plot(packagings_per_load_unit~packaging_weight, col = 'cadetblue4', main = 'Scatterplot for packagings_per_load_unit and packaging_weight')
abline(lm(packagings_per_load_unit~packaging_weight), col = 'red')

plot(packagings_per_load_unit~packaging_x_dim, col = 'cadetblue4', main = 'Scatterplot for packagings_per_load_unit and packaging_x_dim')
abline(lm(packagings_per_load_unit~packaging_x_dim), col = 'red')

plot(packagings_per_load_unit~packaging_load_capacity, col = 'cadetblue4', main = 'Scatterplot for packagings_per_load_unit and packaging_load_capacity')
abline(lm(packagings_per_load_unit~packaging_load_capacity), col = 'red')

plot(packagings_per_load_unit~packaging_y_dim, col = 'cadetblue4', main = 'Scatterplot for packagings_per_load_unit and packaging_y_dim')
abline(lm(packagings_per_load_unit~packaging_y_dim), col = 'red')

# LinearModel 1 with all predictors

attach(x_dim_loadUnit)

sum(is.na(loadUnit))

set.seed(100)
split = sample(1:2, nrow(x_dim_loadUnit), replace = TRUE, prob = c(0.7, 0.3))
train = x_dim_loadUnit[split ==1, ]
val = x_dim_loadUnit[split == 2,]

LinearModel1 = lm(packagings_per_load_unit~., data = train)
summary(LinearModel1)
pred_linearModel1 = predict(LinearModel1, val)
perform_linearModel1 = acc_error(val$packagings_per_load_unit, pred_linearModel1)
perform_linearModel1

# LinearModel2 by removing insignificant variables
# Remove insignificant variables
attach(loadUnit)
x_dim_loadUnit = subset(loadUnit, select = -c(load_unit_x_dim, load_unit_y_dim, load_unit_z_dim, load_unit_weight,
                                              load_unit_volume, product_kogr_number, product_quality_index, product_y_dim,
                                              product_generic_family_name, product_module_number, product_supplier_number,
                                              product_zdim, product_is_dangerous_good, product_name, product_weight,
                                              product_is_esp, product_number, product_x_dim, packagings_per_load_unit))
attach(x_dim_loadUnit)

x_dim_loadUnit["packaging_x_dim"] = log(packaging_x_dim)
x_dim_loadUnit["packaging_y_dim"] = log(packaging_y_dim)
x_dim_loadUnit["packaging_z_dim"] = log(packaging_z_dim)
x_dim_loadUnit["packaging_volume"] = log(packaging_volume)
x_dim_loadUnit["packaging_load_capacity"] = log(packaging_load_capacity)
x_dim_loadUnit["packaging_weight"] = log(packaging_weight)
x_dim_loadUnit["products_per_packaging"] = log(products_per_packaging)
x_dim_loadUnit["packagings_per_load_unit"] = log(packagings_per_load_unit)

attach(x_dim_loadUnit)

x_dim_loadUnit = subset(x_dim_loadUnit, select = -c(packaging_raw_material_name))

sum(is.na(loadUnit))
set.seed(100)
split = sample(1:2, nrow(x_dim_loadUnit), replace = TRUE, prob = c(0.7, 0.3))
train = x_dim_loadUnit[split ==1, ]
val = x_dim_loadUnit[split == 2,]

LinearModel2 = lm(packagings_per_load_unit~., data = train)
summary(LinearModel2)
pred_linearModel2 = predict(LinearModel2, val)
perform_linearModel2 = acc_error(val$packagings_per_load_unit, pred_linearModel2)
perform_linearModel2


# LinearModel3 by removing packaging_raw_material_name
# Remove insignificant variable packaging_raw_material_name
attach(loadUnit)
x_dim_loadUnit = subset(loadUnit, select = -c(load_unit_x_dim, load_unit_y_dim, load_unit_z_dim, load_unit_weight,
                                              load_unit_volume, product_kogr_number, product_quality_index, product_y_dim,
                                              product_generic_family_name, product_module_number, product_supplier_number,
                                              product_zdim, product_is_dangerous_good, product_name, product_weight,
                                              product_is_esp, product_number, product_x_dim, packagings_per_load_unit))
attach(x_dim_loadUnit)

x_dim_loadUnit["packaging_x_dim"] = log(packaging_x_dim)
x_dim_loadUnit["packaging_y_dim"] = log(packaging_y_dim)
x_dim_loadUnit["packaging_z_dim"] = log(packaging_z_dim)
x_dim_loadUnit["packaging_volume"] = log(packaging_volume)
x_dim_loadUnit["packaging_load_capacity"] = log(packaging_load_capacity)
x_dim_loadUnit["packaging_weight"] = log(packaging_weight)
x_dim_loadUnit["products_per_packaging"] = log(products_per_packaging)
x_dim_loadUnit["packagings_per_load_unit"] = log(packagings_per_load_unit)

attach(x_dim_loadUnit)
x_dim_loadUnit = subset(x_dim_loadUnit, select = -c(packaging_raw_material_name))
sum(is.na(loadUnit))
set.seed(100)
split = sample(1:2, nrow(x_dim_loadUnit), replace = TRUE, prob = c(0.7, 0.3))
train = x_dim_loadUnit[split ==1, ]
val = x_dim_loadUnit[split == 2,]

LinearModel3 = lm(packagings_per_load_unit~., data = train)
summary(LinearModel3)
pred_linearModel3 = predict(LinearModel3, val)
perform_linearModel3 = acc_error(val$packagings_per_load_unit, pred_linearModel3)
perform_linearModel3


# LinearModel4 by removing categorical variables
# Remove insignificant variables packaging_raw_material_name
attach(loadUnit)
x_dim_loadUnit = subset(loadUnit, select = -c(load_unit_x_dim, load_unit_y_dim, load_unit_z_dim, load_unit_weight,
                                              load_unit_volume, product_kogr_number, product_quality_index, product_y_dim,
                                              product_generic_family_name, product_module_number, product_supplier_number,
                                              product_zdim, product_is_dangerous_good, product_name, product_weight,
                                              product_is_esp, product_number, product_x_dim, packagings_per_load_unit))
attach(x_dim_loadUnit)

x_dim_loadUnit["packaging_x_dim"] = log(packaging_x_dim)
x_dim_loadUnit["packaging_y_dim"] = log(packaging_y_dim)
x_dim_loadUnit["packaging_z_dim"] = log(packaging_z_dim)
x_dim_loadUnit["packaging_volume"] = log(packaging_volume)
x_dim_loadUnit["packaging_load_capacity"] = log(packaging_load_capacity)
x_dim_loadUnit["packaging_weight"] = log(packaging_weight)
x_dim_loadUnit["products_per_packaging"] = log(products_per_packaging)
x_dim_loadUnit["packagings_per_load_unit"] = log(packagings_per_load_unit)

attach(x_dim_loadUnit)
x_dim_loadUnit = subset(x_dim_loadUnit, select = -c(packaging_raw_material_name, packaging_is_oneway, packaging_is_special))
sum(is.na(loadUnit))
set.seed(100)
split = sample(1:2, nrow(x_dim_loadUnit), replace = TRUE, prob = c(0.7, 0.3))
train = x_dim_loadUnit[split ==1, ]
val = x_dim_loadUnit[split == 2,]

LinearModel4 = lm(packagings_per_load_unit~., data = train)
summary(LinearModel4)
pred_linearModel4 = predict(LinearModel4, val)
perform_linearModel4 = acc_error(val$packagings_per_load_unit, pred_linearModel4)
perform_linearModel4

library(rpart)
library(rpart.plot)

# TreeModel1 with all predictors
# Remove other numerical dependent variables and product variables
attach(loadUnit)
x_dim_loadUnit = subset(loadUnit, select = -c(load_unit_x_dim, load_unit_y_dim, load_unit_z_dim, load_unit_weight,
                                              load_unit_volume, product_kogr_number, product_quality_index, product_y_dim,
                                              product_generic_family_name, product_module_number, product_supplier_number,
                                              product_zdim, product_is_dangerous_good, product_name, product_weight,
                                              product_is_esp, product_number, product_x_dim, packagings_per_load_unit))
attach(x_dim_loadUnit)

x_dim_loadUnit["packaging_x_dim"] = log(packaging_x_dim)
x_dim_loadUnit["packaging_y_dim"] = log(packaging_y_dim)
x_dim_loadUnit["packaging_z_dim"] = log(packaging_z_dim)
x_dim_loadUnit["packaging_volume"] = log(packaging_volume)
x_dim_loadUnit["packaging_load_capacity"] = log(packaging_load_capacity)
x_dim_loadUnit["packaging_weight"] = log(packaging_weight)
x_dim_loadUnit["products_per_packaging"] = log(products_per_packaging)
x_dim_loadUnit["packagings_per_load_unit"] = log(packagings_per_load_unit)

attach(x_dim_loadUnit)
colnames(x_dim_loadUnit)
sum(is.na(loadUnit))
set.seed(100)
split = sample(1:2, nrow(x_dim_loadUnit), replace = TRUE, prob = c(0.7, 0.3))
train = x_dim_loadUnit[split ==1, ]
val = x_dim_loadUnit[split == 2,]

treeModel1 = rpart(packagings_per_load_unit~. , data = train, control = rpart.control(cp = 0.0001)) 
bestcp=treeModel1$cptable[which.min(treeModel1$cptable[,"xerror"]),"CP"]

treeModel1_pruned=prune(treeModel1, cp = bestcp)
pred_treeModel1_pruned = predict(treeModel1_pruned, val)
perform_treeModel1_pruned = acc_error(val$packagings_per_load_unit, pred_treeModel1_pruned)
perform_treeModel1_pruned
printcp(treeModel1_pruned)

treeModel2_pruned=prune(treeModel1_pruned, cp = 0.00022184)
pred_treeModel2_pruned = predict(treeModel2_pruned, val)
perform_treeModel2_pruned = acc_error(val$packagings_per_load_unit, pred_treeModel2_pruned)
perform_treeModel2_pruned
printcp(treeModel2_pruned)

library(randomForest)
randomTree1 = randomForest(packagings_per_load_unit~., data = train, mtry = 3, ntree=155, importance = TRUE, na.action = na.omit)
pred_randomTree1 = predict(randomTree1, val)
perform_randomTree1 = acc_error(val$packagings_per_load_unit, pred_randomTree1)
perform_randomTree1
print(randomTree1)

#Pick best model
perf_comp_linear = rbind(perform_linearModel1, perform_linearModel2, perform_linearModel4)
numb_predictors = c(26,8,6)
cbind(perf_comp_linear, numb_predictors)

perf_comp_tree = rbind(perform_treeModel1_pruned,perform_treeModel2_pruned)
numb_splits = c(52,40)
cp = c(bestcp,0.00022184)
cbind(perf_comp_tree, numb_splits,cp)

perf_comp_randomTree = rbind(perform_randomTree1)
numb_trees = c(155)
cbind(perf_comp_randomTree, numb_trees)

mean(packagings_per_load_unit)
min(packagings_per_load_unit)
max(packagings_per_load_unit)

par(mfrow=c(1,1))

rpart.plot(treeModel2_pruned,type=4,extra="auto",
           main="Packagings per Load Unit (in numerical)")

par(mfrow=c(3,4))

library(visreg)
visreg(treeModel2_pruned)

