#Author: Will Godwin
#Purpose: Analytics for Cayenne
#Date: 10/01/2018

# clear memory
rm(list=ls())

#load packages
pack_lib <- "/Users/madelinecampbell/Desktop/wills_stuff/cayenne/packages"
.libPaths(pack_lib)
library(data.table); library(ggplot2)

#Set directories
in_dir <- "/Users/madelinecampbell/Desktop/wills_stuff/cayenne/data/"

#Read in accounts data
dt_accounts <- fread(paste0(in_dir, "Accounts.csv"))
setnames(dt_accounts, c("Promo Code / Referral Description", "(No column name)"), c("refer_descr", "relationship"))

#Read in orders data
dt_orders <- fread(paste0(in_dir, "Orders.csv"))
setnames(dt_orders, c("Promo Code / Referral Description", "(No column name)"), c("refer_descr", "relationship"))

#Clean dates
dt_orders[, OrderDate := as.Date(as.character(OrderDate), "%A, %B %d, %Y")]
dt_orders[, AccountCreatedDate := as.Date(as.character(AccountCreatedDate), "%A, %B %d, %Y")]

#subset to exclude family, founders, and friends
dt_orders_friends <- dt_orders[relationship == "friend" | relationship == "no relation"]
dt_orders_nofriends <- dt_orders[relationship == "no relation"]
dt_temp <- copy(dt_orders_nofriends)

####################################################################################
#What's the average time between account creation and first purchase?###############
####################################################################################
#Calculate time between account made and first purchase
dt_temp <- dt_temp[order(AccountId, AccountCreatedDate)]
dt_temp <- dt_temp[!duplicated(AccountId),]
dt_temp[, Purchase_days := as.numeric(OrderDate - AccountCreatedDate)]

#plot
ggplot(dt_temp, aes(x=Purchase_days)) + geom_histogram() +
  geom_vline(aes(xintercept=quantile(Purchase_days, .5)), color="blue", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=quantile(Purchase_days, .05)), color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=quantile(Purchase_days, .95)), color="red", linetype="dashed", size=1) +
  ylab("Frequency") + xlab("# Days between account creation and first purchase")

####################################################################################
#Best Performing Categories?########################################################
####################################################################################
dt_temp <- copy(dt_orders_friends)

dt_temp[, count := 1]
t2 <- dt_temp[, sum(count), by = "Product Category"]
setnames(t2, "V1", "Total_Sold")
t <- dt_temp[, sum(GrandTotal), by = "Product Category"]
setnames(t, "V1", "Total_Revenue")
t <- merge(t, t2, by = "Product Category")

dt_temp[, weekend := format(dt_temp$OrderDate, "%u") %in% c(6, 7)]
dt_temp[, weekend := ifelse(weekend == T, 1, 0)]
t2 <- dt_temp[, mean(weekend), by = "Product Category"]
setnames(t2, "V1", "Percent_on_Weekend")
t2[, Percent_on_Weekend := round(Percent_on_Weekend * 100, 1)]
t <- merge(t, t2, by = "Product Category")
t <- t[order(-Total_Revenue)]

#Read in views data
dt_shwviews <- fread(paste0(in_dir, "ShowcaseViews.csv"))
setnames(dt_shwviews, "(No column name)", "relationship")

#collapse orders data to unique row for each showcase view (create variable that counts number of purchases per view)

#how to define success:
  #mostly use: conversion rate = number of purchases / number of showcase views
