library(tidyverse)
library(janitor)
library(reshape)


# Setting the directory---------------------------------------------------------

print(getwd())
setwd("D:/Projects/Marketing-Analytics")
print(getwd())
#-------------------------------------------------------------------------------


# reading csv-------------------------------------------------------------------

df = read.csv("Dataset/ifood_df.csv")

View(df)
#-------------------------------------------------------------------------------


# Data pipeline-----------------------------------------------------------------

data_clean = df %>%
  drop_na() %>%
  janitor::clean_names()

View(data_clean)
#-------------------------------------------------------------------------------


attach(data_clean)

# Normalizing data--------------------------------------------------------------

model1 = chisq.test(BBmisc::normalize(income, "range", c(1000, 2000)))

print(model1)

val1 = mean(model1$observed - model1$expected)

print(val1)


model2 = chisq.test(BBmisc::normalize(kidhome, "range", c(1000, 2000)))

print(model2)

val2 = mean(model2$observed - model2$expected)

print(val2)


model3 = chisq.test(BBmisc::normalize(teenhome, "range", c(1000, 2000)))

print(model3)

val3 = mean(model3$observed - model3$expected)

print(val3)


model4 = chisq.test(BBmisc::normalize(recency, "range", c(1000, 2000)))

print(model4)

val4 = mean(model4$observed - model4$expected)

print(val4)


model5 = chisq.test(BBmisc::normalize(mnt_fruits, "range", c(1000, 2000)))

print(model5)

val5 = mean(model5$observed - model5$expected)

print(val5)


model6 = chisq.test(BBmisc::normalize(mnt_meat_products, "range", c(1000, 2000)))

print(model6)

val6 = mean(model6$observed - model6$expected)

print(val6)


model7 = chisq.test(BBmisc::normalize(mnt_wines, "range", c(1000, 2000)))

print(model7)

val7 = mean(model7$observed - model7$expected)

print(val7)


model8 = chisq.test(BBmisc::normalize(mnt_fish_products, "range", c(1000, 2000)))

print(model8)

val8 = mean(model8$observed - model8$expected)

print(val8)


model9 = chisq.test(BBmisc::normalize(mnt_sweet_products, "range", c(1000, 2000)))

print(model9)

val9 = mean(model9$observed - model9$expected)

print(val9)


model10 = chisq.test(BBmisc::normalize(mnt_gold_prods, "range", c(1000, 2000)))

print(model10)

val10 = mean(model10$observed - model10$expected)

print(val10)


model11 = chisq.test(BBmisc::normalize(num_deals_purchases, "range", c(1000, 2000)))

print(model11)

val11 = mean(model11$observed - model11$expected)

print(val11)


model12 = chisq.test(BBmisc::normalize(num_web_purchases, "range", c(1000, 2000)))

print(model12)

val12 = mean(model12$observed - model12$expected)

print(val12)


model13 = chisq.test(BBmisc::normalize(num_catalog_purchases, "range", c(1000, 2000)))

print(model13)

val13 = mean(model13$observed - model13$expected)

print(val13)


model14 = chisq.test(BBmisc::normalize(num_store_purchases, "range", c(1000, 2000)))

print(model14)

val14 = mean(model14$observed - model14$expected)

print(val14)


model15 = chisq.test(BBmisc::normalize(num_web_visits_month, "range", c(1000, 2000)))

print(model15)

val15 = mean(model15$observed - model15$expected)

print(val15)


model16 = chisq.test(BBmisc::normalize(accepted_cmp1, "range", c(1000, 2000)))

print(model16)

val16 = mean(model16$observed - model16$expected)

print(val16)


model17 = chisq.test(BBmisc::normalize(accepted_cmp2, "range", c(1000, 2000)))

print(model17)

val17 = mean(model17$observed - model17$expected)

print(val17)


model18 = chisq.test(BBmisc::normalize(accepted_cmp3, "range", c(1000, 2000)))

print(model18)

val18 = mean(model18$observed - model18$expected)

print(val18)


model19 = chisq.test(BBmisc::normalize(accepted_cmp4, "range", c(1000, 2000)))

print(model19)

val19 = mean(model19$observed - model19$expected)

print(val19)


model20 = chisq.test(BBmisc::normalize(accepted_cmp5, "range", c(1000, 2000)))

print(model20)

val20 = mean(model20$observed - model20$expected)

print(val20)


model21 = chisq.test(BBmisc::normalize(complain, "range", c(1000, 2000)))

print(model21)

val21 = mean(model21$observed - model21$expected)

print(val21)


model22 = chisq.test(BBmisc::normalize(z_cost_contact, "range", c(1000, 2000)))

print(model22)

val22 = mean(model22$observed - model22$expected)

print(val22)


model23 = chisq.test(BBmisc::normalize(response, "range", c(1000, 2000)))

print(model23)

val23 = mean(model23$observed - model23$expected)

print(val23)


model24 = chisq.test(BBmisc::normalize(age, "range", c(1000, 2000)))

print(model24)

val24 = mean(model24$observed - model24$expected)

print(val24)


model25 = chisq.test(BBmisc::normalize(customer_days, "range", c(1000, 2000)))

print(model25)

val25 = mean(model25$observed - model25$expected)

print(val25)


model26 = chisq.test(BBmisc::normalize(marital_divorced, "range", c(1000, 2000)))

print(model26)

val26 = mean(model26$observed - model26$expected)

print(val26)


model27 = chisq.test(BBmisc::normalize(marital_married, "range", c(1000, 2000)))

print(model27)

val27 = mean(model27$observed - model27$expected)

print(val27)


model28 = chisq.test(BBmisc::normalize(marital_single, "range", c(1000, 2000)))

print(model28)

val28 = mean(model28$observed - model28$expected)

print(val28)


model29 = chisq.test(BBmisc::normalize(marital_together, "range", c(1000, 2000)))

print(model29)

val29 = mean(model29$observed - model29$expected)

print(val29)


model30 = chisq.test(BBmisc::normalize(marital_divorced, "range", c(1000, 2000)))

print(model30)

val30 = mean(model30$observed - model30$expected)

print(val30)


model31 = chisq.test(BBmisc::normalize(education_2n_cycle, "range", c(1000, 2000)))

print(model31)

val31 = mean(model31$observed - model31$expected)

print(val31)


model32 = chisq.test(BBmisc::normalize(education_basic, "range", c(1000, 2000)))

print(model32)

val32 = mean(model32$observed - model32$expected)

print(val32)


model33 = chisq.test(BBmisc::normalize(education_graduation, "range", c(1000, 2000)))

print(model33)

val33 = mean(model33$observed - model33$expected)

print(val33)


model34 = chisq.test(BBmisc::normalize(education_master, "range", c(1000, 2000)))

print(model34)

val34 = mean(model34$observed - model34$expected)

print(val34)


model35 = chisq.test(BBmisc::normalize(education_ph_d, "range", c(1000, 2000)))

print(model35)

val35 = mean(model35$observed - model35$expected)

print(val35)


model36 = chisq.test(BBmisc::normalize(mnt_total, "range", c(1000, 2000)))

print(model36)

val36 = mean(model36$observed - model36$expected)

print(val36)


model37 = chisq.test(BBmisc::normalize(mnt_regular_prods, "range", c(1000, 2000)))

print(model37)

val37 = mean(model37$observed - model37$expected)

print(val37)


model38 = chisq.test(BBmisc::normalize(accepted_cmp_overall, "range", c(1000, 2000)))

print(model38)

val38 = mean(model38$observed - model38$expected)

print(val38)


model39 = chisq.test(BBmisc::normalize(z_revenue, "range", c(1000, 2000)))

print(model39)

val39 = mean(model39$observed - model39$expected)

print(val39)
#-------------------------------------------------------------------------------


# plotting dependent and independent variables----------------------------------

Parameters = c("income", 
               "kidhome", 
               "teenhome",
               "recency",
               "mnt_fruits", 
               "mnt_meat_products",
               "mnt_wines",
               "mnt_fish_products",
               "mnt_sweet_products", 
               "mnt_gold_products", 
               "num_deals_purchases",
               "num_web_purchases",
               "num_catalog_purchases", 
               "num_store_purchases", 
               "num_web_vists_month",
               "accepted_cmp1",
               "accepted_cmp2",
               "accepted_cmp3",
               "accepted_cmp4",
               "accepted_cmp5",
               "complain",
               "z_cost_contact",
               "response",
               "age",
               "customer_days",
               "marital_divorced",
               "marital_married",
               "marital_single",
               "marital_together",
               "marital_widow",
               "education_2n_cycle",
               "education_basic",
               "education_graduation",
               "education_master",
               "education_phd",
               "mnt_total",
               "mnt_regular_prods",
               "accepted_cmp_overall",
               "z_revenue")

Difference = c(val1,
               val2,
               val3,
               val4,
               val5,
               val6,
               val7,
               val8,
               val9,
               val10,
               val11,
               val12,
               val13,
               val14,
               val15,
               val16,
               val17,
               val18,
               val19,
               val20,
               val21,
               val22,
               val23,
               val24,
               val25,
               val26,
               val27,
               val28,
               val29,
               val30,
               val31,
               val32,
               val33,
               val34,
               val35,
               val36,
               val37,
               val38,
               val39)

graph1 = data.frame(Parameters, Difference)

print(head(graph1))

graph1 %>%
  ggplot(aes(Difference, Parameters)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Dependency and Independency variable score") + 
  theme_bw()
#-------------------------------------------------------------------------------


# Correlation Matrix------------------------------------------------------------

cormelt = cor(data_clean[,1:5], method = "pearson")

print(cormelt)

meltcormat = melt(cormelt)

print(meltcormat)

meltcormat %>%
  ggplot(aes(x = X1, y = X2, fill = value)) +
  geom_tile(color = "black") +
  geom_text(aes(label = round(value, 4)), color = "white", size = 4) +
  coord_fixed() + 
  labs(title = "Correlation of first five columns") + 
  theme_bw()
#-------------------------------------------------------------------------------


# Correlation values in csv-----------------------------------------------------

para = c("income", 
         "kidhome", 
         "teenhome",
         "recency",
         "mnt_wines",
         "mnt_fruits", 
         "mnt_meat_products",
         "mnt_fish_products",
         "mnt_sweet_products", 
         "mnt_gold_products", 
         "num_deals_purchases",
         "num_web_purchases",
         "num_catalog_purchases", 
         "num_store_purchases", 
         "num_web_vists_month",
         "accepted_cmp1",
         "accepted_cmp2",
         "accepted_cmp3",
         "accepted_cmp4",
         "accepted_cmp5",
         "complain",
         "z_cost_contact",
         "z_revenue",
         "response",
         "age",
         "customer_days",
         "marital_divorced",
         "marital_married",
         "marital_single",
         "marital_together",
         "marital_widow",
         "education_2n_cycle",
         "education_basic",
         "education_graduation",
         "education_master",
         "education_phd",
         "mnt_total",
         "mnt_regular_prods",
         "accepted_cmp_overall")

corrvals = data.frame(para ,cormelt)

write_csv(corrvals, "corr_values.csv")
#-------------------------------------------------------------------------------


# Plotting graph of correlation score-------------------------------------------

dff = read.csv("corr_values.csv")

dff = dff[-c(22, 23),]

View(dff)


dff %>%
  ggplot(aes(income, para)) +
  geom_bar(stat = "identity") + 
  labs(x = "Income",
       y = "Parameters",
       title = "Correlation scores for income") +
  theme_bw()

dff %>%
  ggplot(aes(kidhome, para)) +
  geom_bar(stat = "identity") + 
  labs(x = "Kidhome",
       y = "Parameters",
       title = "Correlation scores for Kidhome") +
  theme_bw()

dff %>%
  ggplot(aes(teenhome, para)) +
  geom_bar(stat = "identity") + 
  labs(x = "Teenhome",
       y = "Parameters",
       title = "Correlation scores for Teenhome") +
  theme_bw()
  
dff %>%
  ggplot(aes(recency, para)) +
  geom_bar(stat = "identity") + 
  labs(x = "Recency",
       y = "Parameters",
       title = "Correlation scores for Recency") +
  theme_bw()

dff %>%
  ggplot(aes(mnt_wines, para)) +
  geom_bar(stat = "identity") + 
  labs(x = "Mnt_Wines",
       y = "Parameters",
       title = "Correlation scores for mnt_wines") +
  theme_bw()
#-------------------------------------------------------------------------------


# Linear regression (Trend Analysis)--------------------------------------------

data_clean %>%
  ggplot(aes(income, num_store_purchases)) + 
  geom_point(aes(color = income, size = num_store_purchases), alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Effect on income from store purchases") + 
  theme_bw()

model = lm(num_store_purchases ~ income, data = data_clean)

summary(model)


data_clean %>%
  ggplot(aes(income, num_web_purchases)) + 
  geom_point(aes(color = income, size = num_web_purchases), alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Effect on income from web purchases") + 
  theme_bw()

model = lm(num_web_purchases ~ income, data = data_clean)

summary(model)


data_clean %>%
  ggplot(aes(mnt_wines, age)) + 
  geom_point(aes(color = mnt_wines, size = age), alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Age vs wine") + 
  theme_bw()

model = lm(age ~ mnt_wines, data = data_clean)

summary(model)
#-------------------------------------------------------------------------------


# Writing cleaned data file into csv--------------------------------------------

write_csv(data_clean, "data_clean.csv")
#-------------------------------------------------------------------------------
