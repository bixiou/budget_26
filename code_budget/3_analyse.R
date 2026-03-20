##### GCS #####
summary(lm(gcs_support == "Yes" ~ variant_gcs, data = e, weights = weight)) # -.10**
summary(lm(gcs_support == "Yes" ~ gcs_understood, data = e, weights = weight)) # -.02
summary(lm(gcs_support == "Yes" ~ (climate_belief >= 0), data = e, weights = weight)) # .29***
summary(lm(gcs_support == "Yes" ~ (climate_belief > 0), data = e, weights = weight)) # .23***


##### Wealth tax #####
summary(lm(wealth_tax_support ~ variant_wealth_tax, data = e, weights = weight)) # .09* / .05


##### Sustainable future #####
summary(lm(sustainable_future ~ variant_sustainable_future, data = e, weights = weight)) # -.01 


#### Group defended #####
summary(lm((group_defended > 0) ~ variant_group_defended, data = e, weights = weight)) # .12***
summary(lm((group_defended == 0) ~ variant_group_defended, data = e, weights = weight)) # -.13***
summary(lm((group_defended == 1) ~ variant_group_defended, data = e, weights = weight)) # .07**


##### WTP #####
summary(lm(wtp ~ factor(variant_wtp), data = e, weights = weight)) # 1%: .09.; 10%: -15**
