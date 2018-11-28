globalPower = read.csv("globalPower.csv")

globalPowerClean <- globalPower[!is.na(globalPower$estimated_generation_gwh),]
head(globalPowerClean)

  
#-----------------------------------------------------------------------------------#
#Is there a relationship between fuel type and estimated future generation of energy?
#Answer: Yeah! Nuclear has the brightest future: 12814 gWh.
#One household per month probably needs 1,000 kWh > 0.001 gWh.
#So this future energy potential would 12,814,000 homes. 
#
#Additionally, solar and wind energy sources do not appear to have a strong future.
#-----------------------------------------------------------------------------------#
model1 = lm(estimated_generation_gwh ~ fuel1, data = globalPower)
summary(model1) #intercept = BIOMASS


#-----------------------------------------------------------------------------------#
#Is there a relationship between year of commissioning and future generation?
#Answer: No relationship, p > .2
#-----------------------------------------------------------------------------------#

EstGenerationByCommishYear = lm(estimated_generation_gwh ~ commissioning_year, data = globalPowerClean)
summary(EstGenerationByCommishYear)

#-----------------------------------------------------------------------------------#
#The best model is made from combining the two factors into one regression.
#-----------------------------------------------------------------------------------#

model3 = lm(estimated_generation_gwh ~ fuel1 + commissioning_year, 
            data = globalPower)
summary(model3)
