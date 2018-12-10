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



model4 = lm(estimated_generation_gwh ~ fuel1 + commissioning_year + capacity_mw, 
            data = globalPower)
summary(model4)

# Plot the points
plotmodel = lm(estimated_generation_gwh ~ capacity_mw, 
            data = globalPower)
plot(globalPower$capacity_mw,globalPower$estimated_generation_gwh, xlab = 'Capacity (MW)'
     , ylab = 'Estimated Generation (gWh)', main = 'Est. Generation by Capacity')

# Create a line of best fit
b = plotmodel$coefficients[1]
a = plotmodel$coefficients[2]
x = range(globalPower$capacity_mw)
y = b + a*x
lines(x,y)


# Gather USA per capita power production
USAPower <- globalPower[globalPower$country=='USA',][,23]
USAPower <- USAPower[complete.cases(USAPower)]
totalUSAPower <- sum(USAPower)
USAPop <- 325700000
USAPowerPerCap <- totalUSAPower/USAPop
USAPowerPerCap


# Gather CHN per capita power production
CHNPower <- globalPower[globalPower$country=='CHN',][,23]
CHNPower <- CHNPower[complete.cases(CHNPower)]
totalCHNPower <- sum(CHNPower)
CHNPop <- 1386000000
CHNPowerPerCap <- totalCHNPower/CHNPop
CHNPowerPerCap

