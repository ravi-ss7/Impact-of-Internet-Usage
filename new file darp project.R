data=read.csv("C:\\Users\\Harish Kumar Sachdev\\OneDrive\\Documents\\stat proj\\cleaned_dataset.csv")
View(data)

a1=lm(Unemployment..total....of.total.labor.force...modeled.ILO.estimate. ~ Individuals.using.the.Internet....of.population.,data = data)
summary(a1)
## A 1 percentage point increase in internet usage is associated with a 0.0033 percentage point increase in unemployment.
## Internet explains only 0.03% of variation in unemployment (r-square)


cor(data$Individuals.using.the.Internet....of.population.,data$Unemployment..total....of.total.labor.force...modeled.ILO.estimate.,use ="complete.obs") 
## The correlation between internet usage and unemployment is 0.018, which is very close to zero. This suggests that there is no strong linear relationship between the two variables.

#### This suggests that unemployment is influenced by multiple factors and cannot be explained by internet usage alone.


a2=lm(data$Unemployment..total....of.total.labor.force...modeled.ILO.estimate.~data$Individuals.using.the.Internet....of.population.+Wage.and.salaried.workers..total....of.total.employment...modeled.ILO.estimate.+Vulnerable.employment..total....of.total.employment...modeled.ILO.estimate.,data = data)
summary(a2)
## THIS IMPLIES
## If all variables = 0 (internet, wage, vulnerable) then unemployment = 29.79385%
## A 1 percentage point increase in internet usage is associated with a 0.08839 percentage point decrease in unemployment, holding other factors constant.
## A 1% increase in wage employment reduces unemployment by 0.14002
## A 1% increase in vulnerable employment reduces unemployment by 0.30086
## The regression results indicate that internet penetration has a negative association with unemployment, suggesting that digital expansion may improve labor market outcomes. Wage employment also reduces unemployment, reflecting the role of stable jobs. Vulnerable employment shows a negative relationship with unemployment, likely because individuals move into informal employment rather than remaining unemployed.

c=lm(data$Self.employed..total....of.total.employment...modeled.ILO.estimate.~data$Individuals.using.the.Internet....of.population.)        
summary(c) #y is self employment and x is internet using population
## For 1 percent increase in internet usage self employment decrease 0.61458 percent
## 57.39 percent variation in self employed workers is explained by internet usage population

d=lm(data$Wage.and.salaried.workers..total....of.total.employment...modeled.ILO.estimate.~data$Individuals.using.the.Internet....of.population.)
summary(d)
## For 1 percent increase in internet usage wage salaried workers increases 0.61458 percent
## 57.39 percent variation in wage salaried workers is explained by internet usage population

data$Year>="2019"
#year_covid=data[data$Year>="2019",] i had infer about this and not the below one 
year_covid=data[data$Year>="2020" & data$Year<="2022",]

covid_model=lm(year_covid$Unemployment..total....of.total.labor.force...modeled.ILO.estimate.~year_covid$Individuals.using.the.Internet....of.population.)
summary(covid_model)
## for 1% increase in internet usage unemployment total unemployment increases 0.0009912 percent
## 1.004e-05 (which is almost zero) percent variation in total unemployment is explained by internet usage population

cor(year_covid$Employers..total....of.total.employment...modeled.ILO.estimate.,year_covid$Individuals.using.the.Internet....of.population.,use = "complete.obs")
## in the covid time correlation is negative between those two variables

## try to infer something about job finding ratio something like that it increases job finding ratio
## did after 2020 internet usage increased


plot(data$Year,data$Individuals.using.the.Internet....of.population.)
boxplot(data$Individuals.using.the.Internet....of.population.)
?boxplot
#barplot(data$Year,data$Individuals.using.the.Internet....of.population.)
?barplot

hist(data$Individuals.using.the.Internet....of.population.)

# Step 1: Average internet usage per year
internet_trend = aggregate(
  Individuals.using.the.Internet....of.population. ~ Year,
  data = data,
  FUN = mean,
  na.rm = TRUE
)

barplot(data$Individuals.using.the.Internet....of.population.,names.arg = data$Year)


# Step 2: Barplot
barplot(
  internet_trend$Individuals.using.the.Internet....of.population.,
  names.arg = internet_trend$Year,
  xlab = "Year",
  ylab = "Internet Usage (%)",
  main = "Internet Usage Over Time",
  las = 2   # rotates year labels (important)
)
?barplot





barplot(
  data$Employers..total....of.total.employment...modeled.ILO.estimate.,
  names.arg =data$Year,
  xlab = "Year"
)

# Step 1: Take average by year
emp_trend = aggregate(
  Employers..total....of.total.employment...modeled.ILO.estimate. ~ Year,
  data = data,
  FUN = mean,
  na.rm = TRUE
)

# Step 2: Barplot
barplot(
  emp_trend$Employers..total....of.total.employment...modeled.ILO.estimate.,
  names.arg = emp_trend$Year,
  xlab = "Year",
  ylab = "Employers (%)",
  main = "Trend of Employers Over Time",
  las = 2
)


# Step 1: Average unemployment by year
unemp_trend = aggregate(
  Unemployment..total....of.total.labor.force...modeled.ILO.estimate. ~ Year,
  data = data,
  FUN = mean,
  na.rm = TRUE
)

# Step 2: Barplot
barplot(
  unemp_trend$Unemployment..total....of.total.labor.force...modeled.ILO.estimate.,
  names.arg = unemp_trend$Year,
  xlab = "Year",
  ylab = "Unemployment (%)",
  main = "Trend of Unemployment Over Time",
  las = 2
)

## the conclusion from the above graph is that in 2020 and 2021 unemployment rate increased and after that it decreased (REASON COVID-19)
## and for the internet usage it necer decreased it increases year to year 
## in which consecutive years the difference b/w internet usage rates was highest # from the graph it is 2023-2024 there was a huge jump in internet usage rates

median_internet = median(data$Individuals.using.the.Internet....of.population., na.rm = TRUE)

high_internet = data[data$Individuals.using.the.Internet....of.population. >= median_internet, ]
low_internet  = data[data$Individuals.using.the.Internet....of.population. < median_internet, ]

model_high = lm(Unemployment..total....of.total.labor.force...modeled.ILO.estimate. ~ 
                   Individuals.using.the.Internet....of.population., 
                 data = high_internet)

summary(model_high)

model_low = lm(Unemployment..total....of.total.labor.force...modeled.ILO.estimate. ~ 
                  Individuals.using.the.Internet....of.population., 
                data = low_internet)

summary(model_low)


##To analyze heterogeneity across countries, internet penetration is used as a proxy for development. The dataset is divided into high-internet and low-internet groups based on the median value of internet usage.
## High Internet Countries:
##In countries with high internet penetration, the coefficient of internet usage is negative and statistically significant. A 1 percentage point increase in internet usage is associated with a 0.142 percentage point decrease in unemployment. This suggests that in more digitally advanced economies, further expansion of internet access improves labor market outcomes, possibly through better job matching, remote work opportunities, and productivity gains.
##Low Internet Countries:
## In countries with low internet penetration, the coefficient is positive (0.048) and only weakly significant. This suggests that increased internet usage does not significantly reduce unemployment and may even be associated with a slight increase. This could reflect structural challenges such as lack of digital skills, limited job creation, or transition effects where workers are displaced before new opportunities emerge.
## IMPORTANT:
## The results indicate that the impact of internet usage on unemployment is not uniform across countries. While digital expansion reduces unemployment in high-internet economies, its effect is weak or even positive in low-internet economies. This suggests that the benefits of digitalization depend on complementary factors such as education, infrastructure, and labor market flexibility.



names(data)

service_model = lm(data$Employment.in.services....of.total.employment...modeled.ILO.estimate. ~ 
                      Individuals.using.the.Internet....of.population., 
                    data = data)

summary(service_model)


## Main Interpretation:
## The regression results show a strong positive relationship between internet usage and employment in the service sector. A 1 percentage point increase in internet usage is associated with a 0.432 percentage point increase in service sector employment. The coefficient is highly statistically significant, indicating a robust relationship.
## Strength of Relationship:
## The model explains approximately 57.9% of the variation in service sector employment, which is substantially higher than in the unemployment model. This suggests that internet usage is a key factor in explaining structural changes in employment.
## Economic Interpretation:
## The results indicate that increased internet penetration promotes the expansion of the service sector. This can be attributed to the growth of digital services such as IT, finance, e-commerce, online education, and remote work. As internet access increases, economies tend to shift from traditional sectors (such as agriculture and informal employment) toward more service-oriented activities.
## This finding is consistent with earlier results showing that internet usage reduces self-employment and increases wage employment. Together, these results suggest that digital expansion leads to structural transformation in the labor market, shifting workers toward more formal and service-based employment.
## Thus, internet penetration does not just affect unemployment, but fundamentally reshapes the structure of employment toward service-oriented economies.


data = data[order(data$Country.Name, data$Year), ]
data$Internet_lag = ave(
  data$Individuals.using.the.Internet....of.population.,
  data$Country.Name,
  FUN = function(x) c(NA, x[-length(x)])
)
lag_model = lm(Unemployment..total....of.total.labor.force...modeled.ILO.estimate. ~ 
                  Internet_lag, 
                data = data)

summary(lag_model)

## Main Interpretation:
## The lagged regression examines whether past internet usage affects current unemployment. The coefficient of lagged internet usage is positive (0.0022) but statistically insignificant (p-value = 0.831). This indicates that there is no evidence of a lagged effect of internet penetration on unemployment.
## Strength of Result
## The R square value is approximately zero, suggesting that past internet usage explains virtually none of the variation in current unemployment.
## Economic Interpretation
## This result suggests that the impact of internet usage on unemployment is either immediate or negligible, rather than occurring with a time delay. It may also indicate that unemployment is influenced more by macroeconomic conditions, policy factors, and structural characteristics than by past digital expansion alone.
## This finding is consistent with earlier results where internet usage showed little direct impact on unemployment. However, strong effects were observed on employment structure (such as increased service sector employment and wage employment). This suggests that internet penetration influences how people are employed rather than whether they are employed.
## There is no evidence of delayed (lagged) effects of internet usage on unemployment.





plot(data$Individuals.using.the.Internet....of.population.,
     data$Employment.in.services....of.total.employment...modeled.ILO.estimate.,
     xlab="Internet Usage (%)",
     ylab="Service Employment (%)",
     main="Internet vs Service Employment")

abline(service_model, col="blue")
## What the Graph Shows:
## The scatter plot of internet usage and service sector employment shows a clear upward-sloping pattern. As internet usage increases, the share of employment in the service sector also increases.
## Relationship:
## The fitted regression line is positively sloped, indicating a strong positive relationship between internet penetration and service employment.
##  Strength:
## The points are relatively closely clustered around the regression line, suggesting a strong association between the two variables. This visual evidence is consistent with the regression results, which showed a high R square value (around 0.58).
## Economic Meaning:
## The graph suggests that countries with higher internet usage tend to have a larger share of employment in the service sector. This reflects the role of digital technologies in promoting sectors such as IT, finance, e-commerce, and other online services.
## This supports the idea that digital expansion drives structural transformation in the economy, shifting employment away from traditional sectors toward modern service-based activities.
## The graphical analysis confirms a strong positive relationship between internet usage and service sector employment. What the Graph Shows
## The scatter plot of internet usage and service sector employment shows a clear upward-sloping pattern. As internet usage increases, the share of employment in the service sector also increases.

data_new=read.csv("C:\\Users\\Harish Kumar Sachdev\\OneDrive\\extra dataset for gr7.csv")
names(data_new)

library(tidyr)
library(dplyr)

data_long = data_new |>
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year",
    values_to = "Value"
  )
data_long$Year = gsub("X|\\.\\.YR", "", data_long$Year)
data_long$Year = as.numeric(data_long$Year)
data_final = data_long |>
  pivot_wider(
    names_from = Series.Name,
    values_from = Value
  )
names(data_final)
colnames(data_final) = c(
  "Country", "Year",
  "Unemployment",
  "Internet",
  "GDP",
  "Education",
  "Urban"
)
data_final$Unemployment = as.numeric(data_final$Unemployment)
data_final$Internet     = as.numeric(data_final$Internet)
data_final$GDP          = as.numeric(data_final$GDP)
data_final$Education    = as.numeric(data_final$Education)
data_final$Urban        = as.numeric(data_final$Urban)

data_final = na.omit(data_final)

model_final = lm(
  Unemployment ~ Internet + GDP + Education + Urban,
  data = data_final
)

summary(model_final)
## The multiple regression results show that internet usage has a negative and statistically significant effect on unemployment, where a 1 percentage point increase in internet usage reduces unemployment by about 0.10 percentage points, holding other factors constant.
## GDP growth also has a significant negative impact, confirming that economic expansion reduces unemployment.
## Education, however, shows a positive and significant relationship, which may reflect higher labor force participation or skill mismatch effects, while urbanization is not statistically significant. 
## Compared to the earlier simple regression, where internet usage had no effect, the inclusion of GDP and other control variables makes the relationship significant, indicating the presence of omitted variable bias.
## Overall, the results suggest that digitalization contributes to lower unemployment, but its effect operates alongside broader macroeconomic conditions rather than independently.




cor(data_final[, c("Unemployment","Internet","GDP","Education","Urban")],
    use = "complete.obs")

## The correlation results show that internet usage has almost no direct linear relationship with unemployment (correlation ≈ 0.009), indicating a very weak association. 
## GDP has a negative correlation with unemployment, suggesting that higher economic growth is associated with lower unemployment, while education shows a positive correlation, possibly reflecting higher labor force participation or skill mismatch. 
## Internet usage is strongly positively correlated with education and urbanization, indicating that more developed economies tend to have higher digital access. 
## Overall, the weak simple correlation between internet and unemployment supports earlier findings that internet alone does not explain unemployment, but becomes significant in regression when combined with other macroeconomic variables, highlighting the importance of a multivariate approach.



plot(data_final$Internet, data_final$Unemployment,
     xlab="Internet Usage (%)",
     ylab="Unemployment",
     main="Internet vs Unemployment")

abline(lm(Unemployment ~ Internet, data=data_final), col="red")
## The scatter plot of internet usage and unemployment shows a very weak relationship, as the fitted regression line is nearly horizontal, indicating little to no linear association between the two variables. 
## While a large number of observations are concentrated between 3% to 10% unemployment, there are several points far above the main cluster, indicating possible outliers or country-specific effects. 
## Overall, the graph suggests that internet usage alone does not significantly explain unemployment, which is consistent with the near-zero correlation and earlier regression results without control variables.
## Most data points are widely scattered across different levels of unemployment for all levels of internet usage, suggesting high variability. 



library(car)
vif(model_final)
## The Variance Inflation Factor (VIF) values for all variables are below 5, indicating that there is no serious multicollinearity problem in the model.
## Although internet usage, education, and urbanization are moderately correlated, the VIF values (around 3) suggest that this does not significantly distort the regression estimates. 
## Therefore, the coefficients obtained from the model are reliable, and multicollinearity is not a major concern in this analysis.


summary(a1)$r.squared
summary(model_final)$r.squared

## The simple regression model, which includes only internet usage, has an extremely low explanatory power (R² ≈ 0.0003), indicating that internet alone does not explain unemployment. However, the multiple regression model, which includes GDP, education, and urbanization, shows a much higher R² (≈ 0.145), demonstrating a significant improvement in explanatory power. 
## This highlights the presence of omitted variable bias in the simple model and confirms that unemployment is influenced by multiple macroeconomic factors rather than internet usage alone.


plot(model_final$fitted.values, model_final$residuals,
     xlab="Fitted Values",
     ylab="Residuals",
     main="Residual Plot")
abline(h=0, col="red")

## The residual plot shows that most residuals are centered around zero, indicating that the model captures the general relationship reasonably well. 
## However, the spread of residuals increases for higher fitted values and there are several points far above the main cluster, suggesting the presence of heteroscedasticity and possible outliers. 
## This indicates that the variance of errors is not constant across observations, and while the model is useful, it may not fully capture all underlying factors affecting unemployment.


## PERSON2
a1 = read.csv("C:\\Users\\Harish Kumar Sachdev\\OneDrive\\Documents\\stat proj\\cleaned_dataset.csv")
clean_a1 = na.omit(a1)
View(a1)
View(clean_a1)


###Q1

library(dplyr)
neet_inter = na.omit(a1[,c(1,2,8,16)])
View(neet_inter)

country_correlations = neet_inter |>
  group_by(Country.Name) |>
  summarize(
    correlation = cor(Share.of.youth.not.in.education..employment.or.training..total....of.youth.population....modeled.ILO.estimate., Individuals.using.the.Internet....of.population.)
  )

print(country_correlations)


#Across the 18 countries, we're seeing a clear trend: as more people get online, fewer young people are being left behind in the NEET category (Not in Education, Employment, or Training). For most of these nations, the internet is acting like a digital bridge, connecting youth to the classrooms and jobs they need to succeed. This shift is especially powerful in places like China and the Philippines, where the drop in disengaged youth has been remarkably steep as connectivity soared. However, it's not a magic fix for everyone. In a few countries where the correlation is positive(in range 0.6 to 0.8), in some cases the availability of internet is being used for entertainment purposes which is more likely causing a distraction in the Youth.


###Q2

inter_vs_unemp = na.omit(a1[,c(1,2,5,6,16)])
View(inter_vs_unemp)

country_correlations = inter_vs_unemp |>
  group_by(Country.Name) |>
  summarize(
    correlation = cor(Unemployment..youth.total....of.total.labor.force.ages.15.24...modeled.ILO.estimate., Individuals.using.the.Internet....of.population.)
  )

print(country_correlations)



library(ggplot2)
ly = ggplot(a1, aes(x = Year, y = Country.Name, fill = Individuals.using.the.Internet....of.population.)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  labs(title = "The Digital Revolution: Internet Adoption Timeline",
       fill = "Usage %")

ly

library(plotly)
ggplotly(ly)

#The Internet has it's pros and cons. In places like Germany, there is a nearly perfect correlation (-0.93) between getting online and finding work, which suggests people are successfully using digital tools to sharpen their skills and land jobs. However, the story is very different in China, Pakistan, and South Africa (with correlation in the range of 0.8 to 1), where we actually see unemployment rising alongside internet usage. This doesn't necessarily mean people are using the internet wrongly. In these countries, the digital world might be growing faster than the actual job market, or the skills people are picking up online might not be the ones local employers are actually looking for.



###Q3

vuln_vs_inter = na.omit(a1[,c(1,2,4,16)])
View(vuln_vs_inter)

country_correlations = vuln_vs_inter |>
  group_by(Country.Name) |>
  summarize(
    correlation = cor(Vulnerable.employment..total....of.total.employment...modeled.ILO.estimate., Individuals.using.the.Internet....of.population.)
  )

print(country_correlations)

#While the global trend shows the internet helping people move into formal roles, the UK and France serve as important counter-examples. In these high-income countries, the positive correlation suggests a 'Digital Precarity'—where the internet isn't fixing vulnerability, but rather enabling a new era of freelance and gig-based work that lacks the protections of traditional formal employment.
#The relationship between internet penetration and vulnerable employment reveals a fascinating economic divide: while most countries show a negative correlation where digital access helps formalize the workforce by bridging the gap to stable jobs, developed nations like the UK (0.827) and France (0.663) exhibit a positive correlation that signals a shift toward 'digital precarity'. In these high-income economies, widespread connectivity has fueled the rise of the gig economy and independent contracting, which—while technologically advanced—are statistically classified as vulnerable because they lack traditional employer-led protections like pensions and job security. Thus, the data suggests that while the internet acts as a ladder to formal employment in developing regions, it functions as a tool for deconstructing traditional labor structures in mature economies, replacing long-term stability with flexible but precarious freelance work.



###Q4

get_change = function(country_name) {
  temp = a1[a1$Country.Name == country_name, ]
  temp = temp[order(temp$Year), ]
  
  start_val = temp$"Unemployment..total....of.total.labor.force...modeled.ILO.estimate."[1]
  end_val = tail(temp$"Unemployment..total....of.total.labor.force...modeled.ILO.estimate.", 1)
  
  return(end_val - start_val)
}

countries = unique(a1$Country.Name)
changes = sapply(countries, get_change)
sort(changes)


sort_idx = order(changes)
sorted_changes = changes[sort_idx]
sorted_countries = countries[sort_idx]

barplot(sorted_changes, 
        names.arg = sorted_countries, 
        las = 2,
        col = ifelse(sorted_changes < 0, "forestgreen", "firebrick"),
        main = "Total Change in Unemployment (2005-2024)",
        ylab = "Percentage Point Change",
        cex.names = 0.7)

#The Bar Graph shows the net change of the Total Unemployment Rate(%).
#


###Q5

sector_impact = a1 |>
  group_by(Country.Name) |>
  summarize(
    services_vs_neet = cor(Employment.in.services....of.total.employment...modeled.ILO.estimate., 
                           Share.of.youth.not.in.education..employment.or.training..total....of.youth.population....modeled.ILO.estimate., 
                           use = "complete.obs"),
    industry_vs_neet = cor(Employment.in.industry....of.total.employment...modeled.ILO.estimate., 
                           Share.of.youth.not.in.education..employment.or.training..total....of.youth.population....modeled.ILO.estimate., 
                           use = "complete.obs")
  )

print(sector_impact)

#The Services sector demonstrates a more consistent and robust negative correlation with NEET rates across the sampled countries compared to the Industry sector. In high-growth and developing economies like China and Indonesia, the extremely high negative coefficients (exceeding -0.90) suggest that the Services sector is the primary engine for youth integration. While the Industry sector is effective in specific contexts—such as Indonesia and Mexico—its impact is inconsistent globally, occasionally showing positive correlations that suggest industrial growth may not be absorbing the youth demographic as effectively as service-oriented roles. Consequently, the Services sector appears to be the more efficient 'absorber' of youth labor, playing a critical role in keeping the younger population engaged in the workforce and out of the NEET(Share of Youth not in education, employment and training) category.
#


###Q6

#youth sensitivity index:
#Instead of just looking at youth unemployment, you can calculate the Youth Sensitivity Index. This measures how much harder it is for a young person to find a job compared to the average adult in that specific country.
#the sensitivity index by the following formula for each country:
#              index = mean( youth uemployment / mean(total unemployment) )
#


a1$Youth_Index = a1$"Unemployment..youth.total....of.total.labor.force.ages.15.24...modeled.ILO.estimate." / 
  a1$"Unemployment..total....of.total.labor.force...modeled.ILO.estimate."


countries = unique(a1$Country.Name)
avg_indices = sapply(countries, function(c) {
  mean(a1$Youth_Index[a1$Country.Name == c], na.rm = TRUE)
})


sort_idx = order(avg_indices, decreasing = TRUE)
sorted_indices = avg_indices[sort_idx]
sorted_countries = countries[sort_idx]


par(mar = c(10, 4, 4, 2))
barplot(sorted_indices, 
        names.arg = sorted_countries, 
        las = 2, 
        col = "steelblue", 
        main = "Youth Sensitivity Index by Country (2005-2024)",
        ylab = "Ratio (Youth Unemp / Total Unemp)",
        cex.names = 0.8)


abline(h = 1, lty = 2, col = "red")





###PLOTS


#Heatmap of Digital Maturity:
#A heatmap allows you to show three dimensions (Country, Year, and Intensity) in a single grid. It’s perfect for showing exactly when the internet boom happened in each country and how it compares to others.
#

library(ggplot2)
ggplot(a1, aes(x = Year, y = Country.Name, fill = Individuals.using.the.Internet....of.population.)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  labs(title = "The Digital Revolution: Internet Adoption Timeline",
       fill = "Usage %")



#This is a useful plot for the question 2 for visualizing the ranking of the correlation of the % of Internet usage and Youth Unemployment.
#

country_correlations |>
  arrange(correlation) |>
  mutate(Country.Name = factor(Country.Name, levels = Country.Name)) |>
  ggplot(aes(x = Country.Name, y = correlation)) +
  geom_segment(aes(x = Country.Name, xend = Country.Name, y = 0, yend = correlation), color = "grey") +
  geom_point(aes(color = correlation > 0), size = 4) +
  scale_color_manual(values = c("TRUE" = "firebrick", "FALSE" = "forestgreen"), 
                     labels = c("Bridge (Negative)", "Distraction (Positive)")) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Correlation Ranking: Digital Bridge vs. Distraction",
       color = "Impact Category")


#This double horizontal bar graph is useful for representing the impact of sectors in the neet rates of youth.
#This is useful for question 5.
#

library(ggplot2)
library(tidyr)
library(dplyr)

plot_data = sector_impact |>
  pivot_longer(
    cols = c(services_vs_neet, industry_vs_neet), 
    names_to = "Sector", 
    values_to = "Correlation"
  )

ggplot(plot_data, aes(x = reorder(Country.Name, Correlation), y = Correlation, fill = Sector)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("services_vs_neet" = "#2c7fb8", "industry_vs_neet" = "#feb24c"),
                    labels = c("Industry", "Services")) +
  labs(
    title = "Impact of Employment Sectors on Youth Disengagement (NEET)",
    subtitle = "Negative correlation (left) indicates the sector successfully reduces NEET rates",
    x = "Country",
    y = "Correlation Coefficient",
    fill = "Sector"
  ) +
  theme_minimal()


#a line plot for internet usage % vs year for each country, this is helpful for showing how the internet usage % varied for every country.
#


library(ggplot2)
library(dplyr)

a1 |>
  filter(!is.na(Individuals.using.the.Internet....of.population.)) |>
  ggplot(aes(x = Year, y = Individuals.using.the.Internet....of.population.)) +
  geom_line(color = "steelblue", size = 1) +
  facet_wrap(~ Country.Name, scales = "free_y") +
  labs(title = "Internet Adoption Trajectories by Country",
       y = "Internet Usage (%)", x = "Year") +
  theme_minimal()



#The quadrant plot of NEET rate against youth unemployment rate for 2024 reveals distinct youth labor market archetypes across the 18 countries. By splitting the chart at the median values of both indicators, four categories emerge. 'Integrated Youth' (bottom‑left quadrant) includes countries like Japan, Germany, and Mexico, where both NEET and youth unemployment are low, indicating effective education‑to‑employment transitions and strong labor demand for young workers. 'Active Job Seekers' (bottom‑right) features nations such as South Africa and Brazil, where high youth unemployment coexists with relatively moderate NEET; here, young people are actively looking for work but face structural barriers, a pattern often seen in economies with rigid labor markets or skills mismatches. 'Discouraged Youth' (top‑left) is sparsely populated, but countries approaching this zone (e.g., India with elevated NEET despite moderate unemployment) suggest that a significant share of youth have disengaged from both work and study, possibly due to familial constraints or perceived lack of opportunities. Finally, the 'Crisis Youth' quadrant (top‑right) captures countries like Nigeria and Pakistan, where both indicators are alarmingly high—a sign of profound labor market dysfunction and a generation at risk of long‑term exclusion. This typology underscores that a one‑size‑fits‑all policy response is insufficient; each quadrant demands tailored interventions, from job‑matching platforms in 'Active' countries to comprehensive education and social safety nets in 'Crisis' and 'Discouraged' ones.
#



latest_year = a1 |> filter(Year == 2024)

ggplot(latest_year, aes(x = Unemployment..youth.total....of.total.labor.force.ages.15.24...modeled.ILO.estimate.,
                        y = Share.of.youth.not.in.education..employment.or.training..total....of.youth.population....modeled.ILO.estimate.,
                        label = Country.Name)) +
  geom_point(aes(color = Country.Name), size = 4) +
  geom_text(vjust = -1, size = 3) +
  geom_hline(yintercept = median(latest_year$Share.of.youth.not.in.education..employment.or.training..total....of.youth.population....modeled.ILO.estimate., na.rm = TRUE), linetype = "dashed") +
  geom_vline(xintercept = median(latest_year$Unemployment..youth.total....of.total.labor.force.ages.15.24...modeled.ILO.estimate., na.rm = TRUE), linetype = "dashed") +
  labs(title = "Youth Labor Market Typology (2024)",
       x = "Youth Unemployment Rate (%)", y = "NEET (%)") +
  theme_minimal()

##PERSON-3

## Question 1
#Does internet usage influence the employment-to-population ratio across countries?
 ## Question 2
#How does the unemployment rate differ between individuals with basic education and those with advanced education?
 # Question 3
#What is the trend of self-employment over time, and does it indicate a shift in labour market structure?
#  Question 4
#Is there a relationship between part-time employment and employment in the services sector?
  
# Fixing  column names

colnames(data)[which(colnames(data) == "Country.Name")] = "Country"
colnames(data)[which(colnames(data) == "internet")] = "Year"
colnames(data)[which(colnames(data) == 
                       "Individuals.using.the.Internet....of.population.")] = "Internet"
colnames(data)[which(colnames(data) == "Employment.to.population.ratio..15...total......modeled.ILO.estimate.")] = "Emp_Pop"



# Cleaning data

clean_data = na.omit(data[, c("Internet", "Emp_Pop", "Country", "Year")])


#  Correlation

correlation = cor(clean_data$Internet, clean_data$Emp_Pop)
print(paste("Correlation:", round(correlation, 3)))

# Plotting graph

ggplot(clean_data, aes(x=Internet, y=Emp_Pop, color=Country)) +
  geom_point(size=2, alpha=0.7) +
  geom_smooth(method="lm", se=FALSE, color="black") +
  labs(title="Internet Usage vs Employment-Population Ratio",
       x="Internet Usage (% of population)",
       y="Employment-to-Population Ratio") +
  theme_minimal()

### Interpretation : The correlation of -0.038 indicates a very weak negative relationship,
#suggesting that there is no meaningful association between internet usage and employment-to-population ratio.
### Answer2
# Rename columns

colnames(data)[7] = "Unemp_Basic"
colnames(data)[9] = "Unemp_Advanced"

# Cleaning data 

clean_q2 = na.omit(data[, c("Unemp_Basic","Unemp_Advanced")])


# Average unemployment

avg_basic = mean(clean_q2$Unemp_Basic, na.rm=TRUE)
avg_adv = mean(clean_q2$Unemp_Advanced, na.rm=TRUE)

# Bar plot

barplot(c(avg_basic, avg_adv),
        names.arg=c("Basic Education", "Advanced Education"),
        col=c("orange", "green"),
        main="Unemployment by Education Level",
        ylab="Unemployment Rate")
### Interpretation: Advance education has less unemployment rate than basic education it shows that people with advance education are more employed. 


### Answer3
# Renaming columns

colnames(data)[10] = "Self_Employed"

#Cleaning data

clean_q3 = na.omit(data[, c("Year", "Self_Employed")])

# Average by year

library(dplyr)
trend = clean_q3 |>
  group_by(Year) |>
  summarise(avg_self = mean(Self_Employed, na.rm=TRUE))

# Plot

plot(trend$Year, trend$avg_self, type="b",
     col="purple", pch=19,
     main="Trend of Self Employment Over Time",
     xlab="Year", ylab="Self Employment (%)")
### Interpretation : From the graph, we observe a declining trend in self-employment
#from 2005 to around 2020, indicating a shift towards more formal employment. However, after 2020, there is a slight increase in self-employment, which may be due to economic disruptions such as the COVID-19 pandemic, where people shifted towards independent or informal work.

### Answer4
colnames(data)[11] = "Part_Time"
colnames(data)[13] = "Services"

#### Cleaning data

clean_q4 = na.omit(data[, c("Part_Time", "Services")])

# Correlation

cor(clean_q4$Part_Time, clean_q4$Services, use="complete.obs")

# Plot

plot(clean_q4$Part_Time
     , clean_q4$Services,
     main="Part-Time vs Services Employment",
     xlab="Part-Time Employment (%)",
     ylab="Services Employment (%)",
     col="darkgreen", pch=19)

abline(lm(Services ~ Part_Time, data=clean_q4), col="red")
### Interpretation: The correlation between part-time employment and employment
##in the services sector is approximately 0.48, indicating a moderate positive 
#relationship. This suggests that countries with a higher share of employment in
#the services sector tend to have higher levels of part-time employment  


## person 4

a1 = read.csv("C:\\Users\\Harish Kumar Sachdev\\OneDrive\\Documents\\stat proj\\cleaned_dataset.csv")
clean_a1 = na.omit(a1)
View(a1)
View(clean_a1)
# --- DATA PREPARATION (Renaming for readability) ---
df_clean = a1 |>
  rename(
    Wage_Salaried = `Wage.and.salaried.workers..total....of.total.employment...modeled.ILO.estimate.`,
    Vulnerable_Emp = `Vulnerable.employment..total....of.total.employment...modeled.ILO.estimate.`,
    Unemp_Basic = `Unemployment.with.basic.education....of.total.labor.force.with.basic.education.`,
    Unemp_Advanced = `Unemployment.with.advanced.education....of.total.labor.force.with.advanced.education.`,
    NEET_Rate = `Share.of.youth.not.in.education..employment.or.training..total....of.youth.population....modeled.ILO.estimate.`,
    Self_Employed = `Self.employed..total....of.total.employment...modeled.ILO.estimate.`,
    Employers = `Employers..total....of.total.employment...modeled.ILO.estimate.`,
    Service_Emp = `Employment.in.services....of.total.employment...modeled.ILO.estimate.`,
    Industry_Emp = `Employment.in.industry....of.total.employment...modeled.ILO.estimate.`,
    Internet_Usage = `Individuals.using.the.Internet....of.population.`
  )




# --- Q1: THE SKILL-DIGITAL GAP ---
# Calculate the gap between basic and advanced education unemployment
df_clean$Education_Gap = df_clean$Unemp_Basic - df_clean$Unemp_Advanced

ggplot(df_clean, aes(x = Internet_Usage, y = Education_Gap)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Does the Internet Bridge the Education-Unemployment Gap?",
       x = "Internet Usage (% of Population)", y = "Gap (Basic - Advanced Unemp %)") +
  theme_minimal()
q1_analysis = a1 |>
  group_by(Country.Name) |>
  summarize(
    Correlation = cor(
      `Individuals.using.the.Internet....of.population.`, 
      `Share.of.youth.not.in.education..employment.or.training..total....of.youth.population....modeled.ILO.estimate.`, 
      use = "complete.obs"
    )
  ) |>
  arrange(Correlation)

print(q1_analysis)

# --- Q2: PATH TO FORMALIZATION ---
# Ratio of Wage workers to Vulnerable workers
df_clean$Formalization_Ratio = df_clean$Wage_Salaried / df_clean$Vulnerable_Emp

ggplot(df_clean, aes(x = Internet_Usage, y = Formalization_Ratio)) +
  geom_point(aes(color = Country.Name)) +
  geom_smooth(method = "loess", color = "black") +
  scale_y_log10() + # Using log scale for better distribution
  labs(title = "Internet Usage vs Job Formalization Ratio",
       subtitle = "Higher ratio = more stable wage jobs compared to vulnerable ones",
       x = "Internet Usage (%)", y = "Formalization Ratio (Log Scale)") +
  theme_minimal() + theme(legend.position = "none")

# --- Q3: NEET TIPPING POINT ---
# Create buckets for internet usage
df_clean$Internet_Bucket = cut(df_clean$Internet_Usage, 
                                breaks = c(0, 20, 40, 60, 80, 100),
                                labels = c("0-20%", "21-40%", "41-60%", "61-80%", "81-100%"))

neet_summary = df_clean |>
  filter(!is.na(Internet_Bucket)) |>
  group_by(Internet_Bucket) |>
  summarize(Avg_NEET = mean(NEET_Rate, na.rm = TRUE))

ggplot(neet_summary, aes(x = Internet_Bucket, y = Avg_NEET, fill = Internet_Bucket)) +
  geom_bar(stat = "identity") +
  labs(title = "The NEET Tipping Point",
       subtitle = "Average NEET rates drop significantly as internet penetration hits higher tiers",
       x = "Internet Penetration Tier", y = "Average NEET Rate (%)") +
  theme_minimal()

# --- Q4: ENTREPRENEURSHIP VS NECESSITY ---
# Compare Internet correlation with Employers vs Self-Employed
corr_analysis = df_clean |>
  summarize(
    Corr_Employers = cor(Internet_Usage, Employers, use = "complete.obs"),
    Corr_SelfEmployed = cor(Internet_Usage, Self_Employed, use = "complete.obs")
  )
print("Correlation of Internet with Employers vs Self-Employed:")
print(corr_analysis)

# --- Q5: SECTOR ABSORPTION (SERVICES VS INDUSTRY) ---
# Which sector's growth is more strongly tied to NEET reduction?
ggplot(df_clean) +
  geom_smooth(aes(x = Service_Emp, y = NEET_Rate, color = "Services"), method = "lm") +
  geom_smooth(aes(x = Industry_Emp, y = NEET_Rate, color = "Industry"), method = "lm") +
  labs(title = "Sector Efficiency in Reducing NEET",
       x = "% Sector Employment", y = "NEET Rate (%)",
       color = "Economic Sector") +
  theme_minimal()


















# Run the multiple regression model
model_final = lm(Unemployment ~ Internet + GDP + Education + Urban, data = data_final)

# View full regression results
summary(model_final)

# Extract coefficients only
coef(model_final)

colnames(data)
# 1. Wage & Salaried Employment model
model_wage = lm(data$Wage.and.salaried.workers..total....of.total.employment...modeled.ILO.estimate. ~ data$Internet)
summary(model_wage)

# 2. Services Employment model
model_service = lm(data$Services ~ data$Internet)
summary(model_service)

# 3. Self Employment model
model_self = lm(Self_Employed ~ Internet, data = data)
summary(model_self)

# 4. Vulnerable Employment model
model_vulnerable = lm(data$Vulnerable.employment..total....of.total.employment...modeled.ILO.estimate. ~ Internet, data = data)
summary(model_vulnerable)
coef(model_wage)
coef(model_service)
coef(model_self)
coef(model_vulnerable)
data.frame(
  Variable = c("Wage","Services","Self Employment","Vulnerable"),
  Coefficient = c(
    coef(model_wage)[2],
    coef(model_service)[2],
    coef(model_self)[2],
    coef(model_vulnerable)[2]
  ),
  R_squared = c(
    summary(model_wage)$r.squared,
    summary(model_service)$r.squared,
    summary(model_self)$r.squared,
    summary(model_vulnerable)$r.squared
  )
)


library(ggplot2)
library(tidyr)
library(dplyr)

emp_data = data |>
  select(Internet, Wage.and.salaried.workers..total....of.total.employment...modeled.ILO.estimate., Services, Self_Employed, Vulnerable.employment..total....of.total.employment...modeled.ILO.estimate.)

long_data = pivot_longer(emp_data,
                          cols = -Internet,
                          names_to = "Employment_Type",
                          values_to = "Value")

ggplot(long_data, aes(x = Internet, y = Value)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Employment_Type, scales = "free_y", ncol = 2) +
  labs(title = "Internet Usage and Employment Structure",
       x = "Internet Usage (%)",
       y = "Employment Value")
data$Share.of.youth.not.in.education..employment.or.training..total....of.youth.population....modeled.ILO.estimate.


model_gdp = lm(Unemployment ~ GDP, data = data_final)
summary(model_gdp)

plot(data_final$GDP, data_final$Unemployment,
     main="GDP Growth vs Unemployment",
     xlab="GDP (%)",
     ylab="Unemployment (%)")

abline(lm(Unemployment ~ GDP, data=data_final), lwd=2)
cor(data_final$GDP, data_final$Unemployment,use="complete.obs")


internet_country = aggregate(Internet ~ Country, data = data_final, mean, na.rm = TRUE)

internet_country[order(-internet_country$Internet), ]

unemp_country = aggregate(Unemployment ~ Country, data = data_final, mean, na.rm = TRUE)

unemp_country[order(-unemp_country$Unemployment), ]

gdp_country = aggregate(GDP ~ Country, data = data_final, mean, na.rm = TRUE)

gdp_country[order(-gdp_country$GDP), ]

library(dplyr)
library(tidyr)
library(ggplot2)

emp_data = data |>
  select(
    Internet,
    Wage_Salaried = Wage.and.salaried.workers..total....of.total.employment...modeled.ILO.estimate.,
    Services,
    Self_Employed,
    Vulnerable_Employment = Vulnerable.employment..total....of.total.employment...modeled.ILO.estimate.,
    NEET = Share.of.youth.not.in.education..employment.or.training..total....of.youth.population....modeled.ILO.estimate.
  )

# Regression of NEET on Internet usage

model_neet = lm(
  Share.of.youth.not.in.education..employment.or.training..total....of.youth.population....modeled.ILO.estimate. ~ Internet,
  data = data
)

summary(model_neet)

coef(model_neet)

data.frame(
  Variable = "NEET",
  Coefficient = coef(model_neet)[2],
  R_squared = summary(model_neet)$r.squared
)
long_data = pivot_longer(
  emp_data,
  cols = -Internet,
  names_to = "Employment_Type",
  values_to = "Value"
)

ggplot(long_data, aes(x = Internet, y = Value)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Employment_Type, scales = "free_y", ncol = 2) +
  labs(
    title = "Internet Usage and Employment Structure",
    x = "Internet Usage (%)",
    y = "Employment Value"
  )

data$Unemployment..total....of.total.labor.force...modeled.ILO.estimate.
internet_var = data[["Internet"]]
unemp_var = data[["Unemployment..total....of.total.labor.force...modeled.ILO.estimate."]]   # change if needed

median_val = median(internet_var, na.rm = TRUE)

data$group = ifelse(internet_var > median_val, "High", "Low")

table(data$group)

library(dplyr)

ci_results = data |>
  group_by(group) |>
  summarise(
    n = n(),
    mean_unemp = mean(Unemployment..total....of.total.labor.force...modeled.ILO.estimate., na.rm = TRUE),
    sd_unemp = sd(Unemployment..total....of.total.labor.force...modeled.ILO.estimate., na.rm = TRUE),
    se = sd_unemp / sqrt(n),
    lower = mean_unemp - qt(0.975, df = n-1) * se,
    upper = mean_unemp + qt(0.975, df = n-1) * se
  )
t_test_result = t.test(
  Unemployment..total....of.total.labor.force...modeled.ILO.estimate. ~ group,
  data = data
)

t_test_result
ci_results


# Interaction Model

model_interaction = lm(
  Unemployment ~ Internet + Education + Internet:Education,
  data = data_final
)

summary(model_interaction)

coef(model_interaction)
summary(model_interaction)$r.squared

data.frame(
  Variable = names(coef(model_interaction)),
  Coefficient = coef(model_interaction)
)

model_edu2 = lm(
  Unemployment..total....of.total.labor.force...modeled.ILO.estimate. ~ Internet + Unemp_Basic + Unemp_Advanced,
  data = data
)
summary(model_edu2)

model_edu_interact = lm(
  Unemployment..total....of.total.labor.force...modeled.ILO.estimate. ~ Internet +
    Unemp_Basic+
    Unemp_Advanced+
    Internet:Unemp_Basic +
    Internet:Unemp_Advanced,
  data = data
)

summary(model_edu_interact)


# Separate graphs for low and high education groups

data$Edu_Group = ifelse(data$Unemp_Basic > median(data$Unemp_Basic, na.rm=TRUE),
                         "High Basic Education",
                         "Low Basic Education")

ggplot(data, aes(x = Internet,
                 y = Unemployment..total....of.total.labor.force...modeled.ILO.estimate.)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Edu_Group) +
  labs(
    title = "Internet Usage vs Unemployment by Education Group",
    x = "Internet Usage (%)",
    y = "Unemployment Rate (%)"
  )




library(ggplot2)
library(gridExtra)

# Graph 1: Basic Education vs Unemployment
g1 = ggplot(data, aes(x = Unemp_Basic,
                       y = Unemployment..total....of.total.labor.force...modeled.ILO.estimate.)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Basic Education vs Unemployment",
    x = "Basic Education (%)",
    y = "Unemployment (%)"
  )

# Graph 2: Advanced Education vs Unemployment
g2 = ggplot(data, aes(x = Unemp_Advanced,
                       y = Unemployment..total....of.total.labor.force...modeled.ILO.estimate.)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Advanced Education vs Unemployment",
    x = "Advanced Education (%)",
    y = "Unemployment (%)"
  )

# Graph 3: Internet Usage vs Unemployment
g3 = ggplot(data, aes(x = Internet,
                       y = Unemployment..total....of.total.labor.force...modeled.ILO.estimate.)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Internet Usage vs Unemployment",
    x = "Internet Usage (%)",
    y = "Unemployment (%)"
  )
#install.packages("gridExtra")   # run once only
library(gridExtra)

# Show all three graphs
grid.arrange(g1, g2, g3, ncol = 2)



# Model for workers with basic education

model_basic = lm(Unemp_Basic ~ Internet, data = data)

summary(model_basic)

# Model for workers with advanced education

model_advanced = lm(Unemp_Advanced ~ Internet, data = data)

summary(model_advanced)
data.frame(
  Group = c("Basic Education", "Advanced Education"),
  Coefficient = c(
    coef(model_basic)[2],
    coef(model_advanced)[2]
  ),
  R_squared = c(
    summary(model_basic)$r.squared,
    summary(model_advanced)$r.squared
  )
)
library(ggplot2)
library(gridExtra)

g1 = ggplot(data, aes(x = Internet, y = Unemp_Basic)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Internet Usage vs Unemployment (Basic Education)",
    x = "Internet Usage (%)",
    y = "Unemployment Rate (%)"
  )

g2 = ggplot(data, aes(x = Internet, y = Unemp_Advanced)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Internet Usage vs Unemployment (Advanced Education)",
    x = "Internet Usage (%)",
    y = "Unemployment Rate (%)"
  )

gridExtra::grid.arrange(g1, g2, ncol = 2)

library(ggplot2)
library(gridExtra)

# Basic education unemployment graph
g1 = ggplot(data, aes(x = Internet, y = Unemp_Basic)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 1) +
  labs(
    title = "Basic Education",
    x = "Internet Usage (%)",
    y = "Unemployment Rate (%)"
  ) +
  theme_minimal()

# Advanced education unemployment graph
g2 = ggplot(data, aes(x = Internet, y = Unemp_Advanced)) +
  geom_point(alpha = 0.6, color = "darkred") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  labs(
    title = "Advanced Education",
    x = "Internet Usage (%)",
    y = "Unemployment Rate (%)"
  ) +
  theme_minimal()

# Show side by side
gridExtra::grid.arrange(g1, g2, ncol = 2)
library(dplyr)
library(ggplot2)

# Keep only complete rows
data_clean = data |>
  select(Internet,
         Unemployment..total....of.total.labor.force...modeled.ILO.estimate.) |>
  na.omit()

# Median
med = median(data_clean$Internet)

# Create only two groups
data_clean = data_clean |>
  mutate(Group = ifelse(Internet > med,
                        "High Internet",
                        "Low Internet"))

# Summary table
ci_table = data_clean |>
  group_by(Group) |>
  summarise(
    Mean = mean(Unemployment..total....of.total.labor.force...modeled.ILO.estimate.),
    SD = sd(Unemployment..total....of.total.labor.force...modeled.ILO.estimate.),
    n = n()
  ) |>
  mutate(
    SE = SD / sqrt(n),
    Lower = Mean - 1.96 * SE,
    Upper = Mean + 1.96 * SE
  )

# Graph
ggplot(ci_table, aes(x = Group, y = Mean)) +
  geom_point(size = 4, color = "blue") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper),
                width = 0.15,
                color = "red",
                linewidth = 1) +
  labs(
    title = "95% Confidence Interval of Unemployment",
    x = "",
    y = "Mean Unemployment (%)"
  ) +
  theme_minimal()