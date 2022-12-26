# load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load('ggplot2','corrplot','gridExtra', 'dplyr', 'car', 'lmtest', 'statmod',
               'ggalt', 'dplyr', 'readxl', 'plm', 'data.table', 'sandwich', 'data.table',
               'writexl', 'lares', 'ggpubr', 'stargazer', 'fastDummies', 'Hmisc')

###### Load data ######

id <- "1dxIbCsIHd3NzPccU-xYw9hN15oQmY5pj"
data <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))
names(data)

# Transform variable

data$Year<-as.factor(data$Year)

# Rename variables for convenience

names(data) <- gsub(pattern = "corr", replacement = "absence_corr", x = names(data))
names(data) <- gsub(pattern = "lcorr", replacement = "labsence_corr", x = names(data))

# Select variables

all<-data.frame(data$Year, data$Country, data$Efficiency, data$govdbtshgdp, 
                data$lgovdbtshgdp, data$govbalshgdp, data$lgovbalshgdp, 
                data$opn_bdgt, data$lopn_bdgt, data$polity2, data$lpolity2,
                data$govqual,  data$lgovqual, data$gdp_grwth, data$lgdp_grwth,
                data$gdp_per_cap, data$lgdp_per_cap,data$unempl, data$lunempl,
                data$procedures, data$lprocedures, data$days, data$ldays,  
                data$absence_corr, data$labsence_corr)
names(all)

# Remove "data." pattern from columns names

names(all) <- gsub(pattern = "data.", replacement = "", x = names(all))
names(all)

# Rename a country name for convenience

all[133:136,2]<-"Cote dIvoire"

# Add income_group variable (World Bank, July 2016)

all<- all %>% mutate(.,income_group = case_when(
  grepl("Benin|Burkina Faso|Burundi|Chad|Congo, Democratic Rep.|Ethiopia|Gambia, The|Guinea|Haiti|Liberia|Madagascar|Malawi|Mali|Mozambique|Nepal|Rwanda|Senegal|Sierra Leone|Tanzania|Uganda|Zimbabwe",Country) 
  ~ "Low_income",
  grepl("Armenia|Bangladesh|Bhutan|Bolivia|Cambodia|Cameroon|Cape Verde|Cote dIvoire|Egypt|El Salvador|Ghana|Guatemala|Honduras|India|Indonesia|Kenya|Kyrgyz Republic|Lao PDR|Lesotho|Mauritania|Mongolia|Morocco|Moldova|Myanmar|Nicaragua|Nigeria|Pakistan|Philippines|Sri Lanka|Swaziland|Syria|Tajikistan|Timor-Leste|Tunisia|Ukraine|Vietnam|Yemen|Zambia",Country) 
  ~ "Lower_middle_income",
  grepl("Argentina|Albania|Algeria|Angola|Azerbaijan|Belize|Bosnia and Herzegovina|Botswana|Brazil|Bulgaria|China|Colombia|Costa Rica|Dominican Republic|Ecuador|Gabon|Georgia|Guyana|Iran, Islamic Rep.|Jamaica|Jordan|Kazakhstan|Lebanon|Libya|Macedonia, FYR|Malaysia|Mauritius|Mexico|Montenegro|Namibia|Panama|Paraguay|Peru|Romania|Russian Federation|Serbia|South Africa|Suriname|Thailand|Turkey|Venezuela",Country) 
  ~ "Upper_middle_income",
  grepl("Australia|Austria|Bahrain|Barbados|Belgium|Brunei Darussalam|Canada|Chile|Croatia|Cyprus|Czech Republic|Denmark|Estonia|Finland|France|Germany|Greece|Hong Kong SAR|Hungary|Iceland|Ireland|Israel|Italy|Japan|Korea, Rep.|Kuwait|Latvia|Lithuania|Luxembourg|Malta|Netherlands|New Zealand|Norway|Oman|Poland|Portugal|Puerto Rico|Qatar|Saudi Arabia|Seychelles|Singapore|Slovak Republic|Slovenia|Spain|Sweden|Switzerland|Taiwan, China|Trinidad and Tobago|United Arab Emirates|United Kingdom|United States|Uruguay",Country) 
  ~ "High_income"
))

# check

sum(is.na(all$income_group))


# Add region variable (World Economic Forum classification)

all<- all %>% mutate(.,region = case_when(
  grepl("Australia|Brunei Darussalam|Cambodia|China|Hong Kong SAR|Indonesia|Japan|Korea, Rep.|Lao PDR|Malaysia|Mongolia|Myanmar|New Zealand|Philippines|Singapore|Taiwan, China|Thailand|Timor-Leste|Vietnam",Country) 
  ~ "East_Asia_and_Pacific",
  grepl("Armenia|Azerbaijan|Georgia|Kazakhstan|Kyrgyz Republic|Moldova|Russian Federation|Tajikistan|Ukraine",Country) 
  ~ "Eurasia",
  grepl("Albania|Austria|Belgium|Bosnia and Herzegovina|Bulgaria|Canada|Croatia|Cyprus|Czech Republic|Denmark|Estonia|Finland|France|Germany|Greece|Hungary|Iceland|Ireland|Italy|Latvia|Lithuania|Luxembourg|Macedonia, FYR|Malta|Montenegro|Netherlands|Norway|Poland|Portugal|Romania|Serbia|Slovak Republic|Slovenia|Spain|Sweden|Switzerland|Turkey|United Kingdom|United States",Country) 
  ~ "Europe_and_North_America",
  grepl("Argentina|Barbados|Belize|Bolivia|Brazil|Chile|Colombia|Costa Rica|Dominican Republic|Ecuador|El Salvador|Guatemala|Guyana|Haiti|Honduras|Jamaica|Mexico|Nicaragua|Panama|Paraguay|Peru|Puerto Rico|Suriname|Trinidad and Tobago|Uruguay|Venezuela",Country) 
  ~ "Latin_America_and_the_Caribbean",
  grepl("Algeria|Bahrain|Egypt|Iran, Islamic Rep.|Israel|Jordan|Kuwait|Lebanon|Libya|Morocco|Oman|Qatar|Saudi Arabia|Syria|Tunisia|United Arab Emirates|Yemen",Country) 
  ~ "Middle_East_and_North_Africa",
  grepl("Bangladesh|Bhutan|India|Nepal|Pakistan|Sri Lanka",Country) 
  ~ "South_Asia",
  grepl("Angola|Benin|Botswana|Burkina Faso|Burundi|Cameroon|Cape Verde|Chad|Congo, Democratic Rep.|Cote dIvoire|Ethiopia|Gabon|Gambia, The|Ghana|Guinea|Kenya|Lesotho|Liberia|Madagascar|Malawi|Mali|Mauritania|Mauritius|Mozambique|Namibia|Nigeria|Rwanda|Senegal|Seychelles|Sierra Leone|South Africa|Swaziland|Tanzania|Uganda|Zambia|Zimbabwe",Country) 
  ~ "Sub_Saharan_Africa"
))

# check

sum(is.na(all$region))

# Turn some variables from character to numeric

all_num <-all %>% 
  mutate_at(c(3:25), as.numeric)

# Round values to 3 digits after a coma

all_num_round <- 
  data.frame(lapply(all_num, function(y) if(is.numeric(y)) round(y, 3) else y)) 


# Split existing dataset on present and lagged for convenience

## Present effects

present <- data.frame(all_num_round$Year, all_num_round$Country, all_num_round$Efficiency,
                      all_num_round$govdbtshgdp, all_num_round$govbalshgdp, 
                      all_num_round$opn_bdgt, all_num_round$govqual,
                      all_num_round$polity2, all_num_round$gdp_grwth, 
                      all_num_round$gdp_per_cap, all_num_round$unempl, 
                      all_num_round$procedures, all_num_round$days, 
                      all_num_round$absence_corr, all_num_round$income_group,
                      all_num_round$region)
present<-as.data.frame(present)
names(present) <- gsub(pattern = "all_num_round.", replacement = "", x = names(present))

# Check for NA by row

na_rows_pr <- rowSums(is.na(present))
country_delete_pr<-which(na_rows_pr >= 6)
present[country_delete_pr,]

# Delete panels for countries that have NA in at least half of the variables 
# loss of 4,6% of the data

present<-
  present[!grepl("Barbados|Belize|Libya|Myanmar|Puerto Rico|Seychelles|Timor-Leste",
                 present$Country),]

# check
na_rows_pr <- rowSums(is.na(present))
country_delete_pr<-which(na_rows_pr >= 6)
present[country_delete_pr,]

# Impute NA that left with yearly-regionally medians

# Check for NA by column

NAcol_present <- which(colSums(is.na(present)) > 0)

sort(colSums(sapply(present[NAcol_present], is.na)), decreasing = TRUE)
cat('There are', length(NAcol_present), 'columns with missing values')

# For opn_bdgt - ~38% of the data is missed

# How much data is missed in each var?

pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(present,2,pMiss)

# Impute instead of NAs yearly-regionally medians

present<-present %>%
  group_by(Year, region) %>%
  mutate_if(is.numeric,
            function(x) ifelse(is.na(x),
                               median(x, na.rm = TRUE),
                               x))

# Check for NA by column again

NAcol_present <- which(colSums(is.na(present)) > 0)
sort(colSums(sapply(present[NAcol_present], is.na)), decreasing = TRUE)
cat('There are', length(NAcol_present), 'columns with missing values')

## Lagged effects

lagged <- data.frame(all_num_round$Year, all_num_round$Country, all_num_round$Efficiency,
                     all_num_round$lgovdbtshgdp, all_num_round$lgovbalshgdp,
                     all_num_round$lopn_bdgt, all_num_round$lgovqual,
                     all_num_round$lpolity2, all_num_round$lgdp_grwth,
                     all_num_round$lgdp_per_cap, all_num_round$lunempl, 
                     all_num_round$lprocedures, all_num_round$ldays, 
                     all_num_round$labsence_corr, all_num_round$income_group,
                     all_num_round$region)
lagged<-as.data.frame(lagged)
names(lagged) <- gsub(pattern = "all_num_round.l", replacement = "", x = names(lagged))
names(lagged) <- gsub(pattern = "all_num_round.", replacement = "", x = names(lagged))

# Check for NA my row

na_rows_lg <- rowSums(is.na(lagged))
country_delete_lg<-which(na_rows_lg >= 6)
lagged[country_delete_lg,]

# Delete panels for countries that has NA in at least half of the variables 
# loss of 4,6% of the data
# In lagged dataset same countries but Barbados have NA in at least on period
# in at least half of all of the explanatory variables
# but we delete Barbados too to work with the same sample of countries 
# for present and lagged effects

lagged<-
  lagged[!grepl("Barbados|Belize|Libya|Myanmar|Puerto Rico|Seychelles|Timor-Leste",
                lagged$Country),]
# check
na_rows_lg <- rowSums(is.na(lagged))
country_delete_lg<-which(na_rows_lg >= 6)
lagged[country_delete_lg,]

# Impute NA that left with yearly-regionally medians

# Check for NA by column

NAcol_lagged <- which(colSums(is.na(lagged)) > 0)
sort(colSums(sapply(lagged[NAcol_lagged], is.na)), decreasing = TRUE)
cat('There are', length(NAcol_lagged), 'columns with missing values')

# For opn_bdgt - ~44% of the data is missed

# How much data is missed in each var?

apply(lagged,2,pMiss)

# Impute instead of NAs yearly-regionally medians

lagged<-lagged %>%
  group_by(Year, region) %>%
  mutate_if(is.numeric,
            function(x) ifelse(is.na(x),
                               median(x, na.rm = TRUE),
                               x))

# Check for NA by column again

NAcol_lagged <- which(colSums(is.na(lagged)) > 0)
sort(colSums(sapply(lagged[NAcol_lagged], is.na)), decreasing = TRUE)
cat('There are', length(NAcol_lagged), 'columns with missing values')

# Note: Results of the 'test' regression before and after 
# NA imputation are almost identical and could be given by request

###### Summary statistics - Chapter 4 ######

# present - descriptive statistics, Ch. 4
present<-as.data.frame(present)
summary(present) #summary statistics
present %>% summarise_if(is.numeric, sd) #add standard deviation
cv <- function(x) (sd(x) / mean(x)) * 100
present %>% summarise_if(is.numeric, cv) #add coefficient of variation

#lagged - Appendix B
lagged<-as.data.frame(lagged)
summary(lagged) #summary statistics
lagged %>% summarise_if(is.numeric, sd) #add standard deviation
lagged %>% summarise_if(is.numeric, cv) #add coefficient of variation

# Summary statistics by year_income_group
# only for present effects
mean_yr_inc_pr <- present %>%
  group_by(Year, income_group) %>%
  summarise_at(vars(Efficiency:absence_corr), list(mean))
mean_yr_inc_pr<-
  data.frame(lapply(mean_yr_inc_pr, function(y) if(is.numeric(y)) round(y, 3) else y))

# Summary statistics by year_region
# only for present effects
mean_yr_reg_pr <- present %>%
  group_by(Year, region) %>%
  summarise_at(vars(Efficiency:absence_corr), list(mean))
mean_yr_reg_pr <-
  data.frame(lapply(mean_yr_reg_pr , function(y) if(is.numeric(y)) round(y, 3) else y))

###### Data visualization ######

# Let's explore correlations - Ch. 4

# Present variables
num_cols_pr <- unlist(lapply(present, is.numeric))
corrplot_pr <- present[,num_cols_pr] # correlations of all numeric variables
cor_numvar_pr <- cor(corrplot_pr, use="pairwise.complete.obs") 
colnames(cor_numvar_pr) <- 
  c("Efficiency of government spending", "Government debt % of gdp", "Budget balance % of gdp", "Budget transparency", "Government effectiveness",
    "Democracy", "GDP growth", "GDP per capita", "Unemployment", "Procedures for registering property",
    "Days for registering property", "Absence of corruption")
rownames(cor_numvar_pr) <-colnames(cor_numvar_pr)
corrplot(cor_numvar_pr, type="full", tl.col="black", method="color")


# Linear relationship between: Ch. 3
# 1) Efficiency of government spending and number of days
# 2) Efficiency of government spending and number of procedures

grid.arrange(
  
  ggplot(present,aes(x = days, y = Efficiency)) + 
    geom_point() + 
    labs(x = 'Number of days for registering property') +
    labs(y = 'Efficiency of government spending') +
    geom_smooth(method = "loess", se=TRUE) +
    theme_classic() + theme(axis.text = element_text(size = 10)) +
    stat_regline_equation(label.y = 6, label.x = 200, aes(label = ..eq.label..)) +
    stat_regline_equation(label.y = 5, label.x = 200, aes(label = ..rr.label..)),
  
  ggplot(present,aes(x = procedures, y = Efficiency)) + 
    geom_point() + 
    labs(x = 'Number of procedures for registering property') +
    labs(y = 'Efficiency of government spending') +
    geom_smooth(method = "loess", se=TRUE) +
    theme_classic() + theme(axis.text = element_text(size = 10)) +
    stat_regline_equation(label.y = 6, label.x = 8, aes(label = ..eq.label..)) +
    stat_regline_equation(label.y = 5, label.x = 8, aes(label = ..rr.label..)),
  
  ncol = 1, nrow = 2)

# Trends by year_income_group - Ch. 4

# in Efficiency of government spending
# in % of government debt to GDP by income_group
rename <- c(`High_income` = "High-income",`Low_income` = "Low-income",
            `Lower_middle_income`="Lower middle-income",
            `Upper_middle_income`="Upper middle-income")
grid.arrange(
  ggplot(data=mean_yr_inc_pr, aes(x=as.factor(Year), y=Efficiency, group=income_group))+
    geom_line() + geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = F) + labs(x='Year') + 
    labs(y='Efficiency of government spending') + theme_classic() +
    scale_y_continuous()+
    facet_wrap(~income_group, labeller = as_labeller(rename), ncol = 4),
  ggplot(data=mean_yr_inc_pr, aes(x=as.factor(Year), y=govdbtshgdp, group=income_group))+
    geom_line() + geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = F) + labs(x='Year') + 
    labs(y='% of government debt to GDP') + theme_classic() +
    scale_y_continuous()+
    facet_wrap(~income_group, ncol = 4, labeller = as_labeller(rename)), ncol = 1, nrow = 2)

# Trends by year_region - Appendix A

# in Efficiency of government spending
regions <- c(`East_Asia_and_Pacific` = "East Asia and Pacific",
             `Eurasia` = "Eurasia",
             `Europe_and_North_America`="Europe and North America",
             `Latin_America_and_the_Caribbean`="Latin America and the Caribbean",
             `Middle_East_and_North_Africa`="Middle East and North Africa",
             `South_Asia`="South Asia",
             `Sub_Saharan_Africa`="Sub Saharan Africa")

ggplot(data=mean_yr_reg_pr, aes(x=as.factor(Year), y=Efficiency, group=region))+
  geom_line() + geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = F) + labs(x='Year') +
  labs(y='Efficiency of government spending') + theme_classic() +
  scale_y_continuous()+
  facet_wrap(~region, ncol = 3, labeller = as_labeller(regions))

# in % of government debt to GDP

ggplot(data=mean_yr_reg_pr, aes(x=as.factor(Year), y=govdbtshgdp, group=region))+
  geom_line() + geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = F) + labs(x='Year') +
  labs(y='% of government debt to GDP') + theme_classic() +
  scale_y_continuous()+
  facet_wrap(~region, ncol = 3, labeller = as_labeller(regions))

###### Proceed with the model ######

unique(present$Country) # 145 countries in the sample
unique(present$Year)

## Replication of DeSimone (2019)

rep_re<-plm(Efficiency~opn_bdgt+polity2+govqual+gdp_grwth+log(gdp_per_cap)+unempl
            +govbalshgdp+govdbtshgdp+Year, data = present, index = c("Country", "Year"), 
            model = "random")
# Note: to check for the quality of NA imputing procedure
# set '#' before NA imputing lines of script and run the model again 

# coefficient on budget transparency is insignificant
# initially ~40% of data was missed

pbgtest(rep_re) # H0 - no autocorrelation
bptest(rep_re, studentize = F) # H0 - homoskedasticity
rep_re_hac<-coeftest(rep_re, vcovHC, method = "arellano") #HS robust
stargazer(rep_re, rep_re_hac, type = "text", digits = 4)

## My model

# Add additional variables

# present

# We will use ln(gdp_per_cap) for convenience
present$gdp_per_cap<-log(present$gdp_per_cap)
colnames(present)[colnames(present) == "gdp_per_cap"] <- "gdp_per_cap_ln"

# Create square terms

present$days_sq<-(present$days)^2
present$procedures_sq<-(present$procedures)^2

# create yearly means
# Note: d.days, d.proced vary over time

mean_country <- present %>%
  group_by(Country) %>%
  summarise_at(vars(days, days_sq, procedures, procedures_sq,  
                    opn_bdgt, govqual,
                    absence_corr, polity2, gdp_grwth, gdp_per_cap_ln,
                    unempl, govbalshgdp, govdbtshgdp),
               list(mean))

# Change colnames of some columns
colnames(mean_country)[colnames(mean_country) %in% c(
  "days", "days_sq", "procedures", "procedures_sq", 
  "opn_bdgt", "govqual",
  "absence_corr", "polity2",
  "gdp_grwth", "gdp_per_cap_ln", "unempl", 
  "govbalshgdp", "govdbtshgdp"
)] <- 
  c(
    "days_av", "days_sq_av", "procedures_av", "procedures_sq_av", 
    "opn_bdgt_av", "govqual_av",
    "absence_corr_av", "polity2_av",
    "gdp_grwth_av", "gdp_per_cap_ln_av", "unempl_av", 
    "govbalshgdp_av","govdbtshgdp_av")

# left_join
present<-left_join(present, mean_country, by='Country')
present<-
  data.frame(lapply(present, function(y) if(is.numeric(y)) round(y, 3) else y))

# lagged

# We will use ln(gdp_per_cap) for convenience
lagged$gdp_per_cap<-log(lagged$gdp_per_cap)
colnames(lagged)[colnames(lagged) == "gdp_per_cap"] <- "gdp_per_cap_ln"

# Create square terms

lagged$days_sq<-(lagged$days)^2
lagged$procedures_sq<-(lagged$procedures)^2

# create yearly means
# Note: d.days, d.proced vary over time

mean_country <- lagged %>%
  group_by(Country) %>%
  summarise_at(vars(days, days_sq, procedures, procedures_sq,  
                    opn_bdgt, govqual,
                    absence_corr, polity2, gdp_grwth, gdp_per_cap_ln,
                    unempl, govbalshgdp, govdbtshgdp),
               list(mean))

# Change colnames of some columns
colnames(mean_country)[colnames(mean_country) %in% c(
  "days", "days_sq", "procedures", "procedures_sq", 
  "opn_bdgt", "govqual",
  "absence_corr", "polity2",
  "gdp_grwth", "gdp_per_cap_ln", "unempl", 
  "govbalshgdp", "govdbtshgdp"
)] <- 
  c(
    "days_av", "days_sq_av", "procedures_av", "procedures_sq_av", 
    "opn_bdgt_av", "govqual_av",
    "absence_corr_av", "polity2_av",
    "gdp_grwth_av", "gdp_per_cap_ln_av", "unempl_av", 
    "govbalshgdp_av","govdbtshgdp_av")

# left_join
lagged<-left_join(lagged, mean_country, by='Country')
lagged<-
  data.frame(lapply(lagged, function(y) if(is.numeric(y)) round(y, 3) else y))

# How does the number of days and procedures needed to register a property
# affect the efficiency of government spending?

# Is there an optimal time devoted to bureaucracy?
# Check for quadratic relationship

# present

mod_sq_cre<-plm(Efficiency~days+
                  days_sq+procedures+procedures_sq
                +absence_corr+opn_bdgt+govqual+polity2+gdp_grwth+gdp_per_cap_ln+
                  unempl+govbalshgdp+govdbtshgdp+days_av+days_sq_av+procedures_av+
                  procedures_sq_av+absence_corr_av+
                  opn_bdgt_av+govqual_av+polity2_av+gdp_grwth_av+gdp_per_cap_ln_av+unempl_av+govbalshgdp_av+
                  govdbtshgdp_av+Year+region+income_group, 
                data = present, index = c("Country", "Year"), model = "random")    

pbgtest(mod_sq_cre) # H0 - no autocorrelation
bptest(mod_sq_cre, studentize = F) # H0 - homoskedasticity
mod_sq_cre_hc<-coeftest(mod_sq_cre, vcovHC) #HC robust
stargazer(mod_sq_cre, mod_sq_cre_hc, type = 'text', digits = 4)

# lagged

lmod_sq_cre<-plm(Efficiency~days+
                   days_sq+procedures+procedures_sq
                 +absence_corr+opn_bdgt+govqual+polity2+gdp_grwth+gdp_per_cap_ln+
                   unempl+govbalshgdp+govdbtshgdp+days_av+days_sq_av+procedures_av+
                   procedures_sq_av+absence_corr_av+
                   opn_bdgt_av+govqual_av+polity2_av+gdp_grwth_av+gdp_per_cap_ln_av+unempl_av+govbalshgdp_av+
                   govdbtshgdp_av+Year+region+income_group, 
                 data = lagged, index = c("Country", "Year"), model = "random")

pbgtest(lmod_sq_cre) # H0 - no autocorrelation
bptest(lmod_sq_cre, studentize = F) # H0 - homoskedasticity
lmod_sq_cre_hc<-coeftest(lmod_sq_cre, vcovHC) #HC robust
stargazer(lmod_sq_cre, lmod_sq_cre_hc, type = 'text', digits = 4)

# Finding turning point

b1_days<-lmod_sq_cre$coefficients[2]
b2_days<-lmod_sq_cre$coefficients[3]
days_turn_p<-(-b1_days)/(2*b2_days)
days_turn_p
sum(present$days>=days_turn_p)
sum(present$days>=days_turn_p)/sum(!is.na(present$days))*100 

# No squared relationship was detected

#### Proceed with the model

### full sample

## present

modFull_cre<-plm(Efficiency~days+procedures+absence_corr+opn_bdgt+govqual+polity2+gdp_grwth+gdp_per_cap_ln+
                   unempl+govbalshgdp+govdbtshgdp+days_av+procedures_av+absence_corr_av+
                   opn_bdgt_av+govqual_av+polity2_av+gdp_grwth_av+gdp_per_cap_ln_av+unempl_av+govbalshgdp_av+
                   govdbtshgdp_av+Year+region, 
                 data = present, index = c("Country", "Year"), model = "random")

pbgtest(modFull_cre) # H0 - no autocorrelation
bptest(modFull_cre, studentize = F) # H0 - homoskedasticity
modFull_cre_hc<-coeftest(modFull_cre, vcovHC) #HS robust sd
stargazer(modFull_cre, modFull_cre_hc, type = 'text', digits = 4)

# sometimes we receive the following error:
# Error in solve.default(crossprod(ZBeta)) : system is computationally singular
# it could be fixed by excluding govqual variable from the equation
# run the model without govqual variable and compare the results
# possible reason of the error - multicollinearity

modFull_cre2<-plm(Efficiency~days+procedures+absence_corr+opn_bdgt+polity2+gdp_grwth+gdp_per_cap_ln+
                    unempl+govbalshgdp+govdbtshgdp+days_av+procedures_av+absence_corr_av+
                    opn_bdgt_av+polity2_av+gdp_grwth_av+gdp_per_cap_ln_av+unempl_av+govbalshgdp_av+
                    govdbtshgdp_av+Year+region, 
                  data = present, index = c("Country", "Year"), model = "random")

pbgtest(modFull_cre2) # H0 - no autocorrelation
bptest(modFull_cre2, studentize = F) # H0 - homoskedasticity
modFull_cre2_hc<-coeftest(modFull_cre2, vcovHC) #HS robust sd
stargazer(modFull_cre_hc, modFull_cre2_hc, type = 'text', digits = 4)
stargazer(modFull_cre, modFull_cre2, type = 'text', digits = 4)


## lagged

lmodFull_cre<-plm(Efficiency~days+procedures+absence_corr+opn_bdgt+govqual+polity2+gdp_grwth+gdp_per_cap_ln+
                    unempl+govbalshgdp+govdbtshgdp+days_av+procedures_av+absence_corr_av+
                    opn_bdgt_av+govqual_av+polity2_av+gdp_grwth_av+gdp_per_cap_ln_av+unempl_av+govbalshgdp_av+
                    govdbtshgdp_av+Year+region, 
                  data = lagged, index = c("Country", "Year"), model = "random")

pbgtest(lmodFull_cre) # H0 - no autocorrelation
bptest(lmodFull_cre, studentize = F) # H0 - homoskedasticity
lmodFull_cre_hc<-coeftest(lmodFull_cre, vcovHC) #HS robust sd
stargazer(lmodFull_cre, lmodFull_cre_hc, type = 'text', digits = 4)


### subset
# subsets are unbalanced panels
# it is easier to estimate fixed effects as the estimates are the same
# and dummies from the full regression now serves as subset conditions

## present

#number of days is less or equal than (median+sd)
redundancy_days<-median(present$days)+sd(present$days)

modFull_fe1<-plm(Efficiency~days+procedures+absence_corr+opn_bdgt+govqual+polity2+gdp_grwth+gdp_per_cap_ln+
                   unempl+govbalshgdp+govdbtshgdp+Year, 
                 data = subset(present, present$days <= redundancy_days), index = c("Country", "Year"), model = "within")

pbgtest(modFull_fe1) # H0 - no autocorrelation
bptest(modFull_fe1, studentize = F) # H0 - homoskedasticity
modFull_fe1_hc<-coeftest(modFull_fe1, vcovHC(modFull_fe1, method = 'arellano')) #HS robust sd
stargazer(modFull_fe1, modFull_fe1_hc, type = 'text', digits = 4)
stargazer(modFull_fe1_hc, type = 'text', digits = 4)

#number of days is greater than (median+sd)
modFull_fe2<-plm(Efficiency~days+procedures+absence_corr+opn_bdgt+govqual+polity2+gdp_grwth+gdp_per_cap_ln+
                   unempl+govbalshgdp+govdbtshgdp+Year, 
                 data = subset(present, present$days > redundancy_days), index = c("Country", "Year"), model = "within")

pbgtest(modFull_fe2) # H0 - no autocorrelation
bptest(modFull_fe2, studentize = F) # H0 - homoskedasticity
modFull_fe2_hc<-coeftest(modFull_fe2, vcovHC(modFull_fe2, method = 'arellano')) #HS robust sd
stargazer(modFull_fe2, modFull_fe2_hc, type = 'text', digits = 4)
stargazer(modFull_fe2_hc, type = 'text', digits = 4)

#number of procedures is less than (median+sd)
redundancy_proced<-median(present$procedures)+sd(present$procedures)
modFull_fe3<-plm(Efficiency~days+procedures+absence_corr+opn_bdgt+govqual+polity2+gdp_grwth+gdp_per_cap_ln+
                   unempl+govbalshgdp+govdbtshgdp+Year, 
                 data = subset(present, present$procedures <= redundancy_proced), index = c("Country", "Year"), model = "within")

pbgtest(modFull_fe3) # H0 - no autocorrelation
bptest(modFull_fe3, studentize = F) # H0 - homoskedasticity
modFull_fe3_hc<-coeftest(modFull_fe3, vcovHC(modFull_fe3, method = 'arellano')) #HS robust sd
stargazer(modFull_fe3, modFull_fe3_hc, type = 'text', digits = 4)
stargazer(modFull_fe3_hc, type = 'text', digits = 4)

#number of procedures is greater than (median+sd)
modFull_fe4<-plm(Efficiency~days+procedures+absence_corr+opn_bdgt+govqual+polity2+gdp_grwth+gdp_per_cap_ln+
                   unempl+govbalshgdp+govdbtshgdp+Year, 
                 data = subset(present, present$procedures > redundancy_proced), index = c("Country", "Year"), model = "within")

pbgtest(modFull_fe4) # H0 - no autocorrelation
bptest(modFull_fe4, studentize = F) # H0 - homoskedasticity
modFull_fe4_hc<-coeftest(modFull_fe4, vcovHC(modFull_fe4, method = 'arellano')) #HS robust sd
stargazer(modFull_fe4, modFull_fe4_hc, type = 'text', digits = 4)
stargazer(modFull_fe4_hc, type = 'text', digits = 4)

# absence of corruption value is above average
modFull_fe5<-plm(Efficiency~days+procedures+absence_corr+opn_bdgt+govqual+polity2+gdp_grwth+gdp_per_cap_ln+
                   unempl+govbalshgdp+govdbtshgdp+Year, 
                 data = subset(present, absence_corr > mean(absence_corr)), 
                 index = c("Country", "Year"), model = "within")

pbgtest(modFull_fe5) # H0 - no autocorrelation
bptest(modFull_fe5, studentize = F) # H0 - homoskedasticity
modFull_fe5_hc<-coeftest(modFull_fe5, vcovHC(modFull_fe5, method = 'arellano')) #HS robust sd
stargazer(modFull_fe5, modFull_fe5_hc, type = 'text', digits = 4)
stargazer(modFull_fe5_hc, type = 'text', digits = 4)

# absence of corruption value is below average
modFull_fe6<-plm(Efficiency~days+procedures+absence_corr+opn_bdgt+govqual+polity2+gdp_grwth+gdp_per_cap_ln+
                   unempl+govbalshgdp+govdbtshgdp+Year, 
                 data = subset(present, absence_corr < mean(absence_corr)), 
                 index = c("Country", "Year"), model = "within")

pbgtest(modFull_fe6) # H0 - no autocorrelation
bptest(modFull_fe6, studentize = F) # H0 - homoskedasticity
modFull_fe6_hc<-coeftest(modFull_fe6, vcovHC(modFull_fe6, method = 'arellano')) #HS robust sd
stargazer(modFull_fe6, modFull_fe6_hc, type = 'text', digits = 4)
stargazer(modFull_fe6_hc, type = 'text', digits = 4)


# by income_group
table(present$income_group)

# High income countries
modFull_HI_fe<-plm(Efficiency~days+procedures+absence_corr+opn_bdgt+govqual+polity2+gdp_grwth+gdp_per_cap_ln+
                     unempl+govbalshgdp+govdbtshgdp+Year, 
                   data = subset(present, income_group == "High_income"), 
                   index = c("Country", "Year"), model = "within")

pbgtest(modFull_HI_fe) # H0 - no autocorrelation
bptest(modFull_HI_fe, studentize = F) # H0 - homoskedasticity
modFull_HI_fe_hc<-coeftest(modFull_HI_fe, vcovHC(modFull_HI_fe, method = 'arellano')) #HS robust
stargazer(modFull_HI_fe, modFull_HI_fe_hc, type = 'text', digits = 4)
stargazer(modFull_HI_fe_hc, type = 'text', digits = 4)

# Upper-middle income countries
modFull_UMI_fe<-plm(Efficiency~days+procedures+absence_corr+opn_bdgt+govqual+polity2+gdp_grwth+gdp_per_cap_ln+
                      unempl+govbalshgdp+govdbtshgdp+Year, 
                    data = subset(present, income_group == "Upper_middle_income"), 
                    index = c("Country", "Year"), model = "within")

pbgtest(modFull_UMI_fe) # H0 - no autocorrelation
bptest(modFull_UMI_fe, studentize = F) # H0 - homoskedasticity
modFull_UMI_fe_hc<-coeftest(modFull_UMI_fe, vcovHC(modFull_UMI_fe, method = 'arellano')) #HC robust
stargazer(modFull_UMI_fe, modFull_UMI_fe_hc, type = 'text', digits = 4)
stargazer(modFull_UMI_fe_hc, type = 'text', digits = 4)

# Lower middle income countries
modFull_LMI_fe<-plm(Efficiency~days+procedures+absence_corr+opn_bdgt+govqual+polity2+gdp_grwth+gdp_per_cap_ln+
                      unempl+govbalshgdp+govdbtshgdp+Year, 
                    data = subset(present, income_group == "Lower_middle_income"), 
                    index = c("Country", "Year"), model = "within")

pbgtest(modFull_LMI_fe) # H0 - no autocorrelation
bptest(modFull_LMI_fe, studentize = F) # H0 - homoskedasticity
modFull_LMI_fe_hc<-coeftest(modFull_LMI_fe, vcovHC(modFull_LMI_fe, method = 'arellano')) #HC robust
stargazer(modFull_LMI_fe, modFull_LMI_fe_hc, type = 'text', digits = 4)
stargazer(modFull_LMI_fe_hc, type = 'text', digits = 4)

# Low income countries - not enough observations

## Robustness check

# Normalise dependent variable from 0 to 1


# present dataset
present$Efficiency_norm <- 
  (present$Efficiency-min(present$Efficiency))/
  (max(present$Efficiency)-min(present$Efficiency))
summary(present$Efficiency_norm)

# lagged dataset
lagged$Efficiency_norm <- 
  (lagged$Efficiency-min(lagged$Efficiency))/
  (max(lagged$Efficiency)-min(lagged$Efficiency))
summary(lagged$Efficiency_norm)

# run PLDV


# present

modFull_cre_norm<-pldv(Efficiency_norm~days+procedures+absence_corr+opn_bdgt+govqual+polity2+gdp_grwth+gdp_per_cap_ln+
                         unempl+govbalshgdp+govdbtshgdp+days_av+procedures_av+absence_corr_av+
                         opn_bdgt_av+govqual_av+polity2_av+gdp_grwth_av+gdp_per_cap_ln_av+unempl_av+govbalshgdp_av+
                         govdbtshgdp_av+Year+region, 
                       data = present, index = c("Country", "Year"), model = "random")
summary(modFull_cre_norm)
stargazer(modFull_cre_hc, type = "text", digits = 4) #to compare

# lagged

lmodFull_cre_norm<-pldv(Efficiency_norm~days+procedures+absence_corr+opn_bdgt+govqual+polity2+gdp_grwth+gdp_per_cap_ln+
                          unempl+govbalshgdp+govdbtshgdp+days_av+procedures_av+absence_corr_av+
                          opn_bdgt_av+govqual_av+polity2_av+gdp_grwth_av+gdp_per_cap_ln_av+unempl_av+govbalshgdp_av+
                          govdbtshgdp_av+Year+region, 
                        data = lagged, index = c("Country", "Year"), model = "random")
summary(lmodFull_cre_norm)
stargazer(lmodFull_cre_hc, type = "text", digits = 4) #to compare

