# load libraries

loadmanylibs <- c('ggplot2','corrplot','gridExtra', 'dplyr', 'car', 'lmtest', 'statmod',
                  'ggalt', 'dplyr', 'readxl', 'plm', 'data.table', 'sandwich',
                  'writexl', 'lares', 'ggpubr', 'stargazer', 'fastDummies', 'Hmisc')
#install.packages(loadmanylibs)
lapply(loadmanylibs, require, character.only = TRUE)

###### Clean data ######

data <- read_excel("C:/Users/u04a7wq/Documents/thesis/data.xlsx")
names(data)

# Transform variable

data$Year<-as.factor(data$Year)

# Rename variables for convenience

names(data) <- gsub(pattern = "corr", replacement = "absence_corr", x = names(data))


# Select variables

all<-data.frame(data$Year, data$Country, data$Efficiency, data$govdbtshgdp, 
                data$govbalshgdp, 
                data$opn_bdgt, data$polity2, 
                data$govqual, data$gdp_grwth, 
                data$gdp_per_cap, data$unempl, 
                data$procedures, data$days, 
                data$absence_corr)
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

# Turn some variables from character to numeric

all_num <-all %>% 
  mutate_at(c(3:14), as.numeric)

# Round values to 3 digits after a coma

all_num_round <- 
  data.frame(lapply(all_num, function(y) if(is.numeric(y)) round(y, 3) else y)) 

present <- all_num_round
  
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


class(present)

# turn grouped_df back to data.frame format

present<-as.data.frame(present)

# save this data
filepath <- file.path("C:/Users/пк/Desktop/bureaucracy/processed_data")
save(present,file = file.path(filepath,"cleandata.RData"))

# Data visualisation

# add a corplot

num_cols_pr <- unlist(lapply(present, is.numeric))
corrplot_pr <- present[,num_cols_pr] # correlations of all numeric variables
cor_numvar_pr <- cor(corrplot_pr, use="pairwise.complete.obs")
colnames(cor_numvar_pr) <- 
  c("Government spending efficiency", "Government debt % of gdp", "Budget balance % of gdp", "Budget transparency", "Government effectiveness",
    "Democracy", "GDP growth", "GDP per capita", "Unemployment", "Procedures to register a property",
    "Days to register a property", "Absence of corruption")
rownames(cor_numvar_pr) <-colnames(cor_numvar_pr)

corrplot(cor_numvar_pr, type="full", tl.col="black", method="color")

# add a linear plot

grid.arrange(
  
  ggplot(present,aes(x = days, y = Efficiency)) + 
    geom_point() + 
    labs(x = 'Number of days to register a property') +
    labs(y = 'Government spending efficiency') +
    geom_smooth(method = "loess", se=TRUE) +
    theme_classic() + theme(axis.text = element_text(size = 10)) +
    stat_regline_equation(label.y = 6, label.x = 200, aes(label = ..eq.label..)) +
    stat_regline_equation(label.y = 5, label.x = 200, aes(label = ..rr.label..)),
  
  ggplot(present,aes(x = procedures, y = Efficiency)) + 
    geom_point() + 
    labs(x = 'Number of procedures to register a property') +
    labs(y = 'Government spending efficiency') +
    geom_smooth(method = "loess", se=TRUE) +
    theme_classic() + theme(axis.text = element_text(size = 10)) +
    stat_regline_equation(label.y = 6, label.x = 8, aes(label = ..eq.label..)) +
    stat_regline_equation(label.y = 5, label.x = 8, aes(label = ..rr.label..)),
  
  ncol = 1, nrow = 2)

# add grouped plot

# Trends by year_income_group 

# create yearly average data
# Summary statistics by year_income_group
mean_yr_inc_pr <- present %>%
  group_by(Year, income_group) %>%
  summarise_at(vars(Efficiency:absence_corr), list(mean))
mean_yr_inc_pr<-
  data.frame(lapply(mean_yr_inc_pr, function(y) if(is.numeric(y)) round(y, 3) else y))
#save grouped by income yearly averaged data
save(mean_yr_inc_pr,file = file.path(filepath,"yrav_byincgr.RData"))

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
