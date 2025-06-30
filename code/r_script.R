# A few notes about this script.
# 
# This is a mock script that walks through the steps of analyzing mammal data using generalized linear models. It uses slightly modified data from a real study on brown bear livestock predation in Romania that was published in Conservation Science and Practice.    
# 
# [Mihai I. Pop, Marissa A. Dyck, Silviu Chiriac, Berde Lajos, Szilárd Szabó, Cristian I. Iojă, Viorel D. Popescu. (2023). Predictors of brown bear predation events on livestock in the Romanian Carpathians](https://conbio.onlinelibrary.wiley.com/doi/full/10.1111/csp2.12884)  
# 
# This code is derived from course materials developed by Dr. Marissa A. Dyck for undergraduate & graduate coursework at the University of Victoria, as well as international R workshops. The course is free and available online through [Dr. Dyck's GitHub](https://marissadyck.github.io/R-crash-course.github.io/). You may find this a helpful resource for further developing your coding knowledge and skills beyond what we can cover in this workshop.  
# 
# If you have question please email the author,   
# 
# Marissa A. Dyck   
# Postdoctoral research fellow    
# University of Victoria    
# School of Environmental Studies     
# Email: [marissadyck17@gmail.com](marissadyck17@gmail.com)    


# R and RStudio -----------------------------------------------------------

# Before starting you should ensure you have the latest version of R and RStudio downloaded. This code was generated under R version 4.2.3 and with RStudio version 2024.04.2+764.    
# 
# You can download R and RStudio [HERE](https://posit.co/download/rstudio-desktop/)  


# Install packages --------------------------------------------------------


# If you don't already have the following packages installed, use the code below to install them. *NOTE this will not run automatically as eval=FALSE is included in the chunk setup (i.e. I don't want it to run every time I run this code since I have the packages installed)

install.packages('tidyverse') 
install.packages('MuMIn')
install.packages('car')
install.packages('lme4')
install.packages('PerformanceAnalytics')
install.packages('broom')


# Load libraries ----------------------------------------------------------

# Then load the packages to your library so they are available for use during this current R session. I have this chode chunk set to message=FALSE so that my knitted doc doesn't print all the info about each library that is normally printed in the console.

library(tidyverse) # for data formatting, cleaning, and much more!
library(PerformanceAnalytics) # for generating correlation matrix plots 
library(lme4) # for fitting glms
library(car) # companion package for glm analysis with additional functions
library(MuMIn) # for model selection
library(broom) # extracting odds ratios in a tidy format



# Data --------------------------------------------------------------------


# README ------------------------------------------------------------------

# As previously mentioned, this data is a slightly modified version of data associated with [Pop et al., 2023](https://conbio.onlinelibrary.wiley.com/doi/full/10.1111/csp2.12884). There is a published GitHub repository on Dr. Dyck's GitHub with the final data and analysis scripts from the publication if you are interested. 
# 
# [Brown Bear Predation GitHub repository](https://github.com/marissadyck/Brown_bear_predation_RO)  
# 
# Although the data is slightly different, the README that was published with the final analysis should serve as a good enough reference for the data we are using if you want more information about the data collected.  
# 
# [Brown Bear README](data/bear_README.html)  



# Import data -------------------------------------------------------------

# This code will read in the data as a tibble and save it to the environment with a descriptive and tidy name - this is essential for well organized reproducible research to avoid errors with coding. Avoid naming your data files as 'data' or 'dat' for example, because as your workflow gets more complex you may be importing several datasets for a single project or you may be working on several projects at a time in one R session - if all your data are named very similarly or the exact same thing you can easily reference the wrong data set.
# 
# In the same code chunk we will also do a bit of data tidying that I consider the standard now for all of my analyses to ease coding, reduce errors, and increase reproduce-ability. 
# 
# * First, we will set all the column names to lowercase - this reduces keystrokes and possible case sensitive errors while coding  
# 
# * Then we will specify how each of the variables should be read in (e.g. factor, numeric, etc.). This will also reduce potential errors later in the process as R often misinterprets how to read in data. You should always be familiar enough with your data before beginning any analysis to complete this section (i.e. you should know what each column is, how it was measured, and ideally what format you need it to be in for your analysis - some variables can be coded in several ways which are all correct depending on what you are doing with the data). **This is wear README files come in: You have to familiarize yourself with the data before beginning any analysis, and to have truly reproducible research when you publish or share your analysis you will need to have a thorough explanation of your data for someone else. This is all included in a README file and I recommend starting one at this phase in your process if not sooner!**
#   
#   * Lastly, we will check the structure of the data and make sure things read in properly, and make any changes to this code if necessary  

# read in the bear data and do some data tidying
bear_damage <- read_csv('data/raw/pagube_2008_2016_spatial.csv',
                        
                        # specify how the columns are read in
                        col_types = cols(Damage = col_factor(),
                                         Year = col_factor(),
                                         Month = col_factor(),
                                         Targetspp = col_factor(),
                                         Landcover_code = col_factor(),
                                         .default = col_number())) %>% 
  
  # set all column names to lowercase
  rename_with(tolower)

# check the internal structure of the data 
str(bear_damage)


# Data checks and cleaning ------------------------------------------------


# Now we will do some mock data checks and data cleaning. This doesn't necessarily reflect what you would need to do with this exact data but provides some examples of things to check and gives you practice with different code. 


# Years -------------------------------------------------------------------

# check that year is correct
summary(bear_damage$year)


# Months ------------------------------------------------------------------

# check that month is correct 
summary(bear_damage$month)

# >You'll notice after checking that there aren't any 1's, that is because brown bears are hibernating during this period and thus there were no records of damage for January. These are important details to know, check, and make note of in your code and README for reproducible science!


# Filter data -------------------------------------------------------------

# Much of the spatial data for this dataset are represented as proportion (e.g., proportion of different types of habitat on the landscape). If we expect that our proportional data should all sum to 100 we can check that for each site and remove any sites (rows) that don't as a data cleaning step. The code below will do this. 
# 
# We also may be interested in filtering out observations where a lot of animals were involved (likely these are smaller livestock such as chickens etc.), if we only want to use data where a certain number of livestock were killed we can do that data cleaning step here as well.
# 
# We will assign a new data set to the environment for this step so we can compare with the old data

# create new data with prop_check column and filter out observations that don't sum to 100
bear_damage_tidy <- bear_damage %>% 
  
  # create a column that sums across rows of spatial data
  mutate(prop_check = rowSums(across(contains('prop')))) %>% 
  
  # filter to 100 and only livestock events with 10 or fewer animals
  filter(prop_check == 100 &
           livestock_killed <= 10) 

# check new data
summary(bear_damage_tidy)



# Remove old data ---------------------------------------------------------

# remove old data
rm(bear_damage)



# Summary statistics ------------------------------------------------------

# Often times for publications, reports, or other deliverables we need to provide some summary information about the raw data we collected. Besides that, this is a great way to begin to explore your data prior to conducting any formal analyses.


# Total events ------------------------------------------------------------

# First we will calculate the total number of predation *events*, for this dataset, that is anything in the damage column that is coded as a one where zeros represent pseudoabsence events. We will also calculate the total number of livestock killed across all events.
# 
# Remember we are using the 'tidy' dataset which doesn't include all of the raw data. Depending on your needs you may want to use the messier raw data to report your summary stats

# total number of events & total number of livestock killed

# with summary we can look at the number of events (1s in the damage column)
summary(bear_damage_tidy$damage)

# or with summarise we can calculate both 
bear_damage_tidy %>% 
  
  # ensure to only count events of damage
  filter(damage == '1') %>% 
  
  summarise(n_events = n(),
            total_killed = sum(livestock_killed))


# Events per livestock type -----------------------------------------------

# In this data there are several types of livestock that are affected, we may want to report specifics about each type of livestock. For that we can group our data before calculating some summary info.

# damage per livestock type (target species)
bear_damage_tidy %>% 
  
  # ensure to only count events of damage not pseudoabsences
  filter(damage == '1') %>% 
  
  # group by targetspp to get summaries for each species and year
  group_by(targetspp) %>% 
  
  # calculate total number of events (n)
  summarise(n = n())

# bovine highest, alte lowest


# Events per year ---------------------------------------------------------

# This data was also collected over several years, so we may want to report some summary statistics for each year

# damage per year
bear_damage_tidy %>% 
  
  # ensure to only count events of damage
  filter(damage == '1') %>% 
  
  # group by targetspp to get summaries for each species and year
  group_by(year) %>% 
  
  # calculate total number of events (n)
  summarise(n = n()) %>% 
  
  # sort by largest to smallest number of events
  arrange(desc(n))

# 2012 had highest number of events and 2008 had lowest number of events


# Events per month --------------------------------------------------------

# We may also be interested in the monthly number of events

# damage per month
bear_damage_tidy %>% 
  
  # ensure to only count events of damage
  filter(damage == '1') %>% 
  
  # group by targetspp to get summaries for each species and year
  group_by(month) %>% 
  
  # calculate total number of events (n)
  summarise(n = n()) %>% 
  
  # sort by largest to smallest number of events
  arrange(desc(n))

# September had the highest number of events and Dec/Jan had lowest with 0


# Analysis prep -----------------------------------------------------------


# Determine distribution --------------------------------------------------

# We are going to analyze this data in a generalized linear model framework, first we will take a look at our potential response variable/s and determine which to use if there are more than one option and which distribution is appropriate for our model based on the response variable 

# damage is on epossible response variable and the appropriate distribution is binomial as it is 0/1 data
plot(bear_damage_tidy$damage)

# or livestock killed is response variable which is count data so could be poisson or if highly zero-inflated causes overdispersion then negative binomial
hist(bear_damage_tidy$livestock_killed)

# lots of zeros lets do a quick test for dispersion with a simple glm
test_glm <- 
  glm(livestock_killed ~ bear_abund,
      data = bear_damage_tidy,
      family = 'poisson')

summary(test_glm)

# calculate dispersion which is residual deviance / degrees of freedom
1916.5/1118
# 1.74 is high so over-dispersed - use negative binomial


# Explanatory variable data exploration -----------------------------------

# Before we can run an analysis we need to do some exploration of our explanatory variables as well. 
# 
# First we will plot histograms of each of our potential variables to insepct the data for issues and ensure there is enough variability to use each of our variables if we want
# 
# We could plot each with its own line of code or we could run an iteration using a handy tidyverse package called purrr; which is what the code below will do


# using purr to generate histograms of each expl. variable
bear_damage_tidy %>% 
  
  # select only numeric variables 
  select_if(is.numeric) %>% 
  
  # use imap which will retain both the data (x) and the variable names (y)
  imap(~.x %>% 
         
         # use the hist function on the data from previous pipe
         hist(.,
              
              # set the main title to y (each variable)
              main = .y))

# Once we have looked at this we can drop any columns of data that aren't usable or try to merge them with other variables if appropriate and check for multicolinearity between variables which is an assumption we need to meet for GLMs

bear_damage_tidy %>% 
  
  # choose specific variables of interest
  select(bear_abund,
         altitude,
         dist_to_forest,
         dist_to_town,
         shannondivindex,
         prop_pasture,
         prop_deciduous,
         prop_coniferous,
         prop_mixedforest,
         prop_grassland) %>% 
  
  chart.Correlation()

# Above the diagonal of this matrix shows us Pearson's R correlation coefficient for each pairwise combination of our chosen explanatory variables, the diagonal has the name of each variable with a histogram of the raw data, and below the diagonal is a correlation plot of each pairwise combination of variables. 
# 
# We don't want to include any variables that are highly correlated (absolute value r > 0.6) in the same model, so we will write some notes here about any that violate this or are close  
# 
# altitude, prop_coniferous = 0.68  
# altitude, dist_to_town = 0.56 
# prop_deciduous, prop_coniferous = -0.55 ( these are often inversely correlated in forested areas as they are the main two tree types so if there is a lot of one there's less of the other, it's not too high but if we know this ecological relationship exists we may want to be cautious about including them in the same model) 


# Data formatting ---------------------------------------------------------

# Once we've explored both our explanatory and response variables, we may want to do some reformatting to our data. This can be for several reasons, if variables are correlated or lacking enough data to use one individually we may combine them if ecologically justified, etc.
# 
# Here we will create a new variable that combines all the forest cover data because we are interested in overall forest cover not the effect of specific forest types. 

# formatting data to combine variables
bear_damage_tidy <-  bear_damage_tidy %>% 
  
  # add new column that groups all forest types 
  mutate(prop_forest = rowSums(across(c(prop_coniferous, 
                                        prop_deciduous,
                                        prop_mixedforest))))


# check new data
summary(bear_damage_tidy$prop_forest)


# Analysis ----------------------------------------------------------------


# Fit GLMs ----------------------------------------------------------------

# Here we will create a candidate set of models that represent hypotheses about what variables may explain our chosen response and fit these to a glm with the appropriate distribution. As this is a mock analysis and not a course on human-wildlife conflict with bears in eastern europe we won't spend a ton of time justifying these models we will just use a few as examples. 

# Note these are fit to a simple binomial distribution not a negative binomial distribution which is what we determined earlier would truly be the best-fit distribution

# going to scale data first for ease of coding
bear_damage_tidy <- bear_damage_tidy %>% 
  
  # use mutate to change all numeric variables to scaled
  mutate_if(is.numeric, 
            scale)


#I've done 3 models for demonstration

# null model
bear_null <- glm(damage ~ 1,
                 data = bear_damage_tidy,
                 family = 'binomial')

# interaction between distance to forest and distance to town (close to both town and forest would have high prob of damage)
bear_distance_i <- glm(damage ~ dist_to_forest * dist_to_town,
                       data = bear_damage_tidy,
                       family = 'binomial')

# quick check that this model fit
summary(bear_distance_i)

# analog distance model w/o interaction
bear_distance <- glm(damage ~ dist_to_forest +
                       dist_to_town,
                     data = bear_damage_tidy,
                     family = 'binomial')

# quick check that this model fit
summary(bear_distance)


# Model selection ---------------------------------------------------------

# A common approach with GLMs is to use model selection and compare metrics like AIC to determine which set of explanatory variables best explains the observed data we collected. 
# 
# We will do this using a function from the MuMIn package

# model selection on candidate set of models
model.sel(bear_null,
          bear_distance,
          bear_distance_i)

# Lowest AIC and highest model weight generally indicates the best fitting model. The MuMIn function will automatically sort your models in decreasing order of weight
# 
# Based on this our best-fit model is the one with our interaction term


# Check model assumptions -------------------------------------------------

# This section will vary depending on the response variable and other aspects of the data, models, etc. 
# 
# For the best-fit model identified above we want to re-check assumption of independence with variance inflation factor (VIF), we also want to check for overdispersion, and any outliers 

# check assumptions for top model

vif(bear_distance_i)

# plot VIF
vif(bear_distance_i) %>%
  
  # Converts the named vector returned by vif() into a tidy tibble
  enframe(name = 'Predictor', 
          value = 'VIF') %>%
  
  # plot with ggplot
  ggplot(aes(x = reorder(Predictor, VIF), # reorders from smallest VIF to largest 
             y = VIF)) +
  
  # plot as bars
  geom_bar(stat = 'identity', fill = 'skyblue') +
  
  # add labels
  labs(x = 'Predictor',
       y = 'VIF') +
  
  # set theme
  theme_classic()


# dispersion
summary(bear_distance_i)

960.37/1116 # 0.86 slightly under dispersed but not a major issue for glm


# check for observations with high leverage
plot(bear_distance_i) # ignore first three


# Plot Results ------------------------------------------------------------

# There are several ways to plot results, for the purposes of this mock walk through we will plot odds ratios for the explanatory variables in our best-fit model. An odds ratio plot is a great way to show magnitude, confidence, and effect sizes for all your explanatory variables in one figure. 
# 
# First we need to extract the odds ratios in a tidy format

# create a new data frame with the odds ratios
model_odds <- 
  
  # use the broom package to extract odds ratios into a tidy format
  broom::tidy(bear_distance_i,
              exponentiate = TRUE,
              conf.int = TRUE)  %>% 
  
  # remove intercept and interaction term as we don't need to plot
  filter(term  %in% c('dist_to_forest',
                      'dist_to_town'))


# check data
model_odds

# Ignore warning for now as this is a mock analysis 

# Now we need to plot them in a visually pleasing and easily interpretable manner

# plot
# specify data and mapping asesthetics
ggplot(data = model_odds,
       aes(x = term,
           y = estimate)) +
  
  # add points for the odss
  geom_point() +
  
  # add errorbars for the confidence intervals
  geom_errorbar(aes(ymin = conf.low,
                    ymax = conf.high),
                linewidth = 0.5,
                width = 0.4) +
  
  geom_hline(yintercept = 1,
             alpha = 0.5) +
  
  # rename the x axis labels
  scale_x_discrete(labels = c('Distance to forest',
                              'Distance to town')) +
  
  # rename y axis title
  ylab('Odds ratio') +
  
  # flip x and y axis 
  coord_flip() +
  
  # specify theme
  theme_bw() +
  
  # specify theme elements
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())


