# DEPENDENCIES

library(tidyverse)
library(data.table)
library(bundle)
library(butcher)
library(tidymodels)
library(dials)
library(finetune)
library(ranger)
library(doParallel)
library(doRNG)
library(patchwork)
library(pdp)
library(vip)
library(iml)
library(DALEX)
library(DALEXtra)
library(fastshap)
library(shapviz)


# DATA CLEANING FUNCTION ####

clean_df = function(df,
                    background_df = NULL){ 
  # Preprocess the input dataframe to feed the model.
  
  # @ARGS:
  # df -> [data.frame]: The input dataframe containing the raw data
  #(e.g., from PreFer_train_data.csv or PreFer_fake_data.csv).
  # background_df -> [data.frame]: Optional input dataframe containing background 
  #data (e.g., from PreFer_train_background_data.csv or
  #PreFer_fake_background_data.csv).
  
  # @RETURNS:
  # df -> [data.frame]: The cleaned dataframe with only the necessary columns
  #and processed variables.
  
  # setting all datasets as data tables 
  df = setDT(df)
  background_df = setDT(background_df)
  
  # selecting only last information available
  background20 = background_df %>% 
    filter(grepl("^202012", 
                 wave))
  
  # selecting only obs. for which the outcome is available 
  df = df[outcome_available == 1,]
  
  # join with background info for ease of manipulation
  df = df %>%
    left_join(background20, by = "nomem_encr")

  # dplyr's join returns a data.table whose internal self-reference is stale, so
  # the first `:=` below would emit a "shallow copy" warning. Re-registering it
  # here keeps the replication log clean; it has no effect on the data.
  setDT(df)
  
  # extracting subset of predictors ####
  keepcols = c("nomem_encr", # encripted name of respondent
               'age', # age of respondent
               'gender', # gender of respondent
               'intentionB', # fertility intention for future
               'intention2B', # N. of children (intention)
               'intention3B', # Years (intention)
               'health', # Composed Health index
               'limited', # Limitation due to health or emotional problems
               'depress', # Composed depression index
               'implife', # level of satisfaction with achievements in life
               'education', # Education level of respondent
               'partner', # Partner situation of respondent
               'partner_year', # Years of relationship with partner
               'partner_satisfaction', # Partner Satisfaction index
               'coliving', # coliving situation
               'children', # N. of children of respondent
               'partner_children', # N. of living-at-home children (not from respondent)
               'income', # Net income level of respondent
               'migration', # Composed migration background of respondent
               'values_sumc', # Composed traditional value index
               'values_1c',
               'values_2c',
               'values_3c',
               'values_4c',
               'caregiving', # Composed index of pasts caregiving activities  
               'urban', # self-reported neighborhood perception
               'dwelling', # category of dwelling inhabited
               'child_value', # Composed index of children values
               'optimism', # Composed index of optimism of the respondent
               'occupation2', # employment situation of respondent
               'first_age', # Age of the first child of respondent
               'relig', # Composed religiosity index of respondent
               'gynecologist', # is the respondent going to gynecologist (??)
               'domestic', # Domestic situation of the respondent 
               'n_rooms', # N. of rooms of the dwelling
               'mother_help', # Mother availability and support 
               'partner_age', # Interaction term between partner and age
               'gender_emp', # Interaction term between gender and employment
               'soc_lib', # Composed index of political orientation
               'insta' # Usage of social network from respondent
               
  )
  
  # Gender of respondent
  
  # Imputation of gender from background info
  imp.gender = ifelse(is.na(df$cf20m003),df$gender_bg,df$cf20m003)
  
  # Converting gender into factor
  df[, gender := factor(
    imp.gender,
    levels = c(1, 2),
    labels = c('male', 'female'),
    exclude = NULL)]
  
  # Age of respondent
  
  # Ensuring numeric type
  df$birthyear_bg = as.numeric(df$birthyear_bg)
  # Creating the age from the birth year
  age_birth = 2020 - df$birthyear_bg
  
  # Creating classes
  breaks = c(-Inf, 24, 30, 34, 40, Inf)
  lab = c("<=24", "25-30", "31-34", "35-40", ">40")
  df[, age := cut(age_birth, breaks = breaks, labels = lab, right = TRUE)]
  
  # Education level of respondent
  # oplcat_2020 and olpmet_2020
  
  # Coerce to numeric
  df$oplcat_2020 <- as.numeric(df$oplcat_2020)
  df$oplmet_2020 = as.numeric(df$oplmet_2020)
  
  # taking info from oplmet_2020
  df$education = if_else(is.na(df$oplcat_2020),
                         df$oplmet_2020, 
                         df$oplcat_2020)
  
  # now converting 9 to 1 and "squeeze" them into the new "lower-none" category
  df[education == 9, education := 1]
  
  # Coercing to ordered factor
  breaks = c(-Inf, 2, 3, 4, 5, 6, Inf)
  lab = c("lower-none", #low = up to int sec
          "hig_sec", "int_voc", "high_voc", "univ", "missing") 
  df[, education := cut(education, breaks = breaks, labels = lab, right = TRUE)]
  df[is.na(education), education := "missing"]
  
  
  # N. of children *of respondent*
  df$children = ifelse(df$cf20m454==2,0,df$cf20m455)
  
  # retrieving information from the past
  df$children <- ifelse(is.na(df$children),
                        ifelse(df$cf19l454==2, # never had children 2019
                               NA,
                               df$cf19l455), # number of living children in 2019
                        df$children)
  
  df$children <- ifelse(is.na(df$children),
                        ifelse(df$cf18k454==2, #never had children 2018
                               NA,
                               df$cf18k455), # number of living children in 2019
                        df$children)
  
  df$children <- ifelse(is.na(df$children),
                        ifelse(df$cf17j454==2, # never had children 2018
                               NA,
                               df$cf17j455), # number of living children in 2017
                        df$children)
  
  # Fertility intention of respondent 
  # cf20m128: Do you think you will have [more] children in the future?
  # cf20m129: How many [more] children do you think you will have in the future?
  # cf20m130: Within how many years do you hope to have your [next/first] child?
  
  # Intentions from 2020
  df[, intention := factor(
    cf20m128,
    levels = c(1, 2, 3, NA),
    labels = c('yes', 'no', 'notknown', 'missing'),
    exclude = NULL
  )]
  
  df[, intention2 := cut(
    cf20m129,
    breaks = c(-Inf, 0, # 0
               1, #1
               2, #2
               Inf),#>2
    labels = c("0", "1", "2", ">2"),
    right = T
  )]
  
  # Dealing with NAs
  df[intention == "no", intention2 := "0"]
  df[is.na(intention2), intention2 := "missing"]
  
  # handling weird value, such as 2025
  df[cf20m130>2020, cf20m130 := cf20m130-2020]
  
  df[, intention3 := cut(
    cf20m130,
    breaks =  c(-Inf,1, # 0-1
                3, # 2-3
                6, # 4-6
                Inf),# >6
    labels = c("0-1", "2-3", "4-6", ">6"),
    right = T
  )]
  
  # Dealing with NAs
  df[intention == "no", intention3 := "no"]
  df[is.na(intention3), intention3 := "missing"]
  
  # Intentions from the past
  df[, intention_1 := factor(
    df[, cf19l128],
    levels = c(1, 2, 3, NA),
    labels = c('yes', 'no', 'notknown', 'missing'),
    exclude = NULL
  )]
  
  df[, intention_2 := factor(
    df[, cf18k128],
    levels = c(1, 2, 3, NA),
    labels = c('yes', 'no', 'notknown', 'missing'),
    exclude = NULL
  )]
  
  df[, intention_3 := factor(
    df[, cf17j128],
    levels = c(1, 2, 3, NA),
    labels = c('yes', 'no', 'notknown', 'missing'),
    exclude = NULL
  )]
  
  
  # Number of children in the future (relatively to past intention)
  # Basically we want to ensure the fact that the past intention did not
  # change because they had a children in the meantime.
  
  # 2019
  df$cf19l129 <- as.numeric(df$cf19l129)
  breaks = c(-Inf, 0, 1, 2, Inf)
  lab = c("0", "1", "2", ">2")
  df[, intention2_1 := cut(cf19l129,
                           breaks = breaks,
                           labels = lab,
                           right = TRUE)]
  # if intention is "no" then intention2 should be 0
  df[intention_1 == "no", intention2_1 := "0"]
  df[is.na(intention2_1), intention2_1 := "missing"]
  
  #2018
  df$cf18k129 <- as.numeric(df$cf18k129)
  breaks = c(-Inf, 0, 1, 2, Inf)
  lab = c("0", "1", "2", ">2")
  df[, intention2_2 := cut(cf18k129,
                           breaks = breaks,
                           labels = lab,
                           right = TRUE)]
  # if intention is "no" then intention2 should be 0
  df[intention_2 == "no", intention2_2 := "0"]
  df[is.na(intention2_2), intention2_2 := "missing"]
  
  # 2017
  df$cf17j129 <- as.numeric(df$cf17j129)
  breaks = c(-Inf, 0, 1, 2, Inf)
  lab = c("0", "1", "2", ">2")
  df[, intention2_3 := cut(cf17j129,
                           breaks = breaks,
                           labels = lab,
                           right = TRUE)]
  # if intention is "no" then intention2 should be 0
  df[intention2_3 == "no", intention2_3 := "0"]
  df[is.na(intention2_3), intention2_3 := "missing"]
  
  # Within how many years do you hope to have your child
  # handling one weird observation in the training set
  df[cf19l130 > 2019, cf19l130 := cf19l130 - 2019] 
  breaks = c(-Inf, 1, 3, 6, Inf)
  lab = c("0-1", "2-3", "4-6", ">6")
  df[, intention3_1 := cut(cf19l130,
                           breaks = breaks,
                           labels = lab,
                           right = TRUE)]
  # if intention is "no" then intention3 should be "no"
  df[intention_1 == "no", intention3_1 := "no"]
  df[is.na(intention3_1), intention3_1 := "missing"]
  
  # handling one weird observation in the training set
  df[cf18k130 > 2018, cf18k130 := cf18k130 - 2018] 
  
  breaks = c(-Inf, 1, 3, 6, Inf)
  lab = c("0-1", "2-3", "4-6", ">6")
  df[, intention3_2 := cut(cf18k130,
                           breaks = breaks,
                           labels = lab,
                           right = TRUE)]
  # if intention is "no" then intention3 should be "no"
  df[intention_2 == "no", intention3_2 := "no"]
  df[is.na(intention3_2), intention3_2 := "missing"]
  
  # handling one weird observation in the training set
  df[cf17j130 > 2017, cf17j130 := cf17j130 - 2017] 
  
  breaks = c(-Inf, 1, 3, 6, Inf)
  lab = c("0-1", "2-3", "4-6", ">6")
  df[, intention3_3 := cut(cf17j130,
                           breaks = breaks,
                           labels = lab,
                           right = TRUE)]
  # if intention is "no" then intention3 should be "no"
  df[intention_3 == "no", intention3_3 := "no"]
  df[is.na(intention3_3), intention3_3 := "missing"]
  
  
  # Retreiving changes in the n. children in the previous 3 years
  df$children_1 <- ifelse(df$cf19l454 == 2, 0, df$cf19l455)
  df$children_1 <-
    ifelse(is.na(df$children_1),
           ifelse(df$cf18k454 == 2, NA, df$cf18k455),
           df$children_1)
  df$children_1 <-
    ifelse(is.na(df$children_1),
           ifelse(df$cf17j454 == 2, NA, df$cf17j455),
           df$children_1)
  df$children_1 <-
    ifelse(is.na(df$children_1),
           ifelse(df$cf16i454 == 2, NA, df$cf16i455),
           df$children_1)
  
  df$children_2 <- ifelse(df$cf18k454 == 2, 0, df$cf18k455)
  df$children_2 <-
    ifelse(is.na(df$children_2),
           ifelse(df$cf17j454 == 2, NA, df$cf17j455),
           df$children_2)
  df$children_2 <-
    ifelse(is.na(df$children_2),
           ifelse(df$cf16i454 == 2, NA, df$cf16i455),
           df$children_2)
  df$children_2 <-
    ifelse(is.na(df$children_2),
           ifelse(df$cf15h454 == 2, NA, df$cf15h455),
           df$children_2)
  df$children_3 <- ifelse(df$cf17j454 == 2, 0, df$cf17j455)
  df$children_3 <-
    ifelse(is.na(df$children_3),
           ifelse(df$cf16i454 == 2, NA, df$cf16i455),
           df$children_2)
  df$children_3 <-
    ifelse(is.na(df$children_3),
           ifelse(df$cf15h454 == 2, NA, df$cf15h455),
           df$children_2)
  #2014 not considered because of change in variables
  
  # New imputed variables 
  df$intentionB <- df$intention
  df$intention2B <- df$intention2
  df$intention3B <- df$intention3
  
  #intention
  df[((intention == "missing") &
        (children == children_1))
     , intentionB := intention_1]
  
  
  df[((intention == "missing") &
        (children == children_2))
     , intentionB := intention_2]
  
  df[((is.na(intention)) &
        (children == children_3))
     , intentionB := intention_3]
  
  #intention2
  df[((intention2 == "missing") &
        (children == children_1))
     , intention2B := intention2_1]
  
  df[((intention2 == "missing") &
        (children == children_2))
     , intention2B := intention2_2]
  
  df[((intention2 == "missing") &
        (children == children_3))
     , intention2B := intention2_3]
  
  #intention3
  df[((intention3 == "missing") &
        (children == children_1))
     , intention3B := intention3_1]
  
  df[((intention3 == "missing") &
        (children == children_2))
     , intention3B := intention3_2]
  
  df[((intention3 == "missing") &
        (children == children_3))
     , intention3B := intention3_3]
  
  
  # Living-at-home children (not from respondent)
  
  df$partner_children = if_else(
    # if n. children living at home > n. living children of respondent
    df$aantalki > df$children,
    # assuming that the children are from the partner
    df$aantalki - df$children,
    0)
  
  df$partner_children = if_else(
    # if n. children of respondent is NA (respondent never had children)
    # but n. children living at home != NA
    is.na(df$children) & !is.na(df$aantalki),
    # assuming children are all from partner
    df$aantalki,
    df$partner_children
  )
  
  # N. of living children
  df[,children := cut(
    children,
    breaks = c(-Inf,0,
               1,
               2,
               Inf), 
    labels = c("0", "1", "2", ">2"),
    right=T
  )]
  
  # N. of children from partner
  df[, partner_children := cut(
    partner_children,
    breaks = c(-Inf,0, 
               1, 
               2, 
               Inf), 
    labels = c("0", "1", "2", ">2"),
    right=T
  )]
  
  # Dealing with NAs
  df[is.na(children), children := "missing"]
  df[is.na(partner_children), partner_children := "missing"]
  
  # Partner and civil status
  # cf20m024 + burgstat_2020
  
  df$partner <- as.numeric(df$cf20m024==1) # have a partner in 2020
  df$burgstat_2020 <- as.numeric(df$burgstat_2020==1) # married in 2020
  
  # imputing partnership info from civil status (2020)
  df$partner <- ifelse(is.na(df$partner),
                       df$burgstat_2020==1, # check for marriage
                       df$partner) # replaces NAs
  
  df = df[burgstat_2020 == 1, partner := 2] # defining married cat
  df = df[is.na(partner), partner := -1]
  breaks = c(-Inf,-1, # missing
             0, # no parner
             1, # yes partner
             Inf) # married
  lab = c("missing",
          "none",
          "partner",
          "married")
  
  df = df[,
          partner := cut(partner, 
                         breaks = breaks, 
                         labels = lab, 
                         right = TRUE)]
  
  # Domestic Situation
  
  df[, domestic := as.factor(woonvorm_2020)]
  df[is.na(woonvorm_2020), domestic := "missing"]
  
  
  # Partnership year (aka relationship duration) 
  # year start relationship (cf20m028)
  
  df$year_start_p <- df$cf20m028
  
  # Imputing information for married people
  df$year_start_p <- 
    if_else(is.na(df$year_start_p)&df$burgstat_2020==1,
            df$cf19l028, df$year_start_p)
  df$year_start_p <- 
    if_else(is.na(df$year_start_p)&df$burgstat_2020==1,
            df$cf18k028, df$year_start_p)
  df$year_start_p <- 
    if_else(is.na(df$year_start_p)&df$burgstat_2020==1,
            df$cf17j028, df$year_start_p)
  df$year_start_p <- 
    if_else(is.na(df$year_start_p)&df$burgstat_2020==1,
            df$cf16i028, df$year_start_p)
  df$year_start_p <- 
    if_else(is.na(df$year_start_p)&df$burgstat_2020==1,
            df$cf15h028, df$year_start_p)
  df$year_start_p <- 
    if_else(is.na(df$year_start_p)&df$burgstat_2020==1,
            df$cf14g028, df$year_start_p)
  df$year_start_p <- 
    if_else(is.na(df$year_start_p)&df$burgstat_2020==1,
            df$cf13f028, df$year_start_p)
  df$year_start_p <- 
    if_else(is.na(df$year_start_p)&df$burgstat_2020==1,
            df$cf12e028, df$year_start_p)
  df$year_start_p <- 
    if_else(is.na(df$year_start_p)&df$burgstat_2020==1,
            df$cf11d028, df$year_start_p)
  df$year_start_p <- 
    if_else(is.na(df$year_start_p)&df$burgstat_2020==1,
            df$cf10c028, df$year_start_p)
  df$year_start_p <- 
    if_else(is.na(df$year_start_p)&df$burgstat_2020==1,
            df$cf09b028, df$year_start_p)
  df$year_start_p <- 
    if_else(is.na(df$year_start_p)&df$burgstat_2020==1,
            df$cf08a028, df$year_start_p)
  
  # Imputing information for unmarried people
  df$year_start_p <- 
    if_else(is.na(df$year_start_p)&df$partner==1,
            df$cf19l028, df$year_start_p)
  df$year_start_p <- 
    if_else(is.na(df$year_start_p)&df$partner==1,
            df$cf18k028, df$year_start_p)
  
  # making sure is a number
  df$year_start_p <- as.numeric(df$year_start_p)
  breaks = c(-Inf, 2007, 2012, 2017, Inf)
  lab = c("v_old", "old", "med", "new") #how new is the couple
  
  # "Age" of the partnership
  df[, 
     year_couple := cut(year_start_p, 
                        breaks = breaks, 
                        labels = lab, 
                        right = TRUE)]
  
  # coercion to factor
  df <- df %>%
    mutate(partner_year = case_when(
      partner == "none" ~ "no partner",
      year_couple == "v_old" ~ "v_old" ,
      year_couple == "old" ~ "old",
      year_couple == "med" ~ "med",
      year_couple == "new" ~ "new",
      TRUE ~ "partner_miss" # with partner but missing year
    ),
    partner_year = as.factor(partner_year))
  
  # coliving situation (with partner)
  # cf20m025
  df$coliving = factor(df$cf20m025,labels = c("yes","no"))
  
  # no partners
  df[partner == "none", coliving := "no_partner"]
  
  # missing value but with partner
  df[is.na(coliving), coliving := "missing"]
  
  # relationship satisfaction 
  # cf20m180: How satisfied are you with your current relationship?
  df$partner_satisfaction = if_else(df$partner == "none", # no partner
                                    0,
                                    df$cf20m180) # partner satisfaction
  
  breaks = c(-Inf,0, 
             6, 
             7, 
             8, 
             9, 
             Inf) 
  lab = c("no_partner","[1-6]","(6-7]","(7-8]",'(8-9]','10' )
  
  # coercion to ordered factor
  df[,partner_satisfaction := cut(df$partner_satisfaction,
                                  breaks = breaks,
                                  labels = lab,
                                  right = T)]
  
  # dealing with NAs
  df[is.na(partner_satisfaction), partner_satisfaction := "missing"]
  
  # occupation 
  # belbezig_2020
  
  df <- df %>%
    mutate(occupation = case_when(
      belbezig_2020 %in% c(1:2) ~ "employee",
      belbezig_2020 %in% c(3) ~ "self-employed", 
      belbezig_2020 %in% c(4:6, 8:14) ~ "not_work" ,
      belbezig_2020 %in% c(7) ~ "in_school",
      TRUE ~ "missing"
    ))
  
  # coercion to factor
  df[, occupation := factor(occupation)]
  
  # income 
  # nettoink_f_20**
  
  # Imputation from 2019
  df$nettoink_f_2020 = ifelse(is.na(df$nettoink_f_2020),
                              df$nettoink_f_2019,
                              df$nettoink_f_2020)
  
  # Imputation from 2018
  df$nettoink_f_2020 <- ifelse(is.na(df$nettoink_f_2020),
                               df$nettoink_f_2018,
                               df$nettoink_f_2020)
  
  df$nettoink_f_2020 <- as.numeric(df$nettoink_f_2020)
  breaks = c(-Inf, 900, 1900, 2400, Inf)
  lab = c("q1", "q2", "q3",  "q4")
  df[, income := cut(nettoink_f_2020, 
                     breaks = breaks, 
                     labels = lab, 
                     right = TRUE)]
  
  # Dealing with NAs
  df[is.na(nettoink_f_2020), income := "missing"]
  
  # migration background
  df <- df %>%
    mutate(migration = case_when(
      migration_background_bg == 0 ~ "Dutch",
      migration_background_bg == 101 ~ "West_1st" ,
      migration_background_bg == 102 ~ "Not_west_1st",
      migration_background_bg == 201 ~ "West_2nd",
      migration_background_bg == 202 ~ "Not_west_2nd",
      TRUE ~ "missing"
    ))
  
  df[,migration := factor(migration)]
  
  # Self-reported health condition of respondent 
  # ch20m004  How would you describe your health, generally speaking?
  
  df$ch20m004 <- as.numeric(df$ch20m004)
  breaks = c(-Inf, 3, Inf)
  lab = c("low", "high")
  df[, health := cut(ch20m004, breaks = breaks, labels = lab, right = TRUE)]
  df[is.na(health), health := "missing"]
  
  # Limitation due to health or emotional problems
  # ch20m020	To what extent did your physical health or emotional problems 
  #           hinder your daily activities over the past month?
  
  df$ch20m020 <- as.numeric(df$ch20m020)
  breaks = c(-Inf, 1, 2, Inf)
  lab = c("no", "hardly", ">= a bit") #limited?
  df[, limited := cut(ch20m020, breaks = breaks, labels = lab, right = TRUE)]
  df[is.na(limited), limited := "missing"]
  
  
  # Self-reported depression index
  # ch20m011	I felt very anxious
  # ch20m012	I felt so down that nothing could cheer me up
  # ch20m013	I felt calm and peaceful
  # ch20m014	I felt depressed and gloomy
  # ch20m015	I felt happy
  
  # Define a vector of new levels in reverse order 
  new_levels <- c(6, 5, 4, 3, 2, 1)
  
  # Convert the two positive items to a factor with the new levels
  df[, ch20m013 := factor(ch20m013, levels = new_levels)]
  df[, ch20m015 := factor(ch20m015, levels = new_levels)]
  
  df$ch20m011 <- as.numeric(df$ch20m011)
  df$ch20m012 <- as.numeric(df$ch20m012)
  df$ch20m013 <- as.numeric(df$ch20m013)
  df$ch20m014 <- as.numeric(df$ch20m014)
  df$ch20m015 <- as.numeric(df$ch20m015)
  
  # Now, sum all the scores
  df[, sum_depr := rowSums(.SD, na.rm = TRUE), 
     .SDcols = c("ch20m011", "ch20m012", "ch20m013", "ch20m014", "ch20m015")]
  
  df$sum_depr <- as.numeric(df$sum_depr)
  breaks = c(-Inf, 0, 10, 14, Inf)
  lab = c("missing", "low", "med", "high") #level of depression
  df[, depress := cut(sum_depr, breaks = breaks, labels = lab, right = TRUE)]
  
  # Personlity (aka "implife")
  # cp20l017	So far I have gotten the important things I want in life
  
  df$cp20l017 <- as.numeric(df$cp20l017)
  breaks = c(-Inf, 4, 6, Inf)
  lab = c("low", "med", "high") 
  df[, implife := cut(cp20l017, breaks = breaks, labels = lab, right = TRUE)]
  df[is.na(implife), implife := "missing"]
  
  # Traditional values: gender attitude to works and traditional attitude
  
  # cv20l109	A working mother relationship with her children can be just as close and warm 
  # as that of a non-working mother.
  # cv20l110	A child that is not yet attending school is likely to suffer the consequences if 
  # his or her mother has a job.
  # cv20l111	Overall, family life suffers the consequences if the mother has a full-time job.
  # cv20l112	Both father and mother should contribute to the family income.
  # cv20l113	The father should earn money, while the mother takes care of the household 
  # and the family.
  # cv20l151	A woman is more suited to rearing young children than a man.
  # cv20l152	It is actually less important for a girl than for a boy to get a good education.
  # cv20l153	Generally speaking, boys can be reared more liberally than girls.
  # cv20l154	It is unnatural for women in firms to have control over men.
  #  
  # Traditional attitudes
  # cv20l124	Married people are generally happier than unmarried people.
  # cv20l125	People that want to have children should get married.
  # cv20l126	A single parent can raise a child just as well as two parents together
  # cv20l127	It is perfectly fine for a couple to live together without marriage intentions.
  # cv20l129	A divorce is generally the best solution if a married couple cannot solve their 
  # marital problems.
  # cv20l130	It is all right for a married couple with children to get divorced.
  
  # To be reversed, so that for all variables higher values = more traditional
  df$cv20l109_rev <- ifelse(is.na(df$cv20l109), NA, 6 - df$cv20l109)
  df$cv20l112_rev <- ifelse(is.na(df$cv20l112), NA, 6 - df$cv20l112)
  df$cv20l126_rev <- ifelse(is.na(df$cv20l126), NA, 6 - df$cv20l126)
  df$cv20l127_rev <- ifelse(is.na(df$cv20l127), NA, 6 - df$cv20l127)
  df$cv20l129_rev <- ifelse(is.na(df$cv20l129), NA, 6 - df$cv20l129)
  df$cv20l130_rev <- ifelse(is.na(df$cv20l130), NA, 6 - df$cv20l130)
  
  
  #Attitudes toward women/mother's work
  df$values_1 <- rowMeans(cbind(df$cv20l109_rev ,df$cv20l110, df$cv20l111), na.rm = TRUE)
  
  #Attitudes toward gender equality
  df$values_2 <- rowMeans(cbind(df$cv20l153, df$cv20l154), na.rm = TRUE)
  
  #Attitudes toward non-marital childbearing
  df$values_3 <- df$cv20l125
  
  #Attitudes toward divorce
  df$values_4 <- rowMeans(cbind(df$cv20l129_rev, df$cv20l130_rev ), na.rm = TRUE)
  
  
  # Composed index
  df$values_sum <- rowMeans(cbind(df$cv20l109_rev ,df$cv20l110, df$cv20l111,
                                  df$cv20l153, df$cv20l154,
                                  df$cv20l125,
                                  df$cv20l129_rev, df$cv20l130_rev ), na.rm = TRUE)
  
  #Let convert the variable to categorical
  variables <- c("values_1", "values_2", "values_3", "values_4", "values_sum")
  
  # Convert columns to numeric
  df[, (variables) := lapply(.SD, as.numeric), .SDcols = variables]
  breaks = c(-Inf, 2, 3, Inf)
  lab = c("modern", "med", "tradit")
  
  # coercing values_1
  df[,values_1c := cut(values_1,
                       breaks = breaks, 
                       labels = lab, 
                       right = TRUE)]
  
  # coercing values_2
  df[,values_2c := cut(values_2,
                       breaks = breaks, 
                       labels = lab, 
                       right = TRUE)]
  # coercing values_3
  df[,values_3c := cut(values_3,
                       breaks = breaks, 
                       labels = lab, 
                       right = TRUE)]
  
  # coercing values_4
  df[,values_4c := cut(values_4,
                       breaks = breaks, 
                       labels = lab, 
                       right = TRUE)]
  
  # coercing values_sum
  df[,values_sumc := cut(values_sum,
                         breaks = breaks, 
                         labels = lab, 
                         right = TRUE)]
  
  # handling NAs
  df[is.na(values_1), values_1c := "missing"]
  df[is.na(values_2), values_2c := "missing"]
  df[is.na(values_3), values_3c := "missing"]
  df[is.na(values_4), values_4c := "missing"]
  df[is.na(values_sum), values_sumc := "missing"]
  
  # Caregiving
  # cs20m063	Did you perform any informal care over the past 12 months?
  # cs20m066	How many hours of informal care [did/do] you provide per week, on average?
  # cw20m450	Do you provide informal care?
  
  df$care <- ifelse(is.na(df$cs20m063), df$cw20m450, df$cs20m063)
  
  df$cs20m066 <- as.numeric(df$cs20m066)
  breaks = c(-Inf, 4, Inf)
  lab = c("little care", "much care")
  df[, caregiving := cut(cs20m066, breaks = breaks, labels = lab, right = TRUE)]
  df[care==2, caregiving := "no care"]
  df[is.na(care), caregiving := "missing"]
  df[care==1&is.na(cs20m066), caregiving := "little care"]
  df[, caregiving := as.factor(caregiving)]
  
  # Self-reported neighborhood conditions 
  df$sted_2020 <- as.numeric(df$sted_2020)
  breaks = c(-Inf, 2.5, 4, Inf)
  lab = c("very urban", "slightly urban", "rural")
  df[, urban := cut(sted_2020, breaks = breaks, labels = lab, right = TRUE)]
  df[is.na(sted_2020), urban := "missing"] 
  
  # Dwelling type
  df$woning_2020 <- as.numeric(df$woning_2020)
  breaks = c(-Inf, 2, 3, Inf)
  lab = c("owner", "rental", "other")
  df[, dwelling := cut(woning_2020, breaks = breaks, labels = lab, right = FALSE)]
  df[is.na(woning_2020), dwelling := "other"]
  
  # Value of children 
  # cv20l131	Children ought to care for their sick parents.
  # cv20l132	When parents reach old age, they should be able to live with their children.
  # cv20l133	Children that live close by ought to visit their parents at least once a week.
  # cv20l134	Children ought to take unpaid leave in order to care for their sick parents.
  
  df$child_value <- rowMeans(cbind(df$cv20l131,
                                   df$cv20l132,
                                   df$cv20l133,
                                   df$cv20l134), na.rm = TRUE)
  
  df$child_value <- as.numeric(df$child_value)
  breaks = c(-Inf, 2.5, 3.5, Inf)
  lab = c("low", "med", "high")
  df[, 
     child_value := cut(child_value,
                        breaks = breaks,
                        labels = lab,
                        right = TRUE)]
  df[is.na(child_value),
     child_value := "missing"]
  
  # Religiosity composed index
  # cr20m041	Aside from special occasions such as weddings and funerals, how often do 
  # you attend religious gatherings nowadays?
  # cr20m143	Do you see yourself as belonging to a church community or religious group?
  # cr20m144	Which church community or what religious group is that?
  
  df$cr20m041 <- as.numeric(df$cr20m041)
  breaks = c(-Inf, 3, Inf)
  lab = c("often", "not_often") #at least once a week ornot
  df[, relig := cut(cr20m041, breaks = breaks, labels = lab, right = TRUE)]
  df[is.na(cr20m041), relig := "missing"]
  
  # Occupation + job expectations
  # cw20m434  My prospects of career advancement/promotion in my job [are/were] poor.
  # cw20m435  It [is/was] uncertain whether my job [will/would] continue to exist.
  
  df$bad_job <- rowMeans(cbind(df$cw20m434,	df$cw20m435), na.rm = TRUE)
  
  df$occupation2 <- df$occupation
  df[bad_job>=2.5, occupation2 := "bad_job"]
  
  # Optimism composed index
  # cp20l198	In uncertain times, I usually expect the best
  # cp20l201	I'm always optimistic about my future.
  # cp20l206	I rarely count on good things happening to me.
  # cp20l207	Overall, I expect more good things to happen to me than bad.
  # cp20l204	I hardly ever expect things to go my way.
  
  # Reverse the two negative outcomes to go in the same direction of the others
  # higher values = more optimist
  new_levels <- c(5, 4, 3, 2, 1)
  
  # Convert the two negative items to a factor with the new levels
  df[, cp20l206 := factor(cp20l206, levels = new_levels)]
  df[, cp20l204 := factor(cp20l204, levels = new_levels)]
  
  df$opt <- rowMeans(cbind(df$cp20l198,
                           df$cp20l201,
                           df$cp20l206,
                           df$cp20l207,
                           df$cp20l204), na.rm = TRUE)
  
  df$opt <- as.numeric(df$opt)
  breaks = c(-Inf, 3, 4, Inf)
  lab = c("pessimist", "med", "optimist") #at least once a week ornot
  df[, optimism := cut(opt, breaks = breaks, labels = lab, right = TRUE)]
  df[is.na(opt), optimism := "missing"]
  
  # Age of the first child of respondent
  # starting from born year
  first_age = 2023 - df$cf20m456
  
  breaks = c(-Inf,6,
             10,
             15,
             20, Inf)
  labs = c("<7","<11","teen1","teen2","21+")
  df[, first_age := cut(first_age,
                        breaks = breaks,
                        labels = labs,
                        right = T)]
  
  df[(is.na(first_age) &
        children == "0"), first_age := "none"]
  
  df[is.na(first_age), first_age := "missing"]
  
  # Gynecologist
  df[, gynecologist := as.factor(ch20m219)]
  df[is.na(ch20m219), gynecologist := "missing"]
  
  # N. of rooms 
  breaks = c(-Inf,2, 
             3, 
             4,
             5, 
             Inf)
  lab = c("1-2","3","4","5","6+")
  df[, n_rooms := cut(
    cd20m034,
    breaks = breaks,
    labels = lab,
    right = T
  )]
  df[is.na(n_rooms), n_rooms := "missing"]
  
  # Mother availability and support 
  df$cf20m011 <- ifelse(is.na(df$cf20m011)&df$cf19l011==2, 2, df$cf20m011)
  df$cf20m011 <- ifelse(is.na(df$cf20m011)&df$cf18k011==2, 2, df$cf20m011)
  df$cf20m011 <- ifelse(is.na(df$cf20m011)&df$cf17j011==2, 2, df$cf20m011)
  df$cf20m011 <- ifelse(is.na(df$cf20m011)&df$cf16i011==2, 2, df$cf20m011)
  df$cf20m011 <- ifelse(is.na(df$cf20m011)&df$cf15h011==2, 2, df$cf20m011)
  df$cf20m011 <- ifelse(is.na(df$cf20m011)&df$cf14g011==2, 2, df$cf20m011)
  df$cf20m011 <- ifelse(is.na(df$cf20m011)&df$cf13f011==2, 2, df$cf20m011)
  df$cf20m011 <- ifelse(is.na(df$cf20m011)&df$cf12e011==2, 2, df$cf20m011)
  df$cf20m011 <- ifelse(is.na(df$cf20m011)&df$cf11d011==2, 2, df$cf20m011)
  df$cf20m011 <- ifelse(is.na(df$cf20m011)&df$cf10c011==2, 2, df$cf20m011)
  df$cf20m011 <- ifelse(is.na(df$cf20m011)&df$cf09b011==2, 2, df$cf20m011)
  df$cf20m011 <- ifelse(is.na(df$cf20m011)&df$cf08a011==2, 2, df$cf20m011)
  
  df <- df %>%
    mutate(mother_help = case_when(
      (children == 0 & cf20m132 %in% c(1:2)) |
        (children == 0 & cf20m011 == 2)
      ~ "0_nohelp",
      children == 0 & cf20m132==3 ~ "0_help",
      (children != 0 & cf20m134 %in% c(1:2)) |
        (children != 0 & cf20m011 == 2)
      ~ "1+_nohelp" ,
      children != 0 & cf20m134 == 3  ~ "1+_help" ,
      TRUE ~ "missing"
    ))
  
  df$mother_help <- ifelse(df$mother_help =="missing" & df$cf20m011==2, "0_nohelp", df$mother_help)
  df$mother_help <- ifelse(is.na(df$mother_help), "missing", df$mother_help)
  df$mother_help = as.factor(df$mother_help)
  
  # Interaction term: Partner*Age
  df <- df %>%
    mutate(
      partner_age = case_when(
        is.na(partner) | is.na(age) ~ "missing",
        partner == "missing" | age  == "missing" ~ "missing",
        TRUE ~ paste0(partner, "_", age)
      ),
      partner_age = as.factor(partner_age)
    )
  
  # Interaction term: Gender*occupation
  df <- df %>%
    mutate(
      gender_emp = case_when(
        is.na(gender) | is.na(occupation) ~ "missing",
        gender == "missing" | occupation  == "missing" ~ "missing",
        TRUE ~ paste0(gender, "_", occupation)
      ),
      gender_emp = as.factor(gender_emp)
    )
  
  # Composed index for political values
  df$cv20l216 <- ifelse(is.na(df$cv20l216), df$cv19k216, df$cv20l216)
  df$cv20l216 <- ifelse(is.na(df$cv20l216), df$cv18j216, df$cv20l216)
  
  df$cv20l216 <- as.numeric(df$cv20l216)
  breaks = c(-Inf, 2, 6, Inf)
  lab = c("no_soclib", "med_soclib", "soc_lib")
  df[, soc_lib := cut(cv20l216, breaks = breaks, labels = lab, right = TRUE)]
  df[is.na(cv20l216), soc_lib := "missing"]
  
  # Social network usage 
  df$cs20m577 <- as.numeric(df$cs20m577)
  breaks = c(-Inf, 1, 4, Inf)
  lab = c("insta", "med_insta", "no_insta")
  df[, insta := cut(cs20m577, breaks = breaks, labels = lab, right = TRUE)]
  df[is.na(cs20m577), insta := "missing"]
  
  
  # data.table projection
  df =df[, .SD, .SDcols = keepcols]
  
  return(df)
}

# PREDICTION WRAPPER ####

## Class prediction ####
pred_class_cat = function(object,newdata){
  r_probs = predict(object,newdata)$predictions[,2]
  r_preds = if_else(r_probs > 0.5, 1,0)
  return(as.factor(r_preds))
}

pred_class_num = function(object,newdata){
  r_probs = predict(object,newdata)$predictions[,2]
  r_preds = if_else(r_probs > 0.5, 1,0)
  return(r_preds)
}

## Probability prediction ####
pred_probs = function(object,newdata){
  r_probs = predict(object,newdata)$predictions[,2]
  return(r_probs)
}

## Shap prediction ####
pred_shap = function(object,newdata){
  p = predict(object, new_data = newdata, type = "prob")$.pred_1
  p
} 

# PARTIAL DEPENDENCE PLOTS ####

# Line-based partial dependence plot. Retained for reference only: the published
# figures use the bar-based version below, since every profiled predictor is
# categorical. Not called by any script.
ggplot_pdp = function(obj, x, groups = NULL) {
  x_sym = rlang::sym(x)
  
  # Extract unique labels and ensure consistent factor levels
  agr_data = as_tibble(obj$agr_profiles) %>%
    mutate(`_label_` = stringr::str_remove(`_label_`, "^[^_]*_"))
  
  # Get the unique levels in order they appear
  label_levels = unique(agr_data$`_label_`)
  
  # Apply consistent factor levels to both datasets
  agr_data = agr_data %>%
    mutate(`_label_` = factor(`_label_`, levels = label_levels))
  
  cp_data = as_tibble(obj$cp_profiles) %>%
    mutate(`_label_` = factor(stringr::str_remove(`_label_`, "^[^_]*_"), 
                              levels = label_levels))
  
  p = ggplot(agr_data, aes(x = `_x_`, y = `_yhat_`, 
                           color = `_label_`, group = `_label_`)) +
    geom_line(data = cp_data,
              aes(x = !!x_sym, group = `_ids_`),
              linewidth = 0.5, alpha = 0.1, color = "gray50")
  
  if (is.null(groups)) {
    p = p + geom_line(
      data = as_tibble(obj$agr_profiles),
      aes(x = `_x_`, y = `_yhat_`, group = `_label_`),
      color = "midnightblue",
      linewidth = 1.2,
      alpha = 1
    ) +
      geom_text(
        data = as_tibble(obj$agr_profiles),
        aes(
          x = `_x_`,
          y = `_yhat_`,
          label = round(`_yhat_`, 2)
        ),
        vjust = -0.5,
        size = 3,
        color = "black"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.background = element_rect(fill = "white", color = NA),
            axis.text.x = element_text(angle = 45, hjust = 1,size=14)) +
      labs(y = "Predicted Probabilities", x = x)
  } else {
    p = p + 
      geom_line(linewidth = 1.2, alpha = 1) +
      scale_color_brewer(palette = "Dark2") +
      theme_minimal(base_size = 14) +
      theme(plot.background = element_rect(fill = "white", color = NA),
            legend.text = element_text(size = 12),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14)) +
      labs(y = "Predicted Probabilities", x = x, color = groups)
  }
  
  return(p)
}


ggplot_pdp_bar = function(obj, x, groups = NULL) {
  x_sym = rlang::sym(x)

  # Extract and clean aggregated profiles
  agr_data = as_tibble(obj$agr_profiles) %>%
    mutate(`_label_` = stringr::str_remove(`_label_`, "^[^_]*_"))

  label_levels = unique(agr_data$`_label_`)

  agr_data = agr_data %>%
    mutate(`_label_` = factor(`_label_`, levels = label_levels))

  # Extract and clean individual profiles
  cp_data = as_tibble(obj$cp_profiles) %>%
    mutate(`_label_` = factor(stringr::str_remove(`_label_`, "^[^_]*_"),
                              levels = label_levels))

  if (is.null(groups)) {
    p = ggplot(agr_data, aes(x = `_x_`, y = `_yhat_`)) +
      geom_jitter(data = cp_data,
                  aes(x = !!x_sym, y = `_yhat_`),
                  width = 0.2, height = 0,
                  alpha = 0.05, color = "gray50", size = 0.8) +
      geom_col(fill = "midnightblue", alpha = 0.8, width = 0.7) +
      geom_text(aes(label = round(`_yhat_`, 2)),
                vjust = -0.5, size = 3, color = "black") +
      theme_minimal(base_size = 14) +
      theme(plot.background = element_rect(fill = "white", color = NA),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14)) +
      labs(y = "Predicted Probabilities", x = x)
  } else {
    p = ggplot(agr_data, aes(x = `_x_`, y = `_yhat_`, fill = `_label_`)) +
      geom_jitter(data = cp_data,
                  aes(x = !!x_sym, y = `_yhat_`),
                  width = 0.2, height = 0,
                  alpha = 0.05, color = "gray50", size = 0.8,
                  inherit.aes = FALSE) +
      geom_col(position = position_dodge(width = 0.8),
               alpha = 0.8, width = 0.7, color = "black", linewidth = 0.2) +
      scale_fill_brewer(palette = "Dark2") +
      theme_minimal(base_size = 14) +
      theme(plot.background = element_rect(fill = "white", color = NA),
            legend.text = element_text(size = 12),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14)) +
      labs(y = "Predicted Probabilities", x = x, fill = groups)
  }

  return(p)
}