#Assignment One
rm(list=ls())

library()
library()
library(ipumsr)
library(stargazer)
library(tidycensus)
library(tidyverse)
library(ggplot2)
library(sf)


ufo <- read.csv(file = "./Data/scrubbed.csv", header = TRUE, as.is = TRUE, sep = ",")
st_abb <- read.csv(file = "./Data/st_abb.csv")

#Getting data from IPUMS API
  set_ipums_api_key('59cba10d8a5da536fc06b59dfd453dddadad4bb98f1d58a11dffe6e9', save = TRUE, overwrite = TRUE) #Sets your personal key and saves it

  tst <- get_metadata_nhgis("time_series_tables") #This gives you a list of the time series tables that you can get

tst$geog_levels[[1]]
  #Insert Answer to Question 2 Here####

#data_names <- c("A00","A57","B57","B18","CL6","B69") #These are the tables we are using
  data_ext<- define_extract_nhgis(
    description = "ECON 691",
    time_series_tables =  list(
      tst_spec("A00", "state"),
      tst_spec("A57", "state"),
      tst_spec("B57", "state"),
      tst_spec("B18", "state"),
      tst_spec("CL6", "state"),
      tst_spec("BX3", "state")
    )
  )
  
  ts<-submit_extract(data_ext)
  wait_for_extract(ts)
  filepath <- download_extract(ts)
  
  dat <- read_nhgis(filepath)

#Obtain Census Map Data
##Question 4A##
  state <- c("53", "41", "16", "30")
  
  cen.stat <- get_acs(geography = "state", 
                      survey = "acs5",
                      variables = "B01003_001E", 
                      year = 2020,
                      geometry = TRUE)


  cen.map <- cen.stat %>%
    select(GEOID, NAME, geometry)  %>%
    mutate(STATEFP = GEOID) 

 #Basic Clean of data####

    dat2 <- dat %>%
      select(STATEFP, ends_with(c("1970", "1980", "1990", "2000", "2010", "105", "2020", "205"))) %>%
      filter(!is.na(STATEFP)) %>%
      pivot_longer(!STATEFP, names_to = "series", values_to = "estimate") %>%
      mutate(series = str_replace(series, "105", "2010"),
             series = str_replace(series, "205", "2020"),
             year = substr(series, 6, nchar(series)),
             series = substr(series, 1, 5)) %>%
      distinct(STATEFP, series, year, .keep_all = TRUE) %>%
      filter(!is.na(estimate)) %>%
      pivot_wider(id_cols = c(STATEFP, year), names_from = series, values_from = estimate) %>%
      select(-B18AE)  %>%
      mutate(und18 = rowSums(across(B57AA:B57AD)) / A00AA,
             over65 = rowSums(across(B57AP:B57AR)) / A00AA,
             white = (B18AA / A00AA), #White Population
             black = (B18AB / A00AA), #Black Population
             asian = (B18AD / A00AA), #Asian Population
             other = (B18AC / A00AA), #Something other than the above including multi-race
             lessHS = (BX3AA + BX3AB + BX3AC + BX3AG + BX3AH + BX3AI) / A00AA,
             hscol =  (BX3AD + BX3AJ) / A00AA, #12th Grade and some college
             ungrd =  (BX3AE + BX3AK) / A00AA, #4 years of college or Bach Degree
             advdg =  (BX3AF + BX3AL) / A00AA, #More than 4 years or advanced degree
             pov =  (CL6AA / A00AA), #Share of population under Poverty Line
             ln_pop = log(A00AA)) %>%  #Natural Log of Population
      select(STATEFP, year, und18:ln_pop) 
# The code above was basically cleaning the data and transforming it to a specfic format or type to be used in the analysis. To go further, it selected specific columns and filtered out missing values. It then went on to remove duplicates and selecetd specific variables to be worked with or analysis to be done with#
  
  
    ufo.us <- ufo %>%
      filter(country == "us") %>%
      select(-comments) %>%
      mutate(date = as.Date(str_split_i(datetime," ", 1), "%m/%d/%Y"),
             year = year(date),
             decade = year - year %% 10) %>%
      filter(decade > 1959) %>%
      count(state, decade) %>%
      mutate(Abbr = toupper(state),
             year = as.numeric(decade)) %>%
      full_join(., st_abb, by = "Abbr") %>%
      filter(!is.na(n)) %>%
      rename("GEOID" = "Code") %>%
      mutate(GEOID = str_pad(as.character(GEOID), width = 2, side = "left", pad="0"),
      ln_n = log(n))
#Comment about the code above doing: the code above seeks to filter UFO sightings which occurred in the United States by extracting years and decade information after 1959. Also, it count the number of UFO sightings by States and by decades. It joins the dataframe and filter out missing values, rename column GEOID as Code.Lastly, transforms the number of UFO sightings by taking a log#

#Join the data and Map it
   
      # Step 1: Create 'core' by joining cen.map and dat2
    core <- cen.map %>%
      left_join(dat2, by = c("STATEFP")) %>%  
      mutate(decade = as.numeric(year)) %>%   
      select(-ends_with(".x"), -ends_with(".y"))  
    
    # Step 2: Create 'core.all' by joining 'core' with 'ufo.us'
    core.all <- core %>%
      left_join(ufo.us, by = c("GEOID", "decade")) %>%  
      mutate(n = as.numeric(n)) %>% 
      select(-c(GEOID, state, year.y, NAME, Abbr)) %>%  
      filter(!is.na(n))  
    

#Non-Race Variable Graphic Visualization#####

    ggplot(core) +
      geom_sf(aes(fill = pov)) +
      scale_fill_gradient2(low = "white", high = "blue", na.value = NA, 
                           name = "CL6AA",
                           limits = c(0, .5)) +
      theme_bw()+
      theme(axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            legend.position = "bottom") +
      labs(title = "Figure One: Percentage of Population Below Poverty Line Across the Decades") +
      facet_wrap(~ decade) 
    
    ggsave("./Analysis/Output/Figure1.png", dpi = 600)

#Race Variable Graphic Visualization

    ggplot(core) +
      geom_sf(aes(fill = black)) +
      scale_fill_gradient2(low = "white", high = "blue", na.value = NA, 
                           name = "B18AB",
                           limits = c(0, 1)) +
      theme_bw()+
      theme(axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            legend.position = "bottom") +
      labs(title = "Figure Two: Percentage of Population Black Across the Decades") +
      facet_wrap(~ decade)
    
    ggsave("./Analysis/Output/Figure2.png", dpi = 600)

#Summary Statistics

var1 <- c("Percent Under 18", "Percent Over 65", "Percent White", "Percent Black",
          "Percent Asian", "Percent Other Race", "Percent with Less than HS", "Highschool Only",
          "Undergraduate Degree", "Advanced Degree", "Percent below 2X Poverty Line", 
          "LN of Population","Decade","Number of Signthings", "LN of Sightings")

stargazer(as.data.frame(core), type = "html", out = "./Analysis/Output/Table1.html",
          title = "Table One - Summary Statistics",
          covariate.labels=var1)

#Regression Analysis


mod1 <- lm(ln_n ~ und18 + over65 + white + black + asian + other + lessHS + hscol + ungrd + advdg + pov + ln_pop, data = core.all)
mod2 <- lm(ln_n ~ und18 + over65 + white + black + asian + other + lessHS + hscol + ungrd + advdg + pov + ln_pop + factor(decade), data = core.all)
mod3 <- lm(ln_n ~ und18 + over65 + white + black + asian + other + lessHS + hscol + ungrd + advdg + pov + ln_pop + factor(decade) + factor(State), data = core.all)



var2 <- c("Percent Under 18", "Percent Over 65", "Percent White", "Percent Black",
          "Percent Asian", "Percent Other Race", "Percent with Less than HS", "Highschool Only",
          "Undergraduate Degree", "Advanced Degree", "Percent below 2X Poverty Line", 
          "LN of Population","Decade","Number of Signthings", "LN of Sightings")


stargazer(mod1, mod2, mod3,
          omit = ".State.",
          type = "html",
          title = "Table Two - Regression Results",
          out = "./Analysis/Output/Table2.html",
          add.lines=list(c("State F.E." ,"NO", "NO", "YES")),
          dep.var.labels = "LN(Sightings)",
          covariate.labels=var2)

