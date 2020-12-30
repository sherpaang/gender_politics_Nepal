# reading in the csv files after all the necessary data modifications are done

cand2013 <- read.csv("cand2013")
aggregate <- read.csv("aggregate")
enrollmentgdp <- read.csv("enrollmentgdp")
asialiteracy <- read.csv("asialiteracy")

# visualizing gender distribution of candidates across geographical regions
# Assigning the plot to an object to call it later in the main app.

geographical_region <- cand2013 %>%
    ggplot(aes(x = gender, fill = gender)) +
    geom_bar() +
    facet_wrap(~ geographical_region) + 
    theme_classic() +
    theme(legend.position = "none") +
    labs(title = "Gender Distribution of FPTP candidates across regions",
         x = "Gender",
         y = "Number of Candidates")

# visualizing the density distribution of the proportion of women candidates
# in every district along side the proportion of women who won the election
# in 2008.

densitydistribution <- aggregate %>%
    select(district, winner2008, prop2013) %>%
    rename("Winner 2008" = "winner2008",
           "Candidates 2013" = "prop2013") %>%
    
    # using pivot longer to visualize both density graphs at the same time  
    
    pivot_longer(cols = c("Winner 2008", "Candidates 2013"), names_to = "type",
                 values_to = "data") %>%
    ggplot(aes(data, fill = type)) +
    geom_density(alpha = 0.75) +
    
    # using alpha to make both graphs visible  
    
    theme_bw() +
    theme(legend.title = element_blank()) +
    labs(title = "Proportion of Women Winners and Candidates in 2008 and 2013",
         x = "Proportion in a District",
         y = "Density")


# Age distribution of candidates by Gender and Geographical regions

agedistribution <- cand2013 %>%
    ggplot(aes(age, gender, color = geographical_region)) +
    geom_jitter(alpha = 0.2) +
    
    # used facet wrap to divide along geographical regions
    
    facet_wrap(~ geographical_region, nrow = 5) +
    theme_bw() +
    labs(title = "Age distribution of Candidates by Gender",
         subtitle = "There does not seem to be a lot of difference",
         x = "Age",
         y = "Gender")


# HDI demonstration by geographical and development regions
# putting .groups = "drop" to drop the grouping and to remove the error at the
# end.

hdigraph <- aggregate %>%
    select(geographical_region, development_region, hdi2011) %>%
    mutate(geographical_region = as.factor(geographical_region),
           development_region = as.factor(development_region)) %>%
    group_by(geographical_region, development_region) %>%
    summarize(averageHDI = mean(hdi2011), .groups = "drop")

hdigraph$development_region <- factor(hdigraph$development_region,
                                      levels = c("Far-Western", "Mid-Western",
                                                 "Western", "Central",
                                                 "Eastern"))


femliteracypop <- aggregate %>%
    select(development_region, geographical_region,
           femaleliteracy2011, femalepop2011) %>%
    mutate(geographical_region = as.factor(geographical_region),
           development_region = as.factor(development_region)) %>%
    group_by(development_region, geographical_region) %>%
    summarize(avgfemliteracy = mean(femaleliteracy2011),
              totalfempop = sum(femalepop2011),
              .groups = "drop")

# explaining the correlation between proportion of women candidates in a
# district and the hdi of that particular district

hdicorr <- aggregate %>%
    ggplot(aes(hdi2011, prop2013)) +
    geom_point() +
    geom_smooth(method = "lm") +
    scale_x_continuous(breaks = c(0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7)) +
    theme_clean() +
    labs(title = "Correlation between HDI and Proportion of female candidates",
         subtitle = "There seems to be a strong correlation",
         x = "HDI",
         y = "Proportion of female candidates")

# changing the column types to the necessary type for the ggplot

saarcliteracy <- asialiteracy %>%
    pivot_longer(cols = c(datamale, datafemale),
                 names_to = "gender",
                 values_to = "literacy") %>%
    mutate(literacy = as.numeric(literacy), gender = as.factor(gender),
           country_name = as.factor(country_name)) %>%
    ggplot(aes(year, literacy, group = gender, color = gender)) +
    geom_line() +
    facet_wrap(~ country_name) +
    theme_bw() +
    scale_color_manual(values = c("red", "blue"),
                       breaks = c("datamale", "datafemale"),
                       labels = c("Male", "Female")) +
    labs(title = "Literacy rate for South Asian countries across gender",
         subtitle = "Female literacy has always been much lower generally",
         x = "Years",
         y = "Literacy rate")



# correlation between gdp spending on education and the difference in primary
# enrollment between men and women

educationspendingcorr <- enrollmentgdp %>%
    select(country_name, year, enrollmentdifference, educationspending) %>%
    filter(country_name != "South Asia (IDA & IBRD)") %>%
    na.omit() %>%
    mutate(educationspending = as.numeric(educationspending)) %>%
    ggplot(aes(educationspending, enrollmentdifference)) +
    geom_point() +
    geom_smooth(method = "loess") +
    theme_clean() +
    scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8),
                       labels = function(x) paste0(x, '%')) +
    scale_y_continuous(labels = function(x) paste0(x, '%')) +
    labs(title = "Education spending and Primary school enrollment",
         x = "GDP spending in education",
         y = "Gender difference in primary enrollment")

gdpcorr <- enrollmentgdp %>%
    select(country_name, year, enrollmentdifference, datapercapita) %>%
    filter(country_name != "South Asia (IDA & IBRD)") %>%
    na.omit() %>%
    mutate(datapercapita = as.numeric(datapercapita)) %>%
    ggplot(aes(datapercapita, enrollmentdifference)) +
    geom_point() +
    geom_smooth(method = "loess") +
    theme_clean() +
    scale_x_continuous(labels=scales::dollar_format(),
                       breaks = c(0, 1000, 2000, 3000, 4000,
                                  5000, 6000, 7000, 8000)) +
    scale_y_continuous(labels = function(x) paste0(x, '%')) +
    labs(title = "GDP and Primary school enrollment",
         x = "GDP per capita",
         y = "Gender difference in primary enrollment")
