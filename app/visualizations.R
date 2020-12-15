# reading in the csv files after all the necessary data modifications are done

cand2013 <- read.csv("cand2013")
aggregate <- read.csv("aggregate")

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


# Age distribution of canddiates by Gender and Geographical regions

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



