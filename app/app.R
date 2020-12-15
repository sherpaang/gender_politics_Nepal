library(shiny)
library(tidymodels)
library(rstanarm)
library(tidyverse)
library(ggthemes)
library(janitor)
library(shinythemes)
library(leaflet)
library(gt)
library(mathjaxr)
library(gtsummary)
library(broom.mixed)

source("visualizations.R")
error <- read.csv("error")
model2 <- readRDS("model2")
model3 <- readRDS("model3")

# UI

ui <- fluidPage(
    fluidPage(theme = shinytheme("flatly"),
              titlePanel("Determinants of Woman Representativeness in Nepal"),
              mainPanel(
                  tabsetPanel(
                      tabPanel(
                          "Nepal",
                          br(),
                          leafletOutput("basicmap"),
                          br(),
                          p("Nepal is a small country in South Asia, located
                          between India and China. It is home to 28 million
                          people and boasts one of the tallest mountains in the
                          world, including the tallest - Mt. Everest. It is
          also known for being the birthplace of Lord Gautam Buddha - the
          founder of Buddhism."),
                          p("It is home to a hundred different castes, ethnicities and languages
        despite its rather small population. The capital city is located in the
        Kathmandu valley in the Hilly region of the country, situated between
        the Mountaineous region in the north and the Terai plains in the
          South."),
                          p("Administratively, it is divided into 7 states, 77 districts and
          thousands of local municipalities. It has federal governance, which 
          was very recently implemented in 2017. This study is based in the
          election of 2013, when the country did not have a federal governance
          system and consisted of 75 districts overall.")
                      ),
                      tabPanel(
                          "Data",
                          br(),
                          h4("Context"),
                          br(),
                          plotOutput("geo"),
                          br(),
                          p("After a centuries long monarchy rule and a
brief civil war for around 10 years, Nepal finally introduced itself as a Federal
Democratic Republic to the world. After the introduction of democracy, it had a series
of attempts at constitutional implementation, for the purpose of which, it 
elected two Constitutional Assemblies twice - once in 2064 B.S (2008 A.D.), and
another in 2070 B.S. (2013 A.D.). This project attempts at explaining the
                       predictive potential of different explanatory variables
                       to predict the gender of candidates running for the CA
                       elections in 2013."),
                          p("Nepal has a mix of FPTP and PR election seats. This
                       analysis is limited to FPTP candidates only. That should
                       not be a problem as candidates actively running for
                       themselves would approximate gender representation in
                       a particular location more than simply being enlisted in
                       a party list."),
                          br(),
                          h4("Important Data Modifications"),
                          p("Most of the modifications have been aptly mentioned in
                       the code chunks of the R files available in the Github
                       repo. However, I will highlight some of the most
                       important ones here."),
                          p("Firstly, I changed the data format from Nepali B.S. to
                       A.D. for the general ease of comprehension. To do so,
                       I simply replaced 2064 with 2008 and 2070 with 2013.
                       Secondly, I had to convert some Nepali texts in the data
                       to English, for which I simply copied the Nepali text
                       displayed in R and used mutate and case when functions
                       when appropriate."),
                          br(),
                          h2("Female Representation in Nepal"),
                          h4("Exploratory Data Analysis"),
                          h5("Distribution of Gender across district"),
                          plotOutput("density"),
                          br(),
                          h5("Distribution of Gender across age"),
                          plotOutput("age"),
                          h4("Source:"),
                          p("The data sets used in this project were largely
                       extracted from three major sources:"),
                          tags$ul(
                              tags$li(a("Open Data Nepal Project", href="http://data.opennepal.net/")),
                              tags$li(a("Nepal In Data", href="https://nepalindata.com/"))
                          )
                      ),
                      tabPanel(
                          "Model",
                          h3("Different Models"),
                          p("I initially divided the data set into training and testing. 
          After fitting the model into the training data set, I checked their
          predictive power with the test data set."),
                          p("I firstly considered a model with all the explanatory variables I 
        had. The regression coefficients of many variables were 0, so I then
        put all the variables with zero coefficients (defined as the first two
        decimal points being zero) and the non-zero coefficients in two
        different models. I tested the errors associated with all three
          models."),
                          p("The following is the list of models I considered and the errors
        associated with each of the models."),
                          tableOutput("error"),
                          br(),
                          h3("Model"),
                          p("None of the models above have any significant error values, so I
          proceeded with making two models, one with all the zero coefficient
          variables and the other with non-zero coefficient variables."),
                          p("The Mathematical formula for the model with non-zero coefficient
          variables is as follows:"),
                          withMathJax(),
                          helpText("$$ femaleprop_i = \\beta_1winner2008_i + \\beta_{2}income2001_i + \\beta_3hdi2011_i + \\beta_{4}literacy2011_i + \\beta_5femaleliteracy2011_i + \\epsilon_i $$"),
                          p("Here are the intercepts I got for both of my models:"),
                          br(),
                          h4("Model with non-zero coefficient variables"),
                          gt_output(outputId = "nonzerotable"),
                          br(),
                          h4("Model with zero coefficient variables"),
                          gt_output(outputId = "zerotable"),
                          br(),
                          h3("Interpretation"),
                          h4("Terminologies:"),
                          tags$ul(
                              tags$li("Our outcome variable (y variable) is femaleprop, which refers
          to the proportion of female candidates in a district."),
                              tags$li("The error term, Ei, simply refers to the difference between
                  the actual proportion of female candidates and the modeled
                  proportion. It is assumed to be normally distributed with a
                  mean of zero."),
                              tags$li("Each explanatory variables have a Beta value and 95% CI
                  values. The Beta refers to the coefficient of regressions.
                  The Confidence Interval, in Bayesian terms, refers to the
                  interval with the confidence or the probability of the actual
                  regression coefficient falling under these intervals.")
                          ),
                          h4("Explanation of Beta Coefficients:"),
                          tags$ul(
                              tags$li("All the variables in model with zero coefficient variables
                  seemt to have very little or non-existent correlation with
                  the proportion of female candidates in the district."),
                              tags$li("In the model with non-zero coefficient variables however,
                  the Beta of winner2008, which is 0.12, refers to the slope of
                  proportion of female candidates in 2013 with the proportion
                  of women elected officials in the district in 2008. The
                  confidence interval is also relatively narrow for this
                  variable."),
                              tags$li("The coefficients of income 2001 and literacy 2011 are
                  negative. This implies a negative relationship between a
                  higher income of woman (income 2001) and higher general
                  literacy and the proportion of female candidates. However,
                  the coefficient is very small for both. Further, the
                  income2001 variable does not seem to pass the null hypothesis
                  either. Ergo, both of these variables ought to be read as
                  having zero correlations at this point."),
                              tags$li("The highest coefficient among our variables seems to be for
                  hdi 2011. The correlation between HDI and female empowerment
                  is pretty self-explanatory. When people in a community are
                  more empowered, they are more likely to support female
                  empowerment. However, we can see that literacy has a
                  negligible effect in the proportion of female candidates in a
                  district. Therefore, in a surfacial level, the effect of HDI
                  in female election representativeness in Nepal seems to be a
                  result, not of increased literacy (which is a party of HDI),
                  but rather other variables which might aid a higher HDI.
                  Note: the income variable we have belongs to 2001, so we
                  cannot take that as an effective proxy for the income levels
                  of women in these districts in  2013.")
                          )
                      ),
                      tabPanel("About",
                               br(),
                               h4("About Me"),
                               p("Hey there, my name is Ang Sonam Sherpa. I am a sophomore at
                 Harvard concentrating in Social Studies and Mathematics.
                 Welcome  to my final project for Gov 50, a class in data
                 science."),
                               h4("About the project"),
                               p("This project is an attempt at understanding how female
                 representation in Nepal is affected by various district level
                 characteristics ranging from  HDI to literacy. To do so, I
                 extracted a data set consisting of candidates in the 2013
                 election of Nepal and regressed the proportion of female
                 candidates in each district with various district
                 characteristics. "),
                               p("You can find the link to my Github right", 
                                 a("here.", href="https://github.com/sherpaang/gender_politics_Nepal.git"))
                      )
                  )
              )
    ))

# server logic

server <- function(input, output){
    
    output$basicmap <- renderLeaflet({
        leaflet() %>%
            setView(lng = 84.1240, lat = 28.3949, zoom = 6) %>%
            addProviderTiles(providers$Esri.NatGeoWorldMap)
    })
    
    output$administrativemap <- renderLeaflet({
        leaflet() %>%
            setView(lng = 84.1240, lat = 28.3949, zoom = 6) %>%
            addTiles()
        
    })
    
    output$geo <- renderPlot({
        geographical_region
        
    })
    
    output$density <- renderPlot({
        densitydistribution
        
    })
    
    output$age <- renderPlot({
        agedistribution
        
    })
    
    
    output$nonzerotable <- render_gt({
        
        tbl_regression(model2, intercept = TRUE) %>%
            as_gt() %>%
            tab_header(title = md("Regression of proportion of Female candidates in a district with various district characteristics")) %>%
            tab_source_note("Source: Open Project Nepal & Nepal in Data")
        
    })
    
    output$zerotable <- render_gt({
        
        tbl_regression(model3, intercept = TRUE) %>%
            as_gt() %>%
            tab_header(title = md("Regression of proportion of Female candidates in a district with various district characteristics")) %>%
            tab_source_note("Source: Open Project Nepal & Nepal in Data")
        
    })
    
    
    output$error <- render_gt({
        
        error
        
    })
}

# Run the application

shinyApp(ui = ui, server = server)
