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
model2 <- readRDS("model2")

# UI

ui <- fluidPage(
    fluidPage(theme = shinytheme("yeti"),
              titlePanel("Determinants of Woman Representativeness in Nepal"),
              mainPanel(
                  tabsetPanel(
                      tabPanel(
                          "Nepal",
                          br(),
                          p("Nepal is a small country in South Asia, located
                          between India and China. It is home to 28 million
                          people. Located in the heart of Himalayas,
                          natural beauty, cultural diversity, and lots of
                          mo:mos are what defines Nepal for many."),
                          br(),
                          plotOutput("literacygif"),
                          br(),
                          br(),
                          br(),
                          br(),
                          p("Nepal shares the Indian sub-continent with its 7
                            neighbors - all pictured in the plot above.
                            It shares a rich cultural and diplomatic
                            relationship with most of its neighbors in South
                            Asia. However, Nepal lacks significantly behind its
                            neighbors when it comes to gender empowerment as
                            illustrated by the difference in literacy rates in
                            the plot above. Economically as well, Nepal has not
                            fared well in comparison to other nations in the
                            region"),
                          br(),
                          plotOutput("gdpgif"),
                          br(),
                          br(),
                          br(),
                          br(),
                          h4(strong("More on South Asia")),
                          p("As we have seen in the graphs above, South Asia
                            generally has not done very well for female
                            empowerment. One of the best indicators of gender
                            empowerment in a society is the difference in
                            primary school enrollment between boys and girls.
                            Ofcourse, there are other factors which then
                            affects gender empowerment, but primary school
                            enrollment often times is a powerful indicator of
                            gender empowerment. The following two plots
                            illustrate how the gendered difference in primary
                            school enrollment has correlated with various
                            factors over the years in South Asia. They are
                            created using datasets from as far as 1960
                            exclusively for countries belonging to South Asia"),
                          br(),
                          plotOutput("educationcorr"),
                          br(),
                          p("The relationship is clear. More spending in
                            Education clearly leads to a lower gender
                            difference in Primary school enrollment. The effect
                            wanes down after the GDP spending on education
                            reaches 4% however."),
                          plotOutput("gdpcorrelation"),
                          br(),
                          p("There seems to be a very strong correlation
                            between a higher GDP per capita and a lower
                            gendered difference in Primary school enrollment.
                            However, after the GDP per capita reaches around 
                            $2000, the effect does not seem to exist for South
                            Asian countries. Ofcourse, the confidence interval
                            of the relationship is much wider as we approach
                            the said GDP per capita. However, the general truth
                            stands out - there is a very little difference in
                            primary school enrollment generally between men and
                            women after a country in South Asia reaches a GDP
                            per capita of $2000."),
                          br(),
                          h4(strong("Inside Nepal")),
                          p("It is home to a hundred different castes,
                            ethnicities and languages despite its rather small
                            population. The capital city is located in the 
                            Kathmandu valley in the Hilly region of the country,
                            situated between the Mountaineous region in the north
                            and the Terai plains in the South."),
                          p("Administratively, it is divided into 7 states, 77
                          districts and thousands of local municipalities. It
                          has federal governance, which was very recently
                          implemented in 2017. This study is based in the
                          election of 2013, when the country did not have a
                          federal governance system and consisted of 75
                          districts overall."),
                          br(),
                          h4(strong("Map of Nepal")),
                          leafletOutput("basicmap"),
                          br(),
                          br()
                      ),
                      
                      tabPanel(
                          "Data",
                          br(),
                          h4(strong("Context")),
                          h5("Female literacy and population"),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(
                                "select_dr",
                                "Development Region",
                                choices = c("Eastern", "Central",
                                            "Western",
                                            "Mid-Western", "Far-Western")
                              )),
                            mainPanel(plotOutput("femlitpop"))),
                          br(),
                          p("After a centuries long monarchy rule and a brief
                          civil war for around 10 years, Nepal finally
                          introduced itself as a Federal Democratic Republic to
                          the world. After the introduction of democracy, it
                          had a series of attempts at constitutional
                          implementation, for the purpose of which, it elected
                          two Constitutional Assemblies twice - once in 2064
                          B.S (2008 A.D.), and another in 2070 B.S. (2013 A.D.)
                          This project attempts at explaining the predictive
                          potential of different explanatory variables to
                          predict the gender of candidates running for the
                          CA elections in 2013."),
                          p("Nepal has a mix of FPTP and PR election seats.
                            This analysis is limited to FPTP candidates only.
                            That should not be a problem as candidates actively
                            running for themselves would approximate gender
                            representation in a particular location as well
                            despite different dynamics at play when it comes
                            to such candidacy."),
                          br(),
                          h4(strong("Female Representation in Nepal")),
                          plotOutput("density"),
                          br(),
                          p("As we can see, the proportion of female candidates
                            in 2013 and the proportion of female winners in
                            2008 both fall disproportionately around 0-10% in
                            most districts. Districts with no women winners
                            occur in the highest frequency whereas districts
                            with 10% of female candidates occur most
                            frequently."),
                          br(),
                          plotOutput("age"),
                          br(),           
                          p("The distribution of age across geographical region
                            and gender seems to be pretty evenly spread. There
                            is a high concentration in the 30-50 age mark. The
                            number of candidates in Terai is significantly more
                            in comparison, which makes sense since Terai is also
                            more heavily populated than other regions."),
                          br(),
                          plotOutput("geo"),
                          br(),
                          p("Finally, the proportion of female candidates
                            across geographical regions is pretty similar. The
                            number of female candidates from the Mountain region
                            seems to be negligible. But the proportion of male
                            candidates is significantly higher all across the
                            three regions."),
                          br(),
                          h4(strong("Important Data Modifications")),
                          p("Most of the modifications have been aptly
                          mentioned in the code chunks of the R files available
                          in the Github repo. However, I will highlight some of
                          the most important ones here."),
                          p("Firstly, I changed the data format from Nepali B.S.
                          to A.D. for the general ease of comprehension. To do
                          so, I simply replaced 2064 with 2008 and 2070 with
                          2013. Secondly, I had to convert some Nepali texts in
                          the data to English, for which I simply copied the
                          Nepali text displayed in R and used mutate and case
                          when functions when appropriate. Finally, since I 
                          joined data sets from different sources, some
                          data points for some districts were missing, so I
                          removed the missing districts after joining them
                          together for better data analysis. My final analysis
                          had a total of 68 districts with a total of 5154
                          candidates for the model."),
                          br(),
                          h4(strong("Source:")),
                          p("The data sets used in this project were largely
                       extracted from three major sources:"),
                          tags$ul(
                              tags$li(a("World Bank",
                                href="https://databank.worldbank.org/home")),
                              tags$li(a("Open Data Nepal Project",
                                        href="http://data.opennepal.net/")),
                              tags$li(a("Nepal In Data",
                                        href="https://nepalindata.com/"))
                          ),
                          br(),
                          br()
                      ),
                    tabPanel(
                          "Model",
                          br(),
                          p("I initially divided the data set into training and
                            testing. After fitting the model into the training
                            data set, I checked their predictive power with the
                            test data set. To do so, I changed the values of
                            the gender column to 1 if female and 0 if male. I
                            intended to run an OLS regression on the gender of
                            each candidate. While I also considered conducting
                            a logistic regression, seeing no significant 
                            marginal benefits, I did not."),
                          p("I firstly considered a model with all the
                            explanatory variables I had. The regression
                            coefficients of many variables were 0, so I then
                            put all the variables with non-zero coefficients
                            (defined as the first two decimal points not being
                            zero) into a model. I tested the errors associated
                            with two models - one with all the variables and the
                            other with all the non-zero coefficient
                            variables."),
                          p("None of the models had any significant
                          error values, so I proceeded with making a model
                            with all the non-zero coefficient variables. If you
                            would be interested in learning more about the error
                            calculation and its values, please refer to my
                            Github and an Rmd file called model
                            in particular."),
                          br(),
                          h4(strong("Formula")),
                          withMathJax(),
                          helpText("$$ gender_i = \\beta_1winner2008_i +
                                   \\beta_{2}income2001_i + \\beta_3hdi2011_i +
                                   \\beta_{4}developmentregion_i +
                                   \\epsilon_i $$"),
                          br(),
                          p("Here are the intercepts I got for my models:"),
                          gt_output(outputId = "modeltable"),
                          br(),
                          p("As we can see, HDI of a district seems to have an
                            outsized influence in the proportion of female
                            candidates in a district. To better visualize the
                            relationship between the HDI and proportion of
                            female candidates, the following correlation plot
                            should help."),
                          plotOutput("hdicorr"),
                          br(),
                          p("For a better understanding of how HDI is
                            distributed across Nepal, the following interactive
                            plot should be useful. Selecting a geographical
                            region in the left hand side should demonstrate the
                            distribution of HDI across development regions in
                            the right."),
                          h5("HDI by geograhpical region"),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(
                                "select_region",
                                "Select Region",
                                choices = c("Mountain", "Hill", "Terai")
                              )),
                            mainPanel(plotOutput("hdi"))),
                          h4(strong("Terminologies")),
                          tags$ul(
                              tags$li("Our outcome variable (y variable) is
                              gender, which refers to the gender of an
                                      individual candidate."),
                              tags$li("The error term, Ei, simply refers to the
                              difference between the actual proportion of
                                      female candidates and the modeled
                                      proportion. It is assumed to be normally
                                      distributed with a mean of zero."),
                              tags$li("Each explanatory variables have a Beta
                                      value and 95% CI values. The Beta refers
                                      to the coefficient of regressions. The
                                      Confidence Interval, in Bayesian terms,
                                      refers to the interval with the
                                      confidence or the probability of the
                                      actual regression coefficient falling
                                      under these intervals.")),
                          br(),
                          h4(strong("Interpretation of the Model")),
                          p("It is rather strange that so many
                              variables in the model had near-zero coefficients.
                              The statistical interpretation of these
                              coefficients would suggest that there is no
                              relationship between female candidate proportion
                              and these variables. That is a possibility since
                              the gender of candidates in an election might not
                              be a good proxy for gender empowerment."),
                          p("The Beta of winner2008, which
                              is 0.1, refers to the slope of proportion of
                              female candidates in 2013 with the proportion of
                              women elected officials in the district in 2008.
                              The confidence interval is also relatively narrow
                              for this variable."),
                          p("The coefficients of income 2001 is negligible even
                            though it is not exactly 0. First of all, the
                            variable does not pass the null hypothesis since
                            Standard Deviation is broad enough to include zero.
                            Secondly, 0.02 is a rather small coefficient to
                            begin with. Finally, it is to be expected since
                            we are talking about the income data set from
                            2001."),
                          p("The highest coefficient among our variables seems
                          to be for hdi 2011. The correlation between HDI and
                          female empowerment is pretty self-explanatory. When
                          people in a community are more empowered, they are
                          more likely to support female empowerment. However,
                          if we are to ignore the shortcoming of our income
                          2001 data set to represent the incomes of women at
                          2013, we can see that income has a negligible effect
                          in gender of candidates in a district. Therefore, in
                          a superficial level, the effect of HDI in female
                          election representativeness in Nepal seems to be a 
                          result, not of increased income, but rather other
                          variables which might aid to a higher HDI."),
                          p("The coefficients of development regions highlight
                            a small yet significant difference between different
                            development regions. They cannot be interpreted
                            individually. With the addition of a particular
                            hdi value and the winner proportion of 2008, they
                            represent the difference that each development
                            region would bring to the gender of the candidate.
                            The higher the difference, the higher the
                            likelihood of a male candidate, since male is
                            indicated by a value of zero in this regression.
                            As such, the difference between the Eastern, Central
                            and Western seems to be negligible. However, the
                            Far western region seems to have a slightly lower
                            probability for a male candidate with the Far
                            Western having much higher probability of a male
                            candidate."),
                          br(),
                          br()
                      ),
                      tabPanel("About",
                               br(),
                               h4(strong("About Me")),
                               p("Hey there, my name is Ang Sonam Sherpa. I am
                               a sophomore at Harvard concentrating in Social
                               Studies with a secondary in Mathematics. Welcome
                               to my final project for Gov 50. It is a class in
                               data science which I took for the fall semester
                               in 2020. If you are here, I am presuming you went
                               through my project already. Let me know what you
                               think at angsonamsherpa@college.harvard.edu"),
                               h4(strong("About the project")),
                               p("This project is an attempt at understanding
                               how female representation in Nepal is affected by
                               various district level characteristics ranging
                               from  HDI to literacy. To do so, I extracted a
                               data set consisting of candidates in the 2013 
                               election of Nepal and regressed the proportion
                               of female candidates in each district with
                               various district characteristics. I got to learn
                               a lot about Nepal doing so, and I am really glad
                               in having chosen Nepal for my study."),
                               h4(strong("Motivation")),
                               p("I am from Nepal. I feel deeply connected to
                                 everything that happens there. As a
                                 mo:mo-loving Nepali, I used this opportunity
                                 to learn more about my home country. The major
                                 motivation for choosing Nepal was the
                                 promulugation of the Affirmative Action policy
                                 in 2006 which includes a mandatory 33%
                                 representation of women in each party. I
                                 wanted to explore the potential effects of the
                                 policy in the 2013 elections. I could not find
                                 sufficient data sets online about any other
                                 elections. But I greatly appreciate the works
                                 of Open Data Nepal Project and Nepal in Data - 
                                 two of the foremost organizations at the
                                 forefront of bringing Nepal to the digital age
                                 that it belongs to."),
                               p("You can find the link to my Github right", 
                                 a("here.",
                href="https://github.com/sherpaang/gender_politics_Nepal.git"))
                      )
                  )
              )
    ))

# server logic

server <- function(input, output){
  
  output$literacygif  <- renderImage({
    filename <- file.path('literacy.gif')
    list(src = filename,
         contentType = 'image/gif',
         alt = 'Animated')
    },
    deleteFile = FALSE)
  
  output$gdpgif  <- renderImage({
    filename <- file.path('gdp.gif')
    list(src = filename,
         contentType = 'image/gif',
         alt = 'Animated')
  },
  deleteFile = FALSE)
  
  
    output$hdi <- renderPlot({
        
        hdigraph %>%
            filter(geographical_region == input$select_region) %>%
            ggplot(aes(x = development_region,
                       y = averageHDI)) +
            geom_boxplot() +
            theme_classic() +
            scale_y_continuous(limits = c(0.4, 0.65)) +
            labs(title = "Average HDI across development regions",
                 x = "Development Regions",
                 y = "Average HDI")
        
    }
    )
    
    output$educationcorr <- renderPlot({
      
      educationspendingcorr
      
    })
    
    output$gdpcorrelation <- renderPlot({
      
      gdpcorr
      
    })
    
    output$femlitpop <- renderPlot({
        
        # Plotting a geom col and geom line at the same graph with different y axes
        
        femliteracypop %>%
            filter(development_region == input$select_dr) %>%
            ggplot(aes(x = geographical_region)) +
            geom_col(aes(y = totalfempop), size = 1, color = "white",
                     fill = "darkblue") +
            geom_boxplot(aes(y = 20000*avgfemliteracy), size = 1.25,
                         color = "red") +
            scale_y_continuous(
                sec.axis = sec_axis(~./20000, name = "Female literacy",
                                    breaks = c(10, 20, 30, 40, 50, 60, 70)),
                labels = scales::comma,
                breaks = c(0, 250000, 500000, 750000, 1000000,
                           1250000, 1500000, 1750000, 2000000, 2250000)) +
            theme_classic() +
            labs(title = "Total female population and Average female literacy",
                 subtitle = "Bar plots represent population",
                 x = "Geographical Region",
                 y = "Total female population")
        
    })
    
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
    
    
    output$modeltable <- render_gt({
        
        tbl_regression(model2, intercept = TRUE) %>%
            as_gt() %>%
            tab_header(title = md("Regression of candidate's gender
                                  with various district characteristics")) %>%
            tab_source_note("Source: Open Project Nepal & Nepal in Data")
        
    })

    output$error <- render_gt({
        
        error
        
    })
    
    output$hdicorr <- renderPlot({
        
      hdicorr
        
    })
    
    
}

# Run the application

shinyApp(ui = ui, server = server)
