
Hello! Welcome to my final project for Gov 50. Navigation through this repo
ought to be pretty self-explanatory. However, these explanations might help:

## Data Sets

The data sets I collected are in the Raw data folder. All of them have been
accessed at the point of creating this project from the sources listed in the 
project website, i.e., Open Data Project Nepal and Nepal in Data.

All of the data wrangling has been explained thoroughly in the comments in the
wrangling Rmd. After all the important data modifications were done, the
data sets were written into csv files and saved for easier and faster access
for the application.

These csv files have been aptly placed in the main app folder as well, and
they are called cand2013.csv and aggregate.csv.

There are gifs I created to illustrate various plots which have been saved
into www folder inside the app folder to load it later in the final shiny.

## Model

The model can be accessed in an Rmd file called Model.Rmd. It does not belong
to a app.R file for the simple reason that putting it into an RDS file makes
loading the final app much faster and efficient. These RDS files were written
after the model was prepared and were aptly loaded in the final app. They are
called model 2.rds and model 3.rds, while the error terms for the models
considered were put into a csv file called the error.csv

## Shinyapp

The link to my shinyapp is right
[here](https://angsonam.shinyapps.io/gender_politics_Nepal).
