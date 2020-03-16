# SmartKeyShinyApp

Final Capstone project of the Data Science: Statistics and Machine Learning Specialization by Johns Hopkins University on Coursera.

## Goal of the project.

The main goal of the project is to build a machine learning algorithm in R which will be able to predict the next word the user intends to type and build a user-friendly interface around it (web-application). The project is motivated by SwiftKey virtual keyboard app.

The are three .txt files in English language provided for the task:

* collection of tweets from Twitter (167 mb) 
* collection of blog entries (210 mb)
* collection of news items (205 mb)

The following steps were performed:

* Data reading
* Exploratory data analysis (EDA)
* Data preprocessing and cleaning
* Introducing the idea of a predictive algorithm based on n-Grams

You can find a detailed information about the above steps in [Milesone Report](https://rpubs.com/kitazharov/573608). 

The prediction algorithm used in the application was based on the n-gram model (4-gram in our case), using the stupid backoff algorithm.
The implementation of the stupid backoff algorithm for n-gram model can be found in predict.R file.

## Web application

You can find the application [here](https://mykytazharov.shinyapps.io/SmartKeyBoardApp/).

