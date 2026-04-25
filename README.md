My 7 projects can be found in each branch.
1. combining-files-to-csv
   This is a toy example to combine financial data files with delimiter "|" and adding columns to produce a final dataframe. It contains a file "generate_pipe.py" using python code to produce 10 financial data files randomly. Another data file is "data_combination.py", using python to produce the final dataframe.
   
2. credit-data-analysis
   This is a project studying default risk of credit card datast provided by Kaggle. I have used different types of modelling method, including neural network, ridge regression and xgboost, to do a regression against credit limit, classifying default and customer segmentation.
   
3. ecommerce-full-data-analysis
   This is a full data analysis project of simulated ecommerce dataset from Kaggle. I have built up 4 excel worksheets with pivot charts and pivot tables for categorical analysis. Then, I have built up 2 dashboards in Tableau Public for visualizing the data in an interactive view. After that, I have use Python to build 48 linear regression models to study the features, with a deep learning tensorflow neural network model to forecast the revenue. Moreover, I have written a R code to build Product Segmentation Machine Learning Models and a Logistic Regression model to forecast the performance of a new product with price. Lastly, I have produced a report and powerpoint to present my findings, assume I am working in data team and present the material to other departments.
   The link of Tableau Dashboard is https://public.tableau.com/app/profile/chun.hei.poon/viz/Sales_Analysis_1_17690613888060/Dashboard1
 
4. housing-prices-time-series-with-regression
   This is a project studying the rental and transaction of public housing prices in Hong Kong from 2020 to 2023. The first file, "Housing_Prices.ipynb", shows the result and report of the investigation. I have done data analysis first by ploting visuals with Tableau, to highlight the data characteristics so as to understand the dataset. Then, I have done a time series analysis by fourier series. Unfortunately, the time series model shows a significant error.
   As a result, I decided simply using the features of the house to regress against the prices by R. After building scatterplots, testing the multicollinearity and doing the box-cox transformation, I have built up a regression model by forward selection with Akaike Information Criterion. The code can be found in "data_science_project.R".
   
5. movie-rating-neural-network-with-matrix-factorization
   This is a tensorflow deep learning project buiding up a recommendation system to suggest movie for users. "Recommender_System.ipynb" is a report with python codes. I have studied the american movie datasets based on the movie characteristics, user characteristics and the given rating. After analyse and clean the data, I have set up latent vector for each categorical variable to quantify them. Then, I build up neural network layers separately for user and movie, but combining the output in the final layer. This Neural Network Matrix Factorization Method with side information Recommendation System could make a suggestion for existing users or movies, cold-start users or movies. It highlights the use of data science in the marketing field.

6. player_value-data-analysis-with-regression
   This is a project studying football palyer value estimated by transfermarkt from 2000 to 2024. I have build up visuals to study the data pattern by Tableau, then use sql to transform the dataset. The visuals have shown the update month by transfermarkt, variation in market value. By doing a p-test on estimated value and transaction price, the performance of estimation is quite poor. Lastly, I have used R to regress average transfer fee in UK against domestic and world GDP. Surprisingly, the relationship of English Premium League and World GDP is stronger.

7. vba-toy-competition-example
   This is a toy example to show my ability in scrapping discord message and handle competition data in excel by vba. As a competition organizer in a discord server, I have recorded the competitors responses from discord to an excel through the code in "discord_scrap_comp_github.ipynb". Then, I rank and build up a nice output worksheet by vba in "vba_toy_competition_analysis.xlsm". Noted that the data and user name has changed to protect privacy.
