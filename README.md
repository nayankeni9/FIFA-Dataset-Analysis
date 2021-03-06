## FIFA-Dataset-Analysis

### Description
In this project we perform statistical data analysis in R on a Kaggle dataset taken from https://www.kaggle.com/kevinmh/fifa-18-more-complete-player-dataset . The dataset contains information about the each players physical skills and their preferences in position in a team of 11 players. In all, the dataset contains 185 parameters. The goal of our project is to find out whether a given player plays in offensive (1) or defensive position (0). 

### Initial Analysis
We find that not all parameters are useful in our analysis. We find this out by performing correlation analysis on the parameters. We also perform PCA analysis on the dataset and after initial cleaning out dataset now reduces to a total of 72 predictors. 

### Classification  Models
We apply different classifications models on our dataset which include the following:
1. Linear Discriminant Analysis
2. Quadratic Discriminant Analysis
3. Logistic regression
4. K Nearest Neighbors
5. Support vector Classifier
6. Support Vector Machine (Polynomial)
7. Support Vector Machine (Radial)
8. Random Forest

We also compare the different models based on the test error rate/ prediction accuracy and finding area under the curve on the ROC graph.

### Future Scope
We can use this dataset on a much deeper level to perform various analysis. We can predict the best eleven players for our team. We can also predict optimum price for bidding a new player. We can also predict best positions for all the players in a team. This dataset has got tremendous potential to be explored.
 
