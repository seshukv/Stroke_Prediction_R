# Stroke_Prediction_R
The project aims at predicting the stroke for a person. This is achieved by building two models, one with interaction and one without.
The code starts with some cleaning. Following some cleaning, the relation between the stroke variable and other variables are checked using tests like T-test, Wilcoxon, Chi-square and correlation. The insights are used for initial selection for the model. Now mulicollinearity is checked in the model with the initially selected variables and then interaction plot has been plotted to check interactions. Finally backward selection has been used to finalize the variables. The variables in the model passed through as input to the backward selection is based on the initial analysis. 
Finally two models, based on backward selection, are used to predict stroke.
At the end, the results from models with and without interaction are compared using confusionmatrix and the fitness of the model.
