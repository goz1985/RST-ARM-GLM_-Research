# RST-ARM-GLM_-Research

Machine learning interpretability is an important concept in the world of data analytics. How can we be able to trust the decisions made on our data by the models we employ on them.
The research seeks to use Rough Sets which is an information granulation and feature selection method to sift through attributes of any dataset, then model the selected attributes using apirori association rule mining method to find objectives rule from the selected attributes.
The main intention is to make the user see the kind of attribute sthe model is choosing for it's prediction purposes.
The reserach starts off with laoding of the datasets and preprocessing them to ensure that the data is clean and free from errors or omissions.
Preprocessing was the main hurdle in this research especially the kirogo datasets which had 30min interval weather reading.
The structure of the datasets are as follows:
Kirogo attributes were: Date/Time;RH(%),Temp(Degrees celcius), Rainfall
Kariki attributes: Date, Min Temp,Avg Temp, Max Temp, Min humidiy, Max humidity, Avg humidity, windspeed, precipitation amount and rain factor.


Next I run sample machine learning models on the data set to get insights on the data, this is important as it will form the basis for comparing my proposed model performance against other the standard machine learning models. However the main purpose of the research is to show the link between interpretability via decision rules and weights used in a the Logit model for the Generalized linear model.

The project starts with preprocessing, this is important so as to make the data ready for modeling with our chosen models. Also involved during this process will be exploratory analysis of the data, this is necessary so as that we can see the various statistical distribution aspect of the data before making predictions on it.

This is followed by getting the required features for modelling on the data via the Rough Set feature selection method using greedy heuristic method. Feature selection is key in machine learning interpretability because through it only important features will be selected and this makes interpretability a bit more easier. 

Secondly from the deduced feature subset gotten by the Rough Set method in step one above, these features will passed through the arules method to derive decision rules. The selection of rules will be based on the confidence and support metrics used to evaluate how good a rule is on the data it is operating on.

After deducing of the decision rules, a data frame containing the rules will be formulated and then Logit model for classification will be applied on to this rule data-frame. The rule data-frame will contain binary values representing the decision rules choosen in step two above.

The decision rule data frame will be used on the logit model to model the decision variables based on the dataset used. The model will be evaluated against a normal logistic regression model not employing the RAGL framework.

THE RAGL ALGORITHM
