# RST-ARM-GLM_-Research

Machine learning interpretability is an important concept in the world of data analytics. How can we be able to trust the decisions made on our data by the models we employ on them.
The research seeks to use Rough Sets which is an information granulation and feature selection method to sift through attributes of any dataset, then model the selected attributes using apirori association rule mining method to find objectives rule from the selected attributes.
The main intention is to make the user see the kind of attribute sthe model is choosing for it's prediction purposes.
The reserach starts off with laoding of the datasets and preprocessing them to ensure that the data is clean and free from errors or omissions.
Preprocessing was the main hurdle in this research especially the kirogo datasets which had 30min interval weather reading.
The structure of the datasets are as follows:
Kirogo attributes were: Date/Time;RH(%),Temp(Degrees celcius), Rainfall
Kariki attributes: Date, Min Temp,Avg Temp, Max Temp, Min humidiy, Max humidity, Avg humidity, windspeed, precipitation amount and rain factor.


Next I run sample machine learning models on the dataset to get insights on the data, this is important as it will form the basis for comparing my proposed model performance the standard machine learning models i will have choosen for the study.

The project starts with preprocessing, followed by getting the required features for modelling on the data via the RoughSet feature selection method using greedy heuristic method. Feature selection is key in machine learning interpretability because through it only important features will be selected and this makes interpretability abit more easier. Secondly more interpretability will be provided for in the decision ruels section