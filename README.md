# Heart-Failure-
Heart Failure Prediction and Survival Analysis

Cardiovascular Diseases kill approximately 17.9 million people globally every year which accounts for 32% of all deaths worldwide (WHO, 2019).85% were due to heart attack and stroke with over three quarters of Cardiovascular Diseases deaths taking place in low- and middle-income countries.
However, most cardiovascular disease can be prevented by addressing behavioral risk factors to population-wide strategies.This project is aiming to do an exploratory data analysis,  using generalized linear model to detect the most crucial features to predict the heart failure event and apply Cox model, Survival Analysis, and Hazard Ratio to validate the result.

Dataset contained the medical records of 299 heart failure patients collected at the Faisalaband Institute of Cardiology and at the Allied Hospital in Faisalabad (Punjab, Pakistan) in April - December 2015. Data population was made up of 194 Males and 105 Females. at the end of the study, 96 (32%) participants died.
The dataset contains 13 features wwhich report clinical and lifestyle information. some of the features are binary (e.g., diabetic status, anaemeic status, smoking status). information on the serum creatinine levels are available although
dataset does not indicate if any of the patients has a primary kidney disease. The death event feature as our target is a binary classification, states if the patient survived before the end of the follow up period.

Age, Serum Creatinine and Ejection Fraction were identifed as highly correlated to the death event and when modeled using the logistic regression had significant p-vales at 0.05 as it the model cotaining these three values had the lowest AIC score. Model had a 76.27% accuracy(95% C.I 63.41%, 86.38%) in predicitng the outcome death with a Sensitivity of 75% and a Specificity of 85.71%.

In the Survival Analysis, the data set was divided into 2 groups : 
  a. low risk - age < 65, normal ejection fraction (41-75%), normal serum creatinine levels(0.74-          1.35mg/dL)
  b. high risk - age > 65, abnormal ejection fraction and serum creatinine levels 
It was observed that p values(0.08) from the cox model was consistent with result that 
Serum Creatinine, Ejection Fraction and Age are the most important variables in patients with heart disease in predicting death outcome.

Therefore, public education can be directed towards lifestyle modifications that can improve ejection fraction, reduces serum creatine not only for those over 65years but younger as well.


