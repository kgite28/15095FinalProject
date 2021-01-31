# 15.095_project
Fairness in Mortgage Application Approvals
MIT Sloan 15.095 Fall 2020 Project

Team Members: Kiran Gite (ksgite@mit.edu), John Lazenby (jlazenby@mit.edu)

Problem Summary

The Fair Housing Act of 1968 states that it is illegal to discriminate in the sale, rental, and financing of residential dwellings. As a result of this law, financial institutions must take great care to avoid considering protected characteristics like race, ethnicity, age, and gender when determining to whom to underwrite a mortgage. We are interested in investigating to what extent financial institutions are compliant, and how these fair lending restrictions impact a financial institution’s ability to make good predictions.

We will reference current literature on building models for mortgage approval data and on building fair machine learning models.
To investigate this problem, we propose the following project phases:

Phase 1: We will train models to predict approvals and rejections, with the condition that each model should be “fair”. This means that the model should either give predictions that are similar across multiple groups, give similar error rates across groups, or that the model should not include protected classes as significant variables.

Phase 2: We will evaluate models on various fairness metrics to see what cost or benefit a fair model could bring - would a bank’s count of mortgage application approvals change if they had to use a fair model, and by how much? Additionally, how much accuracy would we have to sacrifice if a model is constrained to be fair? We can also compare results between financial institutions. 

Data

We will use the publicly available Home Mortgage Disclosure Act (HMDA) data. This dataset tracks all HMDA reportable mortgage applications in the United States. This is available going back to the early 2000’s. Because recently there have been around 14 million mortgage applications per year we will likely like at the most recent available year (2019) and perhaps only look at certain institutions.

The dataset includes features related to the following:

- Institution receiving the application (type of institution, size of institution) 
- Status of the loan (approved, approved but not accepted, denied, withdrawn etc.)
- Conditions of the loan (term, interest rate, government/conventional, purchase/refinance etc.)
- Creditworthiness of the borrower/co-borrower (credit score, debt to income ratio, loan to value ratio etc.)
- Demographic characteristics of the borrower/co-borrower (age, race/ethnicity, gender)
- Zip code of the property being purchased

https://ffiec.cfpb.gov/data-publication/modified-lar/2019
