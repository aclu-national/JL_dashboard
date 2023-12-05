# Loading packages
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.compose import ColumnTransformer
from sklearn.pipeline import Pipeline
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import accuracy_score, classification_report
from sklearn.preprocessing import OneHotEncoder
from sklearn.model_selection import GridSearchCV
from sklearn.svm import SVC
from sklearn.naive_bayes import MultinomialNB
import joblib

# Loading data
misconduct = r.misconduct_allegation

# Defining dataframes
X = misconduct[['allegation', 'allegation_desc', 'agency']]
y = misconduct['allegation_classified']

# Splitting the data using a 50% train-test split and a random state of 1
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.5, random_state=1)

# Create a transformer to convert allegations to a matrix of TF-IDF features
text_transformer = TfidfVectorizer(max_features=1000, stop_words='english')

# Create a transformer to one-hot encode law enforcement agencies
categorical_transformer = Pipeline(steps=[
    ('onehot', OneHotEncoder(handle_unknown='ignore'))
])

# Create a preprocessor to apply the transformers to respective columns
preprocessor = ColumnTransformer(
    transformers=[
        ('allegation', text_transformer, 'allegation'),
        ('allegation_desc', text_transformer, 'allegation_desc'),
        ('agency', categorical_transformer, ['agency'])
    ])

# Defining the RF grid search parameters
rf_param_grid = {
    'classifier__n_estimators': [100, 200, 500],
    'classifier__max_depth': [None, 10, 20, 50],
    'classifier__min_samples_split': [2, 10, 50]
}

# Defining the SVM grid search parameters
svm_param_grid = {
    'classifier__C': [0.1, 1, 10, 100],
    'classifier__kernel': ['linear', 'rbf', 'poly'],
}

nb_param_grid = {
    'classifier__alpha': [0.1, 0.5, 1.0, 2.0],  # Adjust the alpha values as needed
}

# Defining models
rf_classifier = RandomForestClassifier(random_state=1)
svm_classifier = SVC(random_state=1)
nb_classifier = MultinomialNB()

# Defining RF Grid Search
rf_grid_search = GridSearchCV(
    Pipeline([('preprocessor', preprocessor), ('classifier', rf_classifier)]),
    rf_param_grid,
    cv=5,
    n_jobs=-1
)

# Defining SVM Grid Search
svm_grid_search = GridSearchCV(
    Pipeline([('preprocessor', preprocessor), ('classifier', svm_classifier)]),
    svm_param_grid,
    cv=5,
    n_jobs=-1
)

nb_grid_search = GridSearchCV(
    Pipeline([('preprocessor', preprocessor), ('classifier', nb_classifier)]),
    nb_param_grid,
    cv=5,
    n_jobs=-1
)

rf_grid_search.fit(X_train, y_train)
svm_grid_search.fit(X_train, y_train)
nb_grid_search.fit(X_train, y_train)

best_rf_classifier = rf_grid_search.best_estimator_
best_svm_classifier = svm_grid_search.best_estimator_
best_nb_classifier = nb_grid_search.best_estimator_


y_pred_rf = best_rf_classifier.predict(X_test)
y_pred_svm = best_svm_classifier.predict(X_test)
y_pred_nb = best_nb_classifier.predict(X_test)

rf_accuracy = accuracy_score(y_test, y_pred_rf)
svm_accuracy = accuracy_score(y_test, y_pred_svm)
nb_accuracy = accuracy_score(y_test, y_pred_nb)

print("Random Forest Classifier Accuracy:", rf_accuracy)
print("Support Vector Machine Classifier Accuracy:", svm_accuracy)
print("Multinomial Naive Bayes Classifier Accuracy:", nb_accuracy)


print(classification_report(y_test, y_pred_rf))
print(classification_report(y_test, y_pred_svm))
print(classification_report(y_test, y_pred_nb))












common_allegations = list(set(X_train["allegation"]) & set(X_test["allegation"]))

# Filter test set to include only common dispositions
X_test_uncommon = X_test[-X_test['allegation'].isin(common_allegations)]
y_test_uncommon = y_test.loc[X_test_uncommon.index]

X_test_common = X_test[X_test['allegation'].isin(common_allegations)]
y_test_common = y_test.loc[X_test_common.index]



# Get predictions for the common allegationsX_test_uncommon
y_pred_rf_uncommon = best_rf_classifier.predict(X_test_uncommon)
y_pred_svm_uncommon = best_svm_classifier.predict(X_test_uncommon)
y_pred_nb_uncommon = best_nb_classifier.predict(X_test_uncommon)

y_pred_rf_common = best_rf_classifier.predict(X_test_common)
y_pred_svm_common = best_svm_classifier.predict(X_test_common)
y_pred_nb_common = best_nb_classifier.predict(X_test_common)


# Calculate accuracy on the entire test set
rf_accuracy_uncommon = accuracy_score(y_test_uncommon, y_pred_rf_uncommon)
rf_accuracy_uncommon
svm_accuracy_uncommon = accuracy_score(y_test_uncommon, y_pred_svm_uncommon)
svm_accuracy_uncommon
nb_accuracy_uncommon = accuracy_score(y_test_uncommon, y_pred_nb_uncommon)
nb_accuracy_uncommon

rf_accuracy_common = accuracy_score(y_test_common, y_pred_rf_common)
rf_accuracy_common
svm_accuracy_common = accuracy_score(y_test_common, y_pred_svm_common)
svm_accuracy_common
nb_accuracy_common = accuracy_score(y_test_common, y_pred_nb_common)
nb_accuracy_common

# Compare accuracies
model_accuracies = {
    'Random Forest': rf_accuracy_uncommon,
    'Support Vector Machine': svm_accuracy_uncommon,
    'Naive Bayes': nb_accuracy_uncommon
}

# Find the most accurate model
best_model_name = max(model_accuracies, key=model_accuracies.get)
best_model = None

if best_model_name == 'Random Forest':
    best_model = best_rf_classifier
elif best_model_name == 'Support Vector Machine':
    best_model = best_svm_classifier
elif best_model_name == 'Naive Bayes':
    best_model = best_nb_classifier

# Save the best model
if best_model is not None:
    joblib.dump(best_model, 'scripts/misconduct_data/classification_models/finished_models/allegation_best_model.joblib')
else:
    print("No best model found.")
    

