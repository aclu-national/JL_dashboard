# ----------------------------- Importing Packages ------------------------------
# Importing packages
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.compose import ColumnTransformer
from sklearn.pipeline import Pipeline
from sklearn.svm import SVC
from sklearn.multioutput import MultiOutputClassifier
from sklearn.metrics import accuracy_score, classification_report
from sklearn.model_selection import GridSearchCV
from sklearn.preprocessing import MultiLabelBinarizer
from imblearn.over_sampling import RandomOverSampler
from sklearn.ensemble import RandomForestClassifier
from sklearn.multiclass import OneVsRestClassifier

# ----------------------------- Defining our Data ------------------------------

# Loading the labelled data
misconduct = pd.read_csv("labelled_data.csv")

# Defining our variables
X = misconduct[['allegation', 'allegation_desc']]
X['allegation'] = X['allegation'].astype(str)
X['allegation_desc'] = X['allegation_desc'].astype(str)

# Converting multi-label classification into binary labels
y_labels = [labels.split(', ') for labels in misconduct['classification']]
mlb = MultiLabelBinarizer()
y = mlb.fit_transform(y_labels)

# Splitting the data into training and testing sets using a 50-50 split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.5, random_state=2)

# Creating a transformer to convert allegations to a matrix of TF-IDF features and removing stop-words
text_transformer = TfidfVectorizer(max_features=500, stop_words='english')

# Creating a preprocessor to apply the transformers to the allegation and allegation description variables
preprocessor = ColumnTransformer(
    transformers=[
        ('allegation', text_transformer, 'allegation'),
        ('allegation_desc', text_transformer, 'allegation_desc')
    ])

# ----------------------------- Creating and Fitting SVM ------------------------------

# Defining  SVM grid search parameters
svm_param_grid = {
    'classifier__estimator__C': [0.1, 1, 10, 100, 500],
    'classifier__estimator__kernel': ['linear', 'rbf', 'poly'],
}

# Combining the transformers and classifiers in a pipeline using OneVsRestClassifier classification
svm_pipeline = Pipeline([
    ('preprocessor', preprocessor),
    ('classifier', OneVsRestClassifier(SVC(probability=True, random_state=1)))
])

# Defining the SVM Grid Search
svm_grid_search = GridSearchCV(
    svm_pipeline,
    svm_param_grid,
    cv=5,
    n_jobs=-1
)

# Fitting the SVM grid search on the training data
svm_grid_search.fit(X_train, y_train)

# Getting the best SVM classifier
best_svm_classifier = svm_grid_search.best_estimator_

# Predicting on the test set
y_pred_svm = best_svm_classifier.predict(X_test)

# ----------------------------- Creating and Fitting RF ------------------------------

# Defining the Random Forest grid search parameters
rf_param_grid = {
    'classifier__estimator__n_estimators': [50, 100, 200],
    'classifier__estimator__max_depth': [None, 10, 20],
}

# Defining the multi-label Random Forest classifier using MultiOutputClassifier
rf_classifier = MultiOutputClassifier(RandomForestClassifier(random_state=1))

# Combining the transformers and classifiers in a pipeline
rf_pipeline = Pipeline([('preprocessor', preprocessor), ('classifier', rf_classifier)])

# Defining Random Forest Grid Search
rf_grid_search = GridSearchCV(
    rf_pipeline,
    rf_param_grid,
    cv=5,
    n_jobs=-1
)

# Fitting the Random Forest grid search on the training data
rf_grid_search.fit(X_train, y_train)

# Getting the best Random Forest classifier
best_rf_classifier = rf_grid_search.best_estimator_

# Predicting on the test set
y_pred_rf = best_rf_classifier.predict(X_test)

# Combine predictions with the original test set and actual labels
predict_rf = pd.concat([X_test.reset_index(drop=True), pd.DataFrame(data=y_pred_rf, columns=target_names)], axis=1)
predict_svm = pd.concat([X_test.reset_index(drop=True), pd.DataFrame(data=y_pred_svm, columns=target_names)], axis=1)
test = pd.concat([X_test.reset_index(drop=True), pd.DataFrame(data=y_test, columns=target_names)], axis=1)

# ----------------------------- Exporting Predictions ------------------------------
predict_rf.to_csv("test_and_predictions/rf_predictions.csv")
predict_svm.to_csv("test_and_predictions/svm_predictions.csv")
test.to_csv("test_and_predictions/test_for_predictions.csv")
