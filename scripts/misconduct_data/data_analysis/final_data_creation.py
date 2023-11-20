import joblib

# Load the model
loaded_model = joblib.load('scripts/misconduct_data/creating_final_data/finished_models/action_best_model.joblib')

common_actions = list(set(X_train["action"]) & set(X_test["action"]))

i
predicted_labels = loaded_model.predict(r.misconduct_action)


