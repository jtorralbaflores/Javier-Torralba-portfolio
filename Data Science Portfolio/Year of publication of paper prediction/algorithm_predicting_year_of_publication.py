import logging
import pandas as pd
import json
from sklearn.compose import ColumnTransformer
from sklearn.pipeline import make_pipeline
from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import OneHotEncoder
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.preprocessing import RobustScaler
from sklearn.preprocessing import StandardScaler

def main():
    logging.getLogger().setLevel(logging.INFO)
    logging.info("Loading training/test data")
    train = pd.DataFrame.from_records(json.load(open('train.json'))).fillna('unknown')
    test = pd.DataFrame.from_records(json.load(open('test.json'))).fillna('unknown')

    # Preprocess author and editor columns
    train['author_str'] = train['author'].apply(lambda x: ' ; '.join(x))
    test['author_str'] = test['author'].apply(lambda x: ' ; '.join(x))
    train['editor_str'] = train['editor'].apply(lambda x: ' ; '.join(x) if isinstance(x, list) else 'unknown')
    test['editor_str'] = test['editor'].apply(lambda x: ' ; '.join(x) if isinstance(x, list) else 'unknown')

    # Additional feature engineering
    train['num_authors'] = train['author'].apply(len)
    test['num_authors'] = test['author'].apply(len)
    train['title_length'] = train['title'].apply(lambda x: len(x.split()))
    test['title_length'] = test['title'].apply(lambda x: len(x.split()))
    train['abstract_length'] = train['abstract'].apply(lambda x: len(x.split()))
    test['abstract_length'] = test['abstract'].apply(lambda x: len(x.split()))

    # Feature transformation
    featurizer = ColumnTransformer(
        transformers=[
            ("entrytype", OneHotEncoder(), ["ENTRYTYPE"]),
            ("title", TfidfVectorizer(), "title"),
            ("author", TfidfVectorizer(), "author_str"),
            ("publisher", CountVectorizer(), "publisher"),
            ("abstract_length", StandardScaler(), ["abstract_length"]),
            ("num_authors", StandardScaler(), ["num_authors"]),
            ("editor", TfidfVectorizer(), "editor_str"),
            ("abstract", TfidfVectorizer(), "abstract"), 
            ("title_length", StandardScaler(), ['title_length'])
        ],
        remainder='drop')

    # Model fitting
    log_reg = make_pipeline(featurizer, LogisticRegression(multi_class='multinomial', max_iter=1000))
    logging.info("Fitting logistic regression model on the entire training data")
    log_reg.fit(train.drop('year', axis=1), train['year'].values)

    # Prediction
    logging.info("Predicting on test data")
    pred = log_reg.predict(test)
    test['year'] = pred

    # Writing prediction file
    logging.info("Writing prediction file")
    test.to_json("predicted_log_reg_full_train_third_attempt.json", orient='records', indent=2)

main()
