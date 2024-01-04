# Rename variables for clarity
misconduct = r.misconduct_allegation

# Import necessary libraries
import gensim
from gsdmm import MovieGroupProcess
from tqdm import tqdm
import nltk
from nltk.stem import *

# Additional cleaning and preprocessing
import re
import numpy as np
import pandas as pd

# Set up stopwords
stopwords = set(nltk.corpus.stopwords.words('english'))

# Initialize an empty list for documents
docs = []

# Tokenize and filter stopwords for each allegation
for allegation in misconduct.allegation_better:
    words = [w for w in nltk.tokenize.word_tokenize(allegation) if (w not in stopwords)]
    docs.append(words)

# Create a Gensim dictionary from the documents
dic = gensim.corpora.Dictionary(docs)
vocab_length = len(dic)

# Initialize and fit the MovieGroupProcess model
gsdmm = MovieGroupProcess(K=30, alpha=0.1, beta=0.1, n_iters=30)
y = gsdmm.fit(docs, vocab_length)

# Analyze and print the top clusters and their top words
doc_count = np.array(gsdmm.cluster_doc_count)
top_index = doc_count.argsort()[-15:][::-1]

def top_words(cluster_word_distribution, top_cluster, values):
    for cluster in top_cluster:
        sort_dicts = sorted(gsdmm.cluster_word_distribution[cluster].items(), key=lambda k: k[1], reverse=True)[:values]
        print('Cluster %s : %s' % (cluster, sort_dicts))
        print('-' * 120)

top_words(gsdmm.cluster_word_distribution, top_index, 7)

# Update top_index for a larger number of clusters
top_index = doc_count.argsort()[-40:][::-1]

# Define topic names for better representation
topic_dict = {}
topic_names = ['Topic #{}'.format(i+1) for i in range(len(top_index))]

for i, topic_num in enumerate(top_index):
    topic_dict[i] = topic_names[i]

# Create a function to assign topics to allegations and store in a DataFrame
def create_topics_dataframe(data_text=misconduct.allegation_better, gsdmm=gsdmm, threshold=0.3, topic_dict=topic_dict, stem_text=docs):
    result = pd.DataFrame(columns=['text', 'topic', 'stems'])
    for i, text in enumerate(data_text):
        result.at[i, 'text'] = text
        result.at[i, 'stems'] = stem_text[i]
        prob = gsdmm.choose_best_label(stem_text[i])
        if prob[1] >= threshold:
            result.at[i, 'topic'] = topic_dict[prob[0]]
        else:
            result.at[i, 'topic'] = 'Other'
    return result

# Create the topics DataFrame
dfx = create_topics_dataframe(data_text=misconduct.allegation_better, gsdmm=gsdmm, threshold=0.3, topic_dict=topic_dict, stem_text=docs)
