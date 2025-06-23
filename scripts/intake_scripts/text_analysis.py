# -------------------------- Loading Packages and Data -------------------------

# Importing Packages
import json
import string
import spacy
from nltk.util import ngrams
from nltk.tokenize import word_tokenize, sent_tokenize
from nltk.corpus import stopwords
from nltk.stem import PorterStemmer
import ssl
from collections import Counter, defaultdict
from sentence_transformers import SentenceTransformer, util
import pandas as pd
import re
from dataclasses import dataclass

# try:
#     _create_unverified_https_context = ssl._create_unverified_context
# except AttributeError:
#     pass
# else:
#     ssl._create_default_https_context = _create_unverified_https_context
# 
# nltk.download("stopwords")
# nltk.download('punkt')

# Loading JSON of narratives
with open('narrative_list.json') as f:
    narrative_list = json.load(f)
    
# Getting distinct narratives
narrative_list = list(set(narrative_list))

# Removing none and NaN
cleaned_list = [x for x in narrative_list if x is not None and x == x]

# Defining stop words and stemmer
stop_words = set(stopwords.words("english"))
stemmer = PorterStemmer()

def counter_to_markdown(counter, title):
    df = pd.DataFrame(counter.most_common(n), columns=[title, 'Count'])
    return df.to_markdown(index=False)

# ------------------------------ Creating Lists --------------------------------

# Creating necessary lists
words = [word for paragraph in cleaned_list for word in word_tokenize(paragraph) if word.casefold() not in stop_words and word.isalpha()]
words_stemmed = [stemmer.stem(word) for word in words]
bigrams = [bigram for paragraph in cleaned_list for bigram in ngrams([word.lower() for word in word_tokenize(paragraph) if word.isalpha()], 2)]
trigrams = [trigram for paragraph in cleaned_list for trigram in ngrams([word.lower() for word in word_tokenize(paragraph) if word.isalpha()], 3)]
bigrams_cleaned = [bigram for paragraph in cleaned_list for bigram in ngrams([stemmer.stem(word.lower()) for word in word_tokenize(paragraph) if word.isalpha() and word.casefold() not in stop_words], 2)]
trigrams_cleaned = [bigram for paragraph in cleaned_list for bigram in ngrams([stemmer.stem(word.lower()) for word in word_tokenize(paragraph) if word.isalpha() and word.casefold() not in stop_words], 3)]
sentences = [sentence for paragraph in cleaned_list for sentence in sent_tokenize(paragraph)]
   
# Loading Spacy library
nlp = spacy.load("en_core_web_sm")

persons = [ent.text for sentence in sentences for ent in nlp(sentence).ents if ent.label_ == "PERSON"]
places = [ent.text for sentence in sentences for ent in nlp(sentence).ents if ent.label_ in ("GPE", "LOC")]
nouns = [token.text for sentence in sentences for token in nlp(sentence) if token.pos_ == "NOUN"  and not token.is_stop]
verbs = [token.text for sentence in sentences for token in nlp(sentence) if token.pos_ == "VERB"  and not token.is_stop]
adjectives = [token.text for sentence in sentences for token in nlp(sentence) if token.pos_ == "ADJ"  and not token.is_stop]

# --------------------------------- Counts ------------------------------------

# How many want to see
n = 30

# Most common stemmed words
print(counter_to_markdown(Counter(words_stemmed), "Word"))

# Most common bigrams
print(counter_to_markdown(Counter(bigrams_cleaned), "Bigram"))

# Most common trigrams
print(counter_to_markdown(Counter(trigrams_cleaned), "Trigram"))

# Most common people
print(counter_to_markdown(Counter(persons), "Person"))

# Most common places
print(counter_to_markdown(Counter(places), "Place"))

# Most common nouns
print(counter_to_markdown(Counter(nouns), "Noun"))

# Most common verbs
print(counter_to_markdown(Counter(verbs), "Verb"))

# Most common adjectives
print(counter_to_markdown(Counter(adjectives), "Adjective"))

# -------------------------- Clustering Sentences ------------------------------

# Clustering sentences
model = SentenceTransformer('all-mpnet-base-v2')
embeddings = model.encode(sentences, convert_to_tensor=True)
clusters = util.community_detection(embeddings, min_community_size=10, threshold=0.5)

# Turning clusters into a dataframe
data = []
for cluster_id, cluster in enumerate(clusters):
    for sentence_id in cluster:
        data.append({
            'sentence': sentences[sentence_id],
            'cluster': cluster_id
        })

df = pd.DataFrame(data)

# --------------------------- Key Word Extraction ------------------------------

# Defining key words
key_terms = {
    "Police Actions": [
        (r"shoot\w*|shot", "shoot"),
        (r"firearm\w*", "firearm"),
        (r"gun\w*", "gun"),
        (r"taser\w*", "taser"),
        (r"choke\w*", "choke"),
        (r"strangle\w*", "strangle"),
        (r"kneel\w*", "kneel"),
        (r"restrain\w*", "restrain"),
        (r"abuse\w*", "abuse"),
        (r"arrest\w*", "arrest"),
        (r"beat\w*", "beat"),
        (r"strike\w*", "strike"),
        (r"punch\w*", "punch"),
        (r"assault\w*", "assault"),
        (r"harass\w*", "harass"),
        (r"kick\w*", "kick"),
        (r"spray\w*", "spray"),
        (r"mace\w*", "mace"),
        (r"handcuff\w*|cuff\w*", "handcuff"),
        (r"detain\w*", "detain"),
        (r"apprehend\w*", "apprehend"),
        (r"drag\w*", "drag"),
        (r"slam\w*", "slam"),
        (r"search\w*", "search"),
        (r"raid\w*", "raid"),
        (r"smash\w*", "smash"),
        (r"tackle\w*", "tackle"),
        (r"push\w*", "push"),
        (r"grab\w*", "grab"),
        (r"rape\w*", "rape"),
        (r"sexual assault\w*", "sexual assault"),
        (r"hit\w*", "hit"),
        (r"bash\w*", "bash"),
        (r"throw\w*", "throw"),
        (r"shove\w*", "shove"),
        (r"wrestle\w*", "wrestle"),
        (r"force\w*", "force"),
        (r"struck\w*", "struck"),
        (r"slap\w*", "slap"),
        (r"whip\w*", "whip"),
        (r"deploy\w*", "deploy"),
        (r"tear gas", "tear gas"),
        (r"pepper spray", "pepper spray"),
        (r"club\w*", "club"),
        (r"baton\w*", "baton"),
        (r"chase\w*", "chase"),
    ],
    "Outcomes": [
        (r"kill\w*", "kill"),
        (r"dead|deceased|die\w*", "dead"),
        (r"fatal\w*", "fatal"),
        (r"deaths?", "death"),
        (r"injur\w*", "injury"),
        (r"wound\w*", "wound"),
        (r"bleed\w*", "bleed"),
        (r"bruise\w*", "bruise"),
        (r"hospitals?", "hospital"),
        (r"hospitalize\w*", "hospitalize"),
        (r"emergency room|er", "emergency room"),
        (r"icu", "ICU"),
        (r"coma", "coma"),
        (r"paralyze\w*", "paralyze"),
        (r"disable\w*", "disable"),
        (r"lifeless", "lifeless"),
        (r"autopsy", "autopsy"),
        (r"critical", "critical"),
        (r"fracture\w*", "fracture"),
        (r"disfigure\w*", "disfigure"),
        (r"unconscious\w*", "unconscious"),
        (r"respiratory arrest", "respiratory arrest"),
        (r"cardiac arrest", "cardiac arrest"),
        (r"suffocate\w*", "suffocate"),
    ],
    "Officer Involved": [
        (r"officer\w*", "officer"),
        (r"cop\w*", "cop"),
        (r"police", "police"),
        (r"detective\w*", "detective"),
        (r"sheriff\w*", "sheriff"),
        (r"deputy\w*", "deputy"),
        (r"trooper\w*", "trooper"),
        (r"constable\w*", "constable"),
        (r"marshal\w*", "marshal"),
        (r"agent\w*", "agent"),
    ],
    "Location": [
        (r"traffic\w*", "traffic"),
        (r"home\w*", "home"),
        (r"house\w*", "house"),
        (r"apartment\w*", "apartment"),
        (r"residence\w*", "residence"),
        (r"street\w*", "street"),
        (r"intersection\w*", "intersection"),
        (r"road\w*", "road"),
        (r"highway\w*", "highway"),
        (r"parking lot\w*", "parking lot"),
        (r"protest\w*", "protest"),
        (r"march\w*", "march"),
        (r"rall\w*", "rally"),
        (r"public", "public"),
        (r"store\w*", "store"),
        (r"gas station\w*", "gas station"),
        (r"scene\w*", "scene"),
        (r"school\w*", "school"),
        (r"universit\w*", "university"),
        (r"neighborhood\w*", "neighborhood"),
        (r"community\w*", "community"),
        (r"church\w*", "church"),
        (r"park\w*", "park"),
        (r"block", "block"),
        (r"alley\w*", "alley"),
        (r"sidewalk\w*", "sidewalk"),
        (r"detention center", "detention center"),
        (r"jail", "jail"),
        (r"prison", "prison"),
        (r"court\w*", "court"),
        (r"station\w*", "station"),
        (r"alleyway", "alleyway"),
        (r"side street", "side street"),
        (r"parking garage", "parking garage"),
        (r"bus stop", "bus stop"),
        (r"mall", "mall"),
        (r"suburb\w*", "suburb"),
    ],
    "Other": [
        (r"racism", "racism"),
        (r"racial", "racial"),
        (r"racist", "racist"),
        (r"discrimination", "discrimination"),
        (r"bias", "bias"),
        (r"unarmed", "unarmed"),
        (r"injustice", "injustice"),
        (r"body cam\w*", "body camera"),
        (r"footage", "footage"),
        (r"lawsuit\w*", "lawsuit"),
        (r"complaint\w*", "complaint"),
        (r"civil rights", "civil rights"),
        (r"rights violation", "rights violation"),
        (r"investigation\w*", "investigation"),
        (r"indictment\w*", "indictment"),
        (r"charges", "charges"),
        (r"trial\w*", "trial"),
        (r"convicted", "convicted"),
        (r"accountability", "accountability"),
        (r"reform\w*", "reform"),
        (r"policy", "policy"),
        (r"cover up", "cover up"),
        (r"prosecut\w*", "prosecute"),
        (r"settlement\w*", "settlement"),
        (r"profiling", "profiling"),
        (r"surveillance", "surveillance"),
        (r"whistleblower\w*", "whistleblower"),
        (r"retaliation", "retaliation"),
        (r"misconduct", "misconduct"),
        (r"immunity", "immunity"),
        (r"violence", "violence"),
    ],
    "Emotional Impact": [
        (r"fear\w*", "fear"),
        (r"trauma", "trauma"),
        (r"shaken", "shaken"),
        (r"terrified", "terrified"),
        (r"panic\w*", "panic"),
        (r"cry\w*", "cry"),
        (r"scream\w*", "scream"),
        (r"mental health", "mental health"),
        (r"shock", "shock"),
        (r"breakdown", "breakdown"),
        (r"nightmare", "nightmare"),
        (r"helpless", "helpless"),
        (r"powerless", "powerless"),
        (r"grief", "grief"),
        (r"mourning", "mourning"),
        (r"anxiety", "anxiety"),
        (r"depression", "depression"),
        (r"stress", "stress"),
        (r"ptsd", "PTSD"),
        (r"anguish", "anguish"),
        (r"distress", "distress"),
        (r"fearful", "fearful"),
        (r"dread", "dread"),
        (r"hopeless", "hopeless"),
        (r"vulnerable", "vulnerable"),
    ],
    "Immigration": [
      (r"immigration", "immigration"),
      (r"detention", "detention"),
      (r"deport\w*", "deportation"),
      (r"migrant\w*", "migrant"),
      (r"refugee\w*", "refugee"),
      (r"asylum", "asylum"),
      (r"border patrol", "border patrol"),
      (r"customs and border protection|cbp", "customs and border protection"),
      (r"detainee\w*", "detainee"),
      (r"holding center\w*", "holding center"),
      (r"immigrant detention center", "detention center"),
      (r"immigration enforcement", "immigration enforcement"),
    ]
}

# Defining the category matches
category_matches = defaultdict(lambda: defaultdict(list))

# Looping through the cleaned list and seaching the sentences for matches
for sent in cleaned_list:
    sent_lower = sent.lower()
    for cat, kws in key_terms.items():
        for pattern, label in kws:
            if re.search(pattern, sent_lower):
                category_matches[cat][label].append(sent)

# Defining the dataframe
rows = [
    {
      "Category": cat, 
      "Keyword": label, 
      "Count": len(sents), 
      "Example Sentences": sents[:100]}
    for cat, kw in category_matches.items()
    for label, sents in kw.items()
]

# Creating the dataframe
df = pd.DataFrame(rows).sort_values(["Category", "Count"], ascending=[True, False])
df.to_markdown()
