#loading the required packages

import csv
import pandas as pd
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer

#Reading the required file to perform the sentiment score calculation
input = pd.read_csv("amazon_baby_train.csv")
analyzer = SentimentIntensityAnalyzer()
out = open("sentiment_train.csv",'w')
csv_out=csv.writer(out)
csv_out.writerow(['review','rating','neg', 'pos', 'neu','compound'])
i=0
for sentence in input.review:
    vs = analyzer.polarity_scores(str(sentence))
    csv_out.writerow([str(sentence),str(input.iloc[i].rating), str(vs['neg']), str(vs['pos']), str(vs['neu']), str(vs['compound'])])
    i=i+1
