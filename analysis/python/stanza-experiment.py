# Experimenting with using stanza NLP package to automatically assign sentiment

import stanza # NLP package
import glob
import re
nlp = stanza.Pipeline(lang='en', processors='tokenize,sentiment')

# Read in English translation
files = glob.glob('/Users/Liam/Dropbox/Personal/Claire/Vettius Valens/Texts for analysis/Riley English Translation (2010)/*.txt')
output_file = '../intermediate-files/stanza-sentiments.csv'

with open(output_file, 'w') as output_f:
	for fname in files:
		combo = re.sub('.txt', '', re.sub('-', ' ', re.sub('.*\/', '', fname)))
		with open(fname, 'r') as f:
			text = f.readlines()[0]
			doc = nlp(text)
			sentiments = []
			for i, sentence in enumerate(doc.sentences):
				sentiments.append(sentence.sentiment-1) # sentiment in stanza is 0,1,2 negative,neutral,positive so we want -1,0,1
			output_f.write('%s\n' % (combo+','+str(len(sentiments))+','+str(sum(sentiments)/len(sentiments))))
