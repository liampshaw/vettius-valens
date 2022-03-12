# Analyses the number of aspects in Z-codes:
# conjunction (0), opposition (6), trine (4), quartile (3), sextile (2)
import itertools
import pandas as pd
import random
from math import log

ASPECTS = [0, 6, 4, 3, 2] # defined aspects in preferred order

# Read in Z-codes
Z_codes = []
with open('../../data/Z-codes-1-200-CE.tsv', 'r') as f:
    for line in f.readlines():
        line = line.strip().split(',')
        Z_codes.append(line[1])

# Convert A/B to 10/100
def convertHex(n):
    if n=='A':
        return(10)
    if n=='B':
        return(11)
    else:
        return(int(n))

# Function to count aspects in a Z-code
def Aspect(a, b):
    '''Returns numerical aspect of signs.'''
    a = convertHex(a)
    b = convertHex(b)
    diff = abs(a-b)
    return(min(diff, 12-diff)) # think about it as a clock-face

def countAspects(Z_code):
    '''Counts the aspectual relations in a Z_code.'''
    aspects = []
    for a, b in itertools.combinations(Z_code, 2):
        aspects.append(Aspect(a,b))
    return [aspects.count(i) for i in ASPECTS]

aspect_list = []
for i, Z in enumerate(Z_codes):
    print(i/len(Z_codes) * 100)
    aspect_list.append(countAspects(Z))

df = pd.DataFrame(aspect_list, columns=['conjunction', 'opposition', 'trine', 'quartile', 'sextile'])
df.to_csv('aspects.csv', index=False)

# Compute it for random Z-codes
random_aspect_list = []
for i in range(len(aspect_list)):
    random_aspect_list.append(countAspects(random.choices(list('0123456789AB'), k=7)))
random_df = pd.DataFrame(random_aspect_list, columns=['conjunction', 'opposition', 'trine', 'quartile', 'sextile'])
random_df.to_csv('random_aspects.csv', index=False)

# Also compute for each individual planet
def countAspectsPlanet(Z_code, planet):
    '''Counts the aspectual relations in a Z_code that involve a particular planet. Planet is defined by position wiith conventional ordering.'''
    aspects = []
    a = Z_code[planet]
    for b in [Z_code[i] for i in range(len(Z_code)) if i!=planet]:
          aspects.append(Aspect(a, b))
    return(aspects)

def Shannon(a_vector):
    '''Returns Shannon index of a vector of numbers.'''
    # remove zero entries
    new_vector = [x/sum(a_vector) for x in a_vector if x!=0]
    # normalise so entries sum to 1
    return sum([-p*log(p) for p in new_vector])

def removeBoring(aspect):
    '''Removes the boring integers from a set of aspects.'''

def countAspectualDiversity(aspect_set):
    '''Counts the diversity of aspects involved in a set of aspects.'''
    # An aspect set is a set of vectors that contain numbers from 0-6. Want to ask how diverse this vector is.'''
    # We only count 'true' aspects in the aspect vector
    aspect_vectors = [[x[i] for x in aspect_set] for i in range(6)]
    # Now remove the boring ones and create proportion of interesting ones
    proportions = [len([x for x in v if x in ASPECTS])/len(v) for v in aspect_vectors] 
    # and also get the diversity
    aspect_vectors_interesting = [[x for x in v if x in ASPECTS] for v in aspect_vectors]
    #print(aspect_vectors_interesting)	
    # Make the table for each
    aspect_vectors_table = [[v.count(i) for i in ASPECTS] for v in aspect_vectors]
    print([x for x in aspect_vectors_table]) # this is the sum of interesting aspects in the whole set for each planet
    #return [Shannon(table) for table in aspect_vectors_table]

    return [Shannon(aspect_vector) for aspect_vector in aspect_vectors_interesting]


aspects_by_planet = {p : [] for p in range(7)}
for i, Z in enumerate(Z_codes):
    print(i/len(Z_codes) * 100)
    for planet in range(7):
       aspects_by_planet[planet].append(countAspectsPlanet(Z, planet))

# In a given time period, is Mercury in more diverse aspects than other planets?

