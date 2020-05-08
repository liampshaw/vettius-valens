# Read in Z-codes
Z_codes = []
with open('../../data/Z-codes-0-to-2020.tsv', 'r') as f:
    for line in f.readlines():
        line = line.strip().split(',')
        Z_codes.append(line[1])

# Make vector of the bodies
order_of_bodies = ['Saturn',
                    'Jupiter',
                    'Mars',
                    'Sun',
                    'Venus',
                    'Mercury',
                    'Moon']
# Example triple
triple = ['Jupiter','Sun','Moon']

def countOccurrencesTriple(triple, some_Z_codes):
    """Counts how many times a triple occurs in a given set of Z-codes."""
    # Sort into correct body order
    triple_indices = sorted([order_of_bodies.index(x) for x in triple])
    # Find all Z-codes where this triple is satisfied
    # It will be where the set is of length one (three repeated characters)
    triple_lengths = [len(set([Z[x] for x in triple_indices])) for Z in some_Z_codes]
    # Count those of length one as a percentage
    perc = triple_lengths.count(1)/len(triple_lengths) * 100
    return(perc)

def countOccurrencesDouble(double, some_Z_codes):
    """Counts how many times a double occurs in a given set of Z-codes."""
    # Sort into correct body order
    double_indices = sorted([order_of_bodies.index(x) for x in double])
    # Find all Z-codes where this double is satisfied
    # It will be where the set is of length one (two repeated characters)
    double_lengths = [len(set([Z[x] for x in double_indices])) for Z in some_Z_codes]
    # Count those of length one as a percentage
    perc = double_lengths.count(1)/len(double_lengths) * 100
    return(perc)

# Work out all combinations of three
import itertools
triple_combinations = list()
for a, b, c in itertools.combinations(order_of_bodies, 3):
    triple = [a,b,c]
    triple_indices = sorted([order_of_bodies.index(x) for x in triple])
    triple_combinations.append([order_of_bodies[x] for x in triple_indices])

vettius_triple_combinations = list()
with open('../../data/vettius-triples.csv', 'r') as f:
    for line in f.readlines():
        triple = line.strip().split(',')
        triple_indices = sorted([order_of_bodies.index(x) for x in triple])
        vettius_triple_combinations.append([order_of_bodies[x] for x in triple_indices])

double_combinations = list()
for a, b in itertools.combinations(order_of_bodies, 2):
    double = [a,b]
    double_indices = sorted([order_of_bodies.index(x) for x in double])
    double_combinations.append([order_of_bodies[x] for x in double_indices])

# Use 50 year sliding window
# 50 years = 50 * 365 = 18250
#fifty_year_block = 18250
#all_triple_occurrences = []
#for i in range(0, 200):
#    print(i)
#    date_range = range((i-1)*365, fifty_year_block+i*365)
#    fifty_year_Z_codes = [Z_codes[d] for d in date_range]
#    triple_occurrences = [countOccurrencesTriple(v, fifty_year_Z_codes) for v in triple_combinations]
#    all_triple_occurrences.append(triple_occurrences)

#with open('0-CE-200-CE-occurrence-50-year-sliding-window.csv', 'w') as f:
#    for year, occs in enumerate(all_triple_occurrences):
#        for i, t in enumerate(occs):
#            f.write('%d,%s,%f\n' % (year, '-'.join(triple_combinations[i]), t))

# Calculate for every year
all_triple_occurrences = []
for i in range(1, 200):
    print(i)
    date_range = range((i-1)*365, i*365)
    year_Z_codes = [Z_codes[d] for d in date_range]
    triple_occurrences = [countOccurrencesTriple(v, year_Z_codes) for v in triple_combinations]
    all_triple_occurrences.append(triple_occurrences)

with open('../../data/0-CE-200-CE-triple-occurrence-per-year.csv', 'w') as f:
    for year, occs in enumerate(all_triple_occurrences):
        for i, t in enumerate(occs):
            f.write('%d,%s,%f\n' % (year, '-'.join(triple_combinations[i]), t))

# Doubles for every year
all_double_occurrences = []
for i in range(1, 200):
    print(i)
    date_range = range((i-1)*365, i*365)
    year_Z_codes = [Z_codes[d] for d in date_range]
    double_occurrences = [countOccurrencesDouble(v, year_Z_codes) for v in double_combinations]
    all_double_occurrences.append(double_occurrences)

with open('../../data/0-CE-200-CE-double-occurrence-per-year.csv', 'w') as f:
    for year, occs in enumerate(all_double_occurrences):
        for i, t in enumerate(occs):
            f.write('%d,%s,%f\n' % (year, '-'.join(double_combinations[i]), t))





for i, v in enumerate(vettius_triple_combinations):
    print(v[0], v[1], v[2], vettius_triple_occurrences[i])

# What about in his lifetime?
# February 8 120 - c.175
# 43840 since start until
# 63875
Z_codes_vettius_lifespan = Z_codes[43840:63875]
vettius_triple_occurrences_lifespan = [countOccurrencesTriple(v, Z_codes_vettius_lifespan) for v in vettius_triple_combinations]

# Seem to be pretty fixed over time
for z in zip(vettius_triple_occurrences,vettius_triple_occurrences_lifespan):
    print(z[1]-z[0])
# i.e. these numbers are around zero. So these are fairly constant happenstances in terms of which ones are more likely

# Do sentiment analysis of the triples in Vettius to see whether any correlation between frequency of occurrence and good/bad (and extremity)?
for z in zip(vettius_triple_combinations, vettius_triple_occurrences):
    print(z)
# Note that the most frequent triple is Sun, Venus, Mercury
# which occurs ~10% of the time. And the prediction seems to be exactly what a patron would like to hear (pp.21-22):
# Mercury, the sun, and Venus make polymaths and men of wide experience.
# These men are noble, prominent in the arts and sciences, worthy of trusts and positions.
# They easily regret what has been done, sometimes wavering and moved in all directions, or enjoying changes in their occupations.
# They have many friends, are well known, succeed through their acquaintance with the great, and are honored with a livelihood and high rank, despite being blameworthy.

# Look at them sorted by their frequency
for z in [vettius_triple_combinations[x] for x in sorted(range(len(vettius_triple_occurrences)), key=lambda k: vettius_triple_occurrences[k])]:
    print(z,vettius_triple_occurrences[vettius_triple_combinations.index(z)])
#N.B. These don't change if vettius_triple_occurrences_lifespan is used
# Second-most common is Mars, Sun, Mercury
# "Mars, the sun, and Mercury make men of much experience, inventive in business enterprises...For the most part, they have a livelihood subject to ups and downs." (uh huh...)
# Jupiter, Sun, Mercury:
# "Jupiter, Mercury, and the sun cause men to be easily successful in business enterprises and to have many friends, to be thought worthy of trusts, honors, stewardships, association with the great, and success."
# Sun, Mercury, Moon:
# "Mercury, the sun, and the moon make revered and pure men, those who play their part well, stewards, and those who share honors and positions. They are benefactors, participants in the mysteries, troubleshooters, and they put on a great show of possessions. They become bodyguards, chamberlains, and men placed in charge of money, records, and accounts. The speech of such men will be most effective for advice or instruction."

# Let's look at the other end of the scale - the least frequent. (although N.B. order is quite small, these are all not that frequent)
# Least frequent is Saturn, Jupiter, Moon. Mentions travelling abroad and becoming lords or shipowners (not very common)?
# Saturn, Jupiter, and the moon are in harmony, bringing rank and profit, associations with the great, and gifts. Men travel abroad; they succeed in foreign lands or because of foreigners, not only in their own business, but also in others’. Men also derive benefits from women, and coming into possession /42K/ of estates and land, they become lords. Some become shipowners and thus increase their livelihood, or they manage their livelihood by getting hold of whatever is involved with water.
# Then: Saturn, Mars, Moon
# Highly negative: "Saturn, Mars, and the moon cause men to be venturesome in their business enterprises and noble, but ineffective, meeting with reversals and violence."
# Jupiter, Sun, Moon
# Starts positive but ends very negative: "Jupiter, the sun, and the moon cause distinguished, brilliant, prominent men...They are adorned with all the pomp of wealth, but do not continue happy to the end; they falter in some things and ultimately come to grief."
# Jupiter, Mars, Moon
# Again, mentions exceptional things: "Jupiter, Mars, and the moon produce shrewd men, bold, public men with many friends, men advancing to high place from humble fortune and thought worthy of trust. These men are governors, athletes, distinguished men, leaders, supervisors of the masses and of districts"
# Saturn, Venus, Moon
# "Saturn, Venus, and the moon bring vicissitudes and instability of life, especially with respect to wife, mother, and children. They impose bad manners, ingratitude, as well as jealousy and quarrels, divorces, censure, public exposure, unnatural vices. But in business these men are not without resource, sharp, full of accomplishment, profiting from legacies. They do not however retain this wealth, since they are plotted against by many,"
# In general, a hypothesis would be: the more negative and specific the sentiment is, the less frequent the combination.
# This could be tested with sentiment analysis, either computational or by hand.

# Doubles
# Triples
# +others (quadruples etc.)


# Combinations not listed by Vettius
missing = [x for x in triple_combinations if x not in vettius_triple_combinations]

# How often do these occur?
[countOccurrencesTriple(m, Z_codes) for m in missing]
# I wonder why they're not listed? Nothing mark
