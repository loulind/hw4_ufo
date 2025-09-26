###### TASK 1: Shape Counts #######
# 1. Imports nuforc_sightings.csv from data folder
library(readr)
nuforc_sightings <- read_csv("work/data/nuforc_sightings.csv")
View(nuforc_sightings)

# 2. Filters only reports from USA
# 3. From US-filtered data, gets count per UFO sighting shape
# 4. Cleans and standardizes the shape column
# 5. Constructs Matrix: rows = states, cols = shape category (sorted a->z)
state_by_shape_cnts <- matrix()
# 6. Reports number of distinct shapes and state with most "Circle" shapes



###### TASK 2: PCA on Shapes #######
# 1. Row-normalizes count matrix: proportions of sighting shape per state
state_by_shape_norm <- matrix()
# 2. Scree plot for principle components
# 3. Scatter plot of first 2 PCs (each point is a state)
# 4. Examines first 2 cols of PCA rotation (shapes contributing most to PC1,2)



###### TASK 3: Tokenize Summaries #######
# 1. Converts everything to lowercase
# 2. Uses reg exprssn to remove non-ASCII characters
# 3. Trims white edges + reg exprssn to replace repeated space with single SPC
# 4. Breaks up summaries into an array of words
# 5. Creates histogram of most freq words across dataset
# 6. (OPTIONAL) Word cloud
# 7. Uses stopwords to remove stopwords from tokens, and remake histogram



###### TASK 4: PCA on Summaries #######
# 1. Defines 'vocabulary' of of top 100 words (≥3 letters)
# 2. Creates wide count table with row = state, col = word from vocabulary
# 3. Row-normalizes count matrix: proportions of word usage per state
state_by_word_norm <- matrix()
# 4. Scree plot for principle components
# 5. Scatter plot of first 2 PCs (each point is a state)
# 6. Examines first 2 cols of PCA rotation (shapes contributing most to PC1,2)