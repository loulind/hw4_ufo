library(tidyverse)
library(stopwords)
pdf("~/work/output/hw4_output.pdf", width = 7, height = 5)
plot.new()
text(.5, .5, font=2, cex=1.5, "Lou's HW 4: UFO sightings analysis and plots")




###### TASK 1: Shape Counts #######
plot.new()
text(.5, .5, font=2, cex=1.5, "----- TASK 1: Building shape table -----")

# 1. Imports nuforc_sightings.csv from data folder
nuforc_sightings <- read_csv("work/data/nuforc_sightings.csv")

# 2. Filters only reports from USA (Added dplyr:: because of conflict)
df_filter_usa <- nuforc_sightings %>% 
  dplyr::filter(
    (country == "USA") & (state != "-") & (state != "Corrientes") &
    (state != "ENG") & (state != "England") & (state != "GU") & 
    (state != "GU") & (state != "VI") & (state != "UM") & (state != "PR")
    )

df_sort_state <- df_filter_usa %>% arrange(state)
df_fix_state_names <- df_sort_state %>%  # fixing incorrect state abbrv
  mutate(state = case_when(
    state == "0" ~ "WI",
    state == "Ca" ~ "CA",
    state == "Fl" ~ "FL",
    state == "Ca" ~ "CA",
    state == "Montana" ~ "MT",
    state == "New York" ~ "NY",
    state == "NB" ~ "NE",
    state == "Ohio" ~ "OH",
    state == "West Virginia" ~ "WV",
    state == "Wisconsin" ~ "WI",
    TRUE ~ state
  ))
df_fix_shape_names <- df_fix_state_names %>%  # fixing incorrect shape names
  mutate(shape = case_when(
    shape == "changing" ~ "Changing",
    shape == "cigar" ~ "Cigar",
    shape == "circle" ~ "Circle",
    shape == "cylinder" ~ "Cylinder",
    shape == "diamond" ~ "Diamond",
    shape == "egg" ~ "Egg",
    shape == "fireball" ~ "Fireball",
    shape == "flash" ~ "Flash",
    shape == "light" ~ "Light",
    shape == "other" ~ "Other",
    shape == "oval" ~ "Oval",
    shape == "rectangle" ~ "Rectangle",
    shape == "sphere" ~ "Sphere",
    shape == "triangle" ~ "Triangle",
    shape == "unknown" ~ "Unknown",
    TRUE ~ shape
  ))

# 3. Cleans and standardizes the shape column
df_rm_noshape <- df_fix_shape_names %>%  # drops missing/unknown shapes
  dplyr::filter(
    !(is.na(shape)) & (shape != "Other") & (shape != "Unknown")
    )

# 4. From US-filtered data, gets count for each shape per state
df_shape_cnts <- df_rm_noshape %>%
  group_by(state, shape) %>%
  summarise(count = n(), .groups = 'drop') %>%  # count summary table
  pivot_wider(names_from = shape,  # transform to wide df
              values_from = count, 
              values_fill = 0
              )

# 5. Constructs Matrix: rows = states, cols = shape category (sorted a->z)
mat_shape_cnts <- df_shape_cnts %>% 
  column_to_rownames("state") %>%  # changes to row name
  as.matrix()  # converts to matrix of counts

# 6. Outputs answers to questions to pdf in output folder
count(count(df_fix_shape_names, shape)) - 3  # gives number of unique shapes
# state with most 'Circle' sightings is CA
plot.new()
text(.05, 0.5, font=2, cex=0.8, adj=0, 
"Question 1:
     How many different shapes are in the dataset?\n
Answer 1:
     There are 22 unique (non-'Other' or 'Unknown') variables in the dataset\n\n
Question 2:
     Which state has the most sightings of the 'Circle' shape?\n
Answer 2:
     California (CA) at 1636 sightings"
)




###### TASK 2: PCA on Shapes #######
plot.new()
text(.5, .5, font=2, cex=1.5, "----- TASK 2: PCA on Shapes -----")

# 1. Row-normalizes count matrix and computes PCA residuals
mat_shape_norm <- mat_shape_cnts / rowSums(mat_shape_cnts)
pca_res <- mat_shape_norm %>% 
            prcomp(center = TRUE, scale. = TRUE)  # divides each col by it's SD

# 2. Variance explained and Scree plot for principle components
plot.new()
text(.05, 0.5, font=2, cex=0.8, adj=0, 
"Question 1:
     Can the UFO sightings be summarized by a few key patterns?\n
Answer 1:
     I was surprised to see that the var explained by PC1 is not that
     much greater than PC2. The data in their current state do not
     appear to be effectively represented in fewer dimensions\n
     See next page for scree plot of PCs." 
)

var_explained <- (pca_res$sdev^2) / sum(pca_res$sdev^2)
var_expl_tbl <- tibble(
  PC = seq_along(var_explained),
  cum_var = cumsum(var_explained)
)

ggplot(var_expl_tbl, aes(x = PC, y = var_explained)) +
  geom_col(fill = "steelblue") +
  geom_line(aes(group = 1)) +
  geom_point() +
  ylab("Proportion of Variance Explained") +
  xlab("Principal Component") +
  ggtitle("Variance Explained by PCs of UFO Sightings Dataset (shape type)")

# 3. Scatter plot of first 2 PCs (each point is a state)
plot.new()
text(.05, 0.5, font=2, cex=0.8, adj=0, 
"Question 2:
     Scatterplot of first two PCs: Any regional clusters or outliers?\n
Answer 2:
     There do not appear to be regional clusters in the scatterplot. However,
     there does appear to be an outlier on the leftmost side of the graph.\n
     See next page for scatterplot of PC1 and PC2" 
)

pc_scores <- as_tibble(pca_res$x[, 1:2], .name_repair = "minimal") %>%
  rename(PC1 = 1, PC2 = 2)

ve1 <- scales::percent(var_explained[1])
ve2 <- scales::percent(var_explained[2])

ggplot(pc_scores, aes(x = PC1, y = PC2)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "PCA Scores Scatterplot (Points = States)",
    x = paste0("PC1 (", ve1, ")"),
    y = paste0("PC2 (", ve2, ")")
  ) +
  theme_minimal()

# 4. Examines first 2 cols of PCA rotation (shapes contributing most to PC1,2)
plot.new()
text(.05, 0.5, font=2, cex=0.8, adj=0, 
"Question 3:
     Which shapes contribute most to first two PCs?\n
Answer 3:
     Top 3 shapes for each PC in desc order (abs value in parentheses)
     PC1: Light (0.40), Diamond (0.32), Triangle (0.32)
     PC2: Chevron (0.38), Fireball (0.34), Orb (0.31)"
)

pc_1and2_rotation <- as_tibble(pca_res$rotation[, 1:2], rownames = "Shapes")

top_shapes_pc1 <- pc_1and2_rotation %>%
  select(-PC2) %>%  # removes pc 2
  arrange(desc(abs(PC1))) %>%  # orders PC1 contributions
  slice_head(n = 10)  # shows only top 10 contributing shapes

top_shapes_pc2 <- pc_1and2_rotation %>% 
  select(-PC1) %>%  # removes pc 1
  arrange(desc(abs(PC2))) %>%  # orders PC1 contributions
  slice_head(n = 10)  # shows only top 10 contributing shapes

view(top_shapes_pc1)
view(top_shapes_pc2)




###### TASK 3: Tokenize Summaries #######
plot.new()
text(.5, .5, font=2, cex=1.5, "----- TASK 3: Tokenize Summaries -----")

# 1-3. Lowercase, remove non-ASCII, remove edge spaces, and collapse whitespaces
df_clean_summaries <- df_fix_shape_names %>%
  mutate(summary_clean = summary %>%
    str_to_lower() %>%  # lowercase
    str_replace_all("[^\\x01-\\x7F]", "") %>%  # remove non-ASCII
    str_trim() %>%  # trim whitespace
    str_replace_all("\\s+", " ")  # collapse repeated whitespace
  )

# 4. Breaks up summaries into arrays of words
ufo_tokens <- df_clean_summaries %>%
  mutate(tokens = str_split(summary_clean, " "))

ufo_words <- ufo_tokens %>%
  select(state, tokens) %>%  # removes all vars except state and tokens
  unnest(tokens) %>%  # expands the arrays in the tokens col to separate cols
  filter(tokens != "")  # removes empty tokens

# 5. Creates histogram of most freq words across dataset
plot.new()
text(.05, 0.5, font=2, cex=0.8, adj=0, 
"Question 1:
     What do you observe in this initial output?\n
Answer 1:
     The top words appear to be predominantly generic english words\n
     See next pages for histogram and word cloud"
)

raw_word_freq <- ufo_words %>%
  count(tokens, sort = TRUE)  # gives word frequencies before stopwords removed
raw_word_freq %>%  # histogram of words
  slice_max(n, n = 20) %>%  # retreives the top 20 words most used words
  ggplot(aes(x = reorder(tokens, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 20 Words (Before Stopword Removal)",
       x = "Word", y = "Count")

# 6. Uses stopwords to remove stopwords from tokens, and remake histogram
plot.new()
text(.05, 0.5, font=2, cex=0.8, adj=0, 
"Question 2:
     After stopword removal, which words feel most characteristic 
     of these reports?\n
Answer 2:
     After removing stopwords, the histogram appears to reflect words that
     better represent the summaries--words like...
     'light, bright, sky, moving, white,' etc\n
     See page below for histogram of word freqs without stopwords"
)

stop_words <- stopwords("en")

ufo_words_nostop <- ufo_words %>%
  filter(!(tokens %in% stop_words))  # filters out if a token is in stopwords

nostop_word_freq <- ufo_words_nostop %>%
  count(tokens, sort = TRUE)

nostop_word_freq %>%  # histogram with stopwords removed
  slice_max(n, n = 20) %>%
  ggplot(aes(x = reorder(tokens, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 20 Words (After Stopword Removal)",
       x = "Word", y = "Count")




###### TASK 4: PCA on Summaries #######
plot.new()
text(.5, .5, font=2, cex=1.5, "----- TASK 4: PCA on Summaries -----")

# 1. Defines 'vocabulary' of of top 100 words (≥3 letters)
vocab <- nostop_word_freq %>%
  filter(str_length(tokens) >= 3) %>%  # filters any words less than 3 chrs
  slice_max(n, n = 100) %>%  # gives top 100 words
  pull(tokens)  # extracts the tokens column
view(vocab)

# 2. Creates wide count table with row = state, col = words 
df_word_cnts <- ufo_words_nostop %>%
  filter(tokens %in% vocab) %>%  # keeps only tokens that are in vocab
  count(state, tokens) %>%  # gives counts of vocab words in each state
  pivot_wider(names_from = tokens, values_from = n, values_fill = 0)

# 3. Converts to matrix and row-normalizes
mat_word_cnts <- df_word_cnts %>% 
  column_to_rownames("state") %>%  # changes to row name
  as.matrix()  # converts to matrix of counts

mat_word_norm <- mat_word_cnts / rowSums(mat_word_cnts)
pca_res2 <- mat_word_norm %>% 
  prcomp(center = TRUE, scale. = TRUE)  # divides each col by it's SD

# 4. Scree plot for principle components
plot.new()
text(.05, 0.5, font=2, cex=0.8, adj=0, 
"Question 1:
     Compare this analysis with the shape-based PCA from Task 2\n
Answer 1:
     Both the summary and shape analyses appear to show a strong usage of 
     language related to light (eg, 'bright, light, fire' etc).
     Much like the PCA of the shape data, the PCA of the summary data 
     also seems to be all clustered around a singular centroid with a 
     distinct oulier. However the PCA of summary has a few more outliers
     and appears to be more tightly backed around it's main centroid\n
     See below for scree and scatter plots for the summary data"
)

var_explained2 <- (pca_res2$sdev^2) / sum(pca_res2$sdev^2)
var_expl_tbl2 <- tibble(
  PC = seq_along(var_explained2),
  cum_var = cumsum(var_explained2)
)

ggplot(var_expl_tbl2, aes(x = PC, y = var_explained2)) +
  geom_col(fill = "steelblue") +
  geom_line(aes(group = 1)) +
  geom_point() +
  ylab("Proportion of Variance Explained") +
  xlab("Principal Component") +
  ggtitle("Variance Explained by PCs of UFO Sightings Dataset (word usage)")

# 5. Scatter plot of first 2 PCs (each point is a state)
pc_scores2 <- as_tibble(pca_res2$x[, 1:2], .name_repair = "minimal") %>%
  rename(PC1 = 1, PC2 = 2)

ve1_word <- scales::percent(var_explained2[1])  # PC1 for word freq df
ve2_word <- scales::percent(var_explained2[2])  # PC2 for word freq df

ggplot(pc_scores2, aes(x = PC1, y = PC2)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "PCA Scores Scatterplot (Points = States)",
    x = paste0("PC1 (", ve1_word, ")"),
    y = paste0("PC2 (", ve2_word, ")")
  ) +
  theme_minimal()

# 6. Examines first 2 cols of PCA rotation (shapes contributing most to PC1,2)
pc_1and2_rotation2 <- as_tibble(pca_res2$rotation[, 1:2], rownames = "Words")

top_words_pc1 <- pc_1and2_rotation2 %>%
  select(-PC2) %>%  # removes pc 2
  arrange(desc(abs(PC1))) %>%  # orders PC1 contributions
  slice_head(n = 10)  # shows only top 10 contributing shapes

top_words_pc2 <- pc_1and2_rotation2 %>% 
  select(-PC1) %>%  # removes pc 1
  arrange(desc(abs(PC2))) %>%  # orders PC1 contributions
  slice_head(n = 10)  # shows only top 10 contributing shapes

view(top_words_pc1)
view(top_words_pc2)

dev.off()  # turns off PDF output device