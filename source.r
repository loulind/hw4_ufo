library(tidyverse)

###### TASK 1: Shape Counts #######
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

# 6. Reports number of distinct shapes and state with most "Circle" shapes
count(count(df_fix_shape_names, shape)) - 3  # prints number of unique shapes
view(mat_shape_cnts)  # the state with most Circle sightings is CA!




###### TASK 2: PCA on Shapes #######
# 1. Row-normalizes count matrix and computes PCA residuals
mat_shape_norm <- mat_shape_cnts / rowSums(mat_shape_cnts)
pca_res <- mat_shape_norm %>% 
            prcomp(center = TRUE, scale. = TRUE)  # divides each col by it's SD

# 2. Variance explained and Scree plot for principle components
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
  ggtitle("Scree Plot")

# 3. Scatter plot of first 2 PCs (each point is a state)
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
# 1. Converts everything to lowercase
# 2. Uses reg exprssn to remove non-ASCII characters
# 3. Trims white edges + reg exprssn to replace repeated space with single SPC
# 4. Breaks up summaries into an array of words
# 5. Creates histogram of most freq words across dataset
# 6. (OPTIONAL) Word cloud
# 7. Uses stopwords to remove stopwords from tokens, and remake histogram
library(stopwords)




###### TASK 4: PCA on Summaries #######
# 1. Defines 'vocabulary' of of top 100 words (≥3 letters)
# 2. Creates wide count table with row = state, col = word from vocabulary
# 3. Row-normalizes count matrix: proportions of word usage per state
state_by_word_norm <- matrix()
# 4. Scree plot for principle components
# 5. Scatter plot of first 2 PCs (each point is a state)
# 6. Examines first 2 cols of PCA rotation (shapes contributing most to PC1,2)