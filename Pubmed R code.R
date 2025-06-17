# Install required packages
install.packages(c("readr", "dplyr", "ggplot2", "stringr", "tidyr", 
                   "wordcloud", "RColorBrewer", "openxlsx", "igraph", "ggraph","tidytext","stopwords"))

# Load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(wordcloud)
library(RColorBrewer)
library(openxlsx)
library(igraph)
library(ggraph)
library(tidytext)
library(stopwords)
library(tidyverse)



# Load data
pubmed_data <- read_csv("C:/Users/user/Downloads/csv-Covid19AND-set.csv")

# Clean and select key columns
pubmed_clean <- pubmed_data %>%
  select(
    Title = Title,
    Authors = Authors,
    FirstAuthor = `First Author`,
    Citation = Citation,
    Journal = `Journal/Book`,
    Year = `Publication Year`
  ) %>%
  mutate(
    Year = as.character(Year)
  )

# -------------------------------
# 1. Top 10 Journals
# -------------------------------
top_journals <- pubmed_clean %>%
  count(Journal, sort = TRUE) %>% top_n(10, n)

ggplot(top_journals, aes(x = reorder(Journal, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Journals", x = "Journal", y = "Publications")

# -------------------------------
# 2. Publication Trend by Year
# -------------------------------
pubs_per_year <- pubmed_clean %>% count(Year) %>% filter(!is.na(Year))

ggplot(pubs_per_year, aes(x = as.numeric(Year), y = n)) +
  geom_line(group = 1, color = "darkgreen") +
  geom_point(color = "darkgreen") +
  labs(title = "Publication Trend Over Time", x = "Year", y = "Publications")

# -------------------------------
# 3. Top 10 Authors (any position)
# -------------------------------
author_counts <- pubmed_clean %>%
  separate_rows(Authors, sep = ",|;|\\band\\b") %>%
  mutate(Authors = str_trim(Authors)) %>%
  filter(Authors != "") %>%
  count(Authors, sort = TRUE) %>% top_n(10, n)

ggplot(author_counts, aes(x = reorder(Authors, n), y = n)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(title = "Top 10 Authors", x = "Author", y = "Publications")

# -------------------------------
# 4. Top 10 First Authors
# -------------------------------
top_first <- pubmed_clean %>%
  count(FirstAuthor, sort = TRUE) %>% top_n(3, n)

ggplot(top_first, aes(x = reorder(FirstAuthor, n), y = n)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(title = "Top 10 First Authors", x = "First Author", y = "Publications")

# -------------------------------
# 5. Top Cited Journals (from Citation)
# -------------------------------
top_cited_journals <- pubmed_clean %>%
  filter(!is.na(Citation)) %>%
  mutate(CitedJournal = word(Citation, -1)) %>%
  count(CitedJournal, sort = TRUE) %>%
  filter(!is.na(CitedJournal)) %>% top_n(10, n)

ggplot(top_cited_journals, aes(x = reorder(CitedJournal, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() +
  labs(title = "Most Cited Journals (approx.)", x = "Cited Journal", y = "Frequency")

# -------------------------------
# 6. Keyword Frequency (from Titles)
# -------------------------------
keywords <- pubmed_clean %>%
  select(Title) %>%
  unnest_tokens(word, Title) %>%
  filter(!word %in% stopwords("en"),
         str_detect(word, "^[a-zA-Z]+$")) %>%
  count(word, sort = TRUE) %>%
  filter(n > 3)

# 📊 Visualize top 20 keywords
ggplot(keywords[1:20, ], aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  coord_flip() +
  labs(title = "Top Keywords from Titles", x = "Word", y = "Frequency")
# -------------------------------
# 7. Word Cloud
# -------------------------------
wordcloud(words = keywords$word, freq = keywords$n, min.freq = 3,
          max.words = 100, colors = brewer.pal(8, "Set3"), random.order = FALSE)

# -------------------------------
# 8. Co-Authorship Network (simplified)
# -------------------------------
# 👥 Co-authorship edge list
edges <- pubmed_clean %>%
  separate_rows(Authors, sep = ",|;|\\band\\b") %>%
  mutate(Authors = str_trim(Authors)) %>%
  group_by(Title) %>%
  filter(n_distinct(Authors) > 1) %>%  # ✅ Skip single-author papers
  summarise(pairs = list(combn(unique(Authors), 2, simplify = FALSE))) %>%
  unnest(pairs) %>%
  mutate(
    from = map_chr(pairs, 1),
    to = map_chr(pairs, 2)
  ) %>%
  select(from, to) %>%
  filter(from != to)

# 📈 Build the graph
g <- graph_from_data_frame(edges, directed = FALSE)
g <- simplify(g)

# 🧠 Plot the co-authorship network
# Compute degree (number of connections per node)
V(g)$degree <- degree(g)

# Filter graph to only keep high-degree authors
g_sub <- induced_subgraph(g, vids = V(g)[degree >= 2])

# Plot simplified network
ggraph(g_sub, layout = "fr") +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(size = 3, color = "steelblue") +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_void() +
  labs(title = "Co-authorship Network (Degree ≥ 2)")


p
# Plot the co-authorship network
ggraph(g, layout = "fr") +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(size = 3, color = "skyblue") +
  geom_node_text(aes(label = name), repel = TRUE, size = 2.5) +
  theme_void() +
  labs(title = "Co-authorship Network")

# -------------------------------
# 9. Export Summary to Excel
# -------------------------------
summary_tables <- list(
  "Top Journals" = top_journals,
  "Top Authors" = author_counts,
  "Top First Authors" = top_first,
  "Top Keywords" = keywords
)

write.xlsx(summary_tables, "PubMed_Bibliometric_Summary.xlsx", rowNames = FALSE)
