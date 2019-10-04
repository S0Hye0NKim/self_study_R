Bag of Words
================
Sohyeon Kim
2019 9 27

``` r
library(tidyverse)
library(rtweet)
library(tidytext)
library(tm)
library(qdap)
library(ggplot2)
library(rebus)
library(ggthemes)
```

# Data

``` r
coffee_tweets <- search_tweets(q = "#coffee", n = 10000,
                                      lang = "en",
                                      include_rts = FALSE)
saveRDS(coffee_tweets, "coffee_tweets.rds")
```

``` r
coffee_tweets <- readRDS("coffee_tweets.rds")
coffee_text <- coffee_tweets$text
wine_tweets <- readRDS("wine_tweets.rds")
wine_text <- wine_tweets$text
```

# Make Corpus

``` r
coffee_source <- VectorSource(coffee_text)
class(coffee_source)
```

    ## [1] "VectorSource" "SimpleSource" "Source"

``` r
# Make a volatile corpus from coffee_corpus
coffee_corpus <- VCorpus(coffee_source)

# Print out coffee_corpus
coffee_corpus
```

    ## <<VCorpus>>
    ## Metadata:  corpus specific: 0, document level (indexed): 0
    ## Content:  documents: 9991

``` r
# Print the 15th tweet in coffee_corpus
coffee_corpus[[15]]
```

    ## <<PlainTextDocument>>
    ## Metadata:  7
    ## Content:  chars: 127

``` r
# Print the contents of the 15th tweet in coffee_corpus
coffee_corpus[[15]]$content
```

    ## [1] "If you don't drink #coffee, you are missing out on the Elixir of the Gods...\n\n#fitness #fitfam #health\n\nhttps://t.co/1GPhqlsUG0"

``` r
# Now use content to review plain text of the 10th tweet
content(coffee_corpus[[15]])
```

    ## [1] "If you don't drink #coffee, you are missing out on the Elixir of the Gods...\n\n#fitness #fitfam #health\n\nhttps://t.co/1GPhqlsUG0"

# Some useful function

``` r
# Create the object: text
text <- "<b>She</b> woke up at       6 A.M. It\'s so early!  She was only 10% awake and began drinking coffee in front of her computer."
```

``` r
# Make lowercase
tolower(text) %>% print()
```

    ## [1] "<b>she</b> woke up at       6 a.m. it's so early!  she was only 10% awake and began drinking coffee in front of her computer."

``` r
# Remove punctuation
tm::removePunctuation(text)
```

    ## [1] "bSheb woke up at       6 AM Its so early  She was only 10 awake and began drinking coffee in front of her computer"

``` r
# Remove numbers
removeNumbers(text)
```

    ## [1] "<b>She</b> woke up at        A.M. It's so early!  She was only % awake and began drinking coffee in front of her computer."

``` r
# Remove whitespace
stripWhitespace(text)
```

    ## [1] "<b>She</b> woke up at 6 A.M. It's so early! She was only 10% awake and began drinking coffee in front of her computer."

``` r
# Remove text within brackets
qdap::bracketX(text)
```

    ## [1] "She woke up at 6 A.M. It's so early! She was only 10% awake and began drinking coffee in front of her computer."

``` r
# Replace numbers with words
replace_number(text)
```

    ## [1] "<b>She</b> woke up at six A.M. It's so early! She was only ten% awake and began drinking coffee in front of her computer."

``` r
# Replace abbreviations
replace_abbreviation(text)
```

    ## [1] "<b>She</b> woke up at 6 AM It's so early! She was only 10% awake and began drinking coffee in front of her computer."

``` r
# Replace contractions
replace_contraction(text)
```

    ## [1] "<b>She</b> woke up at 6 A.M. it is so early! She was only 10% awake and began drinking coffee in front of her computer."

``` r
# Replace symbols with words
replace_symbol(text)
```

    ## [1] "<b>She</b> woke up at 6 A.M. It's so early! She was only 10 percent awake and began drinking coffee in front of her computer."

``` r
# Print text without standard stop words
removeWords(text, stopwords("en"))  #stopwords("en") is a standard English stop words
```

    ## [1] "<b>She</b> woke         6 A.M. It's  early!  She   10% awake  began drinking coffee  front   computer."

``` r
# Add "coffee" and "bean" to the list: new_stops
new_stops <- c("coffee", "bean", stopwords("en"))

# Remove stop words from text
removeWords(text, new_stops)
```

    ## [1] "<b>She</b> woke         6 A.M. It's  early!  She   10% awake  began drinking   front   computer."

``` r
complicate <- c("complicated", "complication", "complicatedly")

# Perform word stemming: stem_doc
stem_doc <- stemDocument(complicate)

# Create the completion dictionary: comp_dict
comp_dict <- "complicate"

# Perform stem completion: complete_text 
complete_text <- stemCompletion(stem_doc, comp_dict)

# Print complete_text
complete_text
```

    ##      complic      complic      complic 
    ## "complicate" "complicate" "complicate"

``` r
text <- "In a complicated haste, Tom rushed to fix a new complication, too complicatedly."
comp_dict <- c("In", "a", "complicate", "haste", "Tom", "rush", "to", "fix", "new", "too")

# Remove punctuation: rm_punc
rm_punc <- removePunctuation(text)

# Create character vector: n_char_vec
n_char_vec <- unlist(strsplit(rm_punc, split = " "))

# Perform word stemming: stem_doc
stem_doc <- stemDocument(n_char_vec)

# Print stem_doc
stem_doc
```

    ##  [1] "In"      "a"       "complic" "hast"    "Tom"     "rush"    "to"     
    ##  [8] "fix"     "a"       "new"     "complic" "too"     "complic"

``` r
# Re-complete stemmed document: complete_doc
complete_doc <- stemCompletion(stem_doc, comp_dict)

# Print complete_doc
complete_doc
```

    ##           In            a      complic         hast          Tom 
    ##         "In"          "a" "complicate"      "haste"        "Tom" 
    ##         rush           to          fix            a          new 
    ##       "rush"         "to"        "fix"          "a"        "new" 
    ##      complic          too      complic 
    ## "complicate"        "too" "complicate"

# Cleaning Corpus

``` r
# Alter the function code to match the instructions
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, words = c(stopwords("en"), "coffee", "httpstco" %R% one_or_more(WRD)))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Apply your customized function to the tweet_corp: clean_corp
clean_corp <- clean_corpus(coffee_corpus)

# Print out a cleaned up tweet
content(clean_corp[[100]])
```

    ## [1] "black egg mushroom muffin currentaffairs little sign advising people mental age lunch eatlessmeat costacoffee simplythebest theweek "

``` r
# Print out the same tweet in original form
coffee_text[100]
```

    ## [1] "Black #coffee, egg and mushroom muffin, #currentaffairs and a little sign advising people of my mental age.\n\n#lunch #eatlessmeat #CostaCoffee #SimplytheBest #TheWeek https://t.co/hnzDUxRiIe"

# Document-Term Matrix and Term-Document Matrix

``` r
# Create the document-term matrix from the corpus
coffee_dtm <- DocumentTermMatrix(clean_corp) %>% as.matrix

# Review a portion of the matrix to get some Starbucks
coffee_dtm[1550:1560, c("star", "starbucks")]
```

    ##       Terms
    ## Docs   star starbucks
    ##   1550    0         1
    ##   1551    0         2
    ##   1552    0         0
    ##   1553    0         0
    ##   1554    0         1
    ##   1555    0         0
    ##   1556    0         1
    ##   1557    0         0
    ##   1558    0         1
    ##   1559    1         1
    ##   1560    0         1

``` r
# Create a term-document matrix from the corpus
coffee_tdm <- TermDocumentMatrix(clean_corp) 
coffee_m <- coffee_tdm %>% as.matrix

# Review a portion of the matrix
coffee_m[c("star", "starbucks"), 1550:1560]
```

    ##            Docs
    ## Terms       1550 1551 1552 1553 1554 1555 1556 1557 1558 1559 1560
    ##   star         0    0    0    0    0    0    0    0    0    1    0
    ##   starbucks    1    2    0    0    1    0    1    0    1    1    1

# Top 10 Words

``` r
# Calculate the row sums of coffee_m
term_frequency <- rowSums(coffee_m)

# Sort term_frequency in decreasing order
term_frequency <- sort(term_frequency, decreasing = TRUE)

# View the top 10 most common words
term_frequency[1:10]

# Plot a barchart of the 10 most common words
barplot(term_frequency[1:10], col = "tan", las = 2)
```

![barplot1](https://user-images.githubusercontent.com/44796982/65834122-c5441280-e312-11e9-8f9b-5f2955bcad68.png)

``` r
clean_text <- clean_corp %>% lapply(FUN = content) %>%
  unlist()

# Create frequency
frequency <- freq_terms(
  clean_text, 
  top = 10,    # limit to the top 10 terms
  at.least = 3,  # at least 3 letters per term
  stopwords = "Top200Words" 
)

# Make a frequency barchart
plot(frequency)
```

![plot2](https://user-images.githubusercontent.com/44796982/65834124-d2f99800-e312-11e9-8b10-5450cf348e38.png)

# Word Cloud

``` r
# Load wordcloud package
library(wordcloud)

# Vector of terms
terms_vec <- names(term_frequency)

# Create a wordcloud for the values in word_freqs
set.seed(10)
wordcloud(terms_vec, term_frequency, max.words = 50, colors = c("grey80", "darkgoldenrod1", "tomato"))
```

![plot3](https://user-images.githubusercontent.com/44796982/65834127-dd1b9680-e312-11e9-81fd-55b88181bd0a.png)

# Commonality / Comparison cloud

``` r
coffee_text_clean <- coffee_text %>%
  removePunctuation() %>%
  tolower() %>%
  removeWords(words = c(stopwords("en"), "coffee", "wine", "httpstco" %R% one_or_more(WRD), "amp")) %>%
  stripWhitespace()

wine_text_clean <- wine_text %>%
  removePunctuation() %>%
  tolower() %>%
  removeWords(words = c(stopwords("en"), "coffee", "wine", "httpstco" %R% one_or_more(WRD), "amp")) %>%
  stripWhitespace()
```

``` r
# Create all_coffee
all_coffee <- paste(coffee_text_clean, collapse = " ")

# Create all_chardonnay
all_wine <- paste(wine_text_clean, collapse = " ")

# Create all_tweets
all_tweets <- c(all_coffee, all_wine)

# Convert to a vector source
all_tweets <- VectorSource(all_tweets)

# Create all_corpus
all_corpus <- VCorpus(all_tweets)
```

``` r
# Create all_tdm
all_tdm <- TermDocumentMatrix(all_corpus)

# Create all_m
all_m <- as.matrix(all_tdm)

# Print a commonality cloud
set.seed(1)
wordcloud::commonality.cloud(all_m, max.words = 50, colors = "steelblue1")
```

![commonality\_plot](https://user-images.githubusercontent.com/44796982/65844753-64e4bd80-e372-11e9-8e80-0f0e3460f140.png)

``` r
# Create all_m
all_m <- all_m %>% `colnames<-`(value = c("coffee", "wine"))

# Create comparison cloud
set.seed(10)
wordcloud::comparison.cloud(all_m, colors = c("orange", "blue"), max.words = 50)
```

![comparison\_plot](https://user-images.githubusercontent.com/44796982/65844764-6f9f5280-e372-11e9-8251-db4d70aeef98.png)

# Pyramid plot

``` r
top25_df <- all_m %>%
  as_tibble(rownames = "word") %>% 
  filter_all(all_vars(. > 0)) %>% 
  filter(coffee > 225, wine > 225) %>%
  mutate(coffee_perc = (coffee / length(coffee_text)) * 100, 
         wine_perc = (wine / length(wine_text)) * 100)


plotrix::pyramid.plot(
  top25_df$wine_perc, 
  top25_df$coffee_perc, 
  # Words
  labels = top25_df$word, 
  top.labels = c("wine", "Words", "Coffee"), 
  main = "Words in Common", 
  unit = "%",
  gap = 1.5,
  space = 0.2,
  labelcex = 0.8
)
```

![pyramid\_plot](https://user-images.githubusercontent.com/44796982/65844780-7b8b1480-e372-11e9-9236-80815de1ecdd.png)

# Word Networks

``` r
qdap::word_associate(coffee_text_clean[1:400], match.string = "starbucks", 
               stopwords = c(Top200Words, "coffee", "amp"), 
               network.plot = TRUE, cloud.colors = c("gray85", "darkred"))

# Add title
title(main = "Starbucks Coffee Tweet Associations")
```

![word\_network](https://user-images.githubusercontent.com/44796982/65844785-83e34f80-e372-11e9-994c-959dd1a7783f.png)

# Dendrogram

``` r
# Print the dimensions of tweets_tdm
dim(coffee_tdm)
```

    ## [1] 33761  9991

``` r
# Create tdm1
tdm1 <- removeSparseTerms(coffee_tdm, sparse = 0.95)

# Create tdm2
tdm2 <- removeSparseTerms(coffee_tdm, sparse = 0.975)

# Print tdm1
print(tdm1)
```

    ## <<TermDocumentMatrix (terms: 11, documents: 9991)>>
    ## Non-/sparse entries: 7550/102351
    ## Sparsity           : 93%
    ## Maximal term length: 11
    ## Weighting          : term frequency (tf)

``` r
# Print tdm2
print(tdm2)
```

    ## <<TermDocumentMatrix (terms: 46, documents: 9991)>>
    ## Non-/sparse entries: 19572/440014
    ## Sparsity           : 96%
    ## Maximal term length: 12
    ## Weighting          : term frequency (tf)

``` r
# Create tweets_tdm2
tweets_tdm2 <- removeSparseTerms(coffee_tdm, sparse = 0.975)

# Create tdm_m
tdm_m <- as.matrix(tweets_tdm2)

# Create tweets_dist
tweets_dist <- dist(tdm_m)

# Create hc
hc <- hclust(tweets_dist)  # Hierarchical Cluster

# Plot the dendrogram
plot(hc)
```

![plot1](https://user-images.githubusercontent.com/44796982/66177177-662c2800-e69b-11e9-9ccd-9f4227260b85.png)

``` r
# Create hcd
hcd <- as.dendrogram(hc)

# Change the branch color to red for "marvin" and "gaye"
hcd_colored <- dendextend::branches_attr_by_labels(hcd, c("coffeeaddict", "coffeeshop"), "red")

# Plot hcd
plot(hcd_colored, main = "Better Dendrogram")
```

![plot2](https://user-images.githubusercontent.com/44796982/66177183-70e6bd00-e69b-11e9-91d4-952427f68240.png)

# Association Plot

``` r
# Create associations
associations <- findAssocs(coffee_tdm, "espresso", 0.2)

# Create associations_df
associations_df <- list_vect2df(associations, col2 = "word", col3 = "score")

# Plot the associations_df values
ggplot(associations_df, aes(score, word)) + 
  geom_point(size = 3) + 
  theme_gdocs()
```

![plot3](https://user-images.githubusercontent.com/44796982/66177190-76dc9e00-e69b-11e9-80d8-10f22977a426.png)
