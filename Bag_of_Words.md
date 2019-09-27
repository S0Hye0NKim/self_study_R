``` r
library(tidyverse)
library(rtweet)
library(tidytext)
library(tm)
library(qdap)
```

Data
====

``` r
coffee_tweets <- search_tweets(q = "#coffee", n = 10000,
                                      lang = "en",
                                      include_rts = FALSE)
saveRDS(coffee_tweets, "coffee_tweets.rds")
```

``` r
coffee_tweets <- readRDS("coffee_tweets.rds")
coffee_text <- coffee_tweets$text
```

Make Corpus
===========

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
content(coffee_corpus[[10]])
```

    ## [1] "Happy Friday!! Wherever the weekend takes you, I hope it's a wonderful one!\n\nI'm working in a new manuscript, so tons of coffee is in my near future.\n\nWhat're up to yourself?\n\n#amwriting #coffee #WritingCommunity #thethrillofablankpage #fueledbyfaithandcaffeine https://t.co/1CtuLNl91w"

Some useful function
====================

``` r
# Create the object: text
text <- "<b>She</b> woke up at       6 A.M. It\'s so early!  She was only 10% awake and began drinking coffee in front of her computer."

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

Cleaning Corpus
===============

``` r
# Alter the function code to match the instructions
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, words = c(stopwords("en"), "coffee", "mug"))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Apply your customized function to the tweet_corp: clean_corp
clean_corp <- clean_corpus(coffee_corpus)

# Print out a cleaned up tweet
content(clean_corp[[100]])
```

    ## [1] "black egg mushroom muffin currentaffairs little sign advising people mental age lunch eatlessmeat costacoffee simplythebest theweek httpstcohnzduxriie"

``` r
# Print out the same tweet in original form
coffee_text[100]
```

    ## [1] "Black #coffee, egg and mushroom muffin, #currentaffairs and a little sign advising people of my mental age.\n\n#lunch #eatlessmeat #CostaCoffee #SimplytheBest #TheWeek https://t.co/hnzDUxRiIe"

\#Document-Term Matrix and Term-Document Matrix

``` r
# Create the document-term matrix from the corpus
coffee_dtm <- DocumentTermMatrix(clean_corp) %>% as.matrix

# Print the dimensions of coffee_m
dim(coffee_dtm)
```

    ## [1]  9991 44000

``` r
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
coffee_tdm <- TermDocumentMatrix(clean_corp) %>% as.matrix()

# Print the dimensions of the matrix
dim(coffee_tdm)
```

    ## [1] 44000  9991

``` r
# Review a portion of the matrix
coffee_tdm[c("star", "starbucks"), 1550:1560]
```

    ##            Docs
    ## Terms       1550 1551 1552 1553 1554 1555 1556 1557 1558 1559 1560
    ##   star         0    0    0    0    0    0    0    0    0    1    0
    ##   starbucks    1    2    0    0    1    0    1    0    1    1    1