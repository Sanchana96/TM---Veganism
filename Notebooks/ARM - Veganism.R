# Load required libraries
library(arules)
library(arulesViz)
library(dplyr)

# Load the data

data <- read.csv("C:/Users/Sanchu/Downloads/cleanedveganism.csv")
data_comment_veganism <- data %>% filter(data$veganism == "veganism")
data_comment_nonveganism <- data %>% filter(data$veganism == "non veganism")
data_comment_vegan <- tokenizers::tokenize_words(
  data_comment_veganism$veganism.1,stopwords = stopwords::stopwords("en"), 
   lowercase = T,  strip_punct = T, strip_numeric = T,
   simplify = F)

data_comment_Nvegan <- tokenizers::tokenize_words(
  data_comment_nonveganism$veganism.1,stopwords = stopwords::stopwords("en"), 
  lowercase = T,  strip_punct = T, strip_numeric = T,
  simplify = F)
 
# # Convert the data to transactions
transactions_vegan <- as(data_comment_vegan, "transactions")
transactions_nvegan <- as(data_comment_Nvegan, "transactions")

# Explore the transactions
inspect(transactions_vegan)
inspect(transactions_nvegan)

# Perform association rule mining
rules_vegan <- apriori(transactions_vegan, parameter = list(support = 0.01, confidence = 0.05, minlen = 2),
                       appearance = list(default="lhs", rhs="veganism"),
                       control=list(verbose=FALSE))

# Perform association rule mining
rules_nvegan <- apriori(transactions_vegan, parameter = list(support = 0.01, confidence = 0.05, minlen = 2),
                       appearance = list(default="lhs", rhs="veganism"),
                       control=list(verbose=FALSE))

# Explore the rules
inspect(rules_vegan)
inspect(rules_nvegan)

## Plot of which items are most frequent
itemFrequencyPlot(transactions_vegan, topN=20, type="absolute")



# Sort the rules by different metrics and extract the top 15
support_rules_vegan <- sort(rules_vegan, by = "support", decreasing = TRUE)[1:15]
support_rules_nvegan <- sort(rules_nvegan, by = "support", decreasing = TRUE)[1:15]
confidence_rules_vegan <- sort(rules_vegan, by = "confidence", decreasing = TRUE)[1:15]
confidence_rules_nvegan <- sort(rules_nvegan, by = "confidence", decreasing = TRUE)[1:15]
lift_rules_vegan <- sort(rules_vegan, by = "lift", decreasing = TRUE)[1:15]
lift_rules_nvegan <- sort(rules_nvegan, by = "lift", decreasing = TRUE)[1:15]

# Print the top rules
inspect(support_rules_vegan)
inspect(confidence_rules_vegan)
inspect(lift_rules_vegan)
inspect(support_rules_nvegan)
inspect(confidence_rules_nvegan)
inspect(lift_rules_nvegan)

# Visualize the rules as a graph
plot(support_rules_vegan, method = "graph", engine="htmlwidget")
plot(confidence_rules_vegan, method = "graph",engine="htmlwidget")
plot(support_rules_nvegan, method = "graph", engine="htmlwidget")
plot(confidence_rules_nvegan, method = "graph",engine="htmlwidget")


# Generate a scatterplot of the rules
plot(rules_vegan, method = "scatterplot",engine="htmlwidget")

# Generate a two key of the rules
plot(rules_nvegan, method = "two-key plot",engine="htmlwidget")
