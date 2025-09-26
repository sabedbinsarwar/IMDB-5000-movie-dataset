if (!require(readr)) {
  install.packages("readr")
  library(readr)
} else {
  library(readr)
}
if (!require(naniar)) {
  install.packages("naniar")
  library(naniar)
} else {
  library(naniar)
}
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
} else {
  library(dplyr)
}
if(!require(stringr)) {
  install.packages("stringr")
  library(stringr)
} else { library(stringr) }

if(!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
} else { library(ggplot2) }

if(!require(corrplot)) {
  install.packages("corrplot")
  library(corrplot)
} else { library(corrplot) }

if(!require(tidyr)) {
  install.packages("tidyr")
  library(tidyr)
} else { library(tidyr) }

if(!require(fastDummies)) install.packages("fastDummies")
library(fastDummies)

if(!require(tidyr)) {
  install.packages("tidyr")
  library(tidyr)
} else { library(tidyr) }


url <- "https://drive.google.com/uc?export=download&id=1UiWvPrTNKbMEVyqX0n4mMKrSErR4qRqO"
data <- read_csv(url)

#browse_url<-"https://drive.google.com/file/d/1UiWvPrTNKbMEVyqX0n4mMKrSErR4qRqO/view"
#browseURL(browse_url)

head(data)
View(data)
spec(data)
dim(data)
names(data)
str(data)
summary(data)

sapply(data,function(x) sum(is.na(x)))  

sum(duplicated(data))  


unique(data$genres)
unique(data$language)
unique(data$content_rating)

gg_miss_var(data)     

data_raw<-data

sapply(data,function(x) sum(is.na(x)))  

get_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  names(sort(table(x), decreasing = TRUE))[1]
}


char_cols <- names(Filter(is.character, data))
data[char_cols] <- lapply(data[char_cols], function(x) {
  x <- trimws(x)          
  x[x == ""] <- NA       
  x
})

sapply(data,function(x) sum(is.na(x)))  

mode_color <- get_mode(data$color)
mode_language <- get_mode(data$language)
mode_country <- get_mode(data$country)
mode_aspect <- suppressWarnings(as.numeric(get_mode(data$aspect_ratio)))
if (is.na(mode_aspect)) mode_aspect <- 2.35

med_duration <- median(data$duration, na.rm = TRUE)
med_budget <- median(data$budget, na.rm = TRUE)
med_gross <- median(data$gross, na.rm = TRUE)
med_year <- median(data$title_year, na.rm = TRUE)
med_imdb <- median(data$imdb_score, na.rm = TRUE)


data_clean <- data %>%
  mutate(
    color = coalesce(color, mode_color, "Color"),
    director_name = coalesce(director_name, "Unknown"),
    actor_1_name = coalesce(actor_1_name, "Unknown Actor"),
    actor_2_name = coalesce(actor_2_name, "Unknown Actor"),
    actor_3_name = coalesce(actor_3_name, "Unknown Actor"),
    genres = coalesce(genres, "Unknown"),
    content_rating = coalesce(content_rating, "Unrated"),
    language = coalesce(language, mode_language, "Unknown"),
    country = coalesce(country, mode_country, "Unknown"),
    movie_title = coalesce(movie_title, "Unknown Title"),
    plot_keywords = coalesce(plot_keywords, "Unknown"),
    movie_imdb_link = coalesce(movie_imdb_link, "Unknown"),
    

    num_critic_for_reviews = coalesce(num_critic_for_reviews, 0),
    num_user_for_reviews = coalesce(num_user_for_reviews, 0),
    num_voted_users = coalesce(num_voted_users, 0),
    director_facebook_likes = coalesce(director_facebook_likes, 0),
    actor_1_facebook_likes = coalesce(actor_1_facebook_likes, 0),
    actor_2_facebook_likes = coalesce(actor_2_facebook_likes, 0),
    actor_3_facebook_likes = coalesce(actor_3_facebook_likes, 0),
    cast_total_facebook_likes = coalesce(cast_total_facebook_likes, 0),
    movie_facebook_likes = coalesce(movie_facebook_likes, 0),
    duration = coalesce(duration, med_duration),
    budget = coalesce(budget, med_budget),
    gross = coalesce(gross, med_gross),
    facenumber_in_poster = coalesce(facenumber_in_poster, 0),
    aspect_ratio = coalesce(aspect_ratio, mode_aspect),
    title_year = coalesce(title_year, as.integer(round(med_year))),
    imdb_score = coalesce(imdb_score, med_imdb)
  )
data<-data_clean
sapply(data,function(x) sum(is.na(x)))  

duplicates_all<-data%>%
  filter(duplicated(.))
View(duplicates_all)

data <- data %>% 
  distinct()

sum(duplicated(data))


sum(duplicated(data$movie_title))

data %>%
  group_by(movie_title) %>%
  filter(n() > 1) %>%
  arrange(movie_title) %>%
  View()


data <- data %>%
  distinct(movie_title, .keep_all = TRUE)


char_cols <- names(Filter(is.character, data))
char_cols


title_case_cols <- c("color", "director_name", "actor_1_name", 
                     "actor_2_name", "actor_3_name",
                     "genres","language", 
                      "movie_title")

data[title_case_cols] <- lapply(data[title_case_cols], function(x) {
  str_to_title(x)
})


unique(data$director_name)
unique(data$country)
unique(data$content_rating)


data$genres <- str_trim(data$genres)                     
data$genres <- str_replace_all(data$genres, "\\|\\|+", "|")  


data$movie_title <- str_trim(data$movie_title)          
data$movie_title <- str_replace_all(data$movie_title, "\\*$", "")  


data$plot_keywords <- str_to_lower(data$plot_keywords)  
data$plot_keywords <- str_trim(data$plot_keywords)      


data$movie_imdb_link <- str_trim(data$movie_imdb_link)



num_cols <- c("num_critic_for_reviews", "num_user_for_reviews", 
              "num_voted_users", "director_facebook_likes", 
              "actor_1_facebook_likes", "actor_2_facebook_likes",
              "actor_3_facebook_likes", "cast_total_facebook_likes",
              "movie_facebook_likes", "duration", "budget", 
              "gross", "facenumber_in_poster", "aspect_ratio", 
              "title_year", "imdb_score")


data[num_cols] <- lapply(data[num_cols], as.numeric)

factor_cols <- c("color", "director_name", "actor_1_name", 
                   "actor_2_name", "actor_3_name", "genres", 
                   "content_rating", "language", "country")

data[factor_cols] <- lapply(data[factor_cols], as.factor)


data$title_year <- as.integer(data$title_year)


sapply(data,class)
sapply(data,typeof)


data$duration[data$duration <= 0] <- median(data$duration, na.rm = TRUE)
data$budget[data$budget < 0] <- median(data$budget, na.rm = TRUE)
data$gross[data$gross < 0] <- median(data$gross, na.rm = TRUE)


data$imdb_score[data$imdb_score < 0 | data$imdb_score > 10] <- median(data$imdb_score, na.rm = TRUE)

data2<-data

numeric_for_scaling <- c("duration", "budget", "gross")
data2[numeric_for_scaling] <- lapply(data2[numeric_for_scaling], scale)
View(data2)


factor_cols2 <- c("color", "genres", 
                 "content_rating", "language", "country")



data_encoded <- dummy_cols(data2, select_columns = factor_cols2, remove_first_dummy = TRUE)
View(data_encoded)


data_genres <- data %>%
  separate_rows(genres, sep = "\\|")

genre_count <- data_genres %>%
  count(genres) %>%
  arrange(desc(n))

View(genre_count)


data$profit<-data$gross-data$budget


data$imdb_category<-cut(data$imdb_score,
                        breaks=c(0,5,7,8.5,10),
                        labels=c("Poor","Average","Good","Excellent"))
View(data)


data<-data%>%
  select(-movie_imdb_link)


numeric_cols <- sapply(data, is.numeric)
numeric_cols <- names(numeric_cols[numeric_cols])
numeric_cols


numeric_data <- data %>% select(all_of(numeric_cols))
View(numeric_data)

options(scipen = 999)  
summary(data$duration)
summary(data$budget)
summary(data$gross)
summary(data$imdb_score)
summary(data$profit)
summary(data$title_year)


numeric_cols <- names(Filter(is.numeric, data))


iqr_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  which(x < (Q1 - 1.5*IQR) | x > (Q3 + 1.5*IQR))
}

for (col in numeric_cols) {
  Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  data[[col]][data[[col]] < lower_bound] <- Q1
  data[[col]][data[[col]] > upper_bound] <- Q3
}

outlier_list <- lapply(data[numeric_cols], iqr_outliers)
sapply(outlier_list, length)

sapply(data, function(x) sum(is.na(x)))

summary(numeric_data)

numeric_stats_extended <- numeric_data %>%
  summarise(across(
    everything(),
    list(
      Mean = ~mean(.x, na.rm = TRUE),
      Median = ~median(.x, na.rm = TRUE),
      SD = ~sd(.x, na.rm = TRUE),
      Min = ~min(.x, na.rm = TRUE),
      Max = ~max(.x, na.rm = TRUE),
      Range = ~max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE),
      IQR = ~IQR(.x, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  ))
View(numeric_stats_extended)

top_gross <- data %>%
  arrange(desc(gross)) %>%
  select(movie_title, gross, budget, profit, imdb_score) %>%
  head(10)
View(top_gross)


ggplot(data, aes(x = imdb_score)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "IMDb Score Distribution", x = "IMDb Score", y = "Density")


numeric_vars_long <- data %>%
  select(budget, gross, profit) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

ggplot(numeric_vars_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Boxplots of Budget, Gross, and Profit", y = "Value") +
  theme(legend.position = "none")


numeric_vars_long <- data %>%
  select(budget, gross, profit) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")


ggplot(numeric_vars_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Boxplots of Budget, Gross, and Profit", y = "Value") +
  theme(legend.position = "none") +
  facet_wrap(~Variable, scales = "free")


numeric_vars_long2 <- data %>%
  select(budget, gross) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

ggplot(numeric_vars_long2, aes(x = Value, fill = Variable)) +
  geom_histogram(bins = 30, alpha = 0.6, color = "black") +
  facet_wrap(~Variable, scales = "free") +
  labs(title = "Histograms of Budget and Gross")


ggplot(data, aes(x = imdb_category, y = duration, fill = imdb_category)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Movie Duration by IMDb Category", x = "IMDb Category", y = "Duration (min)") +
  theme(legend.position = "none")


ggplot(data, aes(x = budget, y = gross)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Budget vs Gross Revenue", x = "Budget", y = "Gross")


ggplot(data, aes(x = imdb_score, y = profit, color = imdb_category)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Profit vs IMDb Score by Category", x = "IMDb Score", y = "Profit")


cor_matrix <- cor(numeric_data, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.cex = 0.7)

pairs(
  data[, c("budget", "gross", "profit", "imdb_score")],
  main = "Scatterplot Matrix of Movie Data",
  col = data$imdb_category,   
  pch = 19                     
)


top_genres <- data_genres %>%
  count(genres, sort = TRUE) %>%
  top_n(10, n)

ggplot(top_genres, aes(x = reorder(genres, n), y = n)) +
  geom_bar(stat = "identity", fill = "purple", alpha = 0.7) +
  coord_flip() +
  labs(title = "Top 10 Genres", x = "Genre", y = "Number of Movies")


ggplot(data, aes(x = content_rating, fill = content_rating)) +
  geom_bar() +
  coord_flip() +
  labs(title = "Movies by Content Rating", x = "Content Rating", y = "Count") +
  theme(legend.position = "none")


ggplot(data, aes(x = imdb_category, fill = imdb_category)) +
  geom_bar() +
  labs(title = "Movies by IMDb Category", x = "IMDb Category", y = "Count") +
  theme(legend.position = "none")

data_genres <- data_genres %>%
  mutate(profit = gross - budget)


avg_profit_genre <- data_genres %>%
  group_by(genres) %>%
  summarise(avg_profit = mean(profit, na.rm = TRUE)) %>%  
  arrange(desc(avg_profit)) %>%
  head(10)


ggplot(avg_profit_genre, aes(x = reorder(genres, avg_profit), y = avg_profit)) +
  geom_bar(stat = "identity", fill = "gold", alpha = 0.8) +
  coord_flip() +
  labs(title = "Top 10 Profitable Genres", x = "Genre", y = "Average Profit")


top_countries <- data %>%
  group_by(country) %>%
  summarise(count_movies = n(), avg_imdb = mean(imdb_score, na.rm = TRUE)) %>%
  arrange(desc(count_movies)) %>%
  head(10)

ggplot(top_countries, aes(x = reorder(country, avg_imdb), y = avg_imdb)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Average IMDb Score by Country (Top 10)", x = "Country", y = "Average IMDb Score")


movies_per_year <- data %>%
  group_by(title_year) %>%
  summarise(count_movies = n())

ggplot(movies_per_year, aes(x = title_year, y = count_movies)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red") +
  labs(title = "Number of Movies Released per Year", x = "Year", y = "Number of Movies")

