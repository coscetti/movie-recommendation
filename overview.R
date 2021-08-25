library(rstudioapi)
library(recommenderlab)
library(data.table)
library(ggplot2)

# import data
movies <- read.csv("./data/IMDB-Dataset/movies.csv", stringsAsFactors = FALSE)
str(movies)
ratings <- read.csv("./data/IMDB-Dataset/ratings.csv", stringsAsFactors = FALSE)
str(ratings)

# data pre-processing
movie_genre <- as.data.frame(movies$genres, stringsAsFactors = FALSE)

movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], '[|]', 
                                        type.convert = TRUE), 
                              stringsAsFactors = FALSE)

colnames(movie_genre2) <- c(1:10)

list_genre <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime", "Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery", "Romance",
                "Sci-Fi", "Thriller", "War", "Western")

genre_mat1 <- matrix(0, 10330, 18)
genre_mat1[1,] <- list_genre

for (i in 1:nrow(movie_genre2)) {
  for (c in 1:ncol(movie_genre2)) {
    gen_col <- which(genre_mat1[1,] == movie_genre2[i,c])
    genre_mat1[i+1, gen_col] <- 1
  }
}

genre_mat2 <- as.data.frame(genre_mat1[-1,], stringsAsFactors = FALSE)

for (c in 1:ncol(genre_mat2)) {
  genre_mat2[,c] <- as.integer(genre_mat2[,c])
}
colnames(genre_mat2) <- list_genre
str(genre_mat2)

searchMatrix <- cbind(movies[,1:2], genre_mat2[])
head(searchMatrix)

ratingMatrix <- dcast(ratings, userId~movieId, 
                      value.var = 'rating', 
                      na.rm = FALSE)
ratingMatrix <- as.matrix(ratingMatrix[,-1])
ratingMatrix <- as(ratingMatrix, 'realRatingMatrix')

recommendation_model <- recommenderRegistry$get_entries(dataType = 
                                                          'realRatingMatrix')
names(recommendation_model)

lapply(recommendation_model, '[[', 'description')

recommendation_model$IBCF_realRatingMatrix$parameters

similarity_mat <- similarity(ratingMatrix[1:4,], 
                             method = 'cosine', 
                             which = 'users')

as.matrix(similarity_mat)
image(as.matrix(similarity_mat), main = "User's Similarities")

movie_sim <- similarity(ratingMatrix[1:4,], 
                             method = 'cosine', 
                             which = 'items')

as.matrix(movie_sim)
image(as.matrix(movie_sim), main = "Movies Similarities")

rating_values <- as.vector(ratingMatrix@data)
unique(rating_values)

table_of_ratings <- table(rating_values)

movie_views <- colCounts(ratingMatrix)
table_views <- data.frame(movie = names(movie_views), 
                          views = movie_views)
table_views <- table_views[order(table_views$views, decreasing = TRUE),]
table_views$title <- NA

for (index in 1:10325) {
  table_views[index,3] <- 
    as.character(subset(movies, movies$movieId == table_views[index,1])$title)
}
table_views[1:6,]


ggplot(table_views[1:6,], aes(x = title, y = views)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  geom_text(aes(label=views), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  ggtitle("total Views of the Top Films")


image(ratingMatrix[1:20, 1:25], 
      axes = FALSE, 
      main = "Heatmap of the first 20 rows and 25 columns")
