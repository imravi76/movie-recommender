
#Importing Libraries
library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape)

#Defining Working Directory
setwd("C:/Users/user/Documents/movie_data")

#Importing Datasets
movies_data <- read.csv("movies.csv",stringsAsFactors=FALSE)
summary(movies_data)

ratings_data <- read.csv("ratings.csv")
summary(ratings_data)

#Data Preprocessing on Movies Dataset
#Extracting genres from dataset
movies_genre <- as.data.frame(movies_data$genres, stringsAsFactors=FALSE)

#Separating genres into columns
movies_genre_new <- as.data.frame(tstrsplit(movies_genre[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(movies_genre_new) <- c(1:10)

#Generating all the genres into one place
list_genre <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime","Documentary", "Drama", "Fantasy", "Film-Noir", "Horror", "Musical", "Mystery","Romance", "Sci-Fi", "Thriller", "War", "Western")

#Creating Matrix of movies with respected genres
genre_mat1 <- matrix(0,10330,18)
genre_mat1[1,] <- list_genre
colnames(genre_mat1) <- list_genre


for (index in 1:nrow(movies_genre_new)) {
  for (col in 1:ncol(movies_genre_new)) {
    gen_col = which(genre_mat1[1,] == movies_genre_new[index,col])
    genre_mat1[index+1,gen_col] <- 1
  }
}

#Removing row from first matrix for converting genre_mat1's data from strings to integers
genre_mat2 <- as.data.frame(genre_mat1[-1,], stringsAsFactors=FALSE)

#Now Converting
for (col in 1:ncol(genre_mat2)) {
  genre_mat2[,col] <- as.integer(genre_mat2[,col])
}

genre_mat2

#Binding both data with Movies names and genres for creating a single matrix
SearchMatrix <- cbind(movies_data[,1:2], genre_mat2[])

#Data Preprocessing on Ratings Dataset
ratingsMatrix <- dcast(ratings_data, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingsMatrix <- as.matrix(ratingsMatrix[,-1])

#Converting ratings into sparse matrix
ratingsMatrix <- as(ratingsMatrix, "realRatingMatrix")

#Exploring Data
#Users's Similarities based on cosine method
similarity_mat <- similarity(ratingsMatrix[1:4, ], method = "cosine", which = "users")
as.matrix(similarity_mat)
image(as.matrix(similarity_mat), main = "User's Similarities")

#Movies' Similarity
movies_similarity <- similarity(ratingsMatrix[, 1:4], method = "cosine", which = "items")
as.matrix(movies_similarity)
image(as.matrix(movies_similarity), main = "Movies similarity")

#Finding unique ratings
ratings_values <- as.vector(ratingsMatrix@data)
unique(ratings_values)

#Counting movies similar to ratings
Table_of_Ratings <- table(ratings_values)
Table_of_Ratings

#Visualization
movies_views <- colCounts(ratingsMatrix)
table_views <- data.frame(movie = names(movies_views), views = movies_views)

#Sorting
table_views <- table_views[order(table_views$views, decreasing = TRUE), ]

table_views$title <- NA
for (index in 1:10325){
  table_views[index,3] <- as.character(subset(movies_data, movies_data$movieId == table_views[index, 1])$title)
}

#Visulazing Total number of views of Top Films
ggplot(table_views[1:6, ], aes(x = title, y = views)) +
geom_bar(stat="identity", fill = 'steelblue') +
geom_text(aes(label=views), vjust=-0.3, size=3.5) +
theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
ggtitle("Total Views of the Top Films")

#Data Preparation
#Finding Useful Data based on min 50 movies or users who rated min 50 movies
movies_ratings <- ratingsMatrix[rowCounts(ratingsMatrix) > 50, colCounts(ratingsMatrix) > 50]

minimum_movies<- quantile(rowCounts(movies_ratings), 0.98)
minimum_users <- quantile(colCounts(movies_ratings), 0.98)

#Normalizing the Data for better model creation
normalized_ratings <- normalize(movies_ratings)
sum(rowMeans(normalized_ratings) > 0.00001)

#Binarizing the Data for recommendation system to work efficiently
binary_minimum_movies <- quantile(rowCounts(movies_ratings), 0.95)
binary_minimum_users <- quantile(colCounts(movies_ratings), 0.95)

good_rated_films <- binarize(movies_ratings, minRating = 3)

#Building Collborative Filtering Model
sample_data<- sample(x = c(TRUE, FALSE), size = nrow(movies_ratings), replace = TRUE, prob = c(0.8, 0.2))
training_data <- movies_ratings[sample_data, ]
testing_data <- movies_ratings[!sample_data, ]

recommendation_system <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
recommendation_system$IBCF_realRatingMatrix$parameters

recommender_model <- Recommender(data = training_data, method = "IBCF", parameter = list(k = 30))
recommender_model

#Predicting or Recommending Movies
top_recommendations <- 10 # number of movies to recommend per user
predicted_recommendations <- predict(object = recommender_model, newdata = testing_data, n = top_recommendations)
predicted_recommendations

userOne <- predicted_recommendations@items[[1]] # recommendation for first user
movies_userOne <- predicted_recommendations@itemLabels[userOne]
movies_userTwo <- movies_userOne
for (index in 1:10){
  movies_userTwo[index] <- as.character(subset(movies_data, movies_data$movieId == movies_userOne[index])$title)
}
movies_userTwo


recommendation_matrix <- sapply(predicted_recommendations@items, function(x){ as.integer(colnames(movies_ratings)[x]) })
recommendation_matrix[,1:4]
