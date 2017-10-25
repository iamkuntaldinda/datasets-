download.file("http://files.grouplens.org/datasets/movielens/ml-100k.zip",destfile = "ml-100k.zip")

unzip("ml-100k.zip",exdir ="movies")

dir("movies/ml-100k/")

ratings <- read.table("movies//ml-100k/u.data", header = FALSE, sep = "\t",col.names = c("UserID","MovieID","Rating","Timestamp"))

head(ratings)
ratings$Timestamp <- as.POSIXct(ratings$Timestamp,origin = "1970-01-01")

head(ratings)

library(reshape2)
rm <- dcast(UserID ~ MovieID, data = ratings, value.var ="Rating")

install.packages("useful")
require(useful)
corner(rm)
rownames(rm) <- sprintf("User%s", rm$UserID)
corner(rm)
colnames(rm) <- sprintf("Movie%s", colnames(rm))
corner(rm)
rm$MovieUserID <- NULL

rm <- as.matrix(rm)

install.packages('recommenderlab')
library(recommenderlab)

ratem <- as(rm,"realRatingMatrix")
head(as(ratem,"data.frame"))
as(ratem,"list")[[1]]
as(ratem,"list")[[2]]

image(ratem)

item <- Recommender(ratem, method = "POPULAR")
item

getModel(item)


###### TEXT mining using RTextTools package
install.packages('RTextTools')
library(RTextTools)











