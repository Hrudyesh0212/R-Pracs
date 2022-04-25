setwd("C:/Users/Yash/OneDrive/Desktop/R-Lab/exp 9")
movies <- read.csv("Movie Ratings.csv")
head(movies)
colnames(movies) <- c("Film", "Genre", "CriticRating",
                      "AudienceRating", "BudgetMillions","Year")
head(movies)
tail(movies)
str(movies)
summary(movies)
movies$Film <- factor(movies$Film)
movies$Genre<- factor(movies$Genre)
movies$Year <- factor(movies$Year)
#Pie chart
ggplot(movies,aes(x = "", y =Genre, fill= factor(Genre))) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0)+
  theme_void()
#Bar Chart
ggplot(data=movies) +
  geom_bar(aes(x=Year,fill= factor(Genre)))
#Box Plot
ggplot(data=movies, aes(x=CriticRating, y=AudienceRating,color=Genre))+ geom_boxplot()
#Histogram
ggplot(data=movies) + geom_histogram(binwidth = 10,aes(x=CriticRating),fill="White", color="Blue")
#Scatterplot
ggplot(data=movies, aes(x=CriticRating, y=AudienceRating)) +geom_point(aes(color=Genre))
#Line Graph
ggplot(data=movies, aes(x=CriticRating, y=AudienceRating)) +geom_line()
#Frequency Polygon
ggplot(data=movies,aes(AudienceRating))+geom_area(aes(y=..count..), bins=10, stat='bin', fill='Blue')