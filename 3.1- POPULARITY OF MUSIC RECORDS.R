songs <- read.csv("./data/songs.csv")
# How many observations (songs) are from the year 2010?
length(songs$year[songs$year == 2010])
# How many songs does the dataset include for which the artist name is "Michael Jackson"?
length(songs$artistname[songs$artistname == "Michael Jackson"])
# Which of these songs by Michael Jackson made it to the Top 10? Select all that apply.
songs$songtitle[songs$artistname == "Michael Jackson" & songs$Top10 == 1]
# The variable corresponding to the estimated time signature (timesignature) is discrete, meaning that it only takes
# integer values (0, 1, 2, 3, . . . ). What are the values of this variable that occur in our dataset? Select all that apply.
unique(songs$timesignature)
# Which timesignature value is the most frequent among songs in our dataset?
table(songs$timesignature)
# Out of all of the songs in our dataset, the song with the highest tempo is one of the following songs. Which one is it?
songs$songtitle[which.max(songs$tempo)]

# Creating Our Prediction Model
SongsTrain = subset(songs, songs$year <= 2009)
SongsTest = subset(songs, songs$year  > 2009)
nrow(SongsTrain)
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)

# What is the correlation between the variables "loudness" and "energy" in the training set?
cor(SongsTrain$energy,SongsTrain$loudness)

SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)

# Make predictions on the test set using Model 3. What is the accuracy of
# Model 3 on the test set, using a threshold of 0.45?

predictTest  = predict(SongsLog3,type = "response", newdata = SongsTest)
table(SongsTest$Top10,predictTest > 0.45)

#  What would the accuracy of the baseline model be on the test set? 

length(SongsTest$Top10[SongsTest$Top10 != 1]) / nrow(SongsTest)

