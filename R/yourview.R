################################################################################
# TODO:
# recreate James' analytic pipeline from pre-prepared data table 
# look at how Chris pulled the data 
# create new SQL query to extract features of comments?

################################################################################
# load required libraries
library(RPostgreSQL)
library(ranger)
library(jsonlite)
library(caret)

################################################################################
# read config file
confile <- "~/yourview/config.json"
config <- fromJSON(readLines(confile))

################################################################################
# load the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

# create a connection to the postgres database
con <- dbConnect(drv, dbname = config$database,
                 host = config$host, port = config$port,
                 user = config$username, password = config$password)

################################################################################
# PREPARE DATA
# read Chris' SQL query
fileName <- '~/Documents/2017-02_YourView_data_extraction/comments.sql'
comments.query <- readChar(fileName, file.info(fileName)$size)

# load the comments data frame using said query
comments.df <- dbGetQuery(con, comments.query)

# RECREATE JAMES' RANDOM FOREST APPROACH
# with some modifications - see notes below. 

# these are the variables James used in his analysis. 
selected.vars <- c("user_id",
                   "commenter_credibility",
                   "commenter_top_level_comment_count", 
                   "commenter_reply_count", 
                   "issue_vote_count", 
                   "issue_comment_vote_count", 
                   "issue_comment_count", 
                   "issue_argument_count", 
                   "word_count", 
                   "entry_order", # nb: James' data said "entry_order_in_issue"
                   "entry_order_in_thread", 
                   "argument_entry_order", 
                   "upvotes_count", 
                   "downvotes_count", 
                   "karma_score", 
                   "total_vote_count", 
                   "vp_supporting_upvotes_received", 
                   "vp_neutral_upvotes_received", 
                   "vp_undermining_upvotes_received", 
                   "vp_supporting_downvotes_received", 
                   "vp_neutral_downvotes_received", 
                   "vp_undermining_downvotes_received", 
                   "total_votes", 
                   "commenter_position", 
                   "type",
                   "engaged_upvote_count", 
                   "engaged_downvote_count", 
                   "raw_evaluation")

comments.df2 <- comments.df[, selected.vars]

# check number of NAs in each column.
# some columns have lots!
numnas <- function(x) sum(is.na(x))
na.sum <- apply(comments.df2, 2, numnas)

# select the vars with fewer than 10% NAs
few.nas <- selected.vars[na.sum <= nrow(comments.df2)/10]
comments.df2 <- comments.df2[, few.nas]

# delete remaining rows with any NAs
na.sum <- apply(comments.df2, 1, numnas)
comments.df2 <- comments.df2[na.sum==0, ]

################################################################################
# TRAIN-TEST SPLITS & EVALUATION

# split up USERS into ten groups, and use those to partition comments
users <- unique(comments.df2$user_id)
# note that createFolds returns a list of *indices* of user IDs.
flds <- createFolds(users, k = 10, list = TRUE, returnTrain = FALSE)
# fld holds the "fold number" of each comment
fld <- vector(mode="numeric", length=nrow(comments.df2))
for (i in 1:10) {
  fld[which(comments.df2$user_id %in% users[flds[[i]]])] <- i
}

# create empty vector to hold the predicitons
predicted <- vector(mode="numeric", length=nrow(comments.df2))

for (i in 1:10) {
  # build training set and test set
  train.set <- fld %in% setdiff(1:10, i)
  test.set <- fld == i
  
  # build random forest using ranger package, on training data
  forest <- ranger(raw_evaluation ~ ., comments.df2[train.set, ])
  
  # save predictions on the test set
  test.pred <-  predict(forest, subset(comments.df2, subset=test.set, 
                                                select=-c(raw_evaluation)))
  predicted[test.set] <- test.pred$predictions
}

# correlation between predicted and true values
cor(comments.df2$raw_evaluation, predicted)

# TODO: better evaluation of the model 
# TODO: repeat multiple times and average results
# TODO: set seed for reproducible results
# TODO: check folds for how even/uneven the group sizes are?
# TODO: down-weight comments by prolific users


################################################################################
# NOTES: RECREATE JAMES' RANDOM FOREST APPROACH

# PROBLEM 1: many comments made by a small number of users! 
# 1. how to handle this when splitting the data into train and test sets?
# 2. would not want small number of users to have undue leverage over results
#
# options:
# subsample data when splitting into train & test sets
# downweight comments from users with lots of comments
# 
# the randomForest package does not allow you to weight *cases*. 
# The ranger package allows this. 
# But the ranger package does not have a method for imputing missing data.

# PROBLEM 2: I don't like any of R's methods for dealing with missing data in a 
# random forest context.
#
# Weka handles this by splitting the cases with missing values into "fractional
# instances". See:
# http://weka.8497.n7.nabble.com/random-forest-and-missing-values-td30476.html
# 
# in R, you can impute values, drop cases with missing values, or drop vars.
# 
# For now, I'll just drop vars and cases, but I'll come back to this.

################################################################################
# # DATA EXPLORATION
#
# # list tables in DB
# dbGetQuery(con, 
#            "SELECT * FROM information_schema.tables 
#            WHERE table_schema = 'public'")
# 
# # list columns in the evaluations table
# dbGetQuery(con, 
#            "SELECT column_name FROM information_schema.columns 
#            WHERE table_schema = 'public'
#            AND table_name     = 'evaluations'")
# 
# # have a look at the first few lines of important tables
# dbGetQuery(con, "SELECT * FROM evaluations LIMIT 10")
# dbGetQuery(con, "SELECT * FROM comments LIMIT 10")
# dbGetQuery(con, "SELECT * FROM credibility_scores")
# dbGetQuery(con, "SELECT * FROM domain_memberships LIMIT 10")
# dbGetQuery(con, "SELECT * FROM questions LIMIT 10")
# dbGetQuery(con, "SELECT * FROM issues LIMIT 10")
# dbGetQuery(con, "SELECT * FROM schema_migrations LIMIT 10")
# dbGetQuery(con, "SELECT * FROM states LIMIT 10")
# dbGetQuery(con, "SELECT * FROM tags LIMIT 10")
# dbGetQuery(con, "SELECT * FROM issues LIMIT 10")
# dbGetQuery(con, "SELECT * FROM users LIMIT 10")
# dbGetQuery(con, "SELECT * FROM votes LIMIT 10")
# 
# # couple things I noticed:
# # links issues to fora (and vice versa? Is it a many:many mapping?)
# dbGetQuery(con, "SELECT * FROM issue_forum_links LIMIT 10")
# 
# # users can subscribe to issues
# dbGetQuery(con, "SELECT * FROM issue_subscriptions LIMIT 10")

################################################################################
## names of comments df
# c('id', 'comment_title', 'user_id', 'commenter_credibility', 
# 'commenter_mean_adj_evaluation', 'commenter_mean_raw_evaluation', 
# 'commenter_top_level_comment_count', 'commenter_reply_count', 'issue_id', 
# 'issue_title', 'issue_vote_count', 'issue_comment_vote_count', 
# 'issue_comment_count', 'issue_argument_count', 'word_count', 'entry_order', 
# 'entry_order_in_thread', 'argument_entry_order', 'upvotes_count', 
# 'downvotes_count', 'karma_score', 'total_vote_count', 
# 'vp_supporting_upvotes_received', 'vp_neutral_upvotes_received', 
# 'vp_undermining_upvotes_received', 'vp_supporting_downvotes_received', 
# 'vp_neutral_downvotes_received', 'vp_undermining_downvotes_received', 
# 'total_votes', 'commenter_position', 'type', 'comment_text', 'evaluation_1', 
# 'evaluation_comments_1', 'evaluation_2', 'evaluation_comments_2', 
# 'evaluation_3', 'evaluation_comments_3', 'evaluation_4', 
# 'evaluation_comments_4', 'evaluation_5', 'evaluation_comments_5', 
# 'raw_evaluation', 'adjusted_evaluation', 'evaluated_upvote_count', 
# 'evaluated_downvote_count', 'mean_upvoter_adj_evaluation', 
# 'mean_upvoter_raw_evaluation', 'mean_downvoter_adj_evaluation', 
# 'mean_downvoter_raw_evaluation', 'engaged_upvote_count', 
# 'engaged_downvote_count', 'mean_engaged_upvoter_adj_evaluation', 
# 'mean_engaged_upvoter_raw_evaluation', 'mean_engaged_downvoter_adj_evaluation', 
# 'mean_engaged_downvoter_raw_evaluation', 'evaluated_by', 'selected_evaluator')



