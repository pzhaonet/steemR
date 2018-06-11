#' Download data from steemdata.com
#'
#' @param method character. specifying what report.
#' - A: Account Report;
#' - P: Post Report;
#' - V: Voter Report
#' @param A character. The name of the account for account report
#' @param P character. The name of the account for post report
#' @param V character. The name of the account for voter report
#'
#' @return a list
#' @export
#'
#' @examples
#' info()
info <- function(method = c('A', 'P', 'V'), # A: Account Report; P: Post Report; V: Voter Report
                 A = '',
                 P = '',
                 V = ''){
  method <- match.arg(method)
  myurl <- "mongodb://steemit:steemit@mongo1.steemdata.com:27017/SteemData"

  Sys.setenv(TZ='GMT')
  datatime <- format(as.POSIXct(Sys.time()))

  ### AccountOperations # Operations of an ID
  accountoperations <- mongo(collection = "AccountOperations", url = myurl)
  myao <- accountoperations$find(paste0('{"account" : "', A, '"}'))

  ### Accounts # profile of an ID
  accounts <- mongo(collection = "Accounts", url = myurl)
  myaccounts <- accounts$find(paste0('{"name" : "', A, '"}'))

  ### Comments # time comsuming!
  if (method == 'P') { # switch on only if 'P'
    comments <- mongo(collection = "Comments", url = myurl)
    mycomments <- comments$find(paste0('{"root_identifier" : "', P, '"}'))
    # mycomments <- I(lapply(P, function(P) comments$find(paste0('{"root_identifier" : "', P, '"}')))) # query the comments of multiple given posts
  }

  # ### Operations # time comsuming!
  # operations <- mongo(collection = "Operations", url = myurl)
  # myoperations <- operations$find(paste0('{"worker_account" : "', id, '"}'))

  ### posts
  posts <- mongo(collection = "Posts", url = myurl)
  myposts <- posts$find(paste0('{"author" : "', A, '"}'))

  ### PriceHistory
  # pricehistory <- mongo(collection = "PriceHistory", url = myurl)
  # mypricehistory <- pricehistory$find()

  ### stats
  stats <- mongo(collection = "stats", url = myurl)
  mystats <- stats$find()

  info <- list(method = method,
               datatime = datatime,
               accountoperations = myao,
               accounts = myaccounts,
               comments = ifelse(method == 'P', mycomments, NA),
               posts = myposts,
               stats = mystats)
  return(info)
}

#' Get an ID's detailed info
#'
#' @param id character. Steem ID without '@'
#' @param method character. Steem data server.
#'
#' @return a dataframe of an ID's detailed info
#' @export
#'
#' @examples
#' followlist()
idinfo <- function(id = 'dapeng', method = c('steemdb.com', 'steemdata.com')){
  method <- match.arg(method)
  if (method == 'steemdb.com') {
    ulr <-   paste0("https://steemdb.com/@", id,  "/data")
    theurl <- RCurl::getURL(ulr, .opts = list(ssl.verifypeer = FALSE))
    info <- XML::readHTMLTable(theurl, stringsAsFactors = FALSE)[[1]]
    info_ls <- as.list(info[,2])
    names(info_ls) <- info[, 1]
    return(info_ls)
  } else if(method == 'steemdata.com') {
    myurl <- "mongodb://steemit:steemit@mongo1.steemdata.com:27017/SteemData"
    Sys.setenv(TZ='GMT')
    accounts <- mongo(collection = "Accounts", url = myurl)
    return(accounts$find(paste0('{"name" : "', id, '"}')))
  }
}


#' Get a name list of an ID's followers and following
#' @param id character without '@'
#'
#' @return a name list of an ID's followers and following
#' @export
#'
#' @examples
#' followlist()
followlist <- function(id = 'dapeng', method = c('steemdb.com', 'steemdata.com')){
  method <- match.arg(method)
  # from steemdb.com
  if (method == 'steemdb.com') {
    clearferfing <- function(x){
      y <- gsub('\\"','', gsub('\n', '', gsub(' ', '', x)))
      y <- substr(y, 2, nchar(y) - 1)
      strsplit(y, ',')[[1]]
    }
    info_ls <- idinfo(id = id, method = method)
    return(list(followers = clearferfing(info_ls$followers),
                following = clearferfing(info_ls$following)))
  } else if(method == 'steemdata.com') {
  # from steemdata.com
  myaccounts <- idinfo(id = id, method = method)
  return(list(followers = myaccounts$followers[[1]],
              following = myaccounts$following[[1]]))
  }
}

#' The id list of an id's following  from steemdb.com
#'
#' @param id character without '@'
#'
#' @return character a dataframe of an ID's following info
#' @export
#'
#' @examples
#' following()
following <- function(id = 'dapeng'){
  y <- paste0('https://steemdb.com/@', id, '/following')
  theurl <- RCurl::getURL(y,.opts = list(ssl.verifypeer = FALSE) )
  fing <- XML::readHTMLTable(theurl, stringsAsFactors = FALSE)[[1]]
  # following
  fing$nr <- ifelse(fing$What == 'Follow', 1, -1)
  # fingsum <- beginr::tapplydf(data = fing, 'nr', 'Account', sum)# tapply(fing$nr, fing$Account, sum)
  fing$Account <- as.character(fing$Account)
  return(fing)
}

#' A id list of an id's followers from steemdb.com
#'
#' @param id character without '@'
#'
#' @return character a dataframe of an ID's followers' info
#' @export
#'
#' @examples
#' follower()
follower <- function(id = 'dapeng'){
  x <- paste0("https://steemdb.com/@", id,  "/followers/whales")
  theurl <- RCurl::getURL(x, .opts = list(ssl.verifypeer = FALSE) )
  if (theurl == '') return(data.frame())
  fer <- XML::readHTMLTable(theurl, stringsAsFactors = FALSE)[[1]]
  fer$Account <- as.character(fer$Account)
  fer$Followers <- as.numeric(as.character(fer$Followers))
  fer$Posts <- as.numeric(as.character(fer$Posts))
  fer$Vests <- as.character(fer$Vests)
  fer$Account <- substr(fer$Account, regexpr('\n', fer$Account)[1] + 7, nchar(fer$Account))
  fer$unit <- substr(fer$Vests, nchar(fer$Vests) - 1, nchar(fer$Vests))
  fer$vests <- substr(fer$Vests, 1, nchar(fer$Vests) - 3)
  # matrix(unlist(strsplit(fer$Vests, '聽')), ncol = 2, byrow = TRUE)
  unitconvert <- function(x){
    switch(x,
           'GV' = 10 ^ 9,
           'MV' = 10 ^ 6,
           'kV' = 10 ^ 3)
  }
  fer$multi <- sapply(fer$unit, unitconvert)
  fer$vests <- as.numeric(fer$vests) * fer$multi
  return(fer)


  # finginfer <- unique(fing$Account) %in% fer$Account
  # ferinfing <- fer$Account %in% unique(fing$Account)

  # par(mar = c(0,0,0,0))
  # layout(mat = matrix(c(1, 1, 1, 2, 2, 2, 3, 4, 5), nrow = 3, byrow = TRUE), heights = c(1, 1, 20))
  # beginr::plotblank()
  # text(1, 1, paste0('@',id, "'s followers"), col = 'steelblue', cex = 2.5)
  # beginr::plotblank()
  # text(1, 1, datatime)
  # freq <- 'vests'
  # mywc <- function(freq) {
  #   if (nrow(fer) > 30) wcplot <- fer[order(fer[, freq]), ][nrow(fer): (nrow(fer) - 30), ]
  #   wordcloud(wcplot$Account, freq = wcplot[, freq], colors = brewer.pal(4, "Dark2"), scale = c(4, 0.6))
  #   text(0.5, 1.1, paste0(freq, ' Top 30'), cex = 2)
  # }
  # lapply(c("Followers", "Posts", "vests"), mywc)
  #
  # reminder <- list(notfollower = unique(fing$Account)[!finginfer][order(unique(fing$Account)[!finginfer])],
  #                  notfollowing = fer$Account[!ferinfing][order(fer$Account[!ferinfing])])
  # return(reminder)
}


#' Voter report based on steemdb.com
#'
#' @param post complete link of a post on steemit
#'
#' @return a dataframe of a post's voter information
#' @export
#'
#' @examples
#' vote()
vote <- function(post = 'https://steemit.com/cn/@dapeng/steemit-markdown'){
  y <- paste0(post, '/votes')
  y <- gsub('steemit.com', 'steemdb.com', y)
  theurl <- RCurl::getURL(y, .opts = list(ssl.verifypeer = FALSE) )
  votes <- XML::readHTMLTable(theurl)[[1]]
  votes$Voter <- as.character(votes$Voter)
  votes$percent <- as.numeric(gsub('%', '', votes[,'%']))
  votes$Weight <- substr(votes$Weight, 1, regexpr(' ', votes$Weight)-1)
  votes$'Reward Shares' <- substr(votes$'Reward Shares', 1, regexpr(' ', votes$'Reward Shares') - 1)
  # str(votes)
  return(votes[, -1])
}

#' Convert an id from a character to html hyperlink
#'
#' @param id character without '@'
#'
#' @return a character
#' @export
#'
#' @examples
#' idlink()
idlink <- function(id = 'dapeng') {
  paste0('<a href="https://steemit.com/@', id, '">@', id, '</a>')
}


#' Get the complete info of a given post on steemdb.com
#'
#' @param postlink character. the complete hyperlink of a post on steemdb.com
#' @param selected logic. Whether return only selected info
#' @param newline logic. Whether rbind the returned dataframe with an existing one
#' @param oldcolname character. If newline == TRUE, the returned dataframe is ordered accoring to oldcolname
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' post()
post <- function(postlink = 'https://steemdb.com/cn/@dapeng/steemit-markdown',
                 selected = FALSE,
                 newline = FALSE,
                 oldcolname) {
  postedit <- readLines(postlink, encoding = 'UTF-8')
  postedit <- postedit[grep('Edit History', postedit)]
  postedit <- strsplit(postedit, 'Edit History \\(')[[1]][2]
  postedit <- as.numeric(strsplit(postedit, '\\)')[[1]][1])

  postdatalink <- paste0(postlink, '/data')

  theurl <- RCurl::getURL(postdatalink, .opts = list(ssl.verifypeer = FALSE) )
  postdataraw <- XML::readHTMLTable(theurl, stringsAsFactors = FALSE, encoding = 'UTF-8')#[[1]]
  while (length(postdataraw) < 3){
    theurl <- RCurl::getURL(postdatalink, .opts = list(ssl.verifypeer = FALSE) )
    postdataraw <- XML::readHTMLTable(theurl, encoding = 'UTF-8')#[[1]]
  }
  # str(postdataraw)
  postdata <- postdataraw[[1]]
  postdatat <- as.data.frame(t(as.character(postdata$V2)))
  names(postdatat) <- postdata$V1
  if (newline == TRUE) {
    postdatat[, oldcolname[!(oldcolname %in% names(postdatat))]] <- NA
    postdatat <- postdatat[, oldcolname]
  }
  postdatat$edit <- postedit
  if (selected) return(postdatat[, c('category', 'json_metadata', 'net_votes', 'total_payout_value', 'created', 'active_votes', 'title', 'last_reply', 'edit')]) else return(postdatat)
}

#' Obtain an ID's post hyperlinks from steemdb.com
#'
#' @param id character without '@'
#' @param post_number numeric or NA. The number of the latest posts to be obtained. If NA, the 100 latest posts will be processed.
#'
#' @return character. an ID's post hyperlinks.
#' @export
#'
#' @examples
#' post_id()
postlink_id <- function(id = 'dapeng', post_number = 3) {
  url <-   paste0("https://steemdb.com/@", id,  "/posts")
  postpage <- readLines(url, encoding = 'UTF-8')
  postlinks <- postpage[grep('one wide mobile hidden column', postpage) + 8]
  postlinks <- sapply(strsplit(postlinks, '"'), function(x) x[2])
  if ((!is.na(post_number)) & (length(postlinks) > post_number)) postlinks <- postlinks[1:post_number]
  return(paste0('https://steemdb.com', postlinks))
}

#' Get the detailed information of given posts from steemdb.com
#'
#' @param postlinks character. hyperlinks to target posts
#'
#' @return a dataframe of the detailed information of the given posts
#' @export
#'
#' @examples
#' post_df()
post_df <- function(postlinks) {
  i <- 1
  postlink <- postlinks[i]
  print(paste(i, postlink))
  mypost <- post(postlink)
  oldcolname <- names(mypost)
  for (i in 2:length(postlinks)) {
    print(paste(i, postlink))
    postlink <- postlinks[i]
    newline <- post(postlink, newline = TRUE, oldcolname = oldcolname)
    # ncol(mypost)
    # ncol(newline)
    # names(newline)[!(names(newline) %in% names(mypost))]
    mypost <- rbind(mypost, newline)
  }
  # mypost <- data.frame(Reduce(rbind, lapply(postlinks, post)))
  return(mypost)
}

#' Obtain an ID's post detailed info from steemdb.com
#'
#' @param id character without '@'
#' @param post_number numeric or NA. The number of the latest posts to be obtained. If NA, all the posts will be processed.
#'
#' @return data frame. an ID's post detailed info.
#' @export
#'
#' @examples
#' post_id()
post_dfid <- function(id = 'dapeng', post_number = 3) {
  postlinks <- postlink_id(id = id, post_number = post_number)
  return(post_df(postlinks = postlinks))
}
