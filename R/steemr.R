#' Open Connections to the SteemSQL server
#'
#' @param sql_id A character string of the SteemSQL ID.
#' @param sql_password A character string of the SteemSQL password.
#'
#' @return A connection to the SeemSQL server
#' @export
#'
#' @examples steemsql_connection()
steemsql_connection <- function(sql_id = NA,
                                sql_password = NA){
  sql_connection <- paste0("Driver={SQL Server};server=sql.steemsql.com;database=DBSteem;uid=",
                           sql_id, ";pwd=", sql_password)
  cn <- RODBC::odbcDriverConnect(connection = sql_connection)
  return(cn)
}

#' Get an ID's detailed info
#'
#' @param id A character string of a Steem ID without '@'.
#' @param steemsql_connection A connection to the SteemSQL server.
#' @param method A character string of the Steem data server to connect.
#'
#' @return A list or a dataframe of an ID's detailed info
#' @export
#'
#' @examples
#' id_info(id = 'dapeng')
id_info <- function(id = NA,
                    method = c('steemdb.com', 'steemsql.com', 'appbase_api', 'steemdata.com'),
                    steemsql_connection){
  method <- match.arg(method)
  if (method == 'steemdb.com') {
    ulr <-   paste0("https://steemdb.com/@", id,  "/data")
    theurl <- RCurl::getURL(ulr, .opts = list(ssl.verifypeer = FALSE))
    info <- XML::readHTMLTable(theurl, stringsAsFactors = FALSE)[[1]]
    info_ls <- as.list(info[,2])
    names(info_ls) <- info[, 1]
    return(info_ls)
  } else if(method == 'steemsql.com') {
    sql_query <- paste0("SELECT * FROM Accounts WHERE name = '", id, "'")
    info_df <- RODBC::sqlQuery(steemsql_connection, sql_query, stringsAsFactors = FALSE)
    return(info_df)
  } else if(method == 'appbase_api') {
    info_df <- steemr2::getAccount(username = id)
    return(info_df)
  } else if(method == 'steemdata.com') {
    myurl <- "mongodb://steemit:steemit@mongo1.steemdata.com:27017/SteemData"
    accounts <- mongolite::mongo(collection = "Accounts", url = myurl)
    info_df <- accounts$find(paste0('{"name" : "', id, '"}'))
    return(info_df)
  }
}

#' Get a name list of an ID's followers and following
#' @param id A character string of a Steem ID without '@'.
#' @param method A character string of the Steem data server to connect.
#' @param steemsql_connection A connection to the SteemSQL server.
#'
#' @return A name list of an ID's followers and following
#' @export
#'
#' @examples
#' follower('dapeng')
follower <- function(id = NA,
                     method = c('steemdb.com', 'steemsql.com', 'steemdata.com'),
                     steemsql_connection){
  method <- match.arg(method)
  # from steemdb.com
  if (method == 'steemdb.com') {
    clearferfing <- function(x){
      y <- gsub('\\"','', gsub('\n', '', gsub(' ', '', x)))
      y <- substr(y, 2, nchar(y) - 1)
      strsplit(y, ',')[[1]]
    }
    info_ls <- id_info(id = id, method = method)
    follow_ls <- list(followers = clearferfing(info_ls$followers),
                      following = clearferfing(info_ls$following))
  } else if(method == 'steemsql.com') {
    follower_query <- paste0("SELECT * FROM Followers WHERE following = '",
                             id, "'")
    followers <- RODBC::sqlQuery(channel = steemsql_connection,
                                query = follower_query,
                                stringsAsFactors = FALSE)[, 'follower']
    following_query <- paste0("SELECT * FROM Followers WHERE follower = '",
                              id, "'")
    followings <- RODBC::sqlQuery(channel = steemsql_connection,
                                query = following_query,
                                stringsAsFactors = FALSE)[, 'following']
    follow_ls <- list(followers = followers,
                      following = followings)
  } else if(method == 'steemdata.com') {
  # from steemdata.com
  myaccounts <- id_info(id = id, method = method)
  follow_ls <- list(followers = myaccounts$followers[[1]],
                    following = myaccounts$following[[1]])
  }
  return(follow_ls)
}

#' The id list of an id's following  from steemdb.com
#'
#' @param id A character string of a Steem ID without '@'.
#'
#' @return A dataframe of an ID's following info
#' @export
#'
#' @examples
#' following('dapeng')
following <- function(id = NA){
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
#' @param id A character string of a Steem ID without '@'.
#'
#' @return character a dataframe of an ID's followers' info
#' @export
#'
#' @examples
#' follower_df('dapeng')
follower_df <- function(id = NA){
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


#' A post's vote report based on steemdb.com
#'
#' @param post A character string of the complete link of a post on steemit
#'
#' @return A dataframe of a post's voter information
#' @export
#'
#' @examples
#' vote('cn/@dapeng/steemit-markdown')
vote <- function(postlink = NA){
  y <- paste0('https://steemdb.com/', postlink, '/votes')
  theurl <- RCurl::getURL(y, .opts = list(ssl.verifypeer = FALSE) )
  votes <- XML::readHTMLTable(theurl)[[1]]
  votes$Voter <- as.character(votes$Voter)
  votes$percent <- as.numeric(gsub('%', '', votes[,'%']))
  votes$Weight <- substr(votes$Weight, 1, regexpr(' ', votes$Weight)-1)
  votes$'Reward Shares' <- substr(votes$'Reward Shares', 1, regexpr(' ', votes$'Reward Shares') - 1)
  return(votes[, -1])
}

#' Find which followers have not voted a post yet
#'
#' @param postlink A character string of the link to a target post.
#'
#' @return A character vector of the name list of the followers who have not voted the target post yet.
#' @export
#'
#' @examples
#' who_not_vote('cn/@dapeng/steemit-markdown')
who_not_vote <- function(postlink = NA){
  vr_votes <- vote(post = postlink)$Voter
  postlink <- paste0('https://steemdb.com/', postlink)
  vr_id <- substr(strsplit(postlink, '/')[[1]][5],
                  2, nchar(strsplit(postlink, '/')[[1]][5]))
  vr_follower <- unique(follower(id = vr_id)$followers)
  vr_follower <- vr_follower[!(vr_follower %in% vr_votes)]
  return(vr_follower[order(vr_follower)])
}

#' Get the vote information of given IDs from SteemSQL
#'
#' @param voters A character vector of given Steem IDs
#' @param from A character string of the starting date. in 'Y-m-d' format.
#' @param to A character string of the ending date. in 'Y-m-d' format.
#' @param select A character vector of the selected columns in the SteemSQL query.
#' @param steemsql_connection A connection to the SteemSQL server.
#' @param if_plot_daily A logic value of whether plot the daily votes.
#'
#' @return A list (and a diagram) of the voter report.
#' @export
#'
#' @examples voter(voters = NA)
voter <- function(voters = NA,
                  from = Sys.Date() - 7, # in '2017-10-24' format
                  to = Sys.Date(),
                  select = '*',
                  steemsql_connection = NA,
                  if_plot_daily = FALSE){
  if (NA %in% voters) return(message('Invalid voters.'))
  from <- as.Date(from)
  to <- as.Date(to)
  select <- paste(select, collapse = ', ')
  voters <- paste(voters, collapse = "', '")
  sql_query <- paste0("SELECT ", select,
                      " FROM TxVotes WHERE voter IN ('", voters,
                      "') AND timestamp BETWEEN '",
                      format(from, '%Y/%m/%d'), "' and '",
                      format(to, '%Y/%m/%d'), "'")
  voter_df <- RODBC::sqlQuery(channel = steemsql_connection,
                              query = sql_query,
                              stringsAsFactors = FALSE)
  if(if_plot_daily){
    voter_df$date <- as.Date(voter_df$timestamp)
    votes <- voter_df[order(voter_df$voter, voter_df$date),]
    votes_daily <- as.data.frame(table(votes$date, votes$voter))
    names(votes_daily) <- c('date', 'voter', 'n')
    votes_daily$date <- as.Date(as.character(votes_daily$date))
    votes_n <- as.data.frame(table(votes$voter))
    names(votes_n) <- c('voter', 'n')
    votes_n$daily <- round(votes_n$n / as.numeric(diff(c(from, to))), 2)
    votes_n$n_authors <- tapply(votes$author, votes$voter, function(x) length(unique(x)))
    votes_n$weight <- tapply(votes$weight, votes$voter, mean) / 100
    voter_plot <- ggplot2::qplot(x = date, y = n,
                                 col = voter, data = votes_daily,
                                 geom =  "line",
                                 xlab = 'Date', ylab = 'Daily Votes')
    voter_ls <- list(voter = voter_df, voter_summary = votes_n, voter_plot = voter_plot)
  } else {
    voter_ls <- list(voter = voter_df)
  }
  return(voter_ls)
}

#' Convert an id from a character to html hyperlink
#'
#' @param id A character string of a Steem ID without '@'.
#'
#' @return A character string of the hyperlink to the Steem id.
#' @export
#'
#' @examples
#' idlink('dapeng')
idlink <- function(id = NA) {
  paste0('<a href="https://steemit.com/@', id, '">@', id, '</a>')
}


#' Get the complete info of a single given post on steemdb.com
#'
#' @param postlink A character of the link to a post
#' @param method A character string of the Steem data server to connect.
#' @param steemsql_connection A connection to the SteemSQL server.
#' @param selected A logic value of whether return only selected info
#' @param newline A logic value of whether rbind the returned dataframe with an existing one
#' @param oldcolname A character string. If newline == TRUE, the returned dataframe is ordered accoring to oldcolname
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' post_info('cn/@dapeng/steemit-markdown')
post_info <- function(postlink = NA,
                      method = c('steemdb.com', 'steemsql.com', 'steemdata.com'),
                      steemsql_connection,
                      selected = FALSE,
                      newline = FALSE,
                      oldcolname) {
  method <- match.arg(method)
  if (method == 'steemdb.com') {
    postedit <- readLines(paste0('https://steemdb.com/', postlink), encoding = 'UTF-8')
    postedit <- postedit[grep('Edit History', postedit)]
    postedit <- strsplit(postedit, 'Edit History \\(')[[1]][2]
    postedit <- as.numeric(strsplit(postedit, '\\)')[[1]][1])

    postdatalink <- paste0('https://steemdb.com/', postlink, '/data')

    theurl <- RCurl::getURL(postdatalink, .opts = list(ssl.verifypeer = FALSE) )
    postdataraw <- XML::readHTMLTable(theurl, stringsAsFactors = FALSE, encoding = 'UTF-8')#[[1]]
    while (length(postdataraw) < 3){
      theurl <- RCurl::getURL(postdatalink, .opts = list(ssl.verifypeer = FALSE) )
      postdataraw <- XML::readHTMLTable(theurl, encoding = 'UTF-8')#[[1]]
    }
    postdata <- postdataraw[[1]]
    postdatat <- as.data.frame(t(as.character(postdata$V2)))
    names(postdatat) <- postdata$V1
    if (newline == TRUE) {
      postdatat[, oldcolname[!(oldcolname %in% names(postdatat))]] <- NA
      postdatat <- postdatat[, oldcolname]
    }
    postdatat$edit <- postedit
    if (selected) {
      return(postdatat[, c('category',
                           'json_metadata',
                           'net_votes',
                           'total_payout_value',
                           'created',
                           'active_votes',
                           'title',
                           'last_reply',
                           'edit')])
    }
    else {
      return(postdatat)
    }
  } else if(method == 'steemsql.com') {
    post_author <- gsub(pattern = '@', '', strsplit(postlink, '/')[[1]][2])
    sql_query <- paste0("SELECT * FROM Comments WHERE author = '", post_author,
                        "' AND url = '/", postlink,
                        "' AND depth = 0")
    postdatat <- sqlQuery(steemsql_connection, sql_query, stringsAsFactors = FALSE)
    # get tags. not done yet.
    tags <- lapply(postdatat$json_metadata, function(x) {
        if (sum(nchar(x) == 0)) return(NA)
        y <- rjson::fromJSON(json_str = x)
          return(y$tags)
      })
    return(postdatat)
  }
}

#' Obtain an ID's post hyperlinks
#'
#' @param id A character string of a Steem ID without '@'.
#' @param method A character string of the Steem data server to connect.
#' @param steemsql_connection A connection to the SteemSQL server.
#' @param post_number A numeric value or NA. The number of the latest posts to be obtained. If NA, the 100 latest posts will be processed.
#' @param site A character string of the site of the steem web UI
#'
#' @return A character string vector of an ID's post hyperlinks.
#' @export
#'
#' @examples
#' post_links('dapeng')
post_links <- function(id = NA,
                       method = c('steemdb.com', 'steemsql.com', 'steemdata.com'),
                       steemsql_connection,
                       post_number = 3,
                       site = 'steemit.com') {
  method <- match.arg(method)
  # from steemdb.com
  if (method == 'steemdb.com') {
    url <-   paste0("https://steemdb.com/@", id,  "/posts")
  postpage <- readLines(url, encoding = 'UTF-8')
  postlinks <- postpage[grep('one wide mobile hidden column', postpage) + 8]
  postlinks <- sapply(strsplit(postlinks, '"'), function(x) x[2])
  if ((!is.na(post_number)) & (length(postlinks) > post_number))
    postlinks <- postlinks[1:post_number]
  return(paste0('https://', site, postlinks))
  } else if(method == 'steemsql.com') {
    post_query <- paste0("SELECT url
                            FROM Comments
                            WHERE
                              author = '", id, "'
                              AND
                              depth = 0
                            ORDER by
                              created")
    postlinks <- RODBC::sqlQuery(channel = steemsql_connection,
                                 query = post_query,
                                 stringsAsFactors = FALSE)
    return(paste0('https://', site, unlist(postlinks)))
  }
}

#' Get the detailed information of given posts
#'
#' @param postlinks A character string of hyperlinks to target posts
#' @param method A character string of the Steem data server to connect.
#' @param steemsql_connection A connection to the SteemSQL server.
#'
#' @return a dataframe of the detailed information of the given posts
#' @export
#'
#' @examples
#' post_df(c('cn/@dapeng/xuer-sale', 'utopian-io/@dapeng/steemg-four-more'))
post_df <- function(postlinks = NA,
                    method = c('steemdb.com', 'steemsql.com', 'steemdata.com'),
                    steemsql_connection) {
  method <- match.arg(method)
  # from steemdb.com
  if (method == 'steemdb.com') {
    i <- 1
  postlink <- postlinks[i]
  print(paste(i, postlink))
  mypost <- post_info(postlink)
  oldcolname <- names(mypost)
  for (i in 2:length(postlinks)) {
    print(paste(i, postlink))
    postlink <- postlinks[i]
    newline <- post_info(postlink, newline = TRUE, oldcolname = oldcolname)
    mypost <- rbind(mypost, newline)
  }
  # mypost <- data.frame(Reduce(rbind, lapply(postlinks, post)))
  } else if(method == 'steemsql.com') {
    mypost_ls <- lapply(postlinks,
                     function(x)
                       post_info(postlink = x,
                                 method = 'steemsql.com',
                                 sql_id = sql_id,
                                 sql_password = sql_password))
    mypost_colname <- names(mypost_ls[[1]])
    mypost_df <- do.call(rbind.data.frame, mypost_ls)
  }
  return(mypost)
}

#' Obtain an ID's post detailed info from steemdb.com
#'
#' @param id A character string of a Steem ID without '@'.
#' @param method A character string of the Steem data server to connect.
#' @param steemsql_connection A connection to the SteemSQL server.
#' @param post_number A numeric value or NA. The number of the latest posts to be obtained. If NA, all the posts will be processed.
#'
#' @return A data frame of an ID's post detailed info.
#' @export
#'
#' @examples
#' post_id('dapeng')
post_id <- function(id = NA,
                    method = c('steemdb.com', 'steemsql.com', 'appbase_api', 'steemdata.com'),
                    steemsql_connection,
                    post_number = NA) {
  method <- match.arg(method)
  # from steemdb.com
  if (method == 'steemdb.com') {
    postlinks <- post_links(id = id, post_number = post_number, site = 'steemdb.com')
  postlinks <- gsub('https://steemdb.com/', '', postlinks)
  mypost_id <- post_df(postlinks = postlinks)
  } else if(method == 'steemsql.com') {
    # from steemsql.com
    post_query <- paste0("SELECT *
                         FROM Comments
                         WHERE
                         author = '", id, "'
                         AND
                         depth = 0
                         ORDER by
                         created")
    mypost_id <- RODBC::sqlQuery(channel = steemsql_connection,
                                 query = post_query,
                                 stringsAsFactors = FALSE)
  } else if(method == 'appbase_api') {
    mypost_id <- steemr2::getBlog(username = id)
  } else if(method == 'steemdata.com') {
    myurl <- "mongodb://steemit:steemit@mongo1.steemdata.com:27017/SteemData"
    posts <- mongolite::mongo(collection = "Posts", url = myurl)
    mypost_id <- posts$find(paste0('{"author" : "', id, '"}'))
  }
  return(mypost_id)
}
