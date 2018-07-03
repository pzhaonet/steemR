#' Open Connections to the SteemSQL server. ssql means 'send to sql'.
#'
#' @param uid A character string of the SteemSQL ID.
#' @param pwd A character string of the SteemSQL password.
#'
#' @return A connection to the SeemSQL server.
#' @export
#'
#' @examples ssql()
ssql <- function(uid = NA,
                 pwd = NA){
  if(is.na(uid) | is.na(pwd)) {
    return(message('Please assign the id and/or the password.'))
  }
  sql_con <- paste0("Driver={SQL Server};",
                    "server=sql.steemsql.com;",
                    "database=DBSteem;",
                    "uid=", uid, ";",
                    "pwd=", pwd)
  cn <- RODBC::odbcDriverConnect(connection = sql_con)
  return(cn)
}

#' Get an ID's detailed info. gid means 'get an ID's information'.
#'
#' @param id A character string of a Steem ID without '@'.
#' @param sql_con A connection to the SteemSQL server.
#' @param method A character string of the Steem data server to connect.
#'
#' @return A list or a dataframe of an ID's detailed info
#' @export
#'
#' @examples
#' gid(id = 'dapeng')
gid <- function(id = NA,
                method = c('steemdb.com',
                           'steemsql.com',
                           'appbase_api',
                           'steemdata.com'),
                sql_con){
  if(is.na(id)) {
    return(message('Please give a valid id.'))
  }

  method <- match.arg(method)
  if (method == 'steemdb.com') {
    ulr <-   paste0("https://steemdb.com/@", id,  "/data")
    theurl <- RCurl::getURL(ulr, .opts = list(ssl.verifypeer = FALSE))
    info <- XML::readHTMLTable(theurl, stringsAsFactors = FALSE)[[1]]
    info_ls <- as.list(info[,2])
    names(info_ls) <- info[, 1]
    return(info_ls)
  } else if(method == 'steemsql.com') {
    sql_query <- paste0("SELECT *
                        FROM Accounts
                        WHERE name = '", id, "'")
    info_df <- RODBC::sqlQuery(channel = sql_con,
                               query = sql_query,
                               stringsAsFactors = FALSE)
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

#' Get a name list of an ID's followers and following. gfollow means 'get an ID's follower information'.
#' @param id A character string of a Steem ID without '@'.
#' @param method A character string of the Steem data server to connect.
#' @param sql_con A connection to the SteemSQL server.
#'
#' @return A name list of an ID's followers and following
#' @export
#'
#' @examples
#' gfollow('dapeng')
gfollow <- function(id = NA,
                    method = c('steemdb.com',
                               'steemsql.com',
                               'steemdata.com'),
                    sql_con){
  if(is.na(id)) {
    return(message('Please give a valid id.'))
  }
  method <- match.arg(method)

  if (method == 'steemdb.com') {
    info_ls <- gid(id = id, method = method)
    follow_ls <- list(followers = clearferfing(info_ls$followers),
                      following = clearferfing(info_ls$following))

  } else if(method == 'steemsql.com') {
    follower_query <- paste0("SELECT *
                             FROM Followers
                             WHERE following = '", id, "'")
    followers <- RODBC::sqlQuery(channel = sql_con,
                                 query = follower_query,
                                 stringsAsFactors = FALSE)[, 'follower']
    following_query <- paste0("SELECT *
                              FROM Followers
                              WHERE follower = '", id, "'")
    followings <- RODBC::sqlQuery(channel = sql_con,
                                  query = following_query,
                                  stringsAsFactors = FALSE)[, 'following']
    follow_ls <- list(followers = followers,
                      following = followings)

  } else if(method == 'steemdata.com') {
    myaccounts <- gid(id = id, method = method)
    follow_ls <- list(followers = myaccounts$followers[[1]],
                      following = myaccounts$following[[1]])
  }
  return(follow_ls)
}

#' The id list of an id's following  from steemdb.com. gfollowing means 'get the following information'.
#'
#' @param id A character string of a Steem ID without '@'.
#'
#' @return A dataframe of an ID's following info
#' @export
#'
#' @examples
#' gfollowing('dapeng')
gfollowing <- function(id = NA){
  if(is.na(id)) {
    return(message('Please give a valid id.'))
  }

  y <- paste0('https://steemdb.com/@', id, '/following')
  theurl <- RCurl::getURL(y,
                          .opts = list(ssl.verifypeer = FALSE) )
  fing <- XML::readHTMLTable(theurl,
                             stringsAsFactors = FALSE)[[1]]
  # following
  fing$nr <- ifelse(fing$What == 'Follow', 1, -1)
  # fingsum <- beginr::tapplydf(data = fing, 'nr', 'Account', sum)# tapply(fing$nr, fing$Account, sum)
  fing$Account <- as.character(fing$Account)
  return(fing)
}

#' A list of an id's followers from steemdb.com. gfollower means 'get the follower information'.
#'
#' @param id A character string of a Steem ID without '@'.
#'
#' @return character a dataframe of an ID's followers' info
#' @export
#'
#' @examples
#' gfollower('dapeng')
gfollower <- function(id = NA){
  if(is.na(id)) {
    return(message('Please give a valid id.'))
  }

  x <- paste0("https://steemdb.com/@", id,  "/followers/whales")
  theurl <- RCurl::getURL(x,
                          .opts = list(ssl.verifypeer = FALSE) )
  if (theurl == '') return(data.frame())
  fer <- XML::readHTMLTable(theurl,
                            stringsAsFactors = FALSE)[[1]]
  fer$Account <- as.character(fer$Account)
  fer$Followers <- as.numeric(as.character(fer$Followers))
  fer$Posts <- as.numeric(as.character(fer$Posts))
  fer$Vests <- as.character(fer$Vests)
  fer$Account <- substr(fer$Account,
                        regexpr('\n', fer$Account)[1] + 7,
                        nchar(fer$Account))
  fer$unit <- substr(fer$Vests,
                     nchar(fer$Vests) - 1,
                     nchar(fer$Vests))
  fer$vests <- substr(fer$Vests,
                      1,
                      nchar(fer$Vests) - 3)
  fer$multi <- sapply(fer$unit, unitconvert)
  fer$vests <- as.numeric(fer$vests) * fer$multi
  return(fer)
}


#' A post's vote report based on steemdb.com. gvotep means 'get the vote information of a post.'
#'
#' @param post A character string of the complete link of a post on steemit
#'
#' @return A dataframe of a post's voter information
#' @export
#'
#' @examples
#' gvotep('cn/@dapeng/steemit-markdown')
gvotep <- function(postlink = NA){
  if(is.na(postlink)) {
    return(message('Please give a valid link.'))
  }

  y <- paste0('https://steemdb.com/', postlink, '/votes')
  theurl <- RCurl::getURL(y,
                          .opts = list(ssl.verifypeer = FALSE) )
  votes <- XML::readHTMLTable(theurl)[[1]]
  if(is.null(votes)) return(NULL)
  votes$Voter <- as.character(votes$Voter)
  votes$percent <- as.numeric(gsub('%', '', votes[,'%']))
  votes$Weight <- substr(votes$Weight, 1, regexpr(' ', votes$Weight)-1)
  votes$'Reward Shares' <- substr(votes$'Reward Shares',
                                  1,
                                  regexpr(' ', votes$'Reward Shares') - 1)
  return(votes[, -1])
}

#' Get the vote information of given IDs from SteemSQL. gvoter means 'get the voter activities.'
#'
#' @param voters A character vector of given Steem IDs.
#' @param from A character string of the starting date. in 'Y-m-d' format.
#' @param to A character string of the ending date. in 'Y-m-d' format.
#' @param select A character vector of the selected columns in the SteemSQL query.
#' @param sql_con A connection to the SteemSQL server.
#' @param if_plot A logic value of whether plot the daily votes.
#'
#' @return A list (and a diagram) of the voter report.
#' @export
#'
#' @examples gvoter(voters = NA)
gvoter <- function(voters = NA,
                   from = Sys.Date() - 7,
                   to = Sys.Date(),
                   select = '*',
                   sql_con = NA,
                   if_plot = FALSE){
  if (NA %in% voters) return(message('Invalid voters.'))
  from <- as.Date(from)
  to <- as.Date(to)
  select <- paste(select, collapse = ', ')
  voters <- paste(voters, collapse = "', '")
  sql_query <- paste0("SELECT ", select,
                      " FROM TxVotes
                      WHERE voter IN ('", voters, "')
                      AND timestamp BETWEEN '",
                      format(from, '%Y/%m/%d'), "' and '",
                      format(to, '%Y/%m/%d'), "'")
  voter_df <- RODBC::sqlQuery(channel = sql_con,
                              query = sql_query,
                              stringsAsFactors = FALSE)
  if(if_plot){
    voter_df$date <- as.Date(voter_df$timestamp)
    votes <- voter_df[order(voter_df$voter, voter_df$date),]
    # Calculate the daily data.
    votes_daily <- as.data.frame(table(votes$date, votes$voter))
    names(votes_daily) <- c('date', 'voter', 'n')
    votes_daily$date <- as.Date(as.character(votes_daily$date))
    votes_n <- as.data.frame(table(votes$voter))
    names(votes_n) <- c('voter', 'n')
    votes_n$daily <- round(votes_n$n / as.numeric(diff(c(from, to))), 2)
    votes_n$n_authors <- tapply(votes$author,
                                votes$voter,
                                function(x) length(unique(x)))
    votes_n$weight <- tapply(votes$weight, votes$voter, mean) / 100
    voter_plot <- ggplot2::qplot(x = date, y = n,
                                 col = voter,
                                 data = votes_daily,
                                 geom =  "line",
                                 xlab = 'Date', ylab = 'Daily Votes')
    voter_ls <- list(voter = voter_df,
                     voter_summary = votes_n,
                     voter_plot = voter_plot)
  } else {
    voter_ls <- voter_df
  }
  return(voter_ls)
}


#' Get the complete info of a single given post. gpost means 'get a post'.
#'
#' @param postlink A character of the link to a post
#' @param method A character string of the Steem data server to connect.
#' @param sql_con A connection to the SteemSQL server.
#' @param selected A logic value of whether return only selected info
#' @param newline A logic value of whether rbind the returned dataframe with an existing one
#' @param oldcolname A character string. If newline == TRUE, the returned dataframe is ordered accoring to oldcolname
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' gpost('cn/@dapeng/steemit-markdown')
gpost <- function(postlink = NA,
                  method = c('steemdb.com',
                             'steemsql.com'),
                  sql_con,
                  selected = FALSE,
                  newline = FALSE,
                  oldcolname) {
  if(is.na(postlink)) {
    return(message('Please give a valid link.'))
  }

  method <- match.arg(method)

  if (method == 'steemdb.com') {
    postedit <- readLines(paste0('https://steemdb.com/', postlink),
                          encoding = 'UTF-8')
    postedit <- postedit[grep('Edit History', postedit)]
    postedit <- strsplit(postedit, 'Edit History \\(')[[1]][2]
    postedit <- as.numeric(strsplit(postedit, '\\)')[[1]][1])
    postdatalink <- paste0('https://steemdb.com/', postlink, '/data')
    theurl <- RCurl::getURL(postdatalink,
                            .opts = list(ssl.verifypeer = FALSE) )
    postdataraw <- XML::readHTMLTable(theurl,
                                      stringsAsFactors = FALSE,
                                      encoding = 'UTF-8')#[[1]]
    while (length(postdataraw) < 3){
      theurl <- RCurl::getURL(postdatalink,
                              .opts = list(ssl.verifypeer = FALSE) )
      postdataraw <- XML::readHTMLTable(theurl,
                                        encoding = 'UTF-8')#[[1]]
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
    post_author <- gsub(pattern = '@',
                        '',
                        strsplit(postlink, '/')[[1]][2])
    sql_query <- paste0("SELECT *
                        FROM Comments
                        WHERE author = '", post_author,
                        "' AND url = '/", postlink,
                        "' AND depth = 0")
    postdatat <- RODBC::sqlQuery(channel = sql_con,
                                 query = sql_query,
                                 stringsAsFactors = FALSE)
    return(postdatat)
  }
}

#' Obtain an ID's post hyperlinks. gidpostl means 'get an ID's post links'.
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
#' gidpostl('dapeng')
gidpostl <- function(id = NA,
                     method = c('steemdb.com',
                                'steemsql.com',
                                'steemdata.com'),
                     sql_con,
                     post_number = 3,
                     site = 'steemit.com') {
  if(is.na(id)) {
    return(message('Please give a valid id.'))
  }

  method <- match.arg(method)

  if (method == 'steemdb.com') {
    url <-   paste0("https://steemdb.com/@", id,  "/posts")
    postpage <- readLines(url, encoding = 'UTF-8')
    postlinks <- postpage[grep('one wide mobile hidden column', postpage) + 8]
    postlinks <- sapply(strsplit(postlinks, '"'), function(x) x[2])
    if ((!is.na(post_number)) & (length(postlinks) > post_number)){
      postlinks <- postlinks[1:post_number]
    }
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
    postlinks <- RODBC::sqlQuery(channel = sql_con,
                                 query = post_query,
                                 stringsAsFactors = FALSE)
    return(paste0('https://', site, unlist(postlinks)))
  }
}

#' Get the detailed information of given posts. gposts means 'get posts'.
#'
#' @param postlinks A character string of hyperlinks to target posts
#' @param method A character string of the Steem data server to connect.
#' @param sql_con A connection to the SteemSQL server.
#'
#' @return a dataframe of the detailed information of the given posts
#' @export
#'
#' @examples
#' gposts(c('cn/@dapeng/xuer-sale', 'utopian-io/@dapeng/steemg-four-more'))
gposts <- function(postlinks = NA,
                   method = c('steemdb.com',
                              'steemsql.com',
                              'steemdata.com'),
                   sql_con) {
  if(NA %in% postlinks) {
    return(message('Please give valid links.'))
  }

  method <- match.arg(method)

  if (method == 'steemdb.com') {
    i <- 1
    postlink <- postlinks[i]
    print(paste(i, postlink))
    mypost <- gpost(postlink)
    oldcolname <- names(mypost)
    for (i in 2:length(postlinks)) {
      postlink <- postlinks[i]
      print(paste(i, postlink))
      postlink <- postlinks[i]
      newline <- gpost(postlink, newline = TRUE,
                       oldcolname = oldcolname)
      mypost <- rbind(mypost, newline)
    }
    # mypost <- data.frame(Reduce(rbind, lapply(postlinks, post)))

  } else if(method == 'steemsql.com') {
    mypost_ls <- lapply(postlinks,
                        function(x)
                          gpost(postlink = x,
                                method = 'steemsql.com',
                                sql_con = sql_con))
    mypost_colname <- names(mypost_ls[[1]])
    mypost <- do.call(rbind.data.frame, mypost_ls)
  }
  return(mypost)
}

#' Obtain an ID's post detailed info from steemdb.com. 'gidposts' means 'get an ID's posts'.
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
#' gidposts('dapeng')
gidposts <- function(id = NA,
                     method = c('steemdb.com',
                                'steemsql.com',
                                'appbase_api',
                                'steemdata.com'),
                     sql_con,
                     post_number = NA) {
  if(is.na(id)) {
    return(message('Please give a valid id.'))
  }

  method <- match.arg(method)

  if (method == 'steemdb.com') {
    postlinks <- gidpostl(id = id,
                          post_number = post_number,
                          site = 'steemdb.com')
    postlinks <- gsub('https://steemdb.com/', '', postlinks)
    mypost_id <- gposts(postlinks = postlinks)

  } else if(method == 'steemsql.com') {
    post_query <- paste0("SELECT *
                         FROM Comments
                         WHERE
                         author = '", id, "'
                         AND
                         depth = 0
                         ORDER by
                         created")
    mypost_id <- RODBC::sqlQuery(channel = sql_con,
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

#' Rename the getBlog() function from the steemr2 package
#'
#' @param id A Steem ID
#'
#' @return A dataframe with the ID's posts
#' @export
#'
#' @examples gblog('dapeng')
gblog <- function(id){
  steemr2::getBlog(id)
}

#' Get the Steem account information within a period from SteemSQL
#' gaccounts means 'get account information'.
#'
#' @param from A Date object or character in '2017-10-24' format of the beginning of the period
#' @param to A Date object or character in '2017-10-24' format of the end of the period
#' @param select A character string vector of the column names
#' @param if_plot A logic value of whether plot the time series
#' @param sql_con A SQL connection
#'
#' @return A data frame of the account information with a figure
#' @export
#'
#' @examples gaccounts()
gaccounts <- function(from = Sys.Date() - 7,
                      to = Sys.Date(),
                      select = c('name', 'created', 'post_count', 'last_post'),
                      sql_con,
                      ylab = 'Daily New Accounts',
                      if_plot = FALSE){
  from <- as.Date(from)
  to <- as.Date(to)
  select <- paste(select, collapse = ', ')
  sql_query <- paste0("SELECT ", select,
                      " FROM Accounts
                      WHERE created BETWEEN '",
                      format(from, '%Y/%m/%d'), "' and '",
                      format(to, '%Y/%m/%d'), "'")
  accounts_df <- RODBC::sqlQuery(channel = sql_con,
                                 query = sql_query,
                                 stringsAsFactors = FALSE)
  accounts_df$date <- as.Date(accounts_df$created)
  accounts_daily <- adailyf(mydata = accounts_df,
                            datecol = 'date',
                            ylab = ylab,
                            if_plot = if_plot)
  accounts_ls <- list(accounts = accounts_df, daily = accounts_daily)
  return(accounts_ls)
}



#' Get the comment records of an Steem ID within a period from SteemSQL
#' gcomments means 'get comment information'.
#'
#' @param id A character string of a Steem ID
#' @param from A Date object or character in '2017-10-24' format of the beginning of the period
#' @param to A Date object or character in '2017-10-24' format of the end of the period
#' @param select A character string vector of the column names
#' @param sql_con A SQL connection
#' @param if_plot A logic value of whether plot the time series
#'
#' @return A data frame of the comment information with a figure
#' @export
#'
#' @examples gcomments()
gcomments <- function(id = NA,
                      from = Sys.Date() - 7,
                      to = Sys.Date(),
                      select = c('root_title', 'root_comment', 'created', 'body'),
                      sql_con,
                      ylab = 'Daily Comments',
                      if_plot = FALSE){
  if(is.na(id)) {
    return(message('Please give a valid id.'))
  }

  from <- as.Date(from)
  to <- as.Date(to)
  select <- paste(select, collapse = ', ')
  sql_query <- paste0("SELECT ", select,
                      " FROM Comments
                      WHERE author = '", id, "' AND depth > 0 AND
                      created BETWEEN '",
                      format(from, '%Y/%m/%d'),
                      "' and '", format(to, '%Y/%m/%d'), "'")
  cmt <- sqlQuery(channel = sql_con,
                  query = sql_query,
                  stringsAsFactors = FALSE)
  cmt$date <- as.Date(cmt$created)
  cmt_daily <- adailyf(mydata = cmt,
                       datecol = 'date',
                       if_plot = if_plot,
                       ylab = ylab)
  cmt_ls <- list(comment = cmt, daily = cmt_daily)
  return(cmt_ls)
}

