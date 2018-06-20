#' Get an ID's detailed info
#'
#' @param id A character string of a Steem ID without '@'.
#' @param method A character string of the Steem data server to connect.
#' @param sql_id A character string of the steemsql id. Only valid when `method == 'steemsql'`
#' @param sql_password A character string of the steemsql passwordid. Only valid when `method == 'steemsql'`
#'
#' @return A list or a dataframe of an ID's detailed info
#' @export
#'
#' @examples
#' id_info()
id_info <- function(id = 'dapeng',
                    method = c('steemdb.com', 'steemsql.com', 'appbase_api', 'steemdata.com'),
                    sql_id,
                    sql_password){
  method <- match.arg(method)
  if (method == 'steemdb.com') {
    ulr <-   paste0("https://steemdb.com/@", id,  "/data")
    theurl <- RCurl::getURL(ulr, .opts = list(ssl.verifypeer = FALSE))
    info <- XML::readHTMLTable(theurl, stringsAsFactors = FALSE)[[1]]
    info_ls <- as.list(info[,2])
    names(info_ls) <- info[, 1]
    return(info_ls)
  } else if(method == 'steemsql.com') {
    sql_connection <- paste0("Driver={SQL Server};server=sql.steemsql.com;database=DBSteem;uid=",
                             sql_id, ";pwd=", sql_password)
    cn <- RODBC::odbcDriverConnect(connection = sql_connection)
    sql_query <- paste0("SELECT * FROM Accounts WHERE name = '", id, "'")
    info_df <- RODBC::sqlQuery(cn, sql_query, stringsAsFactors = FALSE)
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
#' @param sql_id A character string of the steemsql id. Only valid when `method == 'steemsql'`
#' @param sql_password A character string of the steemsql passwordid. Only valid when `method == 'steemsql'`
#'
#' @return A name list of an ID's followers and following
#' @export
#'
#' @examples
#' follower()
follower <- function(id = 'dapeng',
                     method = c('steemdb.com', 'steemsql.com', 'steemdata.com'),
                     sql_id,
                     sql_password){
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
    sql_connection <- paste0("Driver={SQL Server};server=sql.steemsql.com;database=DBSteem;uid=",
                             sql_id, ";pwd=", sql_password)
    cn <- RODBC::odbcDriverConnect(connection = sql_connection)
    follower_query <- paste0("SELECT * FROM Followers WHERE following = '",
                             id, "'")
    followers <- RODBC::sqlQuery(channel = cn,
                                query = follower_query,
                                stringsAsFactors = FALSE)[, 'follower']
    following_query <- paste0("SELECT * FROM Followers WHERE follower = '",
                              id, "'")
    followings <- RODBC::sqlQuery(channel = cn,
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
#' @param id A character string of a Steem ID without '@'.
#'
#' @return character a dataframe of an ID's followers' info
#' @export
#'
#' @examples
#' follower_df()
follower_df <- function(id = 'dapeng'){
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
  return(votes[, -1])
}

#' Convert an id from a character to html hyperlink
#'
#' @param id A character string of a Steem ID without '@'.
#'
#' @return A character string of the hyperlink to the Steem id.
#' @export
#'
#' @examples
#' idlink()
idlink <- function(id = 'dapeng') {
  paste0('<a href="https://steemit.com/@', id, '">@', id, '</a>')
}


#' Get the complete info of a single given post on steemdb.com
#'
#' @param postlink A character of the link to a post
#' @param method A character string of the Steem data server to connect.
#' @param sql_id A character string of the steemsql id. Only valid when `method == 'steemsql'`
#' @param sql_password A character string of the steemsql passwordid. Only valid when `method == 'steemsql'`
#' @param selected A logic value of whether return only selected info
#' @param newline A logic value of whether rbind the returned dataframe with an existing one
#' @param oldcolname A character string. If newline == TRUE, the returned dataframe is ordered accoring to oldcolname
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' post_info()
post_info <- function(postlink = 'cn/@dapeng/steemit-markdown',
                      method = c('steemdb.com', 'steemsql.com', 'steemdata.com'),
                      sql_id,
                      sql_password,
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
    sql_connection <- paste0("Driver={SQL Server};server=sql.steemsql.com;database=DBSteem;uid=",
                             sql_id, ";pwd=", sql_password)
    cn <- RODBC::odbcDriverConnect(connection = sql_connection)
    sql_query <- paste0("SELECT * FROM Comments WHERE author = '", post_author,
                        "' AND url = '/", postlink,
                        "' AND depth = 0")
    postdatat <- sqlQuery(cn, sql_query, stringsAsFactors = FALSE)
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
#' @param sql_id A character string of the steemsql id. Only valid when `method == 'steemsql'`
#' @param sql_password A character string of the steemsql passwordid. Only valid when `method == 'steemsql'`
#' @param post_number A numeric value or NA. The number of the latest posts to be obtained. If NA, the 100 latest posts will be processed.
#' @param site A character string of the site of the steem web UI
#'
#' @return A character string vector of an ID's post hyperlinks.
#' @export
#'
#' @examples
#' post_links()
post_links <- function(id = 'dapeng',
                       method = c('steemdb.com', 'steemsql.com', 'steemdata.com'),
                       sql_id,
                       sql_password,
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
    sql_connection <- paste0("Driver={SQL Server};server=sql.steemsql.com;database=DBSteem;uid=",
                             sql_id, ";pwd=", sql_password)
    cn <- RODBC::odbcDriverConnect(connection = sql_connection)
    post_query <- paste0("SELECT url
                            FROM Comments
                            WHERE
                              author = '", id, "'
                              AND
                              depth = 0
                            ORDER by
                              created")
    postlinks <- RODBC::sqlQuery(channel = cn,
                                 query = post_query,
                                 stringsAsFactors = FALSE)
    return(paste0('https://', site, unlist(postlinks)))
  }
}

#' Get the detailed information of given posts
#'
#' @param postlinks A character string of hyperlinks to target posts
#' @param method A character string of the Steem data server to connect.
#' @param sql_id A character string of the steemsql id. Only valid when `method == 'steemsql'`
#' @param sql_password A character string of the steemsql passwordid. Only valid when `method == 'steemsql'`
#'
#' @return a dataframe of the detailed information of the given posts
#' @export
#'
#' @examples
#' post_df()
post_df <- function(postlinks = c('cn/@dapeng/xuer-sale',
                                  'utopian-io/@dapeng/steemg-four-more'),
                    method = c('steemdb.com', 'steemsql.com', 'steemdata.com'),
                    sql_id,
                    sql_password) {
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
#' @param sql_id A character string of the steemsql id. Only valid when `method == 'steemsql'`
#' @param sql_password A character string of the steemsql passwordid. Only valid when `method == 'steemsql'`
#' @param post_number A numeric value or NA. The number of the latest posts to be obtained. If NA, all the posts will be processed.
#'
#' @return A data frame of an ID's post detailed info.
#' @export
#'
#' @examples
#' post_id()
post_id <- function(id = 'dapeng',
                    method = c('steemdb.com', 'steemsql.com', 'appbase_api', 'steemdata.com'),
                    sql_id,
                    sql_password,
                    post_number = NA) {
  method <- match.arg(method)
  # from steemdb.com
  if (method == 'steemdb.com') {
    postlinks <- post_links(id = id, post_number = post_number, site = 'steemdb.com')
  postlinks <- gsub('https://steemdb.com/', '', postlinks)
  mypost_id <- post_df(postlinks = postlinks)
  } else if(method == 'steemsql.com') {
    # from steemsql.com
    sql_connection <- paste0("Driver={SQL Server};server=sql.steemsql.com;database=DBSteem;uid=",
                             sql_id, ";pwd=", sql_password)
    cn <- RODBC::odbcDriverConnect(connection = sql_connection)
    post_query <- paste0("SELECT *
                         FROM Comments
                         WHERE
                         author = '", id, "'
                         AND
                         depth = 0
                         ORDER by
                         created")
    mypost_id <- RODBC::sqlQuery(channel = cn,
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

##' Hour rose plot.
##'
##' @param my_df A data frame containing fields ws and wd.
##' @param ws A character string of the name of the column representing the SBD payout.
##' @param wd A character sring of the name of the column representing the hour of the day.
##' @param ws2 The user can supply a second set of the payout value and hour with which the first can be compared.
##' @param wd2 see ws2.
##' @param ws.int A numeric vector of the payout interval. Default is 2 SBD.
##' @param angle The hour spokes. Other potentially useful angle is 3.
##' @param type type determines how the data are split i.e. conditioned, and then plotted.  It can be 'season', 'year', 'weekday'...  The default is will produce a single plot using the entire data. It can also be a numeric or factor vector.
##' @param cols Colours for plotting.  'default', 'increment', 'heat', 'jet', 'hue' and user defined, such as c("yellow", "green", "blue", "black").
##' @param grid.line Grid line interval. NULL in default.It can also be a numeric value like 10, or a list like list(value = 10, lty = 5, col = "purple").
##' @param width The adjustment factor for width of payout intervals. For example, width = 1.5 will make the paddle width 1.5 times wider. For paddle = TRUE.
##' @param seg  The width of the segments. 0.5 will produce segments 0.5 * angle.
##' @param auto.text  A logical value of whether formatting the names and units automatically in the titles and axis labels
##' @param breaks A numeric vector of the number of break points for payouts. 4 by default, which generates the break points 2, 4, 6, 8 SBD for ws.int default of 2 SBD. It can also be c(0, 1, 10, 100), which breaks the data into segments <1, 1-10, 10-100, >100.
##' @param offset A numeric value (default 10) of the size of the 'hole' in the middle of the plot, expressed as a percentage of the polar axis scale.
##' @param paddle A logic value. TRUE means the 'paddle' style spokes, and FALSE means the 'wedge' style spokes.
##' @param key.header A character string of additional text above the scale key.
##' @param key.footer A character string of additional text below the scale key.
##' @param key.position A character string of the location of the scale key. 'top', 'right', 'bottom' and 'left'.
##' @param key Fine control of the scale key
##' @param dig.lab A numeric value of the signficant digits at which scientific number formatting is used in break point.
##' @param statistic A character string of the statistic to be applied.
##' - 'prop.count' (default) sizes bins according to the proportion of the frequency of the records,
##' - 'prop.mean' sizes bins according to their relative contribution to the mean,
##' - 'abs.count' provides the absolute count of records in each bin.
##' @param pollutant Alternative data series to be sampled.
##' @param annotate A logic value or a character string.
##' - TRUE: the percentage calm and mean values are printed in each panel together with a description of the statistic below the plot.
##' - " ": only the stastic is below the plot.
##' - Custom annotations may be added by setting value to c("annotation 1", "annotation 2").
##' @param border A character string of the border colour for shaded areas.
##' @param cust_labels A numeric vector displayed as the customed labels
##' @param ...
##'
#' @return A figure with the active hour rose
#' @export
#' @examples
#' hourrose(my_df, breaks = seq(0, 24, 1), angle = 15, key.footer = 'votes')
hourrose <- function(my_df, col_time = 'created', ws = "ws", wd = "hour360", ws2 = NA, wd2 = NA,
                     ws.int = 30, angle = 1, type = "default", cols = "default",
                     grid.line = NULL, width = 1, seg = 0.9, auto.text = TRUE,
                     breaks = 4, offset = 10, paddle = FALSE, key.header = NULL,
                     key.footer = "(SBD)", key.position = "right", key = FALSE,
                     dig.lab = 5, statistic = "prop.count", pollutant = NULL,
                     cust_labels = c(0, 6, 12, 18), annotate = FALSE, border = NA, quantile_line = TRUE,
                     ...)
{
  angle <- 15 * angle
  my_df$hour <- round(as.numeric(format(my_df[, col_time], '%H')) + as.numeric(format(my_df[, col_time], '%M'))/60, 1)
  my_df$hour360 <- my_df$hour * 360 / 24
  my_df$ws <- 1

  if (is.null(seg))
    seg <- 0.9
  if (length(cols) == 1 && cols == "greyscale") {
    lattice::trellis.par.set(list(strip.background = list(col = "white")))
    calm.col <- "black"
  }
  else {
    calm.col <- "forestgreen"
  }
  current.strip <- lattice::trellis.par.get("strip.background")
  on.exit(lattice::trellis.par.set("strip.background", current.strip))
  if (360/angle != round(360/angle)) {
    warning("In windRose(...):\n  angle will produce some spoke overlap",
            "\n  suggest one of: 5, 6, 8, 9, 10, 12, 15, 30, 45, etc.",
            call. = FALSE)
  }
  if (angle < 3) {
    warning("In windRose(...):\n  angle too small", "\n  enforcing 'angle = 3'",
            call. = FALSE)
    angle <- 3
  }
  extra.args <- list(...)
  extra.args$xlab <- if ("xlab" %in% names(extra.args))
    openair::quickText(extra.args$xlab, auto.text)
  else openair::quickText("", auto.text)
  extra.args$ylab <- if ("ylab" %in% names(extra.args))
    openair::quickText(extra.args$ylab, auto.text)
  else openair::quickText("", auto.text)
  extra.args$main <- if ("main" %in% names(extra.args))
    openair::quickText(extra.args$main, auto.text)
  else openair::quickText("", auto.text)
  if (is.character(statistic)) {
    ok.stat <- c("prop.count", "prop.mean", "abs.count",
                 "frequency")
    if (!is.character(statistic) || !statistic[1] %in% ok.stat) {
      warning("In windRose(...):\n  statistic unrecognised",
              "\n  enforcing statistic = 'prop.count'", call. = FALSE)
      statistic <- "prop.count"
    }
    if (statistic == "prop.count") {
      stat.fun <- length
      stat.unit <- "%"
      stat.scale <- "all"
      stat.lab <- "Frequency of counts (%)"
      stat.fun2 <- function(x) signif(mean(x, na.rm = TRUE),
                                      3)
      stat.lab2 <- "mean"
      stat.labcalm <- function(x) round(x, 1)
    }
    if (statistic == "prop.mean") {
      stat.fun <- function(x) sum(x, na.rm = TRUE)
      stat.unit <- "%"
      stat.scale <- "panel"
      stat.lab <- "Proportion contribution to the mean (%)"
      stat.fun2 <- function(x) signif(mean(x, na.rm = TRUE),
                                      3)
      stat.lab2 <- "mean"
      stat.labcalm <- function(x) round(x, 1)
    }
    if (statistic == "abs.count" | statistic == "frequency") {
      stat.fun <- length
      stat.unit <- ""
      stat.scale <- "none"
      stat.lab <- "Count"
      stat.fun2 <- function(x) round(length(x), 0)
      stat.lab2 <- "count"
      stat.labcalm <- function(x) round(x, 0)
    }
  }
  if (is.list(statistic)) {
    stat.fun <- statistic$fun
    stat.unit <- statistic$unit
    stat.scale <- statistic$scale
    stat.lab <- statistic$lab
    stat.fun2 <- statistic$fun2
    stat.lab2 <- statistic$lab2
    stat.labcalm <- statistic$labcalm
  }
  vars <- c(wd, ws)
  diff <- FALSE
  rm.neg <- TRUE
  if (!is.na(ws2) & !is.na(wd2)) {
    vars <- c(vars, ws2, wd2)
    diff <- TRUE
    rm.neg <- FALSE
    my_df$ws <- my_df[, ws2] - my_df[, ws]
    my_df$wd <- my_df[, wd2] - my_df[, wd]
    id <- which(my_df$wd < 0)
    if (length(id) > 0)
      my_df$wd[id] <- my_df$wd[id] + 360
    pollutant <- "ws"
    key.footer <- "ws"
    wd <- "wd"
    ws <- "ws"
    vars <- c("ws", "wd")
    if (missing(angle))
      angle <- 10
    if (missing(offset))
      offset <- 20
    if (is.na(breaks[1])) {
      max.br <- max(ceiling(abs(c(min(my_df$ws, na.rm = TRUE),
                                  max(my_df$ws, na.rm = TRUE)))))
      breaks <- c(-1 * max.br, 0, max.br)
    }
    if (missing(cols))
      cols <- c("lightskyblue", "tomato")
    seg <- 1
  }
  if (any(type %in% openair:::dateTypes))
    vars <- c(vars, "date")
  if (!is.null(pollutant))
    vars <- c(vars, pollutant)
  my_df <- openair:::checkPrep(my_df, vars, type, remove.calm = FALSE,
                                remove.neg = rm.neg)
  my_df <- na.omit(my_df)
  if (is.null(pollutant))
    pollutant <- ws
  my_df$x <- my_df[, pollutant]
  my_df[, wd] <- angle * ceiling(my_df[, wd]/angle - 0.5)
  my_df[, wd][my_df[, wd] == 0] <- 360
  my_df[, wd][my_df[, ws] == 0] <- -999
  if (length(breaks) == 1)
    breaks <- 0:(breaks - 1) * ws.int
  if (max(breaks) < max(my_df$x, na.rm = TRUE))
    breaks <- c(breaks, max(my_df$x, na.rm = TRUE))
  if (min(breaks) > min(my_df$x, na.rm = TRUE))
    warning("Some values are below minimum break.")
  breaks <- unique(breaks)
  my_df$x <- cut(my_df$x, breaks = breaks, include.lowest = FALSE,
                  dig.lab = dig.lab)
  theLabels <- gsub("[(]|[)]|[[]|[]]", "", levels(my_df$x))
  theLabels <- gsub("[,]", " to ", theLabels)
  prepare.grid <- function(my_df) {
    if (all(is.na(my_df$x)))
      return()
    levels(my_df$x) <- c(paste("x", 1:length(theLabels),
                                sep = ""))
    all <- stat.fun(my_df[, wd])
    calm <- my_df[my_df[, wd] == -999, ][, pollutant]
    my_df <- my_df[my_df[, wd] != -999, ]
    calm <- stat.fun(calm)
    weights <- tapply(my_df[, pollutant], list(my_df[,
                                                       wd], my_df$x), stat.fun)
    if (stat.scale == "all") {
      calm <- calm/all
      weights <- weights/all
    }
    if (stat.scale == "panel") {
      temp <- stat.fun(stat.fun(weights)) + calm
      calm <- calm/temp
      weights <- weights/temp
    }
    weights[is.na(weights)] <- 0
    weights <- t(apply(weights, 1, cumsum))
    if (stat.scale == "all" | stat.scale == "panel") {
      weights <- weights * 100
      calm <- calm * 100
    }
    panel.fun <- stat.fun2(my_df[, pollutant])
    u <- mean(sin(2 * pi * my_df[, wd]/360))
    v <- mean(cos(2 * pi * my_df[, wd]/360))
    mean.wd <- atan2(u, v) * 360/2/pi
    if (all(is.na(mean.wd))) {
      mean.wd <- NA
    }
    else {
      if (mean.wd < 0)
        mean.wd <- mean.wd + 360
      if (mean.wd > 180)
        mean.wd <- mean.wd - 360
    }
    weights <- cbind(data.frame(weights), wd = as.numeric(row.names(weights)),
                     calm = calm, panel.fun = panel.fun, mean.wd = mean.wd)
    weights
  }
  if (paddle) {
    poly <- function(wd, len1, len2, width, colour, x.off = 0,
                     y.off = 0) {
      theta <- wd * pi/180
      len1 <- len1 + off.set
      len2 <- len2 + off.set
      x1 <- len1 * sin(theta) - width * cos(theta) + x.off
      x2 <- len1 * sin(theta) + width * cos(theta) + x.off
      x3 <- len2 * sin(theta) - width * cos(theta) + x.off
      x4 <- len2 * sin(theta) + width * cos(theta) + x.off
      y1 <- len1 * cos(theta) + width * sin(theta) + y.off
      y2 <- len1 * cos(theta) - width * sin(theta) + y.off
      y3 <- len2 * cos(theta) + width * sin(theta) + y.off
      y4 <- len2 * cos(theta) - width * sin(theta) + y.off
      lpolygon(c(x1, x2, x4, x3), c(y1, y2, y4, y3), col = colour,
               border = border)
    }
  }
  else {
    poly <- function(wd, len1, len2, width, colour, x.off = 0,
                     y.off = 0) {
      len1 <- len1 + off.set
      len2 <- len2 + off.set
      theta <- seq((wd - seg * angle/2), (wd + seg * angle/2),
                   length.out = (angle - 2) * 10)
      theta <- ifelse(theta < 1, 360 - theta, theta)
      theta <- theta * pi/180
      x1 <- len1 * sin(theta) + x.off
      x2 <- rev(len2 * sin(theta) + x.off)
      y1 <- len1 * cos(theta) + x.off
      y2 <- rev(len2 * cos(theta) + x.off)
      lpolygon(c(x1, x2), c(y1, y2), col = colour, border = border)
    }
  }
  my_df <- openair::cutData(my_df, type, ...)
  results.grid <- plyr::ddply(my_df, type, prepare.grid)
  results.grid$calm <- stat.labcalm(results.grid$calm)
  results.grid$mean.wd <- stat.labcalm(results.grid$mean.wd)
  strip.dat <- openair:::strip.fun(results.grid, type, auto.text)
  strip <- strip.dat[[1]]
  strip.left <- strip.dat[[2]]
  pol.name <- strip.dat[[3]]
  if (length(theLabels) < length(cols)) {
    col <- cols[1:length(theLabels)]
  }
  else {
    col <- openColours(cols, length(theLabels))
  }
  max.freq <- max(results.grid[, (length(type) + 1):(length(theLabels) +
                                                       length(type))], na.rm = TRUE)
  off.set <- max.freq * (offset/100)
  box.widths <- seq(0.002^0.25, 0.016^0.25, length.out = length(theLabels))^4
  box.widths <- box.widths * max.freq * angle/5
  legend <- list(col = col, space = key.position, auto.text = auto.text,
                 labels = theLabels, footer = key.footer, header = key.header,
                 height = 0.6, width = 1.5, fit = "scale", plot.style = if (paddle) "paddle" else "other")
  legend <- openair:::makeOpenKeyLegend(key, legend, "windRose")
  temp <- paste(type, collapse = "+")
  myform <- formula(paste("x1 ~ wd | ", temp, sep = ""))
  mymax <- 2 * max.freq
  myby <- if (is.null(grid.line))
    pretty(c(0, mymax), 10)[2]
  else grid.line
  if (myby/mymax > 0.9)
    myby <- mymax * 0.9
  xyplot.args <- list(x = myform, xlim = 1.03 * c(-max.freq -
                                                    off.set, max.freq + off.set), ylim = 1.03 * c(-max.freq -
                                                                                                    off.set, max.freq + off.set), data = results.grid, type = "n",
                      sub = stat.lab, strip = strip, strip.left = strip.left,
                      as.table = TRUE, aspect = 1, par.strip.text = list(cex = 0.8),
                      scales = list(draw = FALSE), panel = function(x, y,
                                                                    subscripts, ...) {
                        panel.xyplot(x, y, ...)
                        angles <- seq(0, 2 * pi, length = 360)
                        sapply(seq(off.set, mymax, by = myby), function(x) llines(x *
                                                                                    sin(angles), x * cos(angles), col = "grey85",
                                                                                  lwd = 1))
                        subdata <- results.grid[subscripts, ]
                        upper <- max.freq + off.set
                        lar <- upper * 0.9
                        for (langle in seq(0, 2 * pi, pi/4)) larrows(0, 0, lar * cos(langle), lar * (sin(langle)), code = 3,length = 0, col = 'grey')
                        for (langle in seq(0, 2 * pi, pi/2)) larrows(0, 0, lar * cos(langle), lar * (sin(langle)), code = 3,length = 0)
                        if (quantile_line) {
                          for (langle in quantile(my_df[, wd], seq(0.25, 1, 0.25)) / 180 * pi) larrows(0, 0, lar * sin(langle), lar * (cos(langle)), code = 1,length = 0, col = 'red')
                        }


                        # larrows(-upper * 0.9, 0, upper * 0.9, 0, code = 3,
                        #         length = 0)
                        # larrows(0, -upper * 0.9, 0, upper * 0.9, code = 3,
                        #         length = 0)

                        ltext(0 * upper, upper * 0.95, cust_labels[1], cex = 1)
                        ltext(upper * 0.95, 0 * upper, cust_labels[2], cex = 1)
                        ltext(0 * upper, upper * -1 * 0.95, cust_labels[3],
                              cex = 1)
                        ltext(upper * -1 * 0.95, 0 * upper, cust_labels[4],
                              cex = 1)
                        if (nrow(subdata) > 0) {
                          for (i in 1:nrow(subdata)) {
                            with(subdata, {
                              for (j in 1:length(theLabels)) {
                                if (j == 1) {
                                  temp <- "poly(wd[i], 0, x1[i], width * box.widths[1], col[1])"
                                } else {
                                  temp <- paste("poly(wd[i], x", j - 1,
                                                "[i], x", j, "[i], width * box.widths[",
                                                j, "], col[", j, "])", sep = "")
                                }
                                eval(parse(text = temp))
                              }
                            })
                          }
                        }
                        ltext(seq((myby + off.set), mymax, myby) * sin(pi/4),
                              seq((myby + off.set), mymax, myby) * cos(pi/4),
                              paste(seq(myby, mymax, by = myby), stat.unit,
                                    sep = ""), cex = 0.7)
                        if (annotate) if (statistic != "prop.mean") {
                          if (!diff) {
                            ltext(max.freq + off.set, -max.freq - off.set,
                                  label = paste(stat.lab2, " = ", subdata$panel.fun[1],
                                                "\ncalm = ", subdata$calm[1], stat.unit,
                                                sep = ""), adj = c(1, 0), cex = 0.7, col = calm.col)
                          }
                          if (diff) {
                            ltext(max.freq + off.set, -max.freq - off.set,
                                  label = paste("mean ws = ", round(subdata$panel.fun[1],
                                                                    1), "\nmean wd = ", round(subdata$mean.wd[1],
                                                                                              1), sep = ""), adj = c(1, 0), cex = 0.7,
                                  col = calm.col)
                          }
                        } else {
                          ltext(max.freq + off.set, -max.freq - off.set,
                                label = paste(stat.lab2, " = ", subdata$panel.fun[1],
                                              stat.unit, sep = ""), adj = c(1, 0), cex = 0.7,
                                col = calm.col)
                        }
                      }, legend = legend)
  xyplot.args <- openair:::listUpdate(xyplot.args, extra.args)
  plt <- do.call(xyplot, xyplot.args)
  if (length(type) == 1)
    plot(plt)
  else plot(useOuterStrips(plt, strip = strip, strip.left = strip.left))
  newdata <- results.grid
  output <- list(plot = plt, data = newdata, call = match.call())
  class(output) <- "openair"
  invisible(output)
}

