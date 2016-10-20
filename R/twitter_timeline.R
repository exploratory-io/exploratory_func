#' Get a user's timeline
#' @export
getTwitterTimeline <- function(user, n=3200, includeRts = FALSE,
                               includeReplies = TRUE, tokenFileId){
  if(!requireNamespace("twitteR")){stop("package twitteR must be installed.")}

  twitter_token = getTwitterToken(tokenFileId)
  twitteR::use_oauth_token(twitter_token)

  # use includeReplies and reverse it for excludeReplies argument for argument consistency
  excludeReplies <- !includeReplies

  ret <- twitteR::userTimeline(user, n = n, includeRts = includeRts, excludeReplies = excludeReplies)

  if(length(ret)>0){
    twitteR::twListToDF(ret)
  } else {
    stop('No Tweets found.')
  }
}
