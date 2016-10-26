#' Get a user's timeline
#' @param user - User id.
#' @param n - Maximum number of tweets. Max of n in userTimeline is 3200, so the default is that number.
#' @param includeReplies - Whether replies should be included in the result.
#' @param tokenFileId - File id for auth token.
#' @param maxID - Maximum tweet id.
#' @param sinceID - Minimum tweet id.
#' @export
getTwitterTimeline <- function(user, n=3200,
                               includeReplies = TRUE, maxID = NULL, sinceID = NULL, tokenFileId){
  if(!requireNamespace("twitteR")){stop("package twitteR must be installed.")}

  twitter_token = getTwitterToken(tokenFileId)
  twitteR::use_oauth_token(twitter_token)

  # use includeReplies and reverse it for excludeReplies argument for argument consistency
  excludeReplies <- !includeReplies

  # as for includeRts, the behaviour is strange, so always set it TRUE
  ret <- twitteR::userTimeline(user, n = n,
                               includeRts = TRUE,
                               excludeReplies = excludeReplies,
                               maxID = maxID,
                               sinceID = sinceID)

  if(length(ret)>0){
    twitteR::twListToDF(ret)
  } else {
    stop('No Tweets found.')
  }
}
