library(RUnit)
library(bv2)
#--------------------------------------------------------------------------------
PORT_RANGE <- 7300:7320
QUIET <- TRUE
#--------------------------------------------------------------------------------
runTests <- function()
{
  testAllOperations()
  testRunOutOfPorts();
  testMultipleOpenCloseOnSamePort()

} # runTests
#--------------------------------------------------------------------------------
testAllOperations <- function()
{
   printf("--- testAllOperations")
   app <- BrowserViz2(PORT_RANGE, quiet=FALSE);
   checkTrue(!ready(app))

   openBrowser(app)
   checkTrue(ready(app))
   checkTrue(port(app) %in% PORT_RANGE)

      # ---- get browser info
   userAgent <- getBrowserInfo(app)
   checkEquals(typeof(userAgent), "character")
   checkTrue(nchar(userAgent) > 5);  # 120 on chrome 40.0.2214.115 (27 feb 2015)

   setBrowserWindowTitle(app, "new title");
   checkEquals(getBrowserWindowTitle(app), "new title")

   x <- getBrowserWindowSize(app)
   checkTrue(all(c("width", "height") %in% names(x)))
   checkTrue(is.integer(x$width))
   checkTrue(is.integer(x$height))

   data <- 99
   data.returned <- fromJSON(roundTripTest(app, data))
   checkEquals(data, data.returned)

   data <- list(lowercase=letters, uppercase=LETTERS)
   data.returned <- fromJSON(roundTripTest(app, data))
   checkEquals(data, data.returned)

   data <- matrix(1:100, nrow=10)
   #colnames(data) <- LETTERS[1:10]
   data.returned <- fromJSON(roundTripTest(app, data))
   checkEquals(data, data.returned)

   data <- matrix(1:10000, nrow=10)
   data.returned <- fromJSON(roundTripTest(app, data))
   checkEquals(data, data.returned)

   closeWebSocket(app)

} # testAllOperations
#--------------------------------------------------------------------------------
testMultipleOpenCloseOnSamePort <- function()
{
   print("--- testMultipleOpenCloseOnSamePort")

   max <- 3

   for(i in 1:max){
     app <- BrowserViz2(PORT_RANGE[1]);
     openBrowser(app)
     checkTrue(ready(app))
     #printf("app instance #%d ready on port %d", i, port(app))
     checkEquals(port(app), PORT_RANGE[1])
     closeWebSocket(app)
     checkTrue(!ready(app))
     #printf("app instance #%d closed", i)
     } # for i


} # testMultipleOpenCloseOnSamePort
#--------------------------------------------------------------------------------
testRunOutOfPorts <- function()
{
   print("--- testRunOutOfPorts")

   max <- 3
   portRange <- PORT_RANGE[1]:(PORT_RANGE[1]+1)
   apps <- lapply(rep("BrowserViz2Class", max), new)

   boundToFail <- function(max, portRange){
      for(i in 1:max){
         app <- BrowserViz2(portRange);
         openBrowser(app)
         apps[[i]] <- app
         checkTrue(ready(app))
         setBrowserWindowTitle(app, sprintf("app %d", i))
         checkTrue(port(app) %in% portRange)
         } # for i
       } # boundToFail

     # should be able to open two connections.  the third should fail
   checkException(boundToFail(max, portRange), silent=TRUE)

     # now close any apps which we managed to open
   for(i in 1:length(apps)){
     if(length(port(apps[[i]])) > 0){
        app <- apps[[i]]
        if(ready(app)) {
           printf("closing app instance on port %d", port(app))
           checkEquals(getBrowserWindowTitle(app), sprintf("app %d", i))
           closeWebSocket(app)
           checkTrue(!ready(app))
           }# if ready
        } # if port(app) has meaningful value
     } # for i

} # testRunOutOfPorts
#--------------------------------------------------------------------------------
