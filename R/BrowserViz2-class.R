#----------------------------------------------------------------------------------------------------
.getBrowser <- function() {getOption("browser")}
printf <- function(...) print(noquote(sprintf(...)))
#----------------------------------------------------------------------------------------------------
BrowserViz.state <- new.env(parent=emptyenv())
#----------------------------------------------------------------------------------------------------
# the semanitcs of toJSON changed between RJSONIO and jsonlite: in the latter, scalars are
# promoted to arrays of length 1.  rather than change our javascript code, and since such
# promotion -- while sensible in the context of R -- strikes me as gratuitous, I follow
# jeroen ooms suggestion, creating this wrapper
toJSON <- function(..., auto_unbox = TRUE)
{
  jsonlite::toJSON(..., auto_unbox = auto_unbox)
}
#----------------------------------------------------------------------------------------------------
# a default html + javascript file, an example, shows how to setup the websocket, get web page
# dimensions, set and get the browser's window  title
browserVizBrowserFile <- system.file(package="BrowserViz", "scripts", "viz.html")

#----------------------------------------------------------------------------------------------------
# this maps from incoming json commands to function calls.  it is file-global rather than
# an object slot because it is used by the function "dispatchMessage" which is called
# from -within- a function we pass to, and which is executed by, httpuv:
#        ws$onMessage(function(binary, rawMessage) {...}
# where "onMessage" means "this is the function to call when an incoming message is received"
dispatchMap <- new.env(parent=emptyenv())

# status is global variable at file scope, invisible outside the package.
# it keeps track of web sockect connection state, and -- crucially --
# holds the result variable returned by the browser.
# this solves the latency problem: when we make
# a request to the code running in the browser, the browser later (though
# often very quickly) sends a JSON message back to R.  If we are, for instance,
# asking for the current browser window title (see 'getBrowserWindowTitle' below), that
# result is sent to the callback we have registered, "handleResponse")
# to make this seem like a synchronous call, the caller sits in a tight sleep loop,
# waiting until status$result is no longer NULL.  getBrowserWindowTitle will then
# parse that JSON response into an R variable.
# the checking of status$result, and its retrieval when ready (no longer null)
# is accomplished by exported methods browserResponseReady and getBrowserResponse,
# to be used by subclasses as well.

status <- new.env(parent=emptyenv())
status$result <- NULL

# the duration of the aforementioned tight loop, waiting for the browser to respond.

sleepTime <- 1

#----------------------------------------------------------------------------------------------------
.BrowserViz2 <- setClass ("BrowserViz2Class",
                         representation = representation (
                                               uri="character",
                                               port="numeric",
                                               wsID="character",
                                               websocketConnection="environment",
                                               browserFile="character",
                                               quiet="logical"),
                         prototype = prototype (uri="http://localhost", 9000)
                         )

#----------------------------------------------------------------------------------------------------
setGeneric('show',                    signature='obj', function(obj) standardGeneric('show'))
setGeneric('openBrowser',             signature='obj', function(obj) standardGeneric('openBrowser'))
setGeneric('port',                    signature='obj', function(obj) standardGeneric('port'))
setGeneric('ready',                   signature='obj', function(obj) standardGeneric('ready'))
setGeneric('getBrowserInfo',          signature='obj', function(obj) standardGeneric('getBrowserInfo'))
setGeneric('send',                    signature='obj', function(obj, msg) standardGeneric('send'))
setGeneric('addMessageHandler',       signature='obj', function(obj, key, functionName) standardGeneric('addMessageHandler'))
setGeneric('browserResponseReady',    signature='obj', function(obj) standardGeneric('browserResponseReady'))
setGeneric('getBrowserResponse',      signature='obj', function(obj) standardGeneric('getBrowserResponse'))
setGeneric('closeWebSocket',          signature='obj', function(obj) standardGeneric('closeWebSocket'))
setGeneric('getBrowserWindowTitle',   signature='obj', function(obj) standardGeneric('getBrowserWindowTitle'))
setGeneric('setBrowserWindowTitle',   signature='obj', function(obj, newTitle, proclaim=FALSE)
                                                                     standardGeneric('setBrowserWindowTitle'))
setGeneric('roundTripTest',           signature='obj', function (obj, ...) standardGeneric('roundTripTest'))
setGeneric('getBrowserWindowSize',    signature='obj', function(obj) standardGeneric('getBrowserWindowSize'))
#----------------------------------------------------------------------------------------------------
BrowserViz2 = function(portRange, host="localhost", title="BrowserViz", quiet=FALSE, browserFile=NA,
                       httpQueryProcessingFunction=NULL)
{
  if(is.na(browserFile))
     browserFile <- browserVizBrowserFile

  if(!quiet) printf("browserFile: %s", browserFile)
  BrowserViz.state[["httpQueryProcessingFunction"]] <- httpQueryProcessingFunction

  stopifnot(file.exists(browserFile))
  wsCon <- new.env(parent=emptyenv())
  result <- .startDaemonizedServerOnFirstAvailableLocalHostPort(portRange, wsCon)
  if(!quiet){
     printf("--- daemonizedServer")
     print(result)
     }

  actualPort <- result$port
  wsID <- result$wsID

  if(is.null(actualPort))
    stop(sprintf("no available ports in range %d:%d", min(portRange), max(portRange)))

  uri = sprintf("http://%s:%s", host, actualPort)

  .BrowserViz2(uri=uri, websocketConnection=wsCon, port=actualPort, wsID=wsID,
               browserFile=browserFile, quiet=quiet)

} # new ctor
#----------------------------------------------------------------------------------------------------
setMethod('openBrowser', 'BrowserViz2Class',

  function (obj) {

    if(!obj@quiet){
       message(sprintf("summoning default browser to get %s", obj@uri))
       }

    wsCon <- .setupWebSocketHandlers(obj@websocketConnection, obj@browserFile, obj@quiet)
    obj@websocketConnection$wsID <- obj@wsID

    if(!obj@quiet)
      message(sprintf("starting daemonized server on port %s", obj@port))

    addMessageHandler(obj, "handleResponse", "handleResponse")

    totalWait <- 0.0
    maxWaitPermitted <- 10000.0

    if(!obj@quiet) printf("openBrowser about to browse to %s", obj@uri)

    browseURL(obj@uri, browser=.getBrowser())

    while (is.null(obj@websocketConnection$ws)){   # becomes non-null when handshake is established
      totalWait <- totalWait + sleepTime
      stopifnot(totalWait < maxWaitPermitted)
      if(!obj@quiet)
         message(sprintf ("BrowserViz websocket not ready, waiting %6.2f seconds", sleepTime));
      Sys.sleep(sleepTime)
      }

    if(!obj@quiet){
      message(sprintf("BrowserViz websocket ready after %6.2f seconds", totalWait));
      message(sprintf("about to return BrowserViz object"));
      }

    }) # openBrowser

#----------------------------------------------------------------------------------------------------
.validWebSocketID <- function(candidate)
{
   if(length(grep("not available", candidate)) == 1)
      return (FALSE)

   return (TRUE)

} # .validWebSocketID
#----------------------------------------------------------------------------------------------------
.startDaemonizedServerOnFirstAvailableLocalHostPort <- function(portRange, wsCon)
{
   done <- FALSE

   port <- portRange[1]
   wsID <- NULL

   while(!done){
     if(port > max(portRange))
        done <- TRUE
     else
        wsID <- tryCatch(startDaemonizedServer("127.0.0.1", port, wsCon),
                        error=function(m){sprintf("port not available: %d", port)})
     if(.validWebSocketID(wsID))
        done <- TRUE
     else
        port <- port + 1;
     } # while

   actualPort <- NULL

   if(.validWebSocketID(wsID))
      actualPort <- port

   list(wsID=wsID, port=actualPort)

} # .startDaemonizedServerOnFirstAvailableLocalHostPort
#----------------------------------------------------------------------------------------------------
setMethod('show', 'BrowserViz2Class',

  function (obj) {
     msg <- sprintf("BrowserViz object");
     cat(msg, '\n', sep='')
     msg <- sprintf("ready? %s", ready(obj))
     cat(msg, '\n', sep='')
     msg <- sprintf("port: %d", port(obj))
     cat(msg, '\n', sep='')
     }) # show

#----------------------------------------------------------------------------------------------------
setMethod('port', 'BrowserViz2Class',

  function (obj) {
     obj@port
     })

#----------------------------------------------------------------------------------------------------
setMethod('closeWebSocket', 'BrowserViz2Class',

  function (obj) {
     if(!obj@websocketConnection$open){
        warning("websocket server is not open, cannot close");
        return()
        }
     obj@websocketConnection$open <- FALSE
     stopDaemonizedServer(obj@websocketConnection$wsID)
     obj@websocketConnection$ws <- NULL
     obj@websocketConnection$ws <- -1

     invisible(obj)
     })

#----------------------------------------------------------------------------------------------------
# test initial variable setup, then send an actual message, and await the reply
setMethod('ready', 'BrowserViz2Class',

  function (obj) {

     sleepIntervalCount <- 0
     sleepInterval <- 0.1

     if(!is.environment(obj@websocketConnection))
        return(FALSE)

     if(length(ls(obj@websocketConnection)) == 0)
        return(FALSE)

     if(!obj@websocketConnection$open)
        return(FALSE)

     send(obj, list(cmd="ready", callback="handleResponse", status="request", payload=""))

     while (!browserResponseReady(obj)){
        if(!obj@quiet) printf("waiting in BrowserViz.ready, browserResponseReady not yet true");
        Sys.sleep(sleepInterval)
        sleepIntervalCount <- sleepIntervalCount + 1
        }

     if(!obj@quiet) printf("browserResponseReady now true, after %d sleepInterval/s of %f",
                           sleepIntervalCount, sleepInterval);
     getBrowserResponse(obj);
     return(TRUE);
     })

#----------------------------------------------------------------------------------------------------
setMethod('browserResponseReady', 'BrowserViz2Class',

  function (obj) {
    return(!is.null(status$result))
    })

#----------------------------------------------------------------------------------------------------
setMethod('getBrowserResponse', 'BrowserViz2Class',

  function (obj) {
    if(!obj@quiet){
       message(sprintf("BrowserViz getBrowserResponse, length %d", length(status$result)))
       }
    return(status$result)
    })

#----------------------------------------------------------------------------------------------------
.setupWebSocketHandlers <- function(wsCon, browserFile, quiet)
{
   if(!quiet){
      printf("--- entering BrowserViz .setupWebSocketHandlers");
      printf("    browserFile: %s", browserFile);
      }

   wsCon$open <- FALSE
   wsCon$ws <- NULL
   wsCon$result <- NULL
     # process http requests
   wsCon$call = function(req) {
      qs <- req$QUERY_STRING
      if(nchar(qs) > 0){
         if(!quiet) print("--- bv$call, about to call dynamically assigned queryProcessor");
         fields <- ls(req)
         for(field in fields){
            #printf("---- request field: %s", field)
            #print(req[[field]]);
            }
         queryProcessorFunction <- BrowserViz.state[["httpQueryProcessingFunction"]]
         if(!is.null(queryProcessorFunction))
           body <- queryProcessorFunction(qs)
         else
           body <- "no query processor registered"
         return(list(status=200L, headers = list('Content-Type' = 'text/html'), body=body))
         } # the request had a query string
      wsUrl = paste(sep='', '"', "ws://",
                   ifelse(is.null(req$HTTP_HOST), req$SERVER_NAME, req$HTTP_HOST),
                   '"')
     list(
       status = 200L,
       headers = list('Content-Type' = 'text/html'),
       body = c(file=browserFile))
       }

      # called whenever a websocket connection is opened
   wsCon$onWSOpen = function(ws) {
      if(!quiet)
         print("BrowserViz..setupWebSocketHandlers, wsCon$onWSOpen");
      wsCon$ws <- ws   # this provides later access (eg wsCon$ws$send) to crucial functions
      ws$onMessage(function(binary, rawMessage) {
         if(!quiet) print("BrowserViz..setupWebSocketHandlers, onMessage ");
         message <- as.list(fromJSON(rawMessage))
         wsCon$lastMessage <- message
         if(!is(message, "list")){
            message("message: new websocket message is not a list");
            return;
            }
         if (! "cmd" %in% names(message)){
            message("error: new websocket message has no 'cmd' field");
            return;
            }
         cmd <- message$cmd
         if(!quiet) printf("BrowserViz dispatching on msg$cmd: %s", message$cmd);
         dispatchMessage(ws, message, quiet);
         }) # onMessage
       wsCon$open <- TRUE
       if(!quiet) print("leaving BrowserViz2-class::.setupWebSocketHandlers.onWSOpen")
   } # onWSOpen

   wsCon

} # .setupWebSocketHandlers
#--------------------------------------------------------------------------------
setMethod('addMessageHandler', 'BrowserViz2Class',

  function (obj, key, functionName){
     dispatchMap[[key]] <- functionName
     })

#---------------------------------------------------------------------------------------------------
dispatchMessage <- function(ws, msg, quiet)
{
   if(!msg$cmd %in% ls(dispatchMap)){
       message(sprintf("dispatchMessage error!  the incoming cmd '%s' is not recognized", msg$cmd))
       return()
       }

   function.name <- dispatchMap[[msg$cmd]]
   success <- TRUE

   if(is.null(function.name)){
       message(sprintf("dispatchMessage error!  cmd ('%s') not recognized", msg$cmd))
       success <- FALSE
       return()
       }

   tryCatch(func <- get(function.name), error=function(m) func <<- NULL)

   if(is.null(func)){
       message(sprintf("dispatchMessage error!  cmd ('%s') recognized but no corresponding function",
              msg$cmd))
       success <- FALSE
       }

   if(success){
      if(!quiet) printf("BrowserViz.dispatchMessage calling function '%s'", function.name);
      do.call(func, list(ws, msg))
      }

} # dispatchMessage
#---------------------------------------------------------------------------------------------------
setMethod('send', 'BrowserViz2Class',

    function(obj, msg) {
      status$result <- NULL
      msg.json <- toJSON(msg)
      obj@websocketConnection$ws$send(toJSON(msg))
      })

#--------------------------------------------------------------------------------
setMethod('getBrowserInfo', 'BrowserViz2Class',

  function (obj) {
     send(obj, list(cmd="getBrowserInfo", callback="handleResponse", status="request", payload=""))
     while (!browserResponseReady(obj)){
        Sys.sleep(.1)
        }
     getBrowserResponse(obj);
     })

#--------------------------------------------------------------------------------
setMethod('roundTripTest', 'BrowserViz2Class',

  function (obj, ...) {
     payload <- toJSON(...)
     send(obj, list(cmd="roundTripTest", callback="handleResponse", status="request", payload=payload))
     while (!browserResponseReady(obj)){
        Sys.sleep(.1)
        }
     getBrowserResponse(obj);
     })

#----------------------------------------------------------------------------------------------------
setMethod('getBrowserWindowTitle', 'BrowserViz2Class',

  function (obj) {
     send(obj, list(cmd="getWindowTitle", callback="handleResponse", status="request", payload=""))
     while (!browserResponseReady(obj)){
        Sys.sleep(.1)
        }
     getBrowserResponse(obj);
     })

#----------------------------------------------------------------------------------------------------
setMethod('setBrowserWindowTitle', 'BrowserViz2Class',

  function (obj, newTitle, proclaim=FALSE) {
     payload = list(title=newTitle, proclaim=proclaim)
     send(obj, list(cmd="setWindowTitle", callback="handleResponse", status="request",
                    payload=payload))
     while (!browserResponseReady(obj)){
        Sys.sleep(.1)
        }
     getBrowserResponse(obj)
     })

#----------------------------------------------------------------------------------------------------
setMethod('getBrowserWindowSize', 'BrowserViz2Class',

  function (obj) {
     send(obj, list(cmd="getWindowSize", callback="handleResponse", status="request", payload=""))
     while (!browserResponseReady(obj)){
        Sys.sleep(.1)
        }
     as.list(fromJSON(getBrowserResponse(obj)))
     })

#----------------------------------------------------------------------------------------------------
handleResponse <- function(ws, msg)
{
   if(msg$status == "success")
      status$result <- msg$payload
   else{
     message(msg$payload)
     status$result <- NA
     }

   NULL

} # handleResponse
#----------------------------------------------------------------------------------------------------
#.processQuery <- function(queryString)
#{
#
#  list(status=200L, headers = list('Content-Type' = 'text/html'),
#       body="hello from bv.processQuery, dynamically assigned")
#
#} # .processQuery
##----------------------------------------------------------------------------------------------------
#queryProcessor <- .processQuery


