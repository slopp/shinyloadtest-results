createLogDf <- function(logsDir, testId) {
  files <- list.files(logsDir)
  log <- list()
  
  for (i in seq_along(files)) {
    fname <- file.path(logsDir, files[i])
    f <- readr::read_csv(fname, skip = 1)
    
    info <- readr::read_lines(fname, n_max = 1)
    sess <- as.numeric(gsub('# --sessions (\\d+) .*', '\\1', info)) 
    
    
    f$sess <- sess
    f$testId <- testId
    
    log[[i]] <- f
  }
  
  logdf <- do.call(rbind, log)
  logdf$time <- lubridate::as_datetime(logdf$timestamp/1000)
  
  # we need to subtract 1 to get a line number that matches with the recorder
  
  logdf
}

# Event Latency
# input: logDf filtered to a single session & expected event duration
# output: thread sess event_start  event_end

eventTimeSec <- function(sess, expected) {
  # if our thread failed, discard 
  if (!('PLAYER_DONE' %in% sess$event)) {
    return(NULL)
  }
  
  # we have line nums for the ws_send and ws_recv  of each event from the recording
  # using those, get the timing info from playback
  events <- list()
  for (i in 1:nrow(expected)) {
    event_start_id <- which(((sess$event == 'WS_SEND_START') + (sess$input_line_number == expected$event_ws_send_line_num[i])) == 2)
    event_end_id <- which(((sess$event == 'WS_RECV_END') + (sess$input_line_number == expected$event_ws_recv_line_num[i])) == 2)
    events[[i]] <- list(
      thread = sess$thread_id[event_start_id],
      tar_sess = sess$sess[event_start_id],
      event_start = sess$time[event_start_id],
      event_end = sess$time[event_end_id],
      expected_duration_sec = expected$exepected_duration_sec[i]
    )
  }
  
  eventsDf <- do.call(rbind.data.frame, events)
  eventsDf$actual_duration_sec = eventsDf$event_end - eventsDf$event_start
  eventsDf$latency_sec = eventsDf$actual_duration_sec - eventsDf$expected_duration_sec
  eventsDf$event_start <- lubridate::as_datetime(eventsDf$event_start)
  eventsDf$event_end <- lubridate::as_datetime(eventsDf$event_end)
  eventsDf
}

eventTimeSecAll <- function(logDf, expected){
  threads <- unique(logDf$thread_id)
  events <- list()
  for(i in seq_along(threads)){
    sessDf <- logDf[logDf$thread_id == threads[i],]
    events[[i]] <- eventTimeSec(sessDf, expected)
  }
  eventsDf <- do.call(rbind, events)
  eventsDf
}


# Concurrent events over test 
# second, concurrent users
conLoadedPages <- function(eventsDf){
  
  # create a sequence of intervals
  # for each second of the test
  min_time <- min(eventsDf$event_start)
  max_time <- max(eventsDf$event_end)
  time_seq <- seq.int(min_time, max_time, 30)
  time_seq_end <- time_seq +1
  test_ints <- lubridate::interval(time_seq, time_seq_end)
  
  # create an interval for each thread
  threads <- unique(eventsDf$thread)
  thread_ints <- list()
  for (t in seq_along(threads)) {
    threadDf <- eventsDf[eventsDf$thread == threads[t],]
    thread_ints[[t]] <- lubridate::interval(
      min(threadDf$event_start),
      max(threadDf$event_end)
    )
  }
  
  
  # for each second of the test
  # calculate how many threads were active
  conLoadedPages <- list()
  for(i in seq_along(test_ints)) {
    count <- 0
    for (j in seq_along(thread_ints)) {
      if (lubridate::int_overlaps(test_ints[i], thread_ints[[j]][1])) {
        count <- count+1
      }
    }
    conLoadedPages[[i]] <- data.frame(
      time = lubridate::int_start(test_ints[i]),
      concurrent = count
    )
  }
  
  do.call(rbind, conLoadedPages)
}

# Trim
# Goal is to only get events that occurred w/ target # of concurrent users
trimEvents <- function(eventsDf, conLoadedPages, concurrentTarget) {
  loaded <- which(conLoadedPages$concurrent >= concurrentTarget)
  start <- min(conLoadedPages$time[loaded])
  end <- max(conLoadedPages$time[loaded])
  
  trimmed <- eventsDf[((eventsDf$event_start > start) & (eventsDf$event_end < end)),]
  trimmed 
}


analyse <- function(testDir, testId, recording, threshold) {
  log <- createLogDf(testDir, testId)
  expected <- expectedDuration(recording)
  events <- eventTimeSecAll(log, expected)
  con <- conLoadedPages(events)
  trimmed <- trimEvents(events, con, threshold)
  
  trimmed
}




# based on recording, calculate expected event durations
expectedDuration <- function(recordingFile) {
  
  # read in the recording file, create a DF
  # with type and time for each line
  lines <- readr::read_lines(recordingFile, skip = 1)
  res <- list()
  for (i in 1:length(lines)) {
    raw <- jsonlite::fromJSON(lines[i])
    res[[i]] <- list(
      type = raw$type,
      time = lubridate::ymd_hms(raw$created)
    )
  }
  resDf <- do.call(rbind.data.frame, res)
  resDf$time <- lubridate::as_datetime(resDf$time)
  
  # identify the ws_recv and ws_send events
  ws_recv <- which(resDf$type == 'WS_RECV')
  ws_send <- which(resDf$type == 'WS_SEND')
  
  
  # for each ws_recv event (except the first), find the preceeding ws_send event
  # the 'elapsed time' should be the time between the ws_recv and ws_send
  paired <- vector(mode = 'numeric', length = length(ws_recv)-1)
  for (e in 2:length(ws_recv)) { # start at 2 since we skip the first ws_recv
    # calculate the timestamp diff between each we_send and the current ws_recv
    # eligible ws_send events happended BEFORE this ws_recv, so they should have a > delta
    # pick the nearest preceding one - min(delta  > 0)
    delta <- resDf[ws_recv[e],'time'] - resDf[ws_send, 'time']
    paired[e-1] <- ws_send[which(delta == min(delta[delta > 0]))]
  }
  
  # compute the elapsed time 
  event_end <- resDf$time[ws_recv[2:length(ws_recv)]]
  event_start <- resDf$time[paired]
  event_duration_sec <- event_end - event_start
  
  # need to add one since we skipped the first line
  data.frame(
    event_ws_send_line_num = paired+1,
    event_ws_recv_line_num = ws_recv[2:length(ws_recv)] +1,
    exepected_duration_sec = event_duration_sec
  )
  
}


pauseDuration <- function(log){
  pause <- log[which(log$event %in% c('PLAYER_SLEEPBEFORE_START', 'PLAYER_SLEEPBEFORE_END')), c('event', 'time')]
  c <- 1
  for (i in 1:nrow(pause)) {
    pause$id[i] <- c;
    if (!(i %% 2)) {
      c <- c + 1
    }
  }
  pause <- tidyr::spread(pause, event, time)
  pause$sleep_dur_sec <- pause$PLAYER_SLEEPBEFORE_END - pause$PLAYER_SLEEPBEFORE_START
  pause
}


  
