library(beepr)

start <- Sys.time()
end <- start + lubridate::dminutes(30)

while(Sys.time() < end) {
  Sys.sleep(rnorm(1,25))
  beep()
}

beep(3)
