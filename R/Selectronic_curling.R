library(tidyverse)
library(httr)
library(glue)
library(duckdb)
library(DBI)
library(lubridate)
library(clock)

source("./R/secrets.R")

src_tz <- "UTC"
tz <- Sys.timezone()

duckdb_path <- "~/SProKit-data/Selectronics_data.duckdb"

login_url <- "https://select.live/login"

getdata_url_template <- "https://select.live/dashboard/getdata/{site_id}"

hfdata_url_template <- "https://select.live/dashboard/hfdata/{site_id}"

ua <- user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 12_1) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/15.0 Safari/605.1.15")

authenticate <- function(login_url=NULL, email=NULL, password=NULL) {
    if (is.null(login_url) | is.null(email) | is.null(password)) {
        stop("Login page URL, email address and password for Select.live must all be supplied")
    }
    login_credentials <- list(email = email, pwd = password)
    attempts <- 0
    status <- 0
    success <- FALSE
    while (status != 200 | !success) {
        backoff_delay <- 2^min(12,attempts) # seconds
        attempts <- attempts + 1
        try(login_resp <- httr::POST(login_url, body = login_credentials))
        status <- httr::status_code(login_resp)
        # message("Email: ", email, ", Password: ", password)
        # message("Status: ", status, ", Backoff delay: ", backoff_delay, ", Attempts: ", attempts)
        if (status == 200) {
            mojo_cookie_value <- cookies(login_resp) %>%
                                        filter(name == "mojolicious") %>%
                                        pull(value)
            if (length(mojo_cookie_value) > 0) {
                success <- TRUE
                auth_cookie <- httr::set_cookies("mojolicious" = mojo_cookie_value)
                return(list(auth_cookie=auth_cookie, status=status, attempts=attempts))
            }
        }
        if (attempts > 100) {
            return(list(auth_cookie=NULL, status=status, attempts=attempts))
        }
        Sys.sleep(backoff_delay)
    }
}

sl_auth <- authenticate(login_url=login_url, email=email, password=password)

get_hfdata <- function(hfdata_url=glue(hfdata_url_template), sl_cookie=sl_auth[["auth_cookie"]]) {
    status <- 0
    try(
        {hfdata_resp <- GET(hfdata_url, sl_cookie , ua, timeout(30))
         status <- httr::status_code(hfdata_resp)}
        )
    if (status == 200) {
        hfdata_tibble <- httr::content(hfdata_resp, "parsed")[["items"]] %>%
                            as_tibble() %>%
                            mutate(timestamp = as.POSIXct(timestamp, origin="1970-01-01")) %>%
                            select(timestamp, everything())
        if (nrow(hfdata_tibble) > 0) return(hfdata_tibble)
    } else {
        return(NULL)
    }
}

get_getdata <- function(getdata_url=glue(getdata_url_template), sl_cookie=sl_auth[["auth_cookie"]]) {
    status <- 0
    try(
        {getdata_resp <- GET(getdata_url, sl_cookie , ua, timeout(30))
         status <- httr::status_code(getdata_resp)}
        )
    if (status == 200) {
        getdata_tibble <- content(getdata_resp, "parsed")[["items"]] %>%
                            as_tibble() %>%
                            rename(saved="__saved") %>%
                            mutate(fetched = as.POSIXct(content(getdata_resp, "parsed")[["now"]],
                                                        origin="1970-01-01"),
                                   saved = as.POSIXct(saved, origin="1970-01-01"),
                                   timestamp = as.POSIXct(timestamp, origin="1970-01-01"),
                                   ts_epoch = as.POSIXct(ts_epoch, origin="1970-01-01")) %>%
                            select(-ts) %>%
                            select(timestamp, saved, ts_epoch, everything())
        if (nrow(getdata_tibble) > 0) return(getdata_tibble)
    } else {
        return(NULL)
    }
}

update_hfdata <- function(interval=10, hf_counter=0) {
        hf_counter <- hf_counter + 1
        con_duck <- dbConnect(duckdb::duckdb(), duckdb_path, read_only=FALSE)
        hfdata_tibble <- get_hfdata(hfdata_url=glue(hfdata_url_template), sl_cookie=sl_auth[["auth_cookie"]])
        if (!is.null(hfdata_tibble)) {
            dbWriteTable(con_duck, "hfdata", hfdata_tibble, append=TRUE)
            hfdata <- tbl(con_duck, 'hfdata')
            row_count <- hfdata %>% count() %>% collect() %>% pull()
            message("hfdata fetch succeeded at ", clock::date_now(tz),
                    " row count is ", row_count, ", counter is ", hf_counter)
            dbWriteTable(con_duck, "fetch_failures",
                         tibble(type="hfdata",
                                failure=FALSE,
                                timestamp=clock::date_now(tz)),
                         append=TRUE)
        } else {
            message("hfdata fetch failed at ", clock::date_now(tz))
            dbWriteTable(con_duck, "fetch_failures",
                         tibble(type="hfdata",
                                failure=TRUE,
                                timestamp=clock::date_now(tz)),
                         append=TRUE)
            message("Trying re-authentication at ", clock::date_now(tz))
            eval(quote(sl_auth <- authenticate(login_url=login_url, email=email, password=password)), parent.frame())
        }
        duckdb::dbDisconnect(con_duck, shutdown=TRUE)
        later::later(update_hfdata, interval)
}

update_getdata <- function(interval=600) {
        con_duck <- dbConnect(duckdb::duckdb(), duckdb_path, read_only=FALSE)
        getdata_tibble <- get_getdata(getdata_url=glue(getdata_url_template), sl_cookie=sl_auth[["auth_cookie"]])
        if (!is.null(getdata_tibble)) {
            dbWriteTable(con_duck, "getdata", getdata_tibble, append=TRUE)
            message("getdata fetch succeeded at ", clock::date_now(tz))
            dbWriteTable(con_duck, "fetch_failures",
                         tibble(type="getdata",
                                failure=FALSE,
                                timestamp=clock::date_now(tz)),
                         append=TRUE)
        } else {
            message("getdata fetch failed at ", clock::date_now(tz))
            dbWriteTable(con_duck, "fetch_failures",
                         tibble(type="getdata",
                                failure=TRUE,
                                timestamp=clock::date_now(tz)),
                         append=TRUE)
            message("Trying re-authentication at ", clock::date_now(tz))
            eval(quote(sl_auth <- authenticate(login_url=login_url, email=email, password=password)), parent.frame())
        }
        duckdb::dbDisconnect(con_duck, shutdown=TRUE)
        later::later(update_getdata, interval)
}

update_hfdata()
update_getdata()

# hfdata <- tbl(con_duck, 'hfdata')
# getdata <- tbl(con_duck, 'getdata')
# fetch_failures <- tbl(con_duck, 'fetch_failures')



# hf <- hfdata %>% collect()
# gd <- getdata %>% collect()
# ff <- fetch_failures %>% collect()
