
#' Send query to an Air Sciences AWS database
#' 
#' \code{query_db} sends the query argument to an Air Sciences database hostes 
#' on Amazon Web Services.
#' 
#'@param db String. Name of database to access, either "owenslake" or 
#'"saltonsea"
#'@param query String. Query string to send to database.
#'@param db_host, db_password, db_user, db_port String. PSQL connection 
#'parameters. 
query_db <- function(db, query, no_message=F){
    if (!(db %in% c("owenslake", "saltonsea"))){
        stop("Incorrect database name!")
    }
    db_user <- Sys.getenv("PSQL_USER")
    db_port <- Sys.getenv("PSQL_PORT")
    if (db=="owenslake"){
        db_host <- Sys.getenv("PSQL_HOST_OWENS")
        db_password <- Sys.getenv("PSQL_PASSWORD_OWENS")
    }
    if (db=="saltonsea"){
        db_host <- Sys.getenv("PSQL_HOST_SS")
        db_password <- Sys.getenv("PSQL_PASSWORD_SS")
    }
    con <- DBI::dbConnect("PostgreSQL", host=db_host, port=db_port,
                                  dbname=db, user=db_user, password=db_password)
    if (no_message){
    dat <- suppressWarnings(DBI::dbGetQuery(con, query))
    } else{
    dat <- DBI::dbGetQuery(con, query)
    }
    DBI::dbDisconnect(con)
    dat
}

upsert_2_db <- function(db, tbl, df1, cols2char, key_cols){
    for (j in cols2char){
        df1[ , j] <- sapply(df1[ , j], function(x) ifelse(is.na(x), NA, 
                                                          paste0("'", x, "'")))
    }
    df1[is.na(df1)] <- 'NULL'
    query_data <- c()
    for (i in 1:nrow(df1)){
        query_data[i] <- paste0("(", paste(df1[i, ], collapse=", "), ")")
    }
    cols <- query_db(db, paste0("SELECT column_name FROM information_schema.columns ", 
                                "WHERE table_name='", strsplit(tbl, "\\.")[[1]][2], "' ", 
                                "AND table_schema='", strsplit(tbl, "\\.")[[1]][1], "';"))
    data_cols <- cols[cols!=key_cols]
    data_string <- paste(paste0(data_cols, "=newvals.", data_cols), collapse=", ")
    key_string <- paste(paste0("newvals.", key_cols, "=", tbl, ".", key_cols), collapse=" AND ")
    full_string <- paste(paste0("newvals.", cols$column_name), collapse=", ")
    null_string <- paste(paste0(tbl, ".", key_cols, " IS NULL"), collapse=" AND ")
    upsert_query <- paste0("BEGIN;", 
                           "CREATE TEMPORARY TABLE newvals ",
                           "AS SELECT * FROM ", tbl, " WITH NO DATA;", 
                           "INSERT INTO newvals ",
                           "VALUES ", paste(query_data, collapse=", "), ";", 
                           "LOCK TABLE ", tbl, " IN EXCLUSIVE MODE;",
                           "UPDATE ", tbl, " ", 
                           "SET ", data_string, " ", 
                           "FROM newvals ",
                           "WHERE ", key_string, ";", 
                           "INSERT INTO ", tbl, " ", 
                           "SELECT ", full_string, " ", 
                           "FROM newvals ",
                           "LEFT OUTER JOIN ", tbl, " ", 
                           "ON ", key_string, " ",
                           "WHERE ", null_string, ";", 
                           "COMMIT;")
    query_db(db, upsert_query)
}

S3_bucket_access <- function(hostname="s3-us-west-2.amazonaws.com", 
                             bucket, key, file){
    aws_access <- Sys.getenv("AWS_ACCESS_KEY")
    aws_secret <- Sys.getenv("AWS_SECRET_KEY")
    RS3::S3_connect(aws_access, aws_secret, hostname)
    RS3::S3_get_object(bucket, key, file)
}
