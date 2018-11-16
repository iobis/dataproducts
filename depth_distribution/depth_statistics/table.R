library(RPostgreSQL)
library(dplyr)

host <- "obisdb-stage.vliz.be"
db <- "obis"
user <- "obisreader"
password <- "0815r3@d3r"
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname=db, host=host, user=user, password=password)

vol <- 1347000000

volumes <- c(68612044, 259672472, 835662541, 164100714, 1517273)

depths <- data.frame(
  start = c(-10, 200, 1000, 4000, 6000),
  end = c(200, 1000, 4000, 6000, 11000),
  fr = volumes / sum(volumes)
)

results <- NULL

for (d in 1:nrow(depths)) {
  query <- sprintf("select count(*) as records, count(distinct(species_id)) as species, count(distinct(datecollected::timestamp::date)) as days from explore.points where depth > %s and depth <= %s;", depths$start[d], depths$end[d])
  message(query)
  res <- dbGetQuery(con, query)
  results <- bind_rows(results, res)
}

depths$vol <- depths$fr * vol

results / depths$vol * 100000
