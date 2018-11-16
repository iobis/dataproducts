require(RPostgreSQL)
require(reshape2)

host <- "obisdb-stage.vliz.be"
db <- "obis"
user <- "obisadmin"
#password <- ""
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname=db, host=host, user=user, password=password)

endyear <- 2018

make <- function(variable, categories) {

  query_fun <- sprintf("create or replace function depthcat(depth double precision) returns int as $$ declare categories int[] := array[%s]; begin for i in array_lower(categories, 1) .. array_upper(categories, 1) loop if depth < categories[i] then return i - 1; end if; end loop; return NULL; end; $$ language 'plpgsql';", paste(categories, collapse = ", "))
  if (is.null(endyear)) {
    query <- paste0("select depthcat(depth) as depth, depthcat(bottomdepth) as bottomdepth, count(*) as records, count(distinct(valid_id)) as taxa from obis.drs left join obis.positions on positions.id = drs.position_id where depth >= 0 and bottomdepth >= 0 and depthcat(depth) is not null and depthcat(bottomdepth) is not null group by depthcat(depth), depthcat(bottomdepth);")
  } else {
    query <- paste0("select depthcat(drs.depth) as depth, depthcat(bottomdepth) as bottomdepth, count(*) as records, count(distinct(drs.valid_id)) as taxa from obis.drs left join obis.dxs on dxs.dr_id = drs.id left join obis.positions on positions.id = drs.position_id where yearcollected <= '", endyear, "' and drs.depth >= 0 and bottomdepth >= 0 and depthcat(drs.depth) is not null and depthcat(bottomdepth) is not null group by depthcat(drs.depth), depthcat(bottomdepth);")
  }
  
  dbGetQuery(con, query_fun)
  obis <- dbGetQuery(con, query)
  obis$depth <- categories[obis$depth]
  obis$bottomdepth <- categories[obis$bottomdepth]

  obis <- dcast(obis, depth ~ bottomdepth, value.var = variable)
  row.names(obis) <- obis[,1]
  obis <- obis[,2:ncol(obis)]
  
  for (i in 1:(ncol(obis)-1)) {
    obis[i, i] <- sum(obis[i:nrow(obis), i], na.rm = TRUE)
    obis[(i+1):nrow(obis), i] <- NA
  }
  
  obis <- as.matrix(obis)
  obis <- obis[nrow(obis):1,]
  
  return(obis)
}

records <- make("records", categories)
save(records, file = paste0("data/records_", endyear, ".dat"))
records_fine <- make("records", finecategories)
save(records_fine, file = paste0("data/records_fine_", endyear, ".dat"))

#taxa <- make("taxa", categories)
#save(taxa, file = "data/taxa.dat")
#taxa_fine <- make("taxa", finecategories)
#save(taxa_fine, file = "data/taxa_fine.dat")

