dataPath <- "C:\\Users\\Steve\\Documents\\R\\Tessera\\Data"
fileName <- "trip_1.csv"

load.raw <- function(path, file.name, nrows = -1, slash = "\\"){
  ## Function reads the rather large raw taxi data file 
  ## as a data.table object.
  require(data.table)
  file.name <- paste(path, slash, file.name, sep = "")
  fread(file.name, nrows = nrows)
}


#taxi <- load.raw(dataPath, fileName)
#taxi <- taxi[pickup_datetime < "2013-01-09 00:00:00", ]
#taxi <- data.frame(taxi)
#write.csv(taxi, file = paste(dataPath, "\\", "taxiOneWeek.csv", sep =""))



load.ddf <- function(path, in.file, out.file, slash = "\\"){
  ## Function creates a ddf from a .csv file of the 
  ## taxi data.
  require(datadr)
  in.file <- paste(path, slash, in.file, sep = "")
  out.file <- paste(path, slash, out.file, sep = "")
  disk.con <- localDiskConn(out.file, autoYes = TRUE)
  drRead.csv(in.file, output = disk.con, header = TRUE, 
             stringsAsFactors = FALSE) 
}

in.file <-  "taxiOneWeek.csv"
out.file <- "taxiddf"
taxi <- load.ddf(dataPath, in.file, out.file)