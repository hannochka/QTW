#http://www.cherryblossom.org/results/1999/cb99f.html
#http://www.cherryblossom.org/results/2000/Cb003f.htm
#http://www.cherryblossom.org/results/2001/oof_f.html
#http://www.cherryblossom.org/results/2002/ooff.htm
#http://www.cherryblossom.org/results/2003/CB03-F.HTM
#http://www.cherryblossom.org/results/2004/women.htm
#http://www.cherryblossom.org/results/2005/CB05-F.htm
#http://www.cherryblossom.org/results/2006/women.htm
#http://www.cherryblossom.org/results/2007/women.htm
#http://www.cherryblossom.org/results/2008/women.htm
#http://www.cherryblossom.org/results/2009/09cucb-F.htm
#http://www.cherryblossom.org/results/2010/2010cucb10m-f.htm
#http://www.cherryblossom.org/results/2011/2011cucb10m-f.htm
#http://www.cherryblossom.org/results/2012/2012cucb10m-f.htm
#2013 results are weird?

library(XML)

#all urls from womens 10k results
womenURLs = 
  c("results/1999/cb99f.html", "results/2000/Cb003f.htm", "results/2001/oof_f.html",
    "results/2002/ooff.htm", "results/2003/CB03-F.HTM",
    "results/2004/women.htm", "results/2005/CB05-F.htm", 
    "results/2006/women.htm", "results/2007/women.htm", 
    "results/2008/women.htm", "results/2009/09cucb-F.htm",
    "results/2010/2010cucb10m-f.htm", 
    "results/2011/2011cucb10m-f.htm",
    "results/2012/2012cucb10m-f.htm")
ubase = "http://www.cherryblossom.org/"
urls = paste(ubase, womenURLs, sep = "")

urls[1:3]
years = 1999:2012

#this gets all the tables from the above urls 
extractResTable =
  #
  # Retrieve data from web site, 
  # find the preformatted text,
  # and write lines or return as a character vector.
  #
  function(url = "http://www.cherryblossom.org/results/1999/cb99f.html",
           year = 1999, sex = "female", file = NULL)
  {
    doc = htmlParse(url)
    
    if (year == 2000) {
      
      # Get preformatted text from 4th font element
      # The top file is ill formed so the <pre> search doesn't work.
      ff = getNodeSet(doc, "//font")
      txt = xmlValue(ff[[4]])
      els = strsplit(txt, "\r\n")[[1]]
    }
    
    else if (year == 1999 & sex == "female") {
      # Get preformatted text from <pre> elements
      pres = getNodeSet(doc, "//pre")
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\n")[[1]]   
    }     
    else {
      # Get preformatted text from <pre> elements
      pres = getNodeSet(doc, "//pre")
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\r\n")[[1]]   
    } 
    
    if (is.null(file)) return(els)
    # Write the lines as a text file.
    writeLines(els, con = file)
  }

#use mapply with the function created to get all the tables together and then name each one by year
womenTables = mapply(extractResTable, url = urls, year = years)
names(womenTables) = years
sapply(womenTables, length)

#extractResTable(urls[1], year = 1999)  

womenTables$'2012'[-1:-8]

#write .txt files into folder WomenTxt and call each one by year
wfilenames = paste("~/Desktop/WomenTxt/", 1999:2012, ".txt", sep = "")
write(womenTables$'1999',wfilenames[1])
write(womenTables$'2000',wfilenames[2])
write(womenTables$'2001',wfilenames[3])
write(womenTables$'2002',wfilenames[4])
write(womenTables$'2003',wfilenames[5])
write(womenTables$'2004',wfilenames[6])
write(womenTables$'2005',wfilenames[7])
write(womenTables$'2006',wfilenames[8])
write(womenTables$'2007',wfilenames[9])
write(womenTables$'2008',wfilenames[10])
write(womenTables$'2009',wfilenames[11])
write(womenTables$'2010',wfilenames[12])
write(womenTables$'2011',wfilenames[13])
write(womenTables$'2012',wfilenames[14])

#if above doesn't work - look at it manually
#write(womenTables$'1999', file = "~/Desktop/WomenTxt/1999.txt")

womenFiles = lapply(wfilenames, readLines)
names(womenFiles) = 1999:2012

#look at 2012 file to see how to extract variables from the text file
#first 8 lines are not part of the data
els = readLines("~/Desktop/WomenTxt/2012.txt")
els[1:10]

eqIndex = grep("^===", els)
eqIndex
first3 = substr(els,1,3)
which(first3 == "===")
spacerRow = els[eqIndex]
headerRow = els[eqIndex - 1]
body = els[-(1:eqIndex)]
head(body)

#some of them are upper/ lower case in different files and we want them to be the same
headerRow = tolower(headerRow)

ageStart = regexpr('ag', headerRow)
ageStart

age = substr(body, start = ageStart, stop = ageStart +1)
head(age)

blankLocs = gregexpr(" ", spacerRow)
blankLocs

searchLocs = c(0,blankLocs[[1]])
searchLocs

Values = mapply(substr, list(body), start = searchLocs[-length(searchLocs)] + 1, stop = searchLocs[-1] - 1)
length(Values)

#create function to find column locations
findColLocs = function(spacerRow) {
  
  spaceLocs = gregexpr(" ", spacerRow)[[1]]
  rowLength = nchar(spacerRow)
  
  if (substring(spacerRow, rowLength, rowLength) != " ")
    return( c(0, spaceLocs, rowLength + 1))
  else return(c(0, spaceLocs))
}

locs2012 <- findColLocs(spacerRow)
length(2012)
length(locs2012)
locs2012

selectCols = 
  function(colNames, headerRow, searchLocs) 
  {
    sapply(colNames, 
           function(name, headerRow, searchLocs)
           {
             startPos = regexpr(name, headerRow)[[1]]
             if (startPos == -1) 
               return( c(NA, NA) )
             
             index = sum(startPos >= searchLocs)
             c(searchLocs[index] + 1, searchLocs[index + 1] - 1)
           },
           headerRow = headerRow, searchLocs = searchLocs )
  }

ageLoc = selectCols("ag", headerRow, searchLocs)
ages = mapply(substr, list(body), start = ageLoc[1,], stop = ageLoc[2,])
summary(as.numeric(ages))

shortColNames = c("name", "home", "age", "gun", "net", "time")
locCols = selectCols(shortColNames, headerRow, searchLocs)
Values = mapply(substr, list(body), start = locCols[1,], stop = locCols[2,])
class(Values)

colnames(Values) = shortColNames
head(Values)
tail(Values)

#create function that does above - extract relevant values 
extractVariables = 
  function(file, varNames =c("name", "home", "ag", "gun",
                             "net", "time"))
  {
    # Find the index of the row with =s
    eqIndex = grep("^===", file)
    # Extract the two key rows and the data
    spacerRow = file[eqIndex] 
    headerRow = tolower(file[ eqIndex - 1 ])
    body = file[ -(1 : eqIndex) ]
    
    # Obtain the starting and ending positions of variables
    searchLocs = findColLocs(spacerRow)
    locCols = selectCols(varNames, headerRow, searchLocs)
    
    Values = mapply(substr, list(body), start = locCols[1, ], 
                    stop = locCols[2, ])
    colnames(Values) = varNames
    
    invisible(Values)
  }

womenResMat = lapply(womenFiles, extractVariables) # this doesn't work

extractVariables(womenFiles[2]) #for some reason I cannot index within each file

#left off with this issue. 


