##Men's
library(XML)

menURLs = 
  c("results/1999/cb99m.html", "results/2000/Cb003m.htm", "results/2001/oof_m.html",
    "results/2002/oofm.htm", "results/2003/CB03-M.HTM",
    "results/2004/men.htm", "results/2005/CB05-M.htm", 
    "results/2006/men.htm", "results/2007/men.htm", 
    "results/2008/men.htm", "results/2009/09cucb-M.htm",
    "results/2010/2010cucb10m-m.htm", 
    "results/2011/2011cucb10m-m.htm",
    "results/2012/2012cucb10m-m.htm")
ubase = "http://www.cherryblossom.org/"
urls = paste(ubase, menURLs, sep = "")

urls[1:3]
years = 1999:2012

extractMenResTable =
  #
  # Retrieve data from web site, 
  # find the preformatted text,
  # and write lines or return as a character vector.
  #
  function(url = "http://www.cherryblossom.org/results/2009/09cucb-F.htm",
           year = 1999, sex = "male", file = NULL)
  {
    doc = htmlParse(url)
    
    if (year == 2000) {
      
      # Get preformatted text from 4th font element
      # The top file is ill formed so the <pre> search doesn't work.
      ff = getNodeSet(doc, "//font")
      txt = xmlValue(ff[[4]])
      els = strsplit(txt, "\r\n")[[1]]
    }
    else if (year == 2009 & sex == "male") {
      # Get preformatted text from <div class="Section1"> element
      # Each line of results is in a <pre> element
      div1 = getNodeSet(doc, "//div[@class='Section1']")
      pres = getNodeSet(div1[[1]], "//pre")
      els = sapply(pres, xmlValue)
    }
    else if (year == 1999 & sex == "male") {
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


menTables = mapply(extractMenResTable, url = urls, year = years)
names(menTables) = years
sapply(menTables, length)

menTables$'2012'[-1:-8]


#m2012 = read.table(file="MenTxt/2012.txt", skip = 8)
m2012 = menTables$'2012'[-1:-8]

#els = readLines("MenTxt/2012.txt")
els = menTables$'2012'

els[1:10]

els2011 = menTables$'2011'
els2011[1:10]

eqIndex = grep("^===", els)
eqIndex

first3 = substr(els, 1, 3)
which(first3 == "===")

spacerRow = els[eqIndex]
headerRow = els[eqIndex - 1]
body = els[ -(1:eqIndex) ]

headerRow = tolower(headerRow)

ageStart = regexpr("ag", headerRow)
ageStart

age = substr(body, start = ageStart, stop = ageStart + 1)
head(age)

summary(as.numeric(age))

blankLocs = gregexpr(" ", spacerRow)
blankLocs

searchLocs = c(0, blankLocs[[1]])

Values = mapply(substr, list(body), 
                start = searchLocs[ -length(searchLocs)] + 1, 
                stop = searchLocs[ -1 ] - 1)

findMenColLocs = function(spacerRow) {
  
  spaceLocs = gregexpr(" ", spacerRow)[[1]]
  rowLength = nchar(spacerRow)
  
  if (substring(spacerRow, rowLength, rowLength) != " ")
    return( c(0, spaceLocs, rowLength + 1))
  else return(c(0, spaceLocs))
}

selectMenCols = 
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

searchLocs = findMenColLocs(spacerRow)
ageLoc = selectMenCols("ag", headerRow, searchLocs) 
ages = mapply(substr, list(body), 
              start = ageLoc[1,], stop = ageLoc[2, ])

summary(as.numeric(ages))

shortColNames = c("name", "home", "ag", "gun", "net", "time")

locCols = selectMenCols(shortColNames, headerRow, searchLocs)

Values = mapply(substr, list(body), start = locCols[1, ], 
                stop = locCols[2, ])

class(Values)

colnames(Values) = shortColNames
head(Values)

tail(Values)[ , 1:3]

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


mfilenames = paste("/Users/stevencocke/Desktop/QTW/Unit 3-4/MenText/", 1999:2012, ".txt", sep = "")
write(menTables$'1999',mfilenames[1])
write(menTables$'2000',mfilenames[2])
write(menTables$'2001',mfilenames[3])
write(menTables$'2002',mfilenames[4])
write(menTables$'2003',mfilenames[5])
write(menTables$'2004',mfilenames[6])
write(menTables$'2005',mfilenames[7])
write(menTables$'2006',mfilenames[8])
write(menTables$'2007',mfilenames[9])
write(menTables$'2008',mfilenames[10])
write(menTables$'2009',mfilenames[11])
write(menTables$'2010',mfilenames[12])
write(menTables$'2011',mfilenames[13])
write(menTables$'2012',mfilenames[14])


mfilenames = paste("/Users/stevencocke/Desktop/QTW/Unit 3-4/MenText/", 1999:2012, ".txt", sep = ";")
menFiles = lapply(mfilenames, readLines)
names(menFiles) = 1999:2012



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
wfilenames = paste("/Users/stevencocke/Desktop/QTW/Unit 3-4/WomenText/", 1999:2012, ".txt", sep = "")
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
#write(womenTables$'1999', file = "/Users/stevencocke/Desktop/QTW/Unit 3-4/WomenText/")

womenFiles = lapply(wfilenames, readLines)
names(womenFiles) = 1999:2012

#look at 2012 file to see how to extract variables from the text file
#first 8 lines are not part of the data
els = readLines("/Users/stevencocke/Desktop/QTW/Unit 3-4/WomenText/2012.txt")
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
  function(file)
  {
    ##I was trying to parse by the first row since there are no headers and then add that row back to the body
    ##with the body= call being altered
    if (length(grep("^===", file)) == 0) {
      varNames = c("1", "6002 Elana MEYER", "34", "Rep Of S.africa", "52:15", "52:16#")
      eqIndex = grep("Elana", file)
      # Extract the two key rows and the data
      spacerRow = file[eqIndex - 1]
      headerRow = file[eqIndex]
      body = file[ -(1 : eqIndex-1) ]
      # Obtain the starting and ending positions of variables
      searchLocs = findColLocs(spacerRow)
      locCols = selectCols(varNames, headerRow, searchLocs)
      
      Values = mapply(substr, list(body), start = locCols[1, ], 
                      stop = locCols[2, ])
      colnames(Values) = c("name", "home", "ag", "gun",
                           "net", "time")
      
      invisible(Values)
      
      
    }
    
    else {
    # Find the index of the row with =s
    eqIndex = grep("^===", file)
    varNames =c("name", "home", "ag", "gun",
                "net", "time")
    # Extract the two key rows and the data
    spacerRow = file[eqIndex] 
    headerRow = tolower(file[ eqIndex - 1 ])
    body = file[ -(1 : eqIndex) ]
    # Obtain the starting and ending positions of variables
    searchLocs = findColLocs(spacerRow)
    locCols = selectCols(varNames, headerRow, searchLocs)
    
    Values = mapply(substr, list(body), start = locCols[1, ], 
                    stop = locCols[2, ])
    colnames(Values) = c("name", "home", "ag", "gun",
                         "net", "time")
    
    invisible(Values)
    }
    
  }

##this works
womenResMat = lapply(womenFiles, extractVariables)

##this doesn't work
womenResMat[3]

##the rest do work just not for 2001
womenResMat[2]

typeof(womenFiles)

p= extractVariables(womenFiles[[1]])


eqIndex = grep("Elana", womenFiles[[3]]) - 1










##testing
testextractVariables = 
  function(file)
  {
  varNames = c("1", "6002 Elana MEYER", "34", "Rep Of S.africa", "52:15", "52:16#")
  eqIndex = grep("Elana", womenFiles[[3]])
  # Extract the two key rows and the data
  spacerRow = womenFiles[[3]][eqIndex - 1]
  headerRow = womenFiles[[3]][eqIndex]
  body = womenFiles[[3]][ -(1 : eqIndex-1) ]
  # Obtain the starting and ending positions of variables
  searchLocs = findColLocs(spacerRow)
  locCols = selectCols(varNames, headerRow, searchLocs)
  
  Values = mapply(substr, list(body), start = locCols[1,], 
                  stop = locCols[2, ])
  colnames(Values) = c("name", "home", "ag", "gun",
                       "net", "time")
  
  invisible(Values)
  }

womenResMat = testextractVariables(womenFiles[[3]])



## test on another year
testextractVariables = 
  function(file)
  {
    varNames =c("name", "home", "ag", "gun",
                "net", "time")
    eqIndex = grep("^===", womenFiles[[2]])
    # Extract the two key rows and the data
    spacerRow = womenFiles[[2]][eqIndex] 
    headerRow = tolower(womenFiles[[2]][ eqIndex - 1 ])
    body = womenFiles[[2]][ -(1 : eqIndex) ]
    # Obtain the starting and ending positions of variables
    searchLocs = findColLocs(spacerRow)
    locCols = selectCols(varNames, headerRow, searchLocs)
    
    Values = mapply(substr, list(body), start = locCols[1, ], 
                    stop = locCols[2, ])
    colnames(Values) = c("name", "home", "ag", "gun",
                         "net", "time")
    
    invisible(Values)
  }