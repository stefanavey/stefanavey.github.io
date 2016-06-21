#################################################################################
## Visualize How Much of The Bile Catholics Hear at Mass                       ##
#################################################################################

#############################
## Load necessary packages ##
#############################
library(openxlsx)                       # for reading excel tables
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)                        # string functions
OTRefFile <- "OldTestamentReference.xlsx"
abbrevFile <- "BibleAbbreviations.xlsx"
LectSundaysFile <- "Lectionary-SundaysAndFeasts.xlsx"
LectWeekdaysFile <- "Lectionary-Weekdays.xlsx"

########################
## Read in references ##
########################
testaments <- c("OT", "NT")
abbrev <- lapply(testaments, function(te) read.xlsx(abbrevFile, sheet = te))
names(abbrev) <- testaments

OTsections <- c("Torah", "Historical", "Wisdom", "MajorProphets", "MinorProphets")
OTRefList <- lapply(OTsections, function(i) read.xlsx(OTRefFile, sheet=i))

tmp <- lapply(OTRefList, function(ref) {
                   res <- apply(ref, 1, function(row) {
                                       book <- row[["Book.Name"]]
                                       nChap <- row[["#.Ch."]]
                                       verses <- rep(NA, nChap)
                                       for(chap in 1:nChap) {
                                         verses[chap] <- as.numeric(row[[as.character(chap)]])
                                       }
                                       data.frame(Book = book,
                                                  Chapter = 1:nChap,
                                                  Verses = verses)
                                     })
                   Reduce(rbind, res)
                 })
OTref <- Reduce(rbind, tmp)

OTref2 <- OTref %>%
  tbl_df() %>%                          # change class for convenience
  mutate(Book = factor(Book,      # Order books in factor levels
             levels = abbrev$OT$Name),
         Abbrv = abbrev$OT$Abbreviation[match(Book, abbrev$OT$Name)])
OTref2

nr <- sum(OTref2$Verses)
cnames <- c(gsub("Verses", "Verse", colnames(OTref2)), "Pos")
lcv <- 1
OTref2Pos <- list()
for(i in 1:nrow(OTref2)) {
  nv <- OTref2$Verses[i]
  inds <- lcv:(lcv+nv-1)
  OTref2Pos[[i]] <- data.frame(Book = as.character(OTref2$Book[i]),
                              Chapter = OTref2$Chapter[i],
                              Verse = 1:OTref2$Verses[i],
                              Abbrv = as.character(OTref2$Abbrv[i]),
                              Pos = inds)
  lcv <- lcv + nv
}
OTref2Pos <- Reduce(rbind, OTref2Pos)


###################################################
## Read in the Lectionary for Sundays and Feasts ##
###################################################
readings <- c("OT", "Psalm", "NT", "Gospel")
LectSundays <- lapply(readings, function(i) read.xlsx(LectSundaysFile, sheet=i))
names(LectSundays) <- readings

OTLect <- rbind(LectSundays$OT, LectSundays$Psalm) %>%
  tbl_df() %>%
  separate(LectNum_Year, c("LectNum", "Year"), sep = '-') %>%
  mutate(YearA = grepl("A", Year),
         YearB = grepl("B", Year),
         YearC = grepl("C", Year))
OTLect

## Can easily now filter by new indicator columns
OTLect %>%
  filter(YearA)                         # only readings from Year A


############################################
## Try out some plotting on a limited set ##
############################################
dat <- OTLect %>%
  filter(YearA) %>%                     # Year A only
  filter(grepl("^Gen ", Reading))       # Genesis
data.frame(dat)

## Functions to take readings and referece and return ranges that are covered

assignChVerse <- function(str, ref, cvs, abbrv) {
  colons <- gregexpr(":", cvs)[[1]]
  if(length(colons) == 0) {
    stop("Must have at least 1 colon")
  } else if (length(colons) == 1) {
      split2 <- strsplit(cvs, ":")
      chapters <- split2[[1]][1]
      vs <- strsplit(split2[[1]][-1], ",")
      verses <- lapply(vs, function(v) {
                         res <- c()
                         for(i in seq_along(v)) {
                           if(grepl("-", v[i])) {
                             tmpSplit <- strsplit(v[i], "-")[[1]]
                             res <- c(res, as.numeric(tmpSplit[1]):as.numeric(tmpSplit[2]))
                           } else {
                               res <- c(res, as.numeric(v[i]))
                             }
                         }
                         return(res)
                       })
    } else if (length(colons) == 2) {
        if(grepl(",", cvs)) {
          warning("Not currently handling commas and 2 colons!")
          return(list(chapters = NA, verses = NA))
        }
        split2 <- strsplit(cvs, "-", fixed=TRUE) # assumes that this is before any commas
        split3 <- strsplit(split2[[1]], ":")
        chapters <- sapply(split3, '[[', 1)
        tmpVerses <- sapply(split3, '[[', 2)
        numVerses <- filter(ref, Abbrv == abbrv,
                            Chapter == as.numeric(chapters[1]))$Verses
        verses <- list()
        verses[[1]] <- as.numeric(tmpVerses[1]):numVerses
        verses[[2]] <- 1:as.numeric(tmpVerses[2])
      } else {
          print(cvs)
          stop("I didn't know it was possible to have more than 1 colon here!")
        }
  return(list(chapters = chapters, verses = verses))
}

##' @param str a character string specifying the reading in a format like
##'            [bookAbbrv] [Chapter]:[Verse]-([Chapter]:)[Verse](,) ([Verse]-[Verse]) (;) ...
##' @param ref a reference data frame specifying the number of chapters and verses in each book
##' @details Assumes that the str only contains a single book abbrviation.
##' @return a data frame that contains one row per verse and the following columns:
##'         Abbrv: e.g. "Gen"
##'         Chapter: Chapter number
##'         Verse: A sinle verse
readingParser <- function(str, ref) {
  print(str)
  ## split the string on whitespace
  split1 <- strsplit(str, " ")[[1]]
  ## Search for any 'or' and ignore second part for now
  or <- grep("^or$", split1)
  if(length(or) > 0) {
    split1 <- split1[1:(or-1)]
  }
  ## Book abbreviation should always be the first element
  if(!is.na(suppressWarnings(as.numeric(split1[1])))) {   # book number (e.g. "1 Sam")
    aind <- 1:2
  } else {
      aind <- 1
    }
  abbrv <- paste(split1[aind], collapse = " ")
  ## Get chapters and verses next
  cvs <- paste(split1[-aind], collapse = "")
  ## Remove any letters (like 12:1-4a should be 12:1-4)
  cvs <- gsub("[a-z]", "", cvs)
  cvs <- gsub("+", ",", cvs, fixed=TRUE)  
  ## If semicolon, then things get more complicated
  chvs <- strsplit(cvs, ";")[[1]]
  assList <- list()
  for(i in seq_along(chvs)) {
    assList[[i]] <- assignChVerse(str, ref, chvs[i], abbrv)
  }
  if(any(is.na(sapply(assList, "[[", "chapters")))) {
    result <- data.frame(Abbrv = abbrv,
                         Chapters = NA,
                         Verses = NA)
  } else {
      chapters <- sapply(assList, "[[", "chapters")
      verses <- sapply(assList, "[[", "verses")  
      vls <- sapply(verses, length)
      allChapters <- as.numeric(Reduce(c, lapply(seq_along(chapters), function(i) {
                                                            rep(chapters[i], each = vls[i])
                                                          })))
      allVerses <- Reduce(c, verses)
      result <- data.frame(Abbrv = abbrv,
                           Chapters = allChapters,
                           Verses = allVerses)
    }
  return(result)
}

############################################################
## Test out parser on all readings to see which ones fail ##
############################################################
tmp <- lapply(OTLect$Reading, readingParser, ref = OTref2)
tmp <- lapply(seq_along(tmp), function(i) {
                         cbind(OTLect[i,], tmp[[i]])
              })
OTLectRanges <- Reduce(rbind, na.omit(tmp))

## Now need to assign "Pos" to reference and then add
## a counter for plotting that counts how many times each
## row is in the reference and plot the 'reference'

pos <- rep(NA, nrow(OTLectRanges))
for(i in 1:nrow(OTLectRanges)) {
  index <- which(as.character(OTref2Pos$Abbrv) == OTLectRanges$Abbrv[i] &
                    OTref2Pos$Chapter == OTLectRanges$Chapter[i] &
                    OTref2Pos$Verse == OTLectRanges$Verses[i])
  if(length(index) == 0) {
    print(i)
    show(OTLectRanges[i,])
    warning("no position set")
  } else {
      pos[i] <- index
    }
}
## Some errors here. Some are due to my bad parsing and some are ambiguous while some may not exist!

OTLectRanges <- cbind(OTLectRanges, Pos = pos)

##############################################################
## Test out some basic visualizations to find the right one ##
##############################################################
## Fake Data
fake <- data.frame(Book = factor(1),
                   Chap = factor(c(rep(1, 10), rep(2, 6))),
                   Verses = c(1:10, 15:20),
                   Pos = c(1:10, 55:60))

ggplot(fake, aes(x = Pos, fill = Chap)) +
  geom_histogram(binwidth = 1) +
  facet_grid(Book ~ .)



## Multiple plot function
## http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
##
## ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
## - cols:   Number of columns in layout
## - layout: A matrix specifying the layout. If present, 'cols' is ignored.
##
## If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
## then plot 1 will go in the upper left, 2 will go in the upper right, and
## 3 will go all the way across the bottom.
##
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  ## Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)

  ## If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
                                        # Make the panel
                                        # ncol: Number of columns of plots
                                        # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
                                        # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

                                        # Make each plot, in the correct location
      for (i in 1:numPlots) {
                                        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                              layout.pos.col = matchidx$col))
      }
    }
}



## Real Data
fname <- "Barplot_days=Sundays_books=OT.pdf"
pdf(fname, width = 6, height = 12)
for(ab in unique(OTLectRanges$Abbrv)) {
  yearCols <- c("YearA", "YearB", "YearC")
  ggList <- list()
  for(year in yearCols) {
    rng <- which(OTref2Pos$Abbrv == ab)
    cs <- cumsum(OTref2$Verses[OTref2$Abbrv == ab]) # cumulative sum
    chapStarts <- c(1, cs[-length(cs)] + 1)
    posStart <- min(OTref2Pos$Pos[OTref2Pos$Abbrv == ab])
    breaks <- chapStarts + posStart
    labels <- as.character(1:length(breaks))
    dat <- OTLectRanges %>% filter(Abbrv == ab) %>%
      filter_(year) %>%
      mutate(Chapters = as.factor(Chapters)) %>%
      select(Abbrv, Chapters, Verses, Pos) %>%
      distinct()      # ignore how many times something appeared (read 2 or 3 times)
    ggList[[year]] <- ggplot(dat, aes(x = Pos)) +
      geom_histogram(binwidth = 1, aes(fill = Chapters),
                     color = "black", fill = "black", show.legend = FALSE) +
      coord_flip() +
      scale_x_continuous(limits = c(min(rng), max(rng)), breaks = breaks,
                         labels = labels) +
      scale_y_continuous(breaks = NULL) +
      xlab("Chapter") +
      ylab(year) +
      ## coord_polar(theta = "x") +
      ggtitle(ab)
  }
  multiplot(plotlist = ggList, cols = 3)
}
dev.off()



ab <- "Gen"
rng <- which(OTref2Pos$Abbrv == ab)
cs <- cumsum(OTref2$Verses[OTref2$Abbrv == ab]) # cumulative sum
chapStarts <- c(1, cs[-length(cs)] + 1)
posStart <- min(OTref2Pos$Pos[OTref2Pos$Abbrv == ab])
breaks <- chapStarts + posStart
labels <- as.character(1:length(breaks))
dat <- OTLectRanges %>% filter(Abbrv == ab) %>%
  mutate(Chapters = as.factor(Chapters)) %>%
  distinct()      # ignore how many times something appeared (read 2 or 3 times)
gg <- ggplot(dat, aes(x = Pos)) +
  geom_histogram(binwidth = 1, aes(fill = Chapters), show.legend = FALSE) +
  coord_flip() +
  scale_x_continuous(limits = c(min(rng), max(rng)), breaks = breaks,
                     labels = labels) +
  scale_y_continuous(breaks = NULL) +
  xlab("Chapter") +
  ylab("") +
  ## coord_polar(theta = "x") +
  ggtitle(ab)
plot(gg)
