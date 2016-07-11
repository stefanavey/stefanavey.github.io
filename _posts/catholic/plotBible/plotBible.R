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
source("helperFunctions.R")
       
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


## Add sections of the Old Testament (Torah, Wisdom, etc.) to OTLectRanges
sections <- lapply(OTRefList, function(l) {
                     books <- l[["Book.Name"]]
                     abbrs <- abbrev$OT$Abbreviation[match(books, abbrev$OT$Name)]
                     return(abbrs)
                   })
names(sections) <- OTsections
## Workaround to match in list:
## http://stackoverflow.com/questions/11002391/fast-way-of-getting-index-of-match-in-list
ss <- rep(seq_along(sections), sapply(sections, length))
OTLectRanges <- OTLectRanges %>%
  mutate(Section = OTsections[ss[match(Abbrv, unlist(sections))]])
  

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


## Plot details for one book
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


                   

## Calculate the position for each section
sectPos <- sapply(OTsections, function(sect) {
                    abbrvs <- sections[[sect]]
                    lastAbbrv <- abbrvs[length(abbrvs)]
                    poss <- OTref2Pos %>%
                      filter(Abbrv == lastAbbrv) %>%
                      select(Pos)
                    maxPos <- max(poss)
                    return(maxPos)
                  })
sectPos2 <- c(0, sectPos[-length(sectPos)])
names(sectPos2) <- names(sectPos)
sectLength <- sectPos - sectPos2

## Plot like a booksehlf
dat <- OTLectRanges %>%
  filter(!is.na(Pos)) %>%
  select(Abbrv, Chapters, Verses, Pos, Section) %>%
  mutate(Pos = (Pos - sectPos2[Section]) / sectLength[Section]) %>%
  mutate(Section = factor(Section, levels = OTsections)) %>%
  distinct()      # ignore how many times something appeared (read 2 or 3 times)

pdf("Barplot_days=Sundays_books=OT_format=bookshelf.pdf", height = 12, width = 8)
gg <- ggplot(dat, aes(x = Pos)) +
  geom_bar(aes(fill = Abbrv), show.legend = FALSE) +
  scale_x_continuous(labels = NULL, breaks = NULL, expand = c(0, 0)) +
  scale_y_continuous(labels = NULL, breaks = NULL, expand = c(0, 0)) +  
  ## scale_y_continuous(expand = c(0, 0)) +
  facet_grid(Section ~ ., scales = "free") +
  theme_bw() +
  xlab("") +
  ylab("") +
  ggtitle("How much of the Old Testament do you hear on Sundays?\n(Three Year Cycle)")
plot(gg)
dev.off()


## What percent is it?
dat %>%
  distinct () %>%
  group_by(Section) %>%
  summarize(FractionCovered = n() / sectLength[Section[1]])

## These percents are around 5-10%
## This is not that surprising considering there are 27,568 verses in the
## Old Testament and only 156 Sundays in 3 years
nrow(OTref2Pos %>% filter(Book != "Psalms")) * 0.05 / 156

## If we read the whole Old Testament in 3 years
nrow(OTref2Pos %>% filter(Book != "Psalms"))  / 156

vpc <- OTref2Pos %>%
  filter(Book != "Psalms") %>%
  group_by(Book, Chapter) %>%
  summarize(VersesPerChapter = n())
