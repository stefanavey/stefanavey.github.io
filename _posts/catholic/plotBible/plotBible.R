#################################################################################
## Visualize How Much of The Bile Catholics Hear at Mass                       ##
#################################################################################

#############################
## Load necessary packages ##
#############################
library(openxlsx)                       # for reading excel tables
library(tidyr)
library(dplyr)
OTRefFile <- "plotBible/OldTestamentReference.xlsx"
abbrevFile <- "plotBible/BibleAbbreviations.xlsx"
LectSundaysFile <- "plotBible/Lectionary-SundaysAndFeasts.xlsx"
LectWeekdaysFile <- "plotBible/Lectionary-Weekdays.xlsx"

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

## Try to plot coverage of this
dat$Reading

## TODO: Write function to take readings and referece and return ranges that are covered

