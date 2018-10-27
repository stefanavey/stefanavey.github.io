#################################################################################
## Visualize How Much of The Bile Catholics Hear at Mass                       ##
#################################################################################

#############################
## Load necessary packages ##
#############################
library(readxl)
## library(openxlsx)                       # for reading excel tables
library(tidyverse)
library(stringr)                        # string functions
library(aveytoolkit)
OTRefFile <- "OldTestamentReference.xlsx"
abbrevFile <- "BibleAbbreviations.xlsx"
LectSundaysFile <- "Lectionary-SundaysAndFeasts.xlsx"
LectWeekdaysFile <- "Lectionary-Weekdays.xlsx"
source("helperFunctions.R")
       
########################
## Read in references ##
########################
testaments <- c("OT", "NT")
abbrev <- map(testaments, ~read_excel(abbrevFile, sheet = .x))
names(abbrev) <- testaments

OTsections <- c("Torah", "Historical", "Wisdom", "MajorProphets", "MinorProphets")
OTref <- map_df(OTsections, function(OTsection) {
    read_excel(OTRefFile, sheet = OTsection, na = ".") %>%
        mutate(Section = OTsection)
}) %>%
    rename(Book = `Book Name`) %>%
    gather(matches("[0-9]+"), key = "Chapter", value = "Verses") %>%
    filter(!is.na(Verses)) %>%
    select(Section, Book, Chapter, Verses) %>%
    mutate(Book = factor(Book,            # Order books in factor levels
                         levels = abbrev$OT$Name),
           Abbrv = abbrev$OT$Abbreviation[match(Book, abbrev$OT$Name)],
           Chapter = as.numeric(Chapter))

## Munge so that reference contains 1 row for every verse in the order they
## appear in the Old Testament with a `Pos` column to denote the position of the
## verse.
OTref2Pos <- OTref %>%
    arrange(Book, Chapter) %>%
    mutate(Verse = map(Verses, function(x) 1:x)) %>%
    unnest() %>%
    mutate(Chapter_Verse = paste(Chapter, Verse, sep = ':')) %>%
    mutate(Pos = 1:n())

###################################################
## Read in the Lectionary for Sundays and Feasts ##
###################################################
readings <- c("OT", "Psalm", "NT", "Gospel")
LectSundays <- map(readings, ~read_excel(LectSundaysFile, sheet = .x))
names(LectSundays) <- readings

## For now, will ignore shorter form readings (which seem to be the 2nd option)
## by ignoring parts of the readings after "or"
OTLect <- bind_rows(LectSundays$OT, LectSundays$Psalm) %>%
    tbl_df() %>%
    separate(LectNum_Year, c("LectNum", "Year"), sep = '-') %>%
    mutate(YearA = grepl("A", Year),
           YearB = grepl("B", Year),
           YearC = grepl("C", Year)) %>%
    mutate(Reading_clean = str_split(Reading, pattern = " or ") %>%
               map_chr(1))
OTLect

## Can easily now filter by new indicator columns
OTLect %>%
  filter(YearA)                         # only readings from Year A


#################################################################################
## Parsing Functions                                                           ##
#################################################################################
## Lost previous function to do parsing. Write new functions to parse readings
## into a data frame of book, chapter, verse. These are NOT efficient but get
## the job done for most references.

##' ParseBasic
##' 
##' Parse a basic reference 
##' 
##' @param str a character string in the form
##'     <Chapter>:<VerseStart>[-<VerseEnd>] where the ending verse is optional
##' @return a data frame with 1 row and 2 columns containing the starting and
##'     ending Chapter:Verse
ParseBasic <- function(str) {
    ## Simple case, no commas or semicolons
    result <- data.frame(start = NA, end = NA)
    result[1, ] <- str_split(str, "-")[[1]]
    if (!str_detect(result[1, 2], ":")) {
        ch <- str_split(result[1, 1], ":")[[1]][1]
        result[1, 2] <- paste(ch, result[1, 2], sep = ":")
    }
    return(result)
}

##' ParseFull
##'
##' Parse a full reference
##' 
##' @param str a character string in the form <BookAbbrev>
##'     <Chapter>:<VerseStart>[-<VerseEnd>] [, <VerseStart>-<VerseEnd>] [;
##'     <Chapter>:<VerseStart>[-<VerseEnd>]]
##' @return a data frame with as many rows as there are continuous verses in the
##'     reference and 3 columns containing the starting and ending Chapter:Verse
##'     as well as the book abbreviation
ParseFull <- function(str) {
    abbrv <- str_extract(str, "^[1-9]?[ ]?[A-Za-z]+ ")[[1]] %>%
        trimws()
    str <- str_replace(str, "^[1-9]?[ ]?[A-Za-z]+ ", "") %>%
        str_replace_all("[a-z]", "") %>%               
        trimws()
    num <- str_count(str, "[;,]") + 1
    result <- data.frame(start = rep(NA, num), end = rep(NA, num))
    str_mult <- str_split(str, ";")[[1]] %>%
        trimws
    lcv <- 1
    for (rr in str_mult) {
        if (!str_detect(rr, "[,]")) {
            ## Basic parsing if there are no commas
            result[lcv,] <- ParseBasic(rr)
            lcv <- lcv + 1
        } else {
            ## Handle commas
            x <- str_split(rr, ",")[[1]] %>%
                trimws()
            ## Loop over x to assign chapter, if not present, and parse basic
            for (jj in seq_along(x)) {
                latest_ch <- map(str_split(x, ":"), function(str)
                    str_extract(str[length(str)-1], "[0-9]+$"))
                if (!str_detect(x[[jj]], ":")) {
                    x[[jj]] <- paste(latest_ch[[jj-1]], x[[jj]], sep = ":")
                }
                result[lcv, ] <- ParseBasic(x[[jj]])
                lcv <- lcv + 1
            }
        }
    }
    result[["Abbrv"]] <- abbrv
    return(result)
}


## Test out the parser on the full Old Testament Lectionary for Sundays 
tmp <- OTLect %>%
    mutate(Pos = map(Reading_clean, function(x) {
        ## cat(x, sep = '\n') # debugging
        ParseFull(x) %>%
        left_join(OTref2Pos, by = c(start = "Chapter_Verse", Abbrv = "Abbrv")) %>%
        left_join(OTref2Pos, by = c(end = "Chapter_Verse", Abbrv = "Abbrv")) %>%
        rowwise() %>%
        do(data.frame(Pos = ifelse(any(is.na(.)), NA, .$Pos.x:.$Pos.y))) %>%
            pull(Pos)}))

comb_dat <- tmp %>%
    unnest(Pos) %>%
    left_join(OTref2Pos, by = "Pos")

## These are all the parsing failures that could be cleaned up by fixing the
## readings or occur when readings don't map onto the reference (e.g. ending
## verse is not in the reference).
comb_dat %>%
    filter(is.na(Pos)) %>%
    pull(Reading_clean)


## ############################################################
## ## Test out parser on all readings to see which ones fail ##
## ############################################################
## tmp <- lapply(OTLect$Reading, readingParser, ref = OTref2)
## tmp <- lapply(seq_along(tmp), function(i) {
##                          cbind(OTLect[i,], tmp[[i]])
##               })
## OTLectRanges <- Reduce(rbind, na.omit(tmp))

## ## Now need to assign "Pos" to reference and then add
## ## a counter for plotting that counts how many times each
## ## row is in the reference and plot the 'reference'

## pos <- rep(NA, nrow(OTLectRanges))
## for(i in 1:nrow(OTLectRanges)) {
##   index <- which(as.character(OTref2Pos$Abbrv) == OTLectRanges$Abbrv[i] &
##                     OTref2Pos$Chapter == OTLectRanges$Chapter[i] &
##                     OTref2Pos$Verse == OTLectRanges$Verses[i])
##   if(length(index) == 0) {
##     print(i)
##     show(OTLectRanges[i,])
##     warning("no position set")
##   } else {
##       pos[i] <- index
##     }
## }
## ## Some errors here. Some are due to my bad parsing and some are ambiguous while some may not exist!

## OTLectRanges <- cbind(OTLectRanges, Pos = pos)


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
plot_dat <- comb_dat
fname <- "Barplot_days=Sundays_books=OT.pdf"
## pdf(fname, width = 6, height = 12)
for(ab in unique(plot_dat$Abbrv)) {
    yearCols <- c("YearA", "YearB", "YearC")
    ggList <- list()
    rng <- which(OTref2Pos$Abbrv == ab)
    cs <- cumsum(OTref2$Verses[OTref2$Abbrv == ab]) # cumulative sum
    chapStarts <- c(1, cs[-length(cs)] + 1)
    posStart <- min(OTref2Pos$Pos[OTref2Pos$Abbrv == ab])
    breaks <- chapStarts + posStart
    labels <- as.character(1:length(breaks))
    for(year in yearCols) {
        dat <- plot_dat %>%
            filter(Abbrv == ab) %>%
            filter(!!sym(year)) %>%
            mutate(Chapter = as.factor(Chapter)) %>%
            select(Abbrv, Chapter, Verses, Pos) %>%
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
            ggtitle(ab) +
            getBaseTheme()
    }
    Multiplot(plotlist = ggList, cols = 3)
}
## dev.off()


## Plot details for one book including times read
ab <- "Ps"
rng <- which(OTref2Pos$Abbrv == ab)
cs <- cumsum(OTref2$Verses[OTref2$Abbrv == ab]) # cumulative sum
chapStarts <- c(1, cs[-length(cs)] + 1)
posStart <- min(OTref2Pos$Pos[OTref2Pos$Abbrv == ab])
breaks <- chapStarts + posStart
labels <- as.character(1:length(breaks))
dat <- plot_dat %>%
    select(Abbrv, Chapter, Verse, Pos) %>%
    filter(Abbrv == ab) %>%
    mutate(Chapter = as.factor(Chapter))
gg <- ggplot(dat, aes(x = Pos)) +
    geom_histogram(binwidth = 1, aes(fill = Chapter), show.legend = FALSE) +
    coord_flip() +
    scale_x_continuous(limits = c(min(rng), max(rng)), breaks = breaks,
                       labels = labels) +
    xlab("Chapter") +
    ylab("Times Read") +
    ## coord_polar(theta = "x") +
    ggtitle(ab) +
    getBaseTheme()
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
dat <- plot_dat %>%
    filter(!is.na(Pos)) %>%
    select(Section, Abbrv, Chapter, Verse, Pos) %>%
    mutate(Pos = (Pos - sectPos2[Section]) / sectLength[Section]) %>%
    mutate(Section = factor(Section, levels = OTsections)) %>%
    mutate(Label = paste0(Abbrv, " ", Chapter, ":", Verse)) %>%
    distinct()      # ignore how many times something appeared (read 2 or 3 times)

## pdf("Barplot_days=Sundays_books=OT_format=bookshelf.pdf", height = 12, width = 8)
gg <- ggplot(dat, aes(x = Pos)) +
    geom_vline(aes(xintercept = Pos, color = Abbrv), show.legend = FALSE) +
    scale_x_continuous(labels = NULL, breaks = NULL, expand = c(0, 0)) +
    scale_y_continuous(labels = NULL, breaks = NULL, expand = c(0, 0)) +  
    ## scale_y_continuous(expand = c(0, 0)) +
    facet_grid(Section ~ ., scales = "fixed") +
    theme_bw() +
    xlab("") +
    ylab("") +
    ggtitle(paste("How much of the Old Testament do you hear on Sundays?",
                  "(Three Year Cycle)", sep = '\n')) +
    getBaseTheme() +
    theme(strip.text = element_text(size = 10, face = "bold"))
plot(gg)
## dev.off()


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
    summarize(VersesPerChapter = n()) %>%
    ungroup()
