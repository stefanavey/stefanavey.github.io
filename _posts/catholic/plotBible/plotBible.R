#################################################################################
## Visualize How Much of The Bile Catholics Hear at Mass                       ##
#################################################################################

#############################
## Load necessary packages ##
#############################
library(readxl)
library(scales)
library(tidyverse)
library(aveytoolkit)
OTRefFile <- "OldTestamentReference.xlsx"
NTRefFile <- "NewTestamentReference.xlsx"
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

## Old Testament
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
           Chapter = as.numeric(Chapter)) %>%
    arrange(Book) %>%
    mutate(Abbrv = factor(Abbrv, levels = unique(Abbrv)))

## Munge so that reference contains 1 row for every verse in the order they
## appear in the Old Testament with a `Pos` column to denote the position of the
## verse.
OTref2Pos <- OTref %>%
    arrange(Book, Chapter) %>%
    mutate(Verse = map(Verses, function(x) 1:x)) %>%
    unnest() %>%
    mutate(Chapter_Verse = paste(Chapter, Verse, sep = ':')) %>%
    mutate(Pos = 1:n()) %>%
    mutate(Testament = "Old")    

## New Testament
NTsections <- c("Gospels", "NT")
NTref <- map_df(NTsections, function(NTsection) {
    read_excel(NTRefFile, sheet = NTsection, na = ".") %>%
        mutate(Section = NTsection) %>%
        mutate_at(vars(matches("[0-9]+")),
                  funs(as.integer(str_replace(., fixed("*"), ""))))
}) %>%
    rename(Abbrv = `Book  Name`) %>%
    gather(matches("[0-9]+"), key = "Chapter", value = "Verses") %>%
    filter(!is.na(Verses)) %>%
    select(Section, Abbrv, Chapter, Verses) %>%
    mutate(Abbrv = factor(Abbrv,
                         levels = abbrev$NT$Abbreviation),
           Book = abbrev$NT$Name[match(Abbrv, abbrev$NT$Abbreviation)],
           Chapter = as.numeric(Chapter)) %>%
    arrange(Abbrv) %>%
    mutate(Book = factor(Book, levels = unique(Book)))

## Munge so that reference contains 1 row for every verse in the order they
## appear in the New Testament with a `Pos` column to denote the position of the
## verse.
NTref2Pos <- NTref %>%
    arrange(Book, Chapter) %>%
    mutate(Verse = map(Verses, function(x) 1:x)) %>%
    unnest() %>%
    mutate(Chapter_Verse = paste(Chapter, Verse, sep = ':')) %>%
    mutate(Pos = 1:n()) %>%
    mutate(Testament = "New")

ref2Pos <- bind_rows(OTref2Pos, NTref2Pos) %>%
    mutate(Pos = 1:n())
    ## mutate(Book = factor(Book, levels = unique(Book))) %>%
    ## mutate(Abbrv = factor(Abbrv, levels = unique(Abbrv)))
    

###################################################
## Read in the Lectionary for Sundays and Feasts ##
###################################################
readings <- c("OT", "Psalm", "NT", "Gospel")
LectSundays <- map(readings, ~read_excel(LectSundaysFile, sheet = .x))
names(LectSundays) <- readings

SunLect <- bind_rows(LectSundays) %>%
    tbl_df() %>%
    separate(LectNum_Year, c("LectNum", "Year"), sep = '-') %>%
    mutate(YearA = grepl("A", Year),
           YearB = grepl("B", Year),
           YearC = grepl("C", Year))
SunLect

## Test out the parser on the full Sunday Lectionary
tmp <- SunLect %>%
    mutate(Pos = map(Reading, function(x) {
        ## cat(x, sep = '\n') # debugging
        ParseFull(x) %>%
        left_join(ref2Pos, by = c(start = "Chapter_Verse", Abbrv = "Abbrv")) %>%
        left_join(ref2Pos, by = c(end = "Chapter_Verse", Abbrv = "Abbrv")) %>%
        rowwise() %>%
            do(data.frame(Pos = if (any(is.na(.))) {
                                    NA
                                } else {
                                    .$Pos.x:.$Pos.y
                                }
                          )) %>%
            pull(Pos)}))

comb_dat <- tmp %>%
    unnest(Pos) %>%
    left_join(ref2Pos, by = "Pos")

## These are all the parsing failures that could be cleaned up by fixing the
## readings or occur when readings don't map onto the reference (e.g. ending
## verse is not in the reference).
## Many have "+" in the reference and I'm not sure how this should be handled
## but this mainly affects the Psalms.
comb_dat %>%
    filter(is.na(Pos)) %>%
    pull(Reading)


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
plot_dat <- comb_dat %>%
    filter(Testament == "Old")

## Barplots for each Book in the Old Testament
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
ab <- "Gen"
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
book_dat <- OTref2Pos %>%
    mutate(Pos = (Pos - sectPos2[Section]) / sectLength[Section]) %>%
    mutate(Section = factor(Section, levels = OTsections)) %>%    
    group_by(Section, Abbrv) %>%
    summarize(Pos = min(Pos)) %>%
    ungroup() %>%
    group_by(Section) %>%
    mutate(y = seq(from = 0.1, to = 0.9, length.out = n())) %>%
    ungroup()

    
## pdf("Barplot_days=Sundays_books=OT_format=bookshelf.pdf", height = 12, width = 8)
gg <- ggplot(dat, x = 0, y = 1) +
    geom_vline(aes(xintercept = Pos, color = Abbrv), show.legend = FALSE) +
    geom_label(data = book_dat, aes(x = Pos, y = y, label = Abbrv),
               hjust = 0, size = 3) +
    scale_x_continuous(labels = percent) +    
    ## scale_x_continuous(labels = NULL, breaks = NULL, expand = c(0, 0)) +
    scale_y_continuous(labels = NULL, breaks = NULL) +
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
  summarize(PercentCovered = percent(n() / sectLength[Section[1]]))

## These percents are around 5-10%
## This is not that surprising considering there are 27,568 verses in the
## Old Testament and only 156 Sundays in 3 years

## This would come out to about 8 verses on average. This can be done more
## rigorously by directly looking at the lectionary to determine number of
## verses in reading.
nrow(OTref2Pos %>% filter(Book != "Psalms")) * 0.05 / 156

## If we read the whole Old Testament in 3 years, we would need to hear 160
## verses from the Old Testament every Sunday!
nrow(OTref2Pos %>% filter(Book != "Psalms"))  / 156


###################################
## Bookshelf including OT and NT ##
###################################

## Get all section lengths from complete Bible
sections <- ref2Pos %>%
    select(Section, Abbrv) %>%
    distinct() %>%
    group_by(Section) %>%
    mutate(Abbrv = list(Abbrv)) %>%
    distinct() %>%
    deframe()
sectPos <- map_int(names(sections), function(sect) {
    abbrvs <- sections[[sect]]
    lastAbbrv <- abbrvs[length(abbrvs)]
    maxPos <- ref2Pos %>%
        filter(Abbrv == lastAbbrv) %>%
        pull(Pos) %>%
        max()
    return(maxPos)
})
names(sectPos) <- names(sections)
sectPos2 <- c(0, sectPos[-length(sectPos)])
names(sectPos2) <- names(sectPos)
sectLength <- sectPos - sectPos2

dat <- comb_dat %>%
    filter(!is.na(Pos)) %>%
    select(Section, Abbrv, Chapter, Verse, Pos) %>%
    mutate(Pos = (Pos - sectPos2[Section]) / sectLength[Section]) %>%
    mutate(Section = factor(Section, levels = names(sections))) %>%
    mutate(Label = paste0(Abbrv, " ", Chapter, ":", Verse)) %>%
    distinct()      # ignore how many times something appeared (read 2 or 3 times)
book_dat <- ref2Pos %>%
    mutate(Abbrv = factor(Abbrv, levels = unique(Abbrv))) %>%
    mutate(Pos = (Pos - sectPos2[Section]) / sectLength[Section]) %>%
    mutate(Section = factor(Section, levels = names(sections))) %>%    
    group_by(Section, Abbrv) %>%
    summarize(Pos = min(Pos)) %>%
    ungroup() %>%
    group_by(Section) %>%
    mutate(y = rep(c(0, 0.33, 0.66, 1), length.out = n())) %>%
    ungroup()

    
## pdf("Barplot_days=Sundays_books=ALL_format=bookshelf.pdf", height = 12, width = 8)
png("Barplot_days=Sundays_books=ALL_format=bookshelf.png", height = 12, width = 8, units = "in", res = 300)
gg <- ggplot(dat, x = 0, y = 1) +
    geom_vline(aes(xintercept = Pos, color = Abbrv), show.legend = FALSE) +
    geom_label(data = book_dat, aes(x = Pos, y = y, label = Abbrv),
               hjust = 0, size = 3) +
    scale_x_continuous(labels = percent) +    
    ## scale_x_continuous(labels = NULL, breaks = NULL, expand = c(0, 0)) +
    scale_y_continuous(labels = NULL, breaks = NULL) +
    ## scale_y_continuous(expand = c(0, 0)) +
    facet_grid(Section ~ ., scales = "fixed") +
    theme_bw() +
    xlab("") +
    ylab("") +
    ggtitle(paste("How much of the Scriptures are Proclaimed on Sundays and Feasts?",
                  "(Three Year Cycle)", sep = '\n')) +
    getBaseTheme() +
    theme(strip.text = element_text(size = 10, face = "bold"))
plot(gg)
dev.off()


