---
title: Gene Summaries Inside Emacs
layout: post
category : lessons
tagline: Get gene info without leaving home row
tags : [emacs, bioinformatics, tutorial]
---
{% include JB/setup %}

## The Problem
As a bioinformatician who works mostly inside of Emacs, I **strongly dislike** having to open a web browser, type in a gene identifier and search for information on that gene just to view a simple summary of what a gene does.  Instead, I'd rather be able to see that right inside of my text editor, _fast_.  Since Emacs is so customizable, I decided to try to see if I could get find a solution by hacking out some `elisp`, `bash scripting` and `R`.


## My first solution

My first attempt used the [Gene Cards](http://www.genecards.org/) website to try to find information.  The function below could be used to get the `thing at point` and plug it into the URL to open the website with the appropriate gene information.

{% highlight elisp %}
(defun spa-find-gene-info ()
  "Opens a browser to view gene annotation for the gene symbol near the point"
  (interactive)
  (let (geneID)
  (setq geneID (thing-at-point 'word))
  (browse-url (concatenate 'string "http://www.genecards.org/cgi-bin/carddisp.pl?gene=" geneID)))
  )
  (global-set-key "\C-cg" 'spa-find-gene-info)
{% endhighlight %}

So if there was a gene that my point was on (say NAT2), I could hit `C-c g` and [this page](http://www.genecards.org/cgi-bin/carddisp.pl?gene=NAT2) would open in my default web browser.  This helped me save time but it still opened in a browser and took a few seconds to load.  This is not usually that big of a deal but I often work on a remote server where opening a GUI program like firefox over ssh with X11 forwarding is even slower (~ 10 seconds).

## A more elegant solution

I was browsing the gene information from [NIH's NCBI Website](http://www.ncbi.nlm.nih.gov/gene) recently and discovered that the `Display Settings` in the top left corner included a plain text output.  Now when I see "plain text", my first thought is Emacs so I started to seek out a way to get those files inside of emacs.

Now my first thought was to use the `browse-url-emacs` function to browse the plain text site inside of Emacs.  I discovered (and I'm not sure why) that this was _even slower_ than opening a browser.

Not quite discouraged enough to stop my last idea was to download all the files and store local copies to view inside of Emacs.

### Creating local database of HTML files
So I did that. All HTML files for genes in Homo Sapiens from the website I saved in `~/.emacs.d/spa-find-gene-info/database/`.

{% highlight R %}
## Load in data to know what entrezIDs to use
load("egid2symbolMap.RData")
destDir <- "~/.emacs.d/spa-find-gene-info/database/"

## Try to download all files for which I have entrezIDs
for(i in sort(unique(egid2symbolMap$ENTREZID))) {
  destFileName <- paste0(destDir, i, ".html")
  if(!file.exists(destFileName)) {      # don't overwrite in case I want to add more
    tryCatch(
      { download.file(paste0("http://www.ncbi.nlm.nih.gov/gene/",
                             i, "?report=full_report&format=text"),
                      destFileName, method="internal", quiet=TRUE) },
      error = function(e) { cat("Entrez Gene ", i, " was not downloaded.\n") })
  }
}
{% endhighlight %}

This is obviously a very naive way to do it (and took about 5 hours) but I figured that I wouldn't need to update the database too often.  Now I basically had a local version of all the gene information from their website and I could simply open the files on my machine if they existed and (otherwise) still use the browser solution.

Here is an example of the first few lines of the HTML file for the gene NAT2 (with entrezID 10).  You can see the online version [here](http://www.ncbi.nlm.nih.gov/gene/10)

{% highlight html %}

<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<pre><pre>NAT2 N-acetyltransferase 2 (arylamine N-acetyltransferase)[Homo sapiens]
Gene ID: 10, updated on 8-Feb-2015

SUMMARY
-------------------------------------------------------------------------------------------------

Official Symbol: NAT2 (provided by HGNC)
Official full name: N-acetyltransferase 2 (arylamine N-acetyltransferase) (provided by HGNC)
Primary source: HGNC:HGNC:7646
See related: Ensembl:ENSG00000156006; HPRD:02000; MIM:612182; Vega:OTTHUMG00000130826
Gene type: protein coding
RefSeq status: REVIEWED
Organism: Homo sapiens

Lineage: Eukaryota; Metazoa; Chordata; Craniata; Vertebrata; Euteleostomi; Mammalia;
Eutheria; Euarchontoglires; Primates; Haplorrhini; Catarrhini; Hominidae; Homo

Also known as: AAC2; PNAT; NAT-2

Summary: This gene encodes an enzyme that functions to both activate and deactivate
arylamine and hydrazine drugs and carcinogens. Polymorphisms in this gene are
responsible for the N-acetylation polymorphism in which human populations
segregate into rapid, intermediate, and slow acetylator phenotypes. Polymorphisms
in this gene are also associated with higher incidences of cancer and drug
toxicity. A second arylamine N-acetyltransferase gene (NAT1) is located near this
gene (NAT2). [provided by RefSeq, Jul 2008]

Annotation information: Note: Loci in other organisms that are functionally homologous to this one are
validly referred to as both NAT1 and NAT2; i.e., the functional homologs of NAT1
include mouse and rat Nat2, while the functional homologs of human NAT2 include
mouse and rat Nat1. Name:sequence associations are consistent with current use in
the field. [13 Feb 2013]

Orthologsall (/gene/?Term=ortholog_gene_10[group]), mouse (http://www.ncbi.nlm.nih.gov/gene/17961)

GENOMIC CONTEXT
-------------------------------------------------------------------------------------------------

{% endhighlight %}

### Modifying my elisp function to work
Then I modified my Emacs lisp function to open this local HTML file (if it exists) and otherwise still open a browser.

{% highlight elisp %}

(defun spa-find-gene-info ()
"If an html file exists in the database for the geneID, open it inside emacs.
Otherwise, open a browser to view gene annotation for the gene symbol near the point"
  (interactive)
  (let (geneID)
  (setq geneID (thing-at-point 'word))
  (if (string-integer-p geneID)
  (browse-url-emacs (concatenate 'string "http://www.ncbi.nlm.nih.gov/gene/" geneID "?report=full_report&format=text"))
  (browse-url (concatenate 'string "http://www.genecards.org/cgi-bin/carddisp.pl?gene=" geneID)))
  ))
(global-set-key "\C-cg" 'spa-find-gene-info)

{% endhighlight %}

So, in the end, I can hit 3 keys `C-c g` and practically instantaneously see not only the gene summary information I was after but also additional details provided by NCBI on the gene (entrez ID or gene symbol) at point _inside of Emacs_.  **Pretty Cool!**

### So...do I need to know the Entrez Gene ID?

Well this is great if you see entrez gene IDs but what about other gene identifiers like symbols? Since the gene symbol is write in the HTML file (see above for NAT2), I wrote a simple bash script to create symbolic links from files like `NAT2.html` to `10.html`.

{% highlight bash %}

#$ -S /bin/sh                                                                                          

## Get all .html files that start with only numbers and create symbolic links to the appropriate html file

files=`ls | grep -E '^[0-9]+\.html$'`

for f in $files
do
    geneSymbol=`grep "Official Symbol:" $f | cut -d' ' -f3`
    newFile=$geneSymbol".html"
    if [ ! -z "$geneSymbol" ]; then
        ln -s $f $newFile               # create symbolic link                                         
    fi
done

{% endhighlight %}

This could be extended to other identifiers as well (one-to-many mappings might be tricky so I only used official gene symbol).

## Other solutions

I called this an "elegant solution" but really what I should say is this is the "most elegant I've found so far."  I would love to hear feedback on alternative strategies from others!
