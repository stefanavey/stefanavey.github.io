---
title: Test R Markdown
published: true
status: publish
layout: post
category: test
---    
{% include JB/setup %}
 

 
 
# Test functionality
 
## Test syntax and output using R
 
Simply run some R basic R code and finish
 

{% highlight r %}
## List the session information 
sessionInfo()
{% endhighlight %}



{% highlight text %}
## R version 3.1.2 (2014-10-31)
## Platform: x86_64-redhat-linux-gnu (64-bit)
## 
## locale:
##  [1] LC_CTYPE=en_US.utf8       LC_NUMERIC=C             
##  [3] LC_TIME=en_US.utf8        LC_COLLATE=en_US.utf8    
##  [5] LC_MONETARY=en_US.utf8    LC_MESSAGES=en_US.utf8   
##  [7] LC_PAPER=en_US.utf8       LC_NAME=C                
##  [9] LC_ADDRESS=C              LC_TELEPHONE=C           
## [11] LC_MEASUREMENT=en_US.utf8 LC_IDENTIFICATION=C      
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] knitr_1.9
## 
## loaded via a namespace (and not attached):
## [1] compiler_3.1.2 evaluate_0.5.5 formatR_1.0    stringr_0.6.2 
## [5] tools_3.1.2
{% endhighlight %}



{% highlight r %}
## Testing whether plots are included
plot(1:10, 1:10)
{% endhighlight %}

![plot of chunk setup](/images/../images/setup-1.png) 
 
## Test syntax and output using elisp

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
