---
title: Creating this Blog
tagline: How you can learn from me and do it much quicker
published: true
status: publish
layout: post
---    
{% include JB/setup %}
 

 
 
# How to begin
Soon I will start this post about how I created this blog, for now, just test that things are working as they should.
 
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
## [1] aveytoolkit_0.1 knitr_1.9      
## 
## loaded via a namespace (and not attached):
##  [1] bitops_1.0-6       caTools_1.17       colorspace_1.2-4  
##  [4] compiler_3.1.2     digest_0.6.4       evaluate_0.5.5    
##  [7] formatR_1.0        gdata_2.13.3       ggplot2_1.0.0     
## [10] gplots_2.13.0      grid_3.1.2         gtable_0.1.2      
## [13] gtools_3.4.1       KernSmooth_2.23-13 limma_3.16.8      
## [16] MASS_7.3-33        munsell_0.4.2      plyr_1.8.1        
## [19] proto_0.3-10       Rcpp_0.11.1        reshape2_1.4      
## [22] scales_0.2.4       stringr_0.6.2      tcltk_3.1.2       
## [25] tools_3.1.2
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
