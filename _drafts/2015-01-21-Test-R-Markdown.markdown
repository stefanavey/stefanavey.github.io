---
title: Test R Markdown
published: true
status: publish
layout: post
category: test
---

# Test functionality

## Test syntax and output using R

Simply run some R basic R code and finish


{% highlight r %}
## List the session information
sessionInfo()
{% endhighlight %}



{% highlight text %}
## R version 3.0.3 (2014-03-06)
## Platform: x86_64-apple-darwin10.8.0 (64-bit)
##
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
##
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base
##
## other attached packages:
## [1] knitr_1.9
##
## loaded via a namespace (and not attached):
## [1] compiler_3.0.3 evaluate_0.5.5 formatR_1.0    stringr_0.6.2
## [5] tools_3.0.3
{% endhighlight %}



{% highlight r %}
## Testing whether plots are included
plot(1:10, 1:10)
{% endhighlight %}

![plot of chunk setup](/images/../images/setup-1.png)

## Test syntax and output using elisp

{% highlight python %}
(defun spa-find-gene-info ()
  "Opens a browser to view gene annotation for the gene symbol near the point"
(interactive)
    (let (geneID)
    (setq geneID (thing-at-point 'word))
(browse-url (concatenate 'string "http://www.genecards.org/cgi-bin/carddisp.pl?gene=" geneID)))
  )
  (global-set-key "\C-cg" 'spa-find-gene-info)
{% endhighlight %}
