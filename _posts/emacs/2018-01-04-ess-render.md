---
layout: post
title: "Something like RStudio's Knit Button in Emacs"
subtitle: ""
category: lessons
tags: [Emacs, ess, R, RStudio, rmarkdown, elisp]
date: 2018-01-04 11:00:00 -0500
layout: post
comments: true
show-share: true
show-subscribe: true
---

## Motivation from RStudio

[RStudio](https://www.rstudio.com/products/rstudio/) has a great set of expanding features including a terminal, Git integration, and a new Connections tab. Almost enough to make me want to switch from using [Emacs](https://www.gnu.org/software/emacs/) as my editor/IDE/operating system. One of the many nice features of RStudio is the Knit button for rendering R Markdown (.Rmd) documents to output formats like HTML or PDF (or even Microsoft Word).

<img src="/img/rmd-knit-button.png" alt="Screenshot of RStudio's knit button" style="width: 1000px;"/>

## My Previous Workflow

Recently I have been writing a lot of [R markdown documents](http://rmarkdown.rstudio.com/) to create reproducible reports at work. The [rmarkdown package](https://cran.r-project.org/package=rmarkdown) makes it very easy. You just need to run the `rmarkdown::render()` function to knit an .Rmd file to markdown and subsequently render the output file (PDF, Word, HTML, etc.) with a single command!

My R Markdown files would always have a chunk at the top which was not evaluated or run and just held the command to run in R to render the output.

{% highlight shell%}
---
title: "Report"
author: Stefan Avey
date: "`r Sys.Date()`"
output: html_document
---

```{r render, eval = FALSE, echo = FALSE}
library(rmarkdown)
rmarkdown::render("path/to/Report.Rmd", output_dir = "../reports")
```

# Body of Report

{% endhighlight %}


So anytime I wanted to render the document I would search for this block, use `C-c C-c` to run it in the current R process, and then wait for the command to finish in R before making my next edit (fixing an error, adding more chunks, etc.).

This strategy is sub-optimal for a few reasons

1. It takes multiple, repetitive keystrokes to reverse search, execute the code, and pop the mark back to the original place `C-r render RET C-c C-c C-u C-SPC`
2. The code gets run in whatever R process the script is currently associated with. This is a minor issue but I prefer the rendering to happen in a "fresh" R process to avoid bugs or issues from interactively defined variables.

## Finding a Better Way

I wasn't sure if support for a better R Markdown workflow was already included in ESS (and I'm trying to dabble in elisp) so I wrote my own elisp function to render the document.

{% highlight elisp %}
;; spa/rmd-render
;; Global history list allows Emacs to "remember" the last
;; render commands and propose as suggestions in the minibuffer.
(defvar rmd-render-history nil "History list for spa/rmd-render.")
(defun spa/rmd-render (arg)
  "Render the current Rmd file to PDF output.
   With a prefix arg, edit the R command in the minibuffer"
  (interactive "P")
  ;; Build the default R render command
  (setq rcmd (concat "rmarkdown::render('" buffer-file-name "',"
                 "output_dir = '../reports',"
                 "output_format = 'pdf_document')"))
  ;; Check for prefix argument
  (if arg
      (progn
    ;; Use last command as the default (if non-nil)
    (setq prev-history (car rmd-render-history))
    (if prev-history
        (setq rcmd prev-history)
      nil)
    ;; Allow the user to modify rcmd
    (setq rcmd
          (read-from-minibuffer "Run: " rcmd nil nil 'rmd-render-history))
    )
    ;; With no prefix arg, add default rcmd to history
    (setq rmd-render-history (add-to-history 'rmd-render-history rcmd)))
  ;; Build and evaluate the shell command
  (setq command (concat "echo \"" rcmd "\" | R --vanilla"))
  (compile command))
(define-key polymode-mode-map (kbd "C-c r")  'spa/rmd-render)

{% endhighlight %}

I shared this function in an [answer on Stack Overflow](https://stackoverflow.com/a/47761267/3636840). The accepted answer is good and uses `polymode-weave` but I prefer to use `rmarkdown::render()` which uses knitr and pandoc under the hood.

So - what does my function (`spa/rmd-render`) actually do?  It simply builds a string containing the R command (in the variable `rcmd`), pipes that to R using a shell command (the variable `command`) and runs it using the `compile` function.

Note that I have some specific parameter settings like `output_dir = '../reports'` because I always have a consistent directory structure and want my report in a particular directory but the elisp can be easily customized to suit your needs by calling it with a prefix argument (e.g., `C-u`). I use this for example when I want to write the same report to multiple output formats that I have specified in my header. I can just type `C-u C-c r` and change the `output_format` from 'pdf_document' to 'all'. The function also has a history - so typing `C-u C-c r` followed by `M-p` will cycle through history of previous render commands.

With this in your init file, you only need to type `C-c r` from inside your .Rmd file (or `C-u C-c r` to render to a different format, location, etc.). The command will open a new window with a buffer called *compilation* where any errors will appear.

## Summary

This could definitely be improved and some may still prefer the built in command `polymode-weave` but I find that this makes me much more efficient. In any case, it was fun to practice coding in Emacs lisp. The most useful thing I learned was that the `compile` function (`M-x compile`) can be used for more than just running `make`. It actually will work with any shell command!



<!--  LocalWords:  RStudio's ess RStudio rmarkdown elisp Rmd img src px RET SPC
 -->
<!--  LocalWords:  rmd knitr pandoc init
 -->
