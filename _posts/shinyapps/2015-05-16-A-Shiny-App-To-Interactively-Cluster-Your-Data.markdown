---
title: A Shiny App To Interactively Cluster Your Data
published: true
status: publish
layout: post
category: r
tags : [R, shiny, bioinformatics]
---
{% include JB/setup %}

# UPDATE
**Since writing this post I've re-implemented the app as a dashboard.  View the [dashboard version](https://sparsedata.shinyapps.io/SparseData-Cluster),  [source code](https://github.com/sparsedata/cluster-analysis), and [new blog post](http://stefanavey.github.io/r/2015/07/29/A-Dashboard-Implementation-Of-Clustering-App).**

# Brief Summary

I built an interactive, online App using Shiny where you can upload your own data (for an example, [download mtcars](https://internal.shinyapps.io/gallery/066-upload-file/mtcars.csv)) and perform basic clustering analysis and view correlations in a heatmap.

[View the Shiny App Here](https://avey.shinyapps.io/cluster-analysis/) and [the Source Code here](https://github.com/stefanavey/cluster-analysis).

# Background

Recently, I was talking with a post-doc who was telling me about what he
would like to be able to do with an analysis.
He had some RNA-seq data that gave him some information about many genes on a
handful of samples corresponding to different cell types.  He wanted to know
what samples clustered together. His collaborators had done some analysis and
produced a heatmap of sample x sample correlation. Now he wanted to know what
would happen when one gene (or two or three) is removed from the analysis.
Would the clusters stay the same? Would they change?
I thought this was kind of interesting and started to look to see whether
there were any existing online tools where he could submit his data, and do the
clustering interactively (choosing which genes to include or exclude).

## Is it already on the web?

I did a quick Google search and found [this post](https://imdevsoftware.wordpress.com/2013/07/07/interactive-heatmaps-and-dendrograms-a-shiny-app/), which was similar to what I was looking for. Unfortunately the links didn't work and it turns out that App is now part of a larger suite of tools that is available for download as [DeviumWeb](https://github.com/dgrapov/DeviumWeb) but not available (that I know of) as a web-based tool. I wanted a tool that was available from any browser without needing to install R or other packages so I decided to practice some my Shiny skills and make my own.

# Interactive Analysis on Your Own Data

These were the specifications:

* Online tool
* Upload your own data
* Choose how clustering is performed
* Choose what features are included in clustering

## Shiny makes it easy

With Shiny this ended up being very easy. There is already a [file upload app](http://shiny.rstudio.com/gallery/upload-file.html) available as a Shiny example so I used that. I tried to use a [Data Table](http://shiny.rstudio.com/gallery/basic-datatable.html) instead of a basic table to view what was uploaded but didn't like that the row names were not visible.

Basically I just modified existing heatmap code to make some options interactive and stitched the upload and analysis into [one markdown file](https://github.com/stefanavey/cluster-analysis/blob/master/mainPage.Rmd).

This was the easiest way to start out but definitely not the most visually appealing, especially since [shinyapps.io](https://www.shinyapps.io) branding is now on each of the embedded Apps.

## Unknown data makes it difficult

The fact that anyone can upload data makes this sort of App attractive but also makes it much harder to code defensively. I am certain that many types of data will break this App and I haven't done a lot of error checking because it currently works for the 1 intended user.

The trickiest part of handling unknown data was creating output panels to select what features to include or exclude. It's tricky because the names of the features are not known before the data is uploaded so they have to go into a [reactive expression](http://shiny.rstudio.com/articles/reactivity-overview.html) that will be updated when the data is changed. The following code excerpt defines 2 user interface panels (usually in ui.R) but is defined in server.R ([see complete server.R file](https://github.com/stefanavey/cluster-analysis/blob/master/clustering/server.R)) and saved in the output list `output` to be passed to ui.R.


{% highlight r %}
## in server.R
## Create 2 output panels that depend on the data passed in
output$condPanel1 <- renderUI({
  conditionalPanel(
    condition = "input.rem == true",
    selectizeInput('toRm', "Exclude",
                   choices=sort(rownames(myData())),
                   multiple=TRUE)
  )
})

output$condPanel2 <- renderUI({
  conditionalPanel(
    condition = "input.incl == true",
    selectizeInput('toIncl', "Include Only",
                   choices=sort(rownames(myData())),
                   multiple=TRUE)
  )
})
{% endhighlight %}

Then in ui.R ([see complete ui.R file](https://github.com/stefanavey/cluster-analysis/blob/master/clustering/ui.R)), those panels actually used in the sidebar.


{% highlight r %}
## In ui.R
shinyUI(fluidPage(
  headerPanel(""),
  sidebarPanel(
    ## Other parameters ...
    uiOutput("condPanel1"),
    ## Other parameters ...
    uiOutput("condPanel2")
  ),
  mainPanel(
    ## Main Panel plot
  )
{% endhighlight %}

I also had a bit of difficulty making the data available across the two apps (file upload and clustering) but settled with a global data variable (`datGlobal`) and a manual check box that updates the local variable from the global variable. I would like to know how to do this automatically but haven't figure it out yet since these 2 apps are separate.

# Summary

The cool thing with this project is that this App allowed this post-doc to reproduce the analysis his collaborators performed and to see how it changed with different clustering methods or genes included!

Recently, more efforts have been made for reproducibility with data portals and online interactive figures associated with some publications (for example see [this portal](http://www.interactivefigures.com/dm3/vaccine-paper/vaccine-landing.gsp) for [this Immunity paper](http://dx.doi.org/10.1016/j.immuni.2012.12.008)). In my opinion, being able to take someone else's published data and "play with it" to reproduce and modify the original analyses in the paper promotes transparency and furthers research.

## Future Plans

My future plans are to modify the layout to use [Shiny dashboard](http://rstudio.github.io/shinydashboard/) instead of an Rmarkdown file. This will make it much cleaner with one page for upload and another for analysis.

As far as improving the functionality, my post-doc collaborator is interested in knowing what genes are important for the clustering so one idea is to have a ranked list of genes by some metric of how they contribute to the clustering.
An alternative approach is to use another method like principal component analysis (PCA) and then if there is separation between the samples, the loadings on each gene can be used for ranking.

## Note on building the Shiny App
I spent about an 1 hour debugging this error while trying to run the Shiny App.

    Error in file(con, "r") : cannot open the connection

Not exactly the most helpful error message... The problem turned out to not be in my code but in some sort of backup file that was now missing.

    The file does not exist: /repos/clusterAnalysis/fileUpload/.#server.R

I only discovered this more detailed error message by running shinyApps::bundleApp.

# Implementation

[Take a look at the Shiny App here](https://avey.shinyapps.io/cluster-analysis/) and [the Source Code](https://github.com/stefanavey/cluster-analysis) and leave a comment to let me know what you think! You can download a sample data set ([mtcars](https://internal.shinyapps.io/gallery/066-upload-file/mtcars.csv)) to try out if you don't have your own.
