---
title: A Dashboard Implementation Of Clustering App
published: true
status: publish
layout: post
category: r
tags : [R, shiny, bioinformatics]
---
{% include JB/setup %}

# Background

I previously built an interactive, online App using Shiny where you can upload your own data, perform basic clustering analysis, and view correlations in a heatmap.  I wrote about this in my [last post](http://stefanavey.github.io/r/2015/05/16/A-Shiny-App-To-Interactively-Cluster-Your-Data). While this worked, it was very ugly and needed a face lift.  With the help of a friend, I implemented a dashboard using [shinydashboard](https://rstudio.github.io/shinydashboard/).

# New implementation

The new implementation organizes the individual apps in a dashboard instead of a single Rmarkdown file.  Essentially the individual apps (upload, clustering, etc.) remained the same (except for some feature improvements).  The big difference is that the outer layer is now a normal shiny app with the standard *app.R*, *ui.R*, and *server.R* files.  The other major change is the addition of three new files controlling the dashboard appearance and contents (*body.R*, *sidebar.R*, and *header.R*). See the shinydashboard [Get Started](https://rstudio.github.io/shinydashboard/get_started.html) for a basic example and the [Structure page](https://rstudio.github.io/shinydashboard/structure.html) for more details.

# Try it out!

Take a look at the [dashboard app](https://sparsedata.shinyapps.io/SparseData-Cluster) and [source code](https://github.com/sparsedata/cluster-analysis) and leave a comment to let me know what you think! You can download a sample data set ([mtcars](https://internal.shinyapps.io/gallery/066-upload-file/mtcars.csv)) to try out if you don't have your own.

## Previous Implementation
<img src="/images/StefanAvey-ClusterAnalysis-Screenshot.png" alt="Screenshot of previous implementation" style="width: 1000px;"/>

## Dashboard Implementation
<img src="/images/SparseData-ClusterAnalysis-Screenshot.png" alt="Screenshot of dashboard implementation" style="width: 1000px;"/>



