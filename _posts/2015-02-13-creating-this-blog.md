---
title: Creating This Blog
layout: post
category : lessons
tagline: How to get up and running
tags : [blog, jekyll, github]
---
{% include JB/setup %}

## Creating a Blog
I was recently inspired by my friend to create a blog.  After thinking, "I'll get around to that when I have time" for many months I decided to just go for it.

It actually took me almost **2 whole days** to fight with different platforms and figure out how hosting on Github works but it turns out it is *really simple* if you know where to start.

For my friend, others, (and frankly myself so I don't forget) I want to explain "How It's Made"

## Github Account
First, you'll need to [join Github](https://github.com/join).  Note that, unless you want to pay for a domain name, your website URL will be `<username>.github.io` so choose your username wisely.

Next, create a repository called `<username>.github.io` where `<username>` is the user name you chose to create your account.

## Jekyll-Bootstrap
I decided to use Jekyll-Bootstrap because I wanted to get something up and working **quickly**.  The [website](http://jekyllbootstrap.com/) boasts "0 to Blog in 3 Minutes" and that is pretty accurate (at least if you have the dependencies installed).  See their nice [quick start guide](http://jekyllbootstrap.com/usage/jekyll-quick-start.html).

These instructions will tell you how to clone the existing repository into a local repository and then link that with the repository you already created on Github.  They also explain how to install jekyll locally, which is important so that you can preview your website content before pushing to Github (which will automagically publish it to your site).

You'll need to have `gem` installed first and you can install `rake` to follow the examples in the quick start guide.

{% highlight bash %}
$ gem install jekyll
{% endhighlight %}

## Helpful Guides
To supplement the Jekyll documentation, I followed the wise advice of others who created their blogs and subsequently blogged about it.

I especially found these posts helpful:

- Fellgernon Big Blog post entitled ["Creating your Jekyll-Bootstrap powered blog for R blogging](http://lcolladotor.github.io/2013/11/09/new-Fellgernon-Bit-setup-in-Github/#.VN4vSB1Qpzh)
- [Jason Fisher's Blog post](http://jfisher-usgs.github.io/lessons/2012/05/30/jekyll-build-on-windows/) for help with getting theme and syntax highlighting for code working

### Syntax Highlighting
I spent a while trying to get syntax highlighting to work (I'm not familar with web-dev, css files, etc.).  In the end, I did this:

1. Added [Jason Fisher's syntax.css](https://github.com/jfisher-usgs/jfisher-usgs.github.com/blob/master/assets/themes/twitter-2.0/css/syntax.css) to `<username>.github.io/assets/themes/twitter-2.0/css/`
2. Added the last line of the following code to `<username>.github.io/_includes/themes/twitter-2.0/default.html`

{% highlight html %}
<link href="{{ ASSET_PATH }}/css/bootstrap-responsive.min.css" rel="stylesheet">
<link href="{{ ASSET_PATH }}/css/style.css?body=1" rel="stylesheet" type="text/css" media="all">
<link href="/assets/themes/twitter-2.0/css/syntax.css" rel="stylesheet" type="text/css">
{% endhighlight %}

## Publishing a post
Once you're all set up, publishing a post could not be simpler!  

Just create a markdown file, add it, commit, and push to Github.


{% highlight bash %}
git add <files>
git commit -a
git push
{% endhighlight %}

My `.gitignore` file looks like this which is pretty standard because you don't actually want to push the `_site` folder (Github handles creating this for you from the contents in the other directories)

{% highlight bash %}
# Temporary Files
*~
*\#

_site/*
_theme_packages/*

Thumbs.db
.DS_Store

!.gitkeep

.rbenv-version
.rvmrc
{% endhighlight %}



### Working with R Markdown
In order to convert R Markdown files to plain markdown (but get syntax highlighting) I used the [convertRMarkdown function](https://github.com/jbryer/jbryer.github.com/blob/master/rmarkdown.r) written by Jason Bryer and described in this [post under Approach Two](http://jason.bryer.org/posts/2012-12-10/Markdown_Jekyll_R_for_Blogging.html).

But to get syntax highlighting, I modified the function to call `render_jekyll` instead of `render_markdown`.

{% highlight r %}
render_jekyll(highlight="pygments")
{% endhighlight %}

This will automatically add the liquid tags necessary to tell Jekyll how to highlight code.

