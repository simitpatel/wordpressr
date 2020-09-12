# wordpressr

<!-- badges: start -->
<!-- badges: end -->

Journalism is in a state of crisis. Profitable outlets are driven by clickbait; unprofitable activities like
investigative journalism or local news reporting simply disappear. How can we fix this?

Perhaps automation can help:

1. Help writers be more productive
1. Make marketing and distribution easier
1. Make data-driven journalism easier for all 

**wordpressr** is an attempt to bring more automation, and the ensuing benefits, to journalism and publishing in general. For a more thorough look at how to get started, please see [the wordpressr vignette](https://storage.googleapis.com/wordpressr/articles/wordpressr.html).  

Are you interested in helping us? R developers can join us on [GitHub](https://github.com/simitpatel/wordpressr/issues/). For organizations interested in learning more about our content marketing services, visit our main website at [sixjupiter.com](https://sixjupiter.com).

## Installation

You can install the released version of wordpressr from [GitHub](https://github.com/simitpatel/wordpressr) with:

``` r
devtools::install_github("simitpatel/wordpressr")
```

## Example: Retrieving Posts

Need to perform text analysis on your posts? Or summarise data by author, category, tag, or time? The
**get_wp_posts()** function is a good place to start. 

``` r
library(wordpressr)
recent_posts = get_wp_posts('https://altmediauncensored.com',300)
```

