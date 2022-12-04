require(knitr)
require(markdown)

knit('Prototype.Rmd', 'Prototype.md')
markdownToHTML('Prototype.md', 'Prototype.html')