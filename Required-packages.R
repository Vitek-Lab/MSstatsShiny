install.packages(c('shiny', 'shinyBS', 'shinybusy', 'shinyjs', 
                   'uuid', 'DT', 'knitr', 'plotly', 'ggrepel', 
                   'gplots', 'tidyverse', 'data.table', 'BiocManager'))

BiocManager::install(c('MSstatsTMT', 'MSstats', 'biomaRt'))

library(shiny)
runApp()
