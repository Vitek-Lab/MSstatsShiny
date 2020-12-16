

### functions ###


# annotation

ensembl <- useEnsembl(biomart = "ensembl")
dbs <- listDatasets(ensembl)


output$Species <- renderUI({
  selectizeInput("species", "", dbs[2], options = list(placeholder = "select"))
})

dataset_input <- reactive({
  req(input$species)
  as.character(dbs$dataset[dbs$description == input$species])
})

ensembl1 <- reactive({
  req(dataset_input())
  useDataset(dataset_input(), mart = ensembl)
  })

filters <- reactive({
  req(ensembl1())
  listFilters(ensembl1())
  })

output$Filter <- renderUI ({
  req(filters())
  selectizeInput("filter", "", filters()[2], options = list(placeholder = "select"))
})

filter_input <- reactive({
  as.character(filters()$name[filters()$description == input$filter])
})

attributes <- reactive({listAttributes(ensembl1())
})

output$Attributes <- renderUI ({
  selectizeInput("attribute_input", "", attributes(), multiple=TRUE, options = list(placeholder="select attributes"))
})

attribute_input <- reactive({
  as.vector(input$attribute_input)
})


id <- reactive(SignificantProteins()[1])
    
  

results <- reactive({
  req(attribute_input())
  getBM(attributes = attribute_input(),
                 filters = filter_input(), 
                 values = id(),
                 mart = ensembl1())
})

# table output

output$annotation <- renderTable({
results()
})

# download

output$table_annot <-downloadHandler(
  filename = function() {
    paste("annotation-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(results(), file)
  }
)

# protein interaction

dbs1 <- get_STRING_species(version="10", species_name=NULL)

output$Species1 <- renderUI({
  selectizeInput("species1", "", dbs1[3], selected = NULL, options = list(placeholder = "select"))
})

entry <- eventReactive(input$species1, {
  number <- as.integer(dbs1$species_id[dbs1$compact_name == input$species1])
  return(number)
})


string_db <- reactive({
  req(entry())
  STRINGdb$new( version="10", species=entry(), score_threshold=0, input_directory="" )
})


tomap <- reactive({
  dataframe <- data.frame(SignificantProteins()[7], SignificantProteins()[3], SignificantProteins()[1])
  colnames(dataframe) <- c("p-value", "LogFC", "protein")
  return(dataframe)
})

example1_mapped <- reactive({
  string_db()$map(tomap(), "protein", removeUnmappedRows = TRUE )
})

output$Hits <- renderUI({
  req(SignificantProteins)
  max <- reactive({
    if (nrow(SignificantProteins()) <= 400) {
      Max <- nrow(SignificantProteins())}
    else {
      Max <- 400
    }
    return(Max)
      })
  numericInput("hits", 
               label = h5("Number of hits",
                          tipify(icon("question-circle"),
                                 title = "Number of hits to plot",
                                 placement = "bottom")),
               value = max(), min = 0, max = max())
})


hits <- function(){
  example1_mapped()$STRING_id[1:input$hits]}

observeEvent(input$interact, {
  output$network <- renderPlot(
    string_db()$plot_network(hits()), width = 600, height = 600)
  # output$link <- renderText(
  #   string_db()$get_link(hits())
  # )
  output$string_link <- renderUI({
    actionButton("redirect", "Open plot with STRING")
  })
})


URL <- function(){
  string_db()$get_link(hits())
  # address <- ""
  # for (p in hits()[!is.na(hits())]) {
  #   address <- paste(address, p, "%0D%0A", sep = "", collapse = "")
  # }
  # address <- paste("http://version10.5.string-db.org/newstring_cgi/show_network_section.pl?limit=0&targetmode=proteins&caller_identity=gene_cards&network_flavor=evidence&identifiers=", address, "&species=", entry(), collapse = "", sep = "")
  # return(address)
}


observeEvent(input$redirect, {
  js <- paste("window.open('", URL(), "')", sep="")
  shinyjs::runjs(js);
})
