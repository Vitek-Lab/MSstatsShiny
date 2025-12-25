#' Decide which data loading path to use
#' 
#' @param filetype The selected file type from the UI
#' @param is_big_file Boolean indicating if the 'big file' checkbox is checked
#' @return string "big_spectronaut" or "standard"
#' @noRd
.get_data_source_type <- function(filetype, is_big_file) {
  if (filetype == "spec" && isTRUE(is_big_file)) {
    "big_spectronaut"
  } else {
    "standard"
  }
}

#' Loadpage Server module for data selection and upload server.
#'
#' This function sets up the loadpage server where it consists of several,
#' options for users to select and upload files.
#'
#' @param id namespace prefix for the module
#' @param parent_session session of the main calling module
#' @param is_web_server boolean indicating if the app is running on a web server
#'
#' @return input object with user selected options
#'
#' @export
#' @examples
#' NA
#' 
loadpageServer <- function(id, parent_session, is_web_server = FALSE) {
  moduleServer(id, function(input, output, session) {
    
    # == shinyFiles LOGIC FOR LOCAL FILE BROWSER =================================
    # Define volumes for the file selection.
    if (!is_web_server) {
      volumes <- shinyFiles::getVolumes()()
      
      # Server-side logic for the shinyFiles button
      shinyFiles::shinyFileChoose(input, "specdata_big_browse", roots = volumes, session = session)
      
      # Reactive to parse and store the full file information (path, name, etc.)
      # This is efficient because parseFilePaths is only called once.
      local_spec_file_info <- reactive({
        req(is.list(input$specdata_big_browse))
        shinyFiles::parseFilePaths(volumes, input$specdata_big_browse)
      })
      
      # Reactive to get just the full datapath, for use in backend processing.
      local_path_spec <- reactive({
        path_info <- local_spec_file_info()
        if (nrow(path_info) > 0) path_info$datapath else NULL
      })
      
      # Render just the filename for user feedback in the UI.
      output$specdata_big_path <- renderPrint({
        req(nrow(local_spec_file_info()) > 0)
        cat(local_spec_file_info()$name)
      })
    } else {
      local_path_spec <- reactive({ NULL })
    }
    
    output$spectronaut_upload_ui <- renderUI({
      req(input$filetype == 'spec', input$BIO != 'PTM')
      
      if (!is_web_server) {
        tagList(
          h4("4. Upload MSstats scheme output from Spectronaut"),
          
          # Checkbox to switch between upload methods
          checkboxInput(session$ns("big_file_spec"), "Use local file browser (for large files)"),
          
          # Standard fileInput, shown when checkbox is NOT checked
          conditionalPanel(
            condition = paste0("!input['", session$ns("big_file_spec"), "']"),
            fileInput(session$ns('specdata'), "", multiple = FALSE, accept = NULL)
          ),
          
          # Local browser button, shown when checkbox IS checked
          conditionalPanel(
            condition = paste0("input['", session$ns("big_file_spec"), "']"),
            shinyFiles::shinyFilesButton(session$ns("specdata_big_browse"), "Browse for local file...", "Please select a file", multiple = FALSE),
            verbatimTextOutput(session$ns("specdata_big_path"))
          ),
          
          radioButtons(session$ns("sep_specdata"),
                       label = h5("Column separator in uploaded file", class = "icon-wrapper", 
                                  icon("question-circle", lib = "font-awesome"),
                                  div("Choose how columns are separated in the uploaded file", class = "icon-tooltip")),
                       c(Comma=",", Semicolon=";", Tab="\t", Pipe="|"),
                       inline = TRUE
          )
        )
      } else {
        tagList(
          h4("4. Upload MSstats scheme output from Spectronaut"),
          
          fileInput(session$ns('specdata'), "", multiple = FALSE, accept = NULL),
          
          radioButtons(session$ns("sep_specdata"),
                       label = h5("Column separator in uploaded file", class = "icon-wrapper", 
                                  icon("question-circle", lib = "font-awesome"),
                                  div("Choose how columns are separated in the uploaded file", class = "icon-tooltip")),
                       c(Comma=",", Semicolon=";", Tab="\t", Pipe="|"),
                       inline = TRUE
          )
        )
      }
    })
    
    # toggle ui (DDA DIA SRM)
    observe({
      print("bio")
      
      print(input$BIO)
      if((input$BIO == "Protein" || input$BIO == "Peptide") && input$DDA_DIA == "LType"){
        runjs("$('[type=radio][name=loadpage-filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
        enable("filetype")
        disable(selector = "[type=radio][value=spmin]")
        runjs("$.each($('[type=radio][name=loadpage-filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
        
      } else if ((input$BIO == "Protein" || input$BIO == "Peptide") && input$DDA_DIA == "TMT"){
        runjs("$('[type=radio][name=loadpage-filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
        enable("filetype")
        disable(selector = "[type=radio][value=sky]")
        disable(selector = "[type=radio][value=prog]")
        disable(selector = "[type=radio][value=spec]")
        disable(selector = "[type=radio][value=open]")
        disable(selector = "[type=radio][value=ump]")
        disable(selector = "[type=radio][value=diann]")
        runjs("$.each($('[type=radio][name=loadpage-filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
        
      } else if (input$BIO == "PTM" && input$DDA_DIA == "LType"){
        runjs("$('[type=radio][name=loadpage-filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
        enable("filetype")
        # disable(selector = "[type=radio][value=sky]")
        disable(selector = "[type=radio][value=prog]")
        disable(selector = "[type=radio][value=PD]")
        disable(selector = "[type=radio][value=openms]")
        disable(selector = "[type=radio][value=spmin]")
        disable(selector = "[type=radio][value=open]")
        disable(selector = "[type=radio][value=ump]")
        disable(selector = "[type=radio][value=phil]")
        disable(selector = "[type=radio][value=diann]")

        runjs("$.each($('[type=radio][name=loadpage-filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })") 
      }else if (input$BIO == "PTM" && input$DDA_DIA == "TMT"){
        runjs("$('[type=radio][name=loadpage-filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
        enable("filetype")
        disable(selector = "[type=radio][value=prog]")
        disable(selector = "[type=radio][value=openms]")
        disable(selector = "[type=radio][value=spec]")
        disable(selector = "[type=radio][value=open]")
        disable(selector = "[type=radio][value=ump]")
        disable(selector = "[type=radio][value=spmin]")
        disable(selector = "[type=radio][value=diann]")
        disable(selector = "[type=radio][value=sky]")
        
        runjs("$.each($('[type=radio][name=loadpage-filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })") 
      }
     
    })

    # observeEvent(input$filetype,{
    #   enable("proceed1")
    # })

    # can remove separator is.null check because shiny by default assigns the first value as the default value for radiobutton
    observe({
      disable("proceed1")
      if(((input$BIO == "Protein") || (input$BIO == "Peptide"))) {
        if(input$DDA_DIA == "LType") {
          if ((!is.null(input$filetype) && length(input$filetype) > 0)) {
            if (input$filetype == "sample") {
              if(!is.null(input$LabelFreeType)) {
                enable("proceed1")
              }
            } else if (input$filetype == "msstats") {
              if(!is.null(input$msstatsdata) && !is.null(input$sep_msstatsdata)) {
                enable("proceed1")
              }
            } else if (input$filetype == "sky") {
              print(input$sep_skylinedata)
              if(!is.null(input$skylinedata) && !is.null(input$sep_skylinedata)) { # && !is.null(input$annot)
                enable("proceed1")
              }
            } else if (input$filetype == "maxq") {
              if(!is.null(input$evidence) && !is.null(input$pGroup)) { # && !is.null(input$annot1)
                enable("proceed1")
              }
            } else if (input$filetype == "prog" || input$filetype == "PD" || input$filetype == "open" || input$filetype == "phil") {
              if(!is.null(input$data) && !is.null(input$sep_data)) { # && !is.null(input$annot)
                enable("proceed1")
              }
            } else if (input$filetype == "openms") {
              if(!is.null(input$data) && !is.null(input$sep_data)) {
                enable("proceed1")
              }
            } else if (input$filetype == "spec") {
              spec_regular_file_ok <- !isTRUE(input$big_file_spec) && !is.null(input$specdata)
              spec_big_file_ok <- isTRUE(input$big_file_spec) && length(local_path_spec()) > 0
              if((spec_regular_file_ok || spec_big_file_ok) && !is.null(input$sep_specdata)) {
                enable("proceed1")
              }
            } else if (input$filetype == "ump") {
              if(!is.null(input$fragSummary) && !is.null(input$peptideSummary) && !is.null(input$protSummary)) {  #&& !is.null(input$annot2)
                enable("proceed1")
              }
            } else if (input$filetype == "diann") {
              if(!is.null(input$dianndata) && !is.null(input$sep_dianndata)) { # && !is.null(input$annot)
                enable("proceed1")
              }
            }
          }
        } else if (input$DDA_DIA == "TMT") {
          if ((!is.null(input$filetype) && length(input$filetype) > 0)) {
            if(input$filetype == "sample" || input$filetype == "msstats") {
              enable("proceed1")
            }
            if (input$filetype == "maxq") {
              if(!is.null(input$evidence) && !is.null(input$pGroup)) { # && !is.null(input$annot1)
                enable("proceed1")
              }
            } else if (input$filetype == "PD") {
              if(!is.null(input$data) && !is.null(input$sep_data)) { # && !is.null(input$annot)
                enable("proceed1")
              }
            } else if (input$filetype == "openms") {
              if(!is.null(input$data) && !is.null(input$sep_data)) {
                enable("proceed1")
              }
            } else if (input$filetype == "spmin" || input$filetype == "phil") {
              if(!is.null(input$data) && !is.null(input$sep_data)) { # && !is.null(input$annot)
                enable("proceed1")
              }
            }
          }
        }
        
      }
      else if ((input$BIO == "PTM")) {
        if (input$DDA_DIA == "LType" || input$DDA_DIA == "TMT") {
          if ((!is.null(input$filetype) && length(input$filetype) > 0)) {
            if (input$filetype == "sample") {
              enable("proceed1")
            } else if (input$filetype == "msstats") {
              if(!is.null(input$msstatsptmdata) && !is.null(input$sep_msstatsptmdata)) {
                enable("proceed1")
              }
            } else if (input$filetype == "sky" || input$filetype == "maxq" || input$filetype == "spec" || input$filetype == "PD") {
              if(!is.null(input$ptm_input) && !is.null(input$fasta)) { # && !is.null(input$ptm_annot)
                enable("proceed1")
              }
            }
            else if (input$filetype == "phil") {
              if(!is.null(input$ptmdata)) { # && !is.null(input$annotation)
                enable("proceed1")
              }
            }
          }
        }
      }
    })

    get_annot = eventReactive(input$proceed1, {
      getAnnot(input)
    })


    get_annot1 = reactive({
      getAnnot1(input)
    })

    get_annot2 = reactive({
      getAnnot2(input)
    })

    get_annot3 = reactive({
      getAnnot3(input)
    })

    get_evidence = reactive({
      getEvidence(input)
    })

    get_evidence2 = reactive({
      getEvidence2(input)
    })

    get_global = reactive({
      getGlobal(input)
    })

    get_proteinGroups = reactive({
      getProteinGroups(input)
    })

    get_proteinGroups2 = reactive({
      getProteinGroups2(input)
    })

    get_FragSummary = reactive({
      getFragSummary(input)
    })

    get_peptideSummary = reactive({
      getPeptideSummary(input)
    })

    get_protSummary = reactive({
      getProtSummary(input)
    })

    get_maxq_ptm_sites = reactive({
      getMaxqPtmSites(input)
    })


    get_data = eventReactive(input$proceed1, {
      data_source <- .get_data_source_type(input$filetype, input$big_file_spec)
      
      if (data_source == "big_spectronaut") {
        # Ensure a file has been selected via the local browser
        req(length(local_path_spec()) > 0)

        # Show a busy indicator as this might take time
        shinybusy::show_modal_spinner(
          spin = "fading-circle",
          text = "Processing large Spectronaut file..."
        )

        # Define the output path using a temporary file.
        #currently can't use the timeporary file because bigSpectronauttoMSstatsFormat modifies the filepath internally 
        #temp_output_path <- tempfile(fileext = ".csv")
        

        # Call the big file conversion function from MSstatsConvert
        converted_data <- MSstatsBig::bigSpectronauttoMSstatsFormat(
          input_file = local_path_spec(),
          output_file_name = "output_file.csv",
          backend = "arrow",
          filter_by_excluded = input$filter_by_excluded,
          filter_by_identified = input$filter_by_identified,
          filter_by_qvalue = input$filter_by_qvalue,
          qvalue_cutoff = input$qvalue_cutoff,
          max_feature_count = input$max_feature_count,
          filter_unique_peptides = input$filter_unique_peptides,
          aggregate_psms = input$aggregate_psms,
          filter_few_obs = input$filter_few_obs
        )
        
        # Attempt to load the data into memory. 
        # If the data is too large for RAM, this will catch the allocation error.
        converted_data <- tryCatch({
          dplyr::collect(converted_data)
        }, error = function(e) {
          shinybusy::remove_modal_spinner()
          showNotification(
            paste("Memory Error: The dataset is too large to process in-memory.", e$message),
            type = "error",
            duration = NULL
          )
          return(NULL)
        })
        
        if (is.null(converted_data)) return(NULL)
        
        # Remove the busy indicator
        shinybusy::remove_modal_spinner()

        return(converted_data)
      } else {
        # For all other cases (non-spec files, or standard spec uploads),
        # use the existing getData function.
        getData(input)
      }
    })


    get_data_code = eventReactive(input$calculate, {
      getDataCode(input)
    })

    get_summary1 = eventReactive(input$proceed1, {
      getSummary1(input,get_data(),get_annot())
    })

    get_summary2 = eventReactive(input$proceed1, {
      getSummary2(input,get_data())
    })

    onclick("proceed1", {
      get_data()
      get_annot()
      shinyjs::show("summary_tables")

      ### outputs ###
      get_summary = reactive({
        if(is.null(get_data())) {
          return(NULL)
        }
        data1 = get_data()
        data_summary = describe(data1)
      })

      output$template = downloadHandler(
        filename = "extdata/templateannotation.csv",

        content = function(file) {
          file.copy("extdata/templateannotation.csv", file)
        },
        contentType = "csv"
      )

      output$template1 = downloadHandler(
        filename = function() {
          paste("extdata/templateevidence", "txt", sep = ".")
        },

        content = function(file) {
          file.copy("extdata/templateevidence.txt", file)
        },
        contentType = "txt"
      )

      output$summary = renderTable(
        {
          head(get_data())
        }, bordered = TRUE
      )
      output$summary_ptm = renderTable(
        {
          head(get_data()$PTM)
        }, bordered = TRUE
      )
      output$summary_prot = renderTable(
        {
          head(get_data()$PROTEIN)
        }, bordered = TRUE
      )


      output$summary1 =  renderTable(
        {
          req(get_data())
          get_summary1()

        }, colnames = FALSE, bordered = TRUE
      )

      output$summary2 =  renderTable(
        {
          req(get_data())
          get_summary2()

        }, colnames = FALSE, bordered = TRUE, align='lr'
      )

      onclick("proceed2", {
        updateTabsetPanel(session = parent_session, inputId = "tablist",
                          selected = "DataProcessing")
      })
      output$summary_tables = renderUI({
        ns <- session$ns
        tagList(
          tags$head(
            tags$style(HTML('#loadpage-proceed2{background-color:orange}'))
          ),
          actionButton(inputId = ns("proceed2"), label = "Next step"),
          h4("Summary of experimental design"),
          tableOutput(ns('summary1')),
          tags$br(),
          h4("Summary of dataset"),
          tableOutput(ns("summary2")),
          tags$br(),
          conditionalPanel(condition = "input['loadpage-BIO'] !== 'PTM'",
                           h4("Top 6 rows of the dataset"),
                           tableOutput(ns("summary"))
          ),
          conditionalPanel(condition = "input['loadpage-BIO'] == 'PTM'",
                           h4("Top 6 rows of the PTM dataset"),
                           tableOutput(ns("summary_ptm")),
                           tags$br(),
                           h4("Top 6 rows of the unmodified protein dataset"),
                           tableOutput(ns("summary_prot"))
          )
        )
      })

    })
    return(
      list(
        input = input,
        getData = get_data
      )
    )
  })

}
