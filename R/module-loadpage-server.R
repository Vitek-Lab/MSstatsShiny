#' Loadpage Server module for data selection and upload server.
#'
#' This function sets up the loadpage server where it consists of several, 
#' options for users to select and upload files.
#'
#' @param id namespace prefix for the module
#' @param parent_session session of the main calling module
#' 
#' @return input object with user selected options
#'
loadpageServer <- function(id, parent_session) {
  moduleServer(id, function(input, output, session) {
    # toggle ui (DDA DIA SRM)
    observe({
      if (input$DDA_DIA == "DDA") {
        runjs("$('[type=radio][name=loadpage-filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
        enable("filetype")
        disable(selector = "[type=radio][value=spec]")
        disable(selector = "[type=radio][value=open]")
        disable(selector = "[type=radio][value=ump]")
        disable(selector = "[type=radio][value=spmin]")
        disable(selector = "[type=radio][value=phil]")
        runjs("$.each($('[type=radio][name=loadpage-filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
      }
      else if (input$DDA_DIA == "DIA") {
        runjs("$('[type=radio][name=loadpage-filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
        enable("filetype")
        disable(selector = "[type=radio][value=maxq]")
        disable(selector = "[type=radio][value=prog]")
        disable(selector = "[type=radio][value=PD]")
        disable(selector = "[type=radio][value=openms]")
        disable(selector = "[type=radio][value=spmin]")
        disable(selector = "[type=radio][value=phil]")
        runjs("$.each($('[type=radio][name=loadpage-filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
      }
      else if (input$DDA_DIA == "SRM_PRM") {
        runjs("$('[type=radio][name=loadpage-filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
        enable("filetype")
        disable(selector = "[type=radio][value=maxq]")
        disable(selector = "[type=radio][value=prog]")
        disable(selector = "[type=radio][value=PD]")
        disable(selector = "[type=radio][value=openms]")
        disable(selector = "[type=radio][value=spec]")
        disable(selector = "[type=radio][value=open]")
        disable(selector = "[type=radio][value=ump]")
        disable(selector = "[type=radio][value=spmin]")
        disable(selector = "[type=radio][value=phil]")
        runjs("$.each($('[type=radio][name=loadpage-filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
      }
      
      else if (input$DDA_DIA == "TMT") {
        runjs("$('[type=radio][name=loadpage-filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
        enable("filetype")
        disable(selector = "[type=radio][value=sky]")
        disable(selector = "[type=radio][value=prog]")
        disable(selector = "[type=radio][value=spec]")
        disable(selector = "[type=radio][value=open]")
        disable(selector = "[type=radio][value=ump]")
        runjs("$.each($('[type=radio][name=loadpage-filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
        
      }
      else if (input$DDA_DIA %in% c("PTM", "PTM_TMT")) {
        runjs("$('[type=radio][name=loadpage-filetype]:disabled').parent().parent().parent().find('div.radio').css('opacity', 1)")
        enable("filetype")
        disable(selector = "[type=radio][value=sky]")
        disable(selector = "[type=radio][value=prog]")
        # disable(selector = "[type=radio][value=PD]")
        disable(selector = "[type=radio][value=openms]")
        # disable(selector = "[type=radio][value=spec]")
        disable(selector = "[type=radio][value=open]")
        disable(selector = "[type=radio][value=ump]")
        disable(selector = "[type=radio][value=spmin]")
        # disable(selector = "[type=radio][value=phil]")
        runjs("$.each($('[type=radio][name=loadpage-filetype]:disabled'), function(_, e){ $(e).parent().parent().css('opacity', 0.4) })")
      }
    })
    
    observeEvent(input$filetype,{
      enable("proceed1")
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
      getData(input)
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
          conditionalPanel(condition = "input['loadpage-DDA_DIA'] !== 'PTM'",
                           h4("Top 6 rows of the dataset"),
                           tableOutput(ns("summary"))
          ),
          conditionalPanel(condition = "input['loadpage-DDA_DIA'] == 'PTM'",
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