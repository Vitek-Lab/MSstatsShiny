#' Help UI module for help page.
#'
#' This module shows the help page for general documentation
#'
#' @param id namespace prefix for the module
#' 
#' @return This function returns nothing, as it sets up the Help UI
#'
#' @export
helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      h3(HTML('<b>General Documentation</b>')),
      tags$br(),
      p("This application is based on the R packages MSstats, MSstatsTMT, and \
  MSstatsPTM. It is designed to analyze the results of LC/MS-MS experiments in \
    bottom-up proteomics. A variety of experimental designs, acquisition \
    methods, and biological questions can be analyzed. The sections below \
    correspond to the analyis tabs in the platform and correspond to the 
    analysis steps of MSstats. These include data upload and conversion, \
    feature level summarization, statistical modeling, and (for label-free \
    experiments) sample size calculations."),
      tags$br(),
      p("MSstats is described in detail in the user manual. If you have any \
  questions, please check the manual (linked below) as it will most likely have\
    the answer."),
      tags$a(href="https://msstats.org/wp-content/uploads/2020/02/MSstats_v3.18.1_manual_2020Feb26-v2.pdf", "MSstats User Manual"),
      tags$br(),
      h4(HTML('<b>Tab: Upload Data</b>')),
      p("MSstatsShiny takes input data in a tabular .csv format derived from \
    spectral processing tools such as Skyline, MaxQuant, Progenesis,\
    MultiQuant, OpenMS or OpenSWATH. MSstatsShiny includes functionality to \
    convert the output of these tools into the required input for downstream \
    analysis functions. Conversion of formats Skyline, MaxQuant, Progenesis, \
    and Proteome Discoverer requires annotation files. The annotation file must\
    be in a specific format. For help building the annotation file please see \
    the MSstats and MSstatsTMT Vignettes located in the help section of \
    MSstats-Shiny."),
      p("After conversion, the 10-column format of dataset is structured as follows :"),
      tags$ul(
        tags$li("ProteinName"),
        tags$li("PeptideSequence"),
        tags$li("PrecursorCharge"),
        tags$li("FragmentIon"),
        tags$li("ProductCharge"),
        tags$li("IsotopeLabelType"),
        tags$li("Condition"),
        tags$li("BioReplicate"),
        tags$li("Run"),
        tags$li("Intensity")
      ),
      tags$br(),
      h4(HTML('<b>Tab : Data Summarization</b>')),
      p("In this tab the converted feature-level data is summarized and prepared \
    for the modeling step. A protein feature is described as the combination of\
    the peptide and charge. These features are summarized into one value for 
    each protein in each MS run. There are a variety of options for both \
    how to summarize the data and other preprocessing steps (such as missing \
    value imputation). The options are described in detail below."),
      h5(HTML('<b>Label-Free (DDA/DIA/SRM/PRM)</b>')),
      tags$ul(
        tags$li("Log transformation: the Intensity column is transformed in either \
            log2 or log10 depending on which the user would prefer."),
        tags$li("Normalization: the available methods are Equalization of medians \
            (indicated when the majority of proteins don't change between runs,\
            less indicated for label-free DDA), use of Global standards \
            (defined by the user), 'quantile' (for label-free, all intensity \
            distributions will become equal across runs; for label-based, all \
            intensity distributions for references will become equal across \
            runs and endogenous intensities shifted according to references), \
            or no normalisation."),
        tags$li("Feature subset: use all feature or top 3 most significant (highest\
            log2 transformed intensity) or top n (custom) most significant, or \
            'high quality' for most informative features (to eliminate \
            unexplainable variation in features - interference). DIA \
            experiments should likely use a subset of features due to the very \
            large number of available features."),
        tags$li("Missing value imputation (can be MAR/MNAR depending on \
            spectral processing tool used.):"),
        tags$ul(
          tags$li("Assume all NAs as censored (ie missing not at random)"),
          tags$li("Assume all intensities between 0 and 1 as censored and NAs as \
              random missing."),
          tags$li("Assume that all missing values are missing at random")
        ),
        p("maxQuantileforCensored: Maximum quantile for deciding censored missing \
      values, default is 0.999"),
        tags$li("Summary method: 'TMP' (default) means Tukey's median polish, \
            which is a robust estimation method. Currenly only TMP is available\
            in the application. To use `linear` (which uses a linear mixed \
            model) please run MSstats via the command line.")
      ),
      h5(HTML('<b>TMT (DDA)</b>')),
      tags$ul(
        tags$li("Peptide level normalization: Global median normalization on \
            peptide level data (equalizing the medians across all the channels \
            and MS runs) It will be performed before protein-level \
            summarization."),
        tags$li("Summarization method: Method to be used for protein summarization.\
            Four different summarization methods to protein-level can be \
            performed : 'msstats'(default), 'MedianPolish', 'Median', 'LogSum'.\
            "),
        tags$li("Local protein normalization: Reference channel based normalization\
            between MS runs on protein level data. Requires one reference \
            channel in each MS run, annotated by 'Norm' in Condition column \
            of annotation file."),
        tags$li("Filtering: Whether to keep or remove normalization channel.")
      ),
      tags$br(),
      p("The preprocessed data is vizualised through plots (for all proteins or \
    individual proteins):"),
      tags$ul(
        tags$li("QC plot: to vizualise systematic biases between runs and to view \
            the effects of normalization"),
        tags$li("Profile plot: to identify potential sources of variation \
            (individual measurements per protein and summarized data) and show \
            missing data"),
      ),
      tags$br(),
      h4(HTML('<b>Tab : Statistical Model</b>')),
      p("Statistical modeling is compeleted in three steps. These are defining the \
    contrast matrix, generating the model, and viewing the results. These steps\
    are defined below."),
      tags$br(),
      p("The contast (comparison) matrix is how the user can specify what and how \
    conditions should be compared in the model. This is done in matrix format \
    and can be done in a pairwise or non-pairwise way. For more information on \
    how to build a contrast matrix please see the MSstats User Manual (linked \
    above)."),
      tags$br(),
      p("The statistical model is described in detail in the User Manual. In \
    general a linear mixed effects model is fit which is based on two \
    assumptions listed below. TMT modeling can optionally include Empirical \
    Bayes moderation."),
      tags$ul(
        tags$li("normal distribution of the measurement errors (QQ plot)"),
        tags$li("constant variance of the measurement error  (residual plot)")
      ),
      tags$br(),
      p("The results of the group comparisons may be vizualised with:"),
      tags$ul(
        tags$li("Volcano plot: to show the comparison between groups and the \
            significance of the difference (adjusted p-value on the y axis): \
            in red are the unregulated proteins, in blue the down regulated \
            ones. A horizontal line signals the false discovery rate cutoff \
            (proteins above the line are statistically significant).  Each \
            comparison has its own plot."),
        tags$li("Heatmap: to view the patterns of regulation of proteins (rows) \
            in the comparisons (columns). Max 180 proteins will be shown for \
            the heat map."),
        tags$li("Comparison plot: to show log-fold changes in the different \
            comparisons for each protein.")
      ),
      tags$br(),
      h5(HTML('<b>Tab : Future Experiments (Label-free only)</b>')),
      p("In light of the statistical analysis performed in the application, the \
    dataset can be viewed as a pilot study (assuming same experimental design \
    and same expression of 99% of proteins) for future experiments, in order to\
    relate the following statistical parameters:"),
      tags$ul(
        tags$li("Sample size: number of biological replicates per sample, number of\
            peptides per protein, number of transition per peptide"),
        tags$li("Power: average across all proteins"),
        tags$li("Fold change: minimal fold change to detect or range"),
        tags$li("False discovery rate")
      ),
      p("All parameters must be specified but one."),
      tags$br(),
      p("The results of the estimations are vizualised with plots."),
      
    )
    
  )
}