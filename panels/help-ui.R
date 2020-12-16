help = fluidPage(
  h4('Documentation'),
  tags$br(),
  p("This web application is based on the R package MSstats.  It does not require any software from the user, only a web browser to access it.  The data accepted by the application can be derived from labelled SRM (Selected Reaction Monitoring) experiments or label-free DDA (Data Dependent Acquisition) or DIA (Data Independent Acquisition). " ),
  tags$br(),
  h5("Tab: LOAD DATA"),
  p("MSstats takes input data in a tabular .csv format derived from any spectral processing tool such as Skyline, SuperHirn, MaxQuant, Progenesis, MultiQuant, OpenMS or OpenSWATH.  These formats can be converted to the required 10-column format through specific commands.  Conversion of formats Skyline, MaxQuant, Progenesis, and Proteome Discoverer requires annotation files."),
  p("The 10-column format of dataset is structured as follows :"),
  tags$ul(
    tags$li("ProteinName: Protein id"),
    tags$li("PeptideSequence"),
    tags$li("PrecursorCharge"),
    tags$li("FragmentIon"),
    tags$li("ProductCharge: Feature of a protein"),
    tags$li("IsotopeLabelType: endogenous peptides (use 'L') or labeled reference peptides (use 'H')"),
    tags$li("Condition: group type for group comparison or timepjoint for time-lapse analysis"),
    tags$li("BioReplicate: biological replicate"),
    tags$li("Run: mass spectrometry run"),
    tags$li("Intensity: quantified signal")
  ),
  tags$br(),
  # h5("Tab : QUALITY CONTROL"),
  # p("The data is transformed, normalised and summarised for statistical modelling."),
  # tags$ul(
  #   tags$li("Transformation: the Intensity column is transformed in either log2 or log10"),
  #   tags$li("Normalisation: the available methods are Equalisation of medians (indicated when the majority of proteins don’t change between runs, less indicated for label-free DDA), use of Global standards (defined by the user), ‘quantile’ (for label-free, all intensity distributions will become equal across runs; for label-based, all intensity distributions for references will become equal across runs and endogenous intensities shifted according to references), or no normalisation."),
  #   tags$li("Feature subset: use all feature or top 3 most significant (highest log2 transformed intensity) or top n (custom) most significant, or ‘high quality’ for most informative features (to eliminate unexplainable variation in features - interference)"),
  #   tags$li("Handling missing values (random missing measurements are independent of the abundance of the peptide; censored missing measurements are due to very low abundance, which falls below the level of detection of the instrument):"),
  #   tags$ul(
  #     tags$li("Assume all NAs as censored"),
  #     tags$li("Assume all intensities between 0 and 1 as censored and NAs as random missing (value of 1 can be changed)"),
  #     tags$li("Assume that all missing values are random missing ")
  #   ),
  #   p("With the assumption of the presence of censored values, a cutoff for the AFT model is determined:"),
  #   tags$ul(
  #     tags$li("Minimum value for each feature across runs"),
  #     tags$li("Minimum value for each run across features"),
  #     tags$li("Smallest value between minimum value of corresponding feature and minimum value of corresponding run (in this case runs with substantial missing runs will be biased so it may be a good option to remove all runs with more than 50% missing values)")
  #   ),
  #   tags$li("Summary method: TMP (Turkey’s median polish) or linear (linear mixed model).  TMP can be with model-based imputation of missing values (censored missing values will be imputed by Accelerated Failure Time model and cutoff is defined by the user- see above) or without imputation (in this case censored values will be substituted with a cutoff value specified by the user - see above). With linear summary method no imputation is performed;  in the presence of censored values parameters are estimated using AFT model, otherwise if all missing values are assumed to be random, the parameters are estimated through linear modelling.   With linear summary the variance in intensity from features can be considered equal or heterogeneous. ")
  # ),
  # tags$br(),
  # p("The preprocessed data is visualised through plots (for all proteins or individual proteins):"),
  # tags$ul(
  #   tags$li("QC plot: to visualise systematic biases between runs and to view the effects of normalisation"),
  #   tags$li("Profile plot: to identify potential sources of variation (individual measurements per protein and summarised data) and show missing data"),
  #   tags$li("Condition plot: to visualise potential differences in protein intensities between conditions (shown with arbitrary confidence interval or standard deviation bars).")
  # ),
  tags$br(),
  h5("Tab : STATISTICAL MODEL"),
  p("Based on the design of the experiment in analysis, a number of contrasts to be tested are specified in the application.  The contrasts will be between different conditions, different time points or different subjects of conditions for each subject.  The application then performs comparisons between the specified groups and returns a table with fold change and significance.  Positive values of fold change indicate up regulation, whereas negative values indicate down regulation."),
  tags$br(),
  p("The statistical model is based on two assumptions:"),
  tags$ul(
    tags$li("normal distribution of the measurement errors (QQ plot)"),
    tags$li("constant variance of the measurement error  (residual plot)")
  ),
  tags$br(),
  p("The results of the group comparisons may be visualised with:"),
  tags$ul(
    tags$li("Vulcano plot: to show the comparison between groups and the significance of the difference (adjusted p-value on the y axis): in red are the unregulated proteins, in blue the down regulated ones.  An horizontal line signals the false discovery rate cutoff (proteins above the line are statistically significant).  Each comparison has its own plot."),
    tags$li("Heat map: to view the patterns of regulation of proteins (rows) in the comparisons (columns).  Max 180 proteins will be shown for heat map.  "),
    tags$li("Comparison plot: to show log-fold changes in the different comparisons for each protein.")
  ),
  tags$br(),
  h5("Tab : FUTURE EXPERIMENTS"),
  p("In light of the statistical analysis performed in the application, the dataset can be viewed as a pilot study (assuming same experimental design and same expression of 99% of proteins) for future experiments, in order to relate the following statistical parameters:"),
  tags$ul(
    tags$li("Sample size: number of biological replicates per sample, number of peptides per protein, number of transition per peptide"),
    tags$li("Power: average across all proteins"),
    tags$li("Fold change: minimal fold change to detect or range"),
    tags$li("False discovery rate")
  ),
  p("All parameters must be specified but one."),
  tags$br(),
  p("The results of the estimations are visualised with plots."),
  # tags$br(),
  # h5("Tab : FUNCTIONAL ANALYSIS"),
  # p("The functional analysis is performed with the R package biomaRt, which accesses the Ensembl database for retrieval of GO terms and KEGG terms."),  
  # p("The analysis requires the selection of the following parameters:"),
  # tags$ul(
  #   tags$li("Organism: in which the analysis is performed (es. Homo sapiens, Mus musculus, etc)"),
  #   tags$li("ID type: format of ID for the proteins in the dataset (es. entrez gene, ensemble gene id, etc)"),
  #   tags$li("Attributes: features to retrieve from the query to the Ensembl database (es. GO id, GO description, KEGG id, etc)")
  # )
   
    
    
    
  

)
