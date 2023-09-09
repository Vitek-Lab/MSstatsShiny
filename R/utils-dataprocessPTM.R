#' Determine if data is label free or TMT
#' @noRd
.check.dataProcess.plotting.data = function(data, type, ylimUp, ylimDown, 
                                            x.axis.size, y.axis.size, 
                                            text.size, text.angle, legend.size,
                                            dot.size.profile, ncol.guide, 
                                            width, height, ptm.title, 
                                            protein.title, which.Protein, 
                                            originalPlot, summaryPlot, address
) {
  
  ## Check input columns
  assertChoice(type, c("PROFILEPLOT", "QCPLOT"), .var.name = "Type") 
  if (!is.logical(ylimUp)){assertNumeric(ylimUp, .var.name = "ylimUp")}
  if (!is.logical(ylimDown)){assertNumeric(ylimDown, .var.name = "ylimDown")}
  assertNumeric(x.axis.size, .var.name = ("x.axis.size"))
  assertNumeric(y.axis.size, .var.name = ("y.axis.size"))
  assertNumeric(dot.size.profile, .var.name = ("dot.size.profile"))
  assertNumeric(text.size, .var.name = ("text.size"))
  assertNumeric(text.angle, .var.name = ("text.angle"))
  assertNumeric(legend.size, .var.name = ("legend.size"))
  assertNumeric(ncol.guide, .var.name = ("ncol.guide"))
  assertNumeric(width, .var.name = ("width"))
  assertNumeric(height, .var.name = ("height"))
  assertCharacter(ptm.title, .var.name = ("ptm.title"))
  assertCharacter(protein.title, .var.name = ("protein.title"))
  assertLogical(originalPlot, .var.name = c("originalPlot"))
  assertLogical(summaryPlot, .var.name = c("summaryPlot"))
  
  
  ## Test if input is labelfree or tmt
  data.ptm = data[['PTM']]$FeatureLevelData
  
  if ('Mixture' %in% colnames(data.ptm)){
    label = "TMT"
  }
  else {
    label = "LabelFree"
  }
  return(label)
}

#' Extract global protein from combined protein and site column
#' @noRd
.extractProteinPlot = function(ptm_model, protein_model){
  
  ## All proteins
  available_proteins = unique(as.character(protein_model$PROTEINNAME))
  available_proteins = available_proteins[order(nchar(available_proteins),
                                                available_proteins,
                                                decreasing = TRUE)]
  available_ptms = unique(as.character(ptm_model$PROTEINNAME))
  
  ## Call Rcpp function
  ptm_proteins = extract_protein_name(available_ptms,
                                      available_proteins)
  global_protein_lookup = data.table(PROTEINNAME = available_ptms, 
                                     GLOBALPROTEIN = ptm_proteins)
  
  return(global_protein_lookup)
}

#' Prepare data into format needed for plotting
#' @noRd
.format.data.process.plots = function(data, label) {
  
  data.ptm = data[['PTM']]
  data.protein = data[['PROTEIN']]
  
  datafeature.ptm = data.ptm$FeatureLevelData
  datafeature.ptm = as.data.table(datafeature.ptm)
  
  datarun.ptm = data.ptm$ProteinLevelData
  datarun.ptm = as.data.table(datarun.ptm)
  
  data.list = list(datafeature.ptm, datarun.ptm)
  
  if (!is.null(data.protein)) {
    datafeature.protein = data.protein$FeatureLevelData
    datafeature.protein = as.data.table(datafeature.protein)
    
    datarun.protein = data.protein$ProteinLevelData
    datarun.protein = as.data.table(datarun.protein)
    
    data.list = list(datafeature.protein, datafeature.ptm, 
                     datarun.protein, datarun.ptm)
  }
  
  ## Adjust colnames to avoid repeating code
  for (i in seq_along(data.list)) {
    colnames(data.list[[i]]) = toupper(colnames(data.list[[i]]))
    if ('INTENSITY' %in% colnames(data.list[[i]])){
      data.list[[i]]$ABUNDANCE = log2(data.list[[i]]$INTENSITY)
    }
    setnames(data.list[[i]], c('LOGINTENSITIES', 'GROUP', 'PROTEIN', 
                               'Protein', 'LOG2INTENSITY'), 
             c('ABUNDANCE', 'CONDITION', 'PROTEINNAME', 'PROTEINNAME',
               'ABUNDANCE'), 
             skip_absent = TRUE)
    
    data.list[[i]][!is.na(data.list[[i]]$ABUNDANCE) &
                     data.list[[i]]$ABUNDANCE < 1, 'ABUNDANCE'] = 0
    data.list[[i]] = data.list[[i]][ABUNDANCE > 0, ]
  }
  
  ## Run Rcpp code to match proteins
  if (!is.null(data.protein)) {
    protein_lookup = .extractProteinPlot(data.list[[2]], data.list[[1]])
    
    ## Add extracted protein name into model
    data.list[[2]] = merge(data.list[[2]], protein_lookup,
                           all.x = TRUE, by = 'PROTEINNAME')
    data.list[[4]] = merge(data.list[[4]], protein_lookup,
                           all.x = TRUE, by = 'PROTEINNAME')
    
    # conditions in feature data
    fea.conds.protein = as.character(unique(data.list[[1]]$CONDITION))
    fea.conds.ptm = as.character(unique(data.list[[2]]$CONDITION))
    # conditions in protein data
    run.conds.protein = as.character(unique(data.list[[3]]$CONDITION))
    run.conds.ptm = as.character(unique(data.list[[4]]$CONDITION))
    
    # only keep the overlapped conditions between feature and run data
    shared.conds = Reduce(intersect, list(fea.conds.protein, fea.conds.ptm,
                                          run.conds.protein, run.conds.ptm))
    for (i in seq_along(data.list)) {
      data.list[[i]]$PROTEINNAME = factor(data.list[[i]]$PROTEINNAME)
      data.list[[i]]$RUN = factor(data.list[[i]]$RUN)
      data.list[[i]]$CONDITION = factor(data.list[[i]]$CONDITION)	
      data.list[[i]] = data.list[[i]][data.list[[i]]$CONDITION %in% shared.conds
                                      ,]
    }
  } else {
    fea.conds.ptm = as.character(unique(data.list[[1]]$CONDITION))
    run.conds.ptm = as.character(unique(data.list[[2]]$CONDITION))
    
    # only keep the overlapped conditions between feature and run data
    shared.conds = Reduce(intersect, list(fea.conds.ptm, run.conds.ptm))
    for (i in seq_along(data.list)) {
      data.list[[i]]$PROTEINNAME = factor(data.list[[i]]$PROTEINNAME)
      data.list[[i]]$RUN = factor(data.list[[i]]$RUN)
      data.list[[i]]$CONDITION = factor(data.list[[i]]$CONDITION)	
      data.list[[i]] = data.list[[i]][data.list[[i]]$CONDITION %in% shared.conds
                                      ,]
    }
  }
  
  return(data.list)
}

#' Format TMT data for profile plot and qcplot
#' @noRd
.preplot.format.tmt = function(datafeature, datarun, y.limup, type){
  
  RUN = CONDITION = CHANNEL = groupAxis = .N = cumGroupAxis = NULL
  
  datafeature = datafeature[with(datafeature, order(RUN, CONDITION, CHANNEL)),]
  
  datafeature$CONDITION = factor(datafeature$CONDITION)
  datarun$CONDITION = factor(datarun$CONDITION)
  
  datafeature$RUN = factor(datafeature$RUN)
  datarun$RUN = factor(datarun$RUN)
  
  ## !! important: order of x-axis
  ## can be reorder by group and then channel, WITHIN Run
  ## first make new column for x-axis
  datafeature$group.channel = paste(datafeature$CONDITION, datafeature$CHANNEL, 
                                    sep = "_")
  
  ## not sure better way for coding
  ## potentially change it.
  datafeature$xorder = factor()
  
  for (k in seq_along(unique(datafeature$RUN))) {
    
    runid = as.character(unique(datafeature$RUN)[k])
    datafeature[datafeature$RUN == runid, ]$xorder = factor(
      datafeature[datafeature$RUN == runid, ]$group.channel,
      levels = unique(datafeature[datafeature$RUN == runid, ]$group.channel),
      labels = seq(1, length(unique(datafeature[datafeature$RUN == runid, 
      ]$group.channel))))
  }
  
  ## need to make data.frame with same variables for condition name
  datafeature$xorder = as.numeric(datafeature$xorder)
  
  ## keep unique information for x-axis labeling. will be used in plotting
  tempGroupName = unique(datafeature[, c("CONDITION", "xorder", "RUN", 
                                         "CHANNEL")])
  groupline = copy(tempGroupName)
  groupline[, groupAxis := .N, by=list(CONDITION, RUN)]
  groupline[, c("xorder", "CHANNEL") := NULL][]
  groupline = groupline[!duplicated(groupline), ]
  
  groupline[, cumGroupAxis := cumsum(groupAxis), by = list(RUN)]
  groupline$cumGroupAxis = groupline$cumGroupAxis + 0.5
  
  ## add coordinate for group id
  groupline$xorder = groupline$cumGroupAxis - groupline$groupAxis / 2
  groupline$abundance = y.limup - 0.5
  
  groupline.all = groupline
  
  ## remove last condition for vertical line between groups
  groupline = groupline[-which(groupline$CONDITION %in% levels(
    groupline$CONDITION)[nlevels(groupline$CONDITION)]), ]
  
  ## need to fill in incomplete rows for Runlevel data
  if (type == 'PROFILEPLOT'){
    haverun = FALSE
    
    if (sum(is.element(colnames(datarun), "RUN")) != 0) {
      datamat = dcast(PROTEINNAME + CHANNEL ~ RUN, data = datarun,
                      value.var = 'ABUNDANCE', keep = TRUE)
      
      datarun = melt(datamat, id.vars=c('PROTEINNAME', 'CHANNEL'))
      colnames(datarun)[colnames(datarun) %in% c("variable", "value")
      ] = c('RUN', 'ABUNDANCE')
      
      ## match x axis order
      datarun = merge(datarun, tempGroupName, by = c('RUN', 'CHANNEL'))
      haverun = TRUE
    }
  }
  
  return(list(datafeature, datarun, groupline, groupline.all))
}

#' Plot standard profile plot for TMT
#' @noRd
.plot.profile.tmt = function(data.list, protein, y.limup, y.limdown, 
                             x.axis.size, y.axis.size, text.size, text.angle, 
                             legend.size, dot.size.profile, ncol.guide){
  
  cumGroupAxis = xorder = abundance = CONDITION = NULL
  
  datafeature = data.list[[1]]
  datarun = data.list[[2]]
  groupline = data.list[[3]]
  groupline.all = data.list[[4]]
  
  sub = datafeature[datafeature$PROTEINNAME == protein, ]
  
  sub$PEPTIDESEQUENCE = factor(as.character(sub$PEPTIDESEQUENCE))
  sub$CHARGE = factor(as.character(sub$CHARGE))
  sub$PSM = factor(as.character(sub$PSM))
  
  # if all measurements are NA,
  if (nrow(sub) == sum(is.na(sub$ABUNDANCE))|
      nrow(sub) == sum(!is.na(sub$ABUNDANCE) & sub$ABUNDANCE == 0)) {
    message(paste0("Can't the Profile plot for ", protein,
                   " because all measurements are NAs or zero."))
  }
  
  ## seq for peptide and charge
  ## for seting up color and linetype
  b = unique(sub[, c("PEPTIDESEQUENCE", "PSM")])
  ## add because if there are missing value, orders are different.
  b = b[with(b, order(PEPTIDESEQUENCE, PSM)), ]
  
  temp1 = xtabs(~PEPTIDESEQUENCE, b)
  
  ## unique charge id within peptide sequence, for line type
  ss = NULL
  ## unique peptide sequence id, for color
  s = NULL
  
  for (j in seq_along(temp1)) {
    temp3 = rep(j, temp1[j])
    s = c(s, temp3)
    temp2 = seq(1, temp1[j])
    ss = c(ss, temp2)
  }
  
  ## for annotation of condition
  groupline.tmp = data.table(groupline, "PSM" = unique(sub$PSM)[1], 
                             "PEPTIDESEQUENCE" = unique(sub$PEPTIDESEQUENCE)[1]
  )
  
  groupline.all.tmp = data.table(groupline.all, "PSM" = unique(sub$PSM)[1],
                                 "PEPTIDESEQUENCE" = unique(sub$PEPTIDESEQUENCE
                                 )[1])
  
  ## 2019. 12. 17, MC : for profile plot, define color for dot
  cbp = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
          "#D55E00", "#CC79A7")
  check.length = length(unique(s)) %/% length(cbp)
  if ( check.length > 0 ){
    cbp = rep(cbp, times=check.length + 1)
  } else {
    cbp = cbp
  }
  
  yaxis.name = 'Log2-intensities'
  
  ## 1st plot for Protein plot
  protein_temp = ggplot(aes_string(x = 'xorder', y = 'ABUNDANCE',
                                   color = 'PSM', linetype = 'PSM'),
                        data = sub) +
    facet_grid(~RUN) +
    geom_point(size=dot.size.profile, na.rm=TRUE) +
    geom_line(size = 0.5, na.rm=TRUE) +
    scale_colour_manual(values=cbp[s]) +
    scale_linetype_manual(values = ss) +
    scale_shape_manual(values = c(16)) +
    labs(title = unique(sub$PROTEINNAME),
         x = 'MS runs') +
    scale_y_continuous(yaxis.name, limits = c(y.limdown, y.limup)) +
    scale_x_continuous('MS runs') +
    geom_vline(data = groupline.tmp,
               aes(xintercept = cumGroupAxis),
               colour = "grey", linetype = "longdash") +
    geom_text(data = groupline.all.tmp,
              aes(x = xorder, y = abundance, label = CONDITION),
              size = text.size,
              angle = text.angle, hjust = .9,
              color = "black") +
    theme_msstats(type = "PROFILEPLOT",x.axis.size, y.axis.size, legend.size,
                  text_angle = text.angle) +
    guides(color = guide_legend(title = paste("# peptide:", nlevels(
      sub$PEPTIDESEQUENCE)),
      title.theme = element_text(size = 13, angle = 0),
      keywidth = 0.4,
      keyheight = 0.1,
      default.unit = 'inch',
      ncol = ncol.guide),
      linetype = guide_legend(
        title = paste("# peptide:",
                      nlevels(sub$PEPTIDESEQUENCE)),
        title.theme = element_text(size = 13, angle = 0),
        keywidth = 0.4,
        keyheight = 0.1,
        default.unit = 'inch',
        ncol = ncol.guide))
  
  return(protein_temp)
  
}

#' Plot summarized profile plot for TMT
#' @noRd
.plot.profile.summary.tmt = function(data.list, protein, y.limup, y.limdown, 
                                     x.axis.size, y.axis.size, text.size, 
                                     text.angle, legend.size, dot.size.profile,
                                     ncol.guide){
  
  cumGroupAxis = xorder = abundance = CONDITION = ABUNDANCE = analysis = NULL
  
  datafeature = data.list[[1]]
  datarun = data.list[[2]]
  groupline = data.list[[3]]
  groupline.all = data.list[[4]]
  
  sub = datafeature[datafeature$PROTEINNAME == protein, ]
  
  sub$PEPTIDESEQUENCE = factor(as.character(sub$PEPTIDESEQUENCE))
  sub$CHARGE = factor(as.character(sub$CHARGE))
  sub$PSM = factor(as.character(sub$PSM))
  
  # if all measurements are NA,
  if (nrow(sub) == sum(is.na(sub$ABUNDANCE))|
      nrow(sub) == sum(!is.na(sub$ABUNDANCE) & sub$ABUNDANCE == 0)) {
    message(paste0("Can't the Profile plot for ", protein,
                   " because all measurements are NAs or zero."))
  }
  
  ## for annotation of condition
  groupline.tmp = data.table(groupline, "PSM" = unique(sub$PSM)[1],
                             "PEPTIDESEQUENCE" = unique(
                               sub$PEPTIDESEQUENCE)[1])
  
  groupline.all.tmp = data.table(groupline.all, "PSM" = unique(sub$PSM)[1],
                                 "PEPTIDESEQUENCE" = unique(
                                   sub$PEPTIDESEQUENCE)[1])
  
  subrun = datarun[datarun$PROTEINNAME == protein, ]
  
  if (nrow(subrun) != 0) {
    
    quantrun = sub[1, ]
    quantrun = quantrun[rep(seq_len(nrow(subrun))), ]
    
    quantrun$PROTEINNAME = subrun$PROTEINNAME
    quantrun$GLOBALPROTEIN = subrun$GLOBALPROTEIN
    quantrun$group.channel = NA
    quantrun$INTENSITY = NA
    quantrun$CONDITION = NA
    quantrun$MIXTURE = NA
    quantrun$TECHREPMIXTURE = NA
    quantrun$BIOREPLICATE = NA
    quantrun$PEPTIDESEQUENCE = "Run summary"
    quantrun$CHARGE = "Run summary"
    quantrun$PSM = "Run summary"
    quantrun$CHANNEL = subrun$CHANNEL
    quantrun$RUN = subrun$RUN
    quantrun$ABUNDANCE = subrun$ABUNDANCE
    quantrun$xorder = subrun$xorder
    
  } else {
    ## if there is only one Run measured across all runs
    ## no Run information for linear with censored
    quantrun = datafeature[1, ]
    quantrun[, 2:ncol(quantrun)] = NA
    
    quantrun$PROTEINNAME = levels(datafeature$PROTEINNAME)[1]
    quantrun$PEPTIDESEQUENCE = "Run summary"
    quantrun$CHARGE = "Run summary"
    quantrun$PSM = "Run summary"
    quantrun$ABUNDANCE = NA
    quantrun$INTENSITY = NA
  }
  
  quantrun$analysis = "Run summary"
  sub$analysis = "Processed feature-level data"
  
  final = rbindlist(list(sub, quantrun), fill=TRUE)
  final$analysis = factor(final$analysis)
  final$PSM = factor(final$PSM)
  yaxis.name = 'Log2-intensities'
  
  ## Draw summarized ptm plot
  ptempall = ggplot(aes_string(x = 'xorder', y = 'ABUNDANCE', 
                               color = 'analysis', linetype = 'PSM', 
                               size = 'analysis'), data = final) +
    facet_grid(~RUN) +
    geom_point(size = dot.size.profile, na.rm=TRUE) +
    geom_line(size = 0.5, na.rm=TRUE) +
    scale_colour_manual(values = c("lightgray", "darkred")) +
    scale_shape_manual(values = c(16)) +
    scale_size_manual(values = c(1.7, 2), guide = "none") +
    scale_linetype_manual(values = c(rep(1, times = length(
      unique(final$PSM))-1), 2), guide = "none") +
    labs(title = unique(sub$PROTEINNAME),
         x = 'MS runs') +
    scale_y_continuous(yaxis.name, limits = c(y.limdown, y.limup)) +
    geom_vline(data = groupline.tmp,
               aes(xintercept = cumGroupAxis),
               colour = "grey", linetype = "longdash") +
    geom_text(data = groupline.all.tmp,
              aes(x = xorder, y = abundance, label = CONDITION),
              size = text.size,
              angle = text.angle, hjust = .9,
              color = "black") +
    theme_msstats(type = "PROFILEPLOT",x.axis.size, y.axis.size, legend.size,
                  text_angle = text.angle) +
    guides(color = guide_legend(order = 1,
                                title = NULL,
                                label.theme = element_text(
                                  size = 10, angle = 0)))
  
  ## draw point again because some red summary dots could be hiden
  ptempall = ptempall + geom_point(data = final, aes(
    x = xorder, y = ABUNDANCE, size = analysis, color = analysis)
  )
  
  return(ptempall)
}

#' Wrapper function for TMT profile plot
#' @noRd
.profile.tmt = function(data.table.list, type, ylimUp, ylimDown, 
                        x.axis.size, y.axis.size,text.size,text.angle, 
                        legend.size, dot.size.profile, ncol.guide, width, 
                        height,which.Protein,originalPlot,summaryPlot,
                        address){
  
  PROTEINNAME = GLOBALPROTEIN = NULL
  
  if (length(data.table.list) == 4){
    datafeature.protein = data.table.list[[1]]
    datafeature.ptm = data.table.list[[2]]
    datarun.protein = data.table.list[[3]]
    datarun.ptm = data.table.list[[4]]
    y.limup = ceiling(max(datafeature.protein$ABUNDANCE,
                          datafeature.ptm$ABUNDANCE, na.rm = TRUE) + 5)
  } else {
    datafeature.ptm = data.table.list[[1]]
    datarun.ptm = data.table.list[[2]]
    
    y.limup = ceiling(max(datafeature.ptm$ABUNDANCE, na.rm = TRUE) + 5)
  }
  ## TODO: Add log
  # processout = rbind(processout,
  #                     c("ProfilePlot plotting started."))
  
  
  if (length(data.table.list) == 4){
    plot_global = TRUE
  } else {
    plot_global = FALSE
  }
  
  ## choose Proteins or not
  if (which.Protein[[1]] != "all") {
    ## check which.Protein is name of Protein
    if (is.character(which.Protein)) {
      temp.name = which.Protein
      
      ## message if name of Protein is wrong.
      if (length(setdiff(temp.name,unique(datafeature.ptm$PROTEINNAME))) > 0) {
        stop("Please check protein name. Dataset does not
             have this protein. - ",
             toString(temp.name))
      }
    }
    
    ## check which.Protein is order number of Protein
    else if (is.numeric(which.Protein)) {
      temp.name = levels(datafeature.ptm$PROTEINNAME)[which.Protein]
      
      ## message if name of Protein is wrong.
      if (length(levels(datafeature.ptm$PROTEINNAME)) < max(which.Protein)) {
        stop("Please check your number of proteins. There are ",
             length(levels(datafeature.ptm$PROTEINNAME))," proteins in this
             dataset.")
      }
    }
    
    ## use only assigned proteins
    datafeature.ptm = datafeature.ptm[which(datafeature.ptm$PROTEINNAME %in%
                                              temp.name), ]
    datafeature.ptm$PROTEINNAME = factor(datafeature.ptm$PROTEINNAME)
    datarun.ptm = datarun.ptm[which(datarun.ptm$PROTEINNAME %in% temp.name), ]
    datarun.ptm$PROTEINNAME = factor(datarun.ptm$PROTEINNAME)
    
    if (length(data.table.list) == 4){
      temp_proteins = as.character(unique(datafeature.ptm$GLOBALPROTEIN))
      
      plot_global = TRUE
      ## Check if there is a corresponding protein for the PTM
      if (!temp_proteins %in% datafeature.protein$PROTEINNAME){
        message(paste0("Global Protein data not available for ", 
                       as.character(temp_proteins), 
                       ", only PTM will be plotted.")
        )
        plot_global = FALSE
      }
      
      datafeature.protein = datafeature.protein[which(
        datafeature.protein$PROTEINNAME %in% temp_proteins), ]
      datafeature.protein$PROTEINNAME = factor(datafeature.protein$PROTEINNAME)
      
      datarun.protein = datarun.protein[which(datarun.protein$PROTEINNAME %in%
                                                temp_proteins), ]
      datarun.protein$PROTEINNAME = factor(datarun.protein$PROTEINNAME)
    }
  }
  
  ## assign upper or lower limit
  if (is.numeric(ylimUp)) {
    y.limup = ylimUp
  }
  if (is.numeric(ylimDown)) {
    y.limdown = ylimDown
  } else {
    y.limdown = 0
  }
  
  ## Apply pre-plot formatting
  ptm.list = .preplot.format.tmt(datafeature.ptm, datarun.ptm, 
                                 y.limup, type)
  datafeature.ptm = ptm.list[[1]]
  datarun.ptm = ptm.list[[2]]
  groupline.ptm = ptm.list[[3]]
  groupline.all.ptm = ptm.list[[4]]
  
  if (plot_global){
    protein.list = .preplot.format.tmt(datafeature.protein, datarun.protein, 
                                       y.limup, type)
    datafeature.protein = protein.list[[1]]
    datarun.protein = protein.list[[2]]
    groupline.protein = protein.list[[3]]
    groupline.all.protein = protein.list[[4]]
    
    ## Only plot proteins that occur in both datasets
    global_proteins = unique(datafeature.protein[, PROTEINNAME])
    ptm_proteins = unique(datafeature.ptm[, GLOBALPROTEIN])
    plot_proteins = intersect(ptm_proteins, global_proteins)
    datafeature.ptm = datafeature.ptm[GLOBALPROTEIN %in% plot_proteins,]
    plot_proteins = unique(datafeature.ptm[, c('PROTEINNAME', 'GLOBALPROTEIN')]
    )
  } else {
    plot_proteins = unique(datafeature.ptm[, c('PROTEINNAME')])
  }
  
  if (originalPlot) {
    if (address != FALSE) {
      allfiles = list.files()
      
      num = 0
      filenaming = paste0(address, "ProfilePlot")
      finalfile = paste0(address, "ProfilePlot.pdf")
      
      while (is.element(finalfile, allfiles)) {
        num = num + 1
        finalfile = paste0(paste(filenaming, num, sep = "-"), ".pdf")
      }
      
      pdf(finalfile, width = width, height = height)
    }
    
    ## factoring for run, channel, condition should be done before loop
    
    for (i in seq_len(nrow(plot_proteins))) {
      
      ptm_temp = .plot.profile.tmt(ptm.list, 
                                   as.character(plot_proteins[, PROTEINNAME][i]), 
                                   y.limup, y.limdown, x.axis.size,
                                   y.axis.size, text.size, text.angle, legend.size,
                                   dot.size.profile, ncol.guide)
      if (plot_global){
        protein_temp = .plot.profile.tmt(protein.list, 
                                         as.character(
                                           plot_proteins[, GLOBALPROTEIN][i]), 
                                         y.limup, y.limdown, x.axis.size,
                                         y.axis.size, text.size, text.angle, 
                                         legend.size, dot.size.profile, ncol.guide)
        
        grid.arrange(ptm_temp, protein_temp, ncol=1)
      } else {print(ptm_temp)}
      message(paste0("Drew the Profile plot for ", 
                     as.character(plot_proteins[, PROTEINNAME][i]),
                     " (", i, " of ", nrow(plot_proteins), ")"))
    }
    # end-loop for each protein
    
    if (address != FALSE) {
      dev.off()
    }
    
  } # end original plot
  
  ############################################
  ## 2nd plot for original plot : summary
  ############################################
  
  if (summaryPlot) {
    if (address != FALSE) {
      allfiles = list.files()
      
      num = 0
      filenaming = paste0(address, "ProfilePlot_wSummarization")
      finalfile = paste0(address, "ProfilePlot_wSummarization.pdf")
      
      while (is.element(finalfile, allfiles)) {
        num = num + 1
        finalfile = paste0(paste(filenaming, num, sep = "-"), ".pdf")
      }
      
      pdf(finalfile, width = width, height = height)
    }
    
    for (i in seq_len(nrow(plot_proteins))) {
      
      ptm_temp = .plot.profile.summary.tmt(ptm.list, as.character(
        plot_proteins[, PROTEINNAME][i]), y.limup, y.limdown, x.axis.size,
        y.axis.size, text.size, text.angle, legend.size,dot.size.profile, 
        ncol.guide)
      if (plot_global){
        protein_temp = .plot.profile.summary.tmt(protein.list, as.character(
          plot_proteins[, GLOBALPROTEIN][i]), y.limup, y.limdown, x.axis.size,
          y.axis.size, text.size, text.angle, legend.size,
          dot.size.profile, ncol.guide)
        
        grid.arrange(ptm_temp, protein_temp, ncol=1)
      } else {print(ptm_temp)}
      
      message(paste("Drew the Profile plot for ", 
                    as.character(plot_proteins[, PROTEINNAME][i]),
                    "(", i, " of ", nrow(plot_proteins), ")"))
      
    }
    
    if (address!=FALSE) {
      dev.off()
    }
  } # end summarization plot
}

#' Plot boxplot of all proteins
#' @noRd
.qc.all.plot = function(datafeature, groupline, groupline.all, title, ylimup, 
                        ylimdown, x.axis.size, y.axis.size, text.size, 
                        text.angle) {
  
  cumGroupAxis = xorder = abundance = CONDITION = NULL
  
  ## for annotation of condition
  groupline.tmp = data.table(groupline, "PSM" = unique(datafeature$PSM)[1], 
                             "PEPTIDESEQUENCE" = unique(
                               datafeature$PEPTIDESEQUENCE)[1])
  
  groupline.all.tmp = data.table(groupline.all, 
                                 "PSM" = unique(datafeature$PSM)[1], 
                                 "PEPTIDESEQUENCE" = unique(
                                   datafeature$PEPTIDESEQUENCE)[1])
  
  ## 1st plot for original plot
  ## for boxplot, x-axis, xorder should be factor
  datafeature$xorder = factor(datafeature$xorder)
  
  ## y-axis labeling
  yaxis.name = 'Log2-intensities'
  
  ptemp = ggplot(aes_string(x = 'xorder', y = 'ABUNDANCE'),
                 data = datafeature) +
    facet_grid(~RUN) +
    geom_boxplot(aes_string(fill = 'CONDITION'), outlier.shape = 1,
                 outlier.size = 1.5) +
    labs(title = title, x = 'MS runs') +
    scale_y_continuous(yaxis.name, limits = c(ylimdown, ylimup)) +
    geom_vline(data = groupline.tmp,
               aes(xintercept = cumGroupAxis),
               colour = "grey", linetype = "longdash") +
    geom_text(data = groupline.all.tmp,
              aes(x = xorder, y = abundance, label = CONDITION),
              size = text.size,
              angle = text.angle, hjust = .9,
              color = "black") +
    theme_msstats(type = "PROFILEPLOT", x.axis.size, y.axis.size, 13, 
                  element_rect(fill = "gray95"),
                  element_text(colour = c("#00B0F6"), size = 14),
                  "none", text_angle = text.angle)
  # theme(
  #   panel.background = element_rect(fill = 'white', colour = "black"),
  #   legend.key = element_rect(fill = 'white', colour = 'white'),
  #   panel.grid.minor = element_blank(),
  #   strip.background = element_rect(fill = 'gray95'),
  #   axis.ticks.x = element_blank(),
  #   axis.text.x = element_blank(),
  #   axis.text.y = element_text(size = y.axis.size, colour = "black"),
  #   axis.ticks = element_line(colour = "black"),
  #   axis.title.x = element_text(size = x.axis.size + 5, vjust = -0.4),
  #   axis.title.y = element_text(size = y.axis.size + 5, vjust = 0.3),
  #   title = element_text(size = x.axis.size + 8, vjust = 1.5),
  #   legend.position = "none")
  
  return(ptemp)
}

#' Plot boxplot of single protein
#' @noRd
.qc.single.plot = function(datafeature, groupline, groupline.all, protein, 
                           ylimup, ylimdown, x.axis.size, y.axis.size, 
                           text.size, text.angle){
  
  ABUNDANCE = cumGroupAxis = xorder = abundance = CONDITION = NULL
  
  sub = datafeature[datafeature$PROTEINNAME == protein]
  sub = sub[!is.na(sub$ABUNDANCE)]
  
  ## if all protein measurements are NA,
  if (nrow(sub) == sum(sub[!is.na(sub$ABUNDANCE), ABUNDANCE])) {
    message(paste("Can't the Quality Control plot for ", unique(
      sub$PROTEINNAME), " because all measurements are NAs."))
  }
  
  ## for annotation of condition
  groupline.tmp = data.table(groupline, "PSM" = unique(sub$PSM)[1], 
                             "PEPTIDESEQUENCE" = unique(sub$PEPTIDESEQUENCE)[1]
  )
  
  groupline.all.tmp = data.table(groupline.all, "PSM" = unique(sub$PSM)[1],
                                 "PEPTIDESEQUENCE" = unique(
                                   sub$PEPTIDESEQUENCE)[1])
  
  ## 1st plot for original plot
  ## for boxplot, x-axis, xorder should be factor
  sub$xorder = factor(sub$xorder)
  yaxis.name = 'Log2-intensities'
  
  ptemp = ggplot(aes_string(x = 'xorder', y = 'ABUNDANCE'),
                 data = sub) +
    facet_grid(~RUN) +
    geom_boxplot(aes_string(fill = 'CONDITION'), outlier.shape = 1,
                 outlier.size = 1.5) +
    labs(title = protein, x = 'MS runs') +
    scale_y_continuous(yaxis.name, limits = c(ylimdown, ylimup)) +
    geom_vline(data = groupline.tmp,
               aes(xintercept = cumGroupAxis),
               colour = "grey", linetype = "longdash") +
    geom_text(data = groupline.all.tmp,
              aes(x = xorder, y = abundance, label = CONDITION),
              size = text.size,
              angle = text.angle, hjust = .9,
              color = "black") +
    theme_msstats(type = "PROFILEPLOT", x.axis.size, y.axis.size, 13, 
                  element_rect(fill = "gray95"),
                  element_text(colour = c("#00B0F6"), size = 14),
                  "none", text_angle = text.angle)
  # theme(
  #   panel.background = element_rect(fill = 'white', colour = "black"),
  #   legend.key = element_rect(fill = 'white', colour = 'white'),
  #   panel.grid.minor = element_blank(),
  #   strip.background = element_rect(fill = 'gray95'),
  #   axis.ticks.x = element_blank(),
  #   axis.text.x = element_blank(),
  #   axis.text.y = element_text(size = y.axis.size, colour = "black"),
  #   axis.ticks = element_line(colour = "black"),
  #   axis.title.x = element_text(size = x.axis.size + 5, vjust = -0.4),
  #   axis.title.y = element_text(size = y.axis.size + 5, vjust = 0.3),
  #   title = element_text(size = x.axis.size + 8, vjust = 1.5),
  #   legend.position = "none")
  
  return(ptemp)
  
}

#' Wrapper function for plotting QC Plots
#' @noRd
.qc.tmt = function(data.table.list, type, ylimUp, ylimDown, width, height,
                   x.axis.size, y.axis.size,text.size, text.angle, 
                   which.Protein, address, ptm_title, protein_title) {
  
  PROTEINNAME = GLOBALPROTEIN = NULL
  
  if (length(data.table.list) == 4){
    datafeature.protein = data.table.list[[1]]
    datafeature.ptm = data.table.list[[2]]
    datarun.protein = data.table.list[[3]]
    datarun.ptm = data.table.list[[4]]
    
    y.limup = ceiling(max(datafeature.protein$ABUNDANCE,
                          datafeature.ptm$ABUNDANCE, na.rm = TRUE) + 3)
  } else {
    datafeature.ptm = data.table.list[[1]]
    datarun.ptm = data.table.list[[2]]
    
    y.limup = ceiling(max(datafeature.ptm$ABUNDANCE, na.rm = TRUE) + 3)
  }
  
  ## save the plots as pdf or not
  ## If there are the file with the same name
  ## add next numbering at the end of file name
  if (address != FALSE) {
    allfiles = list.files()
    
    num = 0
    filenaming = paste0(address,"QCPlot")
    finalfile = paste0(address,"QCPlot.pdf")
    
    while (is.element(finalfile, allfiles)) {
      num = num + 1
      finalfile = paste0(paste(filenaming, num, sep = "-"), ".pdf")
    }
    
    pdf(finalfile, width = width, height = height)
  }
  
  ## assign upper or lower limit
  if (is.numeric(ylimUp)) {
    y.limup = ylimUp
  }
  
  y.limdown = 0
  if (is.numeric(ylimDown)) {
    y.limdown = ylimDown
  }
  
  ## Apply pre-plot formatting
  ptm.list = .preplot.format.tmt(datafeature.ptm, datarun.ptm, y.limup, 
                                 type)
  datafeature.ptm = ptm.list[[1]]
  datarun.ptm = ptm.list[[2]]
  groupline.ptm = ptm.list[[3]]
  groupline.all.ptm = ptm.list[[4]]
  
  if (length(data.table.list) == 4){
    protein.list = .preplot.format.tmt(datafeature.protein, datarun.protein,
                                       y.limup, type)
    datafeature.protein = protein.list[[1]]
    datarun.protein = protein.list[[2]]
    groupline.protein = protein.list[[3]]
    groupline.all.protein = protein.list[[4]]
  }
  
  ## all protein
  if (which.Protein[[1]] == 'all' | which.Protein[[1]] == 'allonly') {
    
    ## Plot all QC
    ptemp.ptm = .qc.all.plot(datafeature.ptm, groupline.all.ptm, 
                             groupline.all.ptm, ptm_title, y.limup, y.limdown, 
                             x.axis.size, y.axis.size, text.size, text.angle)
    if (length(data.table.list) == 4){
      ptemp.protein = .qc.all.plot(datafeature.protein, groupline.protein, 
                                   groupline.all.protein, protein_title, y.limup,
                                   y.limdown, x.axis.size, y.axis.size,text.size, 
                                   text.angle)
      grid.arrange(ptemp.ptm, ptemp.protein, ncol=1)
    } else {
      print(ptemp.ptm)
    }
    message("Drew the Quality Contol plot(boxplot) for all ptms/proteins.")
  }
  
  if (length(data.table.list) == 4){
    plot_global = TRUE
  } else {
    plot_global = FALSE
  }
  
  ## each protein
  ## choose Proteins or not
  if (which.Protein[[1]] != 'allonly') {
    if (which.Protein[[1]] != "all") {
      ## check which.Protein is name of Protein
      if (is.character(which.Protein)) {
        temp.name = which.Protein
        
        ## message if name of Protein is wrong.
        if (length(setdiff(temp.name,unique(datafeature.ptm$PROTEINNAME))) > 0){
          stop("Please check protein name.
               Data set does not have this protein. - ",
               toString(temp.name))
        }
      }
      
      ## check which.Protein is order number of Protein
      if (is.numeric(which.Protein)) {
        temp.name = levels(datafeature.ptm$PROTEINNAME)[which.Protein]
        
        ## message if name of Protein is wrong.
        if (length(levels(datafeature.ptm$PROTEINNAME)) < max(which.Protein)) {
          stop("Please check your number of proteins. There are ",
               length(levels(datafeature.ptm$PROTEINNAME)),
               " proteins in this dataset.")
        }
      }
      
      ## use only assigned proteins
      datafeature.ptm = datafeature.ptm[which(
        datafeature.ptm$PROTEINNAME %in% temp.name), ]
      datafeature.ptm$PROTEINNAME = factor(datafeature.ptm$PROTEINNAME)
      if (length(data.table.list) == 4){
        temp_proteins = unique(datafeature.ptm$GLOBALPROTEIN)
        
        plot_global = TRUE
        ## Check if there is a corresponding protein for the PTM
        if (!temp_proteins %in% datafeature.protein$PROTEINNAME){
          message(paste0("Global Protein data not available for ", 
                         as.character(temp_proteins), 
                         ", only PTM will be plotted.")
          )
          plot_global = FALSE
        }
        
        datafeature.protein = datafeature.protein[
          which(datafeature.protein$PROTEINNAME %in% temp_proteins), ]
        datafeature.protein$PROTEINNAME = factor(
          datafeature.protein$PROTEINNAME)
      }
    }
    
    ## Only plot proteins that occur in both datasets
    if (plot_global){
      global_proteins = unique(datafeature.protein[, PROTEINNAME])
      ptm_proteins = unique(datafeature.ptm[, GLOBALPROTEIN])
      plot_proteins = intersect(ptm_proteins, global_proteins)
      datafeature.ptm = datafeature.ptm[GLOBALPROTEIN %in% plot_proteins,]
      plot_proteins = unique(datafeature.ptm[, c('PROTEINNAME', 'GLOBALPROTEIN')]
      )
    } else {
      plot_proteins = unique(datafeature.ptm[, c('PROTEINNAME')])
    }
    
    for (i in seq_len(nrow(plot_proteins))) {
      
      ptemp.ptm = .qc.single.plot(datafeature.ptm, groupline.ptm, 
                                  groupline.all.ptm, as.character(
                                    plot_proteins[, PROTEINNAME][[i]]), 
                                  y.limup, y.limdown, x.axis.size, y.axis.size, 
                                  text.size, text.angle)
      if (plot_global){
        ptemp.protein = .qc.single.plot(datafeature.protein, groupline.protein, 
                                        groupline.all.protein, as.character(
                                          plot_proteins[, GLOBALPROTEIN][i]), 
                                        y.limup, y.limdown, x.axis.size, 
                                        y.axis.size, text.size, text.angle)
        grid.arrange(ptemp.ptm, ptemp.protein, ncol=1)
      } else {print(ptemp.ptm)}
      
      message(paste("Drew the Quality Contol plot(boxplot) for",
                    as.character(plot_proteins[, PROTEINNAME][i]), "(", i, 
                    " of ", nrow(plot_proteins), ")"))
      
    } # end-loop
  }
  
  if (address != FALSE) {
    dev.off()
  }
}

#' Format labelfree data for plotting
#' @noRd
.preplot.format.lf = function(datafeature, datarun, y.limup, type){
  
  datafeature = datafeature[with(datafeature, 
                                 order(CONDITION, SUBJECT, LABEL)),]
  
  ## Set factors
  datafeature$CONDITION = factor(datafeature$CONDITION)
  datarun$CONDITION = factor(datarun$CONDITION)
  datafeature$RUN = factor(datafeature$RUN)
  datarun$RUN = factor(datarun$RUN)
  
  tempGroupName = unique(datafeature[, c("CONDITION", "RUN")])
  
  groupAxis = as.numeric(xtabs(~CONDITION, tempGroupName))
  cumGroupAxis = cumsum(groupAxis)
  lineNameAxis = cumGroupAxis[-nlevels(datafeature$CONDITION)]
  
  groupName = data.table(RUN=c(0, lineNameAxis) + groupAxis / 2 + 0.5,
                         lineNameAxis = c(0, lineNameAxis),
                         ABUNDANCE=rep(y.limup-1, length(groupAxis)), 
                         Name=levels(datafeature$CONDITION))
  
  
  if (length(unique(datafeature$LABEL)) == 2) {
    datafeature$LABEL = factor(datafeature$LABEL, labels=c("Reference", 
                                                           "Endogenous"))	
  } else {
    if (unique(datafeature$LABEL) == "L") {
      datafeature$LABEL = factor(datafeature$LABEL, labels=c("Endogenous"))	
    }
    if (unique(datafeature$LABEL) == "H") {
      datafeature$LABEL = factor(datafeature$LABEL, labels=c("Reference"))
    }
  }
  
  ## need to fill in incomplete rows for Runlevel data
  if (type == 'PROFILEPLOT'){
    haverun = FALSE
    
    if (sum(is.element(colnames(datarun), "RUN")) != 0) {
      datamat = dcast(PROTEINNAME ~ RUN, data=datarun, value.var='ABUNDANCE', 
                      keep=TRUE) 
      
      datarun = melt(datamat, id.vars=c('PROTEINNAME'))
      colnames(datarun)[colnames(datarun) %in% c("variable", "value")
      ] = c('RUN', 'ABUNDANCE')
      
      haverun = TRUE
    }
  }
  
  return(list(datafeature, datarun, groupName))
}

#' Plot individual profile for label free
#' @noRd
.plot.profile.lf = function(data.list, protein, y.limup, y.limdown, 
                            x.axis.size, y.axis.size, text.size, text.angle, 
                            legend.size, dot.size.profile, ncol.guide){
  
  lineNameAxis = RUN = ABUNDANCE = Name = NULL
  
  datafeature = data.list[[1]]
  datarun = data.list[[2]]
  groupname = data.list[[3]]
  
  sub = datafeature[datafeature$PROTEINNAME == protein, ]
  sub$RUN = as.numeric(sub$RUN)
  sub$PEPTIDE = factor(as.character(sub$PEPTIDE))
  
  # if all measurements are NA,
  if (nrow(sub) == sum(is.na(sub$ABUNDANCE))|
      nrow(sub) == sum(!is.na(sub$ABUNDANCE) & sub$ABUNDANCE == 0)) {
    message(paste0("Can't the Profile plot for ", protein,
                   " because all measurements are NAs or zero."))
  }
  
  ## seq for peptide and charge
  ## for seting up color and linetype
  b = unique(sub[, c("PEPTIDE", "FEATURE")])
  ## add because if there are missing value, orders are different.
  b = b[with(b, order(PEPTIDE, FEATURE)), ]
  
  temp1 = xtabs(~data.frame(b)[, 1])
  
  ## unique charge id within peptide sequence, for line type
  ss = NULL
  ## unique peptide sequence id, for color
  s = NULL
  
  for (j in seq_len(length(temp1))) {
    temp3 = rep(j, temp1[j])
    s = c(s, temp3)
    temp2 = seq(1, temp1[j])
    ss = c(ss, temp2)	
  }
  
  ## for annotation of condition
  groupNametemp = data.table(groupname, 
                             "FEATURE"=unique(sub$FEATURE)[1], 
                             "PEPTIDE"=unique(sub$PEPTIDE)[1])
  
  ## 2019. 12. 17, MC : for profile plot, define color for dot
  cbp = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
          "#0072B2", "#D55E00", "#CC79A7")
  check.length = length(unique(s)) %/% length(cbp)
  if ( check.length > 0 ){
    cbp = rep(cbp, times=check.length + 1)
  }
  
  yaxis.name = 'Log-intensities'
  sub$group_aes = paste(sub$CONDITION, sub$FEATURE, sep = "_")
  
  ## 1st plot for Protein plot
  protein_temp = ggplot(data=sub) + facet_grid(~LABEL) +
    geom_point(aes_string(x='RUN', y='ABUNDANCE', 
                          color='FEATURE', group = 'group_aes'), #
               size=dot.size.profile, na.rm=TRUE) +
    geom_line(aes_string(x='RUN', y='ABUNDANCE', 
                         color='FEATURE', linetype='FEATURE', group = 'group_aes'), #
              size = 0.5, na.rm=TRUE) +
    scale_colour_manual(values=cbp[s]) +
    scale_linetype_manual(values = ss) +
    scale_shape_manual(values = c(16)) +
    labs(title = unique(sub$PROTEINNAME),
         x = 'MS runs') +
    scale_y_continuous(yaxis.name, limits = c(y.limdown, y.limup)) +
    scale_x_continuous('MS runs', breaks=groupNametemp$lineNameAxis) +
    geom_vline(data = groupNametemp[lineNameAxis != 0],
               aes(xintercept = lineNameAxis + 0.5),
               colour = "grey", linetype = "longdash") +
    geom_text(data = groupNametemp,
              aes_string(x = "RUN", y = "ABUNDANCE", label = "Name"),
              size = text.size,
              angle = text.angle, hjust = .9,
              color = "black") +
    theme_msstats(type = "PROFILEPLOT",x.axis.size, y.axis.size, legend.size,
                  text_angle = text.angle) +
    guides(color = guide_legend(title = paste("# peptide:", nlevels(
      sub$PEPTIDE)),
      title.theme = element_text(size = 13, angle = 0),
      keywidth = 0.4,
      keyheight = 0.1,
      default.unit = 'inch',
      ncol = ncol.guide),
      linetype = guide_legend(
        title = paste("# peptide:",
                      nlevels(sub$PEPTIDE)),
        title.theme = element_text(size = 13, angle = 0),
        keywidth = 0.4,
        keyheight = 0.1,
        default.unit = 'inch',
        ncol = ncol.guide))
  
  return(protein_temp)
}

#' Plot summary profile for label free
#' @noRd
.plot.profile.summary.lf = function(data.list, protein, y.limup, y.limdown, 
                                    x.axis.size, y.axis.size, text.size, text.angle, 
                                    legend.size, dot.size.profile, ncol.guide){
  
  lineNameAxis = RUN = ABUNDANCE = Name = NULL
  
  datafeature = data.list[[1]]
  datarun = data.list[[2]]
  groupname = data.list[[3]]
  
  sub = datafeature[datafeature$PROTEINNAME == protein, ]
  sub$RUN = as.numeric(sub$RUN)
  sub$PEPTIDE = factor(as.character(sub$PEPTIDE))
  
  # if all measurements are NA,
  if (nrow(sub) == sum(is.na(sub$ABUNDANCE))|
      nrow(sub) == sum(!is.na(sub$ABUNDANCE) & sub$ABUNDANCE == 0)) {
    message(paste0("Can't the Profile plot for ", protein,
                   " because all measurements are NAs or zero."))
  }
  
  ## seq for peptide and charge
  ## for seting up color and linetype
  b = unique(sub[, c("PEPTIDE", "FEATURE")])
  ## add because if there are missing value, orders are different.
  b = b[with(b, order(PEPTIDE, FEATURE)), ]
  
  temp1 = xtabs(~data.frame(b)[, 1])
  
  ## unique charge id within peptide sequence, for line type
  ss = NULL
  ## unique peptide sequence id, for color
  s = NULL
  
  for (j in seq_len(length(temp1))) {
    temp3 = rep(j, temp1[j])
    s = c(s, temp3)
    temp2 = seq(1, temp1[j])
    ss = c(ss, temp2)	
  }
  
  ## for annotation of condition
  groupNametemp = data.table(groupname, 
                             "FEATURE"=unique(sub$FEATURE)[1], 
                             "PEPTIDE"=unique(sub$PEPTIDE)[1])
  
  subrun = datarun[datarun$PROTEINNAME == protein, ]
  
  subrun$analysis = "Run summary"
  subrun$FEATURE = "Run summary"
  sub$analysis = "Processed feature-level data"
  final = rbindlist(list(sub,subrun), fill = TRUE)
  final$RUN = as.numeric(levels(final$RUN))[final$RUN]
  final = final[order(RUN)]
  #final$RUN = as.factor(final$RUN)
  
  yaxis.name = 'Log2-intensities'
  final$group_aes = paste(final$CONDITION, final$FEATURE, 
                          final$analysis, sep = "_")
  
  ## Draw summarized ptm plot
  ptempall = ggplot(data=final) +
    geom_point(aes_string(x='RUN', y='ABUNDANCE', 
                          color='analysis',
                          group = 'group_aes'), size = dot.size.profile, na.rm=TRUE) +
    geom_line(aes_string(x='RUN', y='ABUNDANCE', 
                         color='analysis', linetype='FEATURE', 
                         group = 'group_aes'), size = 0.5, na.rm=TRUE) + 
    scale_colour_manual(values = c("lightgray", "darkred")) +
    scale_shape_manual(values = c(16)) +
    scale_size_manual(values = c(1.7, 2), guide = "none") +
    scale_linetype_manual(values = c(rep(1, times = length(
      unique(final$FEATURE))-1), 2), guide = "none") +
    labs(title = unique(sub$PROTEINNAME),
         x = 'MS runs') +
    scale_y_continuous(yaxis.name, limits = c(y.limdown, y.limup)) +
    geom_vline(data = groupNametemp[lineNameAxis != 0],
               aes(xintercept = lineNameAxis + 0.5),
               colour = "grey", linetype = "longdash") +
    geom_text(data = groupNametemp,
              aes(x = RUN, y = ABUNDANCE, label = Name),
              size = text.size,
              angle = text.angle, hjust = .9,
              color = "black") +
    theme_msstats(type = "PROFILEPLOT", x.axis.size, y.axis.size, legend.size,
                  text_angle = text.angle) +
    guides(color = guide_legend(order = 1,
                                title = NULL,
                                label.theme = element_text(
                                  size = 10, angle = 0)))
  
  return(ptempall)
}

#' Wrapper function for label free profile plot 
#' @noRd
.profile.lf = function(data.table.list, type, ylimUp, ylimDown, 
                       x.axis.size, y.axis.size,text.size,text.angle, 
                       legend.size, dot.size.profile, ncol.guide, width, 
                       height,which.Protein,originalPlot,summaryPlot,
                       address) {
  
  PROTEINNAME = GLOBALPROTEIN = NULL
  
  if (length(data.table.list) == 4){
    datafeature.protein = data.table.list[[1]]
    datafeature.ptm = data.table.list[[2]]
    datarun.protein = data.table.list[[3]]
    datarun.ptm = data.table.list[[4]]
    
    y.limup = ceiling(max(datafeature.protein$ABUNDANCE,
                          datafeature.ptm$ABUNDANCE, na.rm = TRUE) + 5)
  } else {
    datafeature.ptm = data.table.list[[1]]
    datarun.ptm = data.table.list[[2]]
    
    y.limup = ceiling(max(datafeature.ptm$ABUNDANCE, na.rm = TRUE) + 5)
  }
  
  ## TODO: Add log
  # processout = rbind(processout,
  #                     c("ProfilePlot plotting started."))
  
  if (length(data.table.list) == 4){
    plot_global = TRUE
  } else {
    plot_global = FALSE
  }
  
  ## choose Proteins or not
  if (which.Protein[[1]] != "all") {
    ## check which.Protein is name of Protein
    if (is.character(which.Protein)) {
      temp.name = which.Protein
      
      ## message if name of Protein is wrong.
      if (length(setdiff(temp.name,unique(datafeature.ptm$PROTEINNAME))) > 0) {
        stop("Please check protein name. Dataset does not
             have this protein. - ",
             toString(temp.name))
      }
    }
    
    ## check which.Protein is order number of Protein
    else if (is.numeric(which.Protein)) {
      temp.name = levels(datafeature.ptm$PROTEINNAME)[which.Protein]
      
      ## message if name of Protein is wrong.
      if (length(levels(datafeature.ptm$PROTEINNAME)) < max(which.Protein)) {
        stop("Please check your number of proteins. There are ",
             length(levels(datafeature.ptm$PROTEINNAME))," proteins in this
             dataset.")
      }
    }
    
    ## use only assigned proteins
    datafeature.ptm = datafeature.ptm[which(datafeature.ptm$PROTEINNAME %in%
                                              temp.name), ]
    datarun.ptm = datarun.ptm[which(datarun.ptm$PROTEINNAME %in% temp.name), ]
    datarun.ptm$PROTEINNAME = factor(datarun.ptm$PROTEINNAME)
    datafeature.ptm$PROTEINNAME = factor(datafeature.ptm$PROTEINNAME)
    
    if (length(data.table.list) == 4){
      temp_proteins = as.character(unique(datafeature.ptm$GLOBALPROTEIN))
      
      plot_global = TRUE
      ## Check if there is a corresponding protein for the PTM
      if (!temp_proteins %in% datafeature.protein$PROTEINNAME){
        message(paste0("Global Protein data not available for some PTMs,", 
                       " only PTM will be plotted.")
        )
        plot_global = FALSE
      }
      
      datafeature.protein = datafeature.protein[which(
        datafeature.protein$PROTEINNAME %in% temp_proteins), ]
      datafeature.protein$PROTEINNAME = factor(datafeature.protein$PROTEINNAME)
      
      datarun.protein = datarun.protein[which(datarun.protein$PROTEINNAME %in%
                                                temp_proteins), ]
      datarun.protein$PROTEINNAME = factor(datarun.protein$PROTEINNAME)
    }
  }
  
  ## assign upper or lower limit
  if (is.numeric(ylimUp)) {
    y.limup = ylimUp
  }
  if (is.numeric(ylimDown)) {
    y.limdown = ylimDown
  } else {
    y.limdown = 0
  }
  
  ptm.list = .preplot.format.lf(datafeature.ptm, datarun.ptm, 
                                y.limup, type)
  datafeature.ptm = ptm.list[[1]]
  datarun.ptm = ptm.list[[2]]
  groupName.ptm = ptm.list[[3]]
  
  if (plot_global) {
    ## Apply pre-plot formatting
    protein.list = .preplot.format.lf(datafeature.protein, datarun.protein, 
                                      y.limup, type)
    datafeature.protein = protein.list[[1]]
    datarun.protein = protein.list[[2]]
    groupName.protein = protein.list[[3]]
    
    ## Only plot proteins that occur in both datasets
    global_proteins = unique(datafeature.protein[, PROTEINNAME])
    ptm_proteins = unique(datafeature.ptm[, GLOBALPROTEIN])
    plot_proteins = intersect(ptm_proteins, global_proteins)
    datafeature.ptm = datafeature.ptm[GLOBALPROTEIN %in% plot_proteins,]
    plot_proteins = unique(datafeature.ptm[, c('PROTEINNAME', 'GLOBALPROTEIN')]
    )
  } else {
    plot_proteins = unique(datafeature.ptm[, c('PROTEINNAME')])
  }
  
  if (originalPlot) {
    if (address != FALSE) {
      allfiles = list.files()
      
      num = 0
      filenaming = paste0(address, "ProfilePlot")
      finalfile = paste0(address, "ProfilePlot.pdf")
      
      while (is.element(finalfile, allfiles)) {
        num = num + 1
        finalfile = paste0(paste(filenaming, num, sep = "-"), ".pdf")
      }
      
      pdf(finalfile, width = width, height = height)
    }
    
    ## factoring for run, channel, condition should be done before loop
    
    for (i in seq_len(nrow(plot_proteins))) {
      
      ptm_temp = .plot.profile.lf(ptm.list,
                                  as.character(
                                    plot_proteins[, PROTEINNAME][i]), 
                                  y.limup, y.limdown, x.axis.size,
                                  y.axis.size, text.size, text.angle, 
                                  legend.size, dot.size.profile, ncol.guide)
      if (plot_global) {
        protein_temp = .plot.profile.lf(protein.list, 
                                        as.character(
                                          plot_proteins[, GLOBALPROTEIN][i]), 
                                        y.limup, y.limdown, x.axis.size,
                                        y.axis.size, text.size, text.angle, 
                                        legend.size, dot.size.profile, 
                                        ncol.guide)
        
        grid.arrange(ptm_temp, protein_temp, ncol=1)
      } else{print(ptm_temp)}
      
      message(paste0("Drew the Profile plot for ", 
                     as.character(plot_proteins[, PROTEINNAME][i]),
                     " (", i, " of ", nrow(plot_proteins), ")"))
    }
    # end-loop for each protein
    
    if (address != FALSE) {
      dev.off()
    }
    
  } # end original plot
  
  ############################################
  ## 2nd plot for original plot : summary
  ############################################
  
  if (summaryPlot) {
    if (address != FALSE) {
      allfiles = list.files()
      
      num = 0
      filenaming = paste0(address, "ProfilePlot_wSummarization")
      finalfile = paste0(address, "ProfilePlot_wSummarization.pdf")
      
      while (is.element(finalfile, allfiles)) {
        num = num + 1
        finalfile = paste0(paste(filenaming, num, sep = "-"), ".pdf")
      }
      
      pdf(finalfile, width = width, height = height)
    }
    
    for (i in seq_len(nrow(plot_proteins))) {
      
      ptm_temp = .plot.profile.summary.lf(ptm.list, as.character(
        plot_proteins[, PROTEINNAME][i]), y.limup, y.limdown, x.axis.size,
        y.axis.size, text.size, text.angle, legend.size,dot.size.profile, 
        ncol.guide)
      
      if (plot_global){
        protein_temp = .plot.profile.summary.lf(protein.list, as.character(
          plot_proteins[, GLOBALPROTEIN][i]), y.limup, y.limdown, x.axis.size,
          y.axis.size, text.size, text.angle, legend.size, dot.size.profile, 
          ncol.guide)
        
        grid.arrange(ptm_temp, protein_temp, ncol=1)
      } else {print(ptm_temp)}
      
      message(paste("Drew the Profile plot for ", 
                    as.character(plot_proteins[, PROTEINNAME][i]),
                    "(", i, " of ", nrow(plot_proteins), ")"))
    }
    if (address!=FALSE) {
      dev.off()
    }
  } # end summarization plot
  
}

#' Plot boxplot of all proteins LF
#' @noRd
.qc.all.plot.lf = function(datafeature, groupName, title, 
                           y.limdown, y.limup, x.axis.size, y.axis.size, 
                           text.size) {
  
  lineNameAxis = RUN = ABUNDANCE = Name = NULL
  
  ## for annotation of condition
  groupName.tmp = data.table(groupName, "PSM" = unique(datafeature$FEATURE)[1], 
                             "PEPTIDESEQUENCE" = unique(datafeature$PEPTIDE)[1]
  )
  
  ## y-axis labeling
  yaxis.name = 'Log-intensities'
  
  ptemp = ggplot(aes_string(x = 'RUN', y = 'ABUNDANCE'),
                 data = datafeature) +
    geom_boxplot(aes_string(fill = 'CONDITION'), outlier.shape = 1,
                 outlier.size = 1.5) +
    labs(title = title, x = 'MS runs') +
    scale_y_continuous(yaxis.name, y.limdown, y.limup) +
    geom_vline(data = groupName.tmp[lineNameAxis != 0],
               aes(xintercept = lineNameAxis + 0.5),
               colour = "grey", linetype = "longdash") +
    geom_text(data = groupName.tmp,
              aes(x = RUN, y = ABUNDANCE - 1, label = Name),
              size = text.size,
              angle = 0,color = "black") +
    theme_msstats(type = "PROFILEPLOT", x.axis.size, y.axis.size,
                  13, 
                  element_rect(fill = "gray95"),
                  element_text(colour = c("#00B0F6"), size = 14),
                  "none", text_angle = text.angle)
  # theme(
  #   panel.background = element_rect(fill = 'white', colour = "black"),
  #   legend.key = element_rect(fill = 'white', colour = 'white'),
  #   panel.grid.minor = element_blank(),
  #   strip.background = element_rect(fill = 'gray95'),
  #   axis.ticks.x = element_blank(),
  #   axis.text.x = element_blank(),
  #   axis.text.y = element_text(size = y.axis.size, colour = "black"),
  #   axis.ticks = element_line(colour = "black"),
  #   axis.title.x = element_text(size = x.axis.size + 5, vjust = -0.4),
  #   axis.title.y = element_text(size = y.axis.size + 5, vjust = 0.3),
  #   title = element_text(size = x.axis.size + 8, vjust = 1.5),
  #   legend.position = "none")
  
  return(ptemp)
}

#' Plot boxplot of single proteins LF
#' @noRd
.qc.single.plot.lf = function(datafeature, groupname, protein, y.limdown, y.limup,
                              x.axis.size, y.axis.size,text.size){
  
  PROTEINNAME = ABUNDANCE = lineNameAxis = RUN = Name = NULL
  
  sub = datafeature[PROTEINNAME == protein, ]
  sub = sub[!is.na(sub$ABUNDANCE), ]
  
  ## if all protein measurements are NA,
  if (nrow(sub) == sum(sub[!is.na(sub$ABUNDANCE), ABUNDANCE])) {
    message(paste("Can't the Quality Control plot for ", unique(
      sub$PROTEINNAME), " because all measurements are NAs."))
  }
  
  ## for annotation of condition
  groupname.tmp = data.table(groupname, "FEATURE" = unique(sub$FEATURE)[1], 
                             "PEPTIDESEQUENCE" = unique(sub$PEPTIDE)[1]
  )
  
  ## 1st plot for original plot
  ## for boxplot, x-axis, xorder should be factor
  yaxis.name = 'Log-intensities'
  
  ptemp = ggplot(aes_string(x = 'RUN', y = 'ABUNDANCE'),
                 data = sub) +
    geom_boxplot(aes_string(fill = 'CONDITION'), outlier.shape = 1,
                 outlier.size = 1.5) +
    labs(title = protein, x = 'MS runs') +
    scale_y_continuous(yaxis.name, limits = c(y.limdown, y.limup)) +
    geom_vline(data = groupname.tmp[lineNameAxis != 0],
               aes(xintercept = lineNameAxis + .5),
               colour = "grey", linetype = "longdash") +
    geom_text(data = groupname.tmp,
              aes(x = RUN, y = ABUNDANCE, label = Name),
              size = text.size,
              color = "black") +
    theme_msstats(type = "PROFILEPLOT", x.axis.size, y.axis.size,
                  13, 
                  element_rect(fill = "gray95"),
                  element_text(colour = c("#00B0F6"), size = 14),
                  "none", text_angle = text.angle)
  # theme(
  #   panel.background = element_rect(fill = 'white', colour = "black"),
  #   legend.key = element_rect(fill = 'white', colour = 'white'),
  #   panel.grid.minor = element_blank(),
  #   strip.background = element_rect(fill = 'gray95'),
  #   axis.ticks.x = element_blank(),
  #   axis.text.x = element_blank(),
  #   axis.text.y = element_text(size = y.axis.size, colour = "black"),
  #   axis.ticks = element_line(colour = "black"),
  #   axis.title.x = element_text(size = x.axis.size + 5, vjust = -0.4),
  #   axis.title.y = element_text(size = y.axis.size + 5, vjust = 0.3),
  #   title = element_text(size = x.axis.size + 8, vjust = 1.5),
  #   legend.position = "none")
  return(ptemp)
}

#' Wrapper function for LF QC Plot
#' @noRd
.qc.lf = function(data.table.list, type, ylimUp, ylimDown, width, height,
                  x.axis.size, y.axis.size,text.size, which.Protein,
                  address, ptm_title, protein_title) {
  
  PROTEINNAME = GLOBALPROTEIN = NULL
  
  if (length(data.table.list) == 4){
    datafeature.protein = data.table.list[[1]]
    datafeature.ptm = data.table.list[[2]]
    datarun.protein = data.table.list[[3]]
    datarun.ptm = data.table.list[[4]]
    
    y.limup = ceiling(max(datafeature.protein$ABUNDANCE,
                          datafeature.ptm$ABUNDANCE, na.rm = TRUE) + 3)
  } else {
    datafeature.ptm = data.table.list[[1]]
    datarun.ptm = data.table.list[[2]]
    
    y.limup = ceiling(max(datafeature.ptm$ABUNDANCE, na.rm = TRUE) + 3)
  }
  
  ## save the plots as pdf or not
  ## If there are the file with the same name
  ## add next numbering at the end of file name
  if (address != FALSE) {
    allfiles = list.files()
    
    num = 0
    filenaming = paste0(address,"QCPlot")
    finalfile = paste0(address,"QCPlot.pdf")
    
    while (is.element(finalfile, allfiles)) {
      num = num + 1
      finalfile = paste0(paste(filenaming, num, sep = "-"), ".pdf")
    }
    
    pdf(finalfile, width = width, height = height)
  }
  
  ## assign upper or lower limit
  if (is.numeric(ylimUp)) {
    y.limup = ylimUp
  }
  
  y.limdown = 0
  if (is.numeric(ylimDown)) {
    y.limdown = ylimDown
  }
  
  ## Apply pre-plot formatting
  ptm.list = .preplot.format.lf(datafeature.ptm, datarun.ptm, y.limup, type)
  datafeature.ptm = ptm.list[[1]]
  datarun.ptm = ptm.list[[2]]
  groupName.ptm = ptm.list[[3]]
  
  if (length(data.table.list) == 4){
    protein.list = .preplot.format.lf(datafeature.protein, datarun.protein,
                                      y.limup, type)
    datafeature.protein = protein.list[[1]]
    datarun.protein = protein.list[[2]]
    groupName.protein = protein.list[[3]]
  }
  
  ## all protein
  if (which.Protein[[1]] == 'all' | which.Protein[[1]] == 'allonly') {
    
    ## Plot all QC
    ptemp.ptm = .qc.all.plot.lf(datafeature.ptm, groupName.ptm, ptm_title,
                                y.limdown, y.limup, x.axis.size, y.axis.size, 
                                text.size)
    if (length(data.table.list) == 4){
      ptemp.protein = .qc.all.plot.lf(datafeature.protein, groupName.protein,
                                      protein_title, y.limdown, y.limup, 
                                      x.axis.size, y.axis.size, text.size)
      grid.arrange(ptemp.ptm, ptemp.protein, ncol=1)
    } else{print(ptemp.ptm)}
    
    message("Drew the Quality Contol plot(boxplot) for all ptms/proteins.")
  }
  
  ## each protein
  ## choose Proteins or not
  if (length(data.table.list) == 4){
    plot_global = TRUE
  } else {
    plot_global = FALSE
  }
  
  if (which.Protein[[1]] != 'allonly') {
    if (which.Protein[[1]] != "all") {
      ## check which.Protein is name of Protein
      if (is.character(which.Protein)) {
        temp.name = which.Protein
        
        ## message if name of Protein is wrong.
        if (length(setdiff(temp.name,unique(datafeature.ptm$PROTEINNAME))) > 0){
          stop("Please check protein name.
               Data set does not have this protein. - ",
               toString(temp.name))
        }
      }
      
      ## check which.Protein is order number of Protein
      if (is.numeric(which.Protein)) {
        temp.name = levels(datafeature.ptm$PROTEINNAME)[which.Protein]
        
        ## message if name of Protein is wrong.
        if (length(levels(datafeature.ptm$PROTEINNAME)) < max(which.Protein)) {
          stop("Please check your number of proteins. There are ",
               length(levels(datafeature.ptm$PROTEINNAME)),
               " proteins in this dataset.")
        }
      }
      
      ## use only assigned proteins
      datafeature.ptm = datafeature.ptm[which(
        datafeature.ptm$PROTEINNAME %in% temp.name), ]
      datafeature.ptm$PROTEINNAME = factor(datafeature.ptm$PROTEINNAME)
      
      if (length(data.table.list) == 4){
        temp_proteins = as.character(unique(datafeature.ptm$GLOBALPROTEIN))
        
        plot_global = TRUE
        ## Check if there is a corresponding protein for the PTM
        if (!temp_proteins %in% datafeature.protein$PROTEINNAME){
          message(paste0("Global Protein data not available for ", 
                         as.character(temp_proteins), 
                         ", only PTM will be plotted.")
          )
          plot_global = FALSE
        }
        
        datafeature.protein = datafeature.protein[
          which(datafeature.protein$PROTEINNAME %in% temp_proteins), ]
        datafeature.protein$PROTEINNAME = factor(datafeature.protein$PROTEINNAME)
      }
    }
    
    ## Only plot proteins that occur in both datasets
    if (plot_global){
      global_proteins = unique(datafeature.protein[, PROTEINNAME])
      ptm_proteins = unique(datafeature.ptm[, GLOBALPROTEIN])
      plot_proteins = intersect(ptm_proteins, global_proteins)
      datafeature.ptm = datafeature.ptm[GLOBALPROTEIN %in% plot_proteins,]
      plot_proteins = unique(datafeature.ptm[, c('PROTEINNAME', 
                                                 'GLOBALPROTEIN')])
    } else {
      plot_proteins = unique(datafeature.ptm[, c('PROTEINNAME')])
    }
    
    for (i in seq_len(nrow(plot_proteins))) {
      
      ptemp.ptm = .qc.single.plot.lf(datafeature.ptm, groupName.ptm, 
                                     as.character(plot_proteins[, PROTEINNAME][i]
                                     ),
                                     y.limdown, y.limup, x.axis.size, y.axis.size, 
                                     text.size)
      if (plot_global){
        ptemp.protein = .qc.single.plot.lf(datafeature.protein, groupName.protein, 
                                           as.character(
                                             plot_proteins[, GLOBALPROTEIN][i]),
                                           y.limdown, y.limup, x.axis.size, 
                                           y.axis.size, text.size)
        grid.arrange(ptemp.ptm, ptemp.protein, ncol=1)
      } else {print(ptemp.ptm)}
      message(paste0("Drew the Quality Contol plot(boxplot) for ",
                     as.character(plot_proteins[, PROTEINNAME][i]), " (", i, 
                     " of ", nrow(plot_proteins), ")"))
    } # end-loop
  }
  
  if (address != FALSE) {
    dev.off()
  }
}