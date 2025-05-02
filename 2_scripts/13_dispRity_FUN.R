library(dispRity)


# custom bins
create_custom_dispRity_bins_FUN <- 
  function(data, subset_by, ncol_PCA){
    rownames_DATASETS <- list()
    for(i in levels({{data}}[,{{subset_by}}])){
      rownames_DATASETS[[i]] <- which({{data}}[,{{subset_by}}] == i)
    }
    bins <- 
      dispRity::custom.subsets({{data}}[,c(1:{{ncol_PCA}})], 
                               group = rownames_DATASETS)
    return(bins)
  }


# set.seed(1)
# boot <- dispRity::boot.matrix(subsets, 
#                               bootstraps = 1000)
# set.seed(1)
# disparity_object <- dispRity::dispRity(boot, 
#                                        metric = c(sum, 
#                                                   variances))

create_custom_dispRity_plot_FUN <- 
  function(disparity_object, subset_by, color_scheme, data){
    disp_names <- names({{disparity_object}}$disparity)
    disparity_df_list <- list()
    for(i in disp_names){
      disparity_df_list[[i]] <- data.frame(object = factor(i,
                                                           levels = levels({{data}}[,{{subset_by}}])),
                                           object_n = factor(paste0(i, 
                                                           "\n(n=",nrow({{disparity_object}}$subsets[[i]]$elements),")"),
                                                           levels = paste0(levels({{data}}[,{{subset_by}}]),
                                                                           "\n(n=",nrow({{disparity_object}}$subsets[[i]]$elements),")")),
                                           disparity = as.vector({{disparity_object}}$disparity[[i]][[2]]),
                                           nelements = nrow({{disparity_object}}$subsets[[i]]$elements)
                                           )
    }
    
    ggplot2::ggplot(data = na.omit(do.call(rbind.data.frame, 
                                           disparity_df_list)),
                    ggplot2::aes(x = object_n,
                                 y = disparity,
                                 fill = object,
                                 group = object)) + 
      ggdist::stat_halfeye(adjust = .5,
                           position = ggplot2::position_dodge(0.9)) +
      ggplot2::geom_boxplot(ggplot2::aes(group=object),
                            width = 0.1, fill = "white",
                            position = ggplot2::position_dodge(0.9)) +
      ggplot2::theme_bw() +
      ggplot2::ggtitle(NULL) +
      ggplot2::xlab("") + 
      ggplot2::ylab("Disparity") +
      ggplot2::theme(legend.position="none") +
      ggplot2::scale_fill_manual(values = {{color_scheme}})
    
  }

all_in_one_disparity_FUN <- 
  function(data, ncol_PCA, subset_by, color_scheme, bootstrap_iter = 1000){
    
    set.seed(1)
    disparity_object <- dispRity::dispRity(dispRity::boot.matrix(create_custom_dispRity_bins_FUN(data, subset_by, ncol_PCA),
                                                                 bootstraps = bootstrap_iter),
                                           metric = c(sum,
                                                      variances))
    
    create_custom_dispRity_plot_FUN(disparity_object, subset_by, color_scheme, data)
  }


# summary(disparity_object)
# # Wilcox.test
# dispRity::test.dispRity(disparity_object, 
#                         test = wilcox.test, 
#                         comparisons = "pairwise",
#                         correction = "bonferroni")
# # PERMANOVA
# dispRity::test.dispRity(disparity_object, 
#                         test = adonis.dispRity, 
#                         comparisons = "pairwise",
#                         correction = "bonferroni")