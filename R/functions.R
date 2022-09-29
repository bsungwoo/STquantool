#' Internal Modal function to flip the image
#' @description Flip the image
#' @keywords internal
#' @export
flipModal <- function(text="vertically", input_name="ok_vertical") {
  shiny::modalDialog(
    shiny::div(shiny::tags$b(paste0("Will you ",text," flip the image?"), style = "color: black;")),
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(input_name, "OK")
    )
  )
}

#' Horizontally flip the image
#' @description Horizontally flip the image
#' @param file_dir_vector 'spatial' directory for the 10X Visium-formatted file
#' @importFrom dplyr "%>%"
#' @export
horizontal_flip <- function(file_dir_vector){

  for (file_dir in file_dir_vector){
    orig_file_name <- c('tissue_hires_image.png','tissue_lowres_image.png',
                        'tissue_positions_list.csv','scalefactors_json.json')

    for (x in orig_file_name[1:2]){
      img_orig <- magick::image_read(paste0(file_dir,'/spatial/',x))
      file.rename(paste0(file_dir,'/spatial/',x),
                  paste0(file_dir,'/spatial/',
                         strsplit(x,'[.]')[[1]][1],'_orig.png'))
      img_mod <- magick::image_flop(img_orig)
      if (x == orig_file_name[1]){width_hires <- magick::image_info(img_mod)[[2]]}
      magick::image_write(img_mod, paste0(file_dir,'/spatial/',x))
    }

    coord <- utils::read.csv(paste0(file_dir,'/spatial/',orig_file_name[3]),
                             header=FALSE)
    file.rename(paste0(file_dir,'/spatial/',orig_file_name[3]),
                paste0(file_dir,'/spatial/',
                       strsplit(orig_file_name[3],'[.]')[[1]][1],'_orig.csv'))
    scale_factor <- rjson::fromJSON(file = paste0(file_dir,'/spatial/',orig_file_name[4]))
    coord_mod <- coord %>% dplyr::mutate(V4=127-V4, V6=width_hires/(scale_factor[[2]])-V6)
    utils::write.table(coord_mod, paste0(file_dir,'/spatial/',orig_file_name[3]),
                       row.names = FALSE, col.names = FALSE, sep = ',')
  }
}

#' Vertically flip the image
#' @description Vertically flip the image
#' @param file_dir_vector 'spatial' directory for the 10X Visium-formatted file
#' @importFrom dplyr "%>%"
#' @export
vertical_flip <- function(file_dir_vector){

  for (file_dir in file_dir_vector){
    orig_file_name <- c('tissue_hires_image.png','tissue_lowres_image.png',
                        'tissue_positions_list.csv','scalefactors_json.json')

    for (x in orig_file_name[1:2]){
      img_orig <- magick::image_read(paste0(file_dir,'/spatial/',x))
      file.rename(paste0(file_dir,'/spatial/',x),
                  paste0(file_dir,'/spatial/',
                         strsplit(x,'[.]')[[1]][1],'_orig.png'))
      img_mod <- magick::image_flip(img_orig)
      if (x == orig_file_name[1]){height_hires <- magick::image_info(img_mod)[[3]]}
      magick::image_write(img_mod, paste0(file_dir,'/spatial/',x))
    }

    coord <- utils::read.csv(paste0(file_dir,'/spatial/',orig_file_name[3]),
                             header=FALSE)
    file.rename(paste0(file_dir,'/spatial/',orig_file_name[3]),
                paste0(file_dir,'/spatial/',
                       strsplit(orig_file_name[3],'[.]')[[1]][1],'_orig.csv'))
    scale_factor <- rjson::fromJSON(file = paste0(file_dir,'/spatial/',orig_file_name[4]))
    coord_mod <- coord %>% dplyr::mutate(V3=77-V3, V5=height_hires/(scale_factor[[2]])-V5)
    utils::write.table(coord_mod, paste0(file_dir,'/spatial/',orig_file_name[3]),
                       row.names = FALSE, col.names = FALSE, sep = ',')
  }
}

#' Modal to save the deg output plot files
#' @description Save the deg output plot files
#' @keywords internal
#' @export
save_plot_wh_Modal <- function(slider_input_name="save_dpi", width_name = "deg_width",
                               height_name = "deg_height", width_value=13, height_value=15,
                               action_button_name = "save_start") {
  shiny::modalDialog(
    shiny::div(shiny::tags$b("Options for saving a plot"), style = "color: blue;"),
    shiny::wellPanel(
      shiny::sliderInput(inputId = slider_input_name,
                         label = "dpi to save image",
                         value = 100, min = 0, max=500, step=10),
      shiny::sliderInput(inputId = width_name,
                         label = "Width in cm",
                         value = width_value, min=0, max=50, step=1),
      shiny::sliderInput(inputId = height_name,
                         label = "Height in cm",
                         value = height_value, min=0, max=50, step=1)
    ),
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(action_button_name, "Save")
    )
  )
}

#' Modal to save the rds files
#' @description Modal to save the rds files
#' @keywords internal
#' @export
save_files_Modal <- function(text="single-cell",
                             text_for_purpose="Will you save the ",text_for_add=" data?",
                             text_input_name="sc_save_name",
                             text_input_explain = "Name of the file",
                             file_save_name="sc_data",
                             add_text_input=FALSE,
                             add_text_input_name="sc_save_name_add",
                             add_text_input_explain='Name of the file',
                             add_file_save_name="sc_data", action_button_name = "ok_sc") {
  shiny::modalDialog(
    shiny::div(shiny::tags$b(paste0(text_for_purpose,text,text_for_add), style = "color: black;")),
    shiny::wellPanel(
      shiny::textInput(inputId = text_input_name,
                       label = text_input_explain,
                       value = file_save_name),
      if (add_text_input){
        shiny::textInput(inputId = add_text_input_name,
                         label = add_text_input_explain,
                         value = add_file_save_name)
      }
    ),
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(action_button_name, "OK")
    )
  )
}

#' Modal to load csv or txt files
#' @description Modal to load csv or txt files
#' @keywords internal
#' @export
load_files_Modal <- function(input, output, session, text="csv or txt",
                             text_for_purpose="Will you load the ",text_for_add=" file?",
                             text_input_name="file_load_name",
                             text_input_explain = "Name of the folder",
                             file_save_name = "Data_1",
                             radio_input_name = "radio_delim_type",
                             radio_input_explain = "Delimiter",
                             numeric_input_name1 = "col_num_to_rowname",
                             numeric_input_explain1 = "Select column number to transfer to rownames",
                             numeric_input_name2 = "row_num_to_skip",
                             numeric_input_explain2 = "Select row numbers to skip",
                             check_input_name1="file_load_header_check",
                             check_input_name2="file_load_transpose_check",
                             check_input_name3="file_load_shift_col_check",
                             files_button_name="choose_file_to_convert",
                             files_button_explain="Choose",
                             files_button_text="Select files to convert to 10X format (*.txt/*.csv)",
                             action_button_name1 = "check_load",
                             action_button_name2 = "ok_load",
                             result_table = "table_check") {
  shiny::modalDialog(
    shiny::div(shiny::tags$b(paste0(text_for_purpose,text,text_for_add), style = "color: black;")),
    shiny::wellPanel(
      shiny::textInput(inputId = text_input_name,
                       label = text_input_explain,
                       value = file_save_name),
      shiny::radioButtons(radio_input_name, radio_input_explain,
                          choices = list("Comma","Tab","Space"), selected = "Comma"),
      shiny::numericInput(numeric_input_name1,
                          label = numeric_input_explain1,
                          value = 1, min = 1, max=5),
      shiny::numericInput(numeric_input_name2,
                          label = numeric_input_explain2,
                          value = 0, min = 0, max=5),
      shiny::checkboxInput(check_input_name1, "Header", TRUE),
      shiny::checkboxInput(check_input_name2, "Transpose", FALSE),
      shiny::checkboxInput(check_input_name3, "Shift one column", FALSE),
      shiny::modalButton("Cancel"),
      shinyFiles::shinyFilesButton(files_button_name, files_button_explain,
                                   files_button_text, multiple=FALSE),
      shiny::actionButton(action_button_name1, "Check"),
      shiny::actionButton(action_button_name2, "Convert")
    ),
    shiny::wellPanel(
      shiny::h5("First 20 rows and 3 columns are shown"),
      DT::dataTableOutput(session$ns(result_table)) %>% shinycssloaders::withSpinner(color="#0000FF")
    ),
    footer = shiny::tagList(
    ),
    size = "l"
  )
}


#' Modal to save gene list
#' @description Modal to load csv or txt files
#' @keywords internal
#' @export
save_gene_list_Modal <- function(text_for_purpose='Will you save the given gene list?',
                                 text_input_name="gene_save_name",
                                 text_input_explain = "Name of the gene list",
                                 file_save_name="Gene list",
                                 action_button_name = "ok_gene") {
  shiny::modalDialog(
    shiny::div(shiny::tags$b(paste0(text_for_purpose), style = "color: black;")),
    shiny::wellPanel(
      shiny::textInput(inputId = text_input_name,
                       label = text_input_explain,
                       value = file_save_name)
    ),
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(action_button_name, "OK")
    )
  )
}


#' Simple preprocess data
#' @description Simple preprocess data
#' @param data_dir vector for the directories to load the single-cell data
#' @param grp group name to assigned to each single-cell data
#' @param data_type type of data to integrate (single-cell or spatial)
#' @param filter_nfeature_RNA each element is the lower threshold for total number of detected genes in each single-cell data provided
#' @param filter_percent_mt each element is the upper threshold for fraction of mitochondrial genes in each single-cell data provided
#' @param reference_index vector for index number of the reference single-cell dataset in grp vector (default: NULL)
#' \itemize{
#' \item if input is c(1,5) it means that we will use first to fifth element of group as a reference
#' \item if input is NULL it means that we will use all pairwise anchors for integration
#' }
#' @param n_var_features number of variable genes to be included in each single-cell dataset (default: 2000)
#' @param cluster_dim PC dimension to be used for clustering of the spots (default: 30)
#' @param cluster_resolution resolution during the clustering process (default: 0.5)
#' @param maximum_size maximum size allowed for the analysis (default: 6GB)
#' @return Seurat object with a preprocessed dataset
#' @export
preprocess_data <- function(data_dir, grp, data_type="Single-cell",
                            filter_nfeature_RNA=NULL,filter_percent_mt=NULL,
                            reference_index=NULL,
                            n_var_features=2000,
                            cluster_dim=30, cluster_resolution=0.5,
                            maximum_size=6000 * 1024^2){
  # Create a Progress object
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())

  progress$set(message = "Simple processing", value = 0)

  # Set maximum size to 6 GB
  options(future.globals.maxSize=maximum_size)

  # Preprocess for each of the datasets
  # Increment the progress bar, and update the detail text.
  progress$inc(1/8, detail="Loading data")


  if (data_type=="Single-cell") {
    if ("filtered_feature_bc_matrix" %in% list.files(data_dir)){
      counts <- Seurat::Read10X(data.dir = file.path(data_dir,'filtered_feature_bc_matrix'))
    } else if ("filtered_feature_bc_matrix.h5" %in% list.files(data_dir)){
      counts <- Seurat::Read10X_h5(file.path(data_dir, 'filtered_feature_bc_matrix.h5'))
    } else if ("sc_sparse_matrix.rds" %in% list.files(data_dir)) {
      counts <- readRDS(file.path(data_dir, 'sc_sparse_matrix.rds'))
    } else {
      stop("Single-cell data cannot be read")
    }
    brain_data <- Seurat::CreateSeuratObject(counts, project = grp)
    brain_data$orig.ident <- grp
    brain_data[['percent.mito']] <- Seurat::PercentageFeatureSet(brain_data,
                                                                 pattern = "^mt-")
    if (!is.null(filter_nfeature_RNA)){
      brain_data <- eval(parse(text=paste0('subset(brain_data,subset=nFeature_RNA>',
                                           filter_nfeature_RNA,')')))
    }
    if (!is.null(filter_percent_mt)){
      brain_data <- eval(parse(text=paste0('subset(brain_data,subset=percent.mito<',
                                           filter_percent_mt,')')))
    }
  } else if (data_type=='Spatial'){
    brain_data <- Seurat::Load10X_Spatial(data_dir, slice=grp)
    brain_data$orig.ident <- grp
    brain_data[['percent.mito']] <- Seurat::PercentageFeatureSet(brain_data,pattern = "^mt-")
  }

  progress$inc(1/8, detail="Normalizing data")
  brain_data <- Seurat::NormalizeData(brain_data,
                                      normalization.method = "LogNormalize",
                                      scale.factor = 1e4)
  brain_data <- Seurat::FindVariableFeatures(brain_data,
                                             selection.method = "vst",
                                             nfeatures=n_var_features)

  progress$inc(1/8, detail="Scaling data")
  if (data_type=="Single-cell") {
    brain_data <- Seurat::ScaleData(brain_data, vars.to.regress = c("nCount_RNA","percent.mito"))
  } else if (data_type=="Spatial"){
    brain_data <- Seurat::ScaleData(brain_data, vars.to.regress = c("nCount_Spatial"))
  }

  progress$inc(1/8, detail="Running PCA")
  brain_data <- Seurat::RunPCA(brain_data, verbose = FALSE)


  progress$inc(1/8, detail="Finding neighbors")
  brain_data <- Seurat::FindNeighbors(brain_data, dims = 1:cluster_dim)

  # Change resolution -> different spot cluster numbers
  progress$inc(1/8, detail="Finding clusters")
  brain_data <- Seurat::FindClusters(brain_data, resolution=cluster_resolution)

  progress$inc(1/8, detail="Running UMAP")
  brain_data <- Seurat::RunUMAP(brain_data, dims = 1:cluster_dim)

  progress$inc(1/8, detail="Finishing")
  brain_data$orig.ident <- factor(brain_data$orig.ident, levels=grp)

  return(brain_data)

}


#' Integrate datasets using reciprocal PCA in Seurat
#' @description Integrating single-cell or spatial datasets (Reciprocal PCA and Anchor finding)
#' @param data_type type of data to integrate (single-cell or spatial)
#' @param data_dir vector for the directories to load the single-cell data
#' @param grp group name to assigned to each single-cell data
#' @param filter_nfeature_RNA each element is the lower threshold for total number of detected genes in each single-cell data provided
#' @param filter_percent_mt each element is the upper threshold for fraction of mitochondrial genes in each single-cell data provided
#' @param reference_index vector for index number of the reference single-cell dataset in grp vector (default: NULL)
#' \itemize{
#' \item if input is c(1,5) it means that we will use first to fifth element of group as a reference
#' \item if input is NULL it means that we will use all pairwise anchors for integration
#' }
#' @param n_var_features number of variable genes to be included in each single-cell dataset (default: 2000)
#' @param n_integ_features number of genes to be utilized for integration (default: 2000)
#' @param integ_dim PC dimensions to be used for integration with reciprocal PCA (default: 50)
#' @param cluster_dim PC dimension to be used for clustering of the spots (default: 30)
#' @param cluster_resolution resolution during the clustering process (default: 0.5)
#' @param maximum_size maximum size allowed for the analysis (default: 6GB)
#' @return Seurat object with multiple integrated datasets
#' @export
preprocess_data_integ_rpca <- function(data_dir, grp, data_type='Single-cell',
                                       filter_nfeature_RNA=NULL,filter_percent_mt=NULL,
                                       reference_index=NULL,
                                       n_var_features=2000,n_integ_features=2000,
                                       integ_dim=50,
                                       cluster_dim=30, cluster_resolution=0.5,
                                       maximum_size=6000 * 1024^2){
  # Create a Progress object
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())

  progress$set(message = "Integration", value = 0)

  # Set maximum size to 6 GB
  options(future.globals.maxSize=maximum_size)

  object_list <- list()
  # Preprocess for each of the spatial datasets
  for (i in 1:length(data_dir)){
    # Increment the progress bar, and update the detail text.
    progress$inc(1/(2*length(data_dir)+8), detail=paste("Normalizing data", i))

    if (data_type=="Single-cell") {
      if ("filtered_feature_bc_matrix" %in% list.files(data_dir[i])){
        counts <- Seurat::Read10X(data.dir = file.path(data_dir[i],'filtered_feature_bc_matrix'))
      } else if ("filtered_feature_bc_matrix.h5" %in% list.files(data_dir[i])){
        counts <- Seurat::Read10X_h5(file.path(data_dir[i], 'filtered_feature_bc_matrix.h5'))
      } else if ("sc_sparse_matrix.rds" %in% list.files(data_dir[i])) {
        counts <- readRDS(file.path(data_dir[i], 'sc_sparse_matrix.rds'))
      } else {
        stop("Single-cell data cannot be read")
      }
      object_list[[i]] <- Seurat::CreateSeuratObject(counts, project = grp[i])
      object_list[[i]]$orig.ident <- grp[i]
      object_list[[i]][['percent.mito']] <- Seurat::PercentageFeatureSet(object_list[[i]],
                                                                         pattern = "^mt-")
      if (!is.null(filter_nfeature_RNA[i])){
        object_list[[i]] <- eval(parse(text=paste0('subset(object_list[[i]],subset=nFeature_RNA>',
                                                   filter_nfeature_RNA[i],')')))
      }
      if (!is.null(filter_percent_mt[i])){
        object_list[[i]] <- eval(parse(text=paste0('subset(object_list[[i]],subset=percent.mito<',
                                                   filter_percent_mt[i],')')))
      }
    } else if (data_type=="Spatial"){
      object_list[[i]] <- Seurat::Load10X_Spatial(data_dir[i], slice=grp[i])
      object_list[[i]]$orig.ident <- grp[i]
      object_list[[i]][['percent.mito']] <- Seurat::PercentageFeatureSet(object_list[[i]],
                                                                         pattern = "^mt-")
    }

    object_list[[i]] <- Seurat::NormalizeData(object_list[[i]],
                                              normalization.method = "LogNormalize",
                                              scale.factor = 1e4)
    object_list[[i]] <- Seurat::FindVariableFeatures(object_list[[i]],
                                                     selection.method = "vst",
                                                     nfeatures=n_var_features)
    Sys.sleep(0.1)
  }

  brain.features <- Seurat::SelectIntegrationFeatures(object_list,
                                                      nfeatures=n_integ_features)

  for (i in 1:length(data_dir)){
    progress$inc(1/(2*length(data_dir)+8), detail=paste("Scaling & PCA for data", i))
    if (data_type=="Single-cell") {
      object_list[[i]] <- Seurat::ScaleData(object_list[[i]], features = brain.features,
                                            vars.to.regress = c("nCount_RNA","percent.mito"))
    } else if (data_type=="Spatial"){
      object_list[[i]] <- Seurat::ScaleData(object_list[[i]], features = brain.features,
                                            vars.to.regress = c("nCount_Spatial"))
    }
    object_list[[i]] <- Seurat::RunPCA(object_list[[i]], features = brain.features, verbose = FALSE)
    Sys.sleep(0.1)
  }

  progress$inc(1/(2*length(data_dir)+8), detail="Find integration anchors")

  anchors <- Seurat::FindIntegrationAnchors(object_list, reference = reference_index,
                                            anchor.features = brain.features,
                                            reduction = 'rpca', dims = 1:integ_dim)

  progress$inc(1/(2*length(data_dir)+8), detail="Integating data")
  brain.merge <- Seurat::IntegrateData(anchorset=anchors, dims=1:integ_dim)

  progress$inc(1/(2*length(data_dir)+8), detail="Scaling integrated data")
  brain.merge <- Seurat::ScaleData(brain.merge, verbose=FALSE)

  progress$inc(1/(2*length(data_dir)+8), detail="Running PCA")
  brain.merge <- Seurat::RunPCA(brain.merge, verbose=FALSE)

  progress$inc(1/(2*length(data_dir)+8), detail="Finding neighbors")
  brain.merge <- Seurat::FindNeighbors(brain.merge, dims = 1:cluster_dim)

  # Change resolution -> different spot cluster numbers
  progress$inc(1/(2*length(data_dir)+8), detail="Finding clusters")
  brain.merge <- Seurat::FindClusters(brain.merge, resolution=cluster_resolution)

  progress$inc(1/(2*length(data_dir)+8), detail="Running UMAP")
  brain.merge <- Seurat::RunUMAP(brain.merge, dims = 1:cluster_dim)

  progress$inc(1/(2*length(data_dir)+8), detail="Finishing")
  if (data_type=="Single-cell") {
    Seurat::DefaultAssay(object = brain.merge) <- "RNA"
  } else if (data_type=="Spatial"){
    Seurat::DefaultAssay(object = brain.merge) <- "Spatial"
  }
  brain.merge$orig.ident <- factor(brain.merge$orig.ident, levels=grp)

  return(brain.merge)
}

#' Recluster the dataset
#' @param data single-cell or spatial data to recluster
#' @param split.by name of the clusters to split the single-cell or spatial data
#' @param data_type type of data to integrate (single-cell or spatial)
#' @param n_var_features number of variable genes to be included in each single-cell dataset (default: 2000)
#' @param n_integ_features number of genes to be utilized for integration (default: 2000)
#' @param integ_dim PC dimensions to be used for integration with reciprocal PCA (default: 50)
#' @param cluster_dim PC dimension to be used for clustering of the spots (default: 30)
#' @param cluster_resolution resolution during the clustering process (default: 0.5)
#' @param maximum_size maximum size allowed for the analysis (default: 6GB)
#' @return reclustered Seurat object
#' @export
recluster_dataset_rpca <- function(data, split.by = "orig.ident", data_type='Single-cell',
                                   n_var_features=2000, n_integ_features=2000,
                                   integ_dim=50,
                                   cluster_dim=30, cluster_resolution=0.5,
                                   maximum_size=6000 * 1024^2){
  # Create a Progress object
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())

  progress$set(message = "Integration", value = 0)

  # Set maximum size to 6 GB
  options(future.globals.maxSize=maximum_size)

  grp_levels <- levels(factor(eval(parse(text=paste0('data$',split.by)))))
  progress$inc(1/(2*length(grp_levels)+9), detail="Splitting object")

  if (data_type=="Single-cell") {
    Seurat::DefaultAssay(data) <- "RNA"
  } else if (data_type=="Spatial"){
    Seurat::DefaultAssay(data) <- "Spatial"
  }
  object_list <- Seurat::SplitObject(data, split.by = split.by)

  # Preprocess for each of the spatial datasets
  for (i in 1:length(object_list)){
    # Increment the progress bar, and update the detail text.
    progress$inc(1/(2*length(object_list)+9), detail=paste("Find variable genes", i))

    object_list[[i]] <- Seurat::FindVariableFeatures(object_list[[i]],
                                                     selection.method = "vst",
                                                     nfeatures=n_var_features)
    Sys.sleep(0.1)
  }

  brain.features <- Seurat::SelectIntegrationFeatures(object_list,
                                                      nfeatures=n_integ_features)

  for (i in 1:length(object_list)){
    progress$inc(1/(2*length(object_list)+9), detail=paste("Scaling & PCA for data", i))
    if (data_type=="Single-cell") {
      object_list[[i]] <- Seurat::ScaleData(object_list[[i]], features = brain.features,
                                            vars.to.regress = c("nCount_RNA","percent.mito"))
    } else if (data_type=="Spatial"){
      object_list[[i]] <- Seurat::ScaleData(object_list[[i]], features = brain.features,
                                            vars.to.regress = c("nCount_Spatial"))
    }
    object_list[[i]] <- Seurat::RunPCA(object_list[[i]], features = brain.features, verbose = FALSE)
    Sys.sleep(0.1)
  }

  progress$inc(1/(2*length(object_list)+9), detail="Find integration anchors")

  anchors <- Seurat::FindIntegrationAnchors(object_list, reference = NULL,
                                            anchor.features = brain.features,
                                            reduction = 'rpca', dims = 1:integ_dim)

  progress$inc(1/(2*length(object_list)+9), detail="Integating data")
  brain.merge <- Seurat::IntegrateData(anchorset=anchors, dims=1:integ_dim)

  progress$inc(1/(2*length(object_list)+9), detail="Scaling integrated data")
  brain.merge <- Seurat::ScaleData(brain.merge, verbose=FALSE)

  progress$inc(1/(2*length(object_list)+9), detail="Running PCA")
  brain.merge <- Seurat::RunPCA(brain.merge, verbose=FALSE)

  progress$inc(1/(2*length(object_list)+9), detail="Finding neighbors")
  brain.merge <- Seurat::FindNeighbors(brain.merge, dims = 1:cluster_dim)

  # Change resolution -> different spot cluster numbers
  progress$inc(1/(2*length(object_list)+9), detail="Finding clusters")
  brain.merge <- Seurat::FindClusters(brain.merge, resolution=cluster_resolution)

  progress$inc(1/(2*length(object_list)+9), detail="Running UMAP")
  brain.merge <- Seurat::RunUMAP(brain.merge, dims = 1:cluster_dim)

  progress$inc(1/(2*length(object_list)+9), detail="Finishing")
  if (data_type=="Single-cell") {
    Seurat::DefaultAssay(object = brain.merge) <- "RNA"
  } else if (data_type=="Spatial"){
    Seurat::DefaultAssay(object = brain.merge) <- "Spatial"
  }

  return(brain.merge)
}

#' Visualizing the spot clusters: spatial mapping with labels
#' @param object the seurat object to visualize the clusters
#' @param grp assigned group name
#' @param slide_title change the title of each slide in plot
#' @param cluster_name name of the clusters to visualize
#' @param crop whether to crop the image: default = TRUE
#' @param alpha transparency of the color spot on the tissue: default: c(0,1)
#' @param image.alpha transparency of the tissue image (default 0.6)
#' @param pt.size.factor size of the spot visualized on the tissue
#' @param title_size size of the title for each spatial plot
#' @param face the type of the text on the plot
#' @param label whether to label the spot cluster
#' @param label.size size of the text on the plot
#' @param ncol column numbers for the plot (number of slides in the column)
#' @param slide.to.visualize vector representing number of slide to visualize: eg: c(1,3)
#' @param spot.cluster.highlight vector of spot cluster name to be highlighted
#' @export
spatial_cluster_plot <- function(object,grp=NULL,
                                 slide_title=grp,
                                 cluster_name='seurat_clusters',crop=TRUE,
                                 alpha=c(1,1),image.alpha=0.6, pt.size.factor=1.6,
                                 title_size=14,face='bold',
                                 label=TRUE,label.size=3, ncol=length(grp),
                                 slide.to.visualize=NULL,
                                 spot.cluster.highlight=NULL){
  slide_null <- FALSE
  if (is.null(grp)|is.null(slide_title)){
    grp_orig_names <- levels(factor(object$orig.ident))
    grp_mod_names <- names(object@images)
    if (!identical(setdiff(grp_mod_names, grp_orig_names), character(0))) {
      grp_sel_num <- sapply(grp_mod_names, function(x){match(x, make.names(grp_orig_names))})
      grp_orig_names <- grp_orig_names[grp_sel_num]
      object$orig.ident <- factor(object$orig.ident, levels = grp_orig_names)
    } else {
      object$orig.ident <- factor(object$orig.ident, levels = names(object@images))
    }
    if (is.null(slide_title)){
      slide_title <- levels(object$orig.ident)
      slide_null <- TRUE
    }
    if (is.null(grp)){
      grp <- levels(object$orig.ident)
    }
  }

  if (!is.null(slide.to.visualize)){
    Seurat::Idents(object) <- object$orig.ident
    if (is.numeric(slide.to.visualize)){
      object <- subset(object, idents=grp[slide.to.visualize])
      spatial_image <- names(object@images)
      object@images <- object@images[spatial_image[slide.to.visualize]]
      grp <- grp[slide.to.visualize]
    } else {
      if (sum(slide.to.visualize %in% grp)==length(slide.to.visualize)){
        object <- subset(object, idents=slide.to.visualize)
        spatial_image <- names(object@images)
        sel_number <- match(slide.to.visualize, grp)
        object@images <- object@images[spatial_image[sel_number]]
        grp <- slide.to.visualize
      } else {
        stop("'slide.to.visualize' should be either number of the slide or the element of the grp")
      }
    }
  }

  cluster_info <- eval(parse(text=paste0('object$',cluster_name)))
  cluster_levels <- levels(cluster_info)
  num_cluster <- length(cluster_levels)

  cluster_colors <- list("#56ebd3", "#1c5e39", "#1fa198", "#7aed7b", "#36a620", "#cadba5",
                         "#33547a", "#24a5f7", "#3337a6", "#d678ef", "#9d0d6c", "#b090d4",
                         "#740ece", "#ef3df3", "#69345e", "#829499", "#809b31", "#f8ba7c",
                         "#683c00", "#d9dc22", "#992a13", "#ec102f", "#df6e78", "#fa7922",
                         "#ae783e", "#7fdc64", "#6f2b6e", "#56cac1", "#1b511d", "#ec9fe7",
                         "#214a65", "#b3d9fa", "#1932bf", "#34f50e")

  if (num_cluster>length(cluster_colors)){
    cluster_colors <- rep(cluster_colors, length.out=num_cluster)
  } else {
    cluster_colors <- cluster_colors[1:num_cluster]
  }

  names(cluster_colors) <- cluster_levels
  Seurat::Idents(object) <- cluster_info

  if (!is.null(spot.cluster.highlight)){
    object <- subset(object, idents = spot.cluster.highlight)
  }

  b <- Seurat::SpatialPlot(object, alpha=alpha, image.alpha=image.alpha,
                           label=label, crop=crop, pt.size.factor=pt.size.factor,
                           label.size = label.size, repel=TRUE,
                           combine=FALSE)

  Seurat::Idents(object) <- object$orig.ident
  if (slide_null){slide_title <- grp}

  for (i in 1:length(b)){
    if (!(grp[i] %in% object$orig.ident)){next}
    object_subset <- subset(object, idents=grp[i])
    Seurat::Idents(object_subset) <- eval(parse(text=paste0('object_subset$',cluster_name)))
    subset_levels <- levels(Seurat::Idents(object_subset))

    b[[i]] <- b[[i]] + ggplot2::ggtitle(grp[i]) +
      ggpubr::fill_palette(unlist(cluster_colors[subset_levels])) +
      ggplot2::theme(plot.title=ggplot2::element_text(size=title_size,face=face,hjust=0.5)) +
      ggplot2::ggtitle(slide_title[i]) + Seurat::NoLegend()

  }
  patchwork::wrap_plots(b, ncol=ncol)
}

#' Draw the frequency boxplot for spots
#' @param object the seurat object to visualize the clusters
#' @param grp assigned group name
#' @param slide_title change the title of each slide in plot
#' @param group_of_interest group name to visualize frequency of clusters (default: orig.ident)
#' @param cluster_name cluster name to evaluate frequency in each group (default: seurat_clusters)
#' @param cluster_colors the colors used to visualize each cluster
#' @param x.axis.title x axis title
#' @param y.axis.title y.axis.title
#' @param legend.title title of the legend
#' @param x.axis.title.size x axis title size
#' @param y.axis.title.size y axis title size
#' @param x.axis.text.size x axis text size
#' @param y.axis.text.size y axis text size
#' @param x.axis.text.angle x axis text angle (default: 0)
#' @param legend.title.size legend title size
#' @param legend.text.size legend text size
#' @param vis.freq.text visualize frequency inside of the boxplot
#' @param freq.text.size size of the frequency text inside of the boxplot
#' @param freq.stats whether to return frequency statistics
#' @importFrom dplyr "%>%"
#' @returns plot object and dataframe containing summarized statistics (when freq.stats=TRUE)
#' @export
frequency_boxplot <- function(object,grp=NULL,
                              slide_title=grp,
                              group_of_interest='orig.ident',
                              cluster_name='seurat_clusters',
                              cluster_colors=list("#56ebd3", "#1c5e39", "#1fa198", "#7aed7b", "#36a620", "#cadba5",
                                                  "#33547a", "#24a5f7", "#3337a6", "#d678ef", "#9d0d6c", "#b090d4",
                                                  "#740ece", "#ef3df3", "#69345e", "#829499", "#809b31", "#f8ba7c",
                                                  "#683c00", "#d9dc22", "#992a13", "#ec102f", "#df6e78", "#fa7922",
                                                  "#ae783e", "#7fdc64", "#6f2b6e", "#56cac1", "#1b511d", "#ec9fe7",
                                                  "#214a65", "#b3d9fa", "#1932bf", "#34f50e"),
                              x.axis.title='Groups',y.axis.title='Frequency',
                              legend.title = 'Clusters',
                              x.axis.title.size=20,y.axis.title.size=20,
                              x.axis.text.size=12,y.axis.text.size=12,
                              x.axis.text.angle=0,
                              legend.title.size=12,legend.text.size=12,
                              vis.freq.text=FALSE, freq.text.size=3.5,
                              freq.stats=FALSE){

  cluster_info <- eval(parse(text=paste0('object$',cluster_name)))
  cluster_levels <- levels(factor(cluster_info))
  num_cluster <- length(cluster_levels)

  cluster_colors <- cluster_colors

  if (num_cluster > length(cluster_colors)){
    cluster_colors <- rep(cluster_colors, length.out=num_cluster)
  } else {
    cluster_colors <- cluster_colors[1:num_cluster]
  }

  if (vis.freq.text){
    # https://rpubs.com/cardiomoon/329677
    eval(parse(text=paste0('percentData <- object@meta.data %>% dplyr::group_by(',
                           group_of_interest,
                           ') %>% dplyr::count(',cluster_name,
                           ') %>%', 'dplyr::mutate(ratio=scales::percent(n/sum(n),accuracy=0.1))')))
    p <- ggplot2::ggplot(object@meta.data,
                         eval(parse(text=paste0('ggplot2::aes(',
                                                group_of_interest,',fill=',cluster_name,')')))) +
      ggplot2::geom_bar(position='fill') +
      ggplot2::geom_text(data=percentData, ggplot2::aes(y=n, label=ratio),
                         position=ggplot2::position_fill(vjust=0.5), size=freq.text.size) +
      ggpubr::fill_palette(unlist(cluster_colors)) +
      ggplot2::theme_classic() +
      ggplot2::xlab(x.axis.title) + ggplot2::ylab(y.axis.title) + ggplot2::labs(fill=legend.title) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = x.axis.title.size),
                     axis.title.y = ggplot2::element_text(size = y.axis.title.size),
                     axis.text.x = ggplot2::element_text(size = x.axis.text.size,
                                                         angle = x.axis.text.angle),
                     axis.text.y = ggplot2::element_text(size = y.axis.text.size),
                     legend.title = ggplot2::element_text(size = legend.title.size),
                     legend.text = ggplot2::element_text(size = legend.text.size)) +
      ggplot2::scale_x_discrete(labels=cluster_info)
    if (freq.stats){
      return(list(p, percentData))
    } else {
      return(list(p))
    }
  } else {
    p <- ggplot2::ggplot(object@meta.data,
                         eval(parse(text=paste0('ggplot2::aes(',
                                                group_of_interest,',fill=',cluster_name,')')))) +
      ggplot2::geom_bar(position='fill') + ggpubr::fill_palette(unlist(cluster_colors)) +
      ggplot2::theme_classic() +
      ggplot2::xlab(x.axis.title) + ggplot2::ylab(y.axis.title) + ggplot2::labs(fill=legend.title) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = x.axis.title.size),
                     axis.title.y = ggplot2::element_text(size = y.axis.title.size),
                     axis.text.x = ggplot2::element_text(size = x.axis.text.size,
                                                         angle = x.axis.text.angle),
                     axis.text.y = ggplot2::element_text(size = y.axis.text.size),
                     legend.title = ggplot2::element_text(size = legend.title.size),
                     legend.text = ggplot2::element_text(size = legend.text.size)) +
      ggplot2::scale_x_discrete(labels=cluster_info) # change the name of groups to visualize
    if (freq.stats){
      eval(parse(text=paste0('percentData <- object@meta.data %>% dplyr::group_by(',
                             group_of_interest,
                             ') %>% dplyr::count(',cluster_name,
                             ') %>%', 'dplyr::mutate(ratio=scales::percent(n/sum(n),accuracy=0.1))')))
      return(list(p, percentData))
    } else {
      return(list(p))
    }
  }
}

#' Visualizing spatial gene expression
#' @param object the seurat object to visualize the clusters
#' @param feat feature to be visualized (genes, module scores...)
#' @param grp assigned group name
#' @param slide_title change the title of each slide in plot
#' @param min mininum value to be visualized with colorbar: default: NA
#' @param max maximum value to be visualized with colorbar: default: NA
#' @param crop whether to crop the image: default = TRUE
#' @param pt.size.factor scale the size of the plot: default: 1.8
#' @param alpha transparency of the color spot on the tissue: default: c(0,1)
#' @param image.alpha transparency of the background tissue: default: 1
#' @param title_size size of the title for each spatial plot
#' @param face the type of the text on the plot
#' @param ncol column numbers for the plot (number of slides in the column)
#' @param palette_color color palette used for coloring spots (default: Seurat colormap)
#' @param color_bar_mode change the mode of colorbar visualization (default: 'default')
#' #' \itemize{
#' \item set color_bar_mode = 'combined' to visualize colorbar only once
#' }
#' @param color_bar_loc location of the colorbar: bottom, top, left, right
#' @param slide.to.visualize vector representing number of slide to visualize: eg: c(1,3)
#' @param cluster_name group name of a specific spot cluster to be visualized
#' @param spot.cluster.highlight vector of spot cluster name to be highlighted
#' @references https://wilkelab.org/cowplot/articles/shared_legends.html
#' @export
spatial_feat_plot_groups <- function(object,feat,grp=NULL,
                                     slide_title=grp,min=NA,max=NA,crop=TRUE,
                                     pt.size.factor=1.8,alpha=c(1,1),image.alpha=1,
                                     title_size=14,face='bold',
                                     ncol=length(grp),palette_color=NULL,
                                     color_bar_mode='default',
                                     color_bar_loc='bottom',slide.to.visualize=NULL,
                                     cluster_name='seurat_clusters',spot.cluster.highlight=NULL){

  slide_null <- FALSE
  if (is.null(grp)|is.null(slide_title)){
    grp_orig_names <- levels(factor(object$orig.ident))
    grp_mod_names <- names(object@images)
    if (!identical(setdiff(grp_mod_names, grp_orig_names), character(0))) {
      grp_sel_num <- sapply(grp_mod_names, function(x){match(x, make.names(grp_orig_names))})
      grp_orig_names <- grp_orig_names[grp_sel_num]
      object$orig.ident <- factor(object$orig.ident, levels = grp_orig_names)
    } else {
      object$orig.ident <- factor(object$orig.ident, levels = names(object@images))
    }
    if (is.null(slide_title)){
      slide_title <- levels(object$orig.ident)
      slide_null <- TRUE
    }
    if (is.null(grp)){
      grp <- levels(object$orig.ident)
    }
  }

  if (!is.null(slide.to.visualize)){
    Seurat::Idents(object) <- object$orig.ident
    if (is.numeric(slide.to.visualize)){
      object <- subset(object, idents=grp[slide.to.visualize])
      spatial_image <- names(object@images)
      object@images <- object@images[spatial_image[slide.to.visualize]]
      grp <- grp[slide.to.visualize]
    } else {
      if (sum(slide.to.visualize %in% grp)==length(slide.to.visualize)){
        object <- subset(object, idents=slide.to.visualize)
        spatial_image <- names(object@images)
        sel_number <- match(slide.to.visualize, grp)
        object@images <- object@images[spatial_image[sel_number]]
        grp <- slide.to.visualize
      } else {
        stop("'slide.to.visualize' should be either number of the slide or the element of the grp")
      }
    }
  }

  Seurat::Idents(object) <- eval(parse(text=paste0('object$',cluster_name)))
  if (!is.null(spot.cluster.highlight)){
    object <- subset(object, idents = spot.cluster.highlight)
  }

  plot <- Seurat::SpatialPlot(object, features=feat,
                              min.cutoff=min, max.cutoff=max, crop=crop,
                              pt.size.factor=pt.size.factor,alpha=alpha,
                              image.alpha=image.alpha,
                              combine=FALSE)

  if (is.vector(palette_color)&!is.list(palette_color)){
    if (length(palette_color)>1){
      mapal <- palette_color
    } else if (palette_color %in% rownames(RColorBrewer::brewer.pal.info)){
      num <- RColorBrewer::brewer.pal.info[palette_color,'maxcolors']
      mapal <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(num,palette_color))(256)
    } else if (palette_color %in% names(colormap::colormaps)){
      mapal <- colormap::colormap(eval(parse(text=paste0('colormap=colormap::colormaps$',palette_color))),
                                  nshades=256)
    }
  } else if (is.null(palette_color)){
  } else {
    stop("Color palette is not identified")
  }

  # Check the feasibility of variable 'color_bar_mode'
  if ((color_bar_mode=='combined')|(color_bar_mode=='default')){
  } else {
    stop("'color_bar_mode' should be either 'default' or 'combined'")
  }

  if (color_bar_mode=='combined'){
    if (is.na(max)|is.na(min)){
      stop("'max' and 'min' should be given when 'color_bar_mode' is 'combined'")
    }
  }

  if (slide_null){
    grp_mod <- rep(grp,(length(plot)%/%length(grp)))
  } else {
    grp_mod <- rep(slide_title,(length(plot)%/%length(grp)))
  }
  feat_mod <- rep(feat,each=(length(plot)%/%length(feat)))
  if (color_bar_mode=='combined'){
    grp_mod <- paste0(grp_mod,': ',feat_mod)
  }

  # Plot for all groups
  for (i in 1:length(plot)){
    if (is.null(palette_color)){
      plot[[i]] <- plot[[i]] + ggplot2::theme(legend.position=color_bar_loc) +
        ggplot2::scale_fill_gradientn(colours = Seurat:::SpatialColors(n=100),
                                      limits = c(min,max))
    } else{
      plot[[i]] <- plot[[i]] + ggplot2::theme(legend.position=color_bar_loc) +
        ggplot2::scale_fill_gradientn(colours = mapal, limits = c(min,max))
    }
    plot[[i]] <- plot[[i]] + ggplot2::ggtitle(grp_mod[i]) +
      ggplot2::theme(plot.title=ggplot2::element_text(size=title_size,face=face,hjust = 0.5)) +
      ggplot2::ggtitle(grp_mod[i])
  }

  if (color_bar_mode=='default'){
    patchwork::wrap_plots(plot, ncol=ncol)
  } else if (color_bar_mode=='combined'){
    patchwork::wrap_plots(plot, ncol=ncol) + patchwork::plot_layout(guides = "collect") &
      ggplot2::theme(legend.title=ggplot2::element_blank(), legend.position=color_bar_loc)
  }
}

#' Automatically saving spatial feature plot by n
#' @param object the seurat object to visualize the clusters
#' @param feats_of_interest feature to be visualized (genes, module scores...)
#' @param grp assigned group name
#' @param slide_title change the group title in each plot
#' @param by_n the number of the features to be visualized in each plot
#' @param min mininum value to be visualized with colorbar
#' @param max maximum value to be visualized with colorbar
#' @param crop whether to crop the image: default = TRUE
#' @param pt.size.factor scale the size of the plot: default: 1.6
#' @param alpha transparency of the color spot on the tissue: default: c(0,1)
#' @param image.alpha transparency of the background tissue: default: 1
#' @param title_size size of the title for each spatial plot
#' @param face the type of the text on the plot
#' @param ncol column numbers for the plot (number of slides in the column)
#' @param palette_color color palette used for coloring spots (default: Seurat colormap)
#' @param color_bar_mode change the mode of colorbar visualization (default: 'default')
#' \itemize{
#' \item set color_bar_mode = 'combined' to visualize colorbar only once
#' }
#' @param color_bar_loc location of the colorbar: bottom, top, left, right
#' @param slide.to.visualize vector representing number of slide to visualize: eg: c(1,3)
#' @param cluster_name group name of a specific spot cluster to be visualized
#' @param spot.cluster.highlight vector of spot cluster name to be highlighted
#' @param height height of the printed image (by inch)
#' @param width width of the printed image (by inch)
#' @param dpi dpi of the image to be saved
#' @param save_path the path to save the file
#' @param file_name header name of the image files to save
#' @param sort.by.expression sort the given feature according the the average expression in whole tissue
#' @param assay assay of the data to use for calculation of average expression
#' @param slot name of the slot to use in the given Seurat object
#' @export
save_feat_plot_by_n <- function(object,feats_of_interest,grp=NULL,
                                slide_title=grp,by_n=3,min=NA,max=NA,crop=TRUE,
                                pt.size.factor=1.8,
                                alpha=c(1,1),image.alpha=1,
                                title_size=14,face='bold',
                                ncol=length(grp),
                                palette_color=NULL,
                                color_bar_mode='default',color_bar_loc='bottom',
                                slide.to.visualize=NULL,
                                cluster_name='seurat_clusters',
                                spot.cluster.highlight=NULL,
                                height = 8, width = 8, dpi = 200,
                                save_path='./figures/integ',
                                file_name = 'feats',
                                sort.by.expression=TRUE, assay=NULL, slot='data'){
  # Sort by the expression level
  if (sort.by.expression){
    if (!is.null(spot.cluster.highlight)){
      Seurat::Idents(object) <- object[[cluster_name]]
      object_subset <- subset(object, idents = spot.cluster.highlight)
      Seurat::Idents(object) <- "Total"
      total_exp_data <- Seurat::AverageExpression(object_subset, features = feats_of_interest,
                                                  assay='Spatial', slot='data')[[1]]
    } else {
      Seurat::Idents(object) <- "Total"
      total_exp_data <- Seurat::AverageExpression(object, features = feats_of_interest,
                                                  assay='Spatial', slot='data')[[1]]
    }
    feats_of_interest <- names(total_exp_data[order(total_exp_data, decreasing=TRUE),])
  }

  for (i in 1:((length(feats_of_interest)-1)%/%by_n+1)){
    if (i == ((length(feats_of_interest)-1)%/%by_n+1)){
      if (i==1) {
        tmp_grp <- feats_of_interest
      } else {
        tmp_grp <- feats_of_interest[-c(1:((i-1)*by_n))]
      }
    } else {
      tmp_grp <- feats_of_interest[((i-1)*by_n+1):(i*by_n)]
    }

    if (is.list(slide.to.visualize)){
      for (j in 1:length(slide.to.visualize)){
        spatial_feat_plot_groups(object,tmp_grp,
                                 slide_title=slide_title,min=min,max=max,crop=crop,
                                 pt.size.factor=pt.size.factor,alpha=alpha,
                                 image.alpha=image.alpha,
                                 title_size=title_size,face=face,ncol=ncol,
                                 palette_color=palette_color,
                                 color_bar_mode=color_bar_mode,
                                 color_bar_loc=color_bar_loc,
                                 slide.to.visualize=slide.to.visualize[[j]],
                                 cluster_name=cluster_name,
                                 spot.cluster.highlight=spot.cluster.highlight)

        ggplot2::ggsave(file.path(save_path, paste0(file_name,'_',i,'_',j,'.png')),
                        height=height, width=width, dpi=dpi, bg = "white", units = "cm")
      }
    } else {
      spatial_feat_plot_groups(object,tmp_grp,
                               slide_title=slide_title,min=min,max=max,crop=crop,
                               pt.size.factor=pt.size.factor,alpha=alpha,
                               image.alpha=image.alpha,
                               title_size=title_size,face=face,ncol=ncol,
                               palette_color=palette_color,
                               color_bar_mode=color_bar_mode,
                               color_bar_loc=color_bar_loc,
                               cluster_name=cluster_name,
                               spot.cluster.highlight=spot.cluster.highlight)

      ggplot2::ggsave(file.path(save_path, paste0(file_name,'_',i,'.png')),
                      height=height, width=width, dpi=dpi, bg = "white", units = "cm")
    }
  }
}



#' Modified featureplot
#' @description modified feature plot to fix the min-max values
#' @param object the seurat object to visualize the clusters
#' @param feat feature to be visualized (genes, module scores...)
#' @param min mininum value to be visualized with colorbar
#' @param max maximum value to be visualized with colorbar
#' @param palette_color color palette used for coloring spots (default: blue)
#' @param ncol column numbers for the plot (number of slides in the column)
#' @export
FeaturePlot_mod <- function(object,feat,min=NA,max=NA,palette_color="blue",
                            ncol=length(feat)){
  p <- Seurat::FeaturePlot(object, features = feat,
                           max.cutoff=max, min.cutoff=min,
                           combine = FALSE)
  for (i in 1:length(p)){
    p[[i]] <- p[[i]] +
      ggplot2::scale_colour_gradientn(colours =c("lightgrey",palette_color),
                                      limits = c(min,max))
  }
  patchwork::wrap_plots(p, ncol=ncol)
}


#' Modified ridgeplot
#' @description modified ridgeplot to fix min-max values
#' @param object the seurat object to visualize the clusters
#' @param feat feature to be visualized (genes, module scores...)
#' @param groups groups to visualize with ridgeplot with
#' @param lim minimum and maximum expression value to be visualized with colorbar
#' @param alpha transparency of the ridgeplot (default=0)
#' @param title_size size of the title in each plot
#' @param x.axis.title x axis title
#' @param y.axis.title y.axis.title
#' @param x.axis.title.size x axis title size
#' @param y.axis.title.size y.axis.title.size
#' @param x.axis.text.size x axis text size
#' @param y.axis.text.size y.axis.text.size
#' @param ncol column numbers for the plot (number of plots in the column)
#' @export
RidgePlot_mod <- function(object,feat,groups='seurat_clusters',
                          lim=c(-0.5,6),alpha=0, title_size = 20,
                          x.axis.title='Expression',y.axis.title='Groups',
                          x.axis.title.size=15,y.axis.title.size=15,
                          x.axis.text.size=12,y.axis.text.size=12,
                          ncol=length(feat)){

  n_groups <- length(levels(eval(parse(text=paste0('object$',groups)))))
  ridge_data <- Seurat::FetchData(object, vars=c(feat,groups),slot='data')

  p <- list()
  for (i in 1:length(feat)){
    p[[i]] <- ggplot2::ggplot(ridge_data,
                              eval(parse(text=paste0("ggplot2::aes(x=`",feat[i],"`,y=",groups,
                                                     ',color=',groups,')')))) +
      ggridges::geom_density_ridges(scale = 4, size = 1, alpha = alpha) +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::scale_y_discrete(expand = c(0, 0)) +
      ggplot2::coord_cartesian(clip = "off") +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::xlim(lim) + ggplot2::ggtitle(feat[i]) +
      ggplot2::xlab(x.axis.title) + ggplot2::ylab(y.axis.title) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = title_size, hjust=0.5, face='bold'),
                     axis.title.x = ggplot2::element_text(size = x.axis.title.size),
                     axis.title.y = ggplot2::element_text(size = y.axis.title.size),
                     axis.text.x = ggplot2::element_text(size = x.axis.text.size),
                     axis.text.y = ggplot2::element_text(size = y.axis.text.size),
                     legend.position = "none")

  }
  patchwork::wrap_plots(p, ncol=ncol)
}

#' Quantitation of the regional values
#' @keywords internal
#' @param object Seurat object to use during the analysis
#' @param data.to.use values to use for the quantitation (it can be gene lists or metadata lists, however the two cannot be given together)
#' @param group.to.compare group to compare between: x axis of the plot
#' @param group.to.facet group to facet the plot
#' @param agg.to.boxplot whether to aggregate all the data points included in each group.to.compare group and split and recode
#' @param group.to.split group to split the aggregated plot with
#' @param group.to.recode group to recode the 'group.to.split'
#' \itemize{
#' \item if the split group is orig.ident and is composed of "3mo_WT","7mo_WT","3mo_TG","7mo_TG", the recode group can be defined by reclassifying the each of element to "WT","WT","TG","TG"
#' \item if the recode group is saved in object as 'cluster' then group.to.recode='cluster'
#' }
#' @param calculate_mode values to calculate in each group ('mean','sum', or 'boxplot')
#' @param pairwise.comp.stats whether to calculate pairwise wilcoxon test and visualize p-value on top of each group
#' @param cluster_colors the colors the use for the visualization
#' @param plot_ncol the number of the facets in the column
#' @param x.axis.title x axis title (default: '')
#' @param y.axis.title y axis title (default: 'cell fraction')
#' @param x.axis.title.size x axis title size (default: 20)
#' @param y.axis.title.size y axis title size (default: 20)
#' @param x.axis.text.size x axis text size (default: 10)
#' @param y.axis.text.size y axis text size (default: 12)
#' @param x.axis.text.angle x axis text angle (default: 90)
#' @param lengend.title.size size of the legend title (default: 12)
#' @param legened.text.size size of the legend text(default: 12)
#' @param vis.value.text whether to visualize the values on the barplot
#' @param value.text.size size of the value text (y-axis)
#' @param return.stats whether to return the statistics along with a plot
#' @param spot.total.num.stats whether to calculate and return total number of spots in each group
#' @importFrom dplyr "%>%"
#' @returns plot object and dataframe containing summarized statistics (when return.stats=TRUE)
#' @export
quantitation_plot <- function(object,
                              data.to.use=NULL,
                              group.to.compare='orig.ident',
                              group.to.facet='seurat_clusters',
                              agg.to.boxplot=FALSE,
                              group.to.split='orig.ident',
                              group.to.recode=NULL,
                              calculate_mode='mean',
                              pairwise.comp.stats=NULL,
                              cluster_colors='npg',
                              plot_ncol=4,
                              x.axis.title='', y.axis.title='cell fraction',
                              x.axis.title.size=20,y.axis.title.size=20,
                              x.axis.text.size=10,y.axis.text.size=12,
                              x.axis.text.angle=90,
                              legend.title.size=12, legend.text.size=12,
                              vis.value.text=FALSE, value.text.size=3.5,
                              return.stats=FALSE, spot.total.num.stats=FALSE){

  # Check if orig.ident is factor with levels and if not, define as a factor
  if (is.null(levels(object$orig.ident))){
    grp_orig_names <- levels(factor(object$orig.ident))
    grp_mod_names <- names(object@images)
    if (!identical(setdiff(grp_mod_names, grp_orig_names), character(0))) {
      grp_sel_num <- sapply(grp_mod_names, function(x){match(x, make.names(grp_orig_names))})
      grp_orig_names <- grp_orig_names[grp_sel_num]
      object$orig.ident <- factor(object$orig.ident, levels = grp_orig_names)
    } else {
      object$orig.ident <- factor(object$orig.ident, levels = names(object@images))
    }
  }

  # Check if group.to.recode and group.to.split is feasible
  if (agg.to.boxplot&(is.null(group.to.split)|is.null(group.to.recode))){
    stop("Both 'group.to.split' and 'group.to.recode' should be provided")
  }

  # Metadata include
  sp_metadata <- object@meta.data

  if (is.null(data.to.use)){
    sp_metadata_mod <- sp_metadata[grep('_cellf',colnames(sp_metadata),value=TRUE)]
  } else {
    if (identical(setdiff(data.to.use, colnames(sp_metadata)),character(0))){
      sp_metadata_mod <- sp_metadata[data.to.use]
    } else {
      sp_metadata_mod <- expm1(Seurat::FetchData(object, vars = data.to.use))
      # Expm1 and calculate normalized count (reverse log transform)
    }
  }

  # Assigning colors
  if (agg.to.boxplot){
    num_cluster <- length(levels(factor(object@meta.data[[group.to.recode]])))
  } else {
    num_cluster <- length(colnames(sp_metadata))
  }

  cluster_colors <- ggpubr::get_palette(palette = cluster_colors, num_cluster)
  # Add data column to metadata
  eval(parse(text=paste0('sp_metadata_mod$',group.to.compare,
                         '<- sp_metadata$',group.to.compare)))
  eval(parse(text=paste0('group_levels <- levels(sp_metadata_mod$',
                         group.to.compare,')')))
  if (agg.to.boxplot){
    group.to.facet <- group.to.split
    df_recode <- sp_metadata[,c(group.to.recode, group.to.facet)] %>% dplyr::distinct()
    group_levels_recode <- df_recode[[group.to.recode]]
    names(group_levels_recode) <- df_recode[[group.to.facet]]
  }

  eval(parse(text=paste0('sp_metadata_mod$',group.to.facet,
                         '<- sp_metadata$',group.to.facet)))


  if (calculate_mode=='sum') {
    stats_mode <- 'sum'
    y_axis_name <- 'Sum of '
  } else if (calculate_mode=='mean') {
    stats_mode <- 'mean'
    y_axis_name <- 'Average '
  } else if (calculate_mode=='skewness') {
    stats_mode <- 'skewness'
    y_axis_name <- 'Skewness of '
  } else if (calculate_mode=='kurtosis') {
    stats_mode <- 'kurtosis'
    y_axis_name <- 'Kurtosis of '
  } else if (calculate_mode=='boxplot') {
  } else {
    stop("'calculate_mode' should be 'sum', 'mean', 'skewness', 'kurtosis', or 'boxplot'")
  }

  for (j in 1:length(group_levels)){
    eval(parse(text=paste0('sp_metadata_mod_group <- sp_metadata_mod %>% dplyr::filter(',
                           group.to.compare,'== group_levels[j]) %>%',
                           'dplyr::select(-c(',group.to.compare,'))')))

    index <- (length(colnames(sp_metadata_mod_group))-1)

    for (i in 1:index){
      # Calculate total number of spots per group
      eval(parse(text=paste0('total_number_facet_group <- sp_metadata_mod_group %>%',
                             'dplyr::group_by(',group.to.facet,') %>%',
                             'dplyr::summarize(total=dplyr::n()) %>%',
                             'dplyr::arrange(',group.to.facet,')')))
      if (agg.to.boxplot){
        total_number_facet_group$Recode <- group_levels_recode[total_number_facet_group[[group.to.facet]]]
      }

      if (!(calculate_mode=='boxplot')){
        # Check if the given data is metadata or genes
        if (calculate_mode %in% c("skewness","kurtosis")){
          eval(parse(text=paste0('summ_data <- sp_metadata_mod_group %>%',
                                 'dplyr::group_by(`',group.to.facet,'`) %>%', # cluster_rename
                                 'dplyr::summarize(`',colnames(sp_metadata_mod_group)[i],
                                 '` =moments::',stats_mode,'(`',colnames(sp_metadata_mod_group)[i],'`)) %>%',
                                 'dplyr::mutate(Names = "',colnames(sp_metadata_mod_group)[i],
                                 '", Models = "',group_levels[j],'") %>%',
                                 'dplyr::arrange(',group.to.facet,')')))
        } else {
          eval(parse(text=paste0('summ_data <- sp_metadata_mod_group %>%',
                                 'dplyr::group_by(`',group.to.facet,'`) %>%', # cluster_rename
                                 'dplyr::summarize(`',colnames(sp_metadata_mod_group)[i],
                                 '` =',stats_mode,'(`',colnames(sp_metadata_mod_group)[i],'`)) %>%',
                                 'dplyr::mutate(Names = "',colnames(sp_metadata_mod_group)[i],
                                 '", Models = "',group_levels[j],'") %>%',
                                 'dplyr::arrange(',group.to.facet,')')))
        }
        # Add the spot number information
        summ_data$total <- total_number_facet_group$total
        if (agg.to.boxplot){
          # Add group recode information
          summ_data$Recode <- group_levels_recode[summ_data[[group.to.facet]]]
        }
      } else {
        df_num <- as.data.frame(lapply(total_number_facet_group, rep,
                                       total_number_facet_group$total))

        eval(parse(text=paste0('summ_data <- sp_metadata_mod_group %>%',
                               'dplyr::select(',group.to.facet,',`',
                               colnames(sp_metadata_mod_group)[i],'`) %>%', # cluster_rename
                               'dplyr::mutate(Names = "',colnames(sp_metadata_mod_group)[i],
                               '", Models = "',group_levels[j],'") %>%',
                               'dplyr::arrange(',group.to.facet,')')))
        summ_data$total <- df_num$total
      }
      if (agg.to.boxplot){
        colnames(summ_data) <- c("Groups", "Values", "Names", "Models",
                                 "Numbers", "Recode")
      } else {
        colnames(summ_data) <- c("Groups", "Values", "Names", "Models", "Numbers")
      }

      if (i==1) {
        df_summ <- summ_data
      } else {
        df_summ <- rbind(df_summ, summ_data)
      }
    }
    if (j==1){
      df_total <- df_summ
    } else {
      df_total <- rbind(df_total, df_summ)
    }
  }

  df_total$Names <- factor(df_total$Names, levels=data.to.use)
  df_total$Models <- factor(df_total$Models, levels=group_levels)

  # Check if all the data is genes
  if (!identical(setdiff(data.to.use, colnames(sp_metadata)),character(0))){
    df_total[["Values"]] <- log1p(df_total[["Values"]])
  }

  if (agg.to.boxplot){
    p <- ggpubr::ggboxplot(df_total, "Recode", "Values",
                           color = "Recode", palette = cluster_colors, add="point") +
      ggplot2::facet_grid(Names ~ Models) +
      ggplot2::xlab(x.axis.title) + ggplot2::ylab(paste0(y_axis_name, y.axis.title)) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = x.axis.title.size),
                     axis.title.y = ggplot2::element_text(size = y.axis.title.size),
                     axis.text.x = ggplot2::element_text(size = x.axis.text.size,
                                                         angle = x.axis.text.angle, hjust=1),
                     axis.text.y = ggplot2::element_text(size = y.axis.text.size),
                     legend.title = ggplot2::element_text(size = legend.title.size),
                     legend.text = ggplot2::element_text(size = legend.text.size))
    if (length(levels(factor(total_number_facet_group$Recode))) > 2){
      p <- p + ggpubr::stat_compare_means(method = 'kruskal.test',
                                          ggplot2::aes(label = ifelse(p<1.e-3,sprintf("p < 0.001"),
                                                                      sprintf("p = %4.3f",as.numeric(..p.format..)))),
                                          size=3.5)
    }
    if (!is.null(pairwise.comp.stats)){
      if (pairwise.comp.stats %in% c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")){
        eval(parse(text=paste0('temp <- df_total %>% ',
                               'group_by(Models, Names) %>% ',
                               'rstatix::wilcox_test(Values~Recode)')))
        temp <- temp %>% rstatix::adjust_pvalue(method = pairwise.comp.stats) %>%
          rstatix::add_significance() %>% rstatix::add_xy_position(x = "Recode", dodge = 0.8)
        p <- p + ggpubr::stat_pvalue_manual(temp, label = "p.adj.signif", tip.length = 0.01,
                                            hide.ns=TRUE) +
          ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.01, 0.1)))
      } else {
        stop("Incorrect method name for p-value adjustment")
      }
    }
  } else {
    if (!(calculate_mode=='boxplot')){
      p <- ggpubr::ggbarplot(df_total, "Models", "Values",
                             fill = "Names", color = "Names", palette = cluster_colors,
                             label = vis.value.text, lab.size = value.text.size) +
        ggplot2::facet_wrap(.~ Groups, ncol=plot_ncol) +
        ggplot2::xlab(x.axis.title) + ggplot2::ylab(paste0(y_axis_name, y.axis.title)) +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = x.axis.title.size),
                       axis.title.y = ggplot2::element_text(size = y.axis.title.size),
                       axis.text.x = ggplot2::element_text(size = x.axis.text.size, angle = x.axis.text.angle, hjust=1),
                       axis.text.y = ggplot2::element_text(size = y.axis.text.size),
                       legend.title = ggplot2::element_text(size = legend.title.size),
                       legend.text = ggplot2::element_text(size = legend.text.size))
    } else {
      p <- ggpubr::ggboxplot(df_total, "Models", "Values",
                             color = "Names", palette = cluster_colors) +
        ggplot2::facet_wrap(.~ Groups, ncol=plot_ncol) +
        ggplot2::xlab(x.axis.title) + ggplot2::ylab(y.axis.title) +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = x.axis.title.size),
                       axis.title.y = ggplot2::element_text(size = y.axis.title.size),
                       axis.text.x = ggplot2::element_text(size = x.axis.text.size,
                                                           angle = x.axis.text.angle, hjust=1),
                       axis.text.y = ggplot2::element_text(size = y.axis.text.size),
                       legend.title = ggplot2::element_text(size = legend.title.size),
                       legend.text = ggplot2::element_text(size = legend.text.size))
    }
  }

  if (spot.total.num.stats) {
    if (agg.to.boxplot){
      p <- p + ggpubr::ggline(df_total, "Models", "Numbers",
                              color = "Recode", palette = cluster_colors) +
        ggplot2::facet_wrap(.~ Names, ncol=plot_ncol) +
        ggplot2::xlab(x.axis.title) + ggplot2::ylab('Total number of spots') +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = x.axis.title.size),
                       axis.title.y = ggplot2::element_text(size = y.axis.title.size),
                       axis.text.x = ggplot2::element_text(size = x.axis.text.size,
                                                           angle = x.axis.text.angle, hjust=1),
                       axis.text.y = ggplot2::element_text(size = y.axis.text.size),
                       legend.title = ggplot2::element_text(size = legend.title.size),
                       legend.text = ggplot2::element_text(size = legend.text.size))
    } else {
      p <- p + ggpubr::ggline(df_total, "Models", "Numbers") +
        ggplot2::facet_wrap(.~ Groups, ncol=plot_ncol) +
        ggplot2::xlab(x.axis.title) + ggplot2::ylab('Total number of spots') +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = x.axis.title.size),
                       axis.title.y = ggplot2::element_text(size = y.axis.title.size),
                       axis.text.x = ggplot2::element_text(size = x.axis.text.size,
                                                           angle = x.axis.text.angle, hjust=1),
                       axis.text.y = ggplot2::element_text(size = y.axis.text.size),
                       legend.title = ggplot2::element_text(size = legend.title.size),
                       legend.text = ggplot2::element_text(size = legend.text.size))
    }
  }

  if (return.stats){
    return(list(p, df_total))
  } else {
    return(list(p))
  }
}

#' R wrapper for NSForest
#' @param object Seurat object for the analysis
#' @param assay name of the assay for the analysis
#' @param slot name of the data slot for the analysis
#' @param outdir file directory to save the model and result
#' @param rfTrees Number of trees
#' @param clusterLabelcolumnHeader name of the column where cluster assignments reside.
#' @param Median_Expression_Level median expression level for removing negative markers
#' @param Genes_to_testing How many ranked genes by binary score will be evaluated in permutations by fbeta-score
#' @param betaValue Set values for fbeta weighting. 1 is default f-measure.
#' @param conda.env.name name of the conda environemt (already installed with Anaconda) to use for CellDART analysis (default: 'CellDART')
#' @return dataframe containing optimal combination of marker genes in each cluster
#' @references Aevermann B, Zhang Y, Novotny M, Keshk M, Bakken T, Miller J, Hodge R, Lelieveldt B, Lein E, Scheuermann RH. A machine learning method for the discovery of minimum marker gene combinations for cell type identification from single-cell RNA sequencing. Genome Res. 2021 Oct;31(10):1767-1780. doi: 10.1101/gr.275569.121.
#' @references https://github.com/JCVenterInstitute/NSForest
#' @export
NS_Forest_R <- function(object,assay="RNA",slot="data",
                        outdir='./',
                        clusterLabelcolumnHeader="louvain",
                        rfTrees=1000,Median_Expression_Level=0, Genes_to_testing=6,
                        betaValue=0.5,
                        conda.env.name='NSForest'){
  # Create a Progress object
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  progress$set(message = "NSForest", value = 0)

  # Suppress warnings
  defaultW <- getOption("warn")
  options(warn = -1)

  progress$inc(1/4, detail="Installing conda env")
  try(reticulate::install_miniconda())
  # Setting virtual environment with reticulate
  if (!(conda.env.name %in% reticulate::conda_list()[['name']])){

    reticulate::conda_create(envname = conda.env.name, python_version = '3.8.12')
    # Create conda env and install dependencies
    reticulate::conda_install(conda.env.name, ignore_installed=T,
                              pip = T, "git+https://github.com/mexchy1000/CellDART.git")
    reticulate::conda_install(conda.env.name, ignore_installed=T,
                              packages = c("graphviz", "numexpr"), pip=T)
  }
  reticulate::use_condaenv(conda.env.name, required = T)

  progress$inc(1/4, detail="Converting data")
  ## Create anndata
  scanpy_data <- reticulate::import('anndata', convert = FALSE)

  ## Import python function
  NSForest_dir <- system.file("python", "NSForest_v3.py", package="STquantool")
  reticulate::source_python(NSForest_dir)

  # Define count matrix
  sparse_mtx <- Seurat::GetAssayData(object, slot = slot, assay = assay)

  # Define obs and var (reference from sceasy library)
  obs <- object@meta.data
  if (!clusterLabelcolumnHeader %in% colnames(obs)){
    stop("Column name for the cell annotation should be provided.")
  } else {
    obs <- obs[clusterLabelcolumnHeader]
    obs[[clusterLabelcolumnHeader]] <- factor(obs[[clusterLabelcolumnHeader]])
  }
  var <- data.frame(matrix(nrow=dim(object)[1],ncol=0,
                           dimnames = list(make.names(rownames(object)),NULL)))
  var[['name']] <- make.names(rownames(var)) ## make names (change genes starting with number)

  data_file <- scanpy_data$AnnData(
    X = Matrix::t(sparse_mtx),
    obs = obs,
    var = var
  )
  progress$inc(1/4, detail="Running NSForest")
  result <- NS_Forest(data_file, outdir = outdir,
                      clusterLabelcolumnHeader = clusterLabelcolumnHeader,
                      rfTrees = rfTrees,
                      Median_Expression_Level = Median_Expression_Level,
                      Genes_to_testing = Genes_to_testing,
                      betaValue = betaValue)

  progress$inc(1/4, detail="Finishing")
  options(warn = defaultW)
  return(result)
}

#' Find markers for the cell type based on Seurat FindAllMarkers
#' @description Calculate markers for the cell type based on Seurat FindAllMarkers. Instead, the column containing average expression in each cluster was added
#' @param object Seurat object to find markers
#' @param purpose purpose of the function: marker discovery or differentially expressed genes finding
#' @param group.to.find identity of the group to find markers within
#' @param logfc.threshold refer to Seurat::FindMarkers
#' @param test.use refer to Seurat::FindMarkers
#' @param ident.1 refer to Seurat::FindMarkers
#' @param ident.2 refer to Seurat::FindMarkers
#' @param slot refer to Seurat::FindMarkers
#' @param assay refer to Seurat::FindMarkers
#' @param min.pct refer to Seurat::FindMarkers
#' @param only.pos refer to Seurat::FindMarkers
#' @param latent.vars refer to Seurat::FindMarkers
#' @param min.cells.group refer to Seurat::FindMarkers
#' @return dataframe with average log fold change, percentage of cells detected in each group (ident.1 and ident.2), adjusted p-value and average feature value in each group
#' @importFrom dplyr "%>%"
#' @export
FindMarkers_mod <- function(object, purpose="marker",
                            group.to.find='seurat_clusters',
                            logfc.threshold=0.25, test.use = "wilcox",
                            ident.1=NULL, ident.2=NULL,
                            slot = "data", assay = NULL, min.pct = 0.1,
                            only.pos = FALSE, latent.vars = NULL,
                            min.cells.group = 3){
  Seurat::Idents(object) <- object[[group.to.find]]

  if (purpose=="marker"){
    df <- Seurat::FindAllMarkers(object, logfc.threshold=logfc.threshold,
                                 test.use = test.use, slot = slot, min.pct = min.pct,
                                 only.pos = only.pos, latent.vars = latent.vars,
                                 assay = assay,
                                 min.cells.group = min.cells.group)
  } else if (purpose=="DEG"){
    if (sum(is.null(ident.1),is.null(ident.2))==0){
      df <- Seurat::FindMarkers(object, logfc.threshold=logfc.threshold,
                                test.use = test.use, slot = slot, min.pct = min.pct,
                                ident.1 = ident.1, ident.2 = ident.2,
                                only.pos = only.pos, latent.vars = latent.vars,
                                assay = assay,
                                min.cells.group = min.cells.group)
      df[['gene']] <- rownames(df)
    } else {
      stop("Both 'ident.1' and 'ident.2' should be provided for DEG")
    }
  } else {
    stop("Purpose should be either 'marker' or 'DEG'")
  }

  df_append <- data.frame()
  if (purpose=="marker"){
    total_exp_data <- log1p(Seurat::AverageExpression(object, assays=assay, slot=slot,
                                                      group.by = group.to.find)[[1]])
    for (i in levels(df[['cluster']])){
      df_tmp <- as.data.frame(total_exp_data[df[df['cluster']==i,][['gene']], i])
      df_append <- rbind(df_append, df_tmp)
    }
    colnames(df_append) <- "avg_exp_clust"
  } else {
    Seurat::Idents(object) <- object[[group.to.find]]
    total_exp_data <- Seurat::GetAssayData(object, assay=assay, slot=slot)
    exp_data1 <- Matrix::rowMeans(expm1(total_exp_data[df[['gene']],
                                                       Seurat::WhichCells(object, idents=ident.1)])) %>%
      log1p(.) %>% as.data.frame()
    exp_data2 <- Matrix::rowMeans(expm1(total_exp_data[df[['gene']],
                                                       Seurat::WhichCells(object, idents=ident.2)])) %>%
      log1p(.) %>% as.data.frame()

    df_append <- cbind(exp_data1, exp_data2)
    colnames(df_append) <- c("avg_exp_ident.1","avg_exp_ident.2")
  }

  df <- cbind(df, df_append)

  return(df)
}

#' Predict spatial cell composition
#' @description Predict spatial cell composition using CellDART
#' @param sp_data spatial data (Seurat object) to be used in predicting cell fraction: non-normalized raw data should be in 'counts' slot
#' @param sc_data single-cell data (Seurat object) to be used in making pseudospots: non-normalized raw data should be in 'counts' slot
#' @param outdir the directory to save output files (models and results) (default = '.')
#' @param sp_subset whether to subset spatial data and calculate for specific spot cluster (default = FALSE)
#' @param spot.cluster.name group name of the cluster used for subsetting spatial data (default: 'seurat_clusters')
#' @param spot.cluster.of.interest name of each spot clusters to be used (default: NULL)
#' @param metadata_celltype column name for single-cell annotation data in metadata (default: 'celltype')
#' @param conda.env.name name of the conda environment to use for CellDART analysis (default: 'CellDART')
#' @param gpu check whether to use gpu (True) or not (False) (default = True)
#' @param metadata_celltype column name for single-cell annotation data in metadata (default: 'celltype')
#' @param num_markers number of selected marker genes in each cell-type (default = 20)
#' @param seed_num seed to be used in random sampling (default = 0)
#' @param nmix the number of cells sampled from single-cell data when making a pseudospot (default = 10)
#' @param npseudo a total number of pseudospots (default = 20000)
#' @param alpha loss weights of domain classifier to the source classifier (default = 0.6)
#' @param alpha_lr learning rate for the domain classifier (alpha_lr*0.001, default = 5)
#' @param emb_dim output size of dimensions for feature extractor (default = 64)
#' @param batch_size minibatch size for pseudospots and spatial data during the training (default = 512)
#' @param n_iterations iteration number for the adversarial learning (default = 3000)
#' @param init_train_epoch iteration number for the pre-training process (default = 10)
#' @return spatial data (Seurat object) with predicted cell fraction in metadata (meta.data)
#' @references Sungwoo Bae, Kwon Joong Na, Jaemoon Koh, Dong Soo Lee, Hongyoon Choi, Young Tae Kim, CellDART: cell type inference by domain adaptation of single-cell and spatial transcriptomic data, Nucleic Acids Research, Volume 50, Issue 10, 10 June 2022, Page e57, https://doi.org/10.1093/nar/gkac084
#' @references https://github.com/mexchy1000/CellDART
#' @export
pred_cellf_celldart <- function(sp_data, sc_data, outdir='.',
                                sp_subset=FALSE, spot.cluster.name='seurat_clusters',
                                spot.cluster.of.interest=NULL,
                                conda.env.name='CellDART',
                                gpu=TRUE, metadata_celltype='celltype',
                                num_markers=20, seed_num=0,
                                nmix=8, npseudo=20000, alpha=0.6, alpha_lr=5,
                                emb_dim=64, batch_size=512, n_iterations=3000,
                                init_train_epoch=10){
  # Create a Progress object
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  progress$set(message = "CellDART", value = 0)

  # Suppress warnings
  defaultW <- getOption("warn")
  options(warn = -1)

  progress$inc(1/6, detail="Installing conda env")

  try(reticulate::install_miniconda())
  # Setting virtual environment with reticulate
  if (!(conda.env.name %in% reticulate::conda_list()[['name']])){
    reticulate::conda_create(envname = conda.env.name, python_version = '3.8.12')
    # Create conda env and install dependencies
    reticulate::conda_install(conda.env.name, ignore_installed=T,
                              pip = T, "git+https://github.com/mexchy1000/CellDART.git")
    reticulate::conda_install(conda.env.name, ignore_installed=T,
                              packages = c("graphviz", "numexpr"), pip=T)
  }
  reticulate::use_condaenv(conda.env.name, required = T)

  ## Import anndata
  ann <- reticulate::import('anndata', convert = FALSE)

  ## Import python function
  CellDART <- reticulate::import('CellDART', convert = FALSE)

  progress$inc(1/6, detail="Converting single-cell data")
  ## 1. Saving single-cell data in anndata format
  # Define count matrix
  sparse_mtx <- Seurat::GetAssayData(sc_data, slot = "counts", assay = "RNA")

  # Define obs and var (reference from sceasy library: https://github.com/cellgeni/sceasy)
  obs <- sc_data@meta.data
  if (!metadata_celltype %in% colnames(obs)){
    stop("Column name for the cell annotation should be provided.")
  } else {
    obs <- obs[metadata_celltype]
    obs[[metadata_celltype]] <- factor(obs[[metadata_celltype]])
  }
  var <- data.frame(matrix(nrow=dim(sc_data)[1],ncol=0,
                           dimnames = list(rownames(sc_data),NULL)))
  var[['name']] <- rownames(var)

  adata_sc <- ann$AnnData(
    X = Matrix::t(sparse_mtx),
    obs = obs,
    var = var
  )

  progress$inc(1/6, detail="Converting spatial data")
  ## 2. Subsetting spatial data and save in anndata format
  if (sp_subset){
    cluster_info <- sp_data[[spot.cluster.name]][,1]
    Seurat::Idents(sp_data) <- spot.cluster.name
  }

  if (is.null(spot.cluster.of.interest)){
    sp_data_sub <- sp_data
  } else if (sum(spot.cluster.of.interest%in%levels(factor(cluster_info)))==length(spot.cluster.of.interest)){
    sp_data_sub <- subset(sp_data, idents=spot.cluster.of.interest)
  } else {
    stop("'spot.cluster.of.interest' should be among the levels of 'spot.cluster.name' provided")
  }

  # Define count matrix
  sparse_mtx <- Seurat::GetAssayData(sp_data_sub, slot = "counts", assay = "Spatial")

  # Define obs and var (reference from sceasy library)
  obs <- sp_data_sub@meta.data
  var <- data.frame(matrix(nrow=dim(sp_data_sub)[1],ncol=0,
                           dimnames = list(rownames(sp_data_sub),NULL)))
  var[['name']] <- rownames(var)

  adata_sp <- ann$AnnData(
    X = Matrix::t(sparse_mtx),
    obs = obs,
    var = var
  )

  # Assign the output directory for the models generated
  progress$inc(1/6, detail="Creating output directory")
  if (!file.exists(outdir)){
    dir.create(file.path(outdir, 'results'))
  }
  out_dir <- file.path(getwd(), outdir, 'results')

  # Run CellDART
  progress$inc(1/6, detail="Running CellDART")
  try({
    df <- CellDART$pred_cellf_celldart$pred_cellf_celldart(adata_sp=adata_sp, adata_sc=adata_sc, count_from_raw=FALSE,
                                                           gpu=gpu, celltype=metadata_celltype, num_markers=num_markers,
                                                           nmix=nmix, npseudo=npseudo, alpha=alpha, alpha_lr=alpha_lr,
                                                           batch_size=batch_size, emb_dim=emb_dim, n_iterations=n_iterations,
                                                           init_train_epoch=init_train_epoch,
                                                           outdir=out_dir, return_anndata=FALSE)

    # Saving cell fraction data into the metadata of spatial Seurat object
    sp_data_sub <- Seurat::AddMetaData(sp_data_sub, reticulate::py_to_r(df))
  })

  progress$inc(1/6, detail="Finishing")
  options(warn = defaultW)
  return(sp_data_sub)
}
