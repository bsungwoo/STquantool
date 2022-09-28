library(shiny)
library(DT)
library(dplyr)

options(shiny.maxRequestSize = 50*1024*1024^2)

#' Internal function to close browser
#' @description Close app when closing browser
#' @keywords internal
#' @export
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

#' Internal UI function
#' @description UI function for STquantool
#' @keywords internal
ui <- shiny::navbarPage(title = "STquantool", theme = shinythemes::shinytheme("spacelab"),
                 shiny::tabPanel(title = "Info",
                          shiny::h2(style = "font-family:San-serif", "STquantool"),
                          shiny::br(),
                          shiny::h4(style = "font-family:San-serif",
                             paste0("ST analysis tool to visualize and quantify multiple datasets")),
                          # Image insertion
                          shiny::img(height=400,width=700,src="Main.png"),
                          shinyjs::useShinyjs(),
                          shinyjs::extendShinyjs(text = jscode, functions = c("closeWindow")),
                          shiny::br(),
                          shiny::actionButton("close", "End session")
                 ),

                 shiny::tabPanel(title = "Set directory (Load & Save)",
                          shiny::mainPanel(
                            shiny::fluidRow(
                              shiny::column(7,
                                     shiny::wellPanel(style = "height:350px;",
                                               shiny::h4("Set directory and make output folder"),
                                               shinyFiles::shinyDirButton("dir", "Find directory", "Search"),
                                               shiny::verbatimTextOutput("dir", placeholder = TRUE),
                                               shiny::actionButton(inputId = "set_wd", label = "Set working directory"),
                                               shiny::br(),
                                               shiny::br(),
                                               shiny::textInput(inputId = "output_folder_name",
                                                         label = "Name of the output folder"),
                                               shiny::actionButton(inputId = "output_make", label = "Assign output folder")
                                     )
                              ),

                              shiny::column(5,
                                     shiny::wellPanel(style = "height:350px;",
                                               shiny::h4("Upload & Save"),
                                               shiny::radioButtons("save_radio","Data format",
                                                            choices = list("Single-cell","Spatial", "Genes: stored", "Genes: abundance"),
                                                            selected = "Single-cell"),
                                               shiny::actionButton("data_save", "Data save"),
                                               shinyFiles::shinyFilesButton("data_load", "Data load", "Search", FALSE),
                                               shiny::br(),shiny::br(),
                                               shiny::h4("Convert file to sparse matrix (.rds)"),
                                               shiny::actionButton("convert_file_to_sparse", "Convert")

                                     )
                              )
                            ),
                            shiny::fluidRow(
                              shiny::column(12,
                                     shiny::wellPanel(style = "height:100px;",
                                               shiny::verbatimTextOutput("cmd")
                                     )
                              )
                            )
                          )
                 ),
                 shiny::tabPanel(title = "QC",
                          shiny::sidebarLayout(
                            shiny::sidebarPanel(
                              shiny::tabsetPanel(id = "qc_tabset",
                                          shiny::tabPanel("Directory",
                                                   shiny::br(),
                                                   shinyFiles::shinyDirButton('qc_dir', 'Find directory for QC',
                                                                  'Search'),
                                                   shiny::verbatimTextOutput("qc_dir", placeholder = TRUE),
                                                   shiny::radioButtons("qc_data_type","Data format",
                                                                choices = list("Single-cell", "Spatial"),
                                                                selected = "Single-cell"),
                                                   shiny::actionButton(inputId = "dir_qc", label = "Data load")
                                          ),
                                          shiny::tabPanel("Single-cell",
                                                   shiny::wellPanel(
                                                     sliderInput(inputId = "nCount_RNA",
                                                                 label = "nCount_RNA: threshold",
                                                                 value = 100, min = 0, max = 1000, step=10),
                                                     sliderInput(inputId = "percent.mt",
                                                                 label = "percent.mt: threshold",
                                                                 value = 5, min = 0, max = 100, step=1),
                                                     sliderInput(inputId = "nFeature_RNA",
                                                                 label = "nFeature_RNA: threshold",
                                                                 value = 300, min = 0, max = 1000, step=10)
                                                   ),
                                                   shiny::wellPanel(
                                                     shiny::radioButtons("qc_radio","Histogram choices",
                                                                  choices = list("nCount_RNA", "nFeature_RNA", "percent.mt"),
                                                                  selected = "nCount_RNA"),
                                                     sliderInput(inputId = "histo_breaks",
                                                                 label = "Histogram breaks",
                                                                 value = 5000, min = 0, max = 10000, step=100),
                                                     sliderInput(inputId = "histo_xmax",
                                                                 label = "Histogram limits",
                                                                 value = 2000, min = 0, max = 3000, step=100)
                                                   ),
                                                   shiny::actionButton(inputId = "qc_start", label = "QC plot")
                                          ),
                                          shiny::tabPanel(title = "Spatial",
                                                   shiny::wellPanel(
                                                     sliderInput(inputId = "sp_nCount_Spatial",
                                                                 label = "nCount_Spatial: threshold",
                                                                 value = 100, min = 0, max = 1000, step=10),
                                                     sliderInput(inputId = "sp_percent.mt",
                                                                 label = "percent.mt: threshold",
                                                                 value = 5, min = 0, max = 100, step=1),
                                                     sliderInput(inputId = "sp_nFeature_Spatial",
                                                                 label = "nFeature_RNA: threshold",
                                                                 value = 300, min = 0, max = 1000, step=10)
                                                   ),
                                                   shiny::wellPanel(
                                                     shiny::radioButtons("qc_sp_radio","Histogram & Visualization choices",
                                                                  choices = list("nCount_Spatial", "nFeature_Spatial", "percent.mt"),
                                                                  selected = "nCount_Spatial"),
                                                     sliderInput(inputId = "histo_sp_breaks",
                                                                 label = "Histogram breaks",
                                                                 value = 5000, min = 0, max = 10000, step=100),
                                                     sliderInput(inputId = "histo_sp_xmax",
                                                                 label = "Histogram limits",
                                                                 value = 2000, min = 0, max = 3000, step=100),
                                                     sliderInput(inputId = "tissue_qc_minmax",
                                                                 label = "Mapping on tissue: minmax",
                                                                 value = c(0,100), min = 0, max = 100, step=100)
                                                   ),
                                                   shiny::actionButton(inputId = "qc_sp_start", label = "QC plot"),
                                                   shiny::actionButton(inputId = "horizontal_flip_start", label = "Flip image: horizontal"),
                                                   shiny::actionButton(inputId = "vertical_flip_start", label = "Flip image: vertical")
                                          )
                              )
                            ),
                            shiny::mainPanel(
                              shiny::conditionalPanel(
                                condition = "input.qc_tabset=='Directory' & input.qc_data_type=='Single-cell'",
                                shiny::plotOutput("sc_qc_vlnplot") %>% shinycssloaders::withSpinner(color="#0000FF")
                              ),
                              shiny::conditionalPanel(
                                condition = "input.qc_tabset=='Directory' & input.qc_data_type=='Spatial'",
                                shiny::plotOutput("sp_qc_vlnplot") %>% shinycssloaders::withSpinner(color="#0000FF")
                              ),
                              shiny::conditionalPanel(
                                condition = "input.qc_tabset=='Single-cell' & input.qc_data_type=='Single-cell'",
                                shiny::plotOutput("sc_feat_scatter") %>% shinycssloaders::withSpinner(color="#0000FF"),
                                shiny::plotOutput("sc_hist") %>% shinycssloaders::withSpinner(color="#0000FF")
                              ),
                              shiny::conditionalPanel(
                                condition = "input.qc_tabset=='Spatial' & input.qc_data_type=='Spatial'",
                                shiny::tabsetPanel(
                                  shiny::tabPanel("Plot", shiny::plotOutput("sp_feat_scatter") %>% shinycssloaders::withSpinner(color="#0000FF"),
                                           shiny::plotOutput("sp_hist") %>% shinycssloaders::withSpinner(color="#0000FF")),
                                  shiny::tabPanel("Tissue", shiny::plotOutput("sp_tissue") %>% shinycssloaders::withSpinner(color="#0000FF")
                                  )
                                )
                              )
                            )
                          )
                 ),
                 shiny::tabPanel(title = "Preprocessing",
                          shiny::titlePanel("Preprocessing single or multiple datasets"),
                          shiny::mainPanel(
                            shiny::wellPanel(
                              shinyFiles::shinyDirButton('dir_integ', 'Integration directory',
                                             'Search', multiple=TRUE),
                              shiny::verbatimTextOutput("dir_integ", placeholder = TRUE),
                              shiny::actionButton(inputId = "check_files", label = "Check Files"),
                              shiny::verbatimTextOutput("dir_list", placeholder = TRUE),
                              shiny::radioButtons("preproc_radio","Data format",
                                           choices = list("Single-cell","Spatial"),
                                           selected = "Single-cell"),
                              shiny::selectInput("dir_integ_sel","Select files to include",
                                          choices = "", multiple=TRUE),
                              shiny::selectInput(inputId = "ref_index",
                                          label = "Select files for reference dataset (blank for NULL)",
                                          choices = "", multiple=TRUE),
                              shiny::textInput(inputId = "grp_name",
                                        label = "Names for each data: separated by comma, no space",
                                        value="")
                            ),
                            shiny::conditionalPanel(
                              condition = "input.preproc_radio == 'Single-cell'",
                              shiny::wellPanel(
                                shiny::textInput(inputId = "nFeature_RNA_thres",
                                          label = "Lower thresholds for total number of genes in a cell: separated by comma, no space",
                                          value=""),
                                shiny::textInput(inputId = "percent_mt_thres",
                                          label = "Upper thresholds for mitochondrial gene % in a cell: separated by comma, no space",
                                          value="")
                              )
                            ),
                            shiny::wellPanel(
                              sliderInput(inputId = "n_var_features",
                                          label = "Number of HVGs",
                                          value = 2000, min = 1000, max = 5000, step=10),
                              sliderInput(inputId = "n_integ_features",
                                          label = "Number of genes in integration",
                                          value = 2000, min = 1000, max = 5000, step=10),
                              sliderInput(inputId = "integ_dim",
                                          label = "Number of dimensions used in integration",
                                          value = 50, min = 0, max = 100, step=1),
                              sliderInput(inputId = "cluster_dim",
                                          label = "Number of dimension in clustering",
                                          value = 30, min = 0, max = 50, step=1),
                              sliderInput(inputId = "cluster_resolution",
                                          label = "Clustering resolution",
                                          value = 0.3, min = 0, max = 1, step=0.05),
                              shiny::actionButton(inputId = "integ_start", label = "Integration start")
                            )
                          )
                 ),
                 shiny::navbarMenu(title = "Visualization",
                            shiny::tabPanel(title = "Dimension & Freq. plot",
                                     shiny::titlePanel("Visualize clusters"),
                                     shiny::sidebarLayout(
                                       shiny::sidebarPanel(
                                         shiny::tabsetPanel(id = "cluster_tabset",
                                                     shiny::tabPanel("Dimplot", value = "Dimplot",
                                                              shiny::wellPanel(
                                                                shiny::radioButtons("dimplot_radio","Data format",
                                                                             choices = list("Single-cell","Spatial"),
                                                                             selected = "Single-cell"),
                                                                shiny::selectInput("sc_group_var","Group name",
                                                                            c("orig.ident" = "orig.ident",
                                                                              "seurat_clusters" = "seurat_clusters"),
                                                                            selected = "orig.ident"),
                                                                shiny::textInput(inputId = "sc_vis_title",
                                                                          label = "Title of the plot",
                                                                          value="Cluster plot"),
                                                                sliderInput(inputId = "sc_dot_size",
                                                                            label = "Size of the dot",
                                                                            value = 0, min = 0, max = 2, step=0.05),
                                                                shiny::checkboxInput("sc_vis_label","Label on", value = TRUE),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.sc_vis_label==true",
                                                                  shiny::wellPanel(
                                                                    sliderInput(inputId = "sc_label_size",
                                                                                label = "Size of the label text",
                                                                                value = 4, min = 0, max = 10, step=1)
                                                                  )
                                                                ),
                                                                shiny::checkboxInput("sc_cell_high","Cell highlight", value = FALSE),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.sc_cell_high==true",
                                                                  shiny::wellPanel(
                                                                    shiny::textInput(inputId = "sc_cell_high_color",
                                                                              label = "Cell highlight color",
                                                                              value= "#DE2D26"),
                                                                    shiny::selectInput("sc_cell_high_cluster","Choose clusters to highlight:","",
                                                                                multiple=TRUE)
                                                                  )
                                                                ),
                                                                shiny::actionButton(inputId = "sc_vis_clust_start", label = "Plot"),
                                                                shiny::actionButton("sc_clust_save","Save")
                                                              )
                                                     ),
                                                     shiny::tabPanel("Frequency", value = "Freq",
                                                              shiny::wellPanel(
                                                                shiny::radioButtons("freqplot_radio","Data format",
                                                                             choices = list("Single-cell","Spatial"),
                                                                             selected = "Single-cell"),
                                                                shiny::selectInput("sc_group_var_freq","Group name (x-axis)",
                                                                            c("orig.ident" = "orig.ident",
                                                                              "seurat_clusters" = "seurat_clusters",
                                                                              "cluster" = "cluster"),
                                                                            selected = "orig.ident"),
                                                                shiny::selectInput("sc_cluster_var_freq","Cluster name (y-axis)",
                                                                            c("orig.ident" = "orig.ident",
                                                                              "seurat_clusters" = "seurat_clusters"),
                                                                            selected = "seurat_clusters"),
                                                                shiny::textInput(inputId = "sc_freq_x_title",
                                                                          label = "x-axis title",
                                                                          value= "Groups"),
                                                                shiny::textInput(inputId = "sc_freq_y_title",
                                                                          label = "y-axis title",
                                                                          value= "Frequency"),
                                                                shiny::checkboxInput("sc_check_x_axis_text", "x-axis text", value=FALSE),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.sc_check_x_axis_text==true",
                                                                  sliderInput(inputId = "sc_freq_x_angle",
                                                                              label = "Angle of x-axis text",
                                                                              value = 0, min=0, max=90, step=5),
                                                                  sliderInput(inputId = "sc_freq_x_size",
                                                                              label = "Size of x-axis text",
                                                                              value = 12, min=0, max=30, step=1)
                                                                ),
                                                                shiny::checkboxInput("sc_freq_label","Visualize freq", value = FALSE),
                                                                shiny::actionButton(inputId = "sc_vis_freq_start", label = "Plot"),
                                                                shiny::actionButton("sc_freq_save","Save")
                                                              )
                                                     )
                                         )
                                       ),
                                       shiny::mainPanel(
                                         shiny::conditionalPanel(
                                           condition = "input.cluster_tabset == 'Dimplot'",
                                           shiny::plotOutput("sc_dimplot") %>% shinycssloaders::withSpinner(color="#0000FF")
                                         ),
                                         shiny::conditionalPanel(
                                           condition = "input.cluster_tabset == 'Freq'",
                                           shiny::tabsetPanel(
                                             shiny::tabPanel("Plot", shiny::plotOutput("sc_freqplot") %>% shinycssloaders::withSpinner(color="#0000FF")),
                                             shiny::tabPanel("Table", shiny::dataTableOutput("sc_freqstats") %>% shinycssloaders::withSpinner(color="#0000FF"))
                                           )
                                         )
                                       )
                                     )
                            ),

                            shiny::tabPanel(title = "Feature plot",
                                     shiny::titlePanel("Visualize gene expression"),
                                     shiny::sidebarLayout(
                                       shiny::sidebarPanel(
                                         shiny::radioButtons("featureplot_radio","Data format",
                                                      choices = list("Single-cell","Spatial"),
                                                      selected = "Single-cell"),
                                         shiny::conditionalPanel(
                                           condition = "input.sc_check_metadata==true",
                                           shiny::selectInput(inputId = "sc_vis_metadata",
                                                       label = "Choose metadata to visualize",
                                                       choices = "", multiple = TRUE)
                                         ),
                                         shiny::conditionalPanel(
                                           condition = "input.sc_check_metadata==false",
                                           shiny::selectInput("sc_check_saved_genes","Show saved gene list",
                                                       choice = c("All","Top 1001~2000","Top 2001~3000"),
                                                       multiple=FALSE, selected="All"),
                                           shiny::selectizeInput(inputId = "sc_vis_feat",
                                                          label = "Choose genes to visualize",
                                                          choices="", multiple = TRUE)
                                         ),
                                         shiny::checkboxInput("sc_check_metadata","Show metadata list", value = FALSE),
                                         sliderInput(inputId = "sc_feat_minmax",
                                                     label = "Min max value",
                                                     value = c(0,3), min = 0, max = 10, step=0.1),
                                         shiny::textInput(inputId = "sc_feat_color",
                                                   label = "Color",
                                                   value= "blue"),
                                         numericInput(inputId = "sc_feat_ncol",
                                                      label = "Number of columns",
                                                      value = 2, min = 1, max=10),
                                         shiny::actionButton(inputId = "sc_vis_feat_start", label = "Plot"),
                                         shiny::actionButton("sc_feat_save","Save")
                                       ),
                                       shiny::mainPanel(
                                         shiny::plotOutput("sc_featplot") %>% shinycssloaders::withSpinner(color="#0000FF")
                                       )
                                     )
                            ),

                            shiny::tabPanel(title = "Spatial cluster plot",
                                     shiny::titlePanel("Visualize spatial clusters"),
                                     shiny::sidebarLayout(
                                       shiny::sidebarPanel(
                                         shiny::selectInput(inputId = "sp_clust_cluster_name",
                                                     label = "Group name",
                                                     choices = c("orig.ident","seurat_clusters"),
                                                     selected = "seurat_clusters"),
                                         shiny::checkboxInput("sp_check_cluster_transparency","Transparency", value = FALSE),
                                         shiny::conditionalPanel(
                                           condition = "input.sp_check_cluster_transparency==true",
                                           shiny::wellPanel(
                                             sliderInput(inputId = "sp_cluster_alpha",
                                                         label = "Transparency of spots",
                                                         value = c(1,1), min=0, max=1, step=0.1),
                                             sliderInput(inputId = "sp_cluster_image.alpha",
                                                         label = "Transparency of tissue",
                                                         value = 0.6, min=0, max=1, step=0.1)
                                           )
                                         ),

                                         shiny::checkboxInput("sp_check_cluster_vis","Visualization", value = FALSE),
                                         shiny::conditionalPanel(
                                           condition = "input.sp_check_cluster_vis==true",
                                           shiny::wellPanel(
                                             numericInput(inputId = "sp_cluster_ncol",
                                                          label = "Number of columns",
                                                          value = 4, min = 1, max=20),
                                             shiny::checkboxInput("sp_cluster_label","Label on", value=TRUE),
                                             shiny::conditionalPanel(
                                               condition = "input.sp_cluster_label==true",
                                               shiny::wellPanel(
                                                 sliderInput(inputId = "sp_cluster_label_size",
                                                             label = "Size of the label",
                                                             value = 3, min=0, max=10, step = 1)
                                               )
                                             ),
                                             sliderInput(inputId = "sp_cluster_pt.size.factor",
                                                         label = "Size of the spot",
                                                         value = 1.6, min=0, max=3, step = 0.1),
                                             sliderInput(inputId = "sp_cluster_img_width",
                                                         label = "Width of image panel",
                                                         value = 1000, min=0, max=2000, step=10),
                                             sliderInput(inputId = "sp_cluster_img_height",
                                                         label = "Height of image panel",
                                                         value = 300, min=0, max=2000, step=10),

                                             shiny::checkboxInput("sp_cluster_crop","Crop the image", value=TRUE)
                                           )
                                         ),
                                         shiny::checkboxInput("sp_check_cluster_subset","Subset the spots", value = FALSE),
                                         shiny::conditionalPanel(
                                           condition = "input.sp_check_cluster_subset==true",
                                           shiny::wellPanel(
                                             shiny::selectInput("sp_clust_slide.to.visualize","Choose slide to visualize","",
                                                         multiple=TRUE),
                                             shiny::selectInput("sp_clust_spot.cluster.highlight","Choose clusters to subset","",
                                                         multiple=TRUE)
                                           )
                                         ),
                                         shiny::actionButton(inputId = "sp_vis_cluster_start", label = "Plot"),
                                         shiny::actionButton("sp_cluster_save","Save")
                                       ),
                                       shiny::mainPanel(
                                         shiny::plotOutput("sp_clusterplot") %>% shinycssloaders::withSpinner(color="#0000FF")
                                       )
                                     )
                            ),

                            shiny::tabPanel(title = "Spatial feature plot",
                                     shiny::titlePanel("Visualize spatial gene expression"),
                                     shiny::sidebarLayout(
                                       shiny::sidebarPanel(
                                         shiny::conditionalPanel(
                                           condition = "input.sp_check_metadata==true",
                                           shiny::selectInput(inputId = "sp_vis_metadata",
                                                       label = "Choose metadata to visualize",
                                                       choices = "", multiple=TRUE)
                                         ),
                                         shiny::conditionalPanel(
                                           condition = "input.sp_check_metadata==false",
                                           shiny::selectInput("sp_check_saved_genes","Show saved gene list",
                                                       choice = c("All","Top 1001~2000","Top 2001~3000"),
                                                       multiple=FALSE, selected="All"),
                                           shiny::selectizeInput(inputId = "sp_vis_feat",
                                                          label = "Choose genes to visualize",
                                                          choices = "", multiple=TRUE)
                                         ),
                                         shiny::checkboxInput("sp_check_metadata","Show metadata list", value = FALSE),
                                         sliderInput(inputId = "sp_feat_minmax",
                                                     label = "Min max value",
                                                     value = c(0,3), min = 0, max = 10, step=0.1),
                                         shiny::checkboxInput("sp_check_feat_transparency","Transparency", value = FALSE),
                                         shiny::conditionalPanel(
                                           condition = "input.sp_check_feat_transparency==true",
                                           shiny::wellPanel(
                                             sliderInput(inputId = "sp_feat_alpha",
                                                         label = "Transparency of spots",
                                                         value = c(1,1), min=0, max=1, step=0.1),
                                             sliderInput(inputId = "sp_feat_image.alpha",
                                                         label = "Transparency of tissue",
                                                         value = 0.6, min=0, max=1, step=0.1)
                                           )
                                         ),
                                         shiny::checkboxInput("sp_check_feat_color","Colorbar", value = FALSE),
                                         shiny::conditionalPanel(
                                           condition = "input.sp_check_feat_color==true",
                                           shiny::wellPanel(
                                             shiny::selectInput(inputId = "sp_feat_color",
                                                         label = "Color palette",
                                                         choices = c(rownames(RColorBrewer::brewer.pal.info),
                                                                     names(colormap::colormaps)),
                                                         selected= "RdPu"),
                                             shiny::radioButtons("sp_color_bar_mode","Colorbar mode",
                                                          choices = list("default","combined"),
                                                          selected = "combined"),
                                             shiny::radioButtons("sp_color_bar_loc","Colorbar location",
                                                          choices = list("top","bottom","left","right"),
                                                          selected = "bottom")
                                           )
                                         ),
                                         shiny::checkboxInput("sp_check_feat_vis","Visualization", value = FALSE),
                                         shiny::conditionalPanel(
                                           condition = "input.sp_check_feat_vis==true",
                                           shiny::wellPanel(
                                             numericInput(inputId = "sp_feat_ncol",
                                                          label = "Number of columns",
                                                          value = 4, min = 1, max=20),
                                             numericInput("sp_title_size", "Size of the title", value=10, min=0),
                                             sliderInput(inputId = "sp_feat_pt.size.factor",
                                                         label = "Size of the spot",
                                                         value = 1.8, min=0, max=3, step = 0.1),
                                             sliderInput(inputId = "sp_feat_img_width",
                                                         label = "Width of image panel",
                                                         value = 900, min=0, max=2000, step=10),
                                             sliderInput(inputId = "sp_feat_img_height",
                                                         label = "Height of image panel",
                                                         value = 550, min=0, max=2000, step=10),
                                             shiny::checkboxInput("sp_feat_crop","Crop the image", value=TRUE)
                                           )
                                         ),
                                         shiny::checkboxInput("sp_check_feat_subset","Subset the spots", value = FALSE),
                                         shiny::conditionalPanel(
                                           condition = "input.sp_check_feat_subset==true",
                                           shiny::wellPanel(
                                             shiny::selectInput("sp_slide.to.visualize","Choose slide to visualize","",
                                                         multiple=TRUE),
                                             shiny::selectInput("sp_cluster_name","Group name",
                                                         c("orig.ident","seurat_clusters")),
                                             shiny::selectInput("sp_spot.cluster.highlight","Choose clusters to subset","",
                                                         multiple=TRUE)
                                           )
                                         ),
                                         shiny::actionButton(inputId = "sp_vis_feat_start", label = "Plot"),
                                         shiny::actionButton("sp_feat_save","Save")
                                       ),
                                       shiny::mainPanel(
                                         shiny::tabsetPanel(
                                           shiny::tabPanel("Plot", shiny::plotOutput("sp_featplot") %>% shinycssloaders::withSpinner(color="#0000FF")),
                                           shiny::tabPanel("Save multiple",
                                                    shiny::wellPanel(
                                                      shiny::wellPanel(
                                                        shiny::fileInput("sp_upload_csv", "Upload csv file of gene list",
                                                                  accept = c(
                                                                    "text/csv",
                                                                    "text/comma-separated-values,text/plain",
                                                                    ".csv")
                                                        ),
                                                        shiny::checkboxInput("sp_header_check", "Header", TRUE),
                                                        shiny::selectInput("sp_column_select","Select columns of gene list",
                                                                    "",multiple=FALSE)
                                                      ),
                                                      numericInput(inputId = "sp_feat_by_n",
                                                                   label = "Number of features to save in each plot",
                                                                   value = 2, min = 0, max = 10),
                                                      shiny::textInput("sp_save_name", "Name of the file", value='feats'),
                                                      sliderInput(inputId = "sp_save_by_n_width",
                                                                  label = "Width in cm",
                                                                  value = 18, min=0, max=50, step=1),
                                                      sliderInput(inputId = "sp_save_by_n_height",
                                                                  label = "Height in cm",
                                                                  value = 11, min=0, max=50, step=1),
                                                      sliderInput(inputId = "sp_upload_save_dpi",
                                                                  label = "dpi to save image",
                                                                  value = 100, min = 0, max=500, step=10),
                                                      shiny::actionButton("sp_feat_save_by_n", "Save multiple plots by n"),
                                                      shiny::actionButton("sp_feat_upload","Save gene list")
                                                    ),
                                                    shiny::wellPanel(
                                                      shiny::dataTableOutput("sp_csv_table") %>% shinycssloaders::withSpinner(color="#0000FF")
                                                    )
                                           )
                                         )
                                       )
                                     )
                            ),
                            shiny::tabPanel(title = "Violin plot",
                                     shiny::titlePanel("Violin plot visualization"),
                                     shiny::sidebarLayout(
                                       shiny::sidebarPanel(
                                         shiny::radioButtons("vlnplot_radio","Data format",
                                                      choices = list("Single-cell","Spatial"),
                                                      selected = "Single-cell"),
                                         shiny::conditionalPanel(
                                           condition = "input.vln_check_metadata==true",
                                           shiny::selectInput(inputId = "vln_vis_metadata",
                                                       label = "Choose metadata to visualize",
                                                       choices = "", multiple = TRUE)
                                         ),
                                         shiny::conditionalPanel(
                                           condition = "input.vln_check_metadata==false",
                                           shiny::selectInput("vln_check_saved_genes","Show saved gene list",
                                                       choice = c("All","Top 1001~2000","Top 2001~3000"),
                                                       multiple=FALSE, selected="All"),
                                           shiny::selectizeInput(inputId = "vln_vis_feat",
                                                          label = "Choose genes to visualize",
                                                          choices = "", multiple = TRUE)
                                         ),
                                         shiny::checkboxInput("vln_check_metadata","Show metadata list", value = FALSE),
                                         shiny::selectInput("vln_color_group","Color group",
                                                     c("orig.ident" = "orig.ident",
                                                       "seurat_clusters" = "seurat_clusters",
                                                       "cluster" = "cluster"),
                                                     selected = "orig.ident"),
                                         shiny::checkboxInput("vln_check_facet","Facet plot", value = FALSE),
                                         shiny::conditionalPanel(
                                           condition = "input.vln_check_facet==true",
                                           shiny::selectInput("vln_facet_group","Facet group",
                                                       c("orig.ident" = "orig.ident",
                                                         "seurat_clusters" = "seurat_clusters"))
                                         ),
                                         shiny::checkboxInput("vln_check_vis","Visualization", value = FALSE),
                                         shiny::conditionalPanel(
                                           condition = "input.vln_check_vis==true",
                                           shiny::wellPanel(
                                             sliderInput(inputId = "vln_img_width",
                                                         label = "Width of image panel",
                                                         value = 1000, min=0, max=2000, step=10),
                                             sliderInput(inputId = "vln_img_height",
                                                         label = "Height of image panel",
                                                         value = 800, min=0, max=2000, step=10),
                                             numericInput(inputId = "vln_ncol",
                                                          label = "Number of columns",
                                                          value = 1, min = 1, max=10)
                                           )
                                         ),
                                         shiny::actionButton(inputId = "vln_start", label = "Plot"),
                                         shiny::actionButton("vln_save","Save")
                                       ),
                                       shiny::mainPanel(
                                         shiny::plotOutput("vlnplot") %>% shinycssloaders::withSpinner(color="#0000FF")
                                       )
                                     )
                            ),
                            shiny::tabPanel(title = "Ridge plot",
                                     shiny::titlePanel("Ridge plot visualization"),
                                     shiny::sidebarLayout(
                                       shiny::sidebarPanel(
                                         shiny::radioButtons("ridgeplot_radio","Data format",
                                                      choices = list("Single-cell","Spatial"),
                                                      selected = "Single-cell"),
                                         shiny::conditionalPanel(
                                           condition = "input.ridge_check_metadata==true",
                                           shiny::selectInput(inputId = "ridge_vis_metadata",
                                                       label = "Choose metadata to visualize",
                                                       choices = "", multiple = TRUE)
                                         ),
                                         shiny::conditionalPanel(
                                           condition = "input.ridge_check_metadata==false",
                                           shiny::selectInput("ridge_check_saved_genes","Show saved gene list",
                                                       choice = c("All","Top 1001~2000","Top 2001~3000"),
                                                       multiple=FALSE, selected="All"),
                                           shiny::selectizeInput(inputId = "ridge_vis_feat",
                                                          label = "Choose genes to visualize",
                                                          choices = "", multiple = TRUE)
                                         ),
                                         shiny::checkboxInput("ridge_check_metadata","Show metadata list", value = FALSE),
                                         shiny::selectInput("ridge_vis_group", label = "Group by",
                                                     c("orig.ident" = "orig.ident",
                                                       "seurat_clusters" = "seurat_clusters",
                                                       "cluster" = "cluster"),
                                                     selected = "orig.ident"),
                                         sliderInput(inputId = "ridge_feat_minmax",
                                                     label = "Min max value",
                                                     value = c(-0.5,6), min = -1, max = 10, step=0.1),
                                         sliderInput(inputId = "ridge_alpha",
                                                     label = "Transparency of filled color",
                                                     value = 0, min=0, max=1, step=0.1),
                                         shiny::checkboxInput("ridge_check_vis","Visualization", value = FALSE),
                                         shiny::conditionalPanel(
                                           condition = "input.ridge_check_vis==true",
                                           shiny::wellPanel(
                                             sliderInput(inputId = "ridge_img_width",
                                                         label = "Width of image panel",
                                                         value = 1000, min=0, max=2000, step=10),
                                             sliderInput(inputId = "ridge_img_height",
                                                         label = "Height of image panel",
                                                         value = 800, min=0, max=2000, step=10),
                                             numericInput(inputId = "ridge_feat_ncol",
                                                          label = "Number of columns",
                                                          value = 2, min = 1, max=10)
                                           )
                                         ),
                                         shiny::actionButton(inputId = "ridge_feat_start", label = "Plot"),
                                         shiny::actionButton("ridge_save","Save")
                                       ),
                                       shiny::mainPanel(
                                         shiny::plotOutput("ridgeplot") %>% shinycssloaders::withSpinner(color="#0000FF")
                                       )
                                     )
                            )
                 ),

                 shiny::navbarMenu(title = "Marker & DEG",
                            shiny::tabPanel(title = "Marker",
                                     shiny::titlePanel("Marker genes for clusters"),
                                     shiny::sidebarLayout(
                                       shiny::sidebarPanel(
                                         shiny::radioButtons("marker_radio","Data format",
                                                      choices = list("Single-cell","Spatial"),
                                                      selected = "Single-cell"),
                                         shiny::radioButtons("check_marker_mode","Calculation mode",
                                                      choices = list("Wilcoxon", "NSForest"),
                                                      selected = "Wilcoxon"),
                                         shiny::selectInput("sc_marker_group","Group name",
                                                     c("orig.ident" = "orig.ident",
                                                       "seurat_clusters" = "seurat_clusters"),
                                                     selected = "seurat_clusters"),
                                         shiny::conditionalPanel(
                                           condition = "input.check_marker_mode=='NSForest'",
                                           shiny::wellPanel(
                                             sliderInput(inputId = "sc_marker_rfTrees",
                                                         label = "Number of trees",
                                                         value = 1000, min=0, max=5000, step=100),
                                             sliderInput(inputId = "sc_median_exp_level",
                                                         label = "Median expression level for removing negative markers",
                                                         value = 0, min=-5, max=5, step=0.1),
                                             sliderInput(inputId = "sc_genes_to_testing",
                                                         label = "Number of binary genes to be tested by permutations",
                                                         value = 6, min=0, max=10, step=1),
                                             sliderInput(inputId = "sc_beta_value",
                                                         label = "f-beta weight",
                                                         value = 0.5, min=0, max=1, step=0.01)
                                           )
                                         ),
                                         shiny::conditionalPanel(
                                           condition = "input.check_marker_mode=='Wilcoxon'",
                                           shiny::wellPanel(
                                             shiny::textInput(inputId = "sc_marker_logfc_thres",
                                                       label = "LogFC threshold",
                                                       value = 1.0),
                                             shiny::textInput(inputId = "sc_marker_exp_thres",
                                                       label = "Cluster expression threshold",
                                                       value = 0),
                                             shiny::checkboxInput("sc_marker_posonly","Positive only", value = TRUE)
                                           )
                                         ),
                                         shiny::actionButton(inputId = "sc_marker_start", label = "Find"),
                                         shiny::actionButton(inputId = "sc_marker_upload", label = "Save genes")
                                       ),
                                       shiny::mainPanel(
                                         shiny::dataTableOutput("sc_marker_table") %>% shinycssloaders::withSpinner(color="#0000FF")
                                       )
                                     )
                            ),
                            shiny::tabPanel(title = "DEG",
                                     shiny::titlePanel("DEG between clusters"),
                                     shiny::sidebarLayout(
                                       shiny::sidebarPanel(
                                         shiny::radioButtons("DEG_radio","Data format",
                                                      choices = list("Single-cell","Spatial"),
                                                      selected = "Single-cell"),
                                         shiny::checkboxInput("sc_check_deg_subset","Subset dataset", value = FALSE),
                                         shiny::selectInput("sc_deg_group","Group name",
                                                     c("orig.ident" = "orig.ident",
                                                       "seurat_clusters" = "seurat_clusters"),
                                                     selected = "seurat_clusters"),
                                         shiny::conditionalPanel(
                                           condition = 'input.sc_check_deg_subset==false',
                                           shiny::selectInput("sc_deg_int","Choose idents of interest","",multiple=TRUE),
                                           shiny::selectInput("sc_deg_ref","Choose reference idents","",multiple=TRUE)
                                         ),
                                         shiny::conditionalPanel(
                                           condition = 'input.sc_check_deg_subset==true',
                                           shiny::selectInput("sc_deg_subset_sel","Choose clusters to subset",
                                                       choices = "", multiple=TRUE),
                                           shiny::selectInput("sc_deg_subset_group","Choose group for DEG",
                                                       c("orig.ident" = "orig.ident",
                                                         "seurat_clusters" = "seurat_clusters"),
                                                       selected = "orig.ident"),
                                           shiny::selectInput("sc_deg_subset_int","Choose idents of interest in a subset","",multiple=TRUE),
                                           shiny::selectInput("sc_deg_subset_ref","Choose reference idents in a subset","",multiple=TRUE)
                                         ),
                                         shiny::wellPanel(
                                           shiny::textInput(inputId = "sc_deg_logfc_thres",
                                                     label = "LogFC threshold",
                                                     value = 0),
                                           shiny::textInput(inputId = "sc_deg_exp_thres",
                                                     label = "Expression threshold (idents of interest)",
                                                     value = 0)
                                         ),
                                         shiny::actionButton(inputId = "sc_deg_start", label = "Find"),
                                         shiny::actionButton(inputId = "sc_deg_upload", label = "Save genes")
                                       ),
                                       shiny::mainPanel(
                                         shiny::dataTableOutput("sc_deg_table") %>% shinycssloaders::withSpinner(color="#0000FF")
                                       )
                                     )
                            ),
                            shiny::tabPanel(title = "Volcano plot",
                                     shiny::titlePanel("Volcano plot for DEG"),
                                     shiny::sidebarLayout(
                                       shiny::sidebarPanel(
                                         shiny::br(),
                                         shiny::textInput(inputId = "sc_deg_volcano_p",
                                                   label = "Adjusted P-value threshold", value= 1e-4),
                                         shiny::textInput(inputId = "sc_deg_volcano_logfc",
                                                   label = "LogFC threshold", value= 0.5),
                                         shiny::textInput(inputId = "sc_deg_volcano_x_title",
                                                   label = "x-axis title", value= "Log2 fold change"),
                                         shiny::textInput(inputId = "sc_deg_volcano_y_title",
                                                   label = "y-axis title", value= "-Log10 adj. p-value"),
                                         shiny::actionButton(inputId = "sc_deg_volcano_start", label = "Plot"),
                                         shiny::actionButton("sc_deg_volcano_save","Save plot"),
                                         shiny::actionButton(inputId = "sc_deg_volcano_upload", label = "Save genes")
                                       ),
                                       shiny::mainPanel(
                                         shiny::tabsetPanel(
                                           shiny::tabPanel("Plot", shiny::plotOutput("sc_deg_volcano") %>% shinycssloaders::withSpinner(color="#0000FF")),
                                           shiny::tabPanel("Table", shiny::dataTableOutput("sc_deg_filter_table") %>% shinycssloaders::withSpinner(color="#0000FF"))
                                         )
                                       )
                                     )
                            ),
                            shiny::tabPanel(title = "Enrichment",
                                     shiny::titlePanel("Enrichment analysis for DEG"),
                                     shiny::sidebarLayout(
                                       shiny::sidebarPanel(
                                         shiny::br(),
                                         shiny::radioButtons("sc_deg_enrich_type","Terms to use",
                                                      choices = list("GO","KEGG"),
                                                      selected = "GO"),
                                         shiny::radioButtons("sc_deg_enrich_species","Species",
                                                      choices = list("Mouse","Human"),
                                                      selected = "Mouse"),
                                         shiny::conditionalPanel(
                                           condition = "input.sc_deg_enrich_type=='GO'",
                                           shiny::radioButtons("sc_deg_enrich_terms","Ontology",
                                                        choices = list("BP","CC","MF"),
                                                        selected = "BP")
                                         ),
                                         shiny::textInput(inputId = "sc_deg_enrich_p",
                                                   label = "Adjusted P-value", value= 0.05),
                                         shiny::textInput(inputId = "sc_deg_enrich_logfc",
                                                   label = "LogFC (pos: upper, neg: lower)", value= 0.3),
                                         numericInput(inputId = "sc_deg_enrich_nshow",
                                                      label = "Number of terms to show",
                                                      value = 10, min = 1, max=50),
                                         shiny::actionButton(inputId = "sc_deg_enrich_start", label = "Plot"),
                                         shiny::actionButton("sc_deg_enrich_save","Save")
                                       ),
                                       shiny::mainPanel(
                                         shiny::tabsetPanel(
                                           shiny::tabPanel("Plot", shiny::plotOutput("sc_deg_enrich") %>% shinycssloaders::withSpinner(color="#0000FF")),
                                           shiny::tabPanel("Table", shiny::dataTableOutput("sc_deg_enrich_table") %>% shinycssloaders::withSpinner(color="#0000FF"))
                                         )
                                       )
                                     )
                            )
                 ),

                 shiny::navbarMenu(title = "Utility",
                            shiny::tabPanel(title = "Annotation",
                                     shiny::titlePanel("Annotation of cell or spot clusters"),
                                     shiny::mainPanel(
                                       shiny::wellPanel(
                                         shiny::radioButtons("annotation_radio","Data format",
                                                      choices = list("Single-cell","Spatial"),
                                                      selected = "Single-cell"),
                                         shiny::selectInput("annotation_group","Group name",
                                                     c("orig.ident" = "orig.ident",
                                                       "seurat_clusters" = "seurat_clusters")),
                                         shiny::h5(style = "font-family:San-serif; font-weight:bold",
                                            "Elements of the cluster to annotate"),
                                         shiny::verbatimTextOutput("cluster_members", placeholder = TRUE),
                                         shiny::textInput(inputId = "new_group_name",
                                                   label = "New group name", value = "cluster"),
                                         shiny::textInput(inputId = "annotation_labels",
                                                   label = "New names to annotate: separated by comma, no space"),
                                         shiny::checkboxInput("cluster_annotate_recode","Recode the group and save", value = TRUE),
                                         shiny::actionButton(inputId = "cluster_new_idents", label = "Annotate")
                                       )
                                     )
                            ),
                            shiny::tabPanel(title = "Module score",
                                     shiny::titlePanel("Generate module score"),
                                     shiny::sidebarLayout(
                                       shiny::sidebarPanel(
                                         shiny::radioButtons("module_score_radio","Data format",
                                                      choices = list("Single-cell","Spatial"),
                                                      selected = "Single-cell"),
                                         shiny::checkboxInput("module_score_csv_check","Import csv", value = FALSE),
                                         shiny::conditionalPanel(
                                           condition = "input.module_score_csv_check==true",
                                           shiny::wellPanel(
                                             shiny::fileInput("module_score_csv", "Upload csv file of gene list",
                                                       accept = c(
                                                         "text/csv",
                                                         "text/comma-separated-values,text/plain",
                                                         ".csv")
                                             ),
                                             shiny::checkboxInput("module_score_header", "Header", TRUE)
                                           ),
                                           shiny::wellPanel(
                                             shiny::selectInput("module_table_select","Select columns of gene list",
                                                         "",multiple=FALSE),
                                             shiny::actionButton(inputId = "module_score_table_apply", label = "Apply column"),
                                             shiny::actionButton(inputId = "module_score_gene_upload", label = "Save genes")
                                           )
                                         ),
                                         shiny::textInput(inputId = "module_score_name",
                                                   label = "Name of module score", value = "Gene set"),
                                         shiny::conditionalPanel(
                                           condition = "input.module_score_csv_check==false",
                                           shiny::selectInput("module_check_saved_genes","Show saved gene list",
                                                       choice = c("All","Top 1001~2000","Top 2001~3000"),
                                                       multiple=FALSE, selected="All"),
                                           shiny::selectizeInput(inputId = "module_score_list",
                                                          label = "Gene list for module score",
                                                          choices = "", multiple=TRUE)
                                         ),
                                         shiny::conditionalPanel(
                                           condition = "input.module_score_csv_check==true",
                                           shiny::textInput(inputId = "module_score_list_csv",
                                                     label = "Gene list for module score",
                                                     value = "")
                                         ),
                                         shiny::actionButton(inputId = "module_score_start", label = "Generate")
                                       ),
                                       shiny::mainPanel(
                                         shiny::dataTableOutput("module_score_table") %>% shinycssloaders::withSpinner(color="#0000FF")
                                       )
                                     )
                            ),
                            shiny::tabPanel(title = "Subset data",
                                     shiny::titlePanel("Subset the dataset"),
                                     shiny::mainPanel(
                                       shiny::wellPanel(
                                         shiny::radioButtons("subset_radio","Data format",
                                                      choices = list("Single-cell","Spatial"),
                                                      selected = "Single-cell"),
                                         shiny::selectInput("subset_group","Group for subsetting",
                                                     c("orig.ident" = "orig.ident",
                                                       "seurat_clusters" = "seurat_clusters")),
                                         shiny::selectInput("subset_list","Choose clusters to subset","",multiple=TRUE),
                                         shiny::checkboxInput("subset_recluster","Recluster",value=FALSE),
                                         shiny::conditionalPanel(
                                           condition = "input.subset_recluster==true",
                                           shiny::wellPanel(
                                             sliderInput(inputId = "recluster_n_var_features",
                                                         label = "Number of HVGs",
                                                         value = 2000, min = 1000, max = 5000, step=10),
                                             sliderInput(inputId = "recluster_n_integ_features",
                                                         label = "Number of genes in integration",
                                                         value = 2000, min = 1000, max = 5000, step=10),
                                             sliderInput(inputId = "recluster_integ_dim",
                                                         label = "Number of dimensions used in integration",
                                                         value = 50, min = 0, max = 100, step=1),
                                             sliderInput(inputId = "recluster_cluster_dim",
                                                         label = "Number of dimension in clustering",
                                                         value = 30, min = 0, max = 50, step=1),
                                             sliderInput(inputId = "recluster_cluster_resolution",
                                                         label = "Clustering resolution",
                                                         value = 0.3, min = 0, max = 1, step=0.05)
                                           )
                                         ),
                                         shiny::actionButton(inputId = "subset_start", label = "Subset")
                                       )
                                     )
                            ),
                            shiny::tabPanel(title = "Quantitation",
                                     shiny::titlePanel("Quantitation of values by regions"),
                                     shiny::sidebarLayout(
                                       shiny::sidebarPanel(
                                         shiny::wellPanel(
                                           shiny::checkboxInput("quantitation_agg_mode",
                                                         "Aggregate to boxplot (split by group and recode)",
                                                         value=FALSE),
                                           shiny::selectInput("quantitation_mode","Calculation mode",
                                                       c("mean", "sum",
                                                         "skewness", "kurtosis", "boxplot (spots)"="boxplot"),
                                                       selected = "mean"),
                                           shiny::selectInput("quantitation_comp_group","Comparison group",
                                                       c("orig.ident" = "orig.ident",
                                                         "seurat_clusters" = "seurat_clusters"),
                                                       selected = "orig.ident"),
                                           shiny::conditionalPanel(
                                             condition="input.quantitation_agg_mode==false",
                                             shiny::selectInput("quantitation_facet_group","Facet group",
                                                         c("orig.ident" = "orig.ident",
                                                           "seurat_clusters" = "seurat_clusters"))
                                           ),
                                           shiny::conditionalPanel(
                                             condition="input.quantitation_agg_mode==true",
                                             shiny::selectInput("quantitation_split_group","Split group (Go to 'Utility-Annotation' and define)",
                                                         c("orig.ident" = "orig.ident",
                                                           "seurat_clusters" = "seurat_clusters")),
                                             shiny::selectInput("quantitation_recode_group",
                                                         "Recode group (Go to 'Utility-Annotation' and define)",
                                                         ""),
                                             shiny::checkboxInput("quantitation_check_pairwise",
                                                           "Add pairwise statistics", value = TRUE),
                                             shiny::selectInput("quantitation_pairwise_stats",
                                                         "Choose multiple comparison correction method",
                                                         c("bonferroni","fdr","BH","none"), selected="fdr")
                                           )
                                         ),
                                         shiny::wellPanel(
                                           shiny::radioButtons("quantitation_cellf_mode","Select types of data",
                                                        choices = list("metadata","genes"),
                                                        selected = "metadata"),
                                           shiny::conditionalPanel(
                                             condition="input.quantitation_cellf_mode=='metadata'",
                                             shiny::selectInput("quantitation_cellf1","Metadata to quantify",
                                                         c(""), multiple = TRUE)
                                           ),
                                           shiny::conditionalPanel(
                                             condition="input.quantitation_cellf_mode=='genes'",
                                             shiny::selectInput("quantitation_check_saved_genes","Show saved gene list",
                                                         choice = c("All","Top 1001~2000","Top 2001~3000"),
                                                         multiple=FALSE, selected="All"),
                                             shiny::selectizeInput("quantitation_cellf2","Genes to quantify",
                                                            choices = "", multiple = TRUE)
                                           )
                                         ),
                                         shiny::textInput(inputId = "quantitation_name",
                                                   label = "Name of y-axis", value = "cell fraction"),
                                         shiny::checkboxInput("quantitation_vis_cellf","Visualize values on plot", value = FALSE),
                                         shiny::checkboxInput("spot_total_number","Visualize total spot number", value = FALSE),
                                         shiny::checkboxInput("quantitation_check_vis","Visualization options", value = FALSE),
                                         shiny::conditionalPanel(
                                           condition = "input.quantitation_check_vis==true",
                                           shiny::wellPanel(
                                             sliderInput(inputId = "quantitation_img_width",
                                                         label = "Width of image panel",
                                                         value = 1000, min=0, max=2000, step=10),
                                             sliderInput(inputId = "quantitation_img_height",
                                                         label = "Height of image panel",
                                                         value = 800, min=0, max=2000, step=10),
                                             numericInput(inputId = "quantitation_feat_ncol",
                                                          label = "Number of columns",
                                                          value = 4, min = 1, max=10)
                                           )
                                         ),
                                         shiny::actionButton(inputId = "quantitation_start", label = "Start"),
                                         shiny::actionButton("quantitation_save","Save")
                                       ),
                                       shiny::mainPanel(
                                         shiny::tabsetPanel(
                                           shiny::tabPanel("Plot", shiny::plotOutput("quantitation_plot") %>% shinycssloaders::withSpinner(color="#0000FF")),
                                           shiny::tabPanel("Table", shiny::dataTableOutput("quantitation_table") %>% shinycssloaders::withSpinner(color="#0000FF"))
                                         )
                                       )
                                     )
                            )
                 ),
                 shiny::tabPanel(title = "CellDART",
                          shiny::titlePanel("Prediction of spatial cell fraction"),
                          shiny::mainPanel(
                            shiny::wellPanel(
                              shiny::checkboxInput("celldart_check_subset","Subset spot cluster", value = FALSE),
                              shiny::conditionalPanel(
                                condition = "input.celldart_check_subset==true",
                                shiny::wellPanel(
                                  shiny::selectInput("celldart_group","Group to subset spots",
                                              c("orig.ident" = "orig.ident",
                                                "seurat_clusters" = "seurat_clusters"),
                                              selected = "seurat_clusters"),
                                  shiny::selectInput("celldart_group_sel","Select spot clusters to include",
                                              "", multiple=TRUE)
                                )
                              ),
                              shiny::selectInput("celldart_metadata_celltype","Group for classifying celltypes",
                                          c("orig.ident" = "orig.ident",
                                            "seurat_clusters" = "seurat_clusters"),
                                          selected = "seurat_clusters"),
                              shiny::wellPanel(
                                sliderInput(inputId = "celldart_num_markers",
                                            label = "Number of markers for each celltype",
                                            value = 10, min=1, max=50, step=1),
                                sliderInput(inputId = "celldart_nmix",
                                            label = "Number of cells in a pseudospot",
                                            value = 8, min=1, max=20, step=1),
                                sliderInput(inputId = "celldart_npseudo",
                                            label = "Number of pseudospots",
                                            value = 20000, min=10000, max=400000, step=1000),
                              ),
                              shiny::checkboxInput("celldart_train_param","Training parameters",value=FALSE),
                              shiny::conditionalPanel(
                                condition = "input.celldart_train_param==true",
                                shiny::wellPanel(
                                  sliderInput(inputId = "celldart_alpha",
                                              label = "Domain classifier loss weight",
                                              value = 0.6, min=0.1, max=10, step=0.1),
                                  sliderInput(inputId = "celldart_alpha_lr",
                                              label = "Domain classifier learning rate",
                                              value = 5, min=0.1, max=10, step=0.1),
                                  sliderInput(inputId = "celldart_emb_dim",
                                              label = "Feature embedder dimension",
                                              value = 64, min=2, max=512, step=2),
                                  sliderInput(inputId = "celldart_batch_size",
                                              label = "Feature embedder dimension",
                                              value = 512, min=64, max=1024, step=2),
                                  sliderInput(inputId = "celldart_n_iterations",
                                              label = "Number of iterations",
                                              value = 3000, min=1000, max=10000, step=100),
                                  sliderInput(inputId = "celldart_init_train_epoch",
                                              label = "Initial training number",
                                              value = 10, min=1, max=30, step=1)
                                )
                              ),
                              shiny::actionButton(inputId = "celldart_start", label = "Start")
                            )
                          )
                 )
)


#' Internal server function
#' @description Server function for STquantool
#' @keywords internal
server <- function(input,output,session){
  # Directory setting
  shinyFiles::shinyDirChoose(
    input,
    'dir',
    roots = c(home = '/home/nmadmin/'),
    filetypes = c('',"txt","tsv","csv","rds","png","h5","h5ad")
  )

  global <- shiny::reactiveValues(datapath = '/home/nmadmin/DATA1/Spatial')

  dir <- shiny::reactive(input$dir)

  output$dir <- shiny::renderText({
    global$datapath
  })

  shiny::observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$dir
               },
               handlerExpr = {
                 if (!"path" %in% names(dir())) return()
                 home <- normalizePath("/home/nmadmin")
                 global$datapath <-
                   file.path(home, paste(unlist(dir()$path[-1]),
                                         collapse = .Platform$file.sep))
               })

  # Set working directory
  shiny::observeEvent(input$set_wd, {
    v$setwd_count <- v$setwd_count + 1
    if (v$setwd_count==1){
      # save the original working directory for the first click
      v$orig_wd <- getwd()
    }
    setwd(global$datapath)

    output$cmd <- shiny::renderText({
      "Working directory was set"
    })
  })

  # Make output folder
  shiny::observeEvent(input$output_make,
               {
                 # Assign the name of directories for saving figures
                 if (!file.exists(input$output_folder_name)){
                   dir.create(file.path(global$datapath,input$output_folder_name))
                   fig_dir <- file.path(global$datapath,input$output_folder_name,'data_files')

                   if (!file.exists(fig_dir)){
                     dir.create(fig_dir)
                     output$cmd <- shiny::renderText({
                       "Output and data file folders were created"
                     })}

                 } else {
                   fig_dir <- file.path(global$datapath,input$output_folder_name,'data_files')
                   if (!file.exists(fig_dir)){
                     dir.create(fig_dir)
                     output$cmd <- shiny::renderText({
                       "Output folder already exists\nData file folder was created"
                     })} else{
                       output$cmd <- shiny::renderText({
                         "Output and data file folders already exist"
                       })
                     }
                 }
               })


  # Choose single-cell directory
  shinyFiles::shinyDirChoose(
    input,
    'qc_dir',
    roots = c(home = '/home/nmadmin', wd ='.'),
    filetypes = c('',"txt","tsv","csv","rds","png","h5","h5ad")
  )

  qc_dir <- shiny::reactive(input$qc_dir)

  qc_path <- shiny::eventReactive(input$qc_dir, {
    if (is.null(names(qc_dir()))){

    } else if (qc_dir()$root[[1]]=='wd'){
      file.path(paste(c(getwd(),unlist(qc_dir()$path[-1])),
                      collapse=.Platform$file.sep))
    } else if (qc_dir()$root[[1]]=='home') {
      home <- normalizePath("/home/nmadmin")
      file.path(paste(c(home,unlist(qc_dir()$path[-1])),
                      collapse=.Platform$file.sep))
    }
  }
  )

  output$qc_dir <- shiny::renderText({qc_path()})


  ## Define reactive values
  v <- shiny::reactiveValues()
  v$sc_data <- NULL
  v$sp_data <- NULL
  v$setwd_count <- 0

  # Set original working directory for app.R file
  # tmp <- rstudioapi::getSourceEditorContext()$path
  # v$orig_wd <- substring(tmp, 1, nchar(tmp)-5)

  ## Load single-cell or spatial data for qc
  shiny::observeEvent(input$dir_qc, {
    if (!is.null(qc_path())){
      shinybusy::show_modal_spinner()
      if (input$qc_data_type=='Single-cell'){
        if ("filtered_feature_bc_matrix" %in% list.files(qc_path())){
          data_path <- file.path(qc_path(), "filtered_feature_bc_matrix")
          if (sum(c("barcodes.tsv.gz","features.tsv.gz","matrix.mtx.gz") %in%
                  list.files(data_path))==3){
            counts <- Seurat::Read10X(data.dir = file.path(qc_path(), 'filtered_feature_bc_matrix'))
            v$sc_qc_data <- Seurat::CreateSeuratObject(counts)
            v$sc_qc_data <- Seurat::PercentageFeatureSet(v$sc_qc_data, pattern = "^mt-", col.name = "percent.mt")
          }
        } else if ("filtered_feature_bc_matrix.h5" %in% list.files(qc_path())){
          counts <- Seurat::Read10X_h5(file.path(qc_path(), 'filtered_feature_bc_matrix.h5'))
          v$sc_qc_data <- Seurat::CreateSeuratObject(counts)
          v$sc_qc_data <- Seurat::PercentageFeatureSet(v$sc_qc_data, pattern = "^mt-", col.name = "percent.mt")
        } else if ("sparse_matrix.rds" %in% list.files(qc_path())){
          counts <- readRDS(file.path(qc_path(), 'sparse_matrix.rds'))
          v$sc_qc_data <- Seurat::CreateSeuratObject(counts)
          v$sc_qc_data <- Seurat::PercentageFeatureSet(v$sc_qc_data, pattern = "^mt-", col.name = "percent.mt")
        }
      } else if (input$qc_data_type=='Spatial'){
        data_path_sp <- file.path(qc_path(), "spatial")
        if (sum(c("tissue_hires_image.png","tissue_lowres_image.png",
                  "scalefactors_json.json","tissue_positions_list.csv") %in%
                list.files(data_path_sp))==4 &
            'filtered_feature_bc_matrix.h5' %in% list.files(qc_path())){
          v$sp_qc_data <- Seurat::Load10X_Spatial(data.dir = file.path(qc_path()))
          v$sp_qc_data <- Seurat::PercentageFeatureSet(v$sp_qc_data, pattern = "^mt-", col.name = "percent.mt")
        }
      }
      shinybusy::remove_modal_spinner()
    }
  })

  # single-cell QC plot
  output$sc_qc_vlnplot <- shiny::renderPlot({
    if (!is.null(v$sc_qc_data)){
      v$sc_qc_vlnplot <- Seurat::VlnPlot(v$sc_qc_data,
                                         features = c("nFeature_RNA","nCount_RNA","percent.mt"),
                                         ncol = 3)
      v$sc_qc_vlnplot
    }
  })

  # spatial QC plot
  output$sp_qc_vlnplot <- shiny::renderPlot({
    if (!is.null(v$sp_qc_data)){
      v$sp_qc_vlnplot <- Seurat::VlnPlot(v$sp_qc_data,
                                         features = c("nFeature_Spatial","nCount_Spatial","percent.mt"),
                                         ncol = 3)
      v$sp_qc_vlnplot
    }
  })

  ## QC plot for single-cell
  # Save variables from slidebar and plot
  qc_list_sc <- shiny::eventReactive(input$qc_start, {
    list(input$nCount_RNA, input$percent.mt, input$nFeature_RNA,
         input$histo_breaks, input$histo_xmax, input$qc_radio, v$sc_qc_data)
  })

  output$sc_feat_scatter <- shiny::renderPlot({
    if (!is.null(qc_list_sc()[[7]])){
      temp <- qc_list_sc()[[7]]
      v$p1 <- Seurat::FeatureScatter(temp, feature1 = "nCount_RNA", feature2 = "percent.mt") +
        ggplot2::geom_hline(yintercept = qc_list_sc()[[2]]) + ggplot2::geom_vline(xintercept = qc_list_sc()[[1]])
      v$p2 <- Seurat::FeatureScatter(temp, feature1 = "nCount_RNA", feature2 = "nFeature_RNA") +
        ggplot2::geom_hline(yintercept = qc_list_sc()[[3]]) + ggplot2::geom_vline(xintercept = qc_list_sc()[[1]])
      v$p1 + v$p2
    }
  })
  output$sc_hist <- shiny::renderPlot({
    if (!is.null(qc_list_sc()[[7]])){
      temp <- qc_list_sc()[[7]]
      # Draw histogram for total count, nFeature_RNA, and percent mitochondrial genes
      if (qc_list_sc()[[6]] == "nCount_RNA"){
        grahics::hist(temp@meta.data$nCount_RNA, breaks=qc_list_sc()[[4]],
             xlim=c(0,qc_list_sc()[[5]]), main = "Histogram for nCount_RNA",
             xlab = "nCount_RNA")
      } else if (qc_list_sc()[[6]] == "nFeature_RNA"){
        grahics::hist(temp@meta.data$nFeature_RNA, breaks=qc_list_sc()[[4]],
             xlim=c(0,qc_list_sc()[[5]]), main = "Histogram for nFeature_RNA",
             xlab = "nFeature_RNA")
      } else if (qc_list_sc()[[6]] == "percent.mt"){
        grahics::hist(temp@meta.data$percent.mt, breaks=qc_list_sc()[[4]],
             xlim=c(0,qc_list_sc()[[5]]), main = "Histogram for percent.mt",
             xlab = "percent.mt")
      }
    }
  })

  ## QC plot for spatial
  qc_list_sp <- shiny::eventReactive(input$qc_sp_start, {
    list(input$sp_nCount_Spatial, input$sp_percent.mt, input$sp_nFeature_Spatial,
         input$histo_sp_breaks, input$histo_sp_xmax, input$qc_sp_radio,
         input$tissue_qc_minmax, v$sp_qc_data)
  })
  shiny::observeEvent(c(v$sp_qc_data, input$qc_sp_radio), {
    if (!is.null(v$sp_qc_data)){
      temp <- v$sp_qc_data
      val = eval(parse(text=paste0('temp$',input$qc_sp_radio)))
      sp.max = max(val)

      shiny::updateSliderInput(session, "tissue_qc_minmax",
                        label = "QC metric minmax",
                        value = c(0, sp.max),
                        min = 0, max = sp.max, step = sp.max/100)
    }
  })

  output$sp_feat_scatter <- shiny::renderPlot({
    if (!is.null(qc_list_sp()[[8]])){
      temp <- qc_list_sp()[[8]]
      v$p1 <- Seurat::FeatureScatter(temp, feature1 = "nCount_Spatial", feature2 = "percent.mt") +
        ggplot2::geom_hline(yintercept = qc_list_sp()[[2]]) + ggplot2::geom_vline(xintercept = qc_list_sp()[[1]])
      v$p2 <- Seurat::FeatureScatter(temp, feature1 = "nCount_Spatial", feature2 = "nFeature_Spatial") +
        ggplot2::geom_hline(yintercept = qc_list_sp()[[3]]) + ggplot2::geom_vline(xintercept = qc_list_sp()[[1]])
      v$p1 + v$p2
    }
  })
  output$sp_hist <- shiny::renderPlot({
    if (!is.null(qc_list_sp()[[8]])){
      temp <- qc_list_sp()[[8]]
      # Draw histogram for total count, nFeature_RNA, and percent mitochondrial genes
      if (qc_list_sp()[[6]] == "nCount_Spatial"){
        grahics::hist(temp@meta.data$nCount_Spatial, breaks=qc_list_sp()[[4]],
             xlim=c(0,qc_list_sp()[[5]]), main = "Histogram for nCount_Spatial",
             xlab = "nCount_Spatial")
      } else if (qc_list_sp()[[6]] == "nFeature_Spatial"){
        grahics::hist(temp@meta.data$nFeature_Spatial, breaks=qc_list_sp()[[4]],
             xlim=c(0,qc_list_sp()[[5]]), main = "Histogram for nFeature_Spatial",
             xlab = "nFeature_Spatial")
      } else if (qc_list_sp()[[6]] == "percent.mt"){
        grahics::hist(temp@meta.data$percent.mt, breaks=qc_list_sp()[[4]],
             xlim=c(0,qc_list_sp()[[5]]), main = "Histogram for percent.mt",
             xlab = "percent.mt")
      }
    }
  })
  output$sp_tissue <- shiny::renderPlot({
    if (!is.null(qc_list_sp()[[8]])){
      temp <- qc_list_sp()[[8]]
      # Draw histogram for total count, nFeature_RNA, and percent mitochondrial genes
      Seurat::SpatialFeaturePlot(temp, features = qc_list_sp()[[6]],
                                 min.cutoff = qc_list_sp()[[7]][1], max.cutoff = qc_list_sp()[[7]][2])
    }
  })

  shiny::observeEvent(input$horizontal_flip_start, {
    if (!is.null(qc_path())){
      shiny::showModal(flipModal(text="horizontally", input_name="ok_horizontal"))
    }
  })
  shiny::observeEvent(input$ok_horizontal, {
    horizontal_flip(qc_path())
    shiny::removeModal()
  })
  shiny::observeEvent(input$vertical_flip_start, {
    if (!is.null(qc_path())){
      shiny::showModal(flipModal(text="vertically", input_name="ok_vertical"))
    }
  })
  shiny::observeEvent(input$ok_vertical, {
    vertical_flip(qc_path())
    shiny::removeModal()
  })



  ## Load single-cell or spatial data for integration
  shinyFiles::shinyDirChoose(
    input,
    'dir_integ',
    roots = c(home = '/home/nmadmin', wd ='.'),
    filetypes = c('',"txt","tsv","csv","rds","png","h5","h5ad")
  )

  dir_integ <- shiny::reactive(input$dir_integ)

  integ_path <- shiny::eventReactive(input$dir_integ, {
    if (is.null(names(dir_integ()))){

    } else if (dir_integ()$root[[1]]=='wd'){
      file.path(paste(c(getwd(),unlist(dir_integ()$path[-1])),
                      collapse=.Platform$file.sep))
    } else if (dir_integ()$root[[1]]=='home') {
      home <- normalizePath("/home/nmadmin")
      file.path(paste(c(home,unlist(dir_integ()$path[-1])),
                      collapse=.Platform$file.sep))
    }
  }
  )

  output$dir_integ <- shiny::renderText({integ_path()})


  ## Load single-cell or spatial data and visualize the collected files
  dir_list <- shiny::eventReactive(input$check_files, {
    data_dir <- list()
    a <- list.dirs(integ_path(), recursive = TRUE)
    for (i in 1:length(a)){
      if (input$preproc_radio=='Spatial'){
        data_path_sp <- file.path(a[i], "spatial")
        if (sum(c("tissue_hires_image.png","tissue_lowres_image.png",
                  "scalefactors_json.json","tissue_positions_list.csv") %in%
                list.files(data_path_sp))==4 & 'filtered_feature_bc_matrix.h5' %in% list.files(a[i])){
          data_dir[[i]] <- a[i]
        }
      } else if (input$preproc_radio=='Single-cell') {
        if ("filtered_feature_bc_matrix" %in% list.files(a[i])){
          data_path <- file.path(a[i], "filtered_feature_bc_matrix")
          if (sum(c("barcodes.tsv.gz","features.tsv.gz","matrix.mtx.gz") %in%
                  list.files(data_path))==3){
            data_dir[[i]] <- a[i]
          }
        } else if ("filtered_feature_bc_matrix.h5" %in% list.files(a[i])){
          data_dir[[i]] <- a[i]
        } else if ("sparse_matrix.rds" %in% list.files(a[i])){
          data_dir[[i]] <- a[i]
        }
      }
    }
    unlist(plyr::compact(data_dir))
  })

  output$dir_list <- shiny::renderText({paste(dir_list(), collapse='\n')})

  # Defined customized UI function
  # https://github.com/rstudio/shiny/issues/518

  # Update select input according to resulting directories
  shiny::observeEvent(input$check_files, {
    shiny::updateSelectInput(session, "dir_integ_sel",
                      label = "Select files to include",
                      choices = dir_list())
    shiny::updateSelectInput(session, "ref_index",
                      label = "Select files for reference dataset (blank for NULL)",
                      choices = dir_list())
  })
  shiny::observeEvent(input$dir_integ_sel, {
    shiny::updateSelectInput(session, "ref_index",
                      label = "Select files for reference dataset (blank for NULL)",
                      choices = input$dir_integ_sel)
    dir_names <- as.vector(sapply(input$dir_integ_sel, function(x){
      utils::tail(strsplit(x, '/')[[1]],1)
    }))
    shiny::updateTextInput(session, "grp_name",
                    label = "Names for each data: separated by comma, no space",
                    value = dir_names)
    shiny::updateTextInput(session, "nFeature_RNA_thres",
                    label = "Lower thresholds for total number of genes in a cell: separated by comma, no space",
                    value = rep(0, length(input$dir_integ_sel)))
    shiny::updateTextInput(session, "percent_mt_thres",
                    label = "Upper thresholds for mitochondrial gene % in a cell: separated by comma, no space",
                    value = rep(100, length(input$dir_integ_sel)))
  })


  ## Start integration of single-cell or spatial data
  shiny::observeEvent(input$integ_start, {
    if (length(input$dir_integ_sel)==1){
      if (input$preproc_radio=='Single-cell'){
        shinybusy::show_modal_spinner()
        v$sc_data <- preprocess_data(data_dir=input$dir_integ_sel,
                                     grp=input$grp_name, data_type='Single-cell',
                                     filter_nfeature_RNA=input$nFeature_RNA_thres,
                                     filter_percent_mt=input$percent_mt_thres,
                                     n_var_features=input$n_var_features,
                                     cluster_dim=input$cluster_dim,
                                     cluster_resolution=input$cluster_resolution)
        shinybusy::remove_modal_spinner()
      } else if (input$preproc_radio=='Spatial'){
        shinybusy::show_modal_spinner()
        v$sp_data <- preprocess_data(data_dir=input$dir_integ_sel,
                                     grp=input$grp_name, data_type='Spatial',
                                     n_var_features=input$n_var_features,
                                     cluster_dim=input$cluster_dim,
                                     cluster_resolution=input$cluster_resolution)
        shinybusy::remove_modal_spinner()
      }
    } else if (length(input$dir_integ_sel)>1) {
      grp <- sapply(strsplit(input$grp_name, ",")[[1]],
                    function(x){as.character(x)})

      if (!is.null(input$ref_index)){
        reference <- match(input$ref_index, input$dir_integ_sel)
      } else {
        reference <- input$ref_index
      }

      if (input$preproc_radio=='Single-cell'){
        filter_nfeature_RNA <- sapply(strsplit(input$nFeature_RNA_thres, ",")[[1]],
                                      function(x){as.numeric(x)})
        filter_percent_mt <- sapply(strsplit(input$percent_mt_thres, ",")[[1]],
                                    function(x){as.numeric(x)})
        shinybusy::show_modal_spinner()
        v$sc_data <- preprocess_data_integ_rpca(data_dir=input$dir_integ_sel,
                                                grp=grp, data_type='Single-cell',
                                                filter_nfeature_RNA=filter_nfeature_RNA,
                                                filter_percent_mt=filter_percent_mt,
                                                reference_index=reference,
                                                n_var_features=input$n_var_features,
                                                n_integ_features=input$n_integ_features,
                                                integ_dim=input$integ_dim,
                                                cluster_dim=input$cluster_dim,
                                                cluster_resolution=input$cluster_resolution)
        shinybusy::remove_modal_spinner()
      } else if (input$preproc_radio=='Spatial') {
        shinybusy::show_modal_spinner()
        v$sp_data <- preprocess_data_integ_rpca(data_dir=input$dir_integ_sel,
                                                grp=grp, data_type='Spatial',
                                                reference_index=reference,
                                                n_var_features=input$n_var_features,
                                                n_integ_features=input$n_integ_features,
                                                integ_dim=input$integ_dim,
                                                cluster_dim=input$cluster_dim,
                                                cluster_resolution=input$cluster_resolution)
        shinybusy::remove_modal_spinner()
      }
    }

  })


  ## Update the metadata and genes of single-cell and spatial data
  shiny::observeEvent(v$sc_data, {
    shinybusy::show_modal_spinner()
    temp <- v$sc_data
    v$sc_gene_list <- c()
    v$sc_meta_list_factor <- c()
    v$sc_meta_list_value <- c()

    sel_list <- c()
    sel_list_inv <- c()

    if (!is.null(temp)){
      for (var in colnames(temp@meta.data)){
        if (!is.null(levels(eval(parse(text=paste0('temp$',var)))))|(var=="orig.ident")){
          sel_list <- c(sel_list, var)
        } else {
          sel_list_inv <- c(sel_list_inv, var)
        }
      }
      assay_sel <- intersect(names(temp@assays), c("Spatial","RNA"))
      total_exp_data <- Seurat::GetAssayData(temp, assay=assay_sel, slot='data')
      total_exp_data <- Matrix::rowSums(expm1(total_exp_data))
      v$sc_gene_list <- names(total_exp_data[order(total_exp_data, decreasing=TRUE)])

      v$sc_meta_list_factor <- sort(sel_list)
      v$sc_meta_list_value <- sort(sel_list_inv)
    }
    shinybusy::remove_modal_spinner()
  })

  shiny::observeEvent(v$sp_data, {
    shinybusy::show_modal_spinner()
    temp <- v$sp_data
    v$sp_gene_list <- c()
    v$sp_meta_list_factor <- c()
    v$sp_meta_list_value <- c()
    v$sp_spot_num <- c()

    sel_list <- c()
    sel_list_inv <- c()

    if (!is.null(temp)){
      for (var in colnames(temp@meta.data)){
        if (!is.null(levels(eval(parse(text=paste0('temp$',var)))))|(var=="orig.ident")){
          sel_list <- c(sel_list, var)
        } else {
          sel_list_inv <- c(sel_list_inv, var)
        }
      }
      assay_sel <- intersect(names(temp@assays), c("Spatial","RNA"))
      total_exp_data <- Seurat::GetAssayData(temp, assay=assay_sel, slot='data')
      total_exp_data <- Matrix::rowSums(expm1(total_exp_data))
      v$sp_gene_list <- names(total_exp_data[order(total_exp_data, decreasing=TRUE)])

      v$sp_meta_list_factor <- sort(sel_list)
      v$sp_meta_list_value <- sort(sel_list_inv)
      v$sp_spot_num <- dim(temp)[2]
    }
    shinybusy::remove_modal_spinner()
  })

  ## Update the gene list
  shiny::observeEvent(v$gene_upload, {
    sel_list <- names(v$gene_upload)
    shiny::updateSelectInput(session, "sc_check_saved_genes",
                      label = "Show saved gene list",
                      choices = c("All","Top 1001~2000","Top 2001~3000", sel_list), selected = "All")
    shiny::updateSelectInput(session, "sp_check_saved_genes",
                      label = "Show saved gene list",
                      choices = c("All","Top 1001~2000","Top 2001~3000", sel_list), selected = "All")
    shiny::updateSelectInput(session, "vln_check_saved_genes",
                      label = "Show saved gene list",
                      choices = c("All","Top 1001~2000","Top 2001~3000", sel_list), selected = "All")
    shiny::updateSelectInput(session, "ridge_check_saved_genes",
                      label = "Show saved gene list",
                      choices = c("All","Top 1001~2000","Top 2001~3000", sel_list), selected = "All")
    shiny::updateSelectInput(session, "module_check_saved_genes",
                      label = "Show saved gene list",
                      choices = c("All","Top 1001~2000","Top 2001~3000", sel_list), selected = "All")
    shiny::updateSelectInput(session, "quantitation_check_saved_genes",
                      label = "Show saved gene list",
                      choices = c("All","Top 1001~2000","Top 2001~3000", sel_list), selected = "All")
  })


  # Update the SelectInput function according to input: Dimplot and Frequency plot
  shiny::observeEvent(c(v$sc_data,v$sp_data,input$dimplot_radio), {
    if (input$dimplot_radio=="Single-cell"){
      sel_list <- v$sc_meta_list_factor
    } else if (input$dimplot_radio=="Spatial"){
      sel_list <- v$sp_meta_list_factor
    }
    shiny::updateSelectInput(session, "sc_group_var",
                      label = "Group name",
                      choices = sel_list)
  })

  shiny::observeEvent(c(v$sc_data,v$sp_data,input$freqplot_radio), {
    if (input$freqplot_radio=="Single-cell"){
      sel_list <- v$sc_meta_list_factor
    } else if (input$freqplot_radio=="Spatial"){
      sel_list <- v$sp_meta_list_factor
    }
    shiny::updateSelectInput(session, "sc_group_var_freq",
                      label = "Group name (x-axis)",
                      choices = sel_list)
    shiny::updateSelectInput(session, "sc_cluster_var_freq",
                      label = "Cluster name (y-axis)",
                      choices = sel_list)
  })

  shiny::observeEvent(c(input$sc_group_var,v$sc_data,v$sp_data), {
    if (input$dimplot_radio=="Single-cell"){
      temp <- v$sc_data
    } else if (input$dimplot_radio=="Spatial"){
      temp <- v$sp_data
    }
    if (!is.null(temp)){
      if (input$sc_group_var %in% colnames(temp@meta.data)){
        shiny::updateSelectInput(session, "sc_cell_high_cluster",
                          label = "Choose clusters to highlight:",
                          choices = levels(factor(eval(parse(text=paste0('temp$',
                                                                         input$sc_group_var))))))
      }
    }
  })

  # Visualizing the single-cell dim plot
  sc_dimplot_list <- shiny::eventReactive(input$sc_vis_clust_start, {
    if (input$dimplot_radio=="Single-cell"){
      temp <- v$sc_data
    } else if (input$dimplot_radio=="Spatial"){
      temp <- v$sp_data
    }
    if (!is.null(temp)){
      Seurat::Idents(temp) <- eval(parse(text=paste0('temp$',input$sc_group_var)))
    }
    list(input$sc_vis_label, input$sc_group_var,
         input$sc_label_size, input$sc_vis_title, input$sc_cell_high,
         input$sc_cell_high_color,
         input$sc_cell_high_cluster, temp, input$sc_dot_size)
  })
  # Draw dimplot
  output$sc_dimplot <- shiny::renderPlot({
    if (!is.null(sc_dimplot_list()[[8]])){
      if (sc_dimplot_list()[[5]]){
        p <- list()

        v$dimplot <- Seurat::DimPlot(sc_dimplot_list()[[8]], label = sc_dimplot_list()[[1]],
                                     repel = TRUE, group.by = sc_dimplot_list()[[2]],
                                     label.size = sc_dimplot_list()[[3]],
                                     cells.highlight = Seurat::WhichCells(sc_dimplot_list()[[8]], idents=sc_dimplot_list()[[7]]),
                                     cols.highlight = sc_dimplot_list()[[6]],
                                     pt.size = sc_dimplot_list()[[9]],
                                     cols = c("#56ebd3", "#1c5e39", "#1fa198", "#7aed7b", "#36a620", "#cadba5",
                                              "#33547a", "#24a5f7", "#3337a6", "#d678ef", "#9d0d6c", "#b090d4",
                                              "#740ece", "#ef3df3", "#69345e", "#829499", "#809b31", "#f8ba7c",
                                              "#683c00", "#d9dc22", "#992a13", "#ec102f", "#df6e78", "#fa7922",
                                              "#ae783e", "#7fdc64", "#6f2b6e", "#56cac1", "#1b511d", "#ec9fe7",
                                              "#214a65", "#b3d9fa", "#1932bf", "#34f50e")) +
          ggplot2::ggtitle(sc_dimplot_list()[[4]]) +
          ggplot2::theme(plot.title=ggplot2::element_text(size=14,face='bold',hjust=0.5))
        v$dimplot

      } else {
        v$dimplot <- Seurat::DimPlot(sc_dimplot_list()[[8]], label = sc_dimplot_list()[[1]],
                                     repel = TRUE, group.by = sc_dimplot_list()[[2]],
                                     label.size = sc_dimplot_list()[[3]],
                                     pt.size = sc_dimplot_list()[[9]],
                                     cols = c("#56ebd3", "#1c5e39", "#1fa198", "#7aed7b", "#36a620", "#cadba5",
                                              "#33547a", "#24a5f7", "#3337a6", "#d678ef", "#9d0d6c", "#b090d4",
                                              "#740ece", "#ef3df3", "#69345e", "#829499", "#809b31", "#f8ba7c",
                                              "#683c00", "#d9dc22", "#992a13", "#ec102f", "#df6e78", "#fa7922",
                                              "#ae783e", "#7fdc64", "#6f2b6e", "#56cac1", "#1b511d", "#ec9fe7",
                                              "#214a65", "#b3d9fa", "#1932bf", "#34f50e")) +
          ggplot2::ggtitle(sc_dimplot_list()[[4]]) +
          ggplot2::theme(plot.title=ggplot2::element_text(size=14,face='bold',hjust=0.5))
        v$dimplot
      }
    }
  })


  ## Frequency plot
  shiny::observeEvent(input$sc_vis_freq_start, {
    if (input$freqplot_radio=="Single-cell"){
      temp <- v$sc_data
    } else if (input$freqplot_radio=="Spatial"){
      temp <- v$sp_data
    }
    if (!is.null(temp)){
      v$freq_boxplot <- frequency_boxplot(temp,
                                          group_of_interest=input$sc_group_var_freq,
                                          cluster_name=input$sc_cluster_var_freq,
                                          x.axis.title=input$sc_freq_x_title,
                                          y.axis.title=input$sc_freq_y_title,
                                          vis.freq.text=input$sc_freq_label, freq.stats=TRUE,
                                          x.axis.text.angle=input$sc_freq_x_angle,
                                          x.axis.text.size=input$sc_freq_x_size)
      try({utils::write.csv(v$freq_boxplot[[2]], file.path(global$datapath,input$output_folder_name,
                                                           'data_files',
                                                           paste0(input$freqplot_radio,
                                                                  '_freq_plot_',
                                                                  input$sc_group_var_freq,'_',
                                                                  input$sc_cluster_var_freq,'.csv')))})
    }
  })

  output$sc_freqplot <- shiny::renderPlot({
    v$freq_boxplot[[1]]
  })

  output$sc_freqstats <- DT::renderDataTable({
    DT::datatable(v$freq_boxplot[[2]],
                  options=list(lengthMenu=c(5,10,20,40,80),pageLength=5))
  })


  # Update the SelectInput function according to input: featureplot
  shiny::observeEvent(c(input$sc_check_metadata,v$sc_data,v$sp_data,
                 input$featureplot_radio, input$sc_check_saved_genes), {
                   if (input$featureplot_radio=="Single-cell"){
                     if (!(input$sc_check_saved_genes=="All")){
                       if (input$sc_check_saved_genes=="Top 1001~2000"){gene_list <- v$sc_gene_list[1001:2000]}
                       else if (input$sc_check_saved_genes=="Top 2001~3000"){gene_list <- v$sc_gene_list[2001:3000]}
                       else {gene_list <- intersect(v$gene_upload[[input$sc_check_saved_genes]], v$sc_gene_list)}
                     } else {
                       gene_list <- v$sc_gene_list
                     }
                     sel_list <- v$sc_meta_list_value
                   } else if (input$featureplot_radio=="Spatial"){
                     if (!(input$sc_check_saved_genes=="All")){
                       if (input$sc_check_saved_genes=="Top 1001~2000"){gene_list <- v$sp_gene_list[1001:2000]}
                       else if (input$sc_check_saved_genes=="Top 2001~3000"){gene_list <- v$sp_gene_list[2001:3000]}
                       else {gene_list <- intersect(v$gene_upload[[input$sc_check_saved_genes]], v$sp_gene_list)}
                     } else {
                       gene_list <- v$sp_gene_list
                     }
                     sel_list <- v$sp_meta_list_value
                   }
                   shiny::updateSelectizeInput(session, "sc_vis_feat",
                                        label = "Choose genes to visualize",
                                        choices = gene_list, server = TRUE)
                   shiny::updateSelectInput(session, "sc_vis_metadata",
                                     label = "Choose metadata to visualize",
                                     choices = sel_list)
                 })
  # Update the minmax value according to input
  shiny::observeEvent(c(input$sc_check_metadata,input$sc_vis_metadata,input$featureplot_radio), {
    if (input$featureplot_radio=="Single-cell"){
      temp <- v$sc_data
    } else if (input$featureplot_radio=="Spatial"){
      temp <- v$sp_data
    }
    if (!is.null(temp)){
      if (input$sc_check_metadata) {
        if (!is.null(input$sc_vis_metadata)){
          val = eval(parse(text=paste0('temp$',input$sc_vis_metadata)))
          sc.min = min(val); sc.max = max(val)
          if (sc.min > 0){sc.min = 0}
          if (grepl('_cellf',input$sc_vis_metadata)){
            sc.min=0; sc.max=1
          } else if (grepl('_module|[0-9]$',input$sc_vis_metadata)) {
            sc.min=-4; sc.max=4
          } else {
            sc.min = sc.min; sc.max=sc.max
          }
          shiny::updateSliderInput(session, "sc_feat_minmax",
                            label = "Min max value",
                            value = c(sc.min, sc.max),
                            min = sc.min, max = sc.max, step=(sc.max-sc.min)/100)
        }
      } else {
        shiny::updateSliderInput(session, "sc_feat_minmax",
                          label = "Min max value",
                          value = c(0,3), min = 0, max = 10, step=0.1)
      }
    }
  })

  # Visualizing the single-cell feature plot
  sc_featplot_list <- shiny::eventReactive(input$sc_vis_feat_start, {
    if (input$featureplot_radio=="Single-cell"){
      temp <- v$sc_data
    } else if (input$featureplot_radio=="Spatial"){
      temp <- v$sp_data
    }

    list(input$sc_check_metadata, input$sc_vis_metadata,
         input$sc_vis_feat, input$sc_feat_minmax, input$sc_feat_color,
         input$sc_feat_ncol, temp)
  })


  output$sc_featplot <- shiny::renderPlot({
    if (sc_featplot_list()[[1]]){
      v$sc_feat_list <- sc_featplot_list()[[2]]
    } else {
      v$sc_feat_list <- sc_featplot_list()[[3]]
    }

    if (!is.null(sc_featplot_list()[[7]])){
      v$sc_featureplot <- FeaturePlot_mod(sc_featplot_list()[[7]],
                                          v$sc_feat_list,
                                          min=sc_featplot_list()[[4]][1],
                                          max=sc_featplot_list()[[4]][2],
                                          palette_color=sc_featplot_list()[[5]],
                                          ncol = sc_featplot_list()[[6]])
      v$sc_featureplot
    }

  })


  ## Spatial cluster plot
  # Update the SelectInput function according to input: featureplot
  shiny::observeEvent(v$sp_data, {
    shiny::updateSelectInput(session, "sp_clust_cluster_name",
                      "Group name",
                      choices = v$sp_meta_list_factor)
    if (!is.null(v$sp_data)){
      temp <- v$sp_data
      shiny::updateSelectInput(session, "sp_clust_slide.to.visualize",
                        "Choose slide to visualize",
                        choices = levels(factor(temp$orig.ident)))
    }

  })
  shiny::observeEvent(c(v$sp_data, input$sp_clust_cluster_name), {
    temp <- v$sp_data
    if (!is.null(temp)){
      if (input$sp_clust_cluster_name %in% colnames(temp@meta.data)){
        shiny::updateSelectInput(session, "sp_clust_spot.cluster.highlight",
                          "Choose clusters to subset",
                          choices = levels(factor(eval(parse(text=paste0('temp$',
                                                                         input$sp_clust_cluster_name))))))
      }
    }
  })


  # Visualizing the spatial cluster plot
  sp_clusterplot_list <- shiny::eventReactive(input$sp_vis_cluster_start, {
    temp <- v$sp_data
    list(input$sp_cluster_alpha, input$sp_cluster_image.alpha,
         input$sp_clust_slide.to.visualize,
         input$sp_clust_spot.cluster.highlight,
         input$sp_cluster_ncol, input$sp_cluster_pt.size.factor, input$sp_cluster_crop,
         input$sp_cluster_img_width, input$sp_cluster_img_height, input$sp_clust_cluster_name,
         input$sp_cluster_label, input$sp_cluster_label_size,
         temp)
  })


  output$sp_clusterplot <- shiny::renderPlot({

    if (!is.null(sp_clusterplot_list()[[13]])){
      if (!is.null(sp_clusterplot_list()[[3]])){
        slide_vis <- sp_clusterplot_list()[[3]]
      } else {
        slide_vis <- NULL
      }

      v$sp_clusterplot <- spatial_cluster_plot(sp_clusterplot_list()[[13]],
                                               cluster_name = sp_clusterplot_list()[[10]],
                                               alpha=sp_clusterplot_list()[[1]],
                                               image.alpha=sp_clusterplot_list()[[2]],
                                               slide.to.visualize=slide_vis,
                                               spot.cluster.highlight=sp_clusterplot_list()[[4]],
                                               ncol=sp_clusterplot_list()[[5]],
                                               pt.size.factor=sp_clusterplot_list()[[6]],
                                               crop=sp_clusterplot_list()[[7]],
                                               label=sp_clusterplot_list()[[11]],
                                               label.size=sp_clusterplot_list()[[12]])
      v$sp_clusterplot
    }
  }, width = shiny::reactive({input$sp_cluster_img_width}),
  height = shiny::reactive({input$sp_cluster_img_height}), bg="grey"
  )



  ## Spatial gene expression
  # Update the SelectInput function according to input: featureplot
  shiny::observeEvent(c(v$sp_data, input$sp_check_metadata, input$sp_check_saved_genes), {
    if (!is.null(v$sp_data)){
      temp <- v$sp_data
      shiny::updateSelectInput(session, "sp_slide.to.visualize",
                        label = "Choose slide to visualize",
                        choices = levels(factor(temp$orig.ident)))
    }
    # Check whether to used saved genes
    if (!(input$sp_check_saved_genes=="All")){
      if (input$sp_check_saved_genes=="Top 1001~2000"){gene_list <- v$sp_gene_list[1001:2000]}
      else if (input$sp_check_saved_genes=="Top 2001~3000"){gene_list <- v$sp_gene_list[2001:3000]}
      else {gene_list <- intersect(v$gene_upload[[input$sp_check_saved_genes]], v$sp_gene_list)}
    } else {
      gene_list <- v$sp_gene_list
    }
    shiny::updateSelectizeInput(session, "sp_vis_feat",
                         label = "Choose genes to visualize",
                         choices = gene_list, server = TRUE)
    shiny::updateSelectInput(session, "sp_cluster_name",
                      label = "Group name",
                      choices = v$sp_meta_list_factor)
    shiny::updateSelectInput(session, "sp_vis_metadata",
                      label = "Choose metadata to visualize",
                      choices = v$sp_meta_list_value)
  })
  shiny::observeEvent(c(v$sp_data, input$sp_cluster_name), {
    if (!is.null(v$sp_data)){
      temp <- v$sp_data
      if (input$sp_cluster_name %in% colnames(temp@meta.data)){
        shiny::updateSelectInput(session, "sp_spot.cluster.highlight",
                          label = "Choose clusters to subset",
                          choices = levels(factor(eval(parse(text=paste0('temp$',
                                                                         input$sp_cluster_name))))))
      }
    }
  })
  shiny::observeEvent(c(input$sp_vis_metadata, input$sp_check_metadata), {
    temp <- v$sp_data
    if (!is.null(temp)){
      if (input$sp_check_metadata){
        if (!is.null(input$sp_vis_metadata)){
          val = eval(parse(text=paste0('temp$',input$sp_vis_metadata)))
          sp.min = min(val); sp.max = max(val)
          if (sp.min > 0){sp.min = 0}
          if (grepl('_cellf',input$sp_vis_metadata)){
            sp.min=0; sp.max=1
          } else if (grepl('_module|[0-9]$',input$sp_vis_metadata)) {
            sp.min=-4; sp.max=4
          } else {
            sp.min=sp.min; sp.max=sp.max
          }
          shiny::updateSliderInput(session, "sp_feat_minmax",
                            label = "Min max value",
                            value = c(sp.min, sp.max),
                            min = sp.min, max = sp.max, step=(sp.max-sp.min)/100)
        }
      } else {
        shiny::updateSliderInput(session, "sp_feat_minmax",
                          label = "Min max value",
                          value = c(0,3), min = 0, max = 10, step=0.1)
      }
    }
  })


  # Visualizing the spatial feature plot
  sp_featplot_list <- shiny::eventReactive(input$sp_vis_feat_start, {
    temp <- v$sp_data
    list(input$sp_check_metadata, input$sp_vis_feat, input$sp_vis_metadata,
         input$sp_feat_minmax, input$sp_feat_alpha, input$sp_feat_image.alpha,
         input$sp_feat_color, input$sp_color_bar_mode, input$sp_color_bar_loc,
         input$sp_slide.to.visualize,
         input$sp_cluster_name, input$sp_spot.cluster.highlight,
         input$sp_feat_ncol, input$sp_feat_pt.size.factor, input$sp_feat_crop,
         input$sp_feat_img_width, input$sp_feat_img_height, temp,
         input$sp_title_size)
  })
  sp_featplot_multi_list <- shiny::eventReactive(input$sp_feat_save_by_n, {
    temp <- v$sp_data
    list(input$sp_check_metadata, input$sp_vis_feat, input$sp_vis_metadata,
         input$sp_feat_minmax, input$sp_feat_alpha, input$sp_feat_image.alpha,
         input$sp_feat_color, input$sp_color_bar_mode, input$sp_color_bar_loc,
         input$sp_slide.to.visualize,
         input$sp_cluster_name, input$sp_spot.cluster.highlight,
         input$sp_feat_ncol, input$sp_feat_pt.size.factor, input$sp_feat_crop,
         input$sp_feat_img_width, input$sp_feat_img_height, temp,
         input$sp_title_size)
  })


  output$sp_featplot <- shiny::renderPlot({
    if (sp_featplot_list()[[1]]){
      v$sp_feat_list <- sp_featplot_list()[[3]]
    } else {
      v$sp_feat_list <- sp_featplot_list()[[2]]
    }

    if (!is.null(sp_featplot_list()[[18]])){
      if (!is.null(sp_featplot_list()[[10]])){
        slide_vis <- sp_featplot_list()[[10]]
      } else {
        slide_vis <- NULL
      }

      # Below code to install the static V8 will solve the palette_color error
      # Sys.setenv(DOWNLOAD_STATIC_LIBV8 = 1)
      # install.packages("V8")

      v$sp_featureplot <- spatial_feat_plot_groups(sp_featplot_list()[[18]], v$sp_feat_list,
                                                   min=sp_featplot_list()[[4]][1],
                                                   max=sp_featplot_list()[[4]][2],
                                                   alpha=sp_featplot_list()[[5]],
                                                   image.alpha=sp_featplot_list()[[6]],
                                                   palette_color=sp_featplot_list()[[7]],
                                                   color_bar_mode=sp_featplot_list()[[8]],
                                                   color_bar_loc=sp_featplot_list()[[9]],
                                                   slide.to.visualize=slide_vis,
                                                   cluster_name=sp_featplot_list()[[11]],
                                                   spot.cluster.highlight=sp_featplot_list()[[12]],
                                                   ncol=sp_featplot_list()[[13]],
                                                   pt.size.factor=sp_featplot_list()[[14]],
                                                   crop=sp_featplot_list()[[15]],
                                                   title_size=sp_featplot_list()[[19]])
      v$sp_featureplot
    }
  }, width = shiny::reactive({input$sp_feat_img_width}),
  height = shiny::reactive({input$sp_feat_img_height}), bg="grey"
  )

  ## Upload gene list from feature plot multiple save
  shiny::observeEvent(input$sp_feat_upload, {
    shiny::showModal(save_gene_list_Modal(text_for_purpose='Will you save the given gene list?',
                                   text_input_name="sp_feat_save_name",
                                   text_input_explain = "Name of the gene list",
                                   file_save_name="gene_list",
                                   action_button_name = "ok_sp_feat_upload"))
  })
  shiny::observeEvent(input$ok_sp_feat_upload, {
    shinybusy::show_modal_spinner()
    if (!is.null(v$sp_csv_table)&!is.null(input$sp_column_select)){
      data_table <- v$sp_csv_table
      data_gene <- data_table[[input$sp_column_select]]
      v[['gene_upload']][[input$sp_feat_save_name]] <- intersect(v$sp_gene_list, data_gene)
    }
    shinybusy::remove_modal_spinner()
  })


  ## Save feat plot by n
  # Visualize table that was uploaded
  output$sp_csv_table <- DT::renderDataTable({
    inFile <- input$sp_upload_csv
    if (is.null(inFile))
      return(NULL)
    v$sp_csv_table <- utils::read.csv(inFile$datapath, header = input$sp_header_check)
    DT::datatable(v$sp_csv_table,
                  options=list(lengthMenu=c(5,10,20,40,80),pageLength=5))
  })
  shiny::observeEvent(v$sp_csv_table, {
    if (!is.null(v$sp_csv_table)){
      if(dim(v$sp_csv_table)[2]>0){
        shiny::updateSelectInput(session, "sp_column_select", "Select columns of gene list",
                          choices = colnames(v$sp_csv_table))
      }
    }
  })
  shiny::observeEvent(c(input$sp_feat_img_width, input$sp_feat_img_height),{
    shiny::updateSliderInput(session, "sp_save_by_n_width", "Width in cm",
                      value = round(input$sp_feat_img_width/28.35))
    shiny::updateSliderInput(session, "sp_save_by_n_height", "Height in cm",
                      value = round(input$sp_feat_img_height/28.35))
  })


  # Start save feature by n
  shiny::observeEvent(input$sp_feat_save_by_n, {
    shinybusy::show_modal_spinner()
    if (!is.null(v$sp_csv_table)&!is.null(input$sp_column_select)){
      data_tmp <- v$sp_csv_table
      eval(parse(text=paste0('sp_feat_list <- data_tmp$',input$sp_column_select)))

      if (!is.null(sp_featplot_multi_list()[[18]])){
        sp_feat_list <- intersect(rownames(sp_featplot_multi_list()[[18]]), sp_feat_list)
        if (!identical(sp_feat_list, character(0))){
          if (!is.null(sp_featplot_multi_list()[[10]])){
            slide_vis <- sp_featplot_multi_list()[[10]]
          } else {
            slide_vis <- NULL
          }
          save_feat_plot_by_n(sp_featplot_multi_list()[[18]], sp_feat_list,
                              by_n=input$sp_feat_by_n,
                              min=sp_featplot_multi_list()[[4]][1],
                              max=sp_featplot_multi_list()[[4]][2],
                              alpha=sp_featplot_multi_list()[[5]],
                              image.alpha=sp_featplot_multi_list()[[6]],
                              palette_color=sp_featplot_multi_list()[[7]],
                              color_bar_mode=sp_featplot_multi_list()[[8]],
                              color_bar_loc=sp_featplot_multi_list()[[9]],
                              slide.to.visualize=slide_vis,
                              ncol=sp_featplot_multi_list()[[13]],
                              pt.size.factor=sp_featplot_multi_list()[[14]],
                              crop=sp_featplot_multi_list()[[15]],
                              height = input$sp_save_by_n_height,
                              width = input$sp_save_by_n_width,
                              dpi = input$sp_upload_save_dpi,
                              title_size=sp_featplot_multi_list()[[19]],
                              save_path=file.path(global$datapath,
                                                  input$output_folder_name,
                                                  'data_files/'),
                              file_name = input$sp_save_name)
        }
      }
    }
    shinybusy::remove_modal_spinner()
  })



  ## Draw Violinplot
  # Update the SelectInput function according to input: featureplot
  shiny::observeEvent(c(input$vln_check_metadata,v$sc_data,v$sp_data,
                 input$vlnplot_radio, input$vln_check_saved_genes), {
                   if (input$vlnplot_radio=="Single-cell"){
                     if (!(input$vln_check_saved_genes=="All")){
                       if (input$vln_check_saved_genes=="Top 1001~2000"){gene_list <- v$sc_gene_list[1001:2000]}
                       else if (input$vln_check_saved_genes=="Top 2001~3000"){gene_list <- v$sc_gene_list[2001:3000]}
                       else {gene_list <- intersect(v$gene_upload[[input$vln_check_saved_genes]], v$sc_gene_list)}
                     } else {
                       gene_list <- v$sc_gene_list
                     }
                     sel_list <- v$sc_meta_list_factor
                     sel_list_inv <- v$sc_meta_list_value
                   } else if (input$vlnplot_radio=="Spatial"){
                     if (!(input$vln_check_saved_genes=="All")){
                       if (input$vln_check_saved_genes=="Top 1001~2000"){gene_list <- v$sp_gene_list[1001:2000]}
                       else if (input$vln_check_saved_genes=="Top 2001~3000"){gene_list <- v$sp_gene_list[2001:3000]}
                       else {gene_list <- intersect(v$gene_upload[[input$vln_check_saved_genes]], v$sp_gene_list)}
                     } else {
                       gene_list <- v$sp_gene_list
                     }
                     sel_list <- v$sp_meta_list_factor
                     sel_list_inv <- v$sp_meta_list_value
                   }

                   shiny::updateSelectizeInput(session, "vln_vis_feat",
                                        label = "Choose genes to visualize",
                                        choices = gene_list, server = TRUE)
                   shiny::updateSelectInput(session, "vln_vis_metadata",
                                     label = "Choose metadata to visualize",
                                     choices = sel_list_inv)
                   shiny::updateSelectInput(session, "vln_color_group",
                                     label = "Color group",
                                     choices = sel_list)
                   shiny::updateSelectInput(session, "vln_facet_group",
                                     label = "Facet group",
                                     choices = sel_list)
                 })

  vlnplot_list <- shiny::eventReactive(input$vln_start, {
    if (input$vlnplot_radio=="Single-cell"){
      temp <- v$sc_data
    } else if (input$vlnplot_radio=="Spatial"){
      temp <- v$sp_data
    }
    list(input$vln_check_metadata, input$vln_vis_metadata,
         input$vln_vis_feat, input$vln_color_group,
         input$vln_facet_group, input$vln_ncol, temp,
         input$vln_img_width, input$vln_img_height,
         input$vln_check_facet)

  })

  # Visualize data with Violin plot
  output$vlnplot <- shiny::renderPlot({
    if (vlnplot_list()[[1]]){
      v$vlnplot_feat_list <- vlnplot_list()[[2]]
    } else {
      v$vlnplot_feat_list <- vlnplot_list()[[3]]
    }

    if (!is.null(vlnplot_list()[[7]])){
      temp <- vlnplot_list()[[7]]

      val_color <- eval(parse(text=paste0('temp$',vlnplot_list()[[4]])))
      pal <- colormap::colormap(colormap=c('#FFFFFF','#FF0000'),
                                nshades = length(levels(factor(val_color))), reverse = TRUE)

      plot_tmp <- list()
      # Check whether to facet the violin plot
      if (vlnplot_list()[[10]]){
        val_facet <- eval(parse(text=paste0('.~temp$',vlnplot_list()[[5]])))

        for (i in 1:length(v$vlnplot_feat_list)){
          plot_tmp[[i]] <- Seurat::VlnPlot(temp, features=v$vlnplot_feat_list[i],
                                           group.by=vlnplot_list()[[4]], pt.size=0) +
            ggplot2::facet_grid(val_facet) +
            ggpubr::stat_compare_means(method = "kruskal.test", label='p') +
            ggplot2::xlab('') +
            ggplot2::scale_fill_manual(values = pal) +
            ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12),
                  axis.text.x = ggplot2::element_text(size = 9),
                  axis.text.y = ggplot2::element_text(size = 12),
                  legend.title = ggplot2::element_text(size = 12),
                  legend.text = ggplot2::element_text(size = 12)) + Seurat::NoLegend()
        }
      } else {
        for (i in 1:length(v$vlnplot_feat_list)){
          plot_tmp[[i]] <- Seurat::VlnPlot(temp, features=v$vlnplot_feat_list[i],
                                           group.by=vlnplot_list()[[4]], pt.size=0) +
            ggpubr::stat_compare_means(method = "kruskal.test", label='p') +
            ggplot2::xlab('') +
            ggplot2::scale_fill_manual(values = pal) +
            ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12),
                  axis.text.x = ggplot2::element_text(size = 9),
                  axis.text.y = ggplot2::element_text(size = 12),
                  legend.title = ggplot2::element_text(size = 12),
                  legend.text = ggplot2::element_text(size = 12)) + Seurat::NoLegend()
        }
      }
      v$vlnplot <- plot_tmp
      patchwork::wrap_plots(plot_tmp, ncol = vlnplot_list()[[6]])
    }

  }, width = shiny::reactive({input$vln_img_width}),
  height = shiny::reactive({input$vln_img_height}))


  ## Draw ridgeplot
  shiny::observeEvent(c(input$ridge_check_metadata,v$sc_data,v$sp_data,
                 input$ridgeplot_radio, input$ridge_check_saved_genes), {
                   if (input$ridgeplot_radio=="Single-cell"){
                     if (!(input$ridge_check_saved_genes=="All")){
                       if (input$ridge_check_saved_genes=="Top 1001~2000"){gene_list <- v$sc_gene_list[1001:2000]}
                       else if (input$ridge_check_saved_genes=="Top 2001~3000"){gene_list <- v$sc_gene_list[2001:3000]}
                       else {gene_list <- intersect(v$gene_upload[[input$ridge_check_saved_genes]], v$sc_gene_list)}
                     } else {
                       gene_list <- v$sc_gene_list
                     }
                     sel_list <- v$sc_meta_list_factor
                     sel_list_inv <- v$sc_meta_list_value
                   } else if (input$ridgeplot_radio=="Spatial"){
                     if (!(input$ridge_check_saved_genes=="All")){
                       if (input$ridge_check_saved_genes=="Top 1001~2000"){gene_list <- v$sp_gene_list[1001:2000]}
                       else if (input$ridge_check_saved_genes=="Top 2001~3000"){gene_list <- v$sp_gene_list[2001:3000]}
                       else {gene_list <- intersect(v$gene_upload[[input$ridge_check_saved_genes]], v$sp_gene_list)}
                     } else {
                       gene_list <- v$sp_gene_list
                     }
                     sel_list <- v$sp_meta_list_factor
                     sel_list_inv <- v$sp_meta_list_value
                   }

                   shiny::updateSelectizeInput(session, "ridge_vis_feat",
                                        label = "Choose genes to visualize",
                                        choices = gene_list, server = TRUE)

                   shiny::updateSelectInput(session, "ridge_vis_metadata",
                                     label = "Choose metadata to visualize",
                                     choices = sel_list_inv)
                   shiny::updateSelectInput(session, "ridge_vis_group",
                                     label = "Group by",
                                     choices = sel_list)
                 })

  # Update the minmax value according to input
  shiny::observeEvent(c(input$ridge_check_metadata,input$ridge_vis_metadata,input$ridgeplot_radio), {
    if (input$ridgeplot_radio=="Single-cell"){
      temp <- v$sc_data
    } else if (input$ridgeplot_radio=="Spatial"){
      temp <- v$sp_data
    }
    if (!is.null(temp)){
      if (input$ridge_check_metadata) {
        if (!is.null(input$ridge_vis_metadata)){
          val = eval(parse(text=paste0('temp$',input$ridge_vis_metadata)))
          sc.min = min(val); sc.max = max(val)
          if (sc.min > 0){sc.min = -0.5}
          if (grepl('_cellf',input$ridge_vis_metadata)){
            sc.min=-0.5; sc.max=1
          } else if (grepl('_module|[0-9]$',input$ridge_vis_metadata)) {
            sc.min=-4; sc.max=4
          } else {
            sc.min = sc.min; sc.max=sc.max
          }
          shiny::updateSliderInput(session, "ridge_feat_minmax",
                            label = "Min max value",
                            value = c(sc.min, sc.max),
                            min = sc.min, max = sc.max, step=(sc.max-sc.min)/100)
        }
      } else {
        shiny::updateSliderInput(session, "ridge_feat_minmax",
                          label = "Min max value",
                          value = c(-0.5,6), min = -1, max = 10, step=0.1)
      }
    }
  })

  ridgeplot_list <- shiny::eventReactive(input$ridge_feat_start, {
    if (input$ridgeplot_radio=="Single-cell"){
      temp <- v$sc_data
    } else if (input$ridgeplot_radio=="Spatial"){
      temp <- v$sp_data
    }

    list(input$ridge_check_metadata, input$ridge_vis_metadata,
         input$ridge_vis_feat, input$ridge_feat_minmax,
         input$ridge_vis_group,
         input$ridge_alpha,
         input$sc_feat_ncol, temp,
         input$ridge_img_width, input$ridge_img_height)
  })


  output$ridgeplot <- shiny::renderPlot({
    if (ridgeplot_list()[[1]]){
      v$ridge_feat_list <- ridgeplot_list()[[2]]
    } else {
      v$ridge_feat_list <- ridgeplot_list()[[3]]
    }

    if (!is.null(ridgeplot_list()[[8]])){
      v$ridgeplot <- RidgePlot_mod(ridgeplot_list()[[8]],
                                   v$ridge_feat_list,
                                   groups = ridgeplot_list()[[5]],
                                   lim=ridgeplot_list()[[4]],
                                   alpha = ridgeplot_list()[[6]],
                                   ncol = ridgeplot_list()[[7]])
      v$ridgeplot
    }

  }, width = shiny::reactive({input$ridge_img_width}),
  height = shiny::reactive({input$ridge_img_height}))



  ## Marker gene finding and DEG
  # Update the SelectInput function according to input: marker and deg
  shiny::observeEvent(c(v$sc_data,v$sp_data, input$marker_radio), {
    if (input$marker_radio=="Single-cell"){
      sel_list <- v$sc_meta_list_factor
    } else if (input$marker_radio=="Spatial"){
      sel_list <- v$sp_meta_list_factor
    }
    shiny::updateSelectInput(session, "sc_marker_group",
                      label = "Group name",
                      choices = sel_list)
  })
  shiny::observeEvent(c(v$sc_data, v$sp_data,input$DEG_radio), {
    if (input$DEG_radio=="Single-cell"){
      sel_list <- v$sc_meta_list_factor
    } else if (input$DEG_radio=="Spatial"){
      sel_list <- v$sp_meta_list_factor
    }
    shiny::updateSelectInput(session, "sc_deg_group",
                      label = "Group name",
                      choices = sel_list)
    shiny::updateSelectInput(session, "sc_deg_subset_group",
                      label = "Choose group for DEG",
                      choices = sel_list)
  })

  shiny::observeEvent(c(input$sc_deg_group,v$sc_data,v$sp_data), {
    if (input$DEG_radio=="Single-cell"){
      temp <- v$sc_data
    } else if (input$DEG_radio=="Spatial"){
      temp <- v$sp_data
    }

    if (!is.null(temp)){
      if (input$sc_deg_group %in% colnames(temp@meta.data)){
        shiny::updateSelectInput(session, "sc_deg_ref",
                          label = "Choose reference idents",
                          choices = levels(factor(eval(parse(text=paste0('temp$',
                                                                         input$sc_deg_group))))))
        shiny::updateSelectInput(session, "sc_deg_int",
                          label = "Choose idents of interest",
                          choices = levels(factor(eval(parse(text=paste0('temp$',
                                                                         input$sc_deg_group))))))
        shiny::updateSelectInput(session, "sc_deg_subset_sel",
                          label = "Choose cluster to subset",
                          choices = levels(factor(eval(parse(text=paste0('temp$',
                                                                         input$sc_deg_group))))))
      }
    }
  })
  shiny::observeEvent(c(input$sc_deg_subset_group,input$sc_deg_subset_sel,
                 v$sc_data,v$sp_data), {
                   if (input$DEG_radio=="Single-cell"){
                     temp <- v$sc_data
                   } else if (input$DEG_radio=="Spatial"){
                     temp <- v$sp_data
                   }

                   if (!is.null(temp)){
                     if (input$sc_deg_subset_group %in% colnames(temp@meta.data)){
                       shiny::updateSelectInput(session, "sc_deg_subset_int",
                                         label = "Choose idents of interest in a subset",
                                         choices = levels(factor(eval(parse(text=paste0('temp$',
                                                                                        input$sc_deg_subset_group))))))
                       shiny::updateSelectInput(session, "sc_deg_subset_ref",
                                         label = "Choose reference idents in a subset",
                                         choices = levels(factor(eval(parse(text=paste0('temp$',
                                                                                        input$sc_deg_subset_group))))))
                     }
                   }
                 })

  ## Define reactive values to action button
  sc_marker_list <- shiny::eventReactive(input$sc_marker_start, {
    if (input$marker_radio=="Single-cell"){
      temp <- v$sc_data
    } else if (input$marker_radio=="Spatial"){
      temp <- v$sp_data
    }

    if (!is.na(as.numeric(input$sc_marker_logfc_thres))&
        !is.na(as.numeric(input$sc_marker_exp_thres))){
      Seurat::Idents(temp) <- eval(parse(text=paste0('temp$',input$sc_marker_group)))
      list(input$sc_marker_group, input$sc_marker_logfc_thres,
           input$sc_marker_posonly, temp,
           input$check_marker_mode,
           input$sc_marker_rfTrees, input$sc_median_exp_level,
           input$sc_genes_to_testing, input$sc_beta_value,
           input$sc_marker_exp_thres)
    }
  })

  sc_deg_list <- shiny::eventReactive(input$sc_deg_start, {
    if (input$DEG_radio=="Single-cell"){
      temp <- v$sc_data
    } else if (input$DEG_radio=="Spatial"){
      temp <- v$sp_data
    }

    Seurat::Idents(temp) <- eval(parse(text=paste0('temp$',input$sc_deg_group)))

    if (input$sc_check_deg_subset){
      temp <- subset(temp, idents=input$sc_deg_subset_sel)
      Seurat::Idents(temp) <- eval(parse(text=paste0('temp$',input$sc_deg_subset_group)))
    } else {
    }
    if (!is.na(as.numeric(input$sc_deg_logfc_thres))&
        !is.na(as.numeric(input$sc_deg_exp_thres))){
      list(input$sc_deg_group, input$sc_check_deg_subset,
           input$sc_deg_int, input$sc_deg_ref,
           input$sc_deg_subset_sel,
           input$sc_deg_subset_group, input$sc_deg_subset_int, input$sc_deg_subset_ref,
           temp, input$sc_deg_logfc_thres, input$sc_deg_exp_thres)
    }
  })

  ## DEG process start
  output$sc_marker_table <- DT::renderDataTable({
    if (sc_marker_list()[[5]]=='Wilcoxon'){
      temp <- FindMarkers_mod(sc_marker_list()[[4]], purpose="marker",
                              group.to.find=sc_marker_list()[[1]],
                              logfc.threshold=as.numeric(sc_marker_list()[[2]]),
                              only.pos=sc_marker_list()[[3]],
                              test.use='wilcox')

      v$sc_marker <- temp %>% dplyr::filter(avg_exp_clust > as.numeric(sc_marker_list()[[10]])) %>%
        dplyr::group_by(cluster) %>% dplyr::arrange(dplyr::desc(avg_exp_clust), .by_group = TRUE)
      try({utils::write.csv(v$sc_marker, file.path(global$datapath,input$output_folder_name,
                                                   'data_files',paste0(input$marker_radio,'_marker_',
                                                                       sc_marker_list()[[1]],'.csv')))})
    } else if (sc_marker_list()[[5]]=='NSForest') {
      assay_type <- ifelse(input$marker_radio=='Single-cell', 'RNA',
                           ifelse(input$marker_radio=='Spatial','Spatial', NULL))

      outdir_ns <- file.path(global$datapath,input$output_folder_name,
                             'data_files',paste0(input$marker_radio,'_NSForest_marker'))
      # Make output directory
      if (!file.exists(outdir_ns)){
        dir.create(outdir_ns)
      }

      v$sc_marker <- NS_Forest_R(object=sc_marker_list()[[4]], assay=assay_type, slot="data",
                                 outdir=outdir_ns,
                                 clusterLabelcolumnHeader=sc_marker_list()[[1]],
                                 rfTrees=sc_marker_list()[[6]],
                                 Median_Expression_Level=sc_marker_list()[[7]],
                                 Genes_to_testing=sc_marker_list()[[8]],
                                 betaValue=sc_marker_list()[[9]],
                                 conda.env.name='STquantool')
    }

    DT::datatable(v$sc_marker,
                  options=list(lengthMenu=c(5,10,20,40,80),pageLength=5))
  })

  ## Upload gene list
  shiny::observeEvent(input$sc_marker_upload,{
    shiny::showModal(save_gene_list_Modal(text_for_purpose='Will you save the given gene list?',
                                   text_input_name="marker_save_name",
                                   text_input_explain = "Name of the gene list",
                                   file_save_name="gene_list",
                                   action_button_name = "ok_sc_marker_gene"))
  })
  shiny::observeEvent(input$ok_sc_marker_gene, {
    shinybusy::show_modal_spinner()
    if (!is.null(v$sc_marker)){
      if (sc_marker_list()[[5]]=='Wilcoxon'){
        for (i in levels(factor(v$sc_marker[['cluster']]))){
          marker_genes <- v$sc_marker %>% dplyr::filter(cluster == i)
          v[['gene_upload']][[paste0(input$marker_save_name,'_',i)]] <- marker_genes[['gene']]
        }

      } else if (sc_marker_list()[[5]]=='NSForest'){
        cluster_marker_list <- list()

        for (i in 1:length(v$sc_marker[['Binary_Genes']])){
          cluster_marker_list[[i]] <- v$sc_marker[i,]$Binary_Genes[[1]]
          cluster_marker_list[[i]] <- sapply(cluster_marker_list[[i]], function(x){
            if (grepl('X[0-9]',x)){return(substr(x, 2, nchar(x)))}
            else {return(x)}
          })
          v[['gene_upload']][[paste0(input$marker_save_name,"_",
                                     v$sc_marker[i,]$clusterName)]] <- cluster_marker_list[[i]]
        }
      }
    }
    shinybusy::remove_modal_spinner()
  })

  output$sc_deg_table <- DT::renderDataTable({
    if (sc_deg_list()[[2]]){
      temp <- FindMarkers_mod(sc_deg_list()[[9]], purpose="DEG",
                              group.to.find = sc_deg_list()[[6]],
                              ident.1 = sc_deg_list()[[7]],
                              ident.2 = sc_deg_list()[[8]],
                              test.use='MAST',
                              logfc.threshold = as.numeric(sc_deg_list()[[10]]))
      v$DEG <- temp %>% dplyr::filter(avg_exp_ident.1 > as.numeric(sc_deg_list()[[11]])) %>%
        dplyr::mutate(sign = ifelse(avg_log2FC > 0, "pos", "neg")) %>%
        dplyr::arrange(dplyr::desc(avg_exp_ident.1)) %>% dplyr::arrange(dplyr::desc(sign)) %>%
        dplyr::select(-sign)

      try({utils::write.csv(v$DEG, file.path(global$datapath,input$output_folder_name,
                                             'data_files',
                                             paste0(input$DEG_radio,'_DEG_subset_',
                                                    paste(sc_deg_list()[[5]],collapse="_"),'_btw_',
                                                    paste(sc_deg_list()[[7]],collapse="."),'_',
                                                    paste(sc_deg_list()[[8]],collapse="."),'.csv')))})
      DT::datatable(v$DEG,
                    options=list(lengthMenu=c(5,10,20,40,80), pageLength=5))
    } else {
      temp <- FindMarkers_mod(sc_deg_list()[[9]], purpose="DEG",
                              group.to.find = sc_deg_list()[[1]],
                              ident.1 = sc_deg_list()[[3]],
                              ident.2 = sc_deg_list()[[4]],
                              test.use='MAST',
                              logfc.threshold = as.numeric(sc_deg_list()[[10]]))
      v$DEG <- temp %>% dplyr::filter(avg_exp_ident.1 > as.numeric(sc_deg_list()[[11]])) %>%
        dplyr::mutate(sign = ifelse(avg_log2FC > 0, "pos", "neg")) %>%
        dplyr::arrange(dplyr::desc(avg_exp_ident.1)) %>% dplyr::arrange(dplyr::desc(sign)) %>%
        dplyr::select(-sign)

      try({utils::write.csv(v$DEG, file.path(global$datapath,input$output_folder_name,
                                             'data_files',
                                             paste0(input$DEG_radio,'_DEG_',
                                                    paste(sc_deg_list()[[3]],collapse="."),
                                                    '_',
                                                    paste(sc_deg_list()[[4]],collapse="."),
                                                    '.csv')))})
      DT::datatable(v$DEG,
                    options=list(lengthMenu=c(5,10,20,40,80), pageLength = 5))
    }
  })

  ## Upload gene list
  shiny::observeEvent(input$sc_deg_upload,{
    shiny::showModal(save_gene_list_Modal(text_for_purpose='Will you save the given gene list?',
                                   text_input_name="deg_save_name",
                                   text_input_explain = "Name of the gene list",
                                   file_save_name="gene_list",
                                   action_button_name = "ok_sc_deg"))
  })

  shiny::observeEvent(input$ok_sc_deg, {
    shinybusy::show_modal_spinner()
    if (!is.null(v$DEG)){
      v[['gene_upload']][[input$deg_save_name]] <- v$DEG[['gene']]
    }
    shinybusy::remove_modal_spinner()
  })


  # Draw volcano plot
  sc_deg_plot <- shiny::eventReactive(input$sc_deg_volcano_start,{
    if (!is.null(v$DEG)){
      deg_subset <- v$DEG
      eval(parse(text=paste0('v$deg_subset <- deg_subset[deg_subset$p_val_adj<',
                             input$sc_deg_volcano_p,
                             ' & abs(deg_subset$avg_log2FC)>',
                             input$sc_deg_volcano_logfc,',]')))

      list(input$sc_check_deg_subset, input$sc_deg_volcano_p, input$sc_deg_volcano_logfc,
           input$sc_deg_volcano_x_title, input$sc_deg_volcano_y_title)
    }
  })


  output$sc_deg_volcano <- shiny::renderPlot({
    if(!is.null(v$DEG)){
      if (sc_deg_plot()[[1]]){
        v$volcano <- EnhancedVolcano::EnhancedVolcano(v$DEG,
                                                      lab= rownames(v$DEG),
                                                      x = 'avg_log2FC',
                                                      y = 'p_val_adj',
                                                      xlab = sc_deg_plot()[[4]],
                                                      ylab = sc_deg_plot()[[5]],
                                                      pCutoff = as.numeric(sc_deg_plot()[[2]]),
                                                      FCcutoff= as.numeric(sc_deg_plot()[[3]]),
                                                      col=c('gray20','gray20','gray20','red3'),
                                                      colAlpha = 1,
                                                      title = ,
                                                      subtitle = paste0(sc_deg_list()[[7]],' vs. ',
                                                                        sc_deg_list()[[8]],' in ',
                                                                        paste(sc_deg_list()[[5]],collapse='.')),
                                                      legendPosition = 'none',
                                                      labSize = 3.5,
                                                      drawConnectors = TRUE,
                                                      widthConnectors = 0.5,
                                                      colConnectors = 'gray60',
                                                      caption="")
        v$volcano
      } else {
        v$volcano <- EnhancedVolcano::EnhancedVolcano(v$DEG,
                                                      lab= rownames(v$DEG),
                                                      x = 'avg_log2FC',
                                                      y = 'p_val_adj',
                                                      xlab = sc_deg_plot()[[4]],
                                                      ylab = sc_deg_plot()[[5]],
                                                      pCutoff = as.numeric(sc_deg_plot()[[2]]),
                                                      FCcutoff= as.numeric(sc_deg_plot()[[3]]),
                                                      col=c('gray20','gray20','gray20','red3'),
                                                      colAlpha = 1,
                                                      title = paste0(sc_deg_list()[[3]],' vs. ',
                                                                     sc_deg_list()[[4]]),
                                                      subtitle = "",
                                                      legendPosition = 'none',
                                                      labSize = 3.5,
                                                      drawConnectors = TRUE,
                                                      widthConnectors = 0.5,
                                                      colConnectors = 'gray60',
                                                      caption="")
        v$volcano
      }
    }
  })

  # Visualize the utilized genes in table
  output$sc_deg_filter_table <- DT::renderDataTable({
    DT::datatable(v$deg_subset,
                  options=list(lengthMenu=c(5,10,20,40,80),pageLength=5))
  })

  # Upload gene list
  shiny::observeEvent(input$sc_deg_volcano_upload,{
    shiny::showModal(save_gene_list_Modal(text_for_purpose='Will you save the given gene list?',
                                   text_input_name="deg_volcano_save_name",
                                   text_input_explain = "Name of the gene list",
                                   file_save_name="gene_list",
                                   action_button_name = "ok_sc_deg_volcano"))
  })

  shiny::observeEvent(input$ok_sc_deg_volcano, {
    shinybusy::show_modal_spinner()
    if (!is.null(v$deg_subset)){
      v[['gene_upload']][[input$deg_volcano_save_name]] <- v$deg_subset[['gene']]
    }
    shinybusy::remove_modal_spinner()
  })



  ## Enrichment analysis for selected genes
  sc_deg_enrich_plot <- shiny::eventReactive(input$sc_deg_enrich_start, {
    if(!is.null(v$DEG)){
      deg_subset <- v$DEG
      if (as.numeric(input$sc_deg_enrich_logfc) < 0){logfc <- "-"+input$sc_deg_enrich_logfc}
      else {logfc <-input$sc_deg_enrich_logfc}
      eval(parse(text=paste0('v$deg_enrich_subset <- deg_subset[deg_subset$p_val_adj<',
                             input$sc_deg_enrich_p,
                             ' & deg_subset$avg_log2FC>',logfc,',]')))
    }

    list(input$sc_deg_enrich_type, input$sc_deg_enrich_species,
         input$sc_deg_enrich_p, input$sc_deg_enrich_logfc,
         input$sc_deg_enrich_nshow, input$sc_deg_enrich_terms)
  })

  output$sc_deg_enrich <- shiny::renderPlot({
    if (sc_deg_enrich_plot()[[2]]=="Mouse"){
      require(org.Mm.eg.db)
      species_go <- "org.Mm.eg.db"
      species_kegg <- "mmu"
    } else if (sc_deg_enrich_plot()[[2]]=="Human") {
      require(org.Hs.eg.db)
      species_go <- "org.Hs.eg.db"
      species_kegg <- "hsa"
    }

    if(!is.null(v$DEG)){
      gene.list <- clusterProfiler::bitr(rownames(v$deg_enrich_subset), fromType = "SYMBOL",
                                         toType = c("ENSEMBL", "SYMBOL","ENTREZID"),
                                         OrgDb = species_go)

      if (sc_deg_enrich_plot()[[1]]=="GO"){
        v$go <- clusterProfiler::enrichGO(gene          = gene.list$ENSEMBL,
                                          OrgDb         = species_go,
                                          keyType       = 'ENSEMBL',
                                          ont = sc_deg_enrich_plot()[[6]],
                                          pAdjustMethod = 'BH',
                                          pvalueCutoff  = 0.05,
                                          qvalueCutoff  = 0.2)
        v$dotplot <- clusterProfiler::dotplot(v$go, showCategory=sc_deg_enrich_plot()[[5]]) + ggplot2::ggtitle('GO analysis')
        v$dotplot

      } else if (sc_deg_enrich_plot()[[1]]=="KEGG") {
        v$kegg <- clusterProfiler::enrichKEGG(gene          = gene.list$ENTREZID,
                                              organism         = species_kegg,
                                              pAdjustMethod = "BH",
                                              pvalueCutoff = 0.05,
                                              qvalueCutoff = 0.2)
        v$dotplot <- clusterProfiler::dotplot(v$kegg, showCategory=sc_deg_enrich_plot()[[5]]) + ggplot2::ggtitle('KEGG analysis')
        v$dotplot
      }
    }
  })

  output$sc_deg_enrich_table <- DT::renderDataTable({
    DT::datatable(v$deg_enrich_subset,
                  options=list(lengthMenu=c(5,10,20,40,80),pageLength=5))
  })


  # Annotate cell clusters
  shiny::observeEvent(c(v$sc_data,v$sp_data,input$annotation_radio), {
    if (input$annotation_radio=="Single-cell"){
      sel_list <- v$sc_meta_list_factor
    } else if (input$annotation_radio=="Spatial"){
      sel_list <- v$sp_meta_list_factor
    }
    shiny::updateSelectInput(session, "annotation_group", "Group name", choices = sel_list)
  })

  shiny::observeEvent(c(v$sc_data,v$sp_data,input$annotation_group,
                 input$annotation_radio), {

                   if (input$annotation_radio=="Single-cell"){
                     temp <- v$sc_data
                   } else if (input$annotation_radio=="Spatial"){
                     temp <- v$sp_data
                   }
                   if (!is.null(temp)){
                     if (input$annotation_group %in% colnames(temp@meta.data)){
                       # Arrange the identity for the orig.ident and check if it is NULL
                       if (input$annotation_group=='orig.ident' & is.null(levels(temp$orig.ident)) & input$annotation_radio=="Spatial"){
                         grp_orig_names <- levels(factor(temp$orig.ident))
                         try({grp_mod_names <- names(temp@images)
                         if (!identical(setdiff(grp_mod_names, grp_orig_names), character(0))) {
                           grp_sel_num <- sapply(grp_mod_names, function(x){match(x, make.names(grp_orig_names))})
                           grp_orig_names <- grp_orig_names[grp_sel_num]
                           temp$orig.ident <- factor(temp$orig.ident, levels = grp_orig_names)
                         } else {
                           temp$orig.ident <- factor(temp$orig.ident, levels = names(temp@images))
                         }})
                       }
                       tmp <- levels(eval(parse(text=paste0('temp$',
                                                            input$annotation_group))))
                       if (is.null(tmp)){tmp <- levels(factor(eval(parse(text=paste0('temp$',
                                                                                     input$annotation_group)))))}
                       output$cluster_members <- shiny::renderText({tmp})
                       shiny::updateTextInput(session, "annotation_labels",
                                       label = "New names to annotate: separated by comma, no space",
                                       value = tmp)
                     }
                   }
                 })

  # Perform annotation
  shiny::observeEvent(input$cluster_new_idents, {
    shinybusy::show_modal_spinner()
    if (input$annotation_radio=="Single-cell"){
      temp <- v$sc_data
    } else if (input$annotation_radio=="Spatial"){
      temp <- v$sp_data
    }
    if (!is.null(temp)){
      # Arrange the identity for the orig.ident and check if it is NULL
      if (input$annotation_group=='orig.ident' & is.null(levels(temp$orig.ident)) & input$annotation_radio=="Spatial"){
        grp_orig_names <- levels(factor(temp$orig.ident))
        try({grp_mod_names <- names(temp@images)
        if (!identical(setdiff(grp_mod_names, grp_orig_names), character(0))) {
          grp_sel_num <- sapply(grp_mod_names, function(x){match(x, make.names(grp_orig_names))})
          grp_orig_names <- grp_orig_names[grp_sel_num]
          temp$orig.ident <- factor(temp$orig.ident, levels = grp_orig_names)
        } else {
          temp$orig.ident <- factor(temp$orig.ident, levels = names(temp@images))
        }})
      }

      Seurat::Idents(temp) <- eval(parse(text=paste0('temp$',
                                                     input$annotation_group)))
      new.cluster.ids <- sapply(strsplit(input$annotation_labels, ",")[[1]],
                                function(x){as.character(x)})
      if (length(levels(temp))==length(new.cluster.ids)){
        names(new.cluster.ids) <- levels(temp)
        temp <- Seurat::RenameIdents(temp, new.cluster.ids)

        eval(parse(text=paste0('temp$',input$new_group_name,'<- Seurat::Idents(temp)')))

        if (input$cluster_annotate_recode){
          v[['annotate_recode']][[input$annotation_group]] <- c(input$new_group_name,
                                                                input$annotation_group)
        }

        # Save and update
        if (input$annotation_radio=="Single-cell"){
          v$sc_data <- temp
        } else if (input$annotation_radio=="Spatial"){
          v$sp_data <- temp
        }
      }
    }
    shinybusy::remove_modal_spinner()
  })


  # Module score generation
  shiny::observeEvent(c(v$sc_data, v$sp_data, input$module_score_radio),{
    if (input$module_score_radio=="Single-cell"){
      if (!(input$module_check_saved_genes=="All")){
        if (input$module_check_saved_genes=="Top 1001~2000"){gene_list <- v$sc_gene_list[1001:2000]}
        else if (input$module_check_saved_genes=="Top 2001~3000"){gene_list <- v$sc_gene_list[2001:3000]}
        else {gene_list <- intersect(v$gene_upload[[input$module_check_saved_genes]], v$sc_gene_list)}
      } else {
        gene_list <- v$sc_gene_list
      }
    } else if (input$module_score_radio=="Spatial"){
      if (!(input$module_check_saved_genes=="All")){
        if (input$module_check_saved_genes=="Top 1001~2000"){gene_list <- v$sp_gene_list[1001:2000]}
        else if (input$module_check_saved_genes=="Top 2001~3000"){gene_list <- v$sp_gene_list[2001:3000]}
        else {gene_list <- intersect(v$gene_upload[[input$module_check_saved_genes]], v$sp_gene_list)}
      } else {
        gene_list <- v$sp_gene_list
      }
    }
    if (!input$module_score_csv_check) {
      shiny::updateSelectizeInput(session, "module_score_list",
                           label = "Gene list for module score",
                           choices = gene_list, server = TRUE)
    }
  })

  shiny::observeEvent(input$module_score_start, {
    shinybusy::show_modal_spinner()
    if (input$module_score_radio=="Single-cell"){
      temp <- v$sc_data
    } else if (input$module_score_radio=="Spatial"){
      temp <- v$sp_data
    }
    if (!is.null(temp)){
      if (input$module_score_csv_check){
        feature_list <- sapply(strsplit(input$module_score_list_csv, ",")[[1]],
                               function(x){as.character(x)})
      } else {
        feature_list <- input$module_score_list
      }
      if (!is.null(feature_list)){
        temp <- Seurat::AddModuleScore(temp,
                                       features = list(feature_list),
                                       name = input$module_score_name)
        if (input$module_score_radio=="Single-cell"){
          v$sc_data <- temp
        } else if (input$module_score_radio=="Spatial"){
          v$sp_data <- temp
        }
      }
    }
    shinybusy::remove_modal_spinner()
  })


  output$module_score_table <- DT::renderDataTable({
    inFile <- input$module_score_csv
    if (is.null(inFile))
      return(NULL)
    v$module_table <- utils::read.csv(inFile$datapath, header = input$module_score_header)
    DT::datatable(v$module_table,
                  options=list(lengthMenu=c(5,10,20,40,80),pageLength=5))
  })

  shiny::observeEvent(v$module_table, {
    if (!is.null(v$module_table)){
      if(dim(v$module_table)[2]>0){
        shiny::updateSelectInput(session, "module_table_select", "Select columns of gene list",
                          choices = colnames(v$module_table))
      }
    }
  })

  shiny::observeEvent(c(v$sc_data,v$sp_data,input$module_score_radio,
                 input$module_score_table_apply), {
                   if (!is.null(v$module_table)){
                     if(dim(v$module_table)[1]>0){
                       tb <- v$module_table
                       if (input$module_score_radio=="Single-cell"){
                         gene_list <- v$sc_gene_list
                       } else if (input$module_score_radio=="Spatial"){
                         gene_list <- v$sp_gene_list
                       }
                       if (!is.null(gene_list)){
                         intersect_genes <- intersect(gene_list,
                                                      eval(parse(text=paste0('unique(tb$',input$module_table_select,')'))))
                         shiny::updateTextInput(session, "module_score_list_csv",
                                         label = "Gene list for module score",
                                         value = intersect_genes)
                       }
                     }
                   }
                 })

  ## Upload gene list from feature plot multiple save
  shiny::observeEvent(input$module_score_gene_upload, {
    shiny::showModal(save_gene_list_Modal(text_for_purpose='Will you save the given gene list?',
                                   text_input_name="module_score_feat_save_name",
                                   text_input_explain = "Name of the gene list",
                                   file_save_name="gene_list",
                                   action_button_name = "ok_module_score_feat_upload"))
  })
  shiny::observeEvent(input$ok_module_score_feat_upload, {
    shinybusy::show_modal_spinner()
    if (!is.null(v$module_table)&!is.null(input$module_table_select)){
      data_table <- v$module_table
      data_gene <- data_table[[input$module_table_select]]
      if (input$module_score_radio=="Single-cell"){
        gene_list <- v$sc_gene_list
      } else if (input$module_score_radio=="Spatial"){
        gene_list <- v$sp_gene_list
      }
      v[['gene_upload']][[input$module_score_feat_save_name]] <- intersect(gene_list, data_gene)
    }
    shinybusy::remove_modal_spinner()
  })


  ## Subset the data
  shiny::observeEvent(c(v$sp_data, v$sc_data, input$subset_radio), {
    if (input$subset_radio=="Single-cell"){
      sel_list <- v$sc_meta_list_factor
    } else if (input$subset_radio=="Spatial"){
      sel_list <- v$sp_meta_list_factor
    }
    shiny::updateSelectInput(session, "subset_group",
                      label = "Group for subsetting",
                      choices = sel_list)
  })
  shiny::observeEvent(c(v$sc_data,v$sp_data,input$subset_group), {
    if (input$subset_radio=="Single-cell"){
      temp <- v$sc_data
    } else if (input$subset_radio=="Spatial"){
      temp <- v$sp_data
    }
    if (!is.null(temp)){
      if (input$subset_group %in% colnames(temp@meta.data)){
        shiny::updateSelectInput(session, "subset_list",
                          label = "Choose clusters to subset",
                          choices = levels(factor(eval(parse(text=paste0('temp$',
                                                                         input$subset_group))))))
      }
    }
  })
  shiny::observeEvent(input$subset_start, {
    shinybusy::show_modal_spinner()
    if (input$subset_radio=="Single-cell"){
      temp <- v$sc_data
    } else if (input$subset_radio=="Spatial"){
      temp <- v$sp_data
    }
    if (!is.null(temp)){
      group_members <- eval(parse(text=paste0('temp$',input$subset_group)))
      Seurat::Idents(temp) <- group_members
      temp <- subset(temp, idents=input$subset_list)

      if (input$subset_radio=="Single-cell"){
        if (input$subset_recluster){
          temp <- recluster_dataset_rpca(temp, split.by='orig.ident',
                                         data_type='Single-cell',
                                         n_var_features=input$recluster_n_var_features,
                                         n_integ_features=input$recluster_n_integ_features,
                                         integ_dim=input$recluster_integ_dim,
                                         cluster_dim=input$recluster_cluster_dim,
                                         cluster_resolution=input$recluster_cluster_resolution)
        }
        v$sc_data <- temp
      } else if (input$subset_radio=="Spatial"){
        if (input$subset_group == 'orig.ident'){
          spatial_image <- names(temp@images)
          spatial_orig_names <- levels(factor(group_members))
          if (!identical(setdiff(spatial_image, spatial_orig_names), character(0))) {
            grp_sel_num <- match(make.names(input$subset_list), spatial_image)
            temp@images <- temp@images[spatial_image[grp_sel_num]]
          } else {
            grp_sel_num <- match(input$subset_list, spatial_image)
            temp@images <- temp@images[spatial_image[grp_sel_num]]
          }
        }
        if (input$subset_recluster){
          temp <- recluster_dataset_rpca(temp, split.by='orig.ident',
                                         data_type='Spatial',
                                         n_var_features=input$recluster_n_var_features,
                                         n_integ_features=input$recluster_n_integ_features,
                                         integ_dim=input$recluster_integ_dim,
                                         cluster_dim=input$recluster_cluster_dim,
                                         cluster_resolution=input$recluster_cluster_resolution)
        }
        v$sp_data <- temp
      }
    }
    shinybusy::remove_modal_spinner()
  })


  ## Regional value quantitation
  shiny::observeEvent(v$sp_data, {
    shiny::updateSelectInput(session, "quantitation_comp_group",
                      label = "Comparison group",
                      choices = v$sp_meta_list_factor)
    shiny::updateSelectInput(session, "quantitation_facet_group",
                      label = "Facet group",
                      choices = v$sp_meta_list_factor)
  })
  shiny::observeEvent(input$quantitation_agg_mode, {
    if (input$quantitation_agg_mode){
      shiny::updateSelectInput(session, "quantitation_mode",
                        label = "Calculation mode",
                        choices = c("mean", "sum", "skewness", "kurtosis"), selected = "mean")
    } else {
      shiny::updateSelectInput(session, "quantitation_mode",
                        label = "Calculation mode",
                        choices = c("mean", "sum", "skewness", "kurtosis",
                                    "boxplot (spots)"="boxplot"), selected = "mean")
    }
  })

  shiny::observeEvent(v$annotate_recode, {
    shiny::updateSelectInput(session, "quantitation_split_group",
                      label = "Split group (Go to 'Utility-Annotation' and define)",
                      choices = c(names(v$annotate_recode), "seurat_clusters"))
  })

  shiny::observeEvent(c(input$quantitation_split_group, v$annotate_recode), {
    if (!is.null(v$annotate_recode[[input$quantitation_split_group]])){
      shiny::updateSelectInput(session, "quantitation_recode_group",
                        label = "Recode group (Go to 'Utility-Annotation' and define)",
                        choices = v$annotate_recode[[input$quantitation_split_group]])
    } else {
      shiny::updateSelectInput(session, "quantitation_recode_group",
                        label = "Recode group (Go to 'Utility-Annotation' and define)",
                        choices = "")
    }
  })


  shiny::observeEvent(c(v$sp_data, input$quantitation_cellf_mode,
                 input$quantitation_check_saved_genes), {
                   if (input$quantitation_cellf_mode=="metadata"){
                     if (!identical(grep('_cellf', v$sp_meta_list_value, value=TRUE),character(0))){
                       cellf_list <- grep('_cellf', v$sp_meta_list_value, value=TRUE)
                     } else {
                       cellf_list <- NULL
                     }
                     shiny::updateSelectInput(session, "quantitation_cellf1",
                                       label = "Metadata to quantify",
                                       choices = v$sp_meta_list_value, selected = cellf_list)
                   } else if (input$quantitation_cellf_mode=="genes"){
                     if (!(input$quantitation_check_saved_genes=="All")){
                       if (input$quantitation_check_saved_genes=="Top 1001~2000"){gene_list <- v$sp_gene_list[1001:2000]}
                       else if (input$quantitation_check_saved_genes=="Top 2001~3000"){gene_list <- v$sp_gene_list[2001:3000]}
                       else {gene_list <- intersect(v$gene_upload[[input$quantitation_check_saved_genes]], v$sp_gene_list)}
                     } else {
                       gene_list <- v$sp_gene_list
                     }
                     shiny::updateSelectizeInput(session, "quantitation_cellf2",
                                          label = "Genes to quantify",
                                          choices = gene_list, server=TRUE)
                   }
                 })

  shiny::observeEvent(c(input$quantitation_cellf1, input$quantitation_cellf2), {
    y_axis_name <- "values"
    if (input$quantitation_cellf_mode=="metadata" &
        !is.null(input$quantitation_cellf1)){
      if (grepl('_cellf',input$quantitation_cellf1)){
        y_axis_name <- "cell fraction"
      } else if (grepl('[0-9]$',input$quantitation_cellf1)){
        y_axis_name <- "module score"
      } else {
        y_axis_name <- "values"
      }
    }
    if (input$quantitation_cellf_mode=="genes" &
        !is.null(input$quantitation_cellf2)){
      y_axis_name <- "expression"
    }
    shiny::updateTextInput(session, inputId = "quantitation_name",
                    label = "Name of y-axis", value = y_axis_name)
  })


  ## Quantitation plot
  shiny::observeEvent(input$quantitation_start, {
    shinybusy::show_modal_spinner()
    if (((input$quantitation_agg_mode &
          input$quantitation_comp_group != input$quantitation_split_group)|
         (!input$quantitation_agg_mode &
          input$quantitation_comp_group != input$quantitation_facet_group)) &
        !(is.null(input$quantitation_cellf1)&is.null(input$quantitation_cellf2))){
      temp <- v$sp_data
    } else {
      temp <- NULL
    }

    if (!is.null(temp)){
      if (input$quantitation_cellf_mode=="metadata"){
        quantitation_cellf <- input$quantitation_cellf1
      } else if (input$quantitation_cellf_mode=="genes"){
        quantitation_cellf <- input$quantitation_cellf2
      }

      if (input$quantitation_check_pairwise){
        quantitation_pairwise_stats <- input$quantitation_pairwise_stats
      } else {
        quantitation_pairwise_stats <- NULL
      }

      if ((input$quantitation_agg_mode & input$quantitation_recode_group=="")|
          (is.null(quantitation_cellf))){
      } else {
        v$quantitation_result <- quantitation_plot(temp,
                                                   data.to.use=quantitation_cellf,
                                                   group.to.compare=input$quantitation_comp_group,
                                                   group.to.facet=input$quantitation_facet_group,
                                                   agg.to.boxplot=input$quantitation_agg_mode,
                                                   group.to.split=input$quantitation_split_group,
                                                   group.to.recode=input$quantitation_recode_group,
                                                   calculate_mode=input$quantitation_mode,
                                                   pairwise.comp.stats=quantitation_pairwise_stats,
                                                   x.axis.title='', y.axis.title=input$quantitation_name,
                                                   x.axis.title.size=20,y.axis.title.size=20,
                                                   x.axis.text.size=10,y.axis.text.size=12,
                                                   x.axis.text.angle=90,
                                                   legend.title.size=12,legend.text.size=12,
                                                   vis.value.text=input$quantitation_vis_cellf,
                                                   value.text.size=3.5,
                                                   return.stats=TRUE,
                                                   plot_ncol = input$quantitation_feat_ncol,
                                                   spot.total.num.stats = input$spot_total_number)
        quantitation_name_list <- quantitation_cellf[1:min(5, length(quantitation_cellf))]
        if (input$quantitation_agg_mode){
          try({utils::write.csv(v$quantitation_result[[2]], file.path(global$datapath,input$output_folder_name,
                                                                      'data_files',
                                                                      paste0('Regional_quant_table_',
                                                                             input$quantitation_mode,'_of_',
                                                                             paste(quantitation_name_list, collapse="."),'_by_',
                                                                             input$quantitation_recode_group,'_across_',
                                                                             input$quantitation_comp_group,'_with_spot_number_',
                                                                             input$spot_total_number,'.csv')))})
        } else {
          try({utils::write.csv(v$quantitation_result[[2]], file.path(global$datapath,input$output_folder_name,
                                                                      'data_files',
                                                                      paste0('Regional_quant_table_',
                                                                             input$quantitation_mode,'_of_',
                                                                             paste(quantitation_name_list, collapse="."),'_by_',
                                                                             input$quantitation_facet_group,'_across_',
                                                                             input$quantitation_comp_group,'_with_spot_number_',
                                                                             input$spot_total_number,'.csv')))})
        }
      }
    }
    shinybusy::remove_modal_spinner()
  })

  output$quantitation_plot <- shiny::renderPlot({
    v$quantitation_result[[1]]
  }, width = shiny::reactive({input$quantitation_img_width}),
  height = shiny::reactive({input$quantitation_img_height}))

  output$quantitation_table <- DT::renderDataTable({
    DT::datatable(v$quantitation_result[[2]],
                  options=list(lengthMenu=c(5,10,20,40,80),pageLength=5))
  })

  ## CellDART analysis: cell type deconvolution
  shiny::observeEvent(v$sp_data, {
    shiny::updateSelectInput(session, "celldart_group", "Group to subset spots",
                      choices = v$sp_meta_list_factor)
  })
  shiny::observeEvent(v$sc_data, {
    shiny::updateSelectInput(session, "celldart_metadata_celltype",
                      "Group for classifying celltypes",
                      choices = v$sc_meta_list_factor)
  })
  shiny::observeEvent(c(v$sp_data, input$celldart_group),{
    temp <- v$sp_data
    if (!is.null(temp)){
      if (input$celldart_group %in% colnames(temp@meta.data)){
        shiny::updateSelectInput(session, "celldart_group_sel",
                          "Select spot clusters to include",
                          choices = levels(factor(eval(parse(text=paste0('temp$',
                                                                         input$celldart_group))))))
      }
    }
  })
  shiny::observeEvent(input$celldart_start, {
    shinybusy::show_modal_spinner()
    if (!is.null(v$sp_data)&!is.null(v$sc_data)){
      brain.tmp <- pred_cellf_celldart(sp_data=v$sp_data,sc_data=v$sc_data,
                                       outdir=file.path(global$datapath,input$output_folder_name),
                                       sp_subset=input$celldart_check_subset,
                                       spot.cluster.name=input$celldart_group,
                                       spot.cluster.of.interest=input$celldart_group_sel,
                                       metadata_celltype=input$celldart_metadata_celltype,
                                       conda.env.name='STquantool',gpu=TRUE,
                                       num_markers=input$celldart_num_markers,
                                       seed_num=0,
                                       nmix=input$celldart_nmix, npseudo=input$celldart_npseudo,
                                       alpha=input$celldart_alpha, alpha_lr=input$celldart_alpha_lr,
                                       emb_dim=input$celldart_emb_dim,
                                       batch_size=input$celldart_batch_size,
                                       n_iterations=input$celldart_n_iterations,
                                       init_train_epoch=input$celldart_init_train_epoch)
      v$sp_data <- brain.tmp
    }
    shinybusy::remove_modal_spinner()
  })



  ## Save image files
  # Dimplot
  shiny::observeEvent(input$sc_clust_save, {
    shiny::showModal(save_plot_wh_Modal(slider_input_name = "sc_clust_save_dpi",
                                 width_name = "sc_clust_width",
                                 height_name = "sc_clust_height",
                                 width_value=15, height_value=15,
                                 action_button_name = "sc_clust_save_start"))
  })
  shiny::observeEvent(input$sc_clust_save_start, {
    shinybusy::show_modal_spinner()
    v$dimplot
    if (!is.null(input$sc_cell_high_cluster)&input$sc_cell_high){
      cell_highlight <- paste(input$sc_cell_high_cluster, collapse = '_')
    } else {
      cell_highlight <- ""
    }
    try({ggplot2::ggsave(file.path(global$datapath,input$output_folder_name,
                          'data_files',
                          paste0(input$dimplot_radio,'_dimplot_',
                                 input$sc_group_var,'_',cell_highlight,
                                 '.png')),
                width = input$sc_clust_width,
                height = input$sc_clust_height, units = c("cm"),
                dpi = input$sc_clust_save_dpi, bg = "white")})
    shinybusy::remove_modal_spinner()
    shiny::removeModal()
  })

  # Frequency plot
  shiny::observeEvent(input$sc_freq_save, {
    shiny::showModal(save_plot_wh_Modal(slider_input_name = "sc_freq_save_dpi",
                                 width_name = "sc_freq_width",
                                 height_name = "sc_freq_height",
                                 width_value=15, height_value=20,
                                 action_button_name = "sc_freq_save_start"))
  })

  shiny::observeEvent(input$sc_freq_save_start, {
    shinybusy::show_modal_spinner()
    v$freq_boxplot[[1]]
    try({ggplot2::ggsave(file.path(global$datapath,input$output_folder_name,
                          'data_files',
                          paste0(input$freqplot_radio,'_freqplot_',
                                 input$sc_group_var_freq,'_',
                                 input$sc_cluster_var_freq,'.png')),
                width = input$sc_freq_width, height = input$sc_freq_height, units = c("cm"),
                dpi = input$sc_freq_save_dpi, bg = "white")})
    shinybusy::remove_modal_spinner()
    shiny::removeModal()
  })

  # Feature plot
  shiny::observeEvent(input$sc_feat_save, {
    shiny::showModal(save_plot_wh_Modal(slider_input_name = "sc_feat_save_dpi",
                                 width_name = "sc_feat_width",
                                 height_name = "sc_feat_height",
                                 width_value=15*input$sc_feat_ncol,
                                 height_value=15*(((length(v$sc_feat_list)-1)%/%input$sc_feat_ncol)+1),
                                 action_button_name = "sc_feat_save_start"))
  })
  shiny::observeEvent(input$sc_feat_save_start, {
    shinybusy::show_modal_spinner()
    v$sc_featureplot
    try({ggplot2::ggsave(file.path(global$datapath,input$output_folder_name,
                          'data_files',
                          paste0(input$featureplot_radio,'_featplot_',
                                 paste(v$sc_feat_list,collapse='_'),'.png')),
                width = input$sc_feat_width,
                height = input$sc_feat_height,
                units = c("cm"),
                dpi = input$sc_feat_save_dpi, bg = "white")})
    shinybusy::remove_modal_spinner()
    shiny::removeModal()
  })

  # Spatial cluster plot
  shiny::observeEvent(input$sp_cluster_save, {
    shiny::showModal(save_plot_wh_Modal(slider_input_name = "sp_cluster_save_dpi",
                                 width_name = "sp_cluster_width",
                                 height_name = "sp_cluster_height",
                                 width_value=input$sp_cluster_img_width/28.35,
                                 height_value=input$sp_cluster_img_height/28.35,
                                 action_button_name = "sp_cluster_save_start"))
  })
  shiny::observeEvent(input$sp_cluster_save_start, {
    shinybusy::show_modal_spinner()
    v$sc_clusterplot
    try({ggplot2::ggsave(file.path(global$datapath,input$output_folder_name,
                          'data_files',
                          paste0('spclustplot_seurat_clusters.png')),
                width = input$sp_cluster_width,
                height = input$sp_cluster_height, units = c("cm"),
                dpi = input$sp_cluster_save_dpi, bg = "white")})
    shinybusy::remove_modal_spinner()
    shiny::removeModal()
  })

  # Spatial feature plot
  shiny::observeEvent(input$sp_feat_save, {
    shiny::showModal(save_plot_wh_Modal(slider_input_name = "sp_feat_save_dpi",
                                 width_name = "sp_feat_width",
                                 height_name = "sp_feat_height",
                                 width_value=input$sp_feat_img_width/28.35,
                                 height_value=input$sp_feat_img_height/28.35,
                                 action_button_name = "sp_feat_save_start"))
  })
  shiny::observeEvent(input$sp_feat_save_start, {
    shinybusy::show_modal_spinner()
    v$sp_featureplot
    try({ggplot2::ggsave(file.path(global$datapath,input$output_folder_name,
                          'data_files',
                          paste0('spfeatplot_',
                                 paste(v$sp_feat_list,collapse='_'),'.png')),
                width = input$sp_feat_width,
                height = input$sp_feat_height, units = c("cm"),
                dpi = input$sp_feat_save_dpi, bg = "white")})
    shinybusy::remove_modal_spinner()
    shiny::removeModal()
  })

  # Violin plot
  shiny::observeEvent(input$vln_save, {
    shiny::showModal(save_plot_wh_Modal(slider_input_name = "vln_save_dpi",
                                 width_name = "vln_width",
                                 height_name = "vln_height",
                                 width_value=input$vln_img_width/28.35,
                                 height_value=input$vln_img_height/28.35,
                                 action_button_name = "vln_save_start"))
  })
  shiny::observeEvent(input$vln_save_start, {
    shinybusy::show_modal_spinner()
    if (!is.null(v$vlnplot)){
      patchwork::wrap_plots(v$vlnplot, ncol=input$vln_ncol)
    }
    try({ggplot2::ggsave(file.path(global$datapath,input$output_folder_name,
                          'data_files',
                          paste0(input$vlnplot_radio,'_vlnplot_',
                                 paste(v$vlnplot_feat_list,collapse='_'),'.png')),
                width = input$vln_width,
                height = input$vln_height, units = c("cm"),
                dpi = input$vln_save_dpi, bg = "white")})
    shinybusy::remove_modal_spinner()
    shiny::removeModal()
  })

  # Ridge plot
  shiny::observeEvent(input$ridge_save, {
    shiny::showModal(save_plot_wh_Modal(slider_input_name = "ridge_save_dpi",
                                 width_name = "ridge_width",
                                 height_name = "ridge_height",
                                 width_value=input$ridge_img_width/28.35,
                                 height_value=input$ridge_img_height/28.35,
                                 action_button_name = "ridge_save_start"))
  })
  shiny::observeEvent(input$ridge_save_start, {
    shinybusy::show_modal_spinner()
    v$ridgeplot
    try({ggplot2::ggsave(file.path(global$datapath,input$output_folder_name,
                          'data_files',
                          paste0(input$ridgeplot_radio,'_ridgeplot_',
                                 paste(v$ridge_feat_list,collapse='_'),'.png')),
                width = input$ridge_width,
                height = input$ridge_height, units = c("cm"),
                dpi = input$ridge_save_dpi, bg = "white")})
    shinybusy::remove_modal_spinner()
    shiny::removeModal()
  })


  # Volcano plot
  shiny::observeEvent(input$sc_deg_volcano_save, {
    shiny::showModal(save_plot_wh_Modal(slider_input_name = "sc_deg_volcano_save_dpi",
                                 width_name = "deg_volcano_width",
                                 height_name = "deg_volcano_height",
                                 width_value=13, height_value=15,
                                 action_button_name = "sc_deg_volcano_save_start"))
  })
  shiny::observeEvent(input$sc_deg_volcano_save_start, {
    shinybusy::show_modal_spinner()
    try({if (input$sc_check_deg_subset){
      grDevices::png(file.path(global$datapath,input$output_folder_name,
                               'data_files',
                               paste0(input$DEG_radio,'_volcanoplot_subset_',
                                      paste(input$sc_deg_subset_sel,collapse="_"),'_btw_',
                                      input$sc_deg_subset_int,'_',input$sc_deg_subset_ref,'.png')),
                     width = input$deg_volcano_width, height = input$deg_volcano_height,
                     units = 'cm', res = input$sc_deg_volcano_save_dpi,
                     bg = "white")
    } else {
      grDevices::png(file.path(global$datapath,input$output_folder_name,
                               'data_files',
                               paste0(input$DEG_radio,'_volcanoplot_',
                                      paste0(input$sc_deg_int,'_',
                                             input$sc_deg_ref),'.png')),
                     width = input$deg_volcano_width, height = input$deg_volcano_height,
                     units = 'cm', res = input$sc_deg_volcano_save_dpi,
                     bg = "white")
    }
      print(v$volcano)
      grDevices::dev.off()})
    shinybusy::remove_modal_spinner()
    shiny::removeModal()
  })

  # Enrich plot
  shiny::observeEvent(input$sc_deg_enrich_save, {
    shiny::showModal(save_plot_wh_Modal(slider_input_name = "sc_deg_enrich_save_dpi",
                                 width_name = "deg_enrich_width",
                                 height_name = "deg_enrich_height",
                                 width_value=38, height_value=11,
                                 action_button_name = "sc_deg_enrich_save_start"))
  })
  shiny::observeEvent(input$sc_deg_enrich_save_start, {
    shinybusy::show_modal_spinner()
    try({if (input$sc_check_deg_subset){
      grDevices::png(file.path(global$datapath,input$output_folder_name,
                               'data_files',
                               paste0(input$DEG_radio,'_',input$sc_deg_enrich_type,'_',
                                      input$sc_deg_enrich_species,'_enrichplot_subset_',
                                      paste(input$sc_deg_subset_sel,collapse="_"),'_btw_',
                                      input$sc_deg_subset_int,'_',input$sc_deg_subset_ref,'.png')),
                     width = input$deg_enrich_width, height = input$deg_enrich_height, units = 'cm',
                     res = input$sc_deg_enrich_save_dpi,
                     bg = "white")
    } else {
      grDevices::png(file.path(global$datapath,input$output_folder_name,
                               'data_files',
                               paste0(input$DEG_radio,'_',input$sc_deg_enrich_type,'_',
                                      input$sc_deg_enrich_species,'_enrichplot_',
                                      paste0(input$sc_deg_int,'_',
                                             input$sc_deg_ref),'.png')),
                     width = input$deg_enrich_width, height = input$deg_enrich_height, units = 'cm',
                     res = input$sc_deg_enrich_save_dpi,
                     bg = "white")
    }
      print(v$dotplot)
      grDevices::dev.off()})
    shinybusy::remove_modal_spinner()
    shiny::removeModal()
  })

  # Quantitation
  shiny::observeEvent(input$quantitation_save, {
    shiny::showModal(save_plot_wh_Modal(slider_input_name = "quantitation_save_dpi",
                                 width_name = "quantitation_width",
                                 height_name = "quantitation_height",
                                 width_value=input$quantitation_img_width/28.35,
                                 height_value=input$quantitation_img_height/28.35,
                                 action_button_name = "quantitation_save_start"))
  })
  shiny::observeEvent(input$quantitation_save_start, {
    shinybusy::show_modal_spinner()
    v$quantitation_result[[1]]

    if (input$quantitation_cellf_mode=="metadata"){
      quantitation_cellf <- input$quantitation_cellf1
    } else if (input$quantitation_cellf_mode=="genes"){
      quantitation_cellf <- input$quantitation_cellf2
    }
    try({if (input$quantitation_agg_mode){
      ggplot2::ggsave(file.path(global$datapath,input$output_folder_name,
                       'data_files',
                       paste0('Regional_quantplot_',
                              input$quantitation_mode,'_of_',
                              paste(quantitation_cellf, collapse="."),'_by_',
                              input$quantitation_recode_group,'_across_',
                              input$quantitation_comp_group,'_with_spot_number_',
                              input$spot_total_number,'.png')),
             width = input$quantitation_width,
             height = input$quantitation_height, units = c("cm"),
             dpi = input$quantitation_save_dpi, bg = "white")
    } else {
      ggplot2::ggsave(file.path(global$datapath,input$output_folder_name,
                       'data_files',
                       paste0('Regional_quantplot_',
                              input$quantitation_mode,'_of_',
                              paste(quantitation_cellf, collapse="."),'_by_',
                              input$quantitation_facet_group,'_across_',
                              input$quantitation_comp_group,'_with_spot_number_',
                              input$spot_total_number,'.png')),
             width = input$quantitation_width,
             height = input$quantitation_height, units = c("cm"),
             dpi = input$quantitation_save_dpi, bg = "white")
    }})
    shinybusy::remove_modal_spinner()
    shiny::removeModal()
  })


  # Saving processed single-cell or spatial dataset
  shiny::observeEvent(input$data_save, {
    if (input$save_radio=="Single-cell"){
      shiny::showModal(save_files_Modal(text="single-cell", text_input_name="sc_save_file_name",
                                 file_save_name="sc_data", action_button_name = "ok_sc"))
    } else if (input$save_radio=="Spatial"){
      shiny::showModal(save_files_Modal(text="spatial", text_input_name="sp_save_file_name",
                                 file_save_name="sp_data", action_button_name = "ok_sp"))
    } else if (input$save_radio=="Genes: stored"){
      shiny::showModal(save_files_Modal(text="stored gene list", text_input_name="sp_save_stored_gene",
                                 file_save_name="stored_gene_list", action_button_name = "ok_stored_gene"))
    } else if (input$save_radio=="Genes: abundance"){
      shiny::showModal(save_files_Modal(text="abundance gene list", text_input_name="sp_save_ab_gene",
                                 file_save_name="ab_gene_list", action_button_name = "ok_ab_gene"))
    }
  })
  shiny::observeEvent(input$ok_sc, {
    shinybusy::show_modal_spinner()
    try({saveRDS(v$sc_data, file.path(global$datapath,input$output_folder_name,
                                      paste0(input$sc_save_file_name,'.rds')))})
    shinybusy::remove_modal_spinner()
    shiny::removeModal()
    output$cmd <- shiny::renderText({paste0('The single-cell data was saved as RDS in ',
                                     file.path(global$datapath,input$output_folder_name))})
  })
  shiny::observeEvent(input$ok_sp, {
    shinybusy::show_modal_spinner()
    try({saveRDS(v$sp_data, file.path(global$datapath,input$output_folder_name,
                                      paste0(input$sp_save_file_name,'.rds')))})
    shinybusy::remove_modal_spinner()
    shiny::removeModal()
    output$cmd <- shiny::renderText({paste0('The spatial data was saved as RDS in ',
                                     file.path(global$datapath,input$output_folder_name))})
  })
  shiny::observeEvent(input$ok_stored_gene, {
    shinybusy::show_modal_spinner()
    if (!is.null(v$gene_upload)){
      try({utils::write.table(t(plyr::ldply(v$gene_upload, rbind)),
                              file = file.path(global$datapath,input$output_folder_name,
                                               paste0(input$sp_save_stored_gene,'.csv')),
                              sep=',', col.names=FALSE)})
    }
    shinybusy::remove_modal_spinner()
    shiny::removeModal()
    output$cmd <- shiny::renderText({paste0('The gene lists were saved as csv in ',
                                     file.path(global$datapath,input$output_folder_name))})
  })
  shiny::observeEvent(input$ok_ab_gene, {
    shinybusy::show_modal_spinner()
    if (!is.null(v$sc_gene_list)&!is.null(v$sp_gene_list)){
      try({utils::write.table(t(plyr::ldply(list("Single.cell.top3000"=v$sc_gene_list,
                                                 "Spatial.top3000"=v$sp_gene_list), rbind)),
                              file.path(global$datapath,input$output_folder_name,
                                        paste0(input$sp_save_ab_gene,'.csv')), sep=',',
                              col.names=FALSE)})
    } else if (!is.null(v$sc_gene_list)&is.null(v$sp_gene_list)){
      try({utils::write.csv(data.frame("Single.cell.top3000"=v$sc_gene_list),
                            file.path(global$datapath,input$output_folder_name,
                                      paste0(input$sp_save_ab_gene,'.csv')), col.names = T)})
    } else if (is.null(v$sc_gene_list)&!is.null(v$sp_gene_list)){
      try({utils::write.csv(data.frame("Spatial.top3000"=v$sp_gene_list),
                            file.path(global$datapath,input$output_folder_name,
                                      paste0(input$sp_save_ab_gene,'.csv')), col.names = T)})
    }
    shinybusy::remove_modal_spinner()
    shiny::removeModal()
    output$cmd <- shiny::renderText({paste0('The abundance list was saved as csv in ',
                                     file.path(global$datapath,input$output_folder_name))})
  })


  # Load single-cell or spatial data in RDS format
  shinyFiles::shinyFileChoose(input, 'data_load',
                  roots = c(home = '/home/nmadmin', wd ='.'),
                  filetypes = c('',"txt","tsv","csv","rds","png","h5","h5ad"))

  data_load <- shiny::reactive(input$data_load)

  shiny::observeEvent(input$data_load, {
    next_step <- FALSE
    if (is.null(names(data_load()))){
    } else if (data_load()$root[[1]]=='wd'){
      load_path <- file.path(paste(c(getwd(),unlist(data_load()$files)), collapse=.Platform$file.sep))
      next_step <- TRUE
    } else if (data_load()$root[[1]]=='home') {
      home <- normalizePath("/home/nmadmin")
      load_path <- file.path(paste(c(home,unlist(data_load()$files)), collapse=.Platform$file.sep))
      next_step <- TRUE
    }

    if (next_step){
      if (input$save_radio=="Single-cell"){
        shinybusy::show_modal_spinner()
        v$sc_data <- readRDS(load_path)
        shinybusy::remove_modal_spinner()
        output$cmd <- shiny::renderText({paste0("'",load_path,"' was loaded")})
      } else if (input$save_radio=="Spatial"){
        shinybusy::show_modal_spinner()
        v$sp_data <- readRDS(load_path)
        shinybusy::remove_modal_spinner()
        output$cmd <- shiny::renderText({paste0("'",load_path,"' was loaded")})
      } else if (input$save_radio=="Genes: stored"){
        if (!is.null(v$sp_gene_list)){
          load_list <- lapply(as.list(utils::read.csv(load_path)), FUN = function(x) intersect(x, v$sp_gene_list))
          if (!is.null(v$sc_gene_list)){load_list <- lapply(load_list, FUN = function(x) intersect(x, v$sc_gene_list))}
          output$cmd <- shiny::renderText({paste0("'",load_path,"' was loaded")})
        } else if (!is.null(v$sc_gene_list)){
          load_list <- lapply(as.list(utils::read.csv(load_path)), FUN = function(x) intersect(x, v$sc_gene_list))
          output$cmd <- shiny::renderText({paste0("'",load_path,"' was loaded")})
        } else {
          load_list <- list()
          output$cmd <- shiny::renderText({paste0("Neither spatial nor single-cell data have been uploaded\nEmpty gene list.")})
        }
        v$gene_upload <- c(v$gene_upload, load_list)
      } else {
        output$cmd <- shiny::renderText({paste0("Loading is possible when 'single-cell', 'spatial', or 'Genes: stored' are selected")})
      }
    }
  }
  )

  ## Convert file to 10Xformat
  shiny::observeEvent(input$convert_file_to_sparse, {
    shiny::showModal(load_files_Modal(input=input, output=output, session=session,
                               text="csv or txt",
                               text_for_purpose="Will you convert the ",text_for_add=" file?",
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
                               action_button_name1 = "check_load_file",
                               action_button_name2 = "convert_file_ok",
                               result_table = "table_check"))
  })

  # Choose the files to convert
  shinyFiles::shinyFileChoose(input, 'choose_file_to_convert',
                  roots = c(home = '/home/nmadmin', wd ='.'),
                  filetypes = c('',"txt","csv"))

  choose_file_to_convert <- shiny::reactive(input$choose_file_to_convert)

  # Check the table contents
  shiny::observeEvent(input$check_load_file, {
    next_step <- F
    if (is.null(names(choose_file_to_convert()))){
    } else if (choose_file_to_convert()$root[[1]]=='wd'){
      path_tmp <- unlist(choose_file_to_convert()$files)
      v$load_path <- file.path(paste(c(getwd(),path_tmp),
                                     collapse=.Platform$file.sep))
      next_step <- T
    } else if (choose_file_to_convert()$root[[1]]=='home') {
      home <- normalizePath("/home/nmadmin")
      path_tmp <- unlist(choose_file_to_convert()$files)
      v$load_path <- file.path(paste(c(home,path_tmp),
                                     collapse=.Platform$file.sep))
      next_step <- T
    }
    if (next_step){
      v$delim_type <- ifelse(input$radio_delim_type=="Comma",",",
                             ifelse(input$radio_delim_type=="Tab","\t",
                                    ifelse(input$radio_delim_type=="Space"," "," ")))
      v$skip_row <- input$row_num_to_skip
      if (input$file_load_shift_col_check) {
        v$output_col <- as.character(data.table::fread(v$load_path, sep=v$delim_type,
                                                       header=FALSE,skip=v$skip_row,
                                                       nrows=1))
      }
      read_file <- data.table::fread(v$load_path,
                                     header = input$file_load_header_check,
                                     skip = v$skip_row,
                                     sep = v$delim_type, data.table=FALSE, showProgress=TRUE)
      if (input$file_load_shift_col_check){
        if (dim(read_file)[2]==(length(v$output_col)+1)){
          v$output_col <- c("X", v$output_col)
          colnames(read_file) <- v$output_col
        }
      }
      if (input$file_load_transpose_check){
        read_file <- t(read_file %>% as.data.frame() %>%
                         tibble::column_to_rownames(var = base::colnames(read_file)[input$col_num_to_rowname]))
      } else {
        read_file <- read_file %>% as.data.frame() %>%
          tibble::column_to_rownames(var = base::colnames(read_file)[input$col_num_to_rowname])
      }
      if (dim(read_file)[2]>3){read_file <- read_file[,c(1:3)]}
      output$table_check <- DT::renderDataTable({
        DT::datatable(as.data.frame(read_file),
                      options=list(lengthMenu=c(5,10,20,40,80),pageLength=5))
      })
    }
  })

  ## Convert to 10X format
  shiny::observeEvent(input$convert_file_ok, {
    shinybusy::show_modal_spinner()
    if (!is.null(v$load_path)){
      read_total <- data.table::fread(v$load_path,
                                      header = input$file_load_header_check,
                                      skip = v$skip_row,
                                      sep = v$delim_type, data.table=FALSE, showProgress=TRUE)
      if (!is.null(v$output_col)){colnames(read_total) <- v$output_col}
      if (input$file_load_transpose_check){
        read_total <- t(read_total %>% as.data.frame() %>%
                          tibble::column_to_rownames(var = base::colnames(read_total)[input$col_num_to_rowname]))
      } else {
        read_total <- read_total %>% as.data.frame() %>%
          tibble::column_to_rownames(var = base::colnames(read_total)[input$col_num_to_rowname])
      }

      if (!is.null(read_total)){
        try({
          read_row <- rownames(read_total)
          read_col <- colnames(read_total)
          read_total <- sapply(read_total, as.numeric)
          read_total <- Matrix::Matrix(read_total, sparse=TRUE)
          dimnames(read_total) = list(read_row, read_col)

          save_path <- file.path(global$datapath,input$output_folder_name,input$file_load_name)
          dir.create(save_path)
          # Create HDF5 File
          if (!file.exists(file.path(save_path,
                                     "sparse_matrix.rds"))){
            saveRDS(read_total,
                    file = file.path(save_path, "sparse_matrix.rds"))
            output$cmd <- shiny::renderText({paste0("'",file.path(save_path,
                                                           "sparse_matrix.rds"),"' was generated")})
          } else {
            output$cmd <- shiny::renderText({paste0("'",file.path(save_path,
                                                           "sparse_matrix.rds"),"' already exists")})
          }
        })
      }
    }
    shiny::removeModal()
    shinybusy::remove_modal_spinner()
  })


  # Stop shiny after closing the page
  shiny::observeEvent(input$close, {
    shinyjs::js$closeWindow()
    shiny::stopApp()
  })
}

#' Function to run STquantools app
#' @description ST analysis tool to visualize and quantify multiple datasets
#' @return Starts shiny application for STquantools
#' @examples
#' STquantool::run_app()
#' @export
run_app <- function(){
  shiny::shinyApp(ui = ui, server = server)
}

# Run shiny app
run_app()
