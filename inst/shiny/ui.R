library(shiny)
library(dplyr)
library(DT)

options(shiny.maxRequestSize = 50*1024*1024^2)
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

ui <- shiny::navbarPage(title = "STquantool", theme = shinythemes::shinytheme("spacelab"),
                 shiny::tabPanel(title = "Info",
                          shiny::h2(style = "font-family:San-serif", "STquantool"),
                          shiny::br(),
                          shiny::h4(style = "font-family:San-serif",
                             paste0("ST analysis tool to visualize and quantify multiple datasets")),
                          # Image insertion
                          shiny::img(height=400,width=700,src='Main.png'),
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
                                                     shiny::sliderInput(inputId = "nCount_RNA",
                                                                 label = "nCount_RNA: threshold",
                                                                 value = 100, min = 0, max = 1000, step=10),
                                                     shiny::sliderInput(inputId = "percent.mt",
                                                                 label = "percent.mt: threshold",
                                                                 value = 5, min = 0, max = 100, step=1),
                                                     shiny::sliderInput(inputId = "nFeature_RNA",
                                                                 label = "nFeature_RNA: threshold",
                                                                 value = 300, min = 0, max = 1000, step=10)
                                                   ),
                                                   shiny::wellPanel(
                                                     shiny::radioButtons("qc_radio","Histogram choices",
                                                                  choices = list("nCount_RNA", "nFeature_RNA", "percent.mt"),
                                                                  selected = "nCount_RNA"),
                                                     shiny::sliderInput(inputId = "histo_breaks",
                                                                 label = "Histogram breaks",
                                                                 value = 5000, min = 0, max = 10000, step=100),
                                                     shiny::sliderInput(inputId = "histo_xmax",
                                                                 label = "Histogram limits",
                                                                 value = 2000, min = 0, max = 3000, step=100)
                                                   ),
                                                   shiny::actionButton(inputId = "qc_start", label = "QC plot")
                                          ),
                                          shiny::tabPanel(title = "Spatial",
                                                   shiny::wellPanel(
                                                     shiny::sliderInput(inputId = "sp_nCount_Spatial",
                                                                 label = "nCount_Spatial: threshold",
                                                                 value = 100, min = 0, max = 1000, step=10),
                                                     shiny::sliderInput(inputId = "sp_percent.mt",
                                                                 label = "percent.mt: threshold",
                                                                 value = 5, min = 0, max = 100, step=1),
                                                     shiny::sliderInput(inputId = "sp_nFeature_Spatial",
                                                                 label = "nFeature_RNA: threshold",
                                                                 value = 300, min = 0, max = 1000, step=10)
                                                   ),
                                                   shiny::wellPanel(
                                                     shiny::radioButtons("qc_sp_radio","Histogram & Visualization choices",
                                                                  choices = list("nCount_Spatial", "nFeature_Spatial", "percent.mt"),
                                                                  selected = "nCount_Spatial"),
                                                     shiny::sliderInput(inputId = "histo_sp_breaks",
                                                                 label = "Histogram breaks",
                                                                 value = 5000, min = 0, max = 10000, step=100),
                                                     shiny::sliderInput(inputId = "histo_sp_xmax",
                                                                 label = "Histogram limits",
                                                                 value = 2000, min = 0, max = 3000, step=100),
                                                     shiny::sliderInput(inputId = "tissue_qc_minmax",
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
                              shiny::sliderInput(inputId = "n_var_features",
                                          label = "Number of HVGs",
                                          value = 2000, min = 1000, max = 5000, step=10),
                              shiny::sliderInput(inputId = "n_integ_features",
                                          label = "Number of genes in integration",
                                          value = 2000, min = 1000, max = 5000, step=10),
                              shiny::sliderInput(inputId = "integ_dim",
                                          label = "Number of dimensions used in integration",
                                          value = 50, min = 0, max = 100, step=1),
                              shiny::sliderInput(inputId = "cluster_dim",
                                          label = "Number of dimension in clustering",
                                          value = 30, min = 0, max = 50, step=1),
                              shiny::sliderInput(inputId = "cluster_resolution",
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
                                                                shiny::sliderInput(inputId = "sc_dot_size",
                                                                            label = "Size of the dot",
                                                                            value = 0, min = 0, max = 2, step=0.05),
                                                                shiny::checkboxInput("sc_vis_label","Label on", value = TRUE),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.sc_vis_label==true",
                                                                  shiny::wellPanel(
                                                                    shiny::sliderInput(inputId = "sc_label_size",
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
                                                                  shiny::sliderInput(inputId = "sc_freq_x_angle",
                                                                              label = "Angle of x-axis text",
                                                                              value = 0, min=0, max=90, step=5),
                                                                  shiny::sliderInput(inputId = "sc_freq_x_size",
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
                                             shiny::tabPanel("Table", DT::dataTableOutput("sc_freqstats") %>% shinycssloaders::withSpinner(color="#0000FF"))
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
                                         shiny::sliderInput(inputId = "sc_feat_minmax",
                                                     label = "Min max value",
                                                     value = c(0,3), min = 0, max = 10, step=0.1),
                                         shiny::textInput(inputId = "sc_feat_color",
                                                   label = "Color",
                                                   value= "blue"),
                                         shiny::numericInput(inputId = "sc_feat_ncol",
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
                                             shiny::sliderInput(inputId = "sp_cluster_alpha",
                                                         label = "Transparency of spots",
                                                         value = c(1,1), min=0, max=1, step=0.1),
                                             shiny::sliderInput(inputId = "sp_cluster_image.alpha",
                                                         label = "Transparency of tissue",
                                                         value = 0.6, min=0, max=1, step=0.1)
                                           )
                                         ),

                                         shiny::checkboxInput("sp_check_cluster_vis","Visualization", value = FALSE),
                                         shiny::conditionalPanel(
                                           condition = "input.sp_check_cluster_vis==true",
                                           shiny::wellPanel(
                                             shiny::numericInput(inputId = "sp_cluster_ncol",
                                                          label = "Number of columns",
                                                          value = 4, min = 1, max=20),
                                             shiny::checkboxInput("sp_cluster_label","Label on", value=TRUE),
                                             shiny::conditionalPanel(
                                               condition = "input.sp_cluster_label==true",
                                               shiny::wellPanel(
                                                 shiny::sliderInput(inputId = "sp_cluster_label_size",
                                                             label = "Size of the label",
                                                             value = 3, min=0, max=10, step = 1)
                                               )
                                             ),
                                             shiny::sliderInput(inputId = "sp_cluster_pt.size.factor",
                                                         label = "Size of the spot",
                                                         value = 1.6, min=0, max=3, step = 0.1),
                                             shiny::sliderInput(inputId = "sp_cluster_img_width",
                                                         label = "Width of image panel",
                                                         value = 1000, min=0, max=2000, step=10),
                                             shiny::sliderInput(inputId = "sp_cluster_img_height",
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
                                         shiny::sliderInput(inputId = "sp_feat_minmax",
                                                     label = "Min max value",
                                                     value = c(0,3), min = 0, max = 10, step=0.1),
                                         shiny::checkboxInput("sp_check_feat_transparency","Transparency", value = FALSE),
                                         shiny::conditionalPanel(
                                           condition = "input.sp_check_feat_transparency==true",
                                           shiny::wellPanel(
                                             shiny::sliderInput(inputId = "sp_feat_alpha",
                                                         label = "Transparency of spots",
                                                         value = c(1,1), min=0, max=1, step=0.1),
                                             shiny::sliderInput(inputId = "sp_feat_image.alpha",
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
                                             shiny::numericInput(inputId = "sp_feat_ncol",
                                                          label = "Number of columns",
                                                          value = 4, min = 1, max=20),
                                             shiny::numericInput("sp_title_size", "Size of the title", value=10, min=0),
                                             shiny::sliderInput(inputId = "sp_feat_pt.size.factor",
                                                         label = "Size of the spot",
                                                         value = 1.8, min=0, max=3, step = 0.1),
                                             shiny::sliderInput(inputId = "sp_feat_img_width",
                                                         label = "Width of image panel",
                                                         value = 900, min=0, max=2000, step=10),
                                             shiny::sliderInput(inputId = "sp_feat_img_height",
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
                                                      shiny::numericInput(inputId = "sp_feat_by_n",
                                                                   label = "Number of features to save in each plot",
                                                                   value = 2, min = 0, max = 10),
                                                      shiny::textInput("sp_save_name", "Name of the file", value='feats'),
                                                      shiny::sliderInput(inputId = "sp_save_by_n_width",
                                                                  label = "Width in cm",
                                                                  value = 18, min=0, max=50, step=1),
                                                      shiny::sliderInput(inputId = "sp_save_by_n_height",
                                                                  label = "Height in cm",
                                                                  value = 11, min=0, max=50, step=1),
                                                      shiny::sliderInput(inputId = "sp_upload_save_dpi",
                                                                  label = "dpi to save image",
                                                                  value = 100, min = 0, max=500, step=10),
                                                      shiny::actionButton("sp_feat_save_by_n", "Save multiple plots by n"),
                                                      shiny::actionButton("sp_feat_upload","Save gene list")
                                                    ),
                                                    shiny::wellPanel(
                                                      DT::dataTableOutput("sp_csv_table") %>% shinycssloaders::withSpinner(color="#0000FF")
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
                                             shiny::sliderInput(inputId = "vln_img_width",
                                                         label = "Width of image panel",
                                                         value = 1000, min=0, max=2000, step=10),
                                             shiny::sliderInput(inputId = "vln_img_height",
                                                         label = "Height of image panel",
                                                         value = 800, min=0, max=2000, step=10),
                                             shiny::numericInput(inputId = "vln_ncol",
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
                                         shiny::sliderInput(inputId = "ridge_feat_minmax",
                                                     label = "Min max value",
                                                     value = c(-0.5,6), min = -1, max = 10, step=0.1),
                                         shiny::sliderInput(inputId = "ridge_alpha",
                                                     label = "Transparency of filled color",
                                                     value = 0, min=0, max=1, step=0.1),
                                         shiny::checkboxInput("ridge_check_vis","Visualization", value = FALSE),
                                         shiny::conditionalPanel(
                                           condition = "input.ridge_check_vis==true",
                                           shiny::wellPanel(
                                             shiny::sliderInput(inputId = "ridge_img_width",
                                                         label = "Width of image panel",
                                                         value = 1000, min=0, max=2000, step=10),
                                             shiny::sliderInput(inputId = "ridge_img_height",
                                                         label = "Height of image panel",
                                                         value = 800, min=0, max=2000, step=10),
                                             shiny::numericInput(inputId = "ridge_feat_ncol",
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
                                             shiny::sliderInput(inputId = "sc_marker_rfTrees",
                                                         label = "Number of trees",
                                                         value = 1000, min=0, max=5000, step=100),
                                             shiny::sliderInput(inputId = "sc_median_exp_level",
                                                         label = "Median expression level for removing negative markers",
                                                         value = 0, min=-5, max=5, step=0.1),
                                             shiny::sliderInput(inputId = "sc_genes_to_testing",
                                                         label = "Number of binary genes to be tested by permutations",
                                                         value = 6, min=0, max=10, step=1),
                                             shiny::sliderInput(inputId = "sc_beta_value",
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
                                         DT::dataTableOutput("sc_marker_table") %>% shinycssloaders::withSpinner(color="#0000FF")
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
                                         DT::dataTableOutput("sc_deg_table") %>% shinycssloaders::withSpinner(color="#0000FF")
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
                                           shiny::tabPanel("Table", DT::dataTableOutput("sc_deg_filter_table") %>% shinycssloaders::withSpinner(color="#0000FF"))
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
                                         shiny::numericInput(inputId = "sc_deg_enrich_nshow",
                                                      label = "Number of terms to show",
                                                      value = 10, min = 1, max=50),
                                         shiny::actionButton(inputId = "sc_deg_enrich_start", label = "Plot"),
                                         shiny::actionButton("sc_deg_enrich_save","Save")
                                       ),
                                       shiny::mainPanel(
                                         shiny::tabsetPanel(
                                           shiny::tabPanel("Plot", shiny::plotOutput("sc_deg_enrich") %>% shinycssloaders::withSpinner(color="#0000FF")),
                                           shiny::tabPanel("Table", DT::dataTableOutput("sc_deg_enrich_table") %>% shinycssloaders::withSpinner(color="#0000FF"))
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
                                         DT::dataTableOutput("module_score_table") %>% shinycssloaders::withSpinner(color="#0000FF")
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
                                             shiny::sliderInput(inputId = "recluster_n_var_features",
                                                         label = "Number of HVGs",
                                                         value = 2000, min = 1000, max = 5000, step=10),
                                             shiny::sliderInput(inputId = "recluster_n_integ_features",
                                                         label = "Number of genes in integration",
                                                         value = 2000, min = 1000, max = 5000, step=10),
                                             shiny::sliderInput(inputId = "recluster_integ_dim",
                                                         label = "Number of dimensions used in integration",
                                                         value = 50, min = 0, max = 100, step=1),
                                             shiny::sliderInput(inputId = "recluster_cluster_dim",
                                                         label = "Number of dimension in clustering",
                                                         value = 30, min = 0, max = 50, step=1),
                                             shiny::sliderInput(inputId = "recluster_cluster_resolution",
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
                                             shiny::sliderInput(inputId = "quantitation_img_width",
                                                         label = "Width of image panel",
                                                         value = 1000, min=0, max=2000, step=10),
                                             shiny::sliderInput(inputId = "quantitation_img_height",
                                                         label = "Height of image panel",
                                                         value = 800, min=0, max=2000, step=10),
                                             shiny::numericInput(inputId = "quantitation_feat_ncol",
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
                                           shiny::tabPanel("Table", DT::dataTableOutput("quantitation_table") %>% shinycssloaders::withSpinner(color="#0000FF"))
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
                                shiny::sliderInput(inputId = "celldart_num_markers",
                                            label = "Number of markers for each celltype",
                                            value = 10, min=1, max=50, step=1),
                                shiny::sliderInput(inputId = "celldart_nmix",
                                            label = "Number of cells in a pseudospot",
                                            value = 8, min=1, max=20, step=1),
                                shiny::sliderInput(inputId = "celldart_npseudo",
                                            label = "Number of pseudospots",
                                            value = 20000, min=10000, max=400000, step=1000),
                              ),
                              shiny::checkboxInput("celldart_train_param","Training parameters",value=FALSE),
                              shiny::conditionalPanel(
                                condition = "input.celldart_train_param==true",
                                shiny::wellPanel(
                                  shiny::sliderInput(inputId = "celldart_alpha",
                                              label = "Domain classifier loss weight",
                                              value = 0.6, min=0.1, max=10, step=0.1),
                                  shiny::sliderInput(inputId = "celldart_alpha_lr",
                                              label = "Domain classifier learning rate",
                                              value = 5, min=0.1, max=10, step=0.1),
                                  shiny::sliderInput(inputId = "celldart_emb_dim",
                                              label = "Feature embedder dimension",
                                              value = 64, min=2, max=512, step=2),
                                  shiny::sliderInput(inputId = "celldart_batch_size",
                                              label = "Feature embedder dimension",
                                              value = 512, min=64, max=1024, step=2),
                                  shiny::sliderInput(inputId = "celldart_n_iterations",
                                              label = "Number of iterations",
                                              value = 3000, min=1000, max=10000, step=100),
                                  shiny::sliderInput(inputId = "celldart_init_train_epoch",
                                              label = "Initial training number",
                                              value = 10, min=1, max=30, step=1)
                                )
                              ),
                              shiny::actionButton(inputId = "celldart_start", label = "Start")
                            )
                          )
                 )
)
