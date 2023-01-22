# STquantool
ST analysis tool to visualize and quantify multiple datasets  
* Cite as: Lee EJ, Bae S, Suh M, Choi H, Choi Y, Hwang DW, Lee DS. Spatial transcriptomic brain imaging reveals the effects of immunomodulation therapy upon specific regional brain cells in mouse dementia model. bioRxiv. 2023.  
* https://doi.org/10.1101/2023.01.20.524845  

## Installation and running
### 1. Using in R
```Plain Text
devtools::install_github('bsungwoo/STquantool', force = T)
STquantool::run_app()
```
### 2. Standalone app (packaged with [electron-packager](https://github.com/electron/electron-packager))
Please download file: [STquantool_v1_windows.exe](https://github.com/bsungwoo/STquantool/releases/download/v1.0.1/STquantool_v1_windows.7z)  

## Key packages
** Python  
NSForest 3.0: Copied the code to ./inst/python/NSForest_v3.py  
** R  
For details, please refer to DESCRIPTION file: [R package requirements](https://github.com/bsungwoo/STquantool/blob/main/DESCRIPTION)  

## Usage
The R shiny application panel consists of main, upload, visualization, and analysis sections.  

### 1. Load or Save files:  
#### A. Set working directory and assign output folder  
(1) Click 'Find directory' to browse & select directory and click 'Set working directory' to fix the working directory.  
(2) Put name for the output folder and click 'Assign output folder'. All the results will be saved in '<name of output folder>/data_files'.  

#### B. To upload Seurat object saved as spatial .rds format or Gene lists saved in csv format. (<500 MB)  
To upload Seurat object saved in rds format, browse the rds file by selecting the data type and clicking 'Data load'.  

#### C. To convert tsv/csv/txt format file to count matrix  
(1) Click 'Convert' button in 'Convert to sparse matrix (.rds)'. A window will appear.  
(2) Click 'Choose' to search for the file to upload.  
(3) For the single-cell or spatial count matrix, rownames should be gene names and column names should be barcode names.  
(4) First to check the data composition, click 'Check' to explore the contents (~10rows) of the file uploaded.  
(5) Change the delimeter if it is not correct.  
(6) Check if the matrix should be transposed.  
(7) If the name of the first column is not empty or wrongly assigned (normally, it should be empty or non-meaningful character such as .X) and the column should be shifted.  
(8) If you finished checking, then click 'Convert' button to generate sparse matrix in .rds format.  

#### D. Save processed data  
'Data save' button in 'Upload & Save' will automatically extract and save the single-cell/spatial rds files, stored gene lists, and abundance gene lists.  

### 2. QC process  
QC process is possible if the file is located in 10X-formatted directory.  
(1) Select the 10X-formatted directory for the investigation.  
(2) The directory should contain filtered_feature_bc_matrix folder containing ("barcodes.tsv.gz","features.tsv.gz","matrix.mtx.gz"), filtered_feature_bc_matrix.h5, or sparse_matrix.rds (sparse matrix rds file generated in B) in the directory.  

### 3. Preprocessing with reciprocal PCA  
Preprocessing single or multiple datasets.  
(1) Select integration directory to select the uppermost directory that contains the data files in 10X-format or sparse_matrix.rds.  
(2) Click 'Check Files' to search for the appropriate files inside the given directory.  
(3) Select the data format to integrate and select files to include.  
(4) if there is a reference dataset, then select the files to use as a reference in reciprocal PCA. If not, leave as an empty box.   
(5) Assign names for the each dataset, and enter the lower threshold and upper threshold for the total number of genes and mitochondrial gene percentage. If empty, then the threshold will not be applied.  
(6) Check the parameters and click 'Integration start' to start integration process.  

### 3. Visualization  
(1) Explore the visualization tabs.  
  Dimension/Freuqency plot, Feature plot, Spatial cluster/feature plot, Violin plot, Ridge plot  
(2) All the plots can be saved by clicking 'Save' button.  

### 4. Marker & DEG  
(1) Find cluster markers using Wilcoxon test or NSForest(v3) algorithm.  
(2) Find differentially expressed genes (DEGs) with logFC or average expression threshold in the cluster.  
(3) Draw volcano plots to visualize DEGs found in the (2).  
(4) Draw enrichment plot for the GO/KEGG terms with the extracted DEGs in (2).  

### 5. Utility  
#### A. Annotation of cell or spot clusters  
(1) Click the data format and group to annotate.  
(2) check the elements in the group and assing the new group name and the new element name.  
(3) To recode the existing group to larger group, then check 'Recode the group and save'.  

#### B. Module score  
(1) Click the data format and assign the name of the module score.  
(2) Import csv file with the feature list or choose among the saved gene list to select the genes for module scores.  
(3) Click 'Generate' to make module scores. After generating the score, the score can be visualized in the 3. Visualization tab.  

#### C. Subest the dataset  
(1) Click the data format and select group for the subsetting.  
(2) Choose clusters to subset and click 'recluster' if reclustering is needed.  
(3) Click 'Subset' to proceed with the subsetting procedure.  

#### D. Quantify the dataset  
(1) Select calculation method, comparison group (x-axis), and data values.  
(2) To aggregate the dataset into the recoded group generated in A, then check the box (aggregate to boxplot).  
(3) Select facet group or split/recode group pairs.  
(4) Check visualize values on spot or total spot number to additionally visualize the given values.  
(5) Click start to generate the plot.  

### 6. Cell type deconvolution with CellDART  
(1) Select the column name of metadata representing cell types in 'Group for classifying celltypes'.    
(2) Select the number of marker genes per cell type (default: 20), number of cells in a pseudospot (default: 8), and number of pseudospots (generally optimal in the range of 5~10 times the number of real spots).  
(3) Training parameters may be changed, but it is recommended to run the CellDART with default parameters.  
(4) Click the 'Start' button to start the analysis. Explore the results using 'Visualization' and 'Utility-Quantitation' tabs.  

## Key reference articles  
1. Aevermann B, Zhang Y, Novotny M, Keshk M, Bakken T, Miller J, Hodge R, Lelieveldt B, Lein E, Scheuermann RH. A machine learning method for the discovery of minimum marker gene combinations for cell type identification from single-cell RNA sequencing. Genome Res. 2021;31(10):1767-1780.    
2. Hao Y, Hao S, Andersen-Nissen E, Mauck III WM, Zheng S, Butler A, Lee MJ, Wilk AJ, Darby C, Zager M, Hoffman P. Integrated analysis of multimodal single-cell data. Cell. 2021;184(13):3573-87.
3. Bae S, Na KJ, Koh J, Lee DS, Choi H, Kim YT. CellDART: cell type inference by domain adaptation of single-cell and spatial transcriptomic data. Nucleic Acids Res. 2022;50(10):e57.
4. Wu T, Hu E, Xu S, Chen M, Guo P, Dai Z, Feng T, Zhou L, Tang W, Zhan LI, Fu X. clusterProfiler 4.0: A universal enrichment tool for interpreting omics data. Innovation. 2021;2(3):100141.

## Potential error and solutions (in Windows 11)  
CondaSSLError: OpenSSL appears to be unavailable on this machine.  
(1) If the error persists even after installation of OpenSSL, then, please run the following command (change according to the miniconda path) first and start app in the same cmd window.  
```Plain Text
https://github.com/conda/conda/issues/11795
shell('setx PATH "C:/Users/<User Name>/AppData/Local/r-miniconda/Library/bin;C:/Users/<User Name>/AppData/Local/r-miniconda/condabin"')
STquantool::run_app()
```
(2) If the above solution does not work, then manually install conda environment with the below command.  
```Plain Text
conda create -n STquantool -c conda-forge python=3.8.12
conda activate STquantool
pip install git+https://github.com/mexchy1000/CellDART.git
pip install graphviz numexpr
```
Then run the application again.  
