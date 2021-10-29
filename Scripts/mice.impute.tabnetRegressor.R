#' Imputation by tabnet
#'
#' Imputes univariate missing data using TabNet.
#'
#' @aliases mice.impute.tabnetRegressor
#' @inheritParams mice.impute.pmm
#' @param hpc Call hpc at each iteration, defaults FALSE
#' @param gpu Call with local GPU.
#' @param random_state random state for uuid generator.
#' @param verbose Show each process time
#' @param embedding_cols Optional categorical columns used for embedding.
#' @param \dots Other named arguments passed down to
#' \code{mice:::install.on.demand()}.
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}
#' @details
#' Imputation of \code{y} by tabnet regressor, for continous variables.
#' The method calls \code{tabnet()} implementation in PyTorch
#' (Arik, S. O., & Pfister, T. (2019). TabNet: Attentive Interpretable
#' Tabular Learning. arXiv preprint arXiv:1908.07442.)
#' @author Serra Traynor, Carlos
#' @references
#'
#' Arik, S. O., & Pfister, T. (2019). TabNet: Attentive Interpretable Tabular
#' Learning. arXiv preprint arXiv:1908.07442.
#'
#' @seealso \code{\link{mice}}
#' @family univariate imputation functions
#' @importFrom readr write_csv read_csv
#' @importFrom dplR uuid.gen
#' @export
mice.impute.tabnetRegressor <- function(y, ry, x, wy = NULL, hpc = FALSE, gpu = FALSE, random_state = "", verbose = F, embedding_cols = NULL, dropout = 0.1, ...) {
  if (is.null(wy)) wy <- !ry
  
  ## create unique parent directory
  fun <- dplR::uuid.gen(more.state = random_state)
  uid <- fun()
  parentDir <- file.path("DerivedData/tmp", uid )
  dir.create(parentDir, recursive = TRUE)
  i = 1
  perc = 0.81 ## percentage on training trick to avoid batch size = 1, and ghost norm errors 
  while(!file.exists(file.path(parentDir, "y_pred.csv")) & i <= 30 ) {
    perc = perc - 0.01 # reduce current perc training on 0.01
    ## handle of embeddings
    if(!is.null(embedding_cols)) {
      pred_embedding_cols <- gsub(paste( embedding_cols, collapse = "|" ), "", colnames(x))
      pred_embedding_cols <- gsub(paste(pred_embedding_cols, collapse = "|"), "", colnames(x))
      pred_embedding_cols <- unique(pred_embedding_cols)
      pred_embedding_cols <- pred_embedding_cols[ !grepl("$^", pred_embedding_cols) ]
      
      if(length(pred_embedding_cols) > 0 ) { ## Run Embedding model
        x <- as.data.frame(x)
        drop <- grep(paste(pred_embedding_cols, collapse = "|"), colnames(x))
        for(k in pred_embedding_cols) {
          keep <- grepl(k, colnames(x))
          tmp <- x[ , keep, drop=F]
          if(ncol(tmp) > 1) {
            res <- apply(tmp, 1, function(x) match( max(x), x) ) * apply(tmp, 1, max ) 
          } else {
            res <- apply(tmp, 1, max) 
          }
          res <- data.frame(res = res)
          colnames(res) <- k
          x <- cbind.data.frame(x, res)
        }
        x <- x[ ,-drop, drop = F]
        save_regressor_files(parentDir, x , y, wy, ry)
        readr::write_csv(data.frame(embedding_vars = pred_embedding_cols), file.path(parentDir, "embedding_cols.csv"))
        run_tabnet_regressor_with_embeddings(parentDir, perc, hpc, gpu, dropout )
        
      } else { ### run without embeddings
        save_regressor_files(parentDir, x , y, wy, ry)
        run_tabnet_regressor_no_embeddings(parentDir, perc, hpc, gpu, dropout )
      } ## end 
    } else { ## RUN without embedding
      save_regressor_files(parentDir, x , y, wy, ry)
      run_tabnet_regressor_no_embeddings(parentDir, perc, hpc, gpu, dropout )
    } ## end 
      i = i + 1
  }
  
  y_pred <- suppressMessages(
    readr::read_csv(file.path(parentDir, "y_pred.csv"))
    ) 
  system(paste0("rm -rf ", parentDir))
  unlist(y_pred)
  
}

save_regressor_files <- function(parentDir, x , y, wy, ry) {
  readr::write_csv(data.frame(ry = ry), file.path(parentDir, "ry.csv"))
  readr::write_csv(data.frame(wy = wy), file.path(parentDir, "wy.csv"))
  readr::write_csv(data.frame(y = y), file.path(parentDir, "y.csv"))
  readr::write_csv(as.data.frame(x), file.path(parentDir, "x.csv"))
}

run_tabnet_regressor_with_embeddings <- function(parentDir, perc, hpc, gpu, dropout ) {
  if (hpc) {
    foo <- file.path(parentDir, "mitabnetRegressorEmbedding.nf" )
    system(paste0( "cp ./NextFlow/mitabnetRegressorEmbedding.nf ", foo ) ) 
    system(paste0( "nextflow -q run ", foo) )
  } else if (gpu) {
    system(paste0( "cd ./tabnet; module purge; module load CUDA/10.1.243 cuDNN/7.6.4.38-CUDA-10.1.243 PyTorch/1.4.0-fosscuda-2019a-Python-3.7.2; python ./mice_tabnetRegressorEmbedding_call.py --dropout=", dropout," --perc=", perc," --data_dir=",parentDir," > /dev/null; module purge; module load mro; cd ..") )
  } else {
    system(paste0( "cd ./tabnet; module purge; module load PyTorch/1.4.0-foss-2019a-Python-3.7.2 scikit-learn/0.23.1-foss-2019a-Python-3.7.2; python ./mice_tabnetRegressorEmbedding_call.py --dropout=", dropout," --perc=", perc," --data_dir=",parentDir," > /dev/null; module purge; module load mro; cd ..") ) 
  }
}

run_tabnet_regressor_no_embeddings <- function(parentDir, perc, hpc, gpu, dropout ) {
  if (hpc) {
    foo <- file.path(parentDir, "mitabnetRegressor.nf" )
    system(paste0( "cp ./NextFlow/mitabnetRegressor.nf ", foo ) ) 
    system(paste0( "nextflow -q run ", foo) )
  } else if (gpu) {
    system(paste0( "cd ./tabnet; module purge; module load CUDA/10.1.243 cuDNN/7.6.4.38-CUDA-10.1.243 PyTorch/1.4.0-fosscuda-2019a-Python-3.7.2; python ./mice_tabnetRegressor_call.py --dropout=", dropout," --perc=", perc," --data_dir=",parentDir," > /dev/null; module purge; module load mro; cd ..") )
  } else {
    system(paste0( "cd ./tabnet; module purge; module load PyTorch/1.4.0-foss-2019a-Python-3.7.2 scikit-learn/0.23.1-foss-2019a-Python-3.7.2; python ./mice_tabnetRegressor_call.py  --dropout=", dropout," --perc=", perc," --data_dir=",parentDir," > /dev/null; module purge; module load mro; cd ..") ) 
  }
}