#' Imputation by tabnet
#'
#' Imputes univariate missing data using TabNet.
#'
#' @aliases mice.impute.tabnetClassifier
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
#' Imputation of \code{y} by tabnet Classifier, for categorical variables.
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
#' @importFrom dplyr left_join
#' @export
mice.impute.tabnetClassifier <- function(y, ry, x, wy = NULL, hpc = FALSE, gpu = FALSE, random_state = "", verbose = F, embedding_cols = NULL, ...) {
  if (is.null(wy)) wy <- !ry
  
  ## create unique parent directory
  fun <- dplR::uuid.gen(more.state = random_state)
  uid <- fun()
  parentDir <- file.path("DerivedData/tmp", uid )
  dir.create(parentDir, recursive = TRUE)
  i = 1
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
        save_classifier_files(parentDir, x , y, wy, ry)
        readr::write_csv(data.frame(embedding_vars = pred_embedding_cols), file.path(parentDir, "embedding_cols.csv"))
        run_tabnet_classifier_with_embeddings(parentDir, perc, hpc, gpu, dropout )
        
      } else { ### run without embeddings
        save_classifier_files(parentDir, x , y, wy, ry)
        run_tabnet_classifier_no_embeddings(parentDir, perc, hpc, gpu, dropout )
      } ## end 
    } else { ## RUN without embedding
      save_classifier_files(parentDir, x , y, wy, ry)
      run_tabnet_classifier_no_embeddings(parentDir, perc, hpc, gpu, dropout )
    } ## end 
    i = i + 1
  }
  
  ## prepare output as factor
  y_pred <- suppressMessages(
    readr::read_csv(file.path(parentDir, "y_pred.csv"))
  ) 
  num_y <- as.numeric(y)
  y_df <- data.frame(level_y = y, y = num_y)
  y_df <- unique(y_df)
  y_pred <- apply(as.matrix(y_pred), 1, function(x) sample(seq_len(ncol(y_pred)), 1, prob = x))
  y_pred <- data.frame(y = y_pred)
  y_pred <- dplyr::left_join(y_pred, y_df, by = "y")
  y_pred <- y_pred$level_y
  
  system(paste0("rm -rf ", parentDir))
  return(unlist(y_pred))
  
}

save_classifier_files <- function(parentDir, x , y, wy, ry) {
  readr::write_csv(data.frame(ry = ry), file.path(parentDir, "ry.csv"))
  readr::write_csv(data.frame(wy = wy), file.path(parentDir, "wy.csv"))
  readr::write_csv(data.frame(y = y), file.path(parentDir, "y.csv"))
  readr::write_csv(as.data.frame(x), file.path(parentDir, "x.csv"))
}

run_tabnet_classifier_with_embeddings <- function(parentDir, perc, hpc, gpu, dropout ) {
  if (hpc) {
    foo <- file.path(parentDir, "mitabnetClassifierEmbedding.nf" )
    system(paste0( "cp ./NextFlow/mitabnetClassifierEmbedding.nf ", foo ) ) 
    system(paste0( "nextflow -q run ", foo) )
  } else if (gpu) {
    system(paste0( "cd ./tabnet; module purge; module load CUDA/10.1.243 cuDNN/7.6.4.38-CUDA-10.1.243 PyTorch/1.4.0-fosscuda-2019a-Python-3.7.2; python ./mice_tabnetClassifierEmbedding_call.py --dropout=", dropout," --perc=", perc," --data_dir=",parentDir," > /dev/null; module purge; module load mro; cd ..") )
  } else {
    system(paste0( "cd ./tabnet; module purge; module load PyTorch/1.4.0-foss-2019a-Python-3.7.2 scikit-learn/0.23.1-foss-2019a-Python-3.7.2; python ./mice_tabnetClassifierEmbedding_call.py --dropout=", dropout," --perc=", perc," --data_dir=",parentDir," > /dev/null; module purge; module load mro; cd ..") ) 
  }
}

run_tabnet_classifier_no_embeddings <- function(parentDir, perc, hpc, gpu, dropout ) {
  if (hpc) {
    foo <- file.path(parentDir, "mitabnetClassifier.nf" )
    system(paste0( "cp ./NextFlow/mitabnetClassifier.nf ", foo ) ) 
    system(paste0( "nextflow -q run ", foo) )
  } else if (gpu) {
    system(paste0( "cd ./tabnet; module purge; module load CUDA/10.1.243 cuDNN/7.6.4.38-CUDA-10.1.243 PyTorch/1.4.0-fosscuda-2019a-Python-3.7.2; python ./mice_tabnetClassifier_call.py --dropout=", dropout," --perc=", perc," --data_dir=",parentDir," > /dev/null; module purge; module load mro; cd ..") )
  } else {
    system(paste0( "cd ./tabnet; module purge; module load PyTorch/1.4.0-foss-2019a-Python-3.7.2 scikit-learn/0.23.1-foss-2019a-Python-3.7.2; python ./mice_tabnetClassifier_call.py --dropout=", dropout," --perc=", perc," --data_dir=",parentDir," > /dev/null; module purge; module load mro; cd ..") ) 
  }
}