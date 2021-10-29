# Necessary packages
from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
import argparse
import sys
import os
from pathlib import Path

from pytorch_tabnet.tab_model import TabNetRegressor

import torch
# from sklearn.preprocessing import LabelEncoder
# from sklearn.metrics import mean_squared_error

import pandas as pd
import numpy as np

def main(args):
  
  # call parameters 
  data_dir = args.data_dir
  perc = args.perc
  
  x  = pd.read_csv( os.path.join(data_dir, 'x.csv') )
  ry = pd.read_csv( os.path.join(data_dir, 'ry.csv') )
  wy = pd.read_csv( os.path.join(data_dir, 'wy.csv') )
  y  = pd.read_csv( os.path.join(data_dir, 'y.csv') )
  
  embedding_cols  = pd.read_csv( os.path.join(data_dir, 'embedding_cols.csv') )
  embedding_cols = embedding_cols.values[:,0]
  
  y = y.values
  # x = x.values
  ry = (ry.values)[:,0]
  wy = (wy.values)[:,0]
  yobs = y[ry]
  
  nunique = x.nunique()
  types = x.dtypes
  categorical_columns = []
  categorical_dims =  {}
  for col in x.columns:
    if col in embedding_cols:
      categorical_columns.append(col)
      categorical_dims[col] = x[col].nunique()
  
  features = x.columns
  xobs = x[ry].values
  xmis = x[wy].values
  
  cat_idxs = [ i for i, f in enumerate(features) if f in categorical_columns]
  cat_dims = [ categorical_dims[f] for i, f in enumerate(features) if f in categorical_columns]

  # Unsupervised pre-training
  from pytorch_tabnet.pretraining import TabNetPretrainer
  
  # TabNetPretrainer
  cat_emb_dim = list( ( np.round( np.power( np.array(cat_dims), 1/4 )  ) ).astype('int') )
  unsupervised_model = TabNetPretrainer(
    cat_idxs=cat_idxs,
    cat_dims=cat_dims,
    cat_emb_dim= cat_emb_dim,
    optimizer_fn=torch.optim.Adam,
    optimizer_params=dict(lr=2e-2),
    mask_type='entmax' # "sparsemax"
  )
  
  max_epochs = 1000 if not os.getenv("CI", False) else 2
  
  Set = np.random.choice(["train", "valid"], p =[perc, 1-perc], size=len(yobs))
  X_train = xobs[Set == "train"]
  X_valid = xobs[Set == "valid"]
  y_train = yobs[Set == "train"]
  y_valid = yobs[Set == "valid"]
  
  unsupervised_model.fit(
    X_train=X_train,
    eval_set=[X_valid],
    max_epochs=max_epochs , patience=20,
    batch_size=2048, virtual_batch_size=128,
    num_workers=0,
    drop_last=True,
    pretraining_ratio=0.8) 
  
  # Training
  clf = TabNetRegressor(optimizer_fn=torch.optim.Adam,
                       optimizer_params=dict(lr=2e-2),
                       scheduler_params={"step_size":10, # how to use learning rate scheduler
                                         "gamma":0.9},
                       scheduler_fn=torch.optim.lr_scheduler.StepLR,
                       mask_type='sparsemax' # This will be overwritten if using pretrain model
                       )
  
  clf.fit(X_train=X_train, y_train=y_train,
    eval_set=[(X_train, y_train), (X_valid, y_valid)],
    eval_name=['train', 'valid'],
    eval_metric=['mae', 'rmse', 'mse'],
    max_epochs=max_epochs,
    patience=50,
    batch_size=1024, virtual_batch_size=128,
    num_workers=0,
    drop_last=True,
    from_unsupervised=unsupervised_model) 
  
  preds = clf.predict(xmis)
  preds = pd.DataFrame(preds)
  preds.to_csv(os.path.join(data_dir, 'y_pred.csv') , index = False)
  
if __name__ == '__main__':
  # Inputs for the main function
  parser = argparse.ArgumentParser()
  
  parser.add_argument(
  '--data_dir',
    default=".",
    type=str)
  parser.add_argument(
  '--perc',
    default=0.8,
    type=float)


  args = parser.parse_args()
  # Call main function
  main(args)
