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
  
  y = y.values
  x = x.values
  ry = (ry.values)[:,0]
  wy = (wy.values)[:,0]
  
  xobs = x[ry, :]
  xmis = x[wy, :]
  yobs = y[ry]
  
  
  # Unsupervised pre-training
  from pytorch_tabnet.pretraining import TabNetPretrainer
  
  # TabNetPretrainer
  unsupervised_model = TabNetPretrainer(
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
    batch_size=512, virtual_batch_size=64,
    num_workers=0,
    drop_last=False,
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
    eval_metric=['rmse'],
    max_epochs=max_epochs,
    patience=50,
    batch_size=256, virtual_batch_size=64,
    num_workers=0,
    drop_last=False,
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
