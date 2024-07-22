# config.py
import os
import pandas as pd
import numpy as np
import arviz as az
import matplotlib.pyplot as plt
from IPython import get_ipython
import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)

# set seed to make the results fully reproducible
seed: int = sum(map(ord, "ds4p"))
rng: np.random.Generator = np.random.default_rng(seed=seed)

az.style.use("arviz-darkgrid")
plt.rcParams["figure.dpi"] = 100
plt.rcParams["figure.facecolor"] = "white"

# Get the home directory
home_directory = os.path.expanduser("~")
# Construct the path to the Quarto project directory 
project_directory = os.path.join(
    home_directory, '_repositories', 'ds4p')

# Configure the inline backend for Jupyter notebooks
ipython = get_ipython()
if ipython is not None:
    ipython.run_line_magic("config", "InlineBackend.figure_format = 'retina'")

