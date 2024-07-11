import numpy as np
import pandas as pd

# get historical data dictionary (one column)
hist = {'data': np.load('curvestat/tests/test_data/data_DK.npy', allow_pickle='TRUE')}
# get ensemble dictionary (one column per realization
ensemble = np.load('curvestat/tests/test_data/curves_DKE3.npy', allow_pickle='TRUE').item()

# turn into data frames
hist_df = pd.DataFrame(hist)
ensemble_df = pd.DataFrame(ensemble)

# save as CSV
hist_df.to_csv('historic.csv')
ensemble_df.to_csv('ensemble.csv')
