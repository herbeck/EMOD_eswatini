import sys, os
import json
import pandas
import os
import numpy as np
import random

if len(sys.argv) < 2:
    print('Usage: plot_age.py path/to/calib/directory ...')
    # Usage: python ReEvaluateLikelihood_2.py C:\Users\mudime\Documents\IDM EMOD South Africa model\run_OptimTool_for_South_Africa_LATEST\ExampleCalibrationIngest
    # Where trajectories.csv contains best fitting trajectory output
    exit()

# Where to find the calibration folder from which we want to extract the samples
base_path = sys.argv[1]
# Load the calibration data from the calibmanager.json
calib_data = json.load(open(os.path.join(base_path, 'CalibManager.json')))
# Retrieve the results section which contains parameter values and their associated LL
results = calib_data["results"]

idfs = pandas.DataFrame()

# Go through each iteration to get the Run_Number for the individual simulations
for iteration in range(calib_data["iteration"]+1):
    # Load the iteration JSON
    iteration_data = json.load(open(os.path.join(base_path, 'iter%s\\IterationState.json' % iteration)))

    # Get the simulation data
    simulations_data = iteration_data['simulations']
    experiment_id = iteration_data['experiment_id']

    # Create a DF
    idf = pandas.DataFrame(list(simulations_data.values()))
    idf['simulation_id'] = simulations_data.keys()

    # Add the iteration
    idf['iteration'] = iteration

    # add the experiment ID
    idf['experiment_id'] = experiment_id

    # Concatenate to the global DF containing information for every iterations
    idfs = pandas.concat([idfs, idf])

# Rename the sample column in idfs to make it match with the results
idfs.rename(index=str, columns={"__sample_index__":"sample"}, inplace=True)

# Merge the result with the iteration data
results_df = pandas.merge(pandas.DataFrame(results), idfs)

# sort total likelihood scores from best to worst

results_df_sorted = results_df.sort_values('total', ascending=False)
results_df_sorted = results_df_sorted.reset_index(drop=True)
results_df_sorted['LikelihoodScore'] = np.exp(results_df_sorted['total'])
results_df_sorted['LikelihoodScore'] = results_df_sorted['LikelihoodScore']

# take top N likelihood scores

total_likelihood_scores = 250 # total number of likelihood samples desired
ensure_pick_top_N = 10 # pick top N overall likelihoods

resample_values = total_likelihood_scores - ensure_pick_top_N

# create empty dataframe to populate with likelihood values

# ReEvaluatedLikelihood = pandas.DataFrame(data=None, index=np.arange(0, total_likelihood_scores), columns=results_df.columns)

ReEvaluatedLikelihood_1 = results_df_sorted
ReEvaluatedLikelihood_1 = ReEvaluatedLikelihood_1.iloc[0:ensure_pick_top_N]

results_df_sorted['LikelihoodScore'] = np.exp(results_df_sorted['total'])

p = np.array(results_df_sorted['LikelihoodScore'])
p /= p.sum()

ReEvaluatedLikelihood_nprandomchoice = np.random.choice(results_df_sorted.index.values, resample_values, replace=False, p=p)
ReEvaluatedLikelihood_2 = pandas.DataFrame(data=None, index=np.arange(0, resample_values), columns=results_df_sorted.columns)

for i in range(0, resample_values):

    indextouse = ReEvaluatedLikelihood_nprandomchoice[i]
    ReEvaluatedLikelihood_2.iloc[i,:] = results_df_sorted.iloc[indextouse,:]

frames = [ReEvaluatedLikelihood_1, ReEvaluatedLikelihood_2]

ReEvaluatedLikelihood_tosave = pandas.concat(frames, ignore_index=True)

# save to csv

ReEvaluatedLikelihood_tosave.to_csv('ReEvaluatedLikelihood_full.csv')
