import numpy as np
import itertools as it
import os

from dtk.utils.core.DTKConfigBuilder import DTKConfigBuilder
from simtools.SetupParser import SetupParser
from simtools.ExperimentManager.ExperimentManagerFactory import ExperimentManagerFactory
from simtools.ModBuilder import ModBuilder, ModFn


# TODO: Be paranoid
# When experiments are commissioning, do a spot check of a few random simulations on COMPS to verify that the
# appropriate sweep values and pop. scale factor have actually been set properly (see tags for values).
# I (Clark) did this and seemed ok, but I usually recommend paranoia.

# Run on HPC
SetupParser.default_block = "HPC"

# TODO
SUITE_NAME = 'paper_review_sweeps'

# TODO
BASE_POPULATION_SCALE_FACTOR = 0.05  # Original: 0.05, looked ok in catalyst (minimum): 0.02

# TODO: edit the part before the --'s if you like
EXP_NAME_PATTERN = 'PaperReviewSweep--%s--%d'

# TODO: point this to your own windows-styled path containing downloaded base simulation files
simulation_input_files_dir = 'C:\\Users\\ckirkman\\run\\EMOD_eswatini\\Calibration\\SeparateInput'


def update_all_configs(cb, update_values, **kwargs):
    cb.config['parameters']['Base_Population_Scale_Factor'] = BASE_POPULATION_SCALE_FACTOR # TODO: ADAM DECIDE ON THIS, change the constant above if desired
    for param, value in update_values.items():
        # try and set in config first
        if param in cb.config['parameters']:
            cb.config['parameters'][param] = value
        else: # must be in the campaign
            if param == 'Delay_Period_Mean':
                # set all instances of this parameter
                for event in cb.campaign.Events:
                    if hasattr(event.Event_Coordinator_Config.Intervention_Config, 'Delay_Period_Mean'):
                        event.Event_Coordinator_Config.Intervention_Config.Delay_Period_Mean = value
            else:
                raise Exception('Unknown campaign parameter for setting: %s' % param)
    return update_values

SWEEPS = [
    # 143 sims per base config
    {
        'name': 'delay_and_supression',                                                                                                                                     # TODO: [min, max), step
        'mods': [ModFn(update_all_configs, update_values=values) for values in[{'Delay_Period_Mean': x, 'ART_Viral_Suppression_Multiplier': y} for x, y in it.product(range(0, 360+1, 30), np.arange(0, 0.2+0.001, 0.02))]]
    },

    # 156 sims per base config
    # cannot exceed 5 for Acute_Duration_In_Months, model limitation
    {
        'name': 'acuteness',
        'mods': [ModFn(update_all_configs,
                       update_values=values)                                                                                        # TODO: [min, max), step
                 for values in[{'Acute_Stage_Infectivity_Multiplier': x, 'Acute_Duration_In_Months': y} for x, y in it.product(range(5, 30+1, 1), range(1, 5+1, 1))]]
    }
]

# (143 + 156) * 250 =74750 sims!!

sim_ids = [id for id in os.listdir(path=simulation_input_files_dir) if os.path.join(simulation_input_files_dir, id)]
# sim_id = sim_ids[3] # ck4, testing only
run_sets = []
for sweep in SWEEPS:
    i = 0
    for sim_id in os.listdir(path=simulation_input_files_dir):
        # Create one experiment per base_simulationXsweep pairing. Each exp contains one sim per sweep setting.
        i += 1
        simulation_directory = os.path.join(simulation_input_files_dir, sim_id)
        if not os.path.isdir(simulation_directory):  # skip over Readme file
            continue

        # create a config builder for the current experiment, derived from the base simulation input files
        config_filepath = os.path.join(simulation_directory, 'config.json')
        campaign_filepath = os.path.join(simulation_directory, 'campaign_Swaziland_v05Sep2018_FINAL_reftracktopup_2011_v2.json')
        cb = DTKConfigBuilder.from_files(config_name=config_filepath, campaign_name=campaign_filepath)
        cb.set_input_files_root(path=simulation_directory) # discoverability of other input files, e.g. PFA_Overlay.json

        exp_name = EXP_NAME_PATTERN % (sweep['name'], i)

        # Create a generator of the per-simulation differences from the base simulation
        builder = ModBuilder.from_combos(sweep['mods'])

        # keep track of each run_sim_args dictionary, one per experiment
        run_sets.append({
            'exp_name': exp_name,
            'exp_builder': builder,
            'config_builder': cb,
            'base_simulation_id': sim_id,
            'sweep_name': sweep['name']
        })


# # ck4, for catalyst this block
# run_sim_args = run_sets[1]
# run_sim_args.pop('base_simulation_id')
# run_sim_args.pop('sweep_name')

if __name__ == "__main__":
    SetupParser.init()
    suite_id = None
    # Run all experiments in a shared suite and then exit
    for run_sim_args in run_sets:
        base_simulation_id = run_sim_args.pop('base_simulation_id')
        sweep_name = run_sim_args.pop('sweep_name')
        exp_manager = ExperimentManagerFactory.init()
        exp_manager.experiment_tags = {'base_simulation_id': base_simulation_id, 'sweep_name': sweep_name}

        suite_id = suite_id or exp_manager.create_suite(SUITE_NAME)
        exp_manager.run_simulations(**run_sim_args, suite_id=suite_id)
