import json
import os
import time

import pandas

from dtk.utils.analyzers.DownloadAnalyzerTPI import DownloadAnalyzerTPI
from dtk.utils.builders.ConfigTemplate import ConfigTemplate
from dtk.utils.builders.TaggedTemplate import CampaignTemplate, DemographicsTemplate
from dtk.utils.builders.TemplateHelper import TemplateHelper
from dtk.utils.core.DTKConfigBuilder import DTKConfigBuilder
from simtools.AnalyzeManager.AnalyzeManager import AnalyzeManager
from simtools.ExperimentManager.ExperimentManagerFactory import ExperimentManagerFactory
from simtools.ModBuilder import ModBuilder
from simtools.SetupParser import SetupParser
from simtools.Utilities.Matlab import read_mat_points_file
from simtools.Utilities.COMPSUtilities import create_suite
from ast import literal_eval

SetupParser.default_block = 'HPC'
tpi_csv_filename = "..\CombinedInput\ParameterTable.csv"
suite_name = 'Swaziland sweep'
JUST_TESTING = False

unused_params = [
 'TPI']

parameter_map = {
'SeedYr HIGH':'Start_Year__KP_Seeding_Year',
'ART Link Max':'Actual_IndividualIntervention_Config__KP_ART_Link.Ramp_Max',
'ART Link Mid':'Actual_IndividualIntervention_Config__KP_ART_Link.Ramp_MidYear',
'Base Infectivity':'Base_Infectivity',
'Infmrl Condoms Late':'Society__KP_Defaults.INFORMAL.Relationship_Parameters.Condom_Usage_Probability.Max',
'Infmrl Form Rate':'Society__KP_Defaults.INFORMAL.Pair_Formation_Parameters.Formation_Rate_Constant',
'Infrml Condom Mid':'Society__KP_Defaults.INFORMAL.Relationship_Parameters.Condom_Usage_Probability.Mid',
'Infrml Condom Rate':'Society__KP_Defaults.INFORMAL.Relationship_Parameters.Condom_Usage_Probability.Rate',
'Mrtl Condom Max':'Society__KP_Defaults.MARITAL.Relationship_Parameters.Condom_Usage_Probability.Max',
'Mrtl Condom Mid':'Society__KP_Defaults.MARITAL.Relationship_Parameters.Condom_Usage_Probability.Mid',
'Mrtl Condom Rate':'Society__KP_Defaults.MARITAL.Relationship_Parameters.Condom_Usage_Probability.Rate',
'Mrtl Form Rate':'Society__KP_Defaults.MARITAL.Pair_Formation_Parameters.Formation_Rate_Constant',
'PreART Link Max': 'Actual_IndividualIntervention_Config__KP_PreART_Link.Ramp_Max',
'PreART Link Mid': 'Actual_IndividualIntervention_Config__KP_PreART_Link.Ramp_MidYear',
'PreART Link Min': 'Actual_IndividualIntervention_Config__KP_PreART_Link.Ramp_Min',
'Sexual Debut Age Female Weibull Heterogeneity':'Sexual_Debut_Age_Female_Weibull_Heterogeneity',
'Sexual Debut Age Female Weibull Scale':'Sexual_Debut_Age_Female_Weibull_Scale',
'Sexual Debut Age Male Weibull Heterogeneity':'Sexual_Debut_Age_Male_Weibull_Heterogeneity',
'Sexual Debut Age Male Weibull Scale':'Sexual_Debut_Age_Male_Weibull_Scale',
'Swaziland: Infmrl Condoms Late':'Society__KP_Swaziland.INFORMAL.Relationship_Parameters.Condom_Usage_Probability.Max',
'Swaziland: LOW Risk':'Initial_Distribution__KP_Risk_Swaziland',
'Swaziland: Trns Condoms Late':'Society__KP_Swaziland.TRANSITORY.Relationship_Parameters.Condom_Usage_Probability.Max',
'Trns Condom Late':'Society__KP_Defaults.TRANSITORY.Relationship_Parameters.Condom_Usage_Probability.Max',
'Trns Condom Mid':'Society__KP_Defaults.TRANSITORY.Relationship_Parameters.Condom_Usage_Probability.Mid',
'Trns Condom Rate':'Society__KP_Defaults.TRANSITORY.Relationship_Parameters.Condom_Usage_Probability.Rate',
'Trns Form Rate':'Society__KP_Defaults.TRANSITORY.Pair_Formation_Parameters.Formation_Rate_Constant',
'Male_To_Female_Relative_Infectivity_Multipliers': 'Male_To_Female_Relative_Infectivity_Multipliers',
'[BUILDER] Actual_IndividualIntervention_Config__KP_ART_Link.Ramp_Max': 'Actual_IndividualIntervention_Config__KP_ART_Link.Ramp_Max',
'[BUILDER] Actual_IndividualIntervention_Config__KP_ART_Link.Ramp_MidYear':'Actual_IndividualIntervention_Config__KP_ART_Link.Ramp_MidYear',	
'[BUILDER] Actual_IndividualIntervention_Config__KP_PreART_Link.Ramp_Max': 'Actual_IndividualIntervention_Config__KP_PreART_Link.Ramp_Max',	
'[BUILDER] Actual_IndividualIntervention_Config__KP_PreART_Link.Ramp_MidYear': 'Actual_IndividualIntervention_Config__KP_PreART_Link.Ramp_MidYear',
'[BUILDER] Actual_IndividualIntervention_Config__KP_PreART_Link.Ramp_Min': 'Actual_IndividualIntervention_Config__KP_PreART_Link.Ramp_Min',	
'[BUILDER] Initial_Distribution__KP_Risk_Swaziland': 'Initial_Distribution__KP_Risk_Swaziland',		
'[BUILDER] Society__KP_Defaults.INFORMAL.Pair_Formation_Parameters.Formation_Rate_Constant':'Society__KP_Defaults.INFORMAL.Pair_Formation_Parameters.Formation_Rate_Constant',	
'[BUILDER] Society__KP_Defaults.INFORMAL.Relationship_Parameters.Condom_Usage_Probability.Max':'Society__KP_Defaults.INFORMAL.Relationship_Parameters.Condom_Usage_Probability.Max',		
'[BUILDER] Society__KP_Defaults.INFORMAL.Relationship_Parameters.Condom_Usage_Probability.Mid':'Society__KP_Defaults.INFORMAL.Relationship_Parameters.Condom_Usage_Probability.Mid',	
'[BUILDER] Society__KP_Defaults.INFORMAL.Relationship_Parameters.Condom_Usage_Probability.Rate':'Society__KP_Defaults.INFORMAL.Relationship_Parameters.Condom_Usage_Probability.Rate',		
'[BUILDER] Society__KP_Defaults.MARITAL.Pair_Formation_Parameters.Formation_Rate_Constant':'Society__KP_Defaults.MARITAL.Pair_Formation_Parameters.Formation_Rate_Constant',		
'[BUILDER] Society__KP_Defaults.MARITAL.Relationship_Parameters.Condom_Usage_Probability.Max':'Society__KP_Defaults.MARITAL.Relationship_Parameters.Condom_Usage_Probability.Max',		
'[BUILDER] Society__KP_Defaults.MARITAL.Relationship_Parameters.Condom_Usage_Probability.Mid':'Society__KP_Defaults.MARITAL.Relationship_Parameters.Condom_Usage_Probability.Mid',		
'[BUILDER] Society__KP_Defaults.MARITAL.Relationship_Parameters.Condom_Usage_Probability.Rate':'Society__KP_Defaults.MARITAL.Relationship_Parameters.Condom_Usage_Probability.Rate',	
'[BUILDER] Society__KP_Defaults.TRANSITORY.Pair_Formation_Parameters.Formation_Rate_Constant':'Society__KP_Defaults.TRANSITORY.Pair_Formation_Parameters.Formation_Rate_Constant',	
'[BUILDER] Society__KP_Defaults.TRANSITORY.Relationship_Parameters.Condom_Usage_Probability.Max':'Society__KP_Defaults.TRANSITORY.Relationship_Parameters.Condom_Usage_Probability.Max',	
'[BUILDER] Society__KP_Defaults.TRANSITORY.Relationship_Parameters.Condom_Usage_Probability.Mid':'Society__KP_Defaults.TRANSITORY.Relationship_Parameters.Condom_Usage_Probability.Mid',	
'[BUILDER] Society__KP_Defaults.TRANSITORY.Relationship_Parameters.Condom_Usage_Probability.Rate':'Society__KP_Defaults.TRANSITORY.Relationship_Parameters.Condom_Usage_Probability.Rate',	
'[BUILDER] Society__KP_Swaziland.INFORMAL.Relationship_Parameters.Condom_Usage_Probability.Max':'Society__KP_Swaziland.INFORMAL.Relationship_Parameters.Condom_Usage_Probability.Max',		
'[BUILDER] Society__KP_Swaziland.TRANSITORY.Relationship_Parameters.Condom_Usage_Probability.Max':'Society__KP_Swaziland.TRANSITORY.Relationship_Parameters.Condom_Usage_Probability.Max',
'Nodes.0.Society.INFORMAL.Pair_Formation_Parameters.Formation_Rate_Constant':'Society__KP_Defaults.INFORMAL.Pair_Formation_Parameters.Formation_Rate_Constant',	
'Nodes.0.Society.INFORMAL.Relationship_Parameters.Condom_Usage_Probability.Max':'Society__KP_Defaults.INFORMAL.Relationship_Parameters.Condom_Usage_Probability.Max',
'Nodes.0.Society.INFORMAL.Relationship_Parameters.Condom_Usage_Probability.Rate':'Society__KP_Defaults.INFORMAL.Pair_Formation_Parameters.Formation_Rate_Constant',
'Nodes.0.Society.INFORMAL.Relationship_Parameters.Condom_Usage_Probability.Mid':'Society__KP_Defaults.INFORMAL.Relationship_Parameters.Condom_Usage_Probability.Mid',
'Nodes.0.Society.MARITAL.Relationship_Parameters.Condom_Usage_Probability.Max':'Society__KP_Defaults.MARITAL.Relationship_Parameters.Condom_Usage_Probability.Max',
'Nodes.0.Society.MARITAL.Relationship_Parameters.Condom_Usage_Probability.Mid':'Society__KP_Defaults.MARITAL.Relationship_Parameters.Condom_Usage_Probability.Mid',
'Nodes.0.Society.MARITAL.Relationship_Parameters.Condom_Usage_Probability.Rate':'Society__KP_Defaults.MARITAL.Relationship_Parameters.Condom_Usage_Probability.Rate',
'Nodes.0.Society.MARITAL.Pair_Formation_Parameters.Formation_Rate_Constant':'Society__KP_Defaults.MARITAL.Pair_Formation_Parameters.Formation_Rate_Constant',
'Nodes.0.Society.TRANSITORY.Relationship_Parameters.Condom_Usage_Probability.Max':'Society__KP_Defaults.TRANSITORY.Relationship_Parameters.Condom_Usage_Probability.Max',
'Nodes.0.Society.TRANSITORY.Relationship_Parameters.Condom_Usage_Probability.Mid':'Society__KP_Defaults.TRANSITORY.Relationship_Parameters.Condom_Usage_Probability.Mid',
'Nodes.0.Society.TRANSITORY.Relationship_Parameters.Condom_Usage_Probability.Rate':'Society__KP_Defaults.TRANSITORY.Relationship_Parameters.Condom_Usage_Probability.Rate',
'Nodes.0.Society.TRANSITORY.Pair_Formation_Parameters.Formation_Rate_Constant':'Society__KP_Defaults.TRANSITORY.Pair_Formation_Parameters.Formation_Rate_Constant',
'Run_Number': 'Run_Number'}

def find_and_eval(item):
    if isinstance(item, list):
        for index in range(len(item)):
            check_recurse(item, index)
    elif isinstance(item, dict):
        keys = item.keys()
        for key in keys:
            check_recurse(item, key)
    else:
        # do nothing
        pass

def check_recurse(item, indexer):
    if isinstance(item[indexer], str):
        if '[' in item[indexer]:
            item[indexer] = eval(item[indexer])
    else:
        find_and_eval(item[indexer])

def header_table_to_dict(header, table, index_name=None):
    # do column renaming to model parameter names
    for index in range(len(header)):
        item = header[index]
        header[index] = parameter_map.get(item, header[index])

    df = pandas.DataFrame(data=table, columns=header)

    # initial distribution parameters need to be triplet values: [v, 1-v, 0], for v as the table input value
    for param in df.columns:
        # e.g. ['Homa_Bay', 'Kisii', 'Kisumu', 'Migori', 'Nyamira', 'Siaya'] initial distributions
        if 'Initial_Distribution' in param: # e.g. 'Initial_Distribution__KP_Risk_Homa_Bay'
            new_values = pandas.Series(list( zip(df[param], 1 - df[param], pandas.Series([0]*len(df.index))) ))
            df.loc[:, param] = new_values

    # Drop unused columns
    for unused in unused_params:
        if unused in df.columns: df.drop(unused, 1, inplace=True)
    if index_name:
        df[index_name] = df.index

    reloaded_dict = json.loads(pandas.io.json.dumps(df.to_dict(orient='records')))
    find_and_eval(reloaded_dict)
    return reloaded_dict


def read_csv_points_file(csv_filename):
    import pandas as pd
    df = pd.read_csv(csv_filename)
    header = list(df.columns)
    data = df.as_matrix()
    return header, data


# Create the base path
current_dir = os.path.dirname(os.path.realpath(__file__))
working_dir = os.path.join('..', 'CombinedInput')
plugin_files_dir = os.path.join(working_dir, 'Templates')

# Load the base config file
config = ConfigTemplate.from_file(os.path.join(plugin_files_dir, 'Configs', 'config.json'))
config.set_param("Enable_Demographics_Builtin", 0, allow_new_parameters=True)

# Load the campaigns
cpn1 = CampaignTemplate.from_file(os.path.join(plugin_files_dir, 'Campaigns', 'Baseline.json'))
cpn2 = CampaignTemplate.from_file(os.path.join(plugin_files_dir, 'Campaigns', 'NoVMMC.json'))
cpn3 = CampaignTemplate.from_file(os.path.join(plugin_files_dir, 'Campaigns', 'NoARTnoVMMC.json'))
cpn4 = CampaignTemplate.from_file(os.path.join(plugin_files_dir, 'Campaigns', 'NoART.json'))
cpn5 = CampaignTemplate.from_file(os.path.join(plugin_files_dir, 'Campaigns', 'ART909090.json'))
cpn6 = CampaignTemplate.from_file(os.path.join(plugin_files_dir, 'Campaigns', 'ART100pct.json'))

campaigns = {"cpn1":cpn1, "cpn2":cpn2, "cpn3":cpn3,"cpn4":cpn4,"cpn5":cpn5,"cpn6":cpn6}

# Load the demographics
demog = DemographicsTemplate.from_file( os.path.join(plugin_files_dir, 'Demographics', 'Swaziland_Demographics_With_Properties.json'))
demog_pfa = DemographicsTemplate.from_file( os.path.join(plugin_files_dir, 'Demographics', 'PFA_Overlay.json'))
demog_acc = DemographicsTemplate.from_file( os.path.join(plugin_files_dir, 'Demographics', 'Accessibility_and_Risk_IP_Overlay.json'))
demog_asrt = DemographicsTemplate.from_file( os.path.join(plugin_files_dir, 'Demographics', 'Risk_Assortivity_Overlay.json'))

# Load the scenarios
scenario_header = [
    'Scenario',
    'Campaign_Template'
]

scenarios = [
	 ['Baseline', "cpn1"]
     ]
	 
#	 ['NoVMMC', "cpn2"],
#	 ['NoARTnoVMMC', "cpn3"],
#	 ['NoART', "cpn4"],
#	 ['ART909090', "cpn5"],
#	 ['ART100pct', "cpn6"]
	 
# And the points
# point_header, points = read_mat_points_file(tpi_matlab_filename)
point_header, points = read_csv_points_file(tpi_csv_filename)

# We only take the first 3 points. Comment the following line to run the whole 250 points
if JUST_TESTING:
    points = points[0:2]

# Create the default config builder
config_builder = DTKConfigBuilder()

# Set which executable we want to use for the experiments in the script
config_builder.set_experiment_executable('..\Binary\EMOD_binary_20180921_with_configurable_ART.exe')

# This is REQUIRED by the templates
config_builder.ignore_missing = True

# Get the dicts
points_dict = header_table_to_dict(point_header, points, index_name='TPI')
for point in points_dict:
    tpi = point.pop('TPI')
    if 'TAGS' not in point:
        point['TAGS'] = {}

    point['TAGS']['TPI'] = tpi

with open('points_dict.json', 'w') as fp:
    json.dump(points_dict, fp)


scenarios_dict = header_table_to_dict(scenario_header, scenarios)

with open('scenarios_dict.json', 'w') as fp:
    json.dump(scenarios_dict, fp)

if __name__ == "__main__":
    SetupParser.init()

    experiments = []      # All the experiment managers for all experiments
    experiments_ids = []  # Ids of the created experiments for resuming capabilities

    # Check if we want to resume
    if os.path.exists('ids.json'):
        print("Previous run detected... Run [N]ew, [R]esume, [A]bort?")
        resp = ""
        while resp not in ('N', 'R', 'A'):
            resp = raw_input()
        if resp == "A":
            exit()
        elif resp == "R":
            # In the resume case, retrieve the ids and create the managers
            experiments_ids = json.load(open('ids.json', 'r'))
            for id in experiments_ids:
                experiments.append(ExperimentManagerFactory.from_experiment(str(id)))
        elif resp == "N":
            # Delete shelve file
            if os.path.exists('DownloadAnalyzerTPI.shelf'): os.remove('DownloadAnalyzerTPI.shelf')
            # Delete the ids
            os.remove('ids.json')

    # If experiment_ids is empty -> we need to commission
    if not experiments_ids:
        # Create a suite to hold all the experiments
        suite_id = create_suite(suite_name)

        # Create the scenarios
        for scenario in scenarios_dict:
            scenario_name = scenario['Scenario']
            campaign_tpl = campaigns[scenario.pop('Campaign_Template')]

            # For each scenario, combine with the points first
            combined = []
            for point in points_dict:
                current = {}
                current.update(scenario)
                current.update(point)
                combined.append(current)

            # Extract the headers
            headers = [k.replace('CONFIG.', '').replace('DEMOGRAPHICS.', '').replace('CAMPAIGN.', '') for k in combined[0].keys()]

            # Construct the table
            table = [list(c.values()) for c in combined]

            # Change some things in the config.json
            config.set_param('Config_Name', scenario_name)

            # Initialize the template
            tpl = TemplateHelper()
            tpl.set_dynamic_header_table(headers, table)
            tpl.active_templates = [config, campaign_tpl, demog, demog_pfa, demog_asrt, demog_acc]

            # Create an experiment builder
            experiment_builder = ModBuilder.from_combos(tpl.get_modifier_functions())
            experiment_manager = ExperimentManagerFactory.from_cb(config_builder)
            COMPS_experiment_name = scenario_name
           # COMPS_experiment_name = suite_name # I want hover-over in COMPS to be the suite name

            experiment_manager.run_simulations(exp_name=COMPS_experiment_name, exp_builder=experiment_builder, suite_id=suite_id)
            experiments.append(experiment_manager)
            experiments_ids.append(experiment_manager.experiment.exp_id)

        # Dump the experiment ids for resume
        with open('ids.json', 'w') as out:
            json.dump(experiments_ids, out)
			
    # Every experiments are created at this point -> Analyze
    am = AnalyzeManager(verbose=False, create_dir_map=False)
    for em in experiments:
        am.add_experiment(em.experiment)
#    am.add_analyzer(DownloadAnalyzerTPI(['output\\DemographicsSummary.json', 'config.json', 'output\\ReportHIVART.csv', 'output\\ReportHIVByAgeAndGender.csv'],
#                                        output_dir='Test HIV 1'))
    am.add_analyzer(DownloadAnalyzerTPI(['..\\Output\\ReportHIVByAgeAndGender.csv'], output_dir='ReportHIVByAgeAndGender'))

    # While the experiments are running, we are analyzing every 15 seconds
    while not all([em.finished() for em in experiments]):
        map(lambda e: e.refresh_experiment(), experiments)
        print("Analyzing !")
        am.analyze()
        print("Waiting 15 seconds")
        time.sleep(15)

    # Analyze one last time when everything is complete
    am.analyze()
