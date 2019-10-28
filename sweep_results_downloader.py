import os

from simtools.Analysis.AnalyzeManager import AnalyzeManager
from simtools.Analysis.BaseAnalyzers import DownloadAnalyzer
from simtools.Utilities.General import retrieve_item


class DownloadAnalyzerWithTags(DownloadAnalyzer):

    @staticmethod
    def determine_sweep_set(tags):
        # determine which sweep and which value overrides were used for this simulation for output organization purposes
        # String values (name, params) copied from sweeper.py
        if 'Delay_Period_Mean' in tags:
            sweep_name = 'delay_and_supression'
            param1 = 'Delay_Period_Mean'
            param2 = 'ART_Viral_Suppression_Multiplier'
        else:
            sweep_name = 'acuteness'
            param1 = 'Acute_Stage_Infectivity_Multiplier'
            param2 = 'Acute_Duration_In_Months'
        value1 = tags[param1]
        value2 = tags[param2]
        sweep_set = '%s-%s--%s-%s' % (param1, value1, param2, value2)
        return sweep_name, sweep_set

    def get_sim_folder(self, simulation):
        sweep_name, sweep_set = self.determine_sweep_set(simulation.tags)
        return os.path.join(self.output_path, sweep_name, sweep_set, simulation.id)

    def select_simulation_data(self, data, simulation):
        # Create a folder for the current simulation
        sim_folder = self.get_sim_folder(simulation)
        os.makedirs(sim_folder, exist_ok=True)

        # Create the requested files
        for filename in self.filenames:
            file_path = os.path.join(sim_folder, os.path.basename(filename))
            with open(file_path, 'wb') as outfile:
                outfile.write(data[filename])


if __name__ == "__main__":
    analyzer = DownloadAnalyzerWithTags(filenames=['output\\ReportHIVByAgeAndGender.csv'])
    experiments = retrieve_item(itemid='5a088005-19f8-e911-a2c3-c4346bcb1551')  # suite_id passed here
    experiments = [e.id for e in experiments]
    am = AnalyzeManager(exp_list=experiments, analyzers=analyzer)
    am.analyze()
