import importlib
import logging
from calibtool.CalibSite import CalibSite

logger = logging.getLogger(__name__)

class SwazilandCalibSite(CalibSite):
    metadata = {}

    def __init__(self, **kwargs):
        # required kwargs first
        analyzers = kwargs.get('analyzers', None)
        if not analyzers:
            raise Exception('An analyzer dictionary must be provided to the \'analyzers\' argument.')

        # reference_data must be supplied as a kwarg and is simply stored for direct use by analyzers
        self.reference_data = kwargs.get('reference_data', None)
        if not self.reference_data:
            raise Exception('Obs/reference data object must be provided to the \'reference_data\' argument.')

        # optional kwargs
        force_apply = kwargs.get('force_apply', False)
        max_sims_per_scenario = kwargs.get('max_sims_per_scenario', -1)
        max_sims_per_scenario = kwargs.get('max_sims_per_scenario', -1)

        self.analyzers = []
        for analyzer, weight in analyzers.items():
            AnalyzerClass = getattr(importlib.import_module('hiv.analyzers.%s'%analyzer), analyzer)

            self.analyzers.append(
                AnalyzerClass(
                    self,
                    weight = weight,

                    force_apply = force_apply,
                    max_sims_per_scenario = max_sims_per_scenario,
					
					#Population based on CIA Factbook
                    reference_year = 2014,
                    reference_population = 1419623,
                    age_min = 0,
                    age_max = 100,

                    node_map = {
                            1: "Swaziland"
                               },

                    basedir = '.',
                    fig_format = 'png',
                    fig_dpi = 600,
                    verbose = True
                )
            )


        # Must come at the end:
        super(SwazilandCalibSite, self).__init__('Swaziland')

    def get_setup_functions(self):
        return [ ]

    def get_analyzers(self):
        return self.analyzers
