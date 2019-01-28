from simtools.Analysis.AnalyzeManager import AnalyzeManager
from simtools.Analysis.BaseAnalyzers import DownloadAnalyzer

if __name__ == "__main__":
    analyzer = DownloadAnalyzer(filenames=['campaign_Swaziland_v05Sep2018_FINAL_reftracktopup_2011_v2.json', 'config.json', 'Swaziland_Demographics_With_Properties.json','Risk_Assortivity_Overlay.json', 'PFA_Overlay.json','Accessibility_and_Risk_IP_Overlay.json'])
    am = AnalyzeManager(["6f6ff06d-fed6-e811-a2bd-c4346bcb1555"], analyzers=analyzer)
    am.analyze()

	
	
	