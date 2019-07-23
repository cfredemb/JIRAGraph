# -*- coding: utf-8 -*-
"""
Created on Tue Jul 23 13:07:40 2019

@author: clement
"""

"""Template base class for a custom scorer recipe."""

import numpy as np
import typing
from h2oaicore.metrics import CustomScorer

_global_modules_needed_by_name = []  # Optional global package requirements, for multiple custom recipes in a file


class MyBinomialDevianceScorer(CustomScorer):
    _description = "Capped (10e2) average binomial deviance for average sport prediction"
    _maximize = False  # whether a higher score is better
    _perfect_score = 0.0  # the ideal score, used for early stopping once validation score achieves this value

    _supports_sample_weight = True  # whether the scorer accepts and uses the sample_weight input

    """Please enable the problem types this scorer applies to"""
    _regression = True
    _binary = True
    _multiclass = True

    """Specify the python package dependencies (will be installed via pip install mypackage==1.3.37)"""
    _modules_needed_by_name = []  # List[str]

    @staticmethod
    def is_enabled():
        """Toggle to enable/disable recipe. If disabled, recipe will be completely ignored."""
        return True

    @staticmethod
    def do_acceptance_test():
        """
        Whether to enable acceptance tests during upload of recipe and during start of Driverless AI.
        Acceptance tests perform a number of sanity checks on small data, and attempt to provide helpful instructions
        for how to fix any potential issues. Disable if your recipe requires specific data or won't work on random data.
        """
        return False

    def score(
            self,
            actual: np.array,
            predicted: np.array,
            sample_weight: typing.Optional[np.array] = None,
            labels: typing.Optional[np.array] = None) -> float:
   
    
        """
        cap actual/predicted to 0.01 - 0.99 to avoid log(0) problems
        """
        actual[actual<0.01]=0.01
        predicted[predicted<0.01]=0.01
        actual[actual>0.99]=0.99
        predicted[predicted>0.99]=0.99
        
        """"
        use log10 (as per chess kaggle comp)
        """
        
        score = -(actual*np.log10(predicted)+(1-actual)*np.log10(1-predicted))
        
        return(np.mean(score))
        
   
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
