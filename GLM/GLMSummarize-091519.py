# Name: GLMSummarize.py
# Date: 22 Feb 2019
# Creator: Joseph Hicks
# Python Version: 3.6
# Purpose: Sort and remove zeroes from Coefficients (from a GLM analysis).
#         Summarize the coefficients with mean, median and 95%HPD.
#         Calculate BFs for GLM indicators
# Input: Directory containing multiple GLM log files
########################################################################################################################
import pandas as pd
import numpy as np
import os, shutil
import re

"""
This code (functions calc_min_interval and hpd) was taken form the PyMC library https://github.com/pymc-devs/pymc
"""


def calc_min_interval(x, alpha):
    """Internal method to determine the minimum interval of a given width
    Assumes that x is sorted numpy array.
    """

    n = len(x)
    cred_mass = 1.0 - alpha

    interval_idx_inc = int(np.floor(cred_mass * n))
    n_intervals = n - interval_idx_inc
    interval_width = x[interval_idx_inc:] - x[:n_intervals]

    if len(interval_width) == 0:
        raise ValueError('Too few elements for interval calculation')

    min_idx = np.argmin(interval_width)
    hdi_min = x[min_idx]
    hdi_max = x[min_idx + interval_idx_inc]
    return hdi_min, hdi_max


def hpd(x, alpha=0.05):
    """Calculate highest posterior density (HPD) of array for given alpha.
    The HPD is the minimum width Bayesian credible interval (BCI).
    :Arguments:
        x : Numpy array
        An array containing MCMC samples
        alpha : float
        Desired probability of type I error (defaults to 0.05)
    """

    # Make a copy of trace
    x = x.copy()
    # For multivariate node
    if x.ndim > 1:
        # Transpose first, then sort
        tx = np.transpose(x, list(range(x.ndim))[1:] + [0])
        dims = np.shape(tx)
        # Container list for intervals
        intervals = np.resize(0.0, dims[:-1] + (2,))

        for index in make_indices(dims[:-1]):
            try:
                index = tuple(index)
            except TypeError:
                pass

            # Sort trace
            sx = np.sort(tx[index])
            # Append to list
            intervals[index] = calc_min_interval(sx, alpha)
        # Transpose back before returning
        return np.array(intervals)
    else:
        # Sort univariate node
        sx = np.sort(x)
        return np.array(calc_min_interval(sx, alpha))

def BayesFactor(indicators, n):
    from math import exp, log
    prior = 1 - exp(log(0.5)/n)
    posterior = indicators.mean()
    if posterior == 1:
        posterior = (len(indicators) - 1) / len(indicators)
    BF = (posterior/(1 - posterior))/(prior/(1 - prior))
    return BF


def main():
    # Identify user selected directory path; only ".glm.log" files from Beast v1.10 should be within the directory.
    dirname = input("Enter directory path: ")
    os.chdir(dirname)
    print(os.getcwd())
    # Create empty dataframe with desired columns
    output = pd.DataFrame(columns=['Model', 'Trait', 'Variable', 'CoefMedian', 'CoefLowerHPD', 'CoefUpperHPD', 'pp', 'BF'])

    # Loop through files in the directory
    for filename in os.listdir('.'):
        # Ignore the hidden '.DS_Store' file used in Mac OS and the output file
        if filename != '.DS_Store' and filename != 'GLMSummary.txt':
            print(filename)
            model = os.path.splitext(filename)[0]
            splitfile = filename.split(sep='.')
            trait = splitfile[-3]
            print(model)
            GLMdata = pd.read_table(filename)
            varNum = re.findall('\d+', GLMdata.columns[-1])[0]
            # print(varNum)
            for col in GLMdata.columns:
                if 'Times' in col:
                    varID = re.findall('\d+', col)[0]
                    print(varID)
                    indcol = trait+'.coefIndicators'+varID
                    indicators = GLMdata[indcol]

                    if max(indicators) == 0:
                        interval = [0, 0]
                        median = 0
                    else:
                        NoZero = GLMdata[col].replace(0, np.NaN)
                        NoNan = NoZero.dropna()
                        interval = hpd(NoNan)
                        median = NoNan.median()

                    pp = indicators.mean()
                    bf = BayesFactor(indicators, int(varNum))

                    output = output.append(pd.Series([model, trait, varID, median, interval[0], interval[1], pp, bf], index=output.columns), ignore_index=True)

    output.to_csv('GLMSummary.txt',sep='\t')
    print("Done!")
main()


