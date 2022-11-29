import aesara.tensor as at
import arviz as az
import matplotlib.pyplot as plt
import numpy as np
import pymc as pm
import scipy.stats as stats

RANDOM_SEED = 108727
rng = np.random.default_rng(RANDOM_SEED)

def minmaxpost(base, *args):
    loglik = pm.logp(base, 52) + pm.logp(base, 84) + (10 - 2) * \
             at.log(at.exp(pm.logcdf(base, 84)) - at.exp(pm.logcdf(base, 52)))
    return loglik

with pm.Model() as model:
    mu=pm.Normal("mu", 0, 100);
    sigma=pm.HalfNormal("sigma", 100);
    base=pm.Normal("observations", mu, sigma)
    like=pm.Potential("likelihood", minmaxpost(base))

    idata=pm.sample(1500, progressbar = False)

az.summary(idata)
