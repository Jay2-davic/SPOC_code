from scipy.stats import norm

EPSILON = 1E-07

class ACQUISITION(object):
    def __init__(self):
        pass

    def calculate(mean, std, y_best):
        pass


def EI(mean, std, y_best):
    z = (y_best - mean) / (std + EPSILON)
    return (y_best - mean) * norm.cdf(z) + std * norm.pdf(z)

def LCB(mean, std, y_best=None, ratio=10):
    return - mean + ratio * std

def PI(mean, std, y_best):
    z = (y_best - mean) / (std + EPSILON)
    return norm.cdf(z)
