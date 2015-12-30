
"""
Normal gaussian functions
"""

from math import exp, sqrt, pi

def cdf(x):
    """Normal cumulative distribution function.

    From http://en.wikipedia.org/wiki/Bc_programming_language.

    Algorithm is from 
    George Marsaglia, Journal of Statistical Software,
    July 2004, Vol 11, Issue 5
    """
    s = x
    t = 0.0
    b = x
    q = float(x*x)
    i = 1

    while s != t:
        t = s
        i += 2
        b *= q / i
        s = t + b

    return .5+s*exp(-.5*q-.91893853320467274178)

def pdf(x):
    """Normal probability distribution function."""
    return exp(-x*x/2)/sqrt(2*pi)


def newtons_method(f, fp, x0, eps=1e-15):
    """Find the root of F with newton's method

    http://en.wikipedia.org/wiki/Newton's_method
    """
    
    i = 0
    while True:
        d = fp(x0)
        if d == 0.0:
            # derivative is zero.  stuck.
            raise Exception("Cannot use newton's method")
        
        x1 = x0 - f(x0) / d
        
        if abs(x0-x1) < eps: break
        i += 1
        if i > 20 : break
        x0 = x1
    return x1

def invcdf(x):
    """Inverse cdf of the normal distribution."""

    # Check the extreme values
    if x < 5.5e-16:
        return -10.0
    elif x >= 0.999999999:
        return 10.0
    else:
        # Then solve.
        def f(y):
            return cdf(y) - x
        return newtons_method(f, pdf, 0.0)
    
        
        

    
