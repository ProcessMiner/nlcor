import pandas as pd
import numpy as np
from scipy import stats
import math
import statsmodels.api as sm
from sklearn.linear_model import LinearRegression

MIN_SEGMENT_SIZE = 3

def UpdateSegmentsCor(x_seg,
                      y_seg, 
                      segmentsCor):
    segcor = stats.pearsonr(x_seg, y_seg)
    segmentsCor['cor'] = [segcor[0] if segmentsCor['cor'] == None else segmentsCor['cor'] + segcor[0]]
    segmentsCor['p_value'] = [segcor[1] if segmentsCor['p_value'] == None else  segmentsCor['p_value']+ segcor[1]]
    return(segmentsCor)

def Segment(l, s):
    windowLen = math.floor(s * l)

    numSegments = l // windowLen
    segments = []
    upp = windowLen
    low = 1
    for i in range(1,numSegments+1):
        segments.insert(i,list(range(low,upp+1)))
        low = upp + 1
        upp = upp + windowLen            
    if (l % windowLen != 0):
       seg = list(range(low,l+1))
       if len(seg) <= MIN_SEGMENT_SIZE:        
       # Merge with the last segment
           segments.insert(len(segments),[segments[len(segments)-1]+seg])    
       else:
           segments.insert(len(segments) + 1,seg)       
    return(segments)


def SegmentCorrelation (x_seg,
                        y_seg) :
    
  temp = stats.pearsonr(x_seg,
                        y_seg)
  cor = 0 if pd.isna(temp[0])==True else temp[0]     
  p_value = 1 if pd.isna(temp[0]) else temp[1]                                                      
  segmentCorrelation = {'cor':cor,'p_value':p_value}
  return(segmentCorrelation)

def ValidateRefine (l, 
                    refine):  
  if math.floor(refine * l) < MIN_SEGMENT_SIZE :
    # Means too few data points in a segment.
    # Therefore, increase it.
    while (math.floor(refine * l) <= MIN_SEGMENT_SIZE) :
      refine = refine + 0.01
  return(refine)

def UpdateDfFit (seg,
                 df, 
                 df_fit):
    data = df.iloc[seg]
    y= data['y']
    x= data['x']
    fit = sm.OLS(y, 
                 x,
                 data = data).fit()
    if len(fit.pvalues) == 2:
        fit_significance = fit.pvalues
    else :
        fit_significance = None
    if pd.isna(fit_significance) or fit_significance > 0.01 :
      # TODO: Make the pvalue threshold of 0.01 adjusted as per Bonferroni
      # When the fit is not statistically significant.
      df_fit = pd.concat([df_fit,
                     pd.DataFrame(data = {'x':df["x"].iloc[seg],
                                  'fit': fit.predict()})])  # Adding the fitted values as NA for plotting because no real correlation exist here.
      df_fit.loc[len(df_fit.index)] = None # Last point set to NA for plotting beautification. It results into disjoint lines. Otherwise, the plot is ugly with several cliffs.
    else :
      # Fit is statistically significant.
      df_fit = pd.concat([df_fit,
                     pd.DataFrame(data = {'x': df["x"].iloc[seg],
                                'fit': fit.predict()})])  # Adding the fitted values for plotting.
      df_fit.loc[len(df_fit.index)] = None  # Last point set to NA for plotting beautification. It results into disjoint lines. Otherwise, the plot is ugly with several cliffs
    return(df_fit)
