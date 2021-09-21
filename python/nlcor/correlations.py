#' Compute Nonlinear Correlation
#'
#' Compute nonlinear correlation using adaptive spatial sampling.
#' @param x A numeric vector. NAs are not allowed. The length of the vector should be more than 10.
#' @param y A numeric vector. NAs are not allowed. Length should be same as `x`. The order of
#' `x` and `y` are relevant. `nlcor(x, y)`` is not equal to `nlcor(y, x)`. Therefore, `x`
#' should be a causal and `y` should be a dependent variable.
#' @param refine Optional. If manually set, a small value, e.g., 0.01, increases
#' the granularity of local correlation computation. The runtime is faster if `refine` is manually set.
#' Otherwise, the algorithm automatically finds the best refinement.
#' @param plt Optional. Default value FALSE. Set TRUE to return ggplot2 object
#' for the data correlation visualization.
#' @param line_thickness Optional. Default 1. Thickness of the correlation lines. It is a float argument > 0.
#' @param line_opacity Optional. Default 1, completely opaque. The opacity of the correlation lines. A float between 0-1. 0 is transparent.
#' @return The output is a list containing the nonlinear correlation \code{cor.estimate}, \code{adjusted.p.value}, and \code{cor.plot}. \code{cor.estimate}
#' is between 0 and 1 (a negative nonlinear correlation is undefined). The
#' \code{adjusted.p.value} shows the statistical significance of the
#' \code{cor.estimate}. If \code{adjusted.p.value > 0.05}, the nonlinear
#' correlation estimate can be considered as noise (statistically not
#' significant). If \code{plt = T}, \code{ggplot2} object is return for
#' plotting the identified local linear correlations in the data.
#' @keywords nonlinear correlation
#' @seealso \code{\link{cor}}
#' @export
#' @examples
#' library(nlcor)
#' library(ggplot2)
#' ncor <- nlcor(x1, y1)
#' ncor <- nlcor(x2, y2, plt = TRUE)
#' ncor <- nlcor(x3, y3, refine = 0.01, plt = TRUE)

#import library
import pandas as pd
import numpy as np
from scipy import stats
import statistics
import utilities as util
import visualization as plot
from numbers import Number

def nlcor(x,
          y,
          refine = None,
          plt = True,
          line_thickness = 1,
          line_opacity = 1,
          chart_title = None):
  
  # Initialization with linear correlation
  bestCor, bestPvalue = stats.pearsonr(x,y) # linear correlation
  bestCor = abs(bestCor)  
  bestPvalue = round(bestPvalue, 2)
  
  bestOptimalSegments = list(np.arange(1,len(x)+1))  # The entire segment of data
  if pd.isnull(refine) :
    refinements = np.arange(0.01, 0.16, 0.01)
    for refine in refinements:
      greedyOutput = self.NlcorGreedySearch(x = x,
                                            y = y,
                                            refine = refine)

      if round(greedyOutput['netCor']['cor_estimate'], 2) > round(bestCor, 2) and round(greedyOutput['netCor']['adjusted_p_value'], 2) <= round(bestPvalue, 2):
        # This nonlinear segmentation is better
        # Store this output
        bestCor = greedyOutput['netCor']['cor_estimate']
        bestPvalue = greedyOutput['netCor']['adjusted_p_value']
        bestOptimalSegments = greedyOutput['optimalSegments']
        bestRefine = refine
      else:
        pass
  else:
     greedyOutput = self.NlcorGreedySearch(x = x,
                                           y = y,
                                           refine = refine)
     bestOptimalSegments = greedyOutput['optimalSegments']
     
  if plt:
    # If plot output is required
    cor_plot = plot.PlotNlcor(df=pd.DataFrame(data={'x':x, 'y':y}),
                              segments=bestOptimalSegments,
                              pvalue=bestPvalue,
                              line_thickness=line_thickness,
                              line_opacity=line_opacity,
                              title=chart_title)
    print(cor_plot)

    return(list(cor_estimate = bestCor,
                adjusted_p_value = bestPvalue,
                cor_plot = cor_plot))
  else:
    return(list(cor_estimate = bestCor,
                adjusted_p_value = bestPvalue
    )
  )
  
    

def NlcorGreedySearch (x,
                       y,
                       refine = 0.05) :
      #df = pd.DataFrame({'x': list(x), 'y': list(y)}, columns=['x', 'y'])
      df = pd.DataFrame({'x': x, 'y': y}, columns=['x', 'y'])
      # Sorting by x to spatially sample the df afterwards.
      # The nlcor output is different if the df(x,y) is sorted
      # by y. Therefore, nlcor(x, y) <> nlcor(y, x).
      df = df.sort_values(by = 'x')
      df = df.reset_index(drop=True)
      df.index += 1 
      # Initialization
      l = len(x)
      s = util.ValidateRefine(l = l,
                         refine = refine)
      segments = util.Segment(l = l,
                         s = s)
      df_fit = pd.DataFrame()
      # Empty initialization
      segmentsCor = {'cor':None,'p_value':None}
     
      optimalSegments = list()
      last_segment_merged = False

      seg = segments[0]
      optimalSegments.insert(0,seg)
      
      if len(segments) >= 2:
          for i in range(1,len(segments)):
              previous_optimal_segment_cor = util.SegmentCorrelation(
              df.x[optimalSegments[len(optimalSegments)-1]],
              df.y[optimalSegments[len(optimalSegments)-1]])
              current_segment_cor = util.SegmentCorrelation(df.x[segments[i]],
                                                            df.y[segments[i]])
              combined_segment_cor = util.SegmentCorrelation(
              df.x[optimalSegments[len(optimalSegments)-1] + segments[i]],
              df.y[optimalSegments[len(optimalSegments)-1] + segments[i]])        
              merge_flag = False
              if np.sign(previous_optimal_segment_cor['cor']) == np.sign(current_segment_cor['cor']):
                  # Same direction.
                  if current_segment_cor['p_value'] <= 0.05:
                      # Check for current segment's statistical significance.
                      # Since the direction are same with statistical significance,
                      # we merge the segments.
                      merge_flag = True
                  else:
                      # Check if combined correlation is higher and merge even if
                      # the same direction is not statistically significant to
                      # err in favor of fewer segments.
                      if abs(combined_segment_cor['cor']) >= np.mean([abs(previous_optimal_segment_cor['cor']) , current_segment_cor['cor']]):
                         merge_flag = True         
              else:
                  # Direction is opposite
                  if current_segment_cor['p_value'] <= 0.05:
                      # Check for current segment's statistical significance, if
                      # significant, do NOT merge.
                      merge_flag = False
                  else:
                      #If there is no statistical significance, the change
                      # in direction could be due to noise. So check if combined
                      # correlation is higher and merge.
                      if (abs(combined_segment_cor['cor']) >=
                           np.mean([abs(previous_optimal_segment_cor['cor']),current_segment_cor['cor']])):
                            merge_flag = True  
      
              if merge_flag:
                  # The correlation directions are the same in segments i-1 and i.
                  # Merge the segments
                  seg = optimalSegments[len(optimalSegments)-1] + segments[i]
                  optimalSegments[len(optimalSegments)-1] = seg
                  if i == len(segments):
                      # Update the last segment is merged flag
                      last_segment_merged = True                         
                  else:
                     # Direction changes.
                     # Now work with the previously merged segments
                     # Populate the segmentwise correlation
                     segmentsCor = util.UpdateSegmentsCor(x_seg = df.x[seg],
                                                          y_seg = df.y[seg],
                                                          segmentsCor = segmentsCor)

                     # Reset the segment
                     seg = segments[i]
                     optimalSegments[len(optimalSegments)-1] = seg    

          # Look at the last segment if it still remains
          if last_segment_merged:
              # Populate the segmentwise correlation
              segmentsCor = util.UpdateSegmentsCor(x_seg = df.x[seg],
                                                   y_seg = df.y[seg],
                                                   segmentsCor = segmentsCor)

          else:
              lastSeg = segments[len(segments)-1]
              optimalSegments[len(optimalSegments)-1] = lastSeg
              # Now work with the last unmerged segment
              # Populate the segmentwise correlation
              segmentsCor = util.UpdateSegmentsCor(x.seg = df.x[lastSeg],
                                                   y.seg = df.y[lastSeg],
                                                   segmentsCor = segmentsCor)
                 
      elif len(segments) == 1:     
          # Populate the segmentwise correlation
          segmentsCor = util.UpdateSegmentsCor(x.seg=df.x[seg],
                                               y.seg=df.y[seg],
                                               segmentsCor = segmentsCor)
  
      netCor = self.NetCor(cors = segmentsCor['cor'],
                   p_values = segmentsCor['p_value'],)
      
      return({'netCor':netCor,
             'optimalSegments': optimalSegments})


def NetCor(cors,
           p_values,
           p_threshold = 0.05):
           
    adjusted_p_threshold = p_threshold / len([cors])
    for i in range(0,len([cors])):
        if [p_values][i]>adjusted_p_threshold or [p_values][i]==None:
            [p_values][i] = None
            [cors][i] = 0
    
    netcor = {'cor_estimate':np.mean([abs(ele) for ele in [cors]]),
              'adjusted_p_value':1 - np.prod([(1-d) for (d, remove) in zip([p_values], [isinstance(el, Number) for el in [p_values]]) if remove]),
              'segment_cor':{'cor':[cors], 'pvalue':[p_values]}
             }
    return (netcor)