import pandas as pd
import numpy as np
import utilities as util
import matplotlib.pyplot as plt
import seaborn as sns
sns.set_context("talk")
sns.set_palette("rocket")

def PlotNlcor(df,
              segments,
              pvalue,
              line_thickness = 1,
              line_opacity = 1,
              title = None):

  df_fit = pd.DataFrame()
  for i in range(0,len(segments)):
    # Populate the segmentwise linear fit for plotting
    df_fit = util.UpdateDfFit(seg = segments[i],
                              df = df,
                              df_fit = df_fit)
    if all(pd.isna(df_fit['fit'])):
        # Exception: Only one segment AND the correlation
        # is statistically insignificant.
        df_fit['fit'] = np.mean(df["y"])
    # Configure line type based on its statistical significance
    if(pvalue < 0.05):
      linestyle = "solid"
    else:
     linestyle = "dashed"
   #Add the plots
    fig,ax = plt.subplots()
    ax = sns.scatterplot(x, y, data = df,ax =ax, palette="b")
    ax = sns.lineplot(x, y='fit', 
                      data = df_fit,
                      ax=ax,
                      color = 'red',
                      linestyle = linestyle,
                      alpha =line_opacity )
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title(title)
    
    return (ax)