# Outline of figures
A more visual depiction (albeit crude) is in https://github.com/advayvyas/ForecastEvaluation/issues/1#issuecomment-4724427937. The metrics can change freely in the development process from WIS to rWIS to RSS if needed/recommended.

## Figure 1
This figure will be split into four distinct plots arranged roughly in four rectangles inside a larger rectangle. The top left rectangle will be a bar graph of relative WIS by model, sorted by relative WIS. The top right rectangle will be a bar graph by location instead of model. The bottom left rectangle will be a snapshot of a chosen state (Texas for now but any state with a high amount of HSA coverage on the state will do) and we will look at the state and rWIS by state and how the state forecasts perform for urban and rural centers in that state. Lastly, on the bottom right rectangle, we will have create a table of coverages and then use that to create a pie chart of predictions (over/under/on target predictions).

Top left: makes sense-- overall summary of scores. Will rWIS be relative to the baseline? Will this include only the HSAs or also the states? Will this include all of the models or only the 6 that submitted the majority of the season? I think for this one, we'd maybe want to focus on only the HSAs since this is meant to be comparing model performance at the local level. If rWIS, would maybe not use a bar chart (maybe just points centered around 1 with a log scale e.g. rWIS = 0.5 equidistant from rWIS = 2), but if plotting WIS I think it makes sense. Could do WIS with breakdowns by underprediction, overprediction, and dispersion as a bar chart
Top right: I think you want this by location and model, not just location. Same comment about rWIS as a dot plot and WIS as a bar chart.
Bottom left: Will this show actual forecasts for each model over time for a subset of HSAs? I do think something like this would be useful somehwere early on to remind the reader/ give the reader an overall sense of the problem. Dongah has already made some nice figures like this, which maybe should be included as a separate figure e.g. Figure 0 (we can renumber). 
Bottom right: This could also be a stacked bar chart by model of coverage? Somewhat similar to top left because averaging over locations and models but showing a different aspect of forecast quality. 

## Figure 2
This figure will be split into two distinct plots arranged side-by-side in two tall rectangles. Each rectangle will contain a plot of effects. The left rectangle will contain the model effects while the right rectangle will contain the location effects. Both the effects will be from a regression model on the relative skill scores (RSS), likely.

This seems reasonable! Would add a dotted vertical line at 1? 

## Figure 3
This figure will be split into three distinct plots with the left third occupied by one rectangle and the right will be split horizontally in half. For the leftmost rectangle, it will contain a plot of models (only six so this is doable) where the models are sorted by WIS and each model will have a bar which is split into three regions: overpredict, underpredict, and on target prediction. The top right rectangle will contain the coverage values with shading for easy visibility, each model and each horizon will have a coverage value. The bottom right rectangle will show WIS by the date in a line graph across all six of the models to see if there was a gap during the peak. For easy comparison, the truth graph will be either overlaid or underneath to see when the WIS peaks (and the models become less accurate).

This sounds like a great figure! Will this be for all HSAs and states? Wondering if we should separate performance at the state and HSA level by model(and then maybe Figure 1 and 3 don't have to be separate if we can manage to show both pieces of information in a single figure?). Am not entirely sure the best way to go about this 

## Figure 4
This figure will be split into six distinct plots with 3 evenly spaced columns and 2 evenly spaced rows. The three columns will be the epidemic stages of pre-peak, peak, and post-peak as determined by the truth curve (subject to change). The two rows will be state predictions and local predictions. Each cell will contain a plot of model effects (all 6 models) and the intercept as well, similar to the effect plots in figure 2.

This seems very interesting! Do we expect to have model effects that differ by pandemic phase though e.g. is this an interaction term in the model? I think it could be, but I don't think we pre-specified it that way. Would be good to talk about this more. 
