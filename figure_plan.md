# Outline of figures
A more visual depiction (albeit crude) is in https://github.com/advayvyas/ForecastEvaluation/issues/1#issuecomment-4724427937. The metrics can change freely in the development process from WIS to rWIS to RSS if needed/recommended.

## Figure 1
This figure will be split into four distinct plots arranged roughly in four rectangles inside a larger rectangle. The top left rectangle will be a bar graph of relative WIS by model, sorted by relative WIS. The top right rectangle will be a bar graph by location instead of model. The bottom left rectangle will be a snapshot of a chosen state (Texas for now but any state with a high amount of HSA coverage on the state will do) and we will look at the state and rWIS by state and how the state forecasts perform for urban and rural centers in that state. Lastly, on the bottom right rectangle, we will have create a table of coverages and then use that to create a pie chart of predictions (over/under/on target predictions).

## Figure 2
This figure will be split into two distinct plots arranged side-by-side in two tall rectangles. Each rectangle will contain a plot of effects. The left rectangle will contain the model effects while the right rectangle will contain the location effects. Both the effects will be from a regression model on the relative skill scores (RSS), likely.

## Figure 3
This figure will be split into three distinct plots with the left third occupied by one rectangle and the right will be split horizontally in half. For the leftmost rectangle, it will contain a plot of models (only six so this is doable) where the models are sorted by WIS and each model will have a bar which is split into three regions: overpredict, underpredict, and on target prediction. The top right rectangle will contain the coverage values with shading for easy visibility, each model and each horizon will have a coverage value. The bottom right rectangle will show WIS by the date in a line graph across all six of the models to see if there was a gap during the peak. For easy comparison, the truth graph will be either overlaid or underneath to see when the WIS peaks (and the models become less accurate).

## Figure 4
This figure will be split into six distinct plots with 3 evenly spaced columns and 2 evenly spaced rows. The three columns will be the epidemic stages of pre-peak, peak, and post-peak as determined by the truth curve (subject to change). The two rows will be state predictions and local predictions. Each cell will contain a plot of model effects (all 6 models) and the intercept as well, similar to the effect plots in figure 2.
