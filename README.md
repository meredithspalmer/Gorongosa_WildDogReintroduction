From Meredith, August 2024:

+ These are only the wild dog packs which overlap with the camera trap grid
+ There are two "effort" files
    + One showing, for each pack, what percentage of each season-year combo we have at least GPS record for that day ("wd_effort_seasoncoverage.csv")
    + The other has the start-stop dates of each period of continuous recording, for each pack ("wd_effort_dates.csv")
+ In terms of effort, I'm less worried about gaps of a few days between pings - the statistics we use to derive the home ranges can account for variable recordings. What does emerge is that we don't start monitoring some packs until partway through a season or stop monitoring a few weeks before a season ends. 
    + For the Gorongosa (1st released pack), we may want to ditch the first season of data (low coverage, pack wasn't present during period with no data)
    + For the other packs (where monitoring ends before season does), the packs will still be present even if we aren't recording data. Some of these data are very spare and produce crap models - should we ditch, keep, or extrapolate? 
    + I will flag these in the data for review; note that some of the home ranges generated should not be used until we decide what to do with data-poor HRs
+ There is a csv that records which models were used to construct each HR ("wd_season_modelselection.csv")
