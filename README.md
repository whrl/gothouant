# gothouant (under review)

`gothouant` provides tools for *project-specific* processing and analysis of stride-wise gait kinematics in walking insects. It was developed as a data-processing methods companion for the article:

> “Go thou to the ant: A comparative analysis of locomotion in a phylogenetically diverse sample of Hymenoptera (Hexapoda)”

The package focuses on importing leg trajectories from Tracker (`.trk` files, https://opensourcephysics.github.io/tracker-website/), deriving per-stride and per-cycle measures, visualizing gait patterns (including tripod geometries), and analysing differences between ants and non‑ant arthropods using linear mixed‑effects models.

## Installation

```r
# Development version from GitHub
# install.packages("remotes")
remotes::install_github("whrl/gothouant")
```

Then load the package:

```r
library(gothouant)
# data not yet pushed, rawdata and videos possibly on zenodo
```

## Key features

- Read multi-leg trajectories from Tracker `.trk` files (from the Tracker video analysis software)
- Organize raw 2D coordinates and metadata into tidy formats

- Derive stride-wise gait measures
- Compute leg- and midpoint-based (head-abdomen axis) stride length and stride speed
- Extract contact, swing, and duty times per leg and stride
- Aggregate across trials, legs, and individuals

- Visualize gait and tripod geometry
- Plot stride-wise gait metrics by leg, individual, or group (ant vs non-ant)
- Visualize tripod support patterns and midpoint paths

- Fit linear mixed-effects models with `lme4::lmer()` to compare ants and non-ants
- Account for repeated measures within individuals and species
- Optionally use standardized predictors to compare effect strengths across gait variables



## Citation

If you use `antwalk` in a publication, please cite the associated article:

> “Go thou to the ant: A comparative analysis of locomotion in a phylogenetically diverse sample of Hymenoptera (Hexapoda)”

(final citation once available, currently in review)

## Contributing

This package is tailored to document a very specific comparative locomotion project.

