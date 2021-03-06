
<!-- README.md is generated from README.Rmd. Please edit that file -->
`bwgbiomass` : estimating biomass from allometry data
=====================================================

Uses the [BWG allometry data](https://github.com/SrivastavaLab/allometrydata) to estimate biomass for available species.

Installation
------------

``` r
devtools::install_github("ropenscilabs/datastorr")
devtools::install_github("SrivastavaLab/fwdata")
devtools::install_github("SrivastavaLab/bwgbiomass")
```

Usage
-----

Acquire the latest release of [allometry data](https://github.com/SrivastavaLab/allometrydata) or biomass data

``` r
# Acquire the latest allometry data release

allometry_data <- allometry()

# Acquire the latest bwgbiomass data release

biomass_data <- biomass()
```

Generate the biomass table fresh

``` r
biomass_data <- gen_biomass()
```

You can also create intermediate data frames needed to generate the final biomass table. See documentation below for a description of the data frames and the columns they contain:

``` r
# The allometry matrix

allometry_matrix <- create_allometry_matrix()

# The equation bank:

equation_bank <- create_equation_bank()

# The category lookup table:

category_lookup <- create_category_lookup()

# The biomass table cleared of original biomass info:

biomass_table <- create_biomass_table()
```

To do a data comparison with the original biomass estimates:

``` r
# Get the latest data

biomass_data <- biomass()

# Do the data check

check_data <- sanity_check(biomass_data)
```

Data Version Numbering
----------------------

Data versions are numbered with a `bwgbiomass` version and an `allometrydata` version.

For example:

**v.0.0.1\_0.0.1**

The numbers before the `_` indicate which version of bwgbiomass is being used. When updates are made to the bwgbiomass code, this number will change. The numbers after the `_` indicate which version of the [allometry data](https://github.com/SrivastavaLab/allometrydata) was used. When allometry data are added or changed, this number will change.

Documentation
-------------

### General Assumptions

-   Species in the biomass table with NA `length` and `size_category` are *average* `size_category`
-   For species with a size `range`, `length_mm` is computed as the mean of the range
-   Species with missing stage are given the `stage` *larva*, except for ostracods, which are marked as *adult*
-   In the allometry matrix, `biomass_mg` was computed by dividing by `number_of_individuals`
-   Species found at multiple sites are assumed to have the same biomass x length relationship and the linear model was built using all data, not separated by site, habitat, or researcher
-   Sizes in the biomass table with an underscore (`_`) are assumed to be a size range and the average of the numbers on either side of the `_` was taken
-   Species in the allometry matrix with no `bwg_name` are assumed not to have a `bwg_name` and given a placeholder name of `MISSING_X` where `X` is a unique number for that species

### Allometric equations

Allometric equations are calculated by linear regression of *l**o**g*<sub>10</sub>(mass\_mg) against *l**o**g*<sub>10</sub>(length\_mm).

95% confidence intervals were computed for biomass predictions.

### Provenance

To determine biomass, species from the biomass table are looked up in the allometry matrix. The size specified in the biomass table is matched to the allometry matrix. If not found, it is either derived using an equation bank, or derived using the closest taxonomic relative.

Biomass was determined in the following order, with the meanings of `provenance` defined below:

1.  `length.raw`: Exact length was found in the allometry matrix and used directly to determine biomass
2.  `length.interp`: Length was used to interpolate biomass from the equation bank
3.  `category.raw`: Size category was used to look up a length estimate, which was found in the allometry matrix. Raw biomass was used based on that length estimate.
4.  `category.interp`: Size category was used to look up a length estimate, which was not found in the allometry matrix. Biomass was interpolated based on that length estimate, using the equation bank.

Meanings of `provenance_species`:

1.  `exact`: Exact species was found in the allometry matrix
2.  `related`: The most closely related species (taxonomically) from the allometry matrix was used

### Computing biomass when there is more than one closest relative

When more than one closest relative was available, those relatives with highest quality data were prioritized, and median biomass taken. Following the order of `provenance` above:

1.  Look for relatives with `length_mm` matching the `length_mm` of the target species in the biomass table. If only one relative had a matching `length_mm`, this was used. If more than one relative was found, the median biomass was used. Provenance `length.raw` was assigned. If no relatives were found, go to step 2.
2.  Look for relatives with an equation in the equation bank. Biomass of all relatives was computed using the `length_mm` of the target species in the biomass table. The median of computed biomasses was used and provenance `length.interpolate` assigned.
3.  If the target species has only `length_est_mm`, look for relatives with `length_mm` matching the `length_est_mm` of the target species in the biomass table. The median of all such relatives was used and provenance `category.raw` assigned. If no relatives were found, go to step 4.
4.  Look for relatives with an equation in the equation bank and calculate biomass using `length_est_mm`. Use the median of calculated biomasses.

If no relatives were found in any of the steps, no biomass was assigned.

All close relatives used to compute a median biomass are named in `closest_relative`. Because of the way median was computed, the number of species named in `closest_relative` may differ from the number in `num_relatives`.

Dry biomass was always prioritized over wet biomass.

If two species had the same median biomass, the lower value was used. This gives a more conservative biomass estimate.

### Output

Description of output files and column meanings

#### Allometry matrix

The allometry matrix generated by `create_allometry_matrix()`: The imported and joined XLS files; column descriptions here are copied from the XLS template where possible

-   `bwg_name`: BWG name assigned to each species in the BWG database (e.g., Diptera.200). You can check the BWG names for each species in BWGdb. However, if there is still no BWG name assigned to the species you are to include here, please enter 'NA'.
-   `name`: (morpho)species name assigned by each researcher working at a given field site.
-   `length_mm`: Length of the individual, in millimitres.
-   `length_measured_as`: Information on how the body length of this individual or taxa was measured: e.g., head capsule only, from the head to the last abdominal segment, etc; if this information is not available, please enter 'NA'.
-   `number_of_individuals`: The number of individuals in a pooled sample of mass. For example, if we had to weigh 10 tiny instars of chironomid together because the mass of a single one was below the detection limit, then number of individuals = 10.
-   `stage`: **larva**, **pupa** or **adult**
-   `size_category`: size category name (if one assigned by researcher, e.g. "small",otherwise please enter 'NA')
-   `instar_number`: instar number (if one noted, otherwise enter 'NA')
-   `biomass_mg`: Wet or dry mass of the individual, in milligrams
-   `biomass_type`: **Wet** or **dry** biomass

#### Equation bank

The allometric equation bank, generated with the function `create_equation_bank()`

-   `bwg_name`: The BWG name from the database
-   `stage`: The developmental stage (i.e. "larva", "pupa", "adult")
-   `biomass_type`: **Wet** or **dry** biomass
-   `fit`: The linear model fit
-   `r_squared`: *R*<sup>2</sup> for the allometric equation
-   `sample_size`: sample size for the allometric equation
-   `intercept`: intercept for the allometric equation
-   `slope`: slope for the allometric equation

#### Category lookup table

The category lookup table for matching a category to a length estimate, generated with the function `create_category_lookup()`

-   `bwg_name`: The BWG name from the database
-   `stage`: The developmental stage (i.e. "larva", "pupa", "adult")
-   `size_category`: The categorical name of the size (e.g. "small", "medium")
-   `length_est_mm`: The estimated length for individuals in this size category, measured in millimetres

#### Biomass table

The biomass table acquired from Dropbox and cleared of biomass measurements, generated with the function `create_biomass_table()`

-   `species_id`: a unique ID for the species
-   `measurement_id`: a unique ID for the size measurement
-   `bwg_name`: The BWG name from the database
-   `category_range`: Length measured as a category, or a range?
-   `stage`: The developmental stage (i.e. "larva", "pupa", "adult")
-   `size_category`: The categorical name of the size (e.g. "small", "medium")
-   `length_mm`: The length (if available) of the individual in millimetres

#### Biomass estimates table

The full table of new biomass estimates, using the function `gen_biomass()`

-   `species_id`: a unique ID for the species
-   `measurement_id`: a unique ID for the size measurement
-   `bwg_name`: The BWG name from the database
-   `stage`: The developmental stage (i.e. "larva", "pupa", "adult")
-   `length_measured_as`: length\_measured\_as: Information on how the body length of this individual or taxa was measured: e.g., head capsule only, from the head to the last abdominal segment, etc; if this information is not available, please enter 'NA'.
-   `length_mm`: The length (if available) of the individual in millimetres
-   `length_est_mm`: If a category was used to make an estimate of length, that estimate is placed in this column
-   `biomass_type`: **Wet** or **dry** biomass
-   `biomass_mg`: Wet or dry mass of the individual, in milligrams
-   `biomass_ci_upr` and `biomass_ci_lwr`: bounds on the 95% confidence interval of biomass\_mg if interpolation was used
-   `provenance`: method used to calculate biomass
-   `provenance_species`: species on which biomass calculation was performed
-   `closest_relative`: The name of the closest relative. If this contains multiple bwg\_names, the median biomass of those was used
-   `num_relatives`: how many relatives found in the closest shared taxonomic group
-   `shared_taxon`: at which taxonomic level were relatives found
-   `r_squared`: *R*<sup>2</sup> for the allometric equation
-   `sample_size`: sample size for the allometric equation
-   `intercept`: intercept for the allometric equation
-   `slope`: slope for the allometric equation

#### Data check

Comparison of biomass measurements with Jana's values (from the original table). Generated using the function `sanity_check()`

-   `measurement_id`: a unique ID for the size measurement
-   `species_id`: a unique ID for the species
-   `bwg_name`: The BWG name from the database
-   `stage`: The developmental stage (i.e. "larva", "pupa", "adult")
-   `length_mm`: The length (if available) of the individual in millimetres
-   `provenance`: method used to calculate biomass
-   `provenance_species`: species on which biomass calculation was performed
-   `biomass_orig_mg`: Wet or dry mass of the individual, in milligrams found in the original biomass table
-   `biomass_new_mg`: Wet or dry mass of the individual, in milligrams calculated by this script
-   `new_over_old`: Original biomass divided by my biomass; a value close to one indicates a close match; A value larger than one means the original biomass was larger; A value less than one means the new biomass was larger; Many of the original biomass values are in grams, so were first multiplied by 1000 to get mg.
-   `biomass_ci_upr` and `biomass_ci_lwr`: bounds on the 95% confidence interval of biomass\_new\_mg if interpolation was used
-   `r_squared`: *R*<sup>2</sup> for the allometric equation used to generate biomass\_new\_mg, if applicable

Future Improvements
-------------------

The package is currently very reliant on other packages to function. This makes it on the heavy side.

License
-------

MIT + file LICENSE
