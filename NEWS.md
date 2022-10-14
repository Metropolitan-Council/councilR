# v.0.1.4 (2022-10-14)

## `theme_council()` font patch

`theme_council()` now uses font family names on the machine to determine file names. 

Previously, in order to use Council fonts like Helvetica Neue LT Std, the user must have installed the font family with a particular file name "HelveticaNeueLTStd-Cn.otf". Otherwise, if Helvetica Neue LT Std  was installed using another file name, like "HelveticaNeueLTStd-Cn_0.otf", `theme_council()` would throw an error. 

This patch examines the available font families and determines the file name from there, instead of requiring a single precise file name.  

# v.0.1.3 (2022-08-03)

## New features

- `fetch_county_geos()` uses `{tigris}` to create an `sf` object. It takes one argument, `core`, which indicates whether to include only the core 7-county metro or the 9-county MPO area.
- Input checks using `{rlang}`
- New vignette detailing helpers for accessing spatial data, including snippets.
- New contributor guidelines. 
- `council_theme()` is officially deprecated. Function will return error with message
- Improved testing
    - Package now uses `{httr2}` to access DB credentials securely. If you want to develop and test the package, you must contact @eroten for the `COUNCILR_KEY` to decrypt.
    - Added testing for `colors`
    
### Dependency changes

- `{tigris}`, `{cli}`, `{httr2}` added
- `{repmis}` removed

### General updates

- More uniform documentation standards
- RMarkdown template 
    - New default text in RMarkdown template
    - Remove `{repmis}` and use integrated `{knitr}` function to fetch package dependencies. 



# v0.1.0.9000 (2020-10-21)  

## New functions

* `import_from_gpkg()` function allows the user to import an `sf` object from a geopackage URL. This is particularly useful for accessing Minnesota Geospatial Commons if you are unable to access the Council's ArcGIS catalog. 

# v0.1.0  (2020-05-26)

## New functions  

* `import_from_gis()` function allows the user to import a shapefile from the Council's ArcGIS catalog.  

## Housekeeping  

* Added a `NEWS.md` file to track changes to the package.  
* Updated [Contributor Code of Conduct](.github/CODE_OF_CONDCT.md).  
