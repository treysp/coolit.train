# coolit

coolit is an R package for identify water cooling towers from high-resolution aerial imagery. It consists of functions for:

- Slicing large images into smaller pieces the model can use
- Training a convolutional neural network model on slices known to either contain a cooling tower or not
- Scoring slices with a trained model

This repo contains both functions (as a normal R package does) and code I used to apply the functions in the 'analysis' directory. It is an R package that can be installed from Github using `remotes::install_github()`. Hoewver, the installed package will not contain the 'analysis' directory. If you want that code, you should pull the entire repo then build the package locally using devtools.

# coolit model

coolit identifies images containing a water cooling tower with a convolutional neural network. 

Because we have relatively few tower images, it uses transfer learning instead of training a model from scratch. It uses VGG16 as a base model, trains dense layers on top of that base, then simultaneously fine-tunes the last VGG16 and dense layers.

The model training functions allow the user to specify the model architecture, but as of 2019-05-16 I am using one 256-node and one 128-node layer with dropout of 0.2 for each. 

# coolit data

coolit requires ortho-rectified aerial imagery at resolution 6 inches per pixel or better. Currently, publicly available satellite imagery does not go below about 1 foot per pixel so cannot be used.

Suitable imagery is available from multiple public sources - the ones we have found or used are described in the [project history](#project-history) and [Image sources](#image-sources) sections below.

# Project history

coolit began with known point coordinates of buildings that had cooling towers in New York City, Philadelphia, and Chicago. We used these to label imagery and train our model.

We located imagery for NYC at the [NY State GIS](http://gis.ny.gov/gateway/mg/nysdop_download.cfm) office. We also discovered a publicly available source of spatial data containing specific cooling tower locations at the [NYC Open Data](https://data.cityofnewyork.us/City-Government/Cooling-Tower/miz8-534t) portal. We examined the images intersecting with a tower polygon to create labeled imagery. We did not use every polygon because some towers were not visible (either under another structure or mounted sideways such that the fan is not visible from above). We then trained the model on these labeled images.

We located imagery of Philadelphia from the [Pennsylvania Geospatial Data Clearinghouse](http://www.pasda.psu.edu/uci/SearchResults.aspx?Shortcut=aerial). We examined image slices within a 50 foot radius around the coordinate points of building with towers to determine whether they contained a tower. We then re-trained the model on the labeled NYC and Philadelphia data.

We located imagery of Chicago on the [Cook County image server](https://gisimageserver.cookcountyil.gov/arcgis/rest/services). We examined image slices within a 50 foot radius around the coordinate points of building with towers to determine whether they contained a tower. We then re-trained the model on the labeled NYC, Philadelphia, and Chicago data.

# Lessons learned

I had limited experience with spatial data, no experience with image data, and no experience with computer vision models when I started coolit. This section describes some things I have learned, some of which are specific to this project and many of which are likely known by anyone with more experience.

## 1. Building a model that worked well wasn't that hard

For this task, the modeling piece was surprisingly easy. Based on the book [Deep Learning with R](), I started off using transfer learning with the VGG16 model base, and that is what I'm still using. 

Experiments with other base models and different dense layer structures showed equivalent or poorer performance to VGG16. I assume this is in part to the relatively small number of tower images we have in hand. However, it is also due to my inexperience with these models, and I would welcome input/advice on ways to improve the model.

Because there are far more non-tower images than tower images, the model classes are severely 'unbalanced.' I addressed that by weighting the model loss function based on the relative class proportions.

## 2. Model performance is mostly dependent on the data

### Image slice size

I started out with labeled images sized 250 pixels square, which at 6in/pixel is 125 feet on the ground. The model had performance equivalent to randomly guessing the dominant non-tower class no matter what I did. After checking to make sure this wasn't straight user error, I hypothesized that images that large contained too much noise for the model to learn what was different between tower and non-tower images. This turned out to be correct.

Source imagery seems to come in 5000 pixels square, so I chose 50 pixels square slices for convenience. The model immediately showed performance very high on standard model metrics.

### Image labeling strategy

I began as a tower presence purist. Often images only contain a portion of a tower, and I labeled all those images as having tower regardless of how much tower it contained. This was a poor strategy.

If one image slice contains 5% of a given tower, the other 95% appears in other slices. Since we care about identifying *towers*, not *tower images*, scoring any of those images with high probability counts as success.

Subsequently, I re-labeled the image slices such that only images with enough tower for fan blades to be identifiable were specified as having a tower. This is clearly a subjective criterion, but I did all the labeling so it was relatively consistent within the project. Model performance improved significantly once I removed those images.

### Borderline towers

Even with perfect imagery it is not always possible to distinguish air chillers from small cooling towers by eye. I initially labeled these borderline calls as towers to be inclusive, but that caused the model to score all air chillers with high probability. Labeling all borderline images as non-towers notably improved model performance.

### High probability non-tower images

The vast majority of images do not contain towers. For most of these images the model has perfect performance - it will never score an image containing only grass as high probability. Therefore, most of the images contribute very little informating during model training. 

As we added cities to the project I had almost 60 million image slices that could be used for training. From a practical standpoint that is far too many to use, and from a logical standpoint most of them wouldn't actually contribute to model performance. How should I select non-tower images to use during training?

I wanted to maximize the information each image provided to the model, so I reasoned that images it incorrectly scored with high probability would improve along the discriminative margin. Therefore, I selected non-tower images scored with probability >0.99 to always include in model training. I also discovered that each new city contained new patterns that fooled the model, so each city contributes these high probability non-tower images.

I need to include at least some 'normal' images during model training. To choose those, I sample slices from parent images with probability proportional to the proportion of tower slices in the image. For example, images with no towers contribute no slices during model training; images with many towers contribute more slices than images with few towers.

## 3. Computational bottlenecks are common

Todo


## 4. This task has a severe 'rare disease' problem

Todo



# Image sources

Some states, counties, and cities collect orthoimagery every few years. If those images are publicly available, they are typically hosted by a city or state GIS unit such as those described above for NYC, Philadelphia, and Chicago. We located those by searching Google to identify the relevant party. 

Unfortunately, many places have publicly available imagery that is not of sufficient resolution for tower identification. We have found some other potential image sources, described below.

## USGS EarthExplorer

[EarthExplorer](https://earthexplorer.usgs.gov/) is an aggregator of geospatial data from many sources, and it hosts imagery covering the entire United States from assorted times. 

To search for appropriate imagery: 

1. Specify the area of interest in the front 'Search Criteria' tab
2. In the 'Data Sets' tab select Aerial Imagery --> High Resolution Orthoimagery
3. In the 'Additional Criteria' tab and select resolutions of .5 Feet or smaller and .16 Meter or smaller
4. Click the 'Results' button at the bottom

The results are returned by image tile (typically 5000px square). You can select the ones you want, then download them after you register for an account. If you want many images, there is a separate 'Bulk Download' application you can use - information can be found at the [help page](https://lta.cr.usgs.gov/EEHelp/ee_help). I am in a restricted environment, and the necessary port is not open in our firewall for using the bulk download tool.

## NOAA 

Todo

  ------------

Please note that the 'coolit' project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.
