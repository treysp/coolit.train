# coolit.train

coolit.train is an R package for training a model to identify water cooling towers from high-resolution aerial imagery. It consists of functions for:

- Slicing large images into smaller pieces the model can use
- Training a convolutional neural network model on slices known to either contain a cooling tower or not
- Scoring slices with a trained model

This repo contains both functions (as a normal R package does) and code I used to apply the functions in the 'analysis' directory. It is an R package that can be installed from Github using `remotes::install_github()`. However, the installed package will not contain the 'analysis' directory. If you want that code, you should pull the entire repo then build the package locally using `remotes::install_git()`.

  ------------

Please note that the 'coolit' project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.
