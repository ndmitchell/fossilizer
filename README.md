# Fossilizer

A tool to generate diagrams of 3D fossil bedding surfaces that can be rotated, panned and zoomed. The tool requires two input files:

* A `surface.txt` file containing x/y/z coordinates for the underlying surface. The x/y coordinates are assumed to be on a regular sized grid.
* A `points.txt` file containing x/y/z/name information, where the name is the type of fossil.

These are combined using the `Main.hs` Haskell script to produce an `output` directory which has a file `index.html` which serves as the viewer.

## Technical Details

The tool is a combination of three parts (all of which are available from this repo):

* The Haskell code in this repo, written by Neil Mitchell, which coordinates activities and generates OBJ 3D models from the data.
* The [WebGL Loader](https://code.google.com/p/webgl-loader/) project, which transforms the 3D models into a form suitable for the web.
* The [Open 3D Viewer](https://code.google.com/p/open-3d-viewer/) project, which provides the 3D viewer interface.
