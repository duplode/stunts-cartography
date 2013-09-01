# Stunts Cartography

Power tools for track map rendering and analysis for the classic racing game
[Stunts](http://scr.stunts.hu). Powered by
[Diagrams](http://projects.haskell.org/diagrams/)
(vectorial rendering backend) and
[Threepenny](http://hackage.haskell.org/package/threepenny-gui)
(browser-hosted frontend).

This suite currently just one executable ready to run out of the box, the track
viewer. Nonetheless, despite being somewhat unpolished, the viewer is fully
functional, and supports a number of advanced features.

## Suite components

### Track Viewer

* Executable: sc-trk-viewer
* Starting it: launch it in a console window and navigate in a web browser to
  http://localhost:10000 .
* To generate a track map, fill both path fields as appropriate (the base path
  is prepended to the TRK or RPL file path, so that relative paths can be used
  during a session) and optionally change the rendering parameters. Then, click
  one of the buttons at the top to generate PNG or SVG output.
* Picking an inexistent file or a TRK with size different from 1802 bytes (the
  expected .TRK file size) will suspend the rendering. Invalid parameter
  values, or values out of the ranges presented at the interface, will be
  replaced by the respective default values.
* Copies of both the track file and a bare terrain TRK are created as temporary
  files, and can be saved elsewhere through the links on the side bar. That
  allows for extracting tracks from replays. For that purpose, both replay
  formats of Stunts are accepted.
* To save the track map, right click the image and choose "Save image as...",
  as it would be done in any regular web page.
* The interface background colour changes according to the horizon of the track
  being displayed.
* The annotations box allows users to add some kinds of annotations to the maps
  by specifying them with a simple syntax, described in the help page linked
  from the program interface, without needing to use an image editor. It is
  specially convenient for preparing race analysis maps.

## Repository content highlights

### Modules

* Track.hs: track data processing functions and an extensive data type ontology
  of tiles and track elements (extensive but not exhaustive, as terrain and
  element connectivities are not covered yet).
* Pics.hs: the Diagrams backend for rendering the track tiles. Drawing
  proportions ara parameterized via Reader monad.
* Composition.hs: assembly of tiles to compose the full map and rendering of
  extra diagrams such as grid lines.
* Output.hs: carries out the IO rendering for the track viewer, bridging
  backend and frontend.
* Viewer.hs: the Threepenny frontend of the track viewer. Communicates with the
  Diagrams code through the Output module.
* Animate.hs: a demonstration of how the Diagrams backend can be used to
  generate an animation of a car following a path on the track. Usage
  suggestions are given by the generate-movie.sh script and the video-notes.txt
  remarks.

### Subdirectories

* data: contains sample Stunts tracks (the .TRK files) and driving paths
  extracted from game data to be consumed by the Animate module demo (the .dat
  files). The paths are meant to be overlaid on the track which shares part of
  their file name.
* laptrace: instructions for an arcane procedure driving path extraction from
  an instance of Stunts running under DOSBox. Recent developments provide an
  alternative, less complicated, way of getting hold of the paths; it will
  eventually be included in the suite.
* wwwroot: HTML, CSS, images and other files used to compose the interface.
  Includes some in-program documenation, such as the annotation syntax
  description.

## In closing...

Developed by Daniel Mlot, also known as Duplode. You might want to visit
[The Southern Cross Stunts Trophy](http://scr.stunts.hu), my site dedicated to
this most wonderful of racing games.
