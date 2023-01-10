# Stunts Cartography

Power tools for track map rendering and analysis for the classic racing game
[Stunts](http://scr.stunts.hu). Powered by
[Diagrams](http://projects.haskell.org/diagrams/)
(vectorial rendering backend) and
[Threepenny](http://hackage.haskell.org/package/threepenny-gui)
(browser-hosted frontend).

This suite currently is comprised by a fully funcional track viewer and
repldump2carto, an auxiliary tool for generation of lap traces.

## Suite components

### Track Viewer

* Command: stcarto viewer
* Starting it: launch it in a console window and navigate in a web browser to
  http://localhost:8023 .
* Command line options: -d sets the initially selected directory. If you keep
  the program somewhere other than in a subdirectory of your Stunts folder, it
  may be useful to create a link/shortcut with a more convenient initial
  directory set. -p changes the port used by the application from the default
  (8023), which might be necessary in case there is a conflict with some other
  application.
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
* The flipbook box instructs the program to generate, in addition to the track
  map, animation frames created by overlaying annotations over the path of a
  replay. Once the rendering is complete, which typically takes a few minutes,
  the resulting images are packaged into a zip archive and made available
  through the flipbook download link, next to the track and replay ones. The
  syntax for describing flipbooks, as explained in the annotations help page,
  is mostly the same one used for regular annotations. The flipbook
  functionality is meant to be used in conjunction with the auxiliary
  repldump2carto tool, which generates lap traces from raw simulation ouput.

### r2c (repldump2carto)

* Command: stcarto r2c
* A tool which bridges the gap between dstien's repldump, a DOS tool for
  extraction of raw data from the game engine, and the Stunts Cartography
  track viewer. It generates lap traces for consumption of the flipbook
  functionality of the latter from the binary data files produced by the
  former.
* Usage: in the command line, stcarto r2c [FILE]..., where [FILE]...
  stands for one or more files produced by repldump.
* For extra information, see the REPLDUMP.md file (in the repository, it is
  located at the repldump directory).

### t2c (trackdata2carto)

* Command: stcarto t2c
* Just like repldump2carto, but for trackdata binary dumps. For now it exports
  trackdata09 and trackdata10 coordinates in the same format used by
  repldump2carto.
* Usage: in the command line, stcarto t2c (--09 | --10) [FILE]..., with the
  flag picking one of the trackdata structures to be exported.
* For extra information on trackdata and how to obtain a binary dump of it, see
  [the relevant Stunts Wiki article](http://wiki.stunts.hu/index.php?title=Trackdata).

### biggrid

* Command: stcarto biggrid
* A command line track map generator that can render multiple tracks side by
  side on a grid.
* Usage: in the command line, stcarto biggrid INPUT OUTPUT, where INPUT is a
  text file with one TRK file path on each line (blank lines can be used to
  introduce gaps in the grid), and OUTPUT is the destination path (PNG and SVG
  are supported, depending on the stcarto build configuration). There are
  several rendering oprions available; using the `-h` help flag will list
  them.

### gallery

* Command: stcarto gallery
* A command line map generator which renders all tracks in a directory, in
  separate images.
* Usage: in the command line, stcarto gallery \[INPUTDIR\]. Specifying the
  input directory is optional, defaulting to the current directory. The output
  directory can be specified with `-o`, with INPUTDIR/galcarto as default.
  There are several rendering oprions available; using the `-h` help flag will
  list them.

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

### Subdirectories

* data: contains sample Stunts tracks (the .TRK files) and driving paths
  extracted from game data to be used for lap trace annotations (the .dat
  files). The paths are meant to be overlaid on the track which shares part of
  their file name.
* repldump: source tree and additional documentation on repldump2carto and the
  procedures needed to generate a lap trace from a Stunts replay.
* wwwroot: HTML, CSS, images and other files used to compose the interface.
  Includes some in-program documenation, such as the annotation syntax
  description.

## In closing...

Developed by Daniel Mlot, also known as Duplode. You might want to visit
[The Southern Cross Stunts Trophy](http://scr.stunts.hu), my site dedicated to
this most wonderful of racing games.
