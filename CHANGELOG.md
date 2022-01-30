## 1.0.0.0 (2022-01-30)

* biggrid CLI subcommand, for rendering multiple tracks in a single map.

* r2c CLI subcommand, which corresponds to the formerly separate
  replaydump2carto executable.

* t2c CLI subcommand, analogous to r2c but for trackdata dumps,
  currently supporting trackdata09 (F4 camera positions) and trackdata10
  (corner sign positions).

* Various improvements to the viewer GUI (which is now launched through 
  the viewer subcommand).

* Show track name and scenery in the viewer GUI log. 

* Tracks with Bliss metadata are now accepted by Cartography (they were
  formerly rejected by the file size checks).

* New types of annotation: markers and standalone captions.

* Frame-bound standalone captions for adding timers and other changeable
  information to flipbooks.

* Annotation captions in flipbooks can now show car speed, current gear
  and height, in addition to time.

* Fixes to the behaviour of the caption alignment options

* Alternative two-tone terrain style, inspired by dreadnaut's 4DOPEN
  terrain editor.

* Option for a transparent low ground background.

* Various visual fixes to track diagrams.

* Support for generating SVG flipbooks.

* Support for building Cartography with the SVG and Rasterific backends
  of Diagrams, making the Cairo/GTK dependency optional.

* Updates for building with GHC 9.0.2 and recent versions of
  dependencies. 

## 0.4.0.2 (2014-05-19)

* Last release before 1.0.
