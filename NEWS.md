# QuickPed 4.3.0

## New features

* New button for adding proband arrows.

## Bug fixes

* Fix crashes occurring when trying to update an empty pedigree.


# QuickPed 4.2.0

## New features

* New "miscarriage" button for representing miscarriages with triangle symbols.

* The "Clear selection" button was moved to the top of the "Modify" frame, to make room for the "miscarriage" button.

* The "R code" output now displays time in UTC.

## Bug fixes

* Improved error handling when drawing and saving pedigree plots.

* Fix bug affecting twin status in the "R code" output.

* In the IBD triangle plots, allow arrow mode in the inset pedigree.


# QuickPed 4.1.1

## New features

* New "R code" button to generate R code reproducing the current pedigree plot.

* The 'Add parents' button now supports assigning existing pedigree members as parents. When 2 or 3 individuals are selected, the first is treated as the child, followed by its designated parent(s).

## Bug fixes

* Fixed error message when removing individuals disconnects the pedigree.

* Fixed a bug where previous labels of selected individuals persisted in the background, causing havoc.


# QuickPed 4.0.0

## New features
* New icon-based design for pedigree building! Explanations for the icons are displayed as tooltips when you hover over a button.

* New siblings are now always placed next to the selected individual. Previously, the "Add sibling" button produced a new sib at the right-most end of the sibship in question.

* A new button for adding a sibling to the *left* of the selected individual.

* Detailed text annotations can now be added around and inside each pedigree symbol. Double-click an individual to open a popup window for text entry. Emojis are allowed! 😂

* New styling options, including hatched pedigree symbols and a selection of fill colours.

* The method for changing sex has been updated. New buttons, featuring icons for *Male*, *Female*, and *Unknown*, allow you to set the sex of selected individuals directly. These replace the previous *Sex* button, which swapped males with females. As before, spouses of the affected individuals will be automatically updated as needed.

* When loading *ped* files, it is no longer required that founders are included. This update resolves an issue for users working with animal pedigrees, which often omit founders.
 

# QuickPed 3.2.0

* New feature: Inbred version of IBD triangle.

* New built-in pedigrees: Ancestral (4 gen) and Half sib triangle.


# QuickPed 3.1.1

* Better checks of plot setting inputs


# QuickPed 3.1.0

* Saving as pdf now uses `cairo_pdf()`, allowing a wider range of characters.

* Improved relationship triangle plot.

* New (experimental) plot options: Arrows and straight legs.

* For regular plot options, replace sliders with numeric inputs.


# QuickPed 3.0.3

* Update intro text and citation info.

* Make links open in a new tab.


# QuickPed 3.0.2

* Fixed bug occurring when updating labels in pedigrees with 123 (or more) individuals.


# QuickPed 3.0.1

* Thicker red outline of selected individuals.


# QuickPed 3.0.0

## New features

* Added button "Random pedigree", which uses `pedtools::randomPed()` to produce a random pedigree with 5--15 individuals.

* The button for removing pedigree members has been moved further down and split into two buttons (arrows "up" and "down"). This allows the user to choose if the removal should act upwards (removing ancestors) or downwards (removing descendants) of the selected individuals.

* The button for deselecting all has been renamed to "Deselect", instead of "Selection".

* The entire app is faster and more responsive, due to better organisation of the plotting algorithm, thus avoiding unnecessary computation.


# QuickPed 2.11.0

## New features

* Further adjustments of the plotting algorithm, ensuring better layout of some pedigrees, like 3/4 siblings.

* Reduced the default expansion factor ("cex") from 1.6 to 1.4

## Bug fixes

* Some (rare) pedigrees were poorly handled in the previous version, due to a bug in kinship2. This has now been fixed.


# QuickPed 2.10.0

## New features

* The appearance of pedigree symbols are slightly modified, enforcing equal height and width. (E.g., males should be perfect squares.)

* Margins have been adjusted.

* Labels with more than one line break are now possible. Each occurrence of '  ' (double space) is replaced with a line break. (Previously only two lines was possible).


# QuickPed 2.9.1

## New features

* Tweaked the descriptions of multiple asymmetric relationships, e.g., double avuncular.

* Add link to BMC Bioinformatics paper.


# QuickPed 2.9.0

## New features

* Added relationship "Double 2nd cousins A" and "Double 2nd cousins B".

## Other

* In tables of relatedness coefficients, columns with X-chromosomal coefficients are now suffixed with ".X" (rather than prefixed with "X-"). 

* Tweaked appearance of drop-down menu.


# QuickPed 2.8.0

## New features

* The relatedness coefficient table is extended to include detailed identity coefficients, as well as X-chromosomal counterparts of all available coefficients.


# QuickPed 2.7.2

## New features

* It is now possible to hide all pedigree labels, by clicking "Hide all" in the Labels frame.


# QuickPed 2.7.1

## New features

* Add a label (with the individuals' names) in the relatedness triangle plot.

## Other changes

* Tweak appearance of the relatedness triangle plot.


# QuickPed 2.7.0

## New features

* Triangle plot for visualising kappa coefficients: When two noninbred individuals are selected, pressing the chart button (in the Relationships frame) produces a relatedness triangle marking the location of their kappas. (Based on `ribd::showInTriangle()`.)

* Icons have replaced button texts in the Relationship frame. Help tips are visible when mouse hovering over each button.


# QuickPed 2.6.0

## New features

* The calculated pairwise coefficients now include the *relationship degree*, as used by some software.

* New button and popup menu for downloading a table of relatedness coefficients. This simplifies obtaining coefficients for multiple pedigree members. By default (if no members are selected), all pairs are included.

* Restructured the text showing coefficients for a single pair of individuals.


# QuickPed 2.5.0

## New features

* The function loading .ped files is more liberal and better at guessing the input format. In particular, it now handles files with/without headers, with/without family ID in first column, and with/without affection status column.

* Two new buttons, "PNG" and "PDF", for saving the pedigree plot. These replace the previous "Save plot" button (which only gave png).

## Other changes

* The label fields are slightly smaller (reducing their height from 28px to 24px), to ease the handling of bigger pedigrees.

# QuickPed 2.4.2

## Bug fix

* Fixed a bug in **verbalisr**, which in certain (rare) cases caused some paths to be skipped.

## Other changes

* Tweak title and introduction
* Correct typos

# QuickPed 2.4.1

## New features

* Added two historic pedigrees: Queen Victoria (haemophilia) and the Jicaque pedigree.

* More information about the built-in pedigrees are added in the user manual at https://magnusdv.github.io/pedsuite/articles/web_only/quickped.html

# QuickPed 2.4.0

## New features

* New builtin historic pedigree: The family tree of Tutankhamun.

* Experimental feature: To insert a line break in a labels, use a double space.

## Bug fixes

* Catch (more gracefully than before) discordant MZ twins when changing sex.

## Other changes

* The plotting code has been refactored, but this should not have visible effect.


# QuickPed 2.3.0

## New features

* New button "Sibling" which adds a brother to any pedigree member. If the selected member is a founder, parents are also created.

* The list of builtin pedigrees has been revised and better structured. 

* The royal (and famously inbred) Habsburg family has been added as a builtin pedigree.

* Ped files are now loaded with `encoding = "UTF-8"`, enabling proper parsing of special characters.


# QuickPed 2.2.1

## New features

* The relationship descriptions are better and more comprehensive, using the new version of **verbalisr** (v0.2.0).


# QuickPed 2.2.0

## New features

* You can now specify unknown sex (sex = 0) by double-clicking on a pedigree member. Note: Only non-parent individuals are allowed to have unknown sex. Double-clicking on an individual with unknown sex resets to male sex.


# QuickPed 2.1.1

## Bug fixes

* Avoid crash when trying to add children to an individual with unknown sex.


# QuickPed 2.1.0

## New features

* A new button for automatic, generation-aware labels (I-1, I-2, ...).


# QuickPed 2.0.2

## Minor fixes

* Update links.

* Tweak description text.


# QuickPed 2.0.1

## Bug fixes

* `Reset all` now also resets the plot settings.

* Impossible twins are now caught and reported more sensibly.

* Loading ped files now includes affection status, if there is a column named `aff`.

* Fixes various minor glitches.


# QuickPed 2.0.0

## New features

* A `Quick start` panel allows the user to choose from a list of built-in pedigrees.

* Alternatively, it is now possible to upload an existing `ped` file.

* A new panel `Relationships` with buttons for computing relatedness coefficients and generating verbal descriptions of relationships.

* The layout has been substantially modified to accommodate the new features.


# QuickPed 1.1.0

## New features

* The `Build pedigree` panel has been re-designed to fit more buttons.

* New buttons: `Carriers` and `Deceased` for annotating pedigree.

* New buttons: `MZ` and `DZ` for creating twins.


# QuickPed 1.0.1

## Minor changes

* `Toggle sex` and `Toggle aff` now keeps the selection if only one person is selected.

* Added a paragraph `More information`, with link to GitHub.

* Added version number and link to this changelog.

## Bug fixes

* The `Undo` button did not always update the selected individuals, which sometimes caused the app to crash. This has been fixed.


# QuickPed 1.0.0

* Initial release.
