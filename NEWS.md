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
