# Revision history for input-parsers

## 0.2.2 -- 2021-03-26

* Move Data.ByteString and Data.ByteString.Lazy imports outside ifdef (by Gary Coady)
* Exported all Position methods

## 0.2.1 -- 2021-03-09

* Changed the default instance of `ParserPosition`, made `Position` a subclass of `Ord`.

## 0.2 -- 2021-03-07

* Added `ParserPosition` and made `Position` a class. Deprecated.

## 0.1.0.1 -- 2020-07-19

* Incremented the upper bound of `base` dependency.

## 0.1.0.0 -- 2020-07-18

* First version. Released on an unsuspecting world.
