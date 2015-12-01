# MLB Scores

Retrieves MLB scores for a given date through MLB.com and displays them in a buffer.

## Installation

Clone the repo (or copy `mlb-scores.el`) in your load path. Somewhere in your init file add `(require 'mlb-scores)`.

## Usage

`M-x mlb/get-scores` to get the interactive prompt. The month should be a number with a leading zero (01-12).
`M-x mlb/today` to get the scores for today's games.

## TODO

* Lots of improvements to the code, it's a bit of a mess right now.
* Better handling of errors / no score found
* Clean up the results output (maybe format them with org?)
