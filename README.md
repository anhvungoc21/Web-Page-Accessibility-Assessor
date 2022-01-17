# Webpage Accessibility Assessor

A program which assesses some basic accessibility features of a webpage

### Description

The program fetches a web page and generate information on its accessibility shortcomings:
* How many images lack alt text?
* How many links are embedded in non-discriptive text?
* How many pairs of links are insufficiently separated?

Then, a message is returned which informs the user about those information.

### Dependencies

* Program should be run on platforms which support the Racket language (e.g. DrRacket) and the libraries mentioned at the top of the .rkt file must be installed

### Executing program

```
(assess-accessbility *url*)
