#!/bin/bash

find ${1} -name "*.py" | xargs -I {} flake8 {}


