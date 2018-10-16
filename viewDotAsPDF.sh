#!/bin/bash
dot -Gsize=7,7 -Tps $1.dot > $1.ps
ps2pdf $1.ps
okular $1.pdf
