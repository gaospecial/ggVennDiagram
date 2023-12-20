#!/usr/bin/env Rscript
# Copyright 2023-2023 Chun-Hui Gao <gaospecial@gmail.com>
#
#  This file is free software: you may copy, redistribute and/or modify it
#  under the terms of the GNU General Public License as published by the
#  Free Software Foundation, either version 2 of the License, or (at your
#  option) any later version.
#
#  This file is distributed in the hope that it will be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.

# This script is a standalone R wrapper that can be used to plot Venn and Upset plot
# using ggVennDiagram. It is for command line interface and can be used to build a
# TBtools plugin.

##### STEPS #####

#### install R - firstly install RServer plugin ####


#### install package ####
if (!requireNamespace("pak", quietly = TRUE)){
  install.packages("pak")
}

if (!requireNamespace("ggVennDiagram", quietly = TRUE)){
  pak::pak("ggVennDiagram")
}

if (!requireNamespace("argparse", quietly = TRUE)){
  pak::pak("argparse")
}

library("ggVennDiagram")
library("argparse")

#### process arguments ####
## see https://docs.python.org/3/library/argparse.html#the-add-argument-method
parser <- ArgumentParser(description='This is a ggVennDiagram wrapper,
                         by which you can instantly create publication quality
                         Venn diagram (no more than 7 sets) and Upset plots
                         (unlimited No. of sets). For example: PROGRAM -s a,b,c b,c,d e,f')
parser$add_argument('--set', '-s', nargs = '+',
                    help='set members. must be comma separated string. at least one set is needed.')
parser$add_argument('--type', choices = c("auto", "Venn", "Upset"),
                    default = "auto",
                    help = 'specifiy the type of plot')
parser$add_argument("--out", "-o",
                    default = "plot.png",
                    help = "specify the output file of plot. filetype (png/jpg/pdf) is determined by its suffix automatically.")
parser$print_help()

args = parser$parse_args()

list = as.list(args$set)

#### output results ####
## plot a png/jpg/tiff, and can export as both PDF and PPTX
p = ggVennDiagram(list)
ggplot2::ggsave(args$out, p)
