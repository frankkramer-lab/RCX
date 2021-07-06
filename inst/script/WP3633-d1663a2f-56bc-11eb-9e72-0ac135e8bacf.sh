#!/bin/bash

## Download an example network from NDEx platform

# WP3633 - Caffeine and Theobromine metabolism - Homo sapiens
# Network Size:
#   Nodes: 27
#   Edges: 21
# Network information
# Owner	wikipathways
# Created	1/15/2021 12:04:13 AM
# Last modified	1/15/2021 12:04:13 AM
# UUID	d1663a2f-56bc-11eb-9e72-0ac135e8bacf
# Visibility	Public (searchable)
# Description
# Metabolism pathway of two compounds commonly found in human samples: caffeine and theobromine.
# Rights
# Rights	Waiver-No Rights Reserved (CC0)
# Rights holder	WikiPathways
# Properties
# WikipathwaysVersion	"106746"
# Organism	Homo sapiens
# Author	WikiPathways team
# Labels	WP3633, xenobiotic metabolic pathway, caffeine metabolic pathway
# WikipathwaysID	WP3633
# WikipathwaysIRI	http://identifiers.org/wikipathways/WP3633_r106746
# NetworkType	pathway

## URL: http://public.ndexbio.org/viewer/networks/d1663a2f-56bc-11eb-9e72-0ac135e8bacf

wget \
  -O ../extdata/WP3633-d1663a2f-56bc-11eb-9e72-0ac135e8bacf.cx \
  http://www.ndexbio.org/v2/network/d1663a2f-56bc-11eb-9e72-0ac135e8bacf?download=true

