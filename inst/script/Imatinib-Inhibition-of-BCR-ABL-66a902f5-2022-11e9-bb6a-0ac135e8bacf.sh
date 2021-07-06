#!/bin/bash

## Download an example network from NDEx platform

# Imatinib Inhibition of BCR-ABL
# Network Size:
#   Nodes: 75
#   Edges: 159
# Network information
# Owner	ndexbutler
# Created	1/24/2019 10:52:53 PM
# Last modified	2/1/2019 10:55:04 PM
# UUID	66a902f5-2022-11e9-bb6a-0ac135e8bacf
# Visibility	Public (searchable)
# Description
# Imatinib is a tyrosine kinase inhibitor used to treat cancers such as chronic myelogenous leukemia (CML), a cancer characterized by increased and unregulated growth of white blood cells in the bone marrow and the accumulation of these cells in the blood. The cause of CML pathophysiology is the BCR-ABL fusion protein - the result of a genetic abnormality known as the Philadelphia chromosome in which Abelson Murine Leukemia viral oncogene homolog 1 (ABL1) translocates within the Breakpoint Cluster Region (BCR) gene on chromosome 22. BCR-ABL is a cytoplasm-targeted constitutively active tyrosine kinase that activates several oncogenic pathways which promote increased cell proliferation and survival including the MAPK/ERK Pathway, the JAK-STAT Pathway, and the PI3K/Akt pathway. Imatinib inhibits BCR-ABL activity by binding a highly conserved ATP binding site to effectively lock the tyrosine kinase in an inactive conformation. As a result, phosphate is unable to be transferred from ATP to activate oncogenic signalling cascades. For greater detail, refer to the pathway titled BCR-ABL Action in CML Pathogenesis. Imatinib resistance in the form of BCR-ABL mutations (e.g. T315I) is an ongoing challenge. Next generation inhibitors have been developed to combat this resistance, but further research is necessary.
# Reference
# Jewison T, Su Y, Disfany FM, et al. SMPDB 2.0: Big Improvements to the Small Molecule Pathway Database Nucleic Acids Res. 2014 Jan;42(Database issue):D478-84.
# Properties
# Smpdb	SMP0031694
# SbmlVersion	L3 V1
# Sbml id	Pathway32592
# MetaId	_409074a2-4c7f-4911-a795-e5a6fda27336
# SbmlNetwork	sbml
# Label	Pathway32592
# NetworkType	Drug Action

## URL: http://public.ndexbio.org/viewer/networks/66a902f5-2022-11e9-bb6a-0ac135e8bacf

wget \
  -O ../extdata/Imatinib-Inhibition-of-BCR-ABL-66a902f5-2022-11e9-bb6a-0ac135e8bacf.cx \
  http://www.ndexbio.org/v2/network/66a902f5-2022-11e9-bb6a-0ac135e8bacf?download=true

