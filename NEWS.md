# Changes in version 1.14.0 (2026-01-20)

+ Fix: cyGroups follows a different naming structure in the JSON export. Updated the export function accordingly. 
  Also the CyGroup ids have to be present in RCX object, a check for those references has been added.

# Changes in version 1.12.1 (2025-09-02)

+ Fix: fromGraphNEL didn't use the provided arguments for conversion

# Changes in version 1.3.1 (2023-01-26)

+ Fix: lost print in getAspectClasses

# Changes in version 1.3.1 (2022-12-23)

+ Update extension load mechanism: getAspectClasses returns also extension classes, setExtension() for registration and usage of options()

# Changes in version 1.0.1 (2022-10-05)

+ Bugfix in createCyVisualProperty: if appliesTo and/or view are set to NAs causes an error while it should be accepted as default.

# Changes in version 0.99.0 (2021-10-26)

+ Submitted to Bioconductor