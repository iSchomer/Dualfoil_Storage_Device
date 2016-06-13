# Notes on DualFoil code

* Variable storage
  + most functional data is stored in the `xx` 2D array
  + information found in profiles.out is not stored in `xx`
* Basic Organization
  + main `comp` subroutine updates `xx` using another array: `c`
  + `cellpot` calculates and writes the data found in dualfoil5.out, but the information is not stored in `xx`
  + `nucamb` calculates and writes the data for profiles.out (not stored in `xx`)
* Discrepencies
  + `comp` is called 6 times during initialization of the functional data

## Table of known variable types within the `xx` array

| `xx(i, *)` | Description                                       |
|-----------:|---------------------------------------------------|
|          1 | Concentration of electrolyte                      |
|          2 | Solution Potential                                |
|          3 | `ct1*csx` and `ct3*csy` for respective electrodes |
|          4 | `ct1*csx` and `ct3*csy` for respective electrodes |
|          5 | Current Density                                   |
|          6 | Transfer current (flux density)                   |
|          7 | Matrix potential                                  |
|          8 | Side reaction 1 matl balance                      |
|          9 | Side reaction 1 kinetics                          |
|         10 | Side reaction 2 kinetics                          |
|         11 | Side reaction 2 matl balance                      |
|         12 | Side reaction 3 kinetics                          |
|         13 | Unknown                                           |
|         14 | Unknown                                           |
|         15 | Unknown                                           |
|         16 | Unknown                                           |
|         17 | Unknown                                           |

_Note_: 3 and 4 are most likely current and its derivative
