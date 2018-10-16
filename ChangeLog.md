# Changelog for formura
## version 2.3

- Run also without MPI
- Implement LoadIndex
- Fix `to_pos` functions
- `Formura_Init` function initializes MPI and global data
- Add `Formura_Finalize` function
- Add `Formura_Custom_Init` function
- Add `space_interval_x` field to `Formura_Navi` struct
- Add `total_grid_x` field to `Formura_Navi` struct
- Support `first_step`
- Support `filter`

## version 2.2

- Support OpenMP

## version 2.1

- Add no blocking mode
- Add the name of the global data structure

## version 2.0

- Change the temporal blocking form
- Change the config format in yaml
- Fix bug on MPI
- Fix bug on temporal blocking
