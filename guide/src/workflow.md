# Workflow for mod installation

## Rough workflow

1. `simod init` (run this only once): creates the database and fills it
   using data from the Infinity Engine files (key/bif and existing
   override files).
2. `simod add *target*` (run this one for each mod component): installs a
   mod component by running the mod script to modify the database.
3. `simod save` (run this once the database has been modified): compiles
   the changes from the database back to the game directory.

See `simod --help` for (slightly) more details.

## Current status

> This tool is at the “proof-of-concept” stage.
>
> Since a lot of the infrastructure is still being built,
> its capabilities are very restricted: it can only access and edit game
> items.
