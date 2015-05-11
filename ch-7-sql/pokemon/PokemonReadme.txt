# readme that gives a brief overview 
  over the tables for the pokemon database

# pokemon

- table for individual pokemons
- with columns: 
    id 
    species_id 
    height
    weight

    
# pokemon_species

- table with additional information for individual pokemon
- with: 
    id          (same as id in pokemon)
    identifier  (name of the pokemon)
    evolves_from_species_id
    evolution_chain_id

    
# pokemon_stats

- table with statistics for individual pokemons
- with columns: 
    pokemon_id
    stat_id
    base_stat   (value for statistics)
 
 
# stats

- table with additional information for pokemon_stats
- with columns:
    id                  (same as stat_id in pokemon_stats)
    damage_class_id     (class of attribute for damage calculations)
    identifier          (name of the statistics)
    

# pokemon_types

- table with information of the type of individual pokemons
with columns: 
    pokemon_id
    type_id     
    slot        (1st, 2nd, 3rd, ... type for pokemon)

    
# types

- table with additional information on pokemon_types
- with columns:
    id
    identifier          (name of the type)
    damage_class_id     (class of type for damage calculations)

    
# type_efficacy

- table with information on damage factor for specific combinations of
  damage type and target type 
- with columns:
    damage_type_id  (type of damage dealt)
    target_type_id  (type of target )
    damage_factor   (factor for damage-target combination)
    
