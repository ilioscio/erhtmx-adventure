# ERHTMX Adventure

A classic RPG adventure game built with Erlang, HTMX, and HTML5 Canvas.

## Overview

ERHTMX Adventure is a Link's Awakening-inspired RPG where you explore three areas, fight enemies, collect keys, and ultimately defeat the dragon boss in the dungeon.

## Features

- **5 Character Classes**: Wizard, Archer, Swordsman, Rogue (Dagger), and Lancer (Spear)
- **3 Areas**: Training Grounds, Castle, and Dungeon (16 maps each in a 4x4 grid)
- **Combat System**: Each weapon type has unique attack patterns
- **Enemy Types**: Slimes, Bats, Skeletons, Ghosts, Demons, Spiders, and the Dragon boss
- **Progression**: Collect keys to unlock doors, find items in chests
- **Persistent State**: Game progress saved in browser cookies

## Requirements

- NixOS or Nix package manager
- Or: Erlang/OTP 26+ and Rebar3

## Quick Start

### With Nix

```bash
# Enter development shell
nix develop

# Run the server
rebar3 shell
```

### Without Nix

```bash
# Compile
rebar3 compile

# Run
rebar3 shell
```

Then open http://localhost:8080 in your browser.

## Controls

- **Arrow Keys / WASD**: Move
- **Space / Z**: Attack
- **E / Enter**: Open chests (after defeating all enemies)
- **Click inventory slots**: Equip weapons or use potions

## Weapons

| Weapon | Pattern | Damage | Speed |
|--------|---------|--------|-------|
| Sword | 90° arc in front | High | Medium |
| Dagger | Poke forward | High | Fast |
| Spear | Poke forward (longer) | Medium | Medium |
| Bow | Fast projectile | Low | Medium |
| Fire Staff | Fireball projectile | High | Slow |
| Lightning Staff | Cone attack | Low (multi-hit) | Slow |
| Ice Staff | Circle around player | Medium | Very Slow |

## Game Structure

```
Training Grounds (4x4) → Castle (4x4) → Dungeon (4x4)
                    ↓               ↓
              Castle Key      Dragon Key
                    ↓               ↓
              Locked Door     Dragon's Lair
```

### Goal

1. Explore the Training Grounds
2. Find the Castle Key and enter the Castle
3. Explore the Castle and find the Dragon Key
4. Enter the Dungeon and reach the Dragon's Lair (3,3)
5. Defeat the Dragon!

## Architecture

- **Erlang/Cowboy**: HTTP server, stateless design
- **HTMX**: Partial page updates (UI elements)
- **HTML5 Canvas**: Real-time game rendering
- **JavaScript**: Game engine (movement, combat, AI)
- **Cookies**: Client-side state persistence

## Project Structure

```
erhtmx-adventure/
├── flake.nix              # Nix development environment
├── rebar.config           # Erlang dependencies
├── src/
│   ├── erhtmx_adventure_app.erl  # Application entry point
│   ├── erhtmx_adventure_sup.erl  # Supervisor
│   ├── game_state.erl            # Player state management
│   ├── map_data.erl              # Map generation
│   ├── game_handler.erl          # Main game page handler
│   ├── create_handler.erl        # Character creation handler
│   ├── game_api_handler.erl      # Game state API
│   └── map_api_handler.erl       # Map data API
└── priv/static/
    ├── style.css          # Game styling
    └── game.js            # Canvas game engine
```

## License

MIT
