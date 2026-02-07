/**
 * ERHTMX Adventure - Game Engine
 *
 * This is the main JavaScript game engine that handles:
 * - Canvas rendering (map tiles, player, enemies)
 * - Player input and movement
 * - Combat system with different weapon types
 * - Enemy AI (chaotic movement like Link's Awakening)
 * - Collision detection
 * - Door/map transitions
 * - State persistence via API calls
 */

// ============================================================
// GAME CONSTANTS
// ============================================================

const TILE_SIZE = 40;
const MAP_WIDTH = 16;
const MAP_HEIGHT = 12;
const CANVAS_WIDTH = 640;
const CANVAS_HEIGHT = 480;

// Tile types (matching Erlang map_data.erl)
const TILE_FLOOR = 0;
const TILE_WALL = 1;
const TILE_WATER = 2;
const TILE_DOOR = 3;
const TILE_LOCKED_DOOR = 4;
const TILE_CHEST = 5;
const TILE_STAIRS_UP = 6;
const TILE_STAIRS_DOWN = 7;

// Player size and speed
const PLAYER_SIZE = 32;
const PLAYER_SPEED = 4;

// Enemy constants
const ENEMY_SIZE = 28;
const ENEMY_SPEED = 2;

// ITERATION 4: VERY DISTINCT color palettes - impossible to miss the difference
// Each area now has DRAMATICALLY different colors that are immediately obvious
const AREA_PALETTES = {
    training_grounds: {
        // BRIGHT GREEN nature theme - like a forest clearing
        floor: '#1a5520',       // Vibrant grass green floor
        wall: '#3a6830',        // Leafy green walls
        water: '#104838',       // Deep forest pond
        door: '#5a4020',        // Wooden door
        lockedDoor: '#3a3020',  // Dark wood
        chest: '#908040',       // Brass/gold
        chestOpen: '#6a5030',   // Open wood
        stairsUp: '#2a6830',    // Bright moss
        stairsDown: '#185018',  // Dark forest
        background: '#0a2008'   // Very dark green background
    },
    castle: {
        // BRIGHT BLUE royal theme - like a stone castle
        floor: '#202850',       // Deep blue stone floor
        wall: '#3a4a78',        // Blue stone walls
        water: '#1a3068',       // Royal blue moat
        door: '#4050a0',        // Blue door
        lockedDoor: '#303060',  // Iron blue door
        chest: '#5070c0',       // Silver-blue chest
        chestOpen: '#405090',   // Open blue stone
        stairsUp: '#3a5090',    // Blue-gray up
        stairsDown: '#202848',  // Dark blue down
        background: '#080818'   // Very dark blue background
    },
    dungeon: {
        // BRIGHT RED danger theme - like a volcanic dungeon
        floor: '#501010',       // Blood red floor
        wall: '#702020',        // Crimson walls
        water: '#401028',       // Purple lava
        door: '#602018',        // Rusted iron door
        lockedDoor: '#401010',  // Dark rusted iron
        chest: '#806020',       // Corroded brass
        chestOpen: '#604018',   // Opened dark brass
        stairsUp: '#504020',    // Escape (tan)
        stairsDown: '#681010',  // Deeper red danger
        background: '#180404'   // Very dark red background
    }
};

// Default colors - initialized to training_grounds and will be overridden by updateAreaPalette()
// ITERATION 4: Match default to training_grounds so colors work even before init
let COLORS = {
    floor: '#1a5520',       // Training grounds green
    wall: '#3a6830',
    water: '#104838',
    door: '#5a4020',
    lockedDoor: '#3a3020',
    chest: '#8a6a2a',
    chestOpen: '#5a4a2a',
    stairsUp: '#3a5a3a',
    stairsDown: '#5a3a3a',
    player: '#00d9ff',
    playerOutline: '#0088aa',
    background: '#0a0a15',
    // Enemy colors by type
    slime: '#44dd44',
    skeleton: '#dddddd',
    bat: '#8844dd',
    demon: '#dd4444',
    ghost: '#aaaadd',
    spider: '#664422',
    dragon: '#ff4400'
};

/**
 * Update color palette based on current area.
 */
function updateAreaPalette() {
    // Get the area name, ensuring it's a valid key
    const areaKey = gameState.area || 'training_grounds';

    // Debug: log the area being used
    console.log('Updating area palette for:', areaKey);

    // Get palette for area (fallback to training_grounds)
    const palette = AREA_PALETTES[areaKey];
    if (!palette) {
        console.warn('No palette found for area:', areaKey, '- using training_grounds');
    }
    const selectedPalette = palette || AREA_PALETTES.training_grounds;

    // Completely replace COLORS with the new palette while preserving non-palette colors
    // Store non-palette colors that should persist
    const preservedColors = {
        player: COLORS.player,
        playerOutline: COLORS.playerOutline,
        slime: COLORS.slime,
        skeleton: COLORS.skeleton,
        bat: COLORS.bat,
        demon: COLORS.demon,
        ghost: COLORS.ghost,
        spider: COLORS.spider,
        dragon: COLORS.dragon
    };

    // Apply the area palette
    COLORS.floor = selectedPalette.floor;
    COLORS.wall = selectedPalette.wall;
    COLORS.water = selectedPalette.water;
    COLORS.door = selectedPalette.door;
    COLORS.lockedDoor = selectedPalette.lockedDoor;
    COLORS.chest = selectedPalette.chest;
    COLORS.chestOpen = selectedPalette.chestOpen;
    COLORS.stairsUp = selectedPalette.stairsUp;
    COLORS.stairsDown = selectedPalette.stairsDown;
    COLORS.background = selectedPalette.background;

    // Restore preserved colors
    Object.assign(COLORS, preservedColors);

    // Update game container class for CSS theming
    const container = document.getElementById('game-container');
    if (container) {
        // Remove all area classes
        container.classList.remove('area-training_grounds', 'area-castle', 'area-dungeon');
        // Add current area class
        container.classList.add('area-' + areaKey);
        console.log('Container classes:', container.className);
    }
}

// ============================================================
// GAME STATE
// ============================================================

let canvas, ctx;
let gameState = {};
let player = {};
let enemies = [];
let projectiles = [];
let attacks = [];
let mapTiles = [];
let doors = [];
let chests = [];
let keysHeld = new Set();
let lastTime = 0;
let gameRunning = true;
let transitioning = false; // Flag to prevent updates during map transitions
let gamePaused = false; // Flag for pause menu
let stairsTransitionCooldown = 0; // Cooldown to prevent instant re-trigger on stairs

// Weapon cooldowns (milliseconds)
const WEAPON_COOLDOWNS = {
    sword: 400,
    dagger: 200,
    spear: 350,
    bow: 500,
    staff_fire: 600,
    staff_lightning: 800,
    staff_ice: 1000
};

// Weapon damage
const WEAPON_DAMAGE = {
    sword: 25,
    dagger: 20,
    spear: 18,
    bow: 15,
    staff_fire: 22,
    staff_lightning: 8, // per hit, cone hits multiple times
    staff_ice: 15
};

// ============================================================
// INITIALIZATION
// ============================================================

/**
 * Initialize the game when the page loads.
 */
window.addEventListener('load', () => {
    canvas = document.getElementById('game-canvas');
    ctx = canvas.getContext('2d');

    // Load game state from server-provided data
    loadGameState();

    // Set up input handlers
    setupInput();

    // Start game loop
    requestAnimationFrame(gameLoop);
});

/**
 * Load initial game state from GAME_STATE global (set by server).
 */
function loadGameState() {
    const gs = window.GAME_STATE;

    gameState = {
        playerName: gs.playerName,
        playerClass: gs.playerClass,
        hp: gs.hp,
        maxHp: gs.maxHp,
        area: gs.area,
        mapX: gs.mapX,
        mapY: gs.mapY,
        inventory: gs.inventory || [],
        keys: gs.keys || []
    };

    // Update color palette for current area
    updateAreaPalette();

    // Initialize player
    player = {
        x: gs.tileX * TILE_SIZE + TILE_SIZE / 2,
        y: gs.tileY * TILE_SIZE + TILE_SIZE / 2,
        direction: 'down',
        weapon: gs.inventory[0] || 'sword',
        weaponCooldown: 0,
        invincible: 0, // Invincibility frames after being hit
        knockback: { x: 0, y: 0, time: 0 }
    };

    // Load map data
    mapTiles = gs.mapTiles || [];
    doors = gs.doors || [];
    chests = gs.chests || [];

    // Initialize enemies (filter out killed ones and those in walls)
    const killedIds = new Set(gs.killedEnemies || []);
    const openedChestIds = new Set(gs.openedChests || []);

    enemies = (gs.enemies || [])
        .filter(e => {
            // Filter out killed enemies
            if (killedIds.has(e.id)) return false;
            // Filter out enemies in walls
            const tile = mapTiles[e.y]?.[e.x];
            if (tile === TILE_WALL || tile === TILE_WATER) return false;
            return true;
        })
        .map(e => ({
            id: e.id,
            type: e.type,
            x: e.x * TILE_SIZE + TILE_SIZE / 2,
            y: e.y * TILE_SIZE + TILE_SIZE / 2,
            hp: e.hp,
            maxHp: e.hp,
            direction: Math.random() * Math.PI * 2,
            moveTimer: 0,
            changeDir: 0
        }));

    // Mark opened chests
    chests.forEach(chest => {
        if (openedChestIds.has(chest.id)) {
            chest.opened = true;
        }
    });
}

// ============================================================
// INPUT HANDLING
// ============================================================

/**
 * Set up keyboard input handlers.
 */
function setupInput() {
    window.addEventListener('keydown', (e) => {
        keysHeld.add(e.key.toLowerCase());

        // Toggle pause menu on Escape or P key
        if (e.key === 'Escape' || e.key.toLowerCase() === 'p') {
            togglePauseMenu();
            e.preventDefault();
            return;
        }

        // Don't process other inputs when paused
        if (gamePaused) {
            return;
        }

        // Attack on space or Z
        if (e.key === ' ' || e.key.toLowerCase() === 'z') {
            attack();
            e.preventDefault();
        }

        // Number keys 1-8 to select/use inventory items
        if (e.key >= '1' && e.key <= '8') {
            const slotIndex = parseInt(e.key) - 1;
            selectInventorySlot(slotIndex);
            e.preventDefault();
        }

        // Prevent arrow key scrolling
        if (['ArrowUp', 'ArrowDown', 'ArrowLeft', 'ArrowRight'].includes(e.key)) {
            e.preventDefault();
        }
    });

    window.addEventListener('keyup', (e) => {
        keysHeld.delete(e.key.toLowerCase());
    });
}

/**
 * Select or use an inventory slot by index (0-7).
 */
function selectInventorySlot(index) {
    const item = gameState.inventory[index];
    if (!item) {
        showMessage(`Slot ${index + 1} is empty`);
        return;
    }

    if (isWeapon(item)) {
        // Equip the weapon
        player.weapon = item;

        // Update selection visual
        const slots = document.querySelectorAll('.inventory-slot');
        slots.forEach((s, i) => {
            if (i === index) {
                s.classList.add('selected');
            } else {
                s.classList.remove('selected');
            }
        });

        showMessage(`Equipped ${formatItemName(item)}`);
    } else if (item === 'health_potion') {
        // Use health potion
        gameState.hp = Math.min(gameState.maxHp, gameState.hp + 30);
        gameState.inventory.splice(index, 1);
        updateHpBar();
        updateInventoryDisplay();
        showMessage('Used Health Potion (+30 HP)');

        // Save to server - use_potion action heals and removes from inventory
        fetch('/api/game', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                action: 'use_potion'
            })
        });
    }
}

/**
 * Toggle the pause menu.
 */
function togglePauseMenu() {
    if (!gameRunning) return; // Don't pause if game is over

    gamePaused = !gamePaused;
    const pauseMenu = document.getElementById('pause-menu');

    if (gamePaused) {
        pauseMenu.classList.add('show');
    } else {
        pauseMenu.classList.remove('show');
    }
}

/**
 * Resume the game from pause menu.
 */
function resumeGame() {
    gamePaused = false;
    document.getElementById('pause-menu').classList.remove('show');
}

/**
 * Restart the game (reload with current character).
 */
function restartGame() {
    location.reload();
}

/**
 * Quit to character creation.
 */
function quitGame() {
    location.href = '/create';
}

/**
 * Get movement direction from currently held keys.
 */
function getMovementInput() {
    let dx = 0, dy = 0;

    if (keysHeld.has('arrowup') || keysHeld.has('w')) dy = -1;
    if (keysHeld.has('arrowdown') || keysHeld.has('s')) dy = 1;
    if (keysHeld.has('arrowleft') || keysHeld.has('a')) dx = -1;
    if (keysHeld.has('arrowright') || keysHeld.has('d')) dx = 1;

    // Normalize diagonal movement
    if (dx !== 0 && dy !== 0) {
        dx *= 0.707;
        dy *= 0.707;
    }

    return { dx, dy };
}

// ============================================================
// GAME LOOP
// ============================================================

/**
 * Main game loop - called every frame.
 */
function gameLoop(timestamp) {
    const deltaTime = timestamp - lastTime;
    lastTime = timestamp;

    if (gameRunning && !transitioning && !gamePaused) {
        update(deltaTime);
        render();
    } else if (transitioning) {
        // During transition, just render a loading state
        renderTransition();
    } else if (gamePaused) {
        render(); // Still render the game behind the pause menu
    }

    requestAnimationFrame(gameLoop);
}

/**
 * Render transition screen.
 */
function renderTransition() {
    ctx.fillStyle = '#0a0a15';
    ctx.fillRect(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);
    ctx.fillStyle = '#fff';
    ctx.font = '20px Courier New';
    ctx.textAlign = 'center';
    ctx.fillText('Loading...', CANVAS_WIDTH / 2, CANVAS_HEIGHT / 2);
}

/**
 * Update game state.
 */
function update(deltaTime) {
    // Update cooldowns
    if (player.weaponCooldown > 0) {
        player.weaponCooldown -= deltaTime;
    }
    if (player.invincible > 0) {
        player.invincible -= deltaTime;
    }
    if (stairsTransitionCooldown > 0) {
        stairsTransitionCooldown -= deltaTime;
    }

    // ITERATION 4: Completely rewritten knockback system
    // The key insight is that we should NEVER allow ANY movement that results
    // in collision. Instead of applying velocity and checking after, we
    // pre-compute the SAFE position and teleport there instantly.
    if (player.knockback.time > 0) {
        player.knockback.time -= deltaTime;

        // Store original position BEFORE any changes
        const originalX = player.x;
        const originalY = player.y;

        // Calculate desired knockback movement this frame
        const kbDecay = Math.max(0, player.knockback.time / 200);
        const desiredMoveX = player.knockback.x * kbDecay * 0.25; // Reduced multiplier
        const desiredMoveY = player.knockback.y * kbDecay * 0.25;

        // Only process if movement is significant
        if (Math.abs(desiredMoveX) > 0.05 || Math.abs(desiredMoveY) > 0.05) {
            // Use FULL hitbox for collision (no shortcuts)
            const hitboxSize = PLAYER_SIZE;

            // Calculate target position
            let targetX = player.x + desiredMoveX;
            let targetY = player.y + desiredMoveY;

            // Test in VERY small increments (1 pixel at a time)
            const totalDist = Math.hypot(desiredMoveX, desiredMoveY);
            const numSteps = Math.max(1, Math.ceil(totalDist));

            // Track valid position as we move
            let lastSafeX = player.x;
            let lastSafeY = player.y;

            for (let step = 1; step <= numSteps; step++) {
                const progress = step / numSteps;
                const testX = player.x + desiredMoveX * progress;
                const testY = player.y + desiredMoveY * progress;

                // Check if this position is safe
                if (!checkTileCollision(testX, testY, hitboxSize)) {
                    lastSafeX = testX;
                    lastSafeY = testY;
                } else {
                    // Hit a wall - stop here and cancel knockback in this direction
                    if (Math.abs(testX - player.x) > Math.abs(testY - player.y)) {
                        player.knockback.x = 0; // Cancel X knockback
                    } else {
                        player.knockback.y = 0; // Cancel Y knockback
                    }
                    // Stop moving
                    break;
                }
            }

            // Move to last safe position
            player.x = lastSafeX;
            player.y = lastSafeY;

            // CRITICAL: Verify final position is actually safe
            if (checkTileCollision(player.x, player.y, hitboxSize)) {
                // This should NEVER happen, but if it does, emergency rollback
                console.error('EMERGENCY: Knockback resulted in wall collision! Rolling back.');
                player.x = originalX;
                player.y = originalY;
                player.knockback.time = 0;
                player.knockback.x = 0;
                player.knockback.y = 0;
            }
        }

        // Clamp to SAFE map bounds (inner walkable area only)
        // Map borders are ALWAYS walls, so stay well inside
        const safeMargin = TILE_SIZE + PLAYER_SIZE / 2 + 4;
        const minSafeX = safeMargin;
        const maxSafeX = CANVAS_WIDTH - safeMargin;
        const minSafeY = safeMargin;
        const maxSafeY = CANVAS_HEIGHT - safeMargin;

        player.x = Math.max(minSafeX, Math.min(maxSafeX, player.x));
        player.y = Math.max(minSafeY, Math.min(maxSafeY, player.y));

        // Final safety net - if STILL in wall, push out
        if (checkTileCollision(player.x, player.y, PLAYER_SIZE)) {
            pushOutOfWalls();
        }
    } else {
        // Normal player movement
        updatePlayer(deltaTime);
    }

    // Update enemies
    updateEnemies(deltaTime);

    // Update projectiles
    updateProjectiles(deltaTime);

    // Update attacks (melee animations)
    updateAttacks(deltaTime);

    // Check collisions
    checkCollisions();

    // Check door/transition triggers
    checkDoorTriggers();

    // Check chest interactions
    checkChestInteraction();

    // Final safety check - ALWAYS ensure player isn't stuck in a wall
    // This runs every frame as a failsafe against any edge case
    ensurePlayerNotInWall();
}

/**
 * Final safety check - ensures player is never stuck in a wall.
 * Runs every frame as a failsafe.
 *
 * ITERATION 4: More aggressive check - uses FULL hitbox
 */
function ensurePlayerNotInWall() {
    // Check if player is currently colliding with a wall
    // Use FULL hitbox size - no compromises
    if (checkTileCollision(player.x, player.y, PLAYER_SIZE)) {
        console.warn('Player in wall detected at', player.x, player.y, '- rescuing');

        // Player is stuck! Push them out immediately
        pushOutOfWalls();

        // Also cancel any ongoing knockback to prevent re-entry
        player.knockback.time = 0;
        player.knockback.x = 0;
        player.knockback.y = 0;

        // Double-check we're actually out
        if (checkTileCollision(player.x, player.y, PLAYER_SIZE)) {
            console.error('pushOutOfWalls failed! Emergency teleport to map center');
            // Emergency: teleport to center of map
            player.x = 8 * TILE_SIZE + TILE_SIZE / 2;
            player.y = 6 * TILE_SIZE + TILE_SIZE / 2;

            // If center is ALSO blocked (rare), scan for ANY safe tile
            if (checkTileCollision(player.x, player.y, PLAYER_SIZE)) {
                for (let y = 2; y < MAP_HEIGHT - 2; y++) {
                    for (let x = 2; x < MAP_WIDTH - 2; x++) {
                        const testX = x * TILE_SIZE + TILE_SIZE / 2;
                        const testY = y * TILE_SIZE + TILE_SIZE / 2;
                        if (!checkTileCollision(testX, testY, PLAYER_SIZE)) {
                            player.x = testX;
                            player.y = testY;
                            console.log('Emergency rescue to tile', x, y);
                            return;
                        }
                    }
                }
            }
        }
    }
}

/**
 * Update player position based on input.
 */
function updatePlayer(deltaTime) {
    const input = getMovementInput();

    if (input.dx !== 0 || input.dy !== 0) {
        const newX = player.x + input.dx * PLAYER_SPEED;
        const newY = player.y + input.dy * PLAYER_SPEED;

        // Update direction
        if (Math.abs(input.dx) > Math.abs(input.dy)) {
            player.direction = input.dx > 0 ? 'right' : 'left';
        } else {
            player.direction = input.dy > 0 ? 'down' : 'up';
        }

        // Check collision and move
        if (!checkTileCollision(newX, player.y, PLAYER_SIZE)) {
            player.x = newX;
        }
        if (!checkTileCollision(player.x, newY, PLAYER_SIZE)) {
            player.y = newY;
        }

        // Clamp to map bounds
        player.x = Math.max(PLAYER_SIZE/2, Math.min(CANVAS_WIDTH - PLAYER_SIZE/2, player.x));
        player.y = Math.max(PLAYER_SIZE/2, Math.min(CANVAS_HEIGHT - PLAYER_SIZE/2, player.y));
    }
}

/**
 * Update enemy positions and behavior.
 * Enemies move chaotically like in Link's Awakening.
 * Dragon has special fire breath attack.
 */
function updateEnemies(deltaTime) {
    enemies.forEach(enemy => {
        enemy.moveTimer += deltaTime;
        enemy.changeDir -= deltaTime;

        // Initialize fire cooldown if not set
        if (enemy.fireCooldown === undefined) {
            enemy.fireCooldown = 0;
        }
        enemy.fireCooldown -= deltaTime;

        // Special behavior for dragon
        if (enemy.type === 'dragon') {
            updateDragon(enemy, deltaTime);
            return;
        }

        // Change direction randomly
        if (enemy.changeDir <= 0) {
            enemy.direction = Math.random() * Math.PI * 2;
            enemy.changeDir = 500 + Math.random() * 1000;
        }

        // Move in current direction
        const speed = getEnemySpeed(enemy.type);
        const newX = enemy.x + Math.cos(enemy.direction) * speed;
        const newY = enemy.y + Math.sin(enemy.direction) * speed;

        // Check collision and move
        if (!checkTileCollision(newX, enemy.y, ENEMY_SIZE)) {
            enemy.x = newX;
        } else {
            enemy.direction = Math.PI - enemy.direction; // Bounce
        }

        if (!checkTileCollision(enemy.x, newY, ENEMY_SIZE)) {
            enemy.y = newY;
        } else {
            enemy.direction = -enemy.direction; // Bounce
        }

        // Clamp to walkable area
        enemy.x = Math.max(TILE_SIZE + ENEMY_SIZE/2, Math.min(CANVAS_WIDTH - TILE_SIZE - ENEMY_SIZE/2, enemy.x));
        enemy.y = Math.max(TILE_SIZE + ENEMY_SIZE/2, Math.min(CANVAS_HEIGHT - TILE_SIZE - ENEMY_SIZE/2, enemy.y));
    });
}

/**
 * Update dragon boss behavior.
 * Dragon moves slowly and periodically breathes fire in a cone towards the player.
 */
function updateDragon(dragon, deltaTime) {
    // Dragon moves slower and tracks player loosely
    dragon.changeDir -= deltaTime;

    // Occasionally face the player
    if (dragon.changeDir <= 0) {
        const dx = player.x - dragon.x;
        const dy = player.y - dragon.y;
        dragon.direction = Math.atan2(dy, dx);
        dragon.changeDir = 1500 + Math.random() * 1000;
    }

    // Move slowly
    const speed = 1;
    const newX = dragon.x + Math.cos(dragon.direction) * speed;
    const newY = dragon.y + Math.sin(dragon.direction) * speed;

    if (!checkTileCollision(newX, dragon.y, ENEMY_SIZE * 1.5)) {
        dragon.x = newX;
    }
    if (!checkTileCollision(dragon.x, newY, ENEMY_SIZE * 1.5)) {
        dragon.y = newY;
    }

    // Clamp to walkable area
    dragon.x = Math.max(TILE_SIZE * 2, Math.min(CANVAS_WIDTH - TILE_SIZE * 2, dragon.x));
    dragon.y = Math.max(TILE_SIZE * 2, Math.min(CANVAS_HEIGHT - TILE_SIZE * 2, dragon.y));

    // Fire breath attack - cone of 3 fireballs towards player
    if (dragon.fireCooldown <= 0) {
        dragonFireBreath(dragon);
        dragon.fireCooldown = 2500 + Math.random() * 1000; // 2.5-3.5 seconds between attacks
    }
}

/**
 * Dragon breathes fire in a cone towards the player.
 * Creates 3 fireballs in a spread pattern.
 */
function dragonFireBreath(dragon) {
    const dx = player.x - dragon.x;
    const dy = player.y - dragon.y;
    const baseAngle = Math.atan2(dy, dx);

    // Fire speed - slow enough to dodge
    const fireSpeed = 3;

    // Spread angle for the cone (30 degrees on each side = 60 degree cone)
    const spreadAngle = Math.PI / 6;

    // Create 3 fireballs in a cone pattern
    const angles = [baseAngle - spreadAngle, baseAngle, baseAngle + spreadAngle];

    angles.forEach(angle => {
        projectiles.push({
            type: 'dragon_fire',
            x: dragon.x,
            y: dragon.y,
            vx: Math.cos(angle) * fireSpeed,
            vy: Math.sin(angle) * fireSpeed,
            damage: 20,
            size: 20,
            lifetime: 2000,
            isEnemy: true  // Mark as enemy projectile to damage player
        });
    });
}

/**
 * Get speed for enemy type.
 */
function getEnemySpeed(type) {
    switch (type) {
        case 'bat': return 3;
        case 'ghost': return 2.5;
        case 'slime': return 1.5;
        case 'skeleton': return 2;
        case 'demon': return 2;
        case 'spider': return 3;
        case 'dragon': return 1;
        default: return 2;
    }
}

/**
 * Update projectiles.
 */
function updateProjectiles(deltaTime) {
    projectiles = projectiles.filter(proj => {
        proj.x += proj.vx;
        proj.y += proj.vy;
        proj.lifetime -= deltaTime;

        // Check if projectile hit a wall or is off screen
        if (proj.lifetime <= 0 ||
            proj.x < 0 || proj.x > CANVAS_WIDTH ||
            proj.y < 0 || proj.y > CANVAS_HEIGHT ||
            checkTileCollision(proj.x, proj.y, 8)) {
            return false;
        }

        // Enemy projectiles damage player
        if (proj.isEnemy) {
            const dist = Math.hypot(player.x - proj.x, player.y - proj.y);
            if (dist < PLAYER_SIZE/2 + proj.size/2) {
                damagePlayer(proj.damage, proj.x, proj.y);
                proj.lifetime = 0; // Remove projectile
            }
        } else {
            // Player projectiles damage enemies
            enemies.forEach(enemy => {
                const dist = Math.hypot(enemy.x - proj.x, enemy.y - proj.y);
                if (dist < ENEMY_SIZE/2 + proj.size/2) {
                    damageEnemy(enemy, proj.damage);
                    proj.lifetime = 0; // Remove projectile
                }
            });
        }

        return proj.lifetime > 0;
    });
}

/**
 * Update attack animations (melee).
 */
function updateAttacks(deltaTime) {
    attacks = attacks.filter(attack => {
        attack.lifetime -= deltaTime;

        // Check enemy hits during attack
        if (!attack.hitEnemies) attack.hitEnemies = new Set();

        enemies.forEach(enemy => {
            if (attack.hitEnemies.has(enemy.id)) return;

            if (isInAttackArea(attack, enemy.x, enemy.y)) {
                damageEnemy(enemy, attack.damage);
                attack.hitEnemies.add(enemy.id);
            }
        });

        return attack.lifetime > 0;
    });
}

/**
 * Check if a point is in an attack area.
 */
function isInAttackArea(attack, x, y) {
    switch (attack.type) {
        case 'sword': {
            // 90 degree arc in front
            const dx = x - attack.x;
            const dy = y - attack.y;
            const dist = Math.hypot(dx, dy);
            if (dist > attack.range) return false;

            const angle = Math.atan2(dy, dx);
            const dirAngle = directionToAngle(attack.direction);
            let diff = angle - dirAngle;
            while (diff > Math.PI) diff -= Math.PI * 2;
            while (diff < -Math.PI) diff += Math.PI * 2;
            return Math.abs(diff) < Math.PI / 4; // 90 degree arc
        }

        case 'dagger':
        case 'spear': {
            // Line poke
            const dx = x - attack.x;
            const dy = y - attack.y;
            const dist = Math.hypot(dx, dy);
            if (dist > attack.range) return false;

            const angle = Math.atan2(dy, dx);
            const dirAngle = directionToAngle(attack.direction);
            let diff = angle - dirAngle;
            while (diff > Math.PI) diff -= Math.PI * 2;
            while (diff < -Math.PI) diff += Math.PI * 2;
            return Math.abs(diff) < Math.PI / 8; // Narrow cone
        }

        case 'staff_ice': {
            // Circle around player
            const dist = Math.hypot(x - attack.x, y - attack.y);
            return dist < attack.range;
        }

        case 'staff_lightning': {
            // Cone in front
            const dx = x - attack.x;
            const dy = y - attack.y;
            const dist = Math.hypot(dx, dy);
            if (dist > attack.range) return false;

            const angle = Math.atan2(dy, dx);
            const dirAngle = directionToAngle(attack.direction);
            let diff = angle - dirAngle;
            while (diff > Math.PI) diff -= Math.PI * 2;
            while (diff < -Math.PI) diff += Math.PI * 2;
            return Math.abs(diff) < Math.PI / 3; // Wide cone
        }

        default:
            return false;
    }
}

/**
 * Convert direction string to angle.
 */
function directionToAngle(dir) {
    switch (dir) {
        case 'right': return 0;
        case 'down': return Math.PI / 2;
        case 'left': return Math.PI;
        case 'up': return -Math.PI / 2;
        default: return 0;
    }
}

/**
 * Get direction offset for attacks/projectiles.
 */
function getDirectionOffset(dir) {
    switch (dir) {
        case 'right': return { x: 1, y: 0 };
        case 'left': return { x: -1, y: 0 };
        case 'down': return { x: 0, y: 1 };
        case 'up': return { x: 0, y: -1 };
        default: return { x: 0, y: 1 };
    }
}

// ============================================================
// COMBAT
// ============================================================

/**
 * Perform an attack with the current weapon.
 */
function attack() {
    if (player.weaponCooldown > 0) return;

    const weapon = player.weapon;
    const cooldown = WEAPON_COOLDOWNS[weapon] || 400;
    const damage = WEAPON_DAMAGE[weapon] || 10;

    player.weaponCooldown = cooldown;

    const dir = getDirectionOffset(player.direction);

    switch (weapon) {
        case 'sword':
            attacks.push({
                type: 'sword',
                x: player.x,
                y: player.y,
                direction: player.direction,
                damage: damage,
                range: 50,
                lifetime: 200
            });
            break;

        case 'dagger':
            attacks.push({
                type: 'dagger',
                x: player.x + dir.x * 20,
                y: player.y + dir.y * 20,
                direction: player.direction,
                damage: damage,
                range: 30,
                lifetime: 150
            });
            break;

        case 'spear':
            attacks.push({
                type: 'spear',
                x: player.x + dir.x * 30,
                y: player.y + dir.y * 30,
                direction: player.direction,
                damage: damage,
                range: 60,
                lifetime: 200
            });
            break;

        case 'bow':
            projectiles.push({
                type: 'arrow',
                x: player.x + dir.x * 20,
                y: player.y + dir.y * 20,
                vx: dir.x * 10,
                vy: dir.y * 10,
                damage: damage,
                size: 8,
                lifetime: 1000
            });
            break;

        case 'staff_fire':
            projectiles.push({
                type: 'fireball',
                x: player.x + dir.x * 20,
                y: player.y + dir.y * 20,
                vx: dir.x * 7,
                vy: dir.y * 7,
                damage: damage,
                size: 16,
                lifetime: 1200
            });
            break;

        case 'staff_lightning':
            attacks.push({
                type: 'staff_lightning',
                x: player.x,
                y: player.y,
                direction: player.direction,
                damage: damage,
                range: 100,
                lifetime: 300
            });
            break;

        case 'staff_ice':
            attacks.push({
                type: 'staff_ice',
                x: player.x,
                y: player.y,
                direction: player.direction,
                damage: damage,
                range: 70,
                lifetime: 400
            });
            break;
    }
}

/**
 * Damage an enemy.
 */
function damageEnemy(enemy, damage) {
    enemy.hp -= damage;

    if (enemy.hp <= 0) {
        // Enemy killed
        killEnemy(enemy);
    }
}

/**
 * Remove enemy and update server state.
 */
function killEnemy(enemy) {
    enemies = enemies.filter(e => e.id !== enemy.id);

    // Notify server
    fetch('/api/game', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
            action: 'kill_enemy',
            enemyId: enemy.id
        })
    });

    // Check if dragon was killed
    if (enemy.type === 'dragon') {
        victory();
    }
}

/**
 * Damage the player.
 */
function damagePlayer(damage, fromX, fromY) {
    if (player.invincible > 0) return;

    gameState.hp -= damage;
    player.invincible = 1000; // 1 second invincibility

    // Calculate knockback direction
    const dx = player.x - fromX;
    const dy = player.y - fromY;
    const dist = Math.hypot(dx, dy);

    // Determine knockback velocity (normalized direction * knockback strength)
    // Reduced strength for better control
    let kbX = (dx / dist) * 5;
    let kbY = (dy / dist) * 5;

    // Pre-validate knockback direction - test the FULL knockback range
    // Test multiple points along the knockback path to ensure safety
    const maxKnockbackDist = 25; // Maximum distance player could be knocked

    // Test knockback path in multiple steps
    let validKbX = kbX;
    let validKbY = kbY;

    for (let testDist = 5; testDist <= maxKnockbackDist; testDist += 5) {
        const testX = player.x + (kbX / 5) * testDist;
        const testY = player.y + (kbY / 5) * testDist;

        // Check X direction
        if (validKbX !== 0 && checkTileCollision(testX, player.y, PLAYER_SIZE - 4)) {
            validKbX = 0;
        }

        // Check Y direction
        if (validKbY !== 0 && checkTileCollision(player.x, testY, PLAYER_SIZE - 4)) {
            validKbY = 0;
        }

        // Check diagonal
        if (validKbX !== 0 && validKbY !== 0 && checkTileCollision(testX, testY, PLAYER_SIZE - 4)) {
            // Prefer canceling the smaller component
            if (Math.abs(validKbX) < Math.abs(validKbY)) {
                validKbX = 0;
            } else {
                validKbY = 0;
            }
        }
    }

    kbX = validKbX;
    kbY = validKbY;

    // If both directions blocked, try to find a safe direction with minimal knockback
    if (kbX === 0 && kbY === 0) {
        // Try to find a safe direction - check all 8 directions
        const safeDirections = [
            { x: 1, y: 0 }, { x: -1, y: 0 },
            { x: 0, y: 1 }, { x: 0, y: -1 },
            { x: 0.7, y: 0.7 }, { x: -0.7, y: 0.7 },
            { x: 0.7, y: -0.7 }, { x: -0.7, y: -0.7 }
        ];
        for (const dir of safeDirections) {
            const testDist = 15;
            const safeX = player.x + dir.x * testDist;
            const safeY = player.y + dir.y * testDist;
            if (!checkTileCollision(safeX, safeY, PLAYER_SIZE - 4)) {
                kbX = dir.x * 3;
                kbY = dir.y * 3;
                break;
            }
        }
    }

    player.knockback = {
        x: kbX,
        y: kbY,
        time: 200
    };

    // Update UI
    updateHpBar();

    // Notify server
    fetch('/api/game', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
            action: 'take_damage',
            damage: damage
        })
    });

    if (gameState.hp <= 0) {
        gameOver();
    }
}

/**
 * Check collision with tile map.
 */
function checkTileCollision(x, y, size) {
    const halfSize = size / 2;

    // Check corners of bounding box
    const corners = [
        { x: x - halfSize, y: y - halfSize },
        { x: x + halfSize, y: y - halfSize },
        { x: x - halfSize, y: y + halfSize },
        { x: x + halfSize, y: y + halfSize }
    ];

    for (const corner of corners) {
        const tileX = Math.floor(corner.x / TILE_SIZE);
        const tileY = Math.floor(corner.y / TILE_SIZE);

        if (tileX < 0 || tileX >= MAP_WIDTH || tileY < 0 || tileY >= MAP_HEIGHT) {
            return true; // Out of bounds
        }

        const tile = mapTiles[tileY]?.[tileX];
        if (tile === TILE_WALL || tile === TILE_WATER) {
            return true;
        }

        // Locked doors block if we don't have the key
        if (tile === TILE_LOCKED_DOOR) {
            const door = doors.find(d => isDoorAtTile(d, tileX, tileY));
            if (door && door.keyRequired && !gameState.keys.includes(door.keyRequired)) {
                return true;
            }
        }
    }

    return false;
}

/**
 * Push player out of walls if they somehow got stuck.
 * Uses multiple strategies to find a valid position:
 * 1. Try small pixel adjustments (for minor clipping)
 * 2. Try 8 cardinal/diagonal directions
 * 3. Use spiral search for worst cases
 */
function pushOutOfWalls() {
    // If player is not colliding, nothing to do
    if (!checkTileCollision(player.x, player.y, PLAYER_SIZE - 2)) {
        return;
    }

    console.log('Player stuck in wall at', player.x, player.y, '- attempting rescue');

    // Strategy 1: Try small pixel adjustments (fixes minor clipping)
    // Test increasingly larger offsets in all 8 directions
    const offsetDistances = [4, 8, 12, 16, 20];
    const directions8 = [
        { x: -1, y: 0 }, { x: 1, y: 0 },
        { x: 0, y: -1 }, { x: 0, y: 1 },
        { x: -0.707, y: -0.707 }, { x: 0.707, y: -0.707 },
        { x: -0.707, y: 0.707 }, { x: 0.707, y: 0.707 }
    ];

    for (const dist of offsetDistances) {
        for (const dir of directions8) {
            const testX = player.x + dir.x * dist;
            const testY = player.y + dir.y * dist;
            if (!checkTileCollision(testX, testY, PLAYER_SIZE - 4)) {
                player.x = testX;
                player.y = testY;
                console.log('Rescued with pixel offset:', dist, dir);
                return;
            }
        }
    }

    // Strategy 2: Try moving to adjacent tiles in 8 directions
    const currentTileX = Math.floor(player.x / TILE_SIZE);
    const currentTileY = Math.floor(player.y / TILE_SIZE);

    const directions = [
        { dx: -1, dy: 0 }, { dx: 1, dy: 0 },
        { dx: 0, dy: -1 }, { dx: 0, dy: 1 },
        { dx: -1, dy: -1 }, { dx: 1, dy: -1 },
        { dx: -1, dy: 1 }, { dx: 1, dy: 1 }
    ];

    for (const dir of directions) {
        const tileX = currentTileX + dir.dx;
        const tileY = currentTileY + dir.dy;

        // Check bounds
        if (tileX < 1 || tileX >= MAP_WIDTH - 1 || tileY < 1 || tileY >= MAP_HEIGHT - 1) {
            continue;
        }

        // Check if this tile is walkable
        const tile = mapTiles[tileY]?.[tileX];
        if (tile === TILE_FLOOR || tile === TILE_DOOR || tile === TILE_STAIRS_UP || tile === TILE_STAIRS_DOWN) {
            // Position player at center of this tile
            const newX = tileX * TILE_SIZE + TILE_SIZE / 2;
            const newY = tileY * TILE_SIZE + TILE_SIZE / 2;

            // Final validation
            if (!checkTileCollision(newX, newY, PLAYER_SIZE - 2)) {
                player.x = newX;
                player.y = newY;
                console.log('Rescued to adjacent tile:', tileX, tileY);
                return;
            }
        }
    }

    // Strategy 3: Spiral search for valid position (worst case)
    const maxRadius = 5;

    for (let radius = 2; radius <= maxRadius; radius++) {
        // Check all tiles at this radius
        for (let dy = -radius; dy <= radius; dy++) {
            for (let dx = -radius; dx <= radius; dx++) {
                // Only check tiles on the perimeter of this radius
                if (Math.abs(dx) !== radius && Math.abs(dy) !== radius) continue;

                const tileX = currentTileX + dx;
                const tileY = currentTileY + dy;

                // Check bounds
                if (tileX < 1 || tileX >= MAP_WIDTH - 1 || tileY < 1 || tileY >= MAP_HEIGHT - 1) {
                    continue;
                }

                // Check if this tile is walkable
                const tile = mapTiles[tileY]?.[tileX];
                if (tile === TILE_FLOOR || tile === TILE_DOOR || tile === TILE_STAIRS_UP || tile === TILE_STAIRS_DOWN) {
                    // Position player at center of this tile
                    const newX = tileX * TILE_SIZE + TILE_SIZE / 2;
                    const newY = tileY * TILE_SIZE + TILE_SIZE / 2;

                    // Final validation
                    if (!checkTileCollision(newX, newY, PLAYER_SIZE - 2)) {
                        player.x = newX;
                        player.y = newY;
                        console.log('Rescued with spiral search at radius', radius);
                        return;
                    }
                }
            }
        }
    }

    // Fallback: teleport to map center if all else fails
    console.log('Using fallback: teleporting to map center');
    player.x = 8 * TILE_SIZE + TILE_SIZE / 2;
    player.y = 6 * TILE_SIZE + TILE_SIZE / 2;

    // Verify the fallback position is safe
    if (checkTileCollision(player.x, player.y, PLAYER_SIZE - 2)) {
        // Even the center is blocked! Find ANY walkable tile
        for (let y = 1; y < MAP_HEIGHT - 1; y++) {
            for (let x = 1; x < MAP_WIDTH - 1; x++) {
                const tile = mapTiles[y]?.[x];
                if (tile === TILE_FLOOR || tile === TILE_DOOR) {
                    const testX = x * TILE_SIZE + TILE_SIZE / 2;
                    const testY = y * TILE_SIZE + TILE_SIZE / 2;
                    if (!checkTileCollision(testX, testY, PLAYER_SIZE - 2)) {
                        player.x = testX;
                        player.y = testY;
                        console.log('Emergency rescue to tile:', x, y);
                        return;
                    }
                }
            }
        }
    }
}

/**
 * Check if a door is at a specific tile.
 */
function isDoorAtTile(door, tileX, tileY) {
    switch (door.direction) {
        case 'north': return tileY === 0 && tileX === 7;
        case 'south': return tileY === MAP_HEIGHT - 1 && tileX === 7;
        case 'west': return tileX === 0 && tileY === 5;
        case 'east': return tileX === MAP_WIDTH - 1 && tileY === 5;
    }
    return false;
}

/**
 * Check collisions between player and enemies.
 */
function checkCollisions() {
    if (player.invincible > 0) return;

    enemies.forEach(enemy => {
        const dist = Math.hypot(enemy.x - player.x, enemy.y - player.y);
        if (dist < (PLAYER_SIZE + ENEMY_SIZE) / 2) {
            const damage = getEnemyDamage(enemy.type);
            damagePlayer(damage, enemy.x, enemy.y);
        }
    });
}

/**
 * Get damage dealt by enemy type.
 */
function getEnemyDamage(type) {
    switch (type) {
        case 'slime': return 5;
        case 'bat': return 8;
        case 'skeleton': return 12;
        case 'ghost': return 10;
        case 'demon': return 18;
        case 'spider': return 10;
        case 'dragon': return 25;
        default: return 10;
    }
}

// ============================================================
// MAP TRANSITIONS
// ============================================================

/**
 * Check if player triggered a door/transition.
 */
function checkDoorTriggers() {
    const tileX = Math.floor(player.x / TILE_SIZE);
    const tileY = Math.floor(player.y / TILE_SIZE);
    const tile = mapTiles[tileY]?.[tileX];

    // Check for door tiles at map edges
    if (tile === TILE_DOOR || tile === TILE_LOCKED_DOOR) {
        const door = doors.find(d => isDoorAtTile(d, tileX, tileY));
        if (door) {
            // Check if we have the key for locked doors
            if (door.keyRequired && !gameState.keys.includes(door.keyRequired)) {
                showMessage("You need a key to open this door!");
                return;
            }

            transitionToMap(door.targetArea, door.targetX, door.targetY, door.direction);
        }
    }

    // Check for stairs (with cooldown to prevent instant re-trigger)
    if ((tile === TILE_STAIRS_UP || tile === TILE_STAIRS_DOWN) && stairsTransitionCooldown <= 0) {
        // The transition info is included in the map data
        // For now, trigger map load
        if (tile === TILE_STAIRS_DOWN) {
            transitionArea('down');
        } else {
            transitionArea('up');
        }
    }
}

/**
 * Transition to a new map.
 */
function transitionToMap(area, mapX, mapY, fromDirection) {
    // Determine player entry position
    let entryX, entryY;
    switch (fromDirection) {
        case 'north':
            entryX = 7 * TILE_SIZE + TILE_SIZE / 2;
            entryY = (MAP_HEIGHT - 2) * TILE_SIZE + TILE_SIZE / 2;
            break;
        case 'south':
            entryX = 7 * TILE_SIZE + TILE_SIZE / 2;
            entryY = 1 * TILE_SIZE + TILE_SIZE / 2;
            break;
        case 'west':
            entryX = (MAP_WIDTH - 2) * TILE_SIZE + TILE_SIZE / 2;
            entryY = 5 * TILE_SIZE + TILE_SIZE / 2;
            break;
        case 'east':
            entryX = 1 * TILE_SIZE + TILE_SIZE / 2;
            entryY = 5 * TILE_SIZE + TILE_SIZE / 2;
            break;
        default:
            entryX = 8 * TILE_SIZE + TILE_SIZE / 2;
            entryY = 6 * TILE_SIZE + TILE_SIZE / 2;
    }

    // Update game state
    gameState.area = area;
    gameState.mapX = mapX;
    gameState.mapY = mapY;

    // Update area palette immediately
    updateAreaPalette();

    // Save to server and reload map
    saveAndLoadMap(Math.floor(entryX / TILE_SIZE), Math.floor(entryY / TILE_SIZE));

    // Update player position
    player.x = entryX;
    player.y = entryY;
}

/**
 * Transition to a different area (stairs).
 */
function transitionArea(direction) {
    let newArea, newX, newY;

    if (direction === 'down') {
        switch (gameState.area) {
            case 'training_grounds':
                newArea = 'castle';
                newX = 1;
                newY = 0;
                break;
            case 'castle':
                newArea = 'dungeon';
                newX = 0;
                newY = 1;
                break;
            default:
                return;
        }
    } else {
        switch (gameState.area) {
            case 'castle':
                newArea = 'training_grounds';
                newX = 2;
                newY = 3;
                break;
            case 'dungeon':
                newArea = 'castle';
                newX = 3;
                newY = 2;
                break;
            default:
                return;
        }
    }

    // Save current HP as floor entry HP (for "Try Again" functionality)
    // Chain API calls to avoid race conditions
    gameState.floorEntryHp = gameState.hp;

    gameState.area = newArea;
    gameState.mapX = newX;
    gameState.mapY = newY;

    // Update area palette immediately for the transition screen
    updateAreaPalette();

    // Set cooldown to prevent instant re-trigger when arriving on destination stairs
    stairsTransitionCooldown = 1000; // 1 second cooldown

    // Position player slightly offset from stairs (tile 7,6 instead of 8,6)
    // This prevents immediate re-transition
    player.x = 7 * TILE_SIZE + TILE_SIZE / 2;
    player.y = 6 * TILE_SIZE + TILE_SIZE / 2;

    // Chain: save floor HP first, then save map position
    fetch('/api/game', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ action: 'save_floor_entry_hp' })
    }).then(() => {
        saveAndLoadMap(7, 6);
    });
}

/**
 * Save state to server and load new map data.
 */
function saveAndLoadMap(entryTileX, entryTileY) {
    // Set transitioning flag to prevent updates during load
    transitioning = true;

    // Save state
    fetch('/api/game', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
            action: 'move_map',
            area: gameState.area,
            mapX: gameState.mapX,
            mapY: gameState.mapY,
            tileX: entryTileX,
            tileY: entryTileY
        })
    });

    // Load new map
    fetch(`/api/map/${gameState.area}/${gameState.mapX}/${gameState.mapY}`)
        .then(res => res.json())
        .then(data => {
            mapTiles = data.tiles;
            doors = data.doors;
            chests = data.chests;

            // Calculate player position
            const playerX = entryTileX * TILE_SIZE + TILE_SIZE / 2;
            const playerY = entryTileY * TILE_SIZE + TILE_SIZE / 2;

            // Load enemies, filtering out any that would spawn too close to the player or in walls
            const minSpawnDistance = TILE_SIZE * 2; // Enemies must be at least 2 tiles away
            enemies = data.enemies
                .filter(e => {
                    const enemyX = e.x * TILE_SIZE + TILE_SIZE / 2;
                    const enemyY = e.y * TILE_SIZE + TILE_SIZE / 2;
                    const dist = Math.hypot(enemyX - playerX, enemyY - playerY);
                    // Check distance from player
                    if (dist < minSpawnDistance) return false;
                    // Check if enemy would spawn in a wall
                    const tile = mapTiles[e.y]?.[e.x];
                    if (tile === TILE_WALL || tile === TILE_WATER) return false;
                    return true;
                })
                .map(e => ({
                    id: e.id,
                    type: e.type,
                    x: e.x * TILE_SIZE + TILE_SIZE / 2,
                    y: e.y * TILE_SIZE + TILE_SIZE / 2,
                    hp: e.hp,
                    maxHp: e.maxHp,
                    direction: Math.random() * Math.PI * 2,
                    moveTimer: 0,
                    changeDir: 0
                }));

            // Clear projectiles and attacks
            projectiles = [];
            attacks = [];

            // Update location display
            updateLocationDisplay();

            // Update color palette for the area
            updateAreaPalette();

            // Brief delay before resuming gameplay to prevent visual glitches
            setTimeout(() => {
                transitioning = false;
            }, 100);
        });
}

// ============================================================
// CHESTS
// ============================================================

/**
 * Check if player is interacting with a chest.
 */
function checkChestInteraction() {
    if (!keysHeld.has('e') && !keysHeld.has('enter')) return;

    chests.forEach(chest => {
        if (chest.opened) return;

        const chestX = chest.x * TILE_SIZE + TILE_SIZE / 2;
        const chestY = chest.y * TILE_SIZE + TILE_SIZE / 2;
        const dist = Math.hypot(chestX - player.x, chestY - player.y);

        if (dist < TILE_SIZE * 1.5) {
            // Check if all enemies are dead
            if (enemies.length > 0) {
                showMessage("Defeat all enemies first!");
                return;
            }

            openChest(chest);
        }
    });
}

/**
 * Open a chest and collect its contents.
 * API calls are chained to avoid race conditions that would cause lost updates.
 */
function openChest(chest) {
    chest.opened = true;

    const contents = chest.contents;

    // First, add the item/key to state and save it
    // Then mark the chest as opened (chained to avoid race condition)
    let savePromise;

    if (contents.type === 'key') {
        gameState.keys.push(contents.id);
        showMessage(`Got ${formatKeyName(contents.id)}!`);

        savePromise = fetch('/api/game', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                action: 'add_key',
                key: contents.id
            })
        });
    } else if (contents.type === 'item') {
        gameState.inventory.push(contents.id);
        showMessage(`Got ${formatItemName(contents.id)}!`);
        updateInventoryDisplay();

        savePromise = fetch('/api/game', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                action: 'add_item',
                item: contents.id
            })
        });
    } else {
        // No contents, just mark as opened
        savePromise = Promise.resolve();
    }

    // Chain the open_chest call AFTER the item/key is saved
    // This prevents race conditions where open_chest overwrites the item save
    savePromise.then(() => {
        return fetch('/api/game', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                action: 'open_chest',
                chestId: chest.id
            })
        });
    });
}

/**
 * Format key name for display.
 */
function formatKeyName(keyId) {
    return keyId.replace(/_/g, ' ').replace(/\b\w/g, c => c.toUpperCase());
}

/**
 * Format item name for display.
 */
function formatItemName(itemId) {
    const names = {
        'sword': 'Sword',
        'dagger': 'Dagger',
        'spear': 'Spear',
        'bow': 'Bow',
        'staff_fire': 'Fire Staff',
        'staff_lightning': 'Lightning Staff',
        'staff_ice': 'Ice Staff',
        'health_potion': 'Health Potion'
    };
    return names[itemId] || itemId;
}

// ============================================================
// RENDERING
// ============================================================

/**
 * Render the game.
 */
function render() {
    // Clear canvas with area-specific background color
    ctx.fillStyle = COLORS.background || '#0a0a15';
    ctx.fillRect(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);

    // Draw map tiles
    renderMap();

    // Draw chests
    renderChests();

    // Draw enemies
    renderEnemies();

    // Draw player
    renderPlayer();

    // Draw projectiles
    renderProjectiles();

    // Draw attacks
    renderAttacks();
}

/**
 * Render the tile map.
 */
function renderMap() {
    for (let y = 0; y < MAP_HEIGHT; y++) {
        for (let x = 0; x < MAP_WIDTH; x++) {
            const tile = mapTiles[y]?.[x] ?? 0;

            ctx.fillStyle = getTileColor(tile);
            ctx.fillRect(x * TILE_SIZE, y * TILE_SIZE, TILE_SIZE, TILE_SIZE);

            // Draw tile borders for walls - use a lighter shade of the wall color
            if (tile === TILE_WALL) {
                ctx.strokeStyle = lightenColor(COLORS.wall, 20);
                ctx.lineWidth = 2;
                ctx.strokeRect(x * TILE_SIZE + 1, y * TILE_SIZE + 1, TILE_SIZE - 2, TILE_SIZE - 2);
            }

            // Draw lock icon on locked doors
            if (tile === TILE_LOCKED_DOOR) {
                drawLockIcon(x * TILE_SIZE + TILE_SIZE / 2, y * TILE_SIZE + TILE_SIZE / 2);
            }

            // Draw stairs with more detail
            if (tile === TILE_STAIRS_UP || tile === TILE_STAIRS_DOWN) {
                drawStairs(x * TILE_SIZE, y * TILE_SIZE, tile === TILE_STAIRS_DOWN);
            }
        }
    }
}

/**
 * Draw a lock icon at the specified position.
 */
function drawLockIcon(x, y) {
    ctx.save();

    // Lock body (rectangle)
    ctx.fillStyle = '#ffd700';  // Gold color
    ctx.fillRect(x - 6, y - 2, 12, 10);

    // Lock shackle (arc)
    ctx.strokeStyle = '#ffd700';
    ctx.lineWidth = 3;
    ctx.beginPath();
    ctx.arc(x, y - 4, 5, Math.PI, 0, false);
    ctx.stroke();

    // Keyhole
    ctx.fillStyle = '#000';
    ctx.beginPath();
    ctx.arc(x, y + 2, 2, 0, Math.PI * 2);
    ctx.fill();

    ctx.restore();
}

/**
 * Draw stairs with visual detail.
 */
function drawStairs(x, y, isDown) {
    ctx.save();

    const stepCount = 4;
    const stepHeight = TILE_SIZE / stepCount;
    const stepWidth = TILE_SIZE - 8;

    // Draw steps
    for (let i = 0; i < stepCount; i++) {
        // Steps get darker as they go down (or lighter for up)
        const shade = isDown ? (40 - i * 8) : (20 + i * 8);
        ctx.fillStyle = lightenColor(isDown ? COLORS.stairsDown : COLORS.stairsUp, shade);

        const stepY = isDown ? y + i * stepHeight : y + (stepCount - 1 - i) * stepHeight;
        const stepX = x + 4 + (isDown ? i * 2 : (stepCount - 1 - i) * 2);
        const width = stepWidth - (isDown ? i * 4 : (stepCount - 1 - i) * 4);

        ctx.fillRect(stepX, stepY, width, stepHeight - 1);

        // Step edge highlight
        ctx.fillStyle = lightenColor(isDown ? COLORS.stairsDown : COLORS.stairsUp, shade + 30);
        ctx.fillRect(stepX, stepY, width, 2);
    }

    // Arrow indicator
    ctx.fillStyle = '#ffffff';
    ctx.beginPath();
    const arrowX = x + TILE_SIZE / 2;
    const arrowY = y + TILE_SIZE / 2;
    if (isDown) {
        ctx.moveTo(arrowX, arrowY + 8);
        ctx.lineTo(arrowX - 6, arrowY);
        ctx.lineTo(arrowX + 6, arrowY);
    } else {
        ctx.moveTo(arrowX, arrowY - 8);
        ctx.lineTo(arrowX - 6, arrowY);
        ctx.lineTo(arrowX + 6, arrowY);
    }
    ctx.fill();

    ctx.restore();
}

/**
 * Lighten a hex color by a percentage.
 */
function lightenColor(color, percent) {
    const num = parseInt(color.replace('#', ''), 16);
    const amt = Math.round(2.55 * percent);
    const R = Math.min(255, (num >> 16) + amt);
    const G = Math.min(255, ((num >> 8) & 0x00FF) + amt);
    const B = Math.min(255, (num & 0x0000FF) + amt);
    return '#' + (0x1000000 + R * 0x10000 + G * 0x100 + B).toString(16).slice(1);
}

/**
 * Get color for a tile type.
 */
function getTileColor(tile) {
    switch (tile) {
        case TILE_FLOOR: return COLORS.floor;
        case TILE_WALL: return COLORS.wall;
        case TILE_WATER: return COLORS.water;
        case TILE_DOOR: return COLORS.door;
        case TILE_LOCKED_DOOR: return COLORS.lockedDoor;
        case TILE_CHEST: return COLORS.floor; // Chest drawn separately
        case TILE_STAIRS_UP: return COLORS.stairsUp;
        case TILE_STAIRS_DOWN: return COLORS.stairsDown;
        default: return COLORS.floor;
    }
}

/**
 * Render chests.
 */
function renderChests() {
    chests.forEach(chest => {
        const x = chest.x * TILE_SIZE;
        const y = chest.y * TILE_SIZE;

        ctx.fillStyle = chest.opened ? COLORS.chestOpen : COLORS.chest;
        ctx.fillRect(x + 8, y + 12, TILE_SIZE - 16, TILE_SIZE - 20);

        // Chest lid
        ctx.fillStyle = chest.opened ? '#4a3a1a' : '#9a7a3a';
        ctx.fillRect(x + 6, y + 8, TILE_SIZE - 12, 8);

        // Lock (if closed)
        if (!chest.opened) {
            ctx.fillStyle = '#ffcc00';
            ctx.fillRect(x + TILE_SIZE/2 - 3, y + 20, 6, 8);
        }
    });
}

/**
 * Get emoji for enemy type.
 */
function getEnemyEmoji(type) {
    switch (type) {
        case 'slime': return '🟢';      // Green circle for slime blob
        case 'bat': return '🦇';        // Bat
        case 'skeleton': return '💀';   // Skull for skeleton
        case 'ghost': return '👻';      // Ghost
        case 'demon': return '👿';      // Imp/demon
        case 'spider': return '🕷️';     // Spider
        case 'dragon': return '🐉';     // Dragon
        default: return '👾';           // Generic enemy
    }
}

// Flag to track if emoji rendering works (detected once at startup)
let emojiRenderingSupported = null;

/**
 * Check if the browser/system can render emojis properly as COLOR emojis.
 * Uses MULTIPLE tests to ensure emojis render as proper colorful images,
 * not as monochrome circles or text glyphs.
 *
 * ITERATION 4 FIX: Much stricter emoji detection
 * - Tests multiple emojis (fire, face, heart)
 * - Requires HIGH color variation (>150, up from >100)
 * - Checks for distinct warm colors (orange/red) in fire emoji
 * - Defaults to fallback if any doubt
 */
function checkEmojiSupport() {
    if (emojiRenderingSupported !== null) return emojiRenderingSupported;

    try {
        const testCanvas = document.createElement('canvas');
        testCanvas.width = 60;
        testCanvas.height = 60;
        const testCtx = testCanvas.getContext('2d');

        // Test 1: Fire emoji - should have orange/yellow/red colors
        testCtx.fillStyle = '#ffffff';
        testCtx.fillRect(0, 0, 60, 60);
        testCtx.font = '40px "Segoe UI Emoji", "Apple Color Emoji", "Noto Color Emoji", "Twemoji Mozilla", sans-serif';
        testCtx.textAlign = 'center';
        testCtx.textBaseline = 'middle';
        testCtx.fillText('🔥', 30, 30);

        const imageData = testCtx.getImageData(0, 0, 60, 60).data;
        let minR = 255, maxR = 0;
        let minG = 255, maxG = 0;
        let minB = 255, maxB = 0;
        let nonWhitePixels = 0;
        let orangePixels = 0;  // Specifically look for warm colors
        let redPixels = 0;

        for (let i = 0; i < imageData.length; i += 4) {
            const r = imageData[i];
            const g = imageData[i + 1];
            const b = imageData[i + 2];
            const a = imageData[i + 3];

            // Only check non-white, non-transparent pixels
            if (a > 128 && (r < 230 || g < 230 || b < 230)) {
                nonWhitePixels++;
                minR = Math.min(minR, r); maxR = Math.max(maxR, r);
                minG = Math.min(minG, g); maxG = Math.max(maxG, g);
                minB = Math.min(minB, b); maxB = Math.max(maxB, b);

                // Count orange/red pixels (fire should have these)
                if (r > 180 && g > 80 && g < 200 && b < 100) {
                    orangePixels++;
                }
                if (r > 180 && g < 100 && b < 100) {
                    redPixels++;
                }
            }
        }

        const redRange = maxR - minR;
        const greenRange = maxG - minG;
        const blueRange = maxB - minB;
        const totalVariation = redRange + greenRange + blueRange;

        // STRICTER requirements for emoji support:
        // 1. At least 30 non-white pixels (bigger test area)
        // 2. High color variation (>150) indicating multi-color emoji
        // 3. At least SOME warm-colored pixels (orange or red) for fire emoji
        // This rejects: black circles, gray circles, monochrome text
        const hasFireColors = orangePixels > 5 || redPixels > 5;
        const hasHighVariation = totalVariation > 150;
        const hasEnoughPixels = nonWhitePixels > 30;

        // ALL conditions must pass
        const hasGoodEmojiSupport = hasEnoughPixels && hasHighVariation && hasFireColors;

        emojiRenderingSupported = hasGoodEmojiSupport;

        console.log('Emoji rendering check (STRICT):', {
            nonWhitePixels,
            orangePixels,
            redPixels,
            redRange, greenRange, blueRange,
            totalVariation,
            hasFireColors,
            hasHighVariation,
            hasEnoughPixels,
            supported: emojiRenderingSupported
        });

        // If emoji check passed but barely, still use fallback
        // This catches edge cases where detection is close but emojis look bad
        if (emojiRenderingSupported && totalVariation < 200) {
            console.log('Emoji variation borderline - using fallback for safety');
            emojiRenderingSupported = false;
        }
    } catch (e) {
        console.warn('Error checking emoji support:', e);
        emojiRenderingSupported = false;
    }

    return emojiRenderingSupported;
}

// Force fallback mode for testing or if emoji detection fails
// Set this to true via browser console: window.FORCE_EMOJI_FALLBACK = true
// ITERATION 4: Default to TRUE to always use distinct shapes
let FORCE_EMOJI_FALLBACK = true;

/**
 * Draw an emoji on the canvas with proper rendering.
 * Falls back to canvas-drawn shapes if emojis aren't supported.
 * Always uses fallback if FORCE_EMOJI_FALLBACK is true.
 *
 * ITERATION 4 FIX: Now defaults to fallback mode for guaranteed visibility.
 * The fallback shapes are colorful and distinct, making the game more
 * accessible across all platforms.
 */
function drawEmoji(emoji, x, y, fontSize) {
    // ITERATION 4: Always use fallback shapes by default
    // This guarantees characters are visually distinct on ALL platforms
    // Users can disable fallback with: window.FORCE_EMOJI_FALLBACK = false
    if (FORCE_EMOJI_FALLBACK || window.FORCE_EMOJI_FALLBACK === undefined || window.FORCE_EMOJI_FALLBACK) {
        drawFallbackShape(emoji, x, y, fontSize);
        return;
    }

    // Only reach here if FORCE_EMOJI_FALLBACK is explicitly set to false
    // Check if emojis are supported (cached after first check)
    if (checkEmojiSupport()) {
        // Emojis work - render directly
        ctx.save();
        ctx.font = `${fontSize}px "Segoe UI Emoji", "Apple Color Emoji", "Noto Color Emoji", "Twemoji Mozilla", "Android Emoji", "EmojiSymbols", sans-serif`;
        ctx.textAlign = 'center';
        ctx.textBaseline = 'middle';
        ctx.fillText(emoji, x, y);
        ctx.restore();
    } else {
        // Emojis don't work - use fallback canvas shapes
        drawFallbackShape(emoji, x, y, fontSize);
    }
}

/**
 * Draw fallback shapes for entities when emojis aren't available.
 * Creates visually distinct shapes for each entity type.
 */
function drawFallbackShape(emoji, x, y, fontSize) {
    ctx.save();
    const radius = fontSize / 2 - 2;

    // Determine entity type by emoji and draw appropriate shape
    switch (emoji) {
        // Player emojis
        case '🧙': // Wizard
            drawWizardShape(x, y, radius);
            break;
        case '🧝': // Archer/Elf
            drawArcherShape(x, y, radius);
            break;
        case '🦸': // Warrior with sword
            drawWarriorSwordShape(x, y, radius);
            break;
        case '🥷': // Ninja/dagger warrior
            drawNinjaShape(x, y, radius);
            break;
        case '💂': // Guard/spear warrior
            drawGuardShape(x, y, radius);
            break;
        case '🧑': // Generic person
            drawGenericPersonShape(x, y, radius);
            break;

        // Enemy emojis
        case '🟢': // Slime
            drawSlimeShape(x, y, radius);
            break;
        case '🦇': // Bat
            drawBatShape(x, y, radius);
            break;
        case '💀': // Skeleton
            drawSkeletonShape(x, y, radius);
            break;
        case '👻': // Ghost
            drawGhostShape(x, y, radius);
            break;
        case '👿': // Demon
            drawDemonShape(x, y, radius);
            break;
        case '🕷️': // Spider
            drawSpiderShape(x, y, radius);
            break;
        case '🐉': // Dragon
            drawDragonShape(x, y, radius);
            break;
        case '👾': // Generic enemy
        default:
            drawGenericEnemyShape(x, y, radius);
            break;
    }

    ctx.restore();
}

// === Fallback Player Shapes ===

function drawWizardShape(x, y, r) {
    // Purple wizard with hat
    ctx.fillStyle = '#9966ff';
    ctx.beginPath();
    ctx.arc(x, y + 2, r * 0.7, 0, Math.PI * 2);
    ctx.fill();
    // Hat (triangle)
    ctx.fillStyle = '#6633cc';
    ctx.beginPath();
    ctx.moveTo(x, y - r);
    ctx.lineTo(x - r * 0.6, y - 2);
    ctx.lineTo(x + r * 0.6, y - 2);
    ctx.closePath();
    ctx.fill();
    // Star on hat
    ctx.fillStyle = '#ffcc00';
    ctx.beginPath();
    ctx.arc(x, y - r * 0.5, 3, 0, Math.PI * 2);
    ctx.fill();
}

function drawArcherShape(x, y, r) {
    // Green archer with pointed ears
    ctx.fillStyle = '#66cc66';
    ctx.beginPath();
    ctx.arc(x, y, r * 0.7, 0, Math.PI * 2);
    ctx.fill();
    // Pointed ears
    ctx.fillStyle = '#55aa55';
    ctx.beginPath();
    ctx.moveTo(x - r * 0.5, y - r * 0.3);
    ctx.lineTo(x - r * 0.8, y - r * 0.7);
    ctx.lineTo(x - r * 0.3, y - r * 0.1);
    ctx.closePath();
    ctx.fill();
    ctx.beginPath();
    ctx.moveTo(x + r * 0.5, y - r * 0.3);
    ctx.lineTo(x + r * 0.8, y - r * 0.7);
    ctx.lineTo(x + r * 0.3, y - r * 0.1);
    ctx.closePath();
    ctx.fill();
}

function drawWarriorSwordShape(x, y, r) {
    // Blue warrior with shield shape
    ctx.fillStyle = '#4488ff';
    ctx.beginPath();
    ctx.arc(x, y, r * 0.7, 0, Math.PI * 2);
    ctx.fill();
    // Helmet
    ctx.fillStyle = '#3366cc';
    ctx.beginPath();
    ctx.arc(x, y - r * 0.3, r * 0.5, Math.PI, 0);
    ctx.fill();
    // Sword line
    ctx.strokeStyle = '#cccccc';
    ctx.lineWidth = 3;
    ctx.beginPath();
    ctx.moveTo(x + r * 0.3, y);
    ctx.lineTo(x + r, y - r * 0.3);
    ctx.stroke();
}

function drawNinjaShape(x, y, r) {
    // Dark ninja
    ctx.fillStyle = '#333344';
    ctx.beginPath();
    ctx.arc(x, y, r * 0.7, 0, Math.PI * 2);
    ctx.fill();
    // Eyes only (mask)
    ctx.fillStyle = '#ff3333';
    ctx.beginPath();
    ctx.arc(x - r * 0.2, y - r * 0.1, 3, 0, Math.PI * 2);
    ctx.fill();
    ctx.beginPath();
    ctx.arc(x + r * 0.2, y - r * 0.1, 3, 0, Math.PI * 2);
    ctx.fill();
}

function drawGuardShape(x, y, r) {
    // Red guard with tall hat
    ctx.fillStyle = '#cc4444';
    ctx.beginPath();
    ctx.arc(x, y + 2, r * 0.7, 0, Math.PI * 2);
    ctx.fill();
    // Tall hat (rectangle)
    ctx.fillStyle = '#222222';
    ctx.fillRect(x - r * 0.4, y - r * 1.2, r * 0.8, r * 0.8);
}

function drawGenericPersonShape(x, y, r) {
    // Simple person shape
    ctx.fillStyle = '#ffcc88';
    ctx.beginPath();
    ctx.arc(x, y, r * 0.7, 0, Math.PI * 2);
    ctx.fill();
    // Simple face
    ctx.fillStyle = '#333333';
    ctx.beginPath();
    ctx.arc(x - r * 0.2, y - r * 0.1, 2, 0, Math.PI * 2);
    ctx.fill();
    ctx.beginPath();
    ctx.arc(x + r * 0.2, y - r * 0.1, 2, 0, Math.PI * 2);
    ctx.fill();
}

// === Fallback Enemy Shapes ===

function drawSlimeShape(x, y, r) {
    // Green blob with gooey shape
    ctx.fillStyle = '#44dd44';
    ctx.beginPath();
    ctx.ellipse(x, y + r * 0.2, r, r * 0.7, 0, 0, Math.PI * 2);
    ctx.fill();
    // Shine
    ctx.fillStyle = '#88ff88';
    ctx.beginPath();
    ctx.ellipse(x - r * 0.3, y - r * 0.1, r * 0.2, r * 0.15, -0.3, 0, Math.PI * 2);
    ctx.fill();
    // Eyes
    ctx.fillStyle = '#000000';
    ctx.beginPath();
    ctx.arc(x - r * 0.25, y, 3, 0, Math.PI * 2);
    ctx.fill();
    ctx.beginPath();
    ctx.arc(x + r * 0.25, y, 3, 0, Math.PI * 2);
    ctx.fill();
}

function drawBatShape(x, y, r) {
    // Purple bat with wings
    ctx.fillStyle = '#8844dd';
    ctx.beginPath();
    ctx.arc(x, y, r * 0.5, 0, Math.PI * 2);
    ctx.fill();
    // Wings
    ctx.beginPath();
    ctx.moveTo(x - r * 0.3, y);
    ctx.quadraticCurveTo(x - r, y - r * 0.5, x - r, y + r * 0.3);
    ctx.lineTo(x - r * 0.3, y + r * 0.2);
    ctx.closePath();
    ctx.fill();
    ctx.beginPath();
    ctx.moveTo(x + r * 0.3, y);
    ctx.quadraticCurveTo(x + r, y - r * 0.5, x + r, y + r * 0.3);
    ctx.lineTo(x + r * 0.3, y + r * 0.2);
    ctx.closePath();
    ctx.fill();
    // Eyes
    ctx.fillStyle = '#ff0000';
    ctx.beginPath();
    ctx.arc(x - r * 0.15, y - r * 0.1, 2, 0, Math.PI * 2);
    ctx.fill();
    ctx.beginPath();
    ctx.arc(x + r * 0.15, y - r * 0.1, 2, 0, Math.PI * 2);
    ctx.fill();
}

function drawSkeletonShape(x, y, r) {
    // White skull shape
    ctx.fillStyle = '#eeeeee';
    ctx.beginPath();
    ctx.arc(x, y, r * 0.7, 0, Math.PI * 2);
    ctx.fill();
    // Eye sockets
    ctx.fillStyle = '#000000';
    ctx.beginPath();
    ctx.arc(x - r * 0.25, y - r * 0.1, r * 0.2, 0, Math.PI * 2);
    ctx.fill();
    ctx.beginPath();
    ctx.arc(x + r * 0.25, y - r * 0.1, r * 0.2, 0, Math.PI * 2);
    ctx.fill();
    // Nose hole
    ctx.beginPath();
    ctx.moveTo(x, y + r * 0.1);
    ctx.lineTo(x - r * 0.1, y + r * 0.25);
    ctx.lineTo(x + r * 0.1, y + r * 0.25);
    ctx.closePath();
    ctx.fill();
}

function drawGhostShape(x, y, r) {
    // White ghost with wavy bottom
    ctx.fillStyle = 'rgba(200, 200, 255, 0.8)';
    ctx.beginPath();
    ctx.arc(x, y - r * 0.2, r * 0.7, Math.PI, 0);
    ctx.lineTo(x + r * 0.7, y + r * 0.5);
    ctx.quadraticCurveTo(x + r * 0.35, y + r * 0.3, x, y + r * 0.5);
    ctx.quadraticCurveTo(x - r * 0.35, y + r * 0.3, x - r * 0.7, y + r * 0.5);
    ctx.closePath();
    ctx.fill();
    // Eyes
    ctx.fillStyle = '#000000';
    ctx.beginPath();
    ctx.ellipse(x - r * 0.25, y - r * 0.2, r * 0.12, r * 0.2, 0, 0, Math.PI * 2);
    ctx.fill();
    ctx.beginPath();
    ctx.ellipse(x + r * 0.25, y - r * 0.2, r * 0.12, r * 0.2, 0, 0, Math.PI * 2);
    ctx.fill();
}

function drawDemonShape(x, y, r) {
    // Red demon with horns
    ctx.fillStyle = '#dd4444';
    ctx.beginPath();
    ctx.arc(x, y, r * 0.7, 0, Math.PI * 2);
    ctx.fill();
    // Horns
    ctx.fillStyle = '#880000';
    ctx.beginPath();
    ctx.moveTo(x - r * 0.5, y - r * 0.4);
    ctx.lineTo(x - r * 0.7, y - r);
    ctx.lineTo(x - r * 0.2, y - r * 0.5);
    ctx.closePath();
    ctx.fill();
    ctx.beginPath();
    ctx.moveTo(x + r * 0.5, y - r * 0.4);
    ctx.lineTo(x + r * 0.7, y - r);
    ctx.lineTo(x + r * 0.2, y - r * 0.5);
    ctx.closePath();
    ctx.fill();
    // Evil eyes
    ctx.fillStyle = '#ffff00';
    ctx.beginPath();
    ctx.arc(x - r * 0.25, y - r * 0.1, 3, 0, Math.PI * 2);
    ctx.fill();
    ctx.beginPath();
    ctx.arc(x + r * 0.25, y - r * 0.1, 3, 0, Math.PI * 2);
    ctx.fill();
}

function drawSpiderShape(x, y, r) {
    // Brown spider body
    ctx.fillStyle = '#664422';
    ctx.beginPath();
    ctx.ellipse(x, y, r * 0.4, r * 0.5, 0, 0, Math.PI * 2);
    ctx.fill();
    // Head
    ctx.beginPath();
    ctx.arc(x, y - r * 0.4, r * 0.25, 0, Math.PI * 2);
    ctx.fill();
    // Legs
    ctx.strokeStyle = '#664422';
    ctx.lineWidth = 2;
    for (let i = 0; i < 4; i++) {
        const angle = (i - 1.5) * 0.4;
        ctx.beginPath();
        ctx.moveTo(x - r * 0.3, y + (i - 1.5) * r * 0.2);
        ctx.quadraticCurveTo(x - r, y + (i - 1.5) * r * 0.15, x - r * 0.9, y + r * 0.5 + i * 2);
        ctx.stroke();
        ctx.beginPath();
        ctx.moveTo(x + r * 0.3, y + (i - 1.5) * r * 0.2);
        ctx.quadraticCurveTo(x + r, y + (i - 1.5) * r * 0.15, x + r * 0.9, y + r * 0.5 + i * 2);
        ctx.stroke();
    }
    // Eyes
    ctx.fillStyle = '#ff0000';
    ctx.beginPath();
    ctx.arc(x - r * 0.1, y - r * 0.45, 2, 0, Math.PI * 2);
    ctx.fill();
    ctx.beginPath();
    ctx.arc(x + r * 0.1, y - r * 0.45, 2, 0, Math.PI * 2);
    ctx.fill();
}

function drawDragonShape(x, y, r) {
    // Large red dragon
    ctx.fillStyle = '#ff4400';
    // Body
    ctx.beginPath();
    ctx.ellipse(x, y, r * 0.8, r * 0.6, 0, 0, Math.PI * 2);
    ctx.fill();
    // Head
    ctx.beginPath();
    ctx.ellipse(x + r * 0.6, y - r * 0.3, r * 0.4, r * 0.35, -0.3, 0, Math.PI * 2);
    ctx.fill();
    // Wings
    ctx.fillStyle = '#cc3300';
    ctx.beginPath();
    ctx.moveTo(x - r * 0.2, y - r * 0.3);
    ctx.quadraticCurveTo(x - r * 0.5, y - r * 1.2, x - r, y - r * 0.5);
    ctx.lineTo(x - r * 0.5, y);
    ctx.closePath();
    ctx.fill();
    ctx.beginPath();
    ctx.moveTo(x + r * 0.2, y - r * 0.3);
    ctx.quadraticCurveTo(x + r * 0.5, y - r * 1.2, x + r, y - r * 0.5);
    ctx.lineTo(x + r * 0.5, y);
    ctx.closePath();
    ctx.fill();
    // Eye
    ctx.fillStyle = '#ffff00';
    ctx.beginPath();
    ctx.arc(x + r * 0.7, y - r * 0.35, 4, 0, Math.PI * 2);
    ctx.fill();
    // Fire breath indication
    ctx.fillStyle = '#ffaa00';
    ctx.beginPath();
    ctx.moveTo(x + r, y - r * 0.2);
    ctx.lineTo(x + r * 1.3, y - r * 0.1);
    ctx.lineTo(x + r, y);
    ctx.closePath();
    ctx.fill();
}

function drawGenericEnemyShape(x, y, r) {
    // Purple alien-like shape
    ctx.fillStyle = '#aa44aa';
    ctx.beginPath();
    ctx.arc(x, y, r * 0.7, 0, Math.PI * 2);
    ctx.fill();
    // Antenna
    ctx.strokeStyle = '#aa44aa';
    ctx.lineWidth = 2;
    ctx.beginPath();
    ctx.moveTo(x - r * 0.3, y - r * 0.5);
    ctx.lineTo(x - r * 0.4, y - r);
    ctx.stroke();
    ctx.beginPath();
    ctx.moveTo(x + r * 0.3, y - r * 0.5);
    ctx.lineTo(x + r * 0.4, y - r);
    ctx.stroke();
    // Antenna tips
    ctx.fillStyle = '#ff00ff';
    ctx.beginPath();
    ctx.arc(x - r * 0.4, y - r, 3, 0, Math.PI * 2);
    ctx.fill();
    ctx.beginPath();
    ctx.arc(x + r * 0.4, y - r, 3, 0, Math.PI * 2);
    ctx.fill();
    // Eyes
    ctx.fillStyle = '#00ff00';
    ctx.beginPath();
    ctx.arc(x - r * 0.2, y - r * 0.1, 4, 0, Math.PI * 2);
    ctx.fill();
    ctx.beginPath();
    ctx.arc(x + r * 0.2, y - r * 0.1, 4, 0, Math.PI * 2);
    ctx.fill();
}

/**
 * Render enemies.
 */
function renderEnemies() {
    enemies.forEach(enemy => {
        // Draw enemy emoji
        const emoji = getEnemyEmoji(enemy.type);
        const fontSize = enemy.type === 'dragon' ? 48 : 28;

        drawEmoji(emoji, enemy.x, enemy.y, fontSize);

        // Draw HP bar
        const hpPercent = enemy.hp / enemy.maxHp;
        const hpBarY = enemy.type === 'dragon' ? enemy.y - 30 : enemy.y - ENEMY_SIZE/2 - 8;
        ctx.fillStyle = '#333';
        ctx.fillRect(enemy.x - 15, hpBarY, 30, 4);
        ctx.fillStyle = hpPercent > 0.5 ? '#4a4' : (hpPercent > 0.25 ? '#aa4' : '#a44');
        ctx.fillRect(enemy.x - 15, hpBarY, 30 * hpPercent, 4);
    });
}

/**
 * Get player emoji based on class.
 */
function getPlayerEmoji() {
    switch (gameState.playerClass) {
        case 'wizard': return '🧙';           // Wizard
        case 'archer': return '🧝';           // Elf/archer
        case 'warrior_sword': return '🦸';    // Superhero/warrior
        case 'warrior_dagger': return '🥷';   // Ninja
        case 'warrior_spear': return '💂';    // Guard with spear
        default: return '🧑';                  // Generic person
    }
}

/**
 * Render the player.
 */
function renderPlayer() {
    // Flash when invincible
    if (player.invincible > 0 && Math.floor(player.invincible / 100) % 2) {
        return;
    }

    // Draw player emoji using the emoji drawing function
    const emoji = getPlayerEmoji();
    drawEmoji(emoji, player.x, player.y, 32);

    // Draw direction indicator (small arrow)
    const dir = getDirectionOffset(player.direction);
    ctx.fillStyle = '#00d9ff';
    ctx.beginPath();
    ctx.arc(player.x + dir.x * 18, player.y + dir.y * 18, 4, 0, Math.PI * 2);
    ctx.fill();
}

/**
 * Render projectiles.
 */
function renderProjectiles() {
    projectiles.forEach(proj => {
        switch (proj.type) {
            case 'arrow':
                ctx.fillStyle = '#8b4513';
                ctx.beginPath();
                ctx.arc(proj.x, proj.y, proj.size/2, 0, Math.PI * 2);
                ctx.fill();
                break;
            case 'fireball':
                ctx.fillStyle = '#ff4400';
                ctx.beginPath();
                ctx.arc(proj.x, proj.y, proj.size/2, 0, Math.PI * 2);
                ctx.fill();
                ctx.fillStyle = '#ffaa00';
                ctx.beginPath();
                ctx.arc(proj.x, proj.y, proj.size/3, 0, Math.PI * 2);
                ctx.fill();
                break;
            case 'dragon_fire':
                // Dragon fire - larger, more dangerous looking
                // Outer glow
                ctx.fillStyle = 'rgba(255, 100, 0, 0.5)';
                ctx.beginPath();
                ctx.arc(proj.x, proj.y, proj.size/2 + 4, 0, Math.PI * 2);
                ctx.fill();
                // Main fire
                ctx.fillStyle = '#ff4400';
                ctx.beginPath();
                ctx.arc(proj.x, proj.y, proj.size/2, 0, Math.PI * 2);
                ctx.fill();
                // Hot core
                ctx.fillStyle = '#ffcc00';
                ctx.beginPath();
                ctx.arc(proj.x, proj.y, proj.size/3, 0, Math.PI * 2);
                ctx.fill();
                // White hot center
                ctx.fillStyle = '#ffffff';
                ctx.beginPath();
                ctx.arc(proj.x, proj.y, proj.size/6, 0, Math.PI * 2);
                ctx.fill();
                break;
        }
    });
}

/**
 * Render attack animations.
 */
function renderAttacks() {
    attacks.forEach(attack => {
        const alpha = attack.lifetime / 400;

        switch (attack.type) {
            case 'sword': {
                // 90 degree arc
                ctx.strokeStyle = `rgba(200, 200, 255, ${alpha})`;
                ctx.lineWidth = 6;
                const angle = directionToAngle(attack.direction);
                ctx.beginPath();
                ctx.arc(attack.x, attack.y, attack.range, angle - Math.PI/4, angle + Math.PI/4);
                ctx.stroke();
                break;
            }
            case 'dagger':
            case 'spear': {
                // Line poke
                ctx.strokeStyle = `rgba(200, 200, 200, ${alpha})`;
                ctx.lineWidth = attack.type === 'spear' ? 4 : 3;
                const dir = getDirectionOffset(attack.direction);
                ctx.beginPath();
                ctx.moveTo(attack.x, attack.y);
                ctx.lineTo(attack.x + dir.x * attack.range, attack.y + dir.y * attack.range);
                ctx.stroke();
                break;
            }
            case 'staff_ice': {
                // Circle around player
                ctx.strokeStyle = `rgba(100, 200, 255, ${alpha})`;
                ctx.lineWidth = 8;
                ctx.beginPath();
                ctx.arc(attack.x, attack.y, attack.range, 0, Math.PI * 2);
                ctx.stroke();
                break;
            }
            case 'staff_lightning': {
                // Cone effect
                ctx.fillStyle = `rgba(255, 255, 100, ${alpha * 0.5})`;
                const angle = directionToAngle(attack.direction);
                ctx.beginPath();
                ctx.moveTo(attack.x, attack.y);
                ctx.arc(attack.x, attack.y, attack.range, angle - Math.PI/3, angle + Math.PI/3);
                ctx.closePath();
                ctx.fill();
                break;
            }
        }
    });
}

// ============================================================
// UI UPDATES
// ============================================================

/**
 * Update the HP bar display.
 */
function updateHpBar() {
    const hpBar = document.querySelector('.hp-bar');
    const hpText = document.querySelector('.hp-text');

    if (hpBar && hpText) {
        const percent = Math.max(0, (gameState.hp / gameState.maxHp) * 100);
        hpBar.style.width = percent + '%';
        hpText.textContent = `${Math.max(0, gameState.hp)} / ${gameState.maxHp}`;
    }
}

/**
 * Update the location display.
 */
function updateLocationDisplay() {
    const areaName = document.querySelector('.area-name');
    const mapPos = document.querySelector('.map-pos');

    if (areaName) {
        const names = {
            'training_grounds': 'Training Grounds',
            'castle': 'The Castle',
            'dungeon': 'The Dungeon'
        };
        areaName.textContent = names[gameState.area] || gameState.area;
    }

    if (mapPos) {
        mapPos.textContent = `Map: ${gameState.mapX + 1}, ${gameState.mapY + 1}`;
    }
}

/**
 * Update inventory display.
 */
function updateInventoryDisplay() {
    const slots = document.querySelectorAll('.inventory-slot');
    const icons = {
        'sword': '⚔',
        'dagger': '🗡',
        'spear': '➳',
        'bow': '🏹',
        'staff_fire': '🔥',
        'staff_lightning': '⚡',
        'staff_ice': '❄',
        'health_potion': '🧪'
    };
    const names = {
        'sword': 'Sword',
        'dagger': 'Dagger',
        'spear': 'Spear',
        'bow': 'Bow',
        'staff_fire': 'Fire Staff',
        'staff_lightning': 'Lightning Staff',
        'staff_ice': 'Ice Staff',
        'health_potion': 'Health Potion'
    };

    slots.forEach((slot, index) => {
        const item = gameState.inventory[index];
        const iconEl = slot.querySelector('.item-icon');
        const nameEl = slot.querySelector('.item-name');

        if (item) {
            slot.classList.add('filled');
            slot.classList.remove('empty');
            if (iconEl) iconEl.textContent = icons[item] || '?';
            if (nameEl) nameEl.textContent = names[item] || item;
        } else {
            slot.classList.remove('filled');
            slot.classList.add('empty');
            if (iconEl) iconEl.textContent = '';
            if (nameEl) nameEl.textContent = '';
        }
    });
}

/**
 * Show a message to the player.
 */
function showMessage(text) {
    const display = document.getElementById('message-display');
    if (display) {
        display.textContent = text;
        display.classList.add('show');

        setTimeout(() => {
            display.classList.remove('show');
        }, 2000);
    }
}

// ============================================================
// GAME END CONDITIONS
// ============================================================

/**
 * Handle game over (player death).
 */
function gameOver() {
    gameRunning = false;

    // Create overlay
    const overlay = document.createElement('div');
    overlay.id = 'game-overlay';
    overlay.className = 'show defeat';
    overlay.innerHTML = `
        <h2>GAME OVER</h2>
        <p>You have been defeated...</p>
        <button onclick="location.href='/create'">New Character</button>
        <button onclick="tryAgain()">Try Again</button>
    `;

    document.getElementById('game-container').appendChild(overlay);
}

/**
 * Try again - restore HP to floor entry HP and reload.
 */
function tryAgain() {
    // Restore HP to floor entry HP before reloading
    fetch('/api/game', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ action: 'restore_floor_entry_hp' })
    }).then(() => {
        location.reload();
    });
}

/**
 * Handle victory (dragon defeated).
 */
function victory() {
    gameRunning = false;

    // Notify server
    fetch('/api/game', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ action: 'defeat_dragon' })
    });

    // Create overlay
    const overlay = document.createElement('div');
    overlay.id = 'game-overlay';
    overlay.className = 'show victory';
    overlay.innerHTML = `
        <h2>VICTORY!</h2>
        <p>You have defeated the dragon and saved the realm!</p>
        <button onclick="location.href='/create'">New Adventure</button>
    `;

    document.getElementById('game-container').appendChild(overlay);
}

// ============================================================
// INVENTORY SELECTION
// ============================================================

// Add click handlers to inventory slots
document.addEventListener('DOMContentLoaded', () => {
    document.querySelectorAll('.inventory-slot').forEach((slot, index) => {
        slot.addEventListener('click', () => {
            const item = gameState.inventory[index];
            if (item && isWeapon(item)) {
                player.weapon = item;

                // Update selection visual
                document.querySelectorAll('.inventory-slot').forEach(s => s.classList.remove('selected'));
                slot.classList.add('selected');

                showMessage(`Equipped ${formatItemName(item)}`);
            } else if (item === 'health_potion') {
                // Use health potion
                gameState.hp = Math.min(gameState.maxHp, gameState.hp + 30);
                gameState.inventory.splice(index, 1);
                updateHpBar();
                updateInventoryDisplay();
                showMessage('Used Health Potion (+30 HP)');

                // Save to server - use_potion action heals and removes from inventory
                fetch('/api/game', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({
                        action: 'use_potion'
                    })
                });
            }
        });
    });

    // Select first slot by default
    const firstSlot = document.querySelector('.inventory-slot');
    if (firstSlot) firstSlot.classList.add('selected');
});

/**
 * Check if an item is a weapon.
 */
function isWeapon(item) {
    return ['sword', 'dagger', 'spear', 'bow', 'staff_fire', 'staff_lightning', 'staff_ice'].includes(item);
}
