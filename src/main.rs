use std::{cell::RefCell, cmp::Ordering, collections::BinaryHeap};

use bevy::{
    prelude::*,
    utils::{HashMap, HashSet},
    window::PrimaryWindow,
};

static TILE_SIZE: f32 = 10.;
static TILE_COUNT: i32 = 40;

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_systems(Startup, setup)
        .add_systems(Update, (click_handler, node_render, path_search))
        .run();
}

#[derive(Component)]
struct MainCamera;

#[derive(Default, Debug, Eq, PartialEq)]
enum PathNodeState {
    #[default]
    None,
    Start,
    End,
    Blocked,
    Open,
    Closed,
    Path,
}

#[derive(Component, Debug, Default, Eq, PartialEq)]
struct PathNode {
    g_cost: usize,
    h_cost: usize,
    prev_node: Option<Position>,
    state: PathNodeState,
    x: i32,
    y: i32,
}

impl PathNode {
    #[inline]
    fn f_cost(&self) -> usize {
        self.g_cost + self.h_cost
    }
}

impl PartialOrd for PathNode {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// This impl returns the _reverse_ ordering for f_cost and g_cost
/// because we use this in a [`BinaryHeap`] (which is a max heap).
impl Ord for PathNode {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self.f_cost(), other.f_cost(), self.g_cost, other.g_cost) {
            (a, b, _, _) if a < b => Ordering::Greater,
            (a, b, c, d) if a == b && c < d => Ordering::Greater,
            (a, b, c, d) if a == b && c == d => Ordering::Equal,
            (_, _, _, _) => Ordering::Less,
        }
    }
}

#[derive(Copy, Clone, Component, Debug, Hash, Eq, PartialEq)]
struct Position {
    x: i32,
    y: i32,
}

#[derive(Debug, Default, Resource)]
struct Path {
    start_node: Option<Position>,
    target_node: Option<Position>,
    path_found: Option<bool>,
}

fn setup(mut commands: Commands) {
    commands.init_resource::<Path>();
    commands.spawn((Camera2dBundle::default(), MainCamera));

    let x_offset = TILE_SIZE * TILE_COUNT as f32 / 2.;
    let y_offset = TILE_SIZE * TILE_COUNT as f32 / 2.;

    for y in 0..TILE_COUNT {
        for x in 0..TILE_COUNT {
            commands.spawn((
                SpriteBundle {
                    sprite: Sprite {
                        color: Color::SALMON,
                        custom_size: Some(Vec2::new(TILE_SIZE - 1., TILE_SIZE - 1.)),
                        ..default()
                    },
                    transform: Transform::from_translation(Vec3::new(
                        x as f32 * TILE_SIZE - x_offset,
                        y as f32 * TILE_SIZE - y_offset,
                        0.,
                    )),
                    ..default()
                },
                Position { x, y },
                PathNode {
                    x,
                    y,
                    // Don't set MAX as we will overflow.
                    g_cost: usize::MAX / 2,
                    ..default()
                },
            ));
        }
    }
}

fn node_render(mut q_nodes: Query<(&mut Sprite, &PathNode)>) {
    for (mut sprite, node) in q_nodes.iter_mut() {
        sprite.color = match node.state {
            PathNodeState::None => Color::GRAY,
            PathNodeState::Start => Color::GREEN,
            PathNodeState::End => Color::BLUE,
            PathNodeState::Open => Color::CYAN,
            PathNodeState::Closed => Color::ORANGE,
            PathNodeState::Path => Color::RED,
            PathNodeState::Blocked => Color::BLACK,
        };
    }
}

fn click_handler(
    mouse: Res<Input<MouseButton>>,
    keys: Res<Input<KeyCode>>,
    mut path: ResMut<Path>,
    mut q_nodes: Query<(&Position, &mut PathNode)>,
    q_window: Query<&Window, With<PrimaryWindow>>,
    q_camera: Query<(&Camera, &GlobalTransform), With<MainCamera>>,
) {
    let left_click = mouse.just_pressed(MouseButton::Left);
    let right_click = mouse.just_pressed(MouseButton::Right);
    let shift_key = keys.pressed(KeyCode::ShiftLeft);

    if left_click || right_click {
        let (camera, camera_transform) = q_camera.single();
        let window = q_window.single();
        if let Some(world_position) = window
            .cursor_position()
            .and_then(|cursor| camera.viewport_to_world(camera_transform, cursor))
            .map(|ray| ray.origin.truncate())
        {
            // Determine which tile the click landed on
            let offset = TILE_SIZE * TILE_COUNT as f32 / 2.;
            if world_position.x.abs() < offset && world_position.y < offset {
                // It landed in our grid.
                let x = ((world_position.x + offset) / TILE_SIZE).round() as i32;
                let y = ((world_position.y + offset) / TILE_SIZE).round() as i32;

                for (position, mut node) in q_nodes.iter_mut() {
                    if (left_click && node.state == PathNodeState::Start && !shift_key)
                        || (right_click && node.state == PathNodeState::End)
                    {
                        node.state = PathNodeState::None;
                    }

                    if position.x == x && position.y == y {
                        // This is the node we clicked
                        if left_click {
                            if node.state == PathNodeState::End {
                                path.target_node = None;
                            }
                            if shift_key {
                                if node.state == PathNodeState::Blocked {
                                    node.state = PathNodeState::None;
                                } else {
                                    node.state = PathNodeState::Blocked;
                                }
                            } else {
                                node.state = PathNodeState::Start;
                                path.start_node = Some(*position);
                            }
                        } else if right_click {
                            if node.state == PathNodeState::Start {
                                path.start_node = None;
                            }
                            node.state = PathNodeState::End;
                            path.target_node = Some(*position);
                        }
                    }
                }
            }
        }
    }
}

fn path_search(
    keys: Res<Input<KeyCode>>,
    mut path: ResMut<Path>,
    mut q_nodes: Query<(&Position, &mut PathNode)>,
) {
    if keys.just_pressed(KeyCode::S) {
        if let Some(found) = path.path_found {
            info!("path already searched: found - {found}");
            return;
        }

        // Start the search!
        let Some(start_pos) = path.start_node else {
            warn!("no start position: {path:?}");
            return;
        };

        let Some(target_pos) = path.target_node else {
            warn!("no end position: {path:?}");
            return;
        };

        let positions = {
            let mut positions = HashMap::new();
            for (pos, node) in q_nodes.iter_mut() {
                positions.insert(pos, RefCell::new(node.into_inner()));
            }
            positions
        };

        let mut open_set = BinaryHeap::with_capacity(positions.len());
        let start_node = positions.get(&start_pos).expect("it was just added");
        {
            start_node.borrow_mut().g_cost = 0;
        }
        open_set.push(start_node);

        let mut closed_set = HashSet::new();

        let mut current;

        loop {
            if open_set.is_empty() {
                info!("no more nodes to process");
                break;
            }

            current = open_set.pop().expect("we have validated this has one item");

            // info!("current node: {current:?}");

            let (current_pos, current_g) = {
                let c = current.borrow();
                (Position { x: c.x, y: c.y }, c.g_cost)
            };

            closed_set.insert(current_pos);

            if current_pos == target_pos {
                path.path_found = Some(true);
                // Found
                info!("FOUND PATH");
                let mut pos = Some(target_pos);
                loop {
                    let Some(node_pos) = pos else {
                        break;
                    };
                    let node = positions
                        .get(&node_pos)
                        .expect("we should be tracking all positions here");

                    let mut node_mut = node.borrow_mut();
                    node_mut.state = PathNodeState::Path;
                    pos = node_mut.prev_node;
                }

                positions.get(&start_pos).unwrap().borrow_mut().state = PathNodeState::Start;
                positions.get(&target_pos).unwrap().borrow_mut().state = PathNodeState::End;

                return;
            }

            // Add it to the closed set.
            {
                let mut n_mut = current.borrow_mut();
                n_mut.state = PathNodeState::Closed;
            }

            // For each neighbour
            for n_x in -1..=1i32 {
                for n_y in -1..=1i32 {
                    if n_x == 0 && n_y == 0 {
                        // skip this node
                        continue;
                    }

                    let n_pos = Position {
                        x: current_pos.x + n_x,
                        y: current_pos.y + n_y,
                    };

                    // info!("looking at neighbour: {n_pos:?}");

                    let Some(neighbour) = positions.get(&n_pos) else {
                        // Keep going if there's nothing in this spot.
                        continue;
                    };

                    // info!("found neighbour: {neighbour:?}");
                    {
                        if neighbour.borrow().state == PathNodeState::Blocked
                            || neighbour.borrow().state == PathNodeState::Closed
                        {
                            // info!("neighbour is blocked or closed!");
                            continue;
                        }
                    }
                    // tentative g_score
                    let new_g = current_g + {
                        // diagonal vs straight
                        if n_x.abs() == 1 && n_y.abs() == 1 {
                            14
                        } else {
                            10
                        }
                    };

                    let n_g = {
                        let n = neighbour.borrow();
                        n.g_cost
                    };

                    if new_g < n_g {
                        let in_open = { open_set.iter().any(|i| *i == neighbour) };

                        let mut n_mut = neighbour.borrow_mut();
                        n_mut.prev_node = Some(current_pos);
                        n_mut.g_cost = new_g;
                        n_mut.h_cost = dist(n_pos, target_pos) as usize;

                        if !in_open {
                            n_mut.state = PathNodeState::Open;

                            drop(n_mut);
                            open_set.push(neighbour);
                        }
                    }
                }
            }
        }
        path.path_found = Some(false);
    } else if keys.just_pressed(KeyCode::R) {
        // Reset the state
        path.path_found = None;
        for (_pos, mut node) in q_nodes.iter_mut() {
            reset_node(&mut node);
        }
    }
}

fn reset_node(node: &mut PathNode) {
    node.g_cost = usize::MAX / 2;
    node.h_cost = 0;
    node.prev_node = None;

    if node.state != PathNodeState::Blocked
        && node.state != PathNodeState::Start
        && node.state != PathNodeState::End
    {
        node.state = PathNodeState::None;
    }
}

/// Approximate the distance between two points.
///
/// `10` for horizontal or vertical, `14` for diagonals (`10 * sqrt(2)`).
fn dist(from: Position, to: Position) -> i32 {
    let d_x = (from.x - to.x).abs();
    let d_y = (from.y - to.y).abs();
    if d_x > d_y {
        14 * d_y + 10 * (d_x - d_y)
    } else {
        14 * d_x + 10 * (d_y - d_x)
    }
}
