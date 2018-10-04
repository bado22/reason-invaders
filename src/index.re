open Reprocessing;

type coordsT = (int, int);

type movingT =
  | Up
  | Down
  | Left
  | Right
  | NotMoving;

type gamePieceT = {
  coords: coordsT,
  moving: movingT,
  height: int,
  width: int,
  speed: int,
};

let makeGamePieceT = (~coords, ~moving, ~height, ~width, ~speed) => {
  coords,
  moving,
  height,
  width,
  speed,
};

module Assets = {
  let ship = "assets/ship.png";
  let invader = "assets/enemy1_1.png";
  let laser = "assets/laser.png";
  let enemyLaser = "assets/enemylaser.png";
};

module Helper = {
  let percentChance = percentSuccess =>
    Utils.random(~min=1, ~max=100) <= percentSuccess;
  let moveUp = (speed, coords) => (fst(coords), snd(coords) - speed);
  let moveDown = (speed, coords) => (fst(coords), snd(coords) + speed);
  let moveLeft = (speed, coords) => (fst(coords) - speed, snd(coords));
  let moveRight = (speed, coords) => (fst(coords) + speed, snd(coords));
  let spacebarPressed = env => Env.keyPressed(Space, env);
  let keyPressed = (env, key) => Env.keyPressed(key, env);
  let keyReleased = (env, key) => Env.keyReleased(key, env);
  let leftPressed = env => keyPressed(env, Left);
  let rightPressed = env => keyPressed(env, Right);
  let leftReleased = env => keyReleased(env, Left);
  let rightReleased = env => keyReleased(env, Right);
  let hitRightSide = (coords, env) => fst(coords) >= Env.width(env);
  let hitLeftSide = coords => fst(coords) <= 0;
};

module Bullet = {
  type t = gamePieceT;
  let height = 12;
  let width = 12;
  let speed = 5;
  let img = moving =>
    switch (moving) {
    | Down => Assets.enemyLaser
    | _ => Assets.laser
    };
  let make = (coords, moving) =>
    makeGamePieceT(~coords, ~moving, ~height, ~width, ~speed);
};

module Ship = {
  type t = gamePieceT;
  let height = 28;
  let width = 17;
  let speed = 5;
  let img = Assets.ship;
  let makeBullet = ship =>
    Bullet.make((fst(ship.coords), snd(ship.coords) - 10), Up);
  let make = env =>
    makeGamePieceT(
      ~coords=(Env.width(env) / 2, 600),
      ~moving=NotMoving,
      ~height,
      ~width,
      ~speed,
    );
};

module Invader = {
  type t = gamePieceT;
  let height = 44;
  let width = 44;
  let speed = 2;
  let img = Assets.invader;
  let makeBullet = invader =>
    Bullet.make(
      (fst(invader.coords) + width / 2, snd(invader.coords) + height),
      Down,
    );
  let make = (coords, moving) =>
    makeGamePieceT(~coords, ~moving, ~height, ~width, ~speed);
};

module Game = {
  type playingT =
    | Playing
    | GameOver;
  type collidablesT =
    | Bullet
    | Ship
    | Invader;
  type t = {
    ship: Ship.t,
    invaders: list(Invader.t),
    bullets: list(Bullet.t),
    score: int,
    playing: playingT,
  };
  let drawShip = (state: t, env) =>
    switch (state.playing) {
    | Playing =>
      Draw.image(
        Draw.loadImage(~filename=Assets.ship, ~isPixel=true, env),
        ~pos=state.ship.coords,
        ~width=20,
        ~height=19,
        env,
      )
    | GameOver => ()
    };
  let drawInvaders = (state: t, env) =>
    List.iter(
      ({coords}) =>
        Draw.image(
          Draw.loadImage(~filename=Assets.invader, ~isPixel=true, env),
          ~pos=coords,
          ~width=30,
          ~height=30,
          env,
        ),
      state.invaders,
    );
  let getLaserImg = moving =>
    switch (moving) {
    | Down => Assets.enemyLaser
    | _ => Assets.laser
    };
  let drawBullets = (state: t, env) =>
    List.iter(
      ({coords, moving}) =>
        Draw.image(
          Draw.loadImage(~filename=getLaserImg(moving), ~isPixel=true, env),
          ~pos=coords,
          ~width=5,
          ~height=15,
          env,
        ),
      state.bullets,
    );
  let collided = (piece1, piece2) =>
    Utils.intersectRectRect(
      ~rect1Pos=(
        float_of_int(fst(piece1.coords)),
        float_of_int(snd(piece1.coords)),
      ),
      ~rect1W=float_of_int(piece1.width),
      ~rect1H=float_of_int(piece1.height),
      ~rect2Pos=(
        float_of_int(fst(piece2.coords)),
        float_of_int(snd(piece2.coords)),
      ),
      ~rect2W=float_of_int(piece2.width),
      ~rect2H=float_of_int(piece2.height),
    );
  let collidedWithList = (listOfPieces, piece) =>
    List.exists(collided(piece), listOfPieces);
  let piecesMovingInDifferentDirections = (piece1, piece2) =>
    piece1.moving !== piece2.moving;
  let bulletCollidedWithAnotherBullet = (bullets, targetBullet) =>
    List.exists(
      bullet =>
        piecesMovingInDifferentDirections(targetBullet, bullet)
        && collided(targetBullet, bullet),
      bullets,
    );
  let pieceCollided = (pieceType: collidablesT, state: t, piece) =>
    switch (pieceType) {
    | Bullet =>
      collidedWithList(state.invaders, piece)
      || bulletCollidedWithAnotherBullet(state.bullets, piece)
      || collided(state.ship, piece)
    | Ship =>
      collidedWithList(state.invaders, piece)
      || collidedWithList(state.bullets, piece)
    | Invader =>
      collidedWithList(state.bullets, piece) || collided(state.ship, piece)
    };
  let removeCollidedInvaders = (state, invaders) =>
    List.filter(
      invader => ! pieceCollided(Invader, state, invader),
      invaders,
    );
  let addBulletsFromShip = (condition, state: t, list) =>
    condition ? [Ship.makeBullet(state.ship), ...list] : list;
  let addBulletsFromInvaders = (condition, state: t, bullets) =>
    List.fold_left(
      (_, invader) =>
        condition ? [Invader.makeBullet(invader), ...bullets] : bullets,
      [],
      state.invaders,
    );
  /* end helper fns */
  /* start draw fns */
  let movePiece = piece => {
    ...piece,
    coords:
      switch (piece.moving) {
      | Up => Helper.moveUp(piece.speed, piece.coords)
      | Down => Helper.moveDown(piece.speed, piece.coords)
      | _ => piece.coords
      },
  };
  let updateBulletsCoords = bullets => List.map(movePiece, bullets);
  let removeCollidedBullets = (state, bullets) =>
    List.filter(bullet => ! pieceCollided(Bullet, state, bullet), bullets);
  let updateInvaderCoords = (env, invader) =>
    switch (
      invader.moving,
      Helper.hitLeftSide(invader.coords),
      Helper.hitRightSide(invader.coords, env),
    ) {
    | (Right, _, true) =>
      invader.coords |> Helper.moveDown(Invader.height / 2)
    | (Left, true, _) =>
      invader.coords |> Helper.moveDown(Invader.height / 2)
    | (Right, _, _) => invader.coords |> Helper.moveRight(invader.speed)
    | (Left, _, _) => invader.coords |> Helper.moveLeft(invader.speed)
    | (_, _, _) => invader.coords
    };
  let updateInvaderMoving = (env, invader) =>
    switch (
      invader.moving,
      Helper.hitLeftSide(invader.coords),
      Helper.hitRightSide(invader.coords, env),
    ) {
    | (Right, false, true) => Left
    | (Left, true, false) => Right
    | (_, _, _) => invader.moving
    };
  let updateInvaderWithNewCoords = (env, invader) => {
    ...invader,
    coords: updateInvaderCoords(env, invader),
  };
  let updateInvadersCoords = (env, invaders) =>
    List.map(updateInvaderWithNewCoords(env), invaders);
  let updateInvadersMoving = (env, invaders) =>
    List.map(
      invader => {...invader, moving: updateInvaderMoving(env, invader)},
      invaders,
    );
  let updateShipMoving = (env, state: t) =>
    switch (
      Helper.leftPressed(env),
      Helper.rightPressed(env),
      Helper.leftReleased(env) || Helper.rightReleased(env),
    ) {
    | (true, _, _) => Left
    | (_, true, _) => Right
    | (_, _, true) => NotMoving
    | (_, _, _) => state.ship.moving
    };
  let updateShipCoords = (state: t) =>
    switch (state.ship.moving) {
    | Left => Helper.moveLeft(state.ship.speed, state.ship.coords)
    | Right => Helper.moveRight(state.ship.speed, state.ship.coords)
    | _ => state.ship.coords
    };
};

/* end draw fns */
let makeDefaultInvaders = () => [
  Invader.make((100, 100), Right),
  Invader.make((200, 100), Right),
  Invader.make((300, 100), Right),
  Invader.make((100, 200), Right),
  Invader.make((200, 200), Right),
  Invader.make((300, 200), Right),
  Invader.make((100, 300), Right),
  Invader.make((200, 300), Right),
  Invader.make((300, 300), Right),
];

let makeDefaultSetup = env : Game.t => {
  ship: Ship.make(env),
  invaders: makeDefaultInvaders(),
  bullets: [],
  score: 0,
  playing: Playing,
};

let setup = env : Game.t => {
  let width = 400;
  let height = 650;
  Env.size(~width, ~height, env);
  makeDefaultSetup(env);
};

let draw = (state: Game.t, env) => {
  Draw.background(Reprocessing_Constants.black, env);
  if (state.playing === Playing) {
    Game.drawShip(state, env);
    Game.drawInvaders(state, env);
    Game.drawBullets(state, env);
  };
  switch (state.playing) {
  | GameOver => makeDefaultSetup(env)
  | Playing => {
      ...state,
      invaders:
        state.invaders
        |> Game.removeCollidedInvaders(state)
        |> Game.updateInvadersCoords(env)
        |> Game.updateInvadersMoving(env),
      ship: {
        ...state.ship,
        moving: Game.updateShipMoving(env, state),
        coords: Game.updateShipCoords(state),
      },
      bullets:
        state.bullets
        |> Game.removeCollidedBullets(state)
        |> Game.addBulletsFromShip(Helper.spacebarPressed(env), state)
        |> Game.addBulletsFromInvaders(Helper.percentChance(1), state)
        |> Game.updateBulletsCoords,
      playing:
        switch (
          state.playing,
          Game.pieceCollided(Ship, state, state.ship),
          Helper.spacebarPressed(env),
        ) {
        | (Playing, true, _) => GameOver
        | (Playing, false, _) => Playing
        | (GameOver, _, true) => Playing
        | (GameOver, _, false) => GameOver
        },
    }
  };
};

run(~setup, ~draw, ());