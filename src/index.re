
open Reprocessing.Env;
open Reprocessing;

type direction =
  | Top 
  | Bottom

type fruitState = {
  direction: direction,
  isSliced: bool,
  xPos: int,
  yPos: int
}
type images = {
  coconut: imageT,
  coconutHalfOne: imageT,
  coconutHalfTwo: imageT,
}
type state = {
  frameCounter: int,
  fruits: list(fruitState),
  images: images
}
let screenWidth = 500
let screenHeight = 500
let fruitHeight = 50
let fruitWidth = 50

let setup = (env) => {
  Env.size(~width=screenWidth, ~height=screenHeight, env);
  let coconutImg = Draw.loadImage(~filename="assets/coconut.png", env);
  let coconutHalfOneImg = Draw.loadImage(~filename="assets/coconut_half_1.png", env);
  let coconutHalfTwoImg = Draw.loadImage(~filename="assets/coconut_half_2.png", env);
  {
    frameCounter: 0,
    fruits: [
      { direction: Top, yPos: 100, isSliced: false, xPos: 100 },
      { direction: Top, yPos: 200, isSliced: false, xPos: 200 },
      { direction: Top, yPos: 300, isSliced: false, xPos: 300 },
      { direction: Top, yPos: 400, isSliced: false, xPos: 400 },
      { direction: Top, yPos: 500, isSliced: false, xPos: 500 },
      { direction: Top, yPos: 100, isSliced: false, xPos: 0 },
    ],
    images: {
      coconut: coconutImg,
      coconutHalfOne: coconutHalfOneImg,
      coconutHalfTwo: coconutHalfTwoImg,
    }
  };
};
let isFruitMouseCollidingOneDirection = (fruitPos, mousePos, offset) => {
  fruitPos + offset > mousePos && fruitPos < mousePos
}


let isFruitMouseColliding = (fruitXPos, fruitYPos, mouseX, mouseY) => {
  isFruitMouseCollidingOneDirection(fruitXPos, mouseX, fruitWidth) && 
  isFruitMouseCollidingOneDirection(fruitYPos, mouseY, fruitHeight)
}

let draw = (state: state, env) => {
  Draw.background(Utils.color(~r=255, ~g=217, ~b=229, ~a=255), env);
  /* print_endline(_state);
  print_endline(string_of_int(state.frameCounter));
   */
  /* let (mouseX, mouseY) as squarePos = mousePressed(env) */
  let isClicked = mousePressed(env)
  let (mouseX, mouseY) = pmouse(env);
  /* print_endline(string_of_bool(isClicked));
  print_endline(string_of_int(mouseX));
  print_endline(string_of_int(mouseY)); */

  let speed = 4
  let newFruits = List.map(
    fruit => {
      let newIsSliced = 
        switch(isClicked, fruit.isSliced) {
        | (_, true) => fruit.isSliced
        | (false, _) => fruit.isSliced
        | (true, false) => isFruitMouseColliding(fruit.xPos, fruit.yPos, mouseX, mouseY)
      }


      print_endline(string_of_bool(isClicked));

      let newDirection = 
      switch(fruit.yPos) {
        | _idc when fruit.yPos > (screenHeight - fruitHeight) && !fruit.isSliced => Top
        | _idc when fruit.yPos < 0 => Bottom
        | _ => fruit.direction
      }
      /*
      let newYPos = fruit.yPos + 20 * (state.frameCounter / 1000) - 10 * (state.frameCounter / 1000);
      */
      let newYPos =
      switch (newDirection) {
      | Top => fruit.yPos - speed
      | Bottom => fruit.yPos + (speed * 3)
      };
      {
        ...fruit,
        yPos: newYPos,
        isSliced: newIsSliced,
        direction: newDirection
      }
    },
    state.fruits
  );

  List.iter(
    (fruit) => {
      /* 
      let rColor = fruit.isSliced ? 10 : 250
      Draw.fill(Utils.color(~r=rColor, ~g=166, ~b=244, ~a=255), env);
      Draw.rect(~pos=(fruit.xPos, fruit.yPos), ~width=fruitWidth, ~height=fruitHeight, env); */

      /* print_endline(string_of_bool(fruit.isSliced)) */
      let itDoesntMakeSense = if (!fruit.isSliced) {
        Draw.image(state.images.coconut, ~pos =
          (fruit.xPos, fruit.yPos),~width=fruitWidth, ~height=fruitHeight, env);
      } else {
        Draw.image(state.images.coconutHalfOne, ~pos =
          (fruit.xPos + 10, fruit.yPos + 10), ~width=fruitWidth, ~height=fruitHeight, env);

        Draw.image(state.images.coconutHalfTwo, ~pos =
          (fruit.xPos, fruit.yPos),~width=fruitWidth, ~height=fruitHeight, env);
      }
      
    },
    newFruits
  );
  {
    ...state,
    fruits: newFruits,
    frameCounter: state.frameCounter + 1
  }
  /* { direction: newDirection, xPos: newXPos } */
};

run(~setup, ~draw, ());
