
open Reprocessing.Env;
open Reprocessing;

type direction =
  | Top 
  | Bottom

type fruitType = 
  | Cocunut
  | Banana;

type fruitState = {
  direction: direction,
  isSliced: bool,
  xPos: int,
  yPos: int,
  slicedOffset: int,
  fruitType: fruitType,
};

type images = {
  coconut: imageT,
  coconutHalfOne: imageT,
  coconutHalfTwo: imageT,
  banana: imageT,
  bananaHalfOne: imageT,
  bananaHalfTwo: imageT,
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
  let bananaImg = Draw.loadImage(~filename="assets/banana.png", env);
  let bananaHalfOneImg = Draw.loadImage(~filename="assets/banana_half_2_small.png", env);
  let bananaHalfTwoImg = Draw.loadImage(~filename="assets/banana_half_1.png", env);
  {
    frameCounter: 0,
    fruits: [
      { direction: Top, slicedOffset: 0, yPos: 100, isSliced: false, xPos: 100, fruitType: Cocunut },
      { direction: Top, slicedOffset: 0, yPos: 200, isSliced: false, xPos: 200, fruitType: Banana },
      { direction: Top, slicedOffset: 0, yPos: 300, isSliced: false, xPos: 300, fruitType: Banana },
      { direction: Top, slicedOffset: 0, yPos: 400, isSliced: false, xPos: 400, fruitType: Cocunut },
      { direction: Top, slicedOffset: 0, yPos: 500, isSliced: false, xPos: 500, fruitType: Banana },
      { direction: Top, slicedOffset: 0, yPos: 100, isSliced: false, xPos: 0, fruitType: Cocunut },
    ],
    images: {
      coconut: coconutImg,
      coconutHalfOne: coconutHalfOneImg,
      coconutHalfTwo: coconutHalfTwoImg,
      banana: bananaImg,
      bananaHalfOne: bananaHalfOneImg,
      bananaHalfTwo: bananaHalfTwoImg,
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
  
  let isClicked = mousePressed(env)
  let (mouseX, mouseY) = pmouse(env);

  let speed = 4
  let newFruits = List.map(
    fruit => {
      let newIsSliced = 
        switch(isClicked, fruit.isSliced) {
        | (_, true) => fruit.isSliced
        | (false, _) => fruit.isSliced
        | (true, false) => isFruitMouseColliding(fruit.xPos, fruit.yPos, mouseX, mouseY)
      }

      let newDirection = 
      switch(fruit.yPos) {
        | _idc when fruit.yPos > (screenHeight - fruitHeight) && !fruit.isSliced => Top
        | _idc when fruit.yPos < 0 => Bottom
        | _ => fruit.direction
      }

      let newYPos =
      switch (newDirection) {
      | Top => fruit.yPos - speed
      | Bottom => fruit.yPos + (speed * 3)
      };
      {
        ...fruit,
        yPos: newYPos,
        isSliced: newIsSliced,
        direction: newDirection,
        slicedOffset: fruit.isSliced ? fruit.slicedOffset + 1 : fruit.slicedOffset,
      }
    },
    state.fruits
  );

  List.iter(
    (fruit) => {
      let itDoesntMakeSense = if (!fruit.isSliced) {
        let imgSrc = 
        switch(fruit.fruitType) {
          | Banana => state.images.banana
          | Cocunut => state.images.coconut
        }
        Draw.image(imgSrc, ~pos =
          (fruit.xPos, fruit.yPos),~width=fruitWidth, ~height=fruitHeight, env);
      } else {
        let leftHalfImgSrc =
        switch(fruit.fruitType) {
          | Banana => state.images.bananaHalfOne
          | Cocunut => state.images.coconutHalfOne
        }
        let rightHalfImgSrc =
        switch(fruit.fruitType) {
          | Banana => state.images.bananaHalfTwo
          | Cocunut => state.images.coconutHalfTwo
        }
        let randomDivNumber = 3
        Draw.image(leftHalfImgSrc, ~pos = (
          fruit.xPos + 10 + (fruit.slicedOffset / randomDivNumber), fruit.yPos + 10),
          ~width=fruitWidth,
          ~height=fruitHeight,
          env
        );

        Draw.image(rightHalfImgSrc, ~pos = (
          fruit.xPos - (fruit.slicedOffset / randomDivNumber), fruit.yPos),
          ~width=fruitWidth,
          ~height=fruitHeight,
          env
        );
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
