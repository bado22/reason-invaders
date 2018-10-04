Reason Invaders
---

This was a proof of concept project to get more familiar with ReasonML.

This was written mostly on plane ride after watching the excellent livestream by the creators of [Reprocessing](https://github.com/Schmavery/reprocessing) which can be viewed here: https://www.youtube.com/watch?v=5aD3aPvNpyQ

## How to
```
git clone https://github.com/bado22/reason-invaders.git
```
### Install
```
npm install
```

### Build
```
npm run build
```

### Start
```
npm start
```

This will build the bytecode executable which is at `./lib/bs/bytecode/index.byte`.

To build to JS run `npm run build:web` and then run a static server, like `python -m SimpleHTTPServer` and go to `localhost:8000`.

To build to native run `npm run build:native` and run `./lib/bs/native/index.native`

The build system used is [bsb-native](https://github.com/bsansouci/bsb-native).
