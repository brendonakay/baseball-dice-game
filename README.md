# Baseball Dice Game

Simulated baseball [dice game](https://milb.bamcontent.com/documents/8/5/8/313285858/BaseballDiceGame_LouisvilleBats.pdf).

## TODO

- [ ] Refactor the various `run` Game state functions into a Free Monadic eDSL.
      Refer to Functional Design and Architecture by Granin for more information on
      this pattern.
- [ ] Create an architectural diagram separating pure and impure layers.
- [ ] Do I rename the project to baseball card game? I was thinking "Waxball".
      Or something. Or do we stick with the dice theme and try to keep that involved.

### Game

- [ ] Cards

### UI

- [ ] HTMX web app.
- [ ] Card design.

### Misc

- [ ] Hot reloading.
- [ ] Use lenses for game state operations.
- [x] Add the ability to print state changes for debugging.
- [x] Use `aeson` for marshalling data structures into JSON. For sending via a
      backend web API.

## Architectural Roadmap

- [ ] HTMX Web App.
- [ ] Cloud hosted.
- [ ] Authentication / user base
