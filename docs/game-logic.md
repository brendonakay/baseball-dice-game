# Baseball Dice Game - At-Bat Logic

This document describes the game logic flow for an at-bat in the baseball dice game.

## Mermaid Diagram

```mermaid
flowchart TD
    Start([At-Bat Begins]) --> Roll[Roll 3 Dice]

    Roll --> Pitch{First Die:<br/>Even = Ball<br/>Odd = Strike}

    Pitch -->|Ball| AddBall[Add Ball to Count]
    Pitch -->|Strike| StrikeAction[Determine Strike Action<br/>Using Dice 2 & 3]

    AddBall --> CheckBalls{Ball Count = 4?}
    CheckBalls -->|No| Roll
    CheckBalls -->|Yes| Walk[Walk - Runner to First]

    StrikeAction --> PlayerCheck{Player Stats Check<br/>Batting Avg + Dice Modifier}

    PlayerCheck -->|Hit| HitType{Determine Hit Type<br/>Based on Slugging %}
    PlayerCheck -->|Out| OutType{Determine Out Type<br/>Based on Dice Combo}

    HitType --> Single[Single<br/>All Runners +1 Base]
    HitType --> Double[Double<br/>All Runners +2 Bases]
    HitType --> Triple[Triple<br/>All Runners +3 Bases]
    HitType --> Homer[Home Run<br/>All Runners Score]

    OutType --> Called[Called Strike<br/>Add Strike]
    OutType --> Ground[Ground Out<br/>Add Out]
    OutType --> Fly[Fly Out<br/>Add Out]
    OutType --> Pop[Pop Out<br/>Add Out]
    OutType --> HBP[Hit By Pitch<br/>Runner to First]
    OutType --> Error[Fielding Error<br/>Runner to First]

    Called --> CheckStrikes{Strike Count = 3?}
    CheckStrikes -->|No| Roll
    CheckStrikes -->|Yes| Strikeout[Strikeout<br/>Add Out]

    Ground --> AddOut1[Add Out]
    Fly --> AddOut2[Add Out]
    Pop --> AddOut3[Add Out]
    Strikeout --> AddOut4[Add Out]

    AddOut1 --> CheckOuts{Outs = 3?}
    AddOut2 --> CheckOuts
    AddOut3 --> CheckOuts
    AddOut4 --> CheckOuts

    Single --> NextBatter1[Next Batter Up]
    Double --> NextBatter2[Next Batter Up]
    Triple --> NextBatter3[Next Batter Up]
    Homer --> NextBatter4[Next Batter Up]
    Walk --> NextBatter5[Next Batter Up]
    HBP --> NextBatter6[Next Batter Up]
    Error --> NextBatter7[Next Batter Up]

    CheckOuts -->|No| NextBatter8[Next Batter Up]
    CheckOuts -->|Yes| NextInning[Next Half Inning<br/>Reset Outs/Bases]

    NextBatter1 --> NewAtBat([New At-Bat])
    NextBatter2 --> NewAtBat
    NextBatter3 --> NewAtBat
    NextBatter4 --> NewAtBat
    NextBatter5 --> NewAtBat
    NextBatter6 --> NewAtBat
    NextBatter7 --> NewAtBat
    NextBatter8 --> NewAtBat
    NextInning --> NewAtBat

    style Start fill:#90EE90
    style NewAtBat fill:#90EE90
    style Roll fill:#FFE4B5
    style PlayerCheck fill:#FFB6C1
    style CheckBalls fill:#87CEEB
    style CheckStrikes fill:#87CEEB
    style CheckOuts fill:#87CEEB
```

## Key Implementation Details

### Primary Functions

- **Entry Point**: `advanceGameState` in `src/Game/State.hs:162-175`
- **Core Logic**: `runPitch` in `src/Game/Logic.hs:397-413`
- **Dice Processing**: `pitchBallOrStrike` in `src/Game/State.hs:65-69`

### Decision Logic

1. **Pitch Type**: First die determines Ball (even) vs Strike (odd)
2. **Hit Determination**: Player's batting average + dice modifier
3. **Hit Types**: Based on player's slugging percentage
4. **Out Types**: Special dice combinations or random distribution

### State Management

- Ball/Strike counts tracked and checked for walks/strikeouts
- Base runners advanced according to hit type
- Outs accumulated and checked for inning changes
- Batting order cycles through team lineup

