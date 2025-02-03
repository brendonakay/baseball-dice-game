#!/usr/bin/env bash

cabal run | sed '1,/^.*Game Log: /d' >game.json

sqlite3 ':memory:' -header -column -cmd "
    CREATE TABLE temp_json(
      currentBatter JSON,
      strikeAction  TEXT,
      inning        INTEGER,
      halfInning    TEXT,
      homeBatting   INTEGER,
      awayBatting   INTEGER,
      homeScore     INTEGER,
      awayScore     INTEGER,
      balls         INTEGER,
      strikes       INTEGER,
      outs          INTEGER,
      bases         JSON);
    INSERT INTO temp_json (
      currentBatter,
      strikeAction,
      inning,
      halfInning,
      homeBatting,
      awayBatting,
      homeScore,
      awayScore,
      balls,
      strikes,
      outs,
      bases) 
      SELECT  
          json_extract(value, '$.currentBatter_') AS currentBatter,
          json_extract(value, '$.strikeAction_')  AS strikeAction,
          json_extract(value, '$.inning_')        AS inning,
          json_extract(value, '$.halfInning_')    AS halfInning,
          json_extract(value, '$.homeBatting_')   AS homeBatting,
          json_extract(value, '$.awayBatting_')   AS awayBatting,
          json_extract(value, '$.homeScore_')     AS homeScore,
          json_extract(value, '$.awayScore_')     AS awayScore,
          json_extract(value, '$.balls_')         AS balls,
          json_extract(value, '$.strikes_')       AS strikes,
          json_extract(value, '$.outs_')          AS outs,
          json_extract(value, '$.bases_')         AS bases
          FROM json_each(readfile('game.json'));
    SELECT homeScore, awayScore, inning, halfInning, currentBatter, balls, strikes, outs, strikeAction, awayBatting, homeBatting FROM temp_json;" |
	less

rm game.json
