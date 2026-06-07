-- Migration: 001_initial_schema.sql
-- Description: Create initial schema with users, players, sets, and cards tables
-- Version: 1

CREATE TABLE IF NOT EXISTS users (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  username TEXT UNIQUE NOT NULL,
  email TEXT UNIQUE NOT NULL,
  password TEXT NOT NULL,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS players (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL,
  number INTEGER NOT NULL,
  batting_average REAL NOT NULL,
  on_base_percentage REAL NOT NULL,
  slugging_percentage REAL NOT NULL,
  era REAL  -- NULL for batters; set for pitchers / two-way players
);

CREATE TABLE IF NOT EXISTS sets (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT UNIQUE NOT NULL,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS cards (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  number TEXT NOT NULL,
  player_id INTEGER REFERENCES players(id),
  set_id INTEGER REFERENCES sets(id),
  user_id INTEGER REFERENCES users(id),
  team TEXT NOT NULL,
  card_class TEXT NOT NULL CHECK (card_class IN ('BASE', 'INSERT', 'PARALLEL')),
  special TEXT CHECK (special IN ('AUTOGRAPH', 'SERIAL')),
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_cards_user_id ON cards(user_id);

-- 1 indexed because user_version defaults to 0, which would be a blank
-- database.
PRAGMA user_version = 1;
