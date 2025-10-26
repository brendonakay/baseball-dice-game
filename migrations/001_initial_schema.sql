-- Migration: 000_initial_schema.sql
-- Description: Create initial schema
-- Version: 1

CREATE TABLE IF NOT EXISTS users (
  userID INTEGER PRIMARY KEY AUTOINCREMENT,
  username TEXT UNIQUE NOT NULL,
  email TEXT UNIQUE NOT NULL,
  password TEXT NOT NULL,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- 1 indexed because user_version defaults to 0, which would be a blank
-- database.
PRAGMA user_version = 1;
