-- Migration: 001_initial_schema.sql
-- Description: Create initial users table
-- Version: 1

PRAGMA user_version = 1;

CREATE TABLE IF NOT EXISTS users (
  userID INTEGER PRIMARY KEY AUTOINCREMENT,
  username TEXT UNIQUE NOT NULL,
  email TEXT UNIQUE NOT NULL,
  password TEXT NOT NULL,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);