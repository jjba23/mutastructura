CREATE TABLE migrations_history (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name VARCHAR(255) NOT NULL,
    checksum VARCHAR(64) NOT NULL,
    applied_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    UNIQUE (name, checksum)
);
