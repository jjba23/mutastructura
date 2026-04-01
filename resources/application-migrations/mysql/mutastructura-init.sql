CREATE TABLE migrations_history (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    checksum VARCHAR(64) NOT NULL,
    applied_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    UNIQUE (name, checksum)
);
