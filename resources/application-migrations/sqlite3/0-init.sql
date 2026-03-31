CREATE TABLE migrations_history (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    version VARCHAR(100) NOT NULL UNIQUE,
    name VARCHAR(255) NOT NULL,          
    script_path TEXT NOT NULL,           
    checksum VARCHAR(64) NOT NULL,       
    status TEXT NOT NULL,
    execution_time_ms INTEGER,
    error_message TEXT,
    applied_by VARCHAR(100) NOT NULL,
    applied_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_migrations_history_version ON migrations_history(version);
