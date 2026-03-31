CREATE TYPE migration_status AS ENUM (
    'PENDING', 
    'IN_PROGRESS', 
    'APPLIED', 
    'FAILED', 
    'ROLLED_BACK'
);

CREATE TABLE migrations_history (
    id SERIAL PRIMARY KEY,
    
    -- Identification
    version VARCHAR(100) NOT NULL UNIQUE, -- e.g., '20231024103000' or 'V1.2.3'
    name VARCHAR(255) NOT NULL,           -- e.g., 'create_users_table'
    
    -- File specifics
    script_path TEXT NOT NULL,            -- e.g., 'db/migrations/20231024_create_users.sql'
    checksum VARCHAR(64) NOT NULL,        -- SHA-256 hash of the script's contents
    
    -- Execution details
    status migration_status NOT NULL DEFAULT 'PENDING',
    execution_time_ms INTEGER,            -- How long the migration took to run
    error_message TEXT,                   -- Stack trace or error if it failed
    
    -- Audit trail
    applied_by VARCHAR(100) NOT NULL,     -- Database user or app service name
    applied_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Index for quickly querying the latest migrations
CREATE INDEX idx_migrations_history_version ON migrations_history(version);
