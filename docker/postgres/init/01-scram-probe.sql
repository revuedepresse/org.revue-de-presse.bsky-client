-- Minimal target for the SCRAM round-trip test.
-- Validates that the SCRAM-SHA-256 handshake completed and a query executes.
CREATE TABLE IF NOT EXISTS scram_probe (
    id integer PRIMARY KEY,
    note text NOT NULL
);

INSERT INTO scram_probe (id, note) VALUES (1, 'scram-sha-256 ok')
ON CONFLICT (id) DO NOTHING;
